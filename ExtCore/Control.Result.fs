(*

Copyright 2010-2012 TidePowerd Ltd.
Copyright 2011 Tomas Petricek
Copyright 2013 Jack Pappas

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

*)

namespace ExtCore.Control

open ExtCore


/// <summary>
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type ReaderResultFunc<'Env, 'T, 'Error> =
    'Env -> Result<'T, 'Error>

/// <summary>
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type ReaderProtectedStateFunc<'Env, 'State, 'T, 'Error> =
    'Env -> 'State -> Result<'T * 'State, 'Error>

/// <summary>
/// </summary>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type StatefulResultFunc<'State, 'T, 'Error> =
    'State -> Result<'T, 'Error> * 'State

/// <summary>
/// </summary>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type AsyncResult<'T, 'Error> =
    Async<Result<'T, 'Error>>


/// <summary>
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type AsyncReaderResultFunc<'Env, 'T, 'Error> =
    'Env -> Async<Result<'T, 'Error>>

/// <summary>
/// </summary>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type AsyncProtectedStateFunc<'State, 'T, 'Error> =
    'State -> Async<Result<'T * 'State, 'Error>>

/// <summary>
/// </summary>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type AsyncStatefulResultFunc<'State, 'T, 'Error> =
    'State -> Async<Result<'T, 'Error> * 'State>


(*** Workflow Builders ***)


/// <summary>
/// </summary>
[<Sealed>]
type ResultBuilder () =
    /// The zero value for this builder never changes and is immutable,
    /// so create and reuse a single instance of it to avoid unnecessary allocations.
    static let zero = Ok ()

    // 'T -> M<'T>
    member __.Return value : Result<'T, 'Error> =
        Ok value

    // Error operation. Similar to the Return method ('return'), but used for returning an error value.
    [<CustomOperation("error")>]
    member __.Error value : Result<'T, 'Error> =
        Error value

    // M<'T> -> M<'T>
    member __.ReturnFrom (m : Result<'T, 'Error>) =
        m

    // unit -> M<'T>
    member __.Zero () : Result<unit, 'Error> =
        zero

    // (unit -> M<'T>) -> M<'T>
    member __.Delay (generator : unit -> Result<'T, 'Error>) : unit -> Result<'T, 'Error> =
        generator

    //
    member __.Run (generator : unit -> Result<'T, 'Error>) : Result<'T, 'Error> =
        generator ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (value, binder : 'T -> Result<'U, 'Error>) : Result<'U, 'Error> =
        match value with
        | Error error ->
            Error error
        | Ok x ->
            binder x

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline __.Combine (r1, r2) : Result<'T, 'Error> =
        match r1 with
        | Error error ->
            Error error
        | Ok () ->
            r2

    //
    member __.Combine (r1 : Result<'T, 'Error>, r2) : Result<'U, 'Error> =
        Result.bind r2 r1

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member inline __.TryWith (body : unit -> Result<'T, 'Error>, handler) =
        try body ()
        with ex ->
            handler ex

    // M<'T> * (unit -> unit) -> M<'T>
    member inline __.TryFinally (body : unit -> Result<'T, 'Error>, handler) =
        try body ()
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : _ -> Result<_,_>)
        : Result<'U, 'Error> =
        try body resource
        finally
            if not <| isNull (box resource) then
                resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : unit -> Result<unit, 'Error>) : Result<_,_> =
        if guard () then
            match body () with
            | Ok () ->
                this.While (guard, body)
            | err -> err
        else
            // Return Choice1Of2 () to indicate success when the loop
            // finishes normally (because the guard returned false).
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> Result<unit, 'Error>) =
        use enumerator = sequence.GetEnumerator ()

        let mutable errorResult = None
        while enumerator.MoveNext () && Option.isNone errorResult do
            match body enumerator.Current with
            | Ok () -> ()
            | error ->
                errorResult <- Some error

        // If we broke out of the loop early because the 'body' function
        // returned an error for some element, return the error.
        // Otherwise, return the 'zero' value (representing a 'success' which carries no value).
        if Option.isNone errorResult then this.Zero () else errorResult.Value


/// <summary>
/// </summary>
[<Sealed>]
type ReaderResultBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : ReaderResultFunc<'Env, 'T, 'Error> =
        fun _ -> Ok value

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : ReaderResultFunc<'Env, 'T, 'Error> =
        func

    // unit -> M<'T>
    member __.Zero ()
        : ReaderResultFunc<'Env, unit, 'Error> =
        fun _ -> Ok ()

    // (unit -> M<'T>) -> M<'T>
    member __.Delay (generator : unit -> ReaderResultFunc<'Env, 'T, 'Error>)
        : ReaderResultFunc<'Env, 'T, 'Error> =
        generator ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (f : ReaderResultFunc<_,_,_>, binder : 'T -> ReaderResultFunc<_,_,_>)
        : ReaderResultFunc<'Env, 'U, 'Error> =
        fun env ->
        match f env with
        | Error error ->
            Error error
        | Ok result ->
            binder result env

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member __.Combine (r1 : ReaderResultFunc<_,_,_>, r2 : ReaderResultFunc<_,_,_>)
        : ReaderResultFunc<'Env, 'T, 'Error> =
        fun env ->
        match r1 env with
        | Error error ->
            Error error
        | Ok () ->
            r2 env

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member __.TryWith (body : ReaderResultFunc<_,_,_>, handler : exn -> ReaderResultFunc<_,_,_>)
        : ReaderResultFunc<'Env, 'T, 'Error> =
        fun env ->
        try body env
        with ex ->
            handler ex env

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : ReaderResultFunc<_,_,_>, handler)
        : ReaderResultFunc<'Env, 'T, 'Error> =
        fun env ->
        try body env
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member __.Using (resource : ('T :> System.IDisposable), body : 'T -> ReaderResultFunc<_,_,_>)
        : ReaderResultFunc<'Env, 'U, 'Error> =
        fun env ->
        try body resource env
        finally
            if not <| isNull (box resource) then
                resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ReaderResultFunc<_,_,_>)
        : ReaderResultFunc<'Env, unit, 'Error>=
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ReaderResultFunc<_,_,_>)
        : ReaderResultFunc<'Env, unit, 'Error> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))


/// <summary>
/// </summary>
[<Sealed>]
type ProtectedStateBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : ProtectedStateFunc<'State, 'T, 'Error> =
        fun state ->
        Ok (value, state)

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : ProtectedStateFunc<'State, 'T, 'Error> =
        func

    // unit -> M<'T>
    member inline this.Zero ()
        : ProtectedStateFunc<'State, unit, 'Error> =
        this.Return ()

    // (unit -> M<'T>) -> M<'T>
    member __.Delay (generator : unit -> ProtectedStateFunc<_,_,_>)
        : ProtectedStateFunc<'State, 'T, 'Error> =
        fun state -> generator () state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : ProtectedStateFunc<_,_,_>, binder : 'T -> ProtectedStateFunc<_,_,_>)
        : ProtectedStateFunc<'State, 'U, 'Error> =
        fun state ->
        match m state with
        | Error error ->
            Error error
        | Ok (value, state) ->
            binder value state

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : ProtectedStateFunc<_,_,_>, r2 : ProtectedStateFunc<_,_,_>)
        : ProtectedStateFunc<'State, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member __.TryWith (body : ProtectedStateFunc<_,_,_>, handler : exn -> ProtectedStateFunc<_,_,_>)
        : ProtectedStateFunc<'State, 'T, 'Error> =
        fun state ->
        try body state
        with ex ->
            handler ex state

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : ProtectedStateFunc<_,_,_>, handler)
        : ProtectedStateFunc<'State, 'T, 'Error> =
        fun state ->
        try body state
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ProtectedStateFunc<_,_,_>)
        : ProtectedStateFunc<'State, 'U, 'Error> =
        fun state ->
        try
            body resource state
        finally
            if not <| isNull (box resource) then
                resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ProtectedStateFunc<_,_,_>)
        : ProtectedStateFunc<'State, unit, 'Error> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ProtectedStateFunc<_,_,_>)
        : ProtectedStateFunc<'State, unit, 'Error> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))


/// <summary>
/// </summary>
[<Sealed>]
type ReaderProtectedStateBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : ReaderProtectedStateFunc<'Env, 'State, 'T, 'Error> =
        fun _ state ->
        Ok (value, state)

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : ReaderProtectedStateFunc<'Env, 'State, 'T, 'Error> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : ReaderProtectedStateFunc<'Env, 'State, unit, 'Error> =
        fun _ state ->
        Ok ((), state)

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> ReaderProtectedStateFunc<_,_,_,_>)
        : ReaderProtectedStateFunc<'Env, 'State, 'T, 'Error> =
        fun env state -> f () env state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : ReaderProtectedStateFunc<_,_,_,_>, binder : 'T -> ReaderProtectedStateFunc<_,_,_,_>)
        : ReaderProtectedStateFunc<'Env, 'State, 'U, 'Error> =
        fun env state ->
        match m env state with
        | Error error ->
            Error error
        | Ok (value, state) ->
            binder value env state

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : ReaderProtectedStateFunc<_,_,_,_>, r2 : ReaderProtectedStateFunc<_,_,_,_>)
        : ReaderProtectedStateFunc<'Env, 'State, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member __.TryWith (body : ReaderProtectedStateFunc<_,_,_,_>, handler : exn -> ReaderProtectedStateFunc<_,_,_,_>)
        : ReaderProtectedStateFunc<'Env, 'State, 'T, 'Error> =
        fun env state ->
        try body env state
        with ex ->
            handler ex env state

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : ReaderProtectedStateFunc<_,_,_,_>, handler)
        : ReaderProtectedStateFunc<'Env, 'State, 'T, 'Error> =
        fun env state ->
        try body env state
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ReaderProtectedStateFunc<_,_,_,_>)
        : ReaderProtectedStateFunc<'Env, 'State, 'U, 'Error> =
        fun env state ->
        try
            body resource env state
        finally
            if not <| isNull (box resource) then
                resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ReaderProtectedStateFunc<_,_,_,_>)
        : ReaderProtectedStateFunc<'Env, 'State, unit, 'Error> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ReaderProtectedStateFunc<_,_,_,_>)
        : ReaderProtectedStateFunc<'Env, 'State, unit, 'Error> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))


/// <summary>
/// </summary>
[<Sealed>]
type StatefulResultBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : StatefulResultFunc<'State, 'T, 'Error> =
        fun state ->
        (Ok value), state

    // M<'T> -> M<'T>
    member __.ReturnFrom (func)
        : StatefulResultFunc<_,_,_> =
        func

    // unit -> M<'T>
    member inline this.Zero ()
        : StatefulResultFunc<'State, unit, 'Error> =
        this.Return ()

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (generator : unit -> StatefulResultFunc<_,_,_>)
        : StatefulResultFunc<'State, 'T, 'Error> =
        fun state -> generator () state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (computation : StatefulResultFunc<_,_,_>, binder : 'T -> StatefulResultFunc<_,_,_>)
        : StatefulResultFunc<'State, 'U, 'Error> =
        fun state ->
        match computation state with
        | (Ok value), state ->
            binder value state
        | (Error error), state ->
            (Error error), state

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : StatefulResultFunc<_,_,_>, r2 : StatefulResultFunc<_,_,_>)
        : StatefulResultFunc<'State, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member __.TryWith (body : StatefulResultFunc<_,_,_>, handler : exn -> StatefulResultFunc<_,_,_>)
        : StatefulResultFunc<'State, 'T, 'Error> =
        fun state ->
        try body state
        with ex ->
            handler ex state

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : StatefulResultFunc<_,_,_>, handler)
        : StatefulResultFunc<'State, 'T, 'Error> =
        fun state ->
        try body state
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> StatefulResultFunc<_,_,_>)
        : StatefulResultFunc<'State, 'U, 'Error> =
        fun state ->
        try
            body resource state
        finally
            if not <| isNull (box resource) then
                resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : StatefulResultFunc<_,_,_>)
        : StatefulResultFunc<'State, _, 'Error> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> StatefulResultFunc<_,_,_>)
        : StatefulResultFunc<'State, _, 'Error> =
        this.Using (sequence.GetEnumerator (),
            (fun enum ->
                this.While (
                    enum.MoveNext,
                    this.Delay (fun () ->
                        body enum.Current))))


/// <summary>
/// </summary>
[<Sealed>]
type AsyncResultBuilder () =
    // 'T -> M<'T>
    member (*inline*) __.Return value : Async<Result<'T, 'Error>> =
        Ok value
        |> async.Return

    // M<'T> -> M<'T>
    member (*inline*) __.ReturnFrom (asyncResult : Async<Result<'T, 'Error>>) =
        asyncResult

    // unit -> M<'T>
    member inline this.Zero () : Async<Result<unit, 'Error>> =
        this.Return ()

    // (unit -> M<'T>) -> M<'T>
    member inline this.Delay (generator : unit -> Async<Result<'T, 'Error>>) : Async<Result<'T, 'Error>> =
        async.Delay generator

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member (*inline*) __.Combine (r1, r2) : Async<Result<'T, 'Error>> =
        async {
        let! r1' = r1
        match r1' with
        | Error error ->
            return Error error
        | Ok () ->
            return! r2
        }

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member (*inline*) __.Bind (value : Async<Result<'T, 'Error>>, binder : 'T -> Async<Result<'U, 'Error>>)
        : Async<Result<'U, 'Error>> =
        async {
        let! value' = value
        match value' with
        | Error error ->
            return Error error
        | Ok x ->
            return! binder x
        }

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member inline __.TryWith (computation : Async<Result<'T, 'Error>>, catchHandler : exn -> Async<Result<'T, 'Error>>)
        : Async<Result<'T, 'Error>> =
        async.TryWith(computation, catchHandler)

    // M<'T> * (unit -> unit) -> M<'T>
    member inline __.TryFinally (computation : Async<Result<'T, 'Error>>, compensation : unit -> unit)
        : Async<Result<'T, 'Error>> =
        async.TryFinally (computation, compensation)

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member inline __.Using (resource : ('T :> System.IDisposable), binder : _ -> Async<Result<'U, 'Error>>)
        : Async<Result<'U, 'Error>> =
        async.Using (resource, binder)

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : Async<Result<unit, 'Error>>) : Async<Result<_,_>> =
        if guard () then
            // OPTIMIZE : This could be simplified so we don't need to make calls to Bind and While.
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> Async<Result<unit, 'Error>>) =
        // OPTIMIZE : This could be simplified so we don't need to make calls to Using, While, Delay.
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))


/// <summary>
/// </summary>
[<Sealed>]
type AsyncReaderResultBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : AsyncReaderResultFunc<'Env, 'T, 'Error> =
        fun _ ->
            async.Return (Ok value)

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : AsyncReaderResultFunc<'Env, 'T, 'Error> =
        func

    // unit -> M<'T>
    member inline this.Zero ()
        : AsyncReaderResultFunc<'Env, unit, 'Error> =
        this.Return ()

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (generator : unit -> AsyncReaderResultFunc<_,_,_>)
        : AsyncReaderResultFunc<'Env, 'T, 'Error> =
        fun env -> generator () env

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : AsyncReaderResultFunc<_,_,_>, k : 'T -> AsyncReaderResultFunc<_,_,_>)
        : AsyncReaderResultFunc<'Env, 'U, 'Error> =
        fun env ->
            async {
            let! result = m env
            match result with
            | Error error ->
                return Error error
            | Ok value ->
                return! k value env
            }

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : AsyncReaderResultFunc<_,_,_>, r2 : AsyncReaderResultFunc<_,_,_>)
        : AsyncReaderResultFunc<'Env, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member __.TryWith (body : AsyncReaderResultFunc<_,_,_>, handler : exn -> AsyncReaderResultFunc<_,_,_>)
        : AsyncReaderResultFunc<'Env, 'T, 'Error> =
        fun env ->
            async.TryWith (
                async.Delay (fun () -> body env),
                fun ex ->
                    async.Delay (fun () -> handler ex env))

    // M<'T> * (unit -> unit) -> M<'T>
    member this.TryFinally (body : AsyncReaderResultFunc<_,_,_>, handler)
        : AsyncReaderResultFunc<'Env, 'T, 'Error> =
        fun env ->
            async.TryFinally (
                async.Delay (fun () -> body env),
                handler)

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> AsyncReaderResultFunc<_,_,_>)
        : AsyncReaderResultFunc<'Env, 'U, 'Error> =
        this.TryFinally (
            this.Delay (fun () ->
                body resource),
            fun () ->
                if not <| isNull (box resource) then
                    resource.Dispose ())

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : AsyncReaderResultFunc<_,_,_>)
        : AsyncReaderResultFunc<'Env, unit, 'Error> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> AsyncReaderResultFunc<_,_,_>)
        : AsyncReaderResultFunc<'Env, unit, 'Error> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))

/// <summary>
/// </summary>
[<Sealed>]
type AsyncProtectedStateBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : AsyncProtectedStateFunc<'State, 'T, 'Error> =
        fun state ->
            Ok (value, state)
            |> async.Return

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : AsyncProtectedStateFunc<'State, 'T, 'Error> =
        func

    // unit -> M<'T>
    member inline this.Zero ()
        : AsyncProtectedStateFunc<'State, unit, 'Error> =
        this.Return ()

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (generator : unit -> AsyncProtectedStateFunc<_,_,_>)
        : AsyncProtectedStateFunc<'State, 'T, 'Error> =
        fun state -> generator () state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : AsyncProtectedStateFunc<_,_,_>, k : 'T -> AsyncProtectedStateFunc<_,_,_>)
        : AsyncProtectedStateFunc<'State, 'U, 'Error> =
        fun state ->
            async {
            let! result = m state
            match result with
            | Error error ->
                return Error error
            | Ok (value, state) ->
                return! k value state
            }

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : AsyncProtectedStateFunc<_,_,_>, r2 : AsyncProtectedStateFunc<_,_,_>)
        : AsyncProtectedStateFunc<'State, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member __.TryWith (body : AsyncProtectedStateFunc<_,_,_>, handler : exn -> AsyncProtectedStateFunc<_,_,_>)
        : AsyncProtectedStateFunc<'State, 'T, 'Error> =
        fun state ->
            async.TryWith (
                async.Delay (fun () -> body state),
                fun ex ->
                    async.Delay (fun () -> handler ex state))

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : AsyncProtectedStateFunc<_,_,_>, handler)
        : AsyncProtectedStateFunc<'State, 'T, 'Error> =
        fun state ->
            async.TryFinally (
                async.Delay (fun () -> body state),
                handler)

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> AsyncProtectedStateFunc<_,_,_>)
        : AsyncProtectedStateFunc<'State, 'U, 'Error> =
        this.TryFinally (
            this.Delay (fun () ->
                body resource),
            fun () ->
                if not <| isNull (box resource) then
                    resource.Dispose ())

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : AsyncProtectedStateFunc<_,_,_>)
        : AsyncProtectedStateFunc<'State, unit, 'Error> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> AsyncProtectedStateFunc<_,_,_>)
        : AsyncProtectedStateFunc<'State, unit, 'Error> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))

/// <summary>
/// </summary>
[<Sealed>]
type AsyncStatefulResultBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : AsyncStatefulResultFunc<'State, 'T, 'Error> =
        fun state ->
            async.Return (Ok value, state)

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : AsyncStatefulResultFunc<'State, 'T, 'Error> =
        func

    // unit -> M<'T>
    member inline this.Zero ()
        : AsyncStatefulResultFunc<'State, unit, 'Error> =
        this.Return ()

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (generator : unit -> AsyncStatefulResultFunc<_,_,_>)
        : AsyncStatefulResultFunc<'State, 'T, 'Error> =
        fun state -> generator () state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : AsyncStatefulResultFunc<_,_,_>, k : 'T -> AsyncStatefulResultFunc<_,_,_>)
        : AsyncStatefulResultFunc<'State, 'U, 'Error> =
        fun state ->
            async {
            let! result, state = m state
            match result with
            | Error error ->
                return (Error error, state)
            | Ok value ->
                return! k value state
            }

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : AsyncStatefulResultFunc<_,_,_>, r2 : AsyncStatefulResultFunc<_,_,_>)
        : AsyncStatefulResultFunc<'State, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member __.TryWith (body : AsyncStatefulResultFunc<_,_,_>, handler : exn -> AsyncStatefulResultFunc<_,_,_>)
        : AsyncStatefulResultFunc<'State, 'T, 'Error> =
        fun state ->
            async.TryWith (
                async.Delay (fun () -> body state),
                fun ex ->
                    async.Delay (fun () ->handler ex state))

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : AsyncStatefulResultFunc<_,_,_>, handler)
        : AsyncStatefulResultFunc<'State, 'T, 'Error> =
        fun state ->
            async.TryFinally (
                async.Delay (fun () -> body state),
                handler)

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> AsyncStatefulResultFunc<_,_,_>)
        : AsyncStatefulResultFunc<'State, 'U, 'Error> =
        this.TryFinally (
            this.Delay (fun () ->
                body resource),
            fun () ->
                if not <| isNull (box resource) then
                    resource.Dispose ())

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : AsyncStatefulResultFunc<_,_,_>)
        : AsyncStatefulResultFunc<'State, unit, 'Error> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> AsyncStatefulResultFunc<_,_,_>)
        : AsyncStatefulResultFunc<'State, unit, 'Error> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))




/// <summary>
/// </summary>
[<AutoOpen>]
module ResultWorkflowBuilders =
    //
    [<CompiledName("Result")>]
    let result = ResultBuilder ()
    //
    [<CompiledName("ReaderResult")>]
    let readerResult = ReaderResultBuilder ()
    //
    [<CompiledName("ProtectedState")>]
    let protectedState = ProtectedStateBuilder ()
    //
    [<CompiledName("ReaderProtectedState")>]
    let readerProtectedState = ReaderProtectedStateBuilder ()
    //
    [<CompiledName("StatefulResult")>]
    let statefulResult = StatefulResultBuilder ()
    //
    [<CompiledName("AsyncResult")>]
    let asyncResult = AsyncResultBuilder ()
    //
    [<CompiledName("AsyncReaderResult")>]
    let asyncReaderResult = AsyncReaderResultBuilder ()
    //
    [<CompiledName("AsyncProtectedState")>]
    let asyncProtectedState = AsyncProtectedStateBuilder ()
    //
    [<CompiledName("AsyncStatefulResult")>]
    let asyncStatefulResult = AsyncStatefulResultBuilder ()


(*** Workflow helper modules ***)

/// <summary>
/// </summary>
[<RequireQualifiedAccess>]
module ProtectedState =
    //
    [<CompiledName("LiftState")>]
    let inline liftState (stateFunc : StateFunc<'State, 'T>)
        : ProtectedStateFunc<'State, 'T, 'Error> =
        fun state ->
            Ok (stateFunc state)

    //
    [<CompiledName("LiftResult")>]
    let inline liftResult (choice : Result<'T, 'Error>)
        : ProtectedStateFunc<'State, 'T, 'Error> =
        match choice with
        | Error error ->
            fun _ -> Error error
        | Ok value ->
            fun (state : 'State) ->
            Ok (value, state)

    /// Adapts a function designed for use with the Reader monad
    /// so it can be used with the ProtectedState monad.
    /// Used for functions which only need to read from the state.
    [<CompiledName("LiftReader")>]
    let inline liftReader (readerFunc : ReaderFunc<'Env, 'T>)
        : ProtectedStateFunc<'Env, 'T, 'Error> =
        fun env ->
            let result = readerFunc env
            Ok (result, env)

    //
    [<CompiledName("LiftReaderResult")>]
    let inline liftReaderResult (readerResultFunc : 'State -> Result<'T, 'Error>)
        : ProtectedStateFunc<'State, 'T, 'Error> =
        fun state ->
            match readerResultFunc state with
            | Error error ->
                Error error
            | Ok result ->
                Ok (result, state)

    //
    [<CompiledName("SetState")>]
    let inline setState (state : 'State) : Result<unit * 'State, 'Error> =
        Ok ((), state)

    //
    [<CompiledName("GetState")>]
    let inline getState (state : 'State) : Result<'State * 'State, 'Error> =
        Ok (state, state)

    /// Sets an error value in the computation. The monadic equivalent of raising an exception.
    [<CompiledName("SetError")>]
    let inline setError error (_ : 'State) : Result<'T * 'State, 'Error> =
        Error error

    /// The monadic equivalent of F#'s built-in 'failwith' operator.
    [<CompiledName("Failwith")>]
    let inline failwith (errorMsg : string) (_ : 'State) : Result<'T * 'State, string> =
        Error errorMsg

    /// Discards the state value.
    /// Useful when the state value is only needed during the computation;
    /// by discarding the state when the computation is complete, the return
    /// value can be adapted to the Choice workflow.
    [<CompiledName("DiscardState")>]
    let inline discardState (protectedStateFunc : ProtectedStateFunc<'State, 'T, 'Error>) =
        fun state ->
            match protectedStateFunc state with
            | Error error ->
                Error error
            | Ok (result, _) ->
                Ok result

/// <summary>
/// </summary>
[<RequireQualifiedAccess>]
module StatefulResult =
    //
    [<CompiledName("LiftState")>]
    let liftState (stateFunc : StateFunc<'State, 'T>) : StatefulResultFunc<'State, 'T, 'Error> =
        fun state ->
        let value, state = stateFunc state
        (Ok value), state

    //
    [<CompiledName("LiftResult")>]
    let inline liftResult (choice : Result<'T, 'Error>) : StatefulResultFunc<'State, 'T, 'Error> =
        fun state ->
        choice, state

    //
    [<CompiledName("SetState")>]
    let setState (state : 'State) : StatefulResultFunc<_,_,'Error> =
        fun _ ->
        (Ok ()), state

    //
    [<CompiledName("GetState")>]
    let inline getState (state : 'State) =
        (Ok state), state

    let private ``return`` value =
        fun state ->
        (Ok value), state

    //
    let private bind k m =
        fun state ->
        match m state with
        | (Ok value), state ->
            k value state
        | (Error error), state ->
            (Error error), state

    /// Transforms a value in the StatefulChoice workflow by using a specified mapping function.
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'U) (m : StatefulResultFunc<'State, 'T, 'Error>)
            : StatefulResultFunc<'State, 'U, 'Error> =
        bind (mapping >> ``return``) m

    //
    [<CompiledName("Attempt")>]
    let attempt (generator : unit -> 'T) : StatefulResultFunc<'State, 'T, exn> =
        statefulResult {
        let! state = getState
        return! fun _ -> Result.attempt generator, state
        }

    //
    [<CompiledName("MapError")>]
    let mapError (map : 'Error1 -> 'Error2) (value : StatefulResultFunc<'State, 'T, 'Error1>)
            : StatefulResultFunc<'State, 'T, 'Error2> =
        statefulResult {
        let! state = getState
        let choice, state' = value state
        return!
            match choice with
            | Ok c -> fun _ -> Ok c, state'
            | Error error -> fun _ -> Error (map error), state'
        }



/// Functions for working with AsyncChoice workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AsyncResult =
    open Microsoft.FSharp.Control

    /// Creates an AsyncChoice from an error value.
    [<CompiledName("Error")>]
    let inline error value : AsyncResult<'T, 'Error> =
        async.Return (Error value)

    /// Creates an AsyncChoice representing an error value.
    /// The error value in the Choice is the specified error message.
    [<CompiledName("FailWith")>]
    let inline failwith errorMsg : AsyncResult<'T, string> =
        async.Return (Error errorMsg)

    /// <summary>
    /// When the choice value is <c>Ok(x)</c>, returns <c>Ok (f x)</c>.
    /// Otherwise, when the choice value is <c>Error(x)</c>, returns <c>Error(x)</c>.
    /// </summary>
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'U) (value : AsyncResult<'T, 'Error>) : AsyncResult<'U, 'Error> =
        async {
        // Get the input value.
        let! x = value

        // Apply the mapping function and return the result.
        match x with
        | Ok result ->
            return Ok (mapping result)
        | Error error ->
            return (Error error)
        }

    /// <summary>
    /// When the choice value is <c>Choice1Of2(x)</c>, returns <c>Choice1Of2 (f x)</c>.
    /// Otherwise, when the choice value is <c>Choice2Of2(x)</c>, returns <c>Choice2Of2(x)</c>.
    /// </summary>
    [<CompiledName("MapAsync")>]
    let mapAsync (mapping : 'T -> Async<'U>) (value : AsyncResult<'T, 'Error>) : AsyncResult<'U, 'Error> =
        async {
        // Get the input value.
        let! x = value

        // Apply the mapping function and return the result.
        match x with
        | Ok result ->
            let! mappedResult = mapping result
            return Ok mappedResult
        | Error error ->
            return (Error error)
        }
