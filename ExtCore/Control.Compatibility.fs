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

namespace ExtCore.Control.Compatibility

open ExtCore
open ExtCore.Control
open ExtCore.Compatibility

(*** Workflow Monoids ***)

/// <summary>
/// </summary>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type ProtectedStateFunc<'State, 'T, 'Error> =
    'State -> Choice<'T * 'State, 'Error>

/// <summary>
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type ReaderProtectedStateFunc<'Env, 'State, 'T, 'Error> =
    'Env -> 'State -> Choice<'T * 'State, 'Error>


/// <summary>
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type ReaderChoiceFunc<'Env, 'T, 'Error> =
    'Env -> Choice<'T, 'Error>


/// <summary>
/// </summary>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type StatefulChoiceFunc<'State, 'T, 'Error> =
    'State -> Choice<'T, 'Error> * 'State

/// <summary>
/// </summary>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type AsyncChoice<'T, 'Error> =
    Async<Choice<'T, 'Error>>

/// <summary>
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type AsyncReaderChoiceFunc<'Env, 'T, 'Error> =
    'Env -> Async<Choice<'T, 'Error>>

/// <summary>
/// </summary>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type AsyncProtectedStateFunc<'State, 'T, 'Error> =
    'State -> Async<Choice<'T * 'State, 'Error>>

/// <summary>
/// </summary>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type AsyncStatefulChoiceFunc<'State, 'T, 'Error> =
    'State -> Async<Choice<'T, 'Error> * 'State>

(*** Workflow Builders ***)



/// <summary>
/// </summary>
[<Sealed>]
type ChoiceBuilder () =
    /// The zero value for this builder never changes and is immutable,
    /// so create and reuse a single instance of it to avoid unnecessary allocations.
    static let zero = Choice1Of2 ()

    // 'T -> M<'T>
    member __.Return value : Choice<'T, 'Error> =
        Choice1Of2 value

    // Error operation. Similar to the Return method ('return'), but used for returning an error value.
    [<CustomOperation("error")>]
    member __.Error value : Choice<'T, 'Error> =
        Choice2Of2 value

    // M<'T> -> M<'T>
    member __.ReturnFrom (m : Choice<'T, 'Error>) =
        m

    // unit -> M<'T>
    member __.Zero () : Choice<unit, 'Error> =
        zero

    // (unit -> M<'T>) -> M<'T>
    member __.Delay (generator : unit -> Choice<'T, 'Error>) : unit -> Choice<'T, 'Error> =
        generator

    //
    member __.Run (generator : unit -> Choice<'T, 'Error>) : Choice<'T, 'Error> =
        generator ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (value, binder : 'T -> Choice<'U, 'Error>) : Choice<'U, 'Error> =
        match value with
        | Choice2Of2 error ->
            Choice2Of2 error
        | Choice1Of2 x ->
            binder x

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline __.Combine (r1, r2) : Choice<'T, 'Error> =
        match r1 with
        | Choice2Of2 error ->
            Choice2Of2 error
        | Choice1Of2 () ->
            r2

    //
    member __.Combine (r1 : Choice<'T, 'Error>, r2) : Choice<'U, 'Error> =
        Choice.bind r2 r1

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member inline __.TryWith (body : unit -> Choice<'T, 'Error>, handler) =
        try body ()
        with ex ->
            handler ex

    // M<'T> * (unit -> unit) -> M<'T>
    member inline __.TryFinally (body : unit -> Choice<'T, 'Error>, handler) =
        try body ()
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : _ -> Choice<_,_>)
        : Choice<'U, 'Error> =
        try body resource
        finally
            if not <| isNull (box resource) then
                resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : unit -> Choice<unit, 'Error>) : Choice<_,_> =
        if guard () then
            match body () with
            | Choice1Of2 () ->
                this.While (guard, body)
            | err -> err
        else
            // Return Choice1Of2 () to indicate success when the loop
            // finishes normally (because the guard returned false).
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> Choice<unit, 'Error>) =
        use enumerator = sequence.GetEnumerator ()

        let mutable errorResult = Unchecked.defaultof<_>
        while enumerator.MoveNext () && isNull errorResult do
            match body enumerator.Current with
            | Choice1Of2 () -> ()
            | error ->
                errorResult <- error

        // If we broke out of the loop early because the 'body' function
        // returned an error for some element, return the error.
        // Otherwise, return the 'zero' value (representing a 'success' which carries no value).
        if isNull errorResult then this.Zero () else errorResult

/// <summary>
/// </summary>
[<Sealed>]
type ReaderChoiceBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : ReaderChoiceFunc<'Env, 'T, 'Error> =
        fun _ -> Choice1Of2 value

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : ReaderChoiceFunc<'Env, 'T, 'Error> =
        func

    // unit -> M<'T>
    member __.Zero ()
        : ReaderChoiceFunc<'Env, unit, 'Error> =
        fun _ -> Choice1Of2 ()

    // (unit -> M<'T>) -> M<'T>
    member __.Delay (generator : unit -> ReaderChoiceFunc<'Env, 'T, 'Error>)
        : ReaderChoiceFunc<'Env, 'T, 'Error> =
        generator ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (f : ReaderChoiceFunc<_,_,_>, binder : 'T -> ReaderChoiceFunc<_,_,_>)
        : ReaderChoiceFunc<'Env, 'U, 'Error> =
        fun env ->
        match f env with
        | Choice2Of2 error ->
            Choice2Of2 error
        | Choice1Of2 result ->
            binder result env

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member __.Combine (r1 : ReaderChoiceFunc<_,_,_>, r2 : ReaderChoiceFunc<_,_,_>)
        : ReaderChoiceFunc<'Env, 'T, 'Error> =
        fun env ->
        match r1 env with
        | Choice2Of2 error ->
            Choice2Of2 error
        | Choice1Of2 () ->
            r2 env

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member __.TryWith (body : ReaderChoiceFunc<_,_,_>, handler : exn -> ReaderChoiceFunc<_,_,_>)
        : ReaderChoiceFunc<'Env, 'T, 'Error> =
        fun env ->
        try body env
        with ex ->
            handler ex env

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : ReaderChoiceFunc<_,_,_>, handler)
        : ReaderChoiceFunc<'Env, 'T, 'Error> =
        fun env ->
        try body env
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member __.Using (resource : ('T :> System.IDisposable), body : 'T -> ReaderChoiceFunc<_,_,_>)
        : ReaderChoiceFunc<'Env, 'U, 'Error> =
        fun env ->
        try body resource env
        finally
            if not <| isNull (box resource) then
                resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ReaderChoiceFunc<_,_,_>)
        : ReaderChoiceFunc<'Env, unit, 'Error>=
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ReaderChoiceFunc<_,_,_>)
        : ReaderChoiceFunc<'Env, unit, 'Error> =
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
        Choice1Of2 (value, state)

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
        | Choice2Of2 error ->
            Choice2Of2 error
        | Choice1Of2 (value, state) ->
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
        Choice1Of2 (value, state)

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : ReaderProtectedStateFunc<'Env, 'State, 'T, 'Error> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : ReaderProtectedStateFunc<'Env, 'State, unit, 'Error> =
        fun _ state ->
        Choice1Of2 ((), state)

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> ReaderProtectedStateFunc<_,_,_,_>)
        : ReaderProtectedStateFunc<'Env, 'State, 'T, 'Error> =
        fun env state -> f () env state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : ReaderProtectedStateFunc<_,_,_,_>, binder : 'T -> ReaderProtectedStateFunc<_,_,_,_>)
        : ReaderProtectedStateFunc<'Env, 'State, 'U, 'Error> =
        fun env state ->
        match m env state with
        | Choice2Of2 error ->
            Choice2Of2 error
        | Choice1Of2 (value, state) ->
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
type StatefulChoiceBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : StatefulChoiceFunc<'State, 'T, 'Error> =
        fun state ->
        (Choice1Of2 value), state

    // M<'T> -> M<'T>
    member __.ReturnFrom (func)
        : StatefulChoiceFunc<_,_,_> =
        func

    // unit -> M<'T>
    member inline this.Zero ()
        : StatefulChoiceFunc<'State, unit, 'Error> =
        this.Return ()

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (generator : unit -> StatefulChoiceFunc<_,_,_>)
        : StatefulChoiceFunc<'State, 'T, 'Error> =
        fun state -> generator () state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (computation : StatefulChoiceFunc<_,_,_>, binder : 'T -> StatefulChoiceFunc<_,_,_>)
        : StatefulChoiceFunc<'State, 'U, 'Error> =
        fun state ->
        match computation state with
        | (Choice1Of2 value), state ->
            binder value state
        | (Choice2Of2 error), state ->
            (Choice2Of2 error), state    
        
    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : StatefulChoiceFunc<_,_,_>, r2 : StatefulChoiceFunc<_,_,_>)
        : StatefulChoiceFunc<'State, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member __.TryWith (body : StatefulChoiceFunc<_,_,_>, handler : exn -> StatefulChoiceFunc<_,_,_>)
        : StatefulChoiceFunc<'State, 'T, 'Error> =
        fun state ->
        try body state
        with ex ->
            handler ex state

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : StatefulChoiceFunc<_,_,_>, handler)
        : StatefulChoiceFunc<'State, 'T, 'Error> =
        fun state ->
        try body state
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> StatefulChoiceFunc<_,_,_>)
        : StatefulChoiceFunc<'State, 'U, 'Error> =
        fun state ->
        try
            body resource state
        finally
            if not <| isNull (box resource) then
                resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : StatefulChoiceFunc<_,_,_>)
        : StatefulChoiceFunc<'State, _, 'Error> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> StatefulChoiceFunc<_,_,_>)
        : StatefulChoiceFunc<'State, _, 'Error> =
        this.Using (sequence.GetEnumerator (),
            (fun enum ->
                this.While (
                    enum.MoveNext,
                    this.Delay (fun () ->
                        body enum.Current))))


/// <summary>
/// </summary>
[<Sealed>]
type AsyncReaderChoiceBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : AsyncReaderChoiceFunc<'Env, 'T, 'Error> =
        fun _ ->
            async.Return (Choice1Of2 value)

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : AsyncReaderChoiceFunc<'Env, 'T, 'Error> =
        func

    // unit -> M<'T>
    member inline this.Zero ()
        : AsyncReaderChoiceFunc<'Env, unit, 'Error> =
        this.Return ()

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (generator : unit -> AsyncReaderChoiceFunc<_,_,_>)
        : AsyncReaderChoiceFunc<'Env, 'T, 'Error> =
        fun env -> generator () env

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : AsyncReaderChoiceFunc<_,_,_>, k : 'T -> AsyncReaderChoiceFunc<_,_,_>)
        : AsyncReaderChoiceFunc<'Env, 'U, 'Error> =
        fun env ->
            async {
            let! result = m env
            match result with
            | Choice2Of2 error ->
                return Choice2Of2 error
            | Choice1Of2 value ->
                return! k value env
            }

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : AsyncReaderChoiceFunc<_,_,_>, r2 : AsyncReaderChoiceFunc<_,_,_>)
        : AsyncReaderChoiceFunc<'Env, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member __.TryWith (body : AsyncReaderChoiceFunc<_,_,_>, handler : exn -> AsyncReaderChoiceFunc<_,_,_>)
        : AsyncReaderChoiceFunc<'Env, 'T, 'Error> =
        fun env ->
            async.TryWith (
                async.Delay (fun () -> body env),
                fun ex ->
                    async.Delay (fun () -> handler ex env))

    // M<'T> * (unit -> unit) -> M<'T>
    member this.TryFinally (body : AsyncReaderChoiceFunc<_,_,_>, handler)
        : AsyncReaderChoiceFunc<'Env, 'T, 'Error> =
        fun env ->
            async.TryFinally (
                async.Delay (fun () -> body env),
                handler)

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> AsyncReaderChoiceFunc<_,_,_>)
        : AsyncReaderChoiceFunc<'Env, 'U, 'Error> =
        this.TryFinally (
            this.Delay (fun () ->
                body resource),
            fun () ->
                if not <| isNull (box resource) then
                    resource.Dispose ())

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : AsyncReaderChoiceFunc<_,_,_>)
        : AsyncReaderChoiceFunc<'Env, unit, 'Error> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> AsyncReaderChoiceFunc<_,_,_>)
        : AsyncReaderChoiceFunc<'Env, unit, 'Error> =
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
            Choice1Of2 (value, state)
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
            | Choice2Of2 error ->
                return Choice2Of2 error
            | Choice1Of2 (value, state) ->
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
type AsyncChoiceBuilder () =
    // 'T -> M<'T>
    member (*inline*) __.Return value : Async<Choice<'T, 'Error>> =
        Choice1Of2 value
        |> async.Return

    // M<'T> -> M<'T>
    member (*inline*) __.ReturnFrom (asyncChoice : Async<Choice<'T, 'Error>>) =
        asyncChoice

    // unit -> M<'T>
    member inline this.Zero () : Async<Choice<unit, 'Error>> =
        this.Return ()

    // (unit -> M<'T>) -> M<'T>
    member inline this.Delay (generator : unit -> Async<Choice<'T, 'Error>>) : Async<Choice<'T, 'Error>> =
        async.Delay generator

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member (*inline*) __.Combine (r1, r2) : Async<Choice<'T, 'Error>> =
        async {
        let! r1' = r1
        match r1' with
        | Choice2Of2 error ->
            return Choice2Of2 error
        | Choice1Of2 () ->
            return! r2
        }

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member (*inline*) __.Bind (value : Async<Choice<'T, 'Error>>, binder : 'T -> Async<Choice<'U, 'Error>>)
        : Async<Choice<'U, 'Error>> =
        async {
        let! value' = value
        match value' with
        | Choice2Of2 error ->
            return Choice2Of2 error
        | Choice1Of2 x ->
            return! binder x
        }

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member inline __.TryWith (computation : Async<Choice<'T, 'Error>>, catchHandler : exn -> Async<Choice<'T, 'Error>>)
        : Async<Choice<'T, 'Error>> =
        async.TryWith(computation, catchHandler)

    // M<'T> * (unit -> unit) -> M<'T>
    member inline __.TryFinally (computation : Async<Choice<'T, 'Error>>, compensation : unit -> unit)
        : Async<Choice<'T, 'Error>> =
        async.TryFinally (computation, compensation)

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member inline __.Using (resource : ('T :> System.IDisposable), binder : _ -> Async<Choice<'U, 'Error>>)
        : Async<Choice<'U, 'Error>> =
        async.Using (resource, binder)

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : Async<Choice<unit, 'Error>>) : Async<Choice<_,_>> =
        if guard () then
            // OPTIMIZE : This could be simplified so we don't need to make calls to Bind and While.
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> Async<Choice<unit, 'Error>>) =
        // OPTIMIZE : This could be simplified so we don't need to make calls to Using, While, Delay.
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))


/// <summary>
/// </summary>
[<Sealed>]
type AsyncStatefulChoiceBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : AsyncStatefulChoiceFunc<'State, 'T, 'Error> =
        fun state ->
            async.Return (Choice1Of2 value, state)

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : AsyncStatefulChoiceFunc<'State, 'T, 'Error> =
        func

    // unit -> M<'T>
    member inline this.Zero ()
        : AsyncStatefulChoiceFunc<'State, unit, 'Error> =
        this.Return ()

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (generator : unit -> AsyncStatefulChoiceFunc<_,_,_>)
        : AsyncStatefulChoiceFunc<'State, 'T, 'Error> =
        fun state -> generator () state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : AsyncStatefulChoiceFunc<_,_,_>, k : 'T -> AsyncStatefulChoiceFunc<_,_,_>)
        : AsyncStatefulChoiceFunc<'State, 'U, 'Error> =
        fun state ->
            async {
            let! result, state = m state
            match result with
            | Choice2Of2 error ->
                return (Choice2Of2 error, state)
            | Choice1Of2 value ->
                return! k value state
            }

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : AsyncStatefulChoiceFunc<_,_,_>, r2 : AsyncStatefulChoiceFunc<_,_,_>)
        : AsyncStatefulChoiceFunc<'State, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member __.TryWith (body : AsyncStatefulChoiceFunc<_,_,_>, handler : exn -> AsyncStatefulChoiceFunc<_,_,_>)
        : AsyncStatefulChoiceFunc<'State, 'T, 'Error> =
        fun state ->
            async.TryWith (
                async.Delay (fun () -> body state),
                fun ex ->
                    async.Delay (fun () ->handler ex state))

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : AsyncStatefulChoiceFunc<_,_,_>, handler)
        : AsyncStatefulChoiceFunc<'State, 'T, 'Error> =
        fun state ->
            async.TryFinally (
                async.Delay (fun () -> body state),
                handler)

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> AsyncStatefulChoiceFunc<_,_,_>)
        : AsyncStatefulChoiceFunc<'State, 'U, 'Error> =
        this.TryFinally (
            this.Delay (fun () ->
                body resource),
            fun () ->
                if not <| isNull (box resource) then
                    resource.Dispose ())

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : AsyncStatefulChoiceFunc<_,_,_>)
        : AsyncStatefulChoiceFunc<'State, unit, 'Error> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> AsyncStatefulChoiceFunc<_,_,_>)
        : AsyncStatefulChoiceFunc<'State, unit, 'Error> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))

/// <summary>
/// </summary>
[<AutoOpen>]
module WorkflowBuilders =
    //
    [<CompiledName("Choice")>]
    let choice = ChoiceBuilder ()
    //
    [<CompiledName("ReaderChoice")>]
    let readerChoice = ReaderChoiceBuilder ()
    //
    [<CompiledName("ProtectedState")>]
    let protectedState = ProtectedStateBuilder ()
    //
    [<CompiledName("ReaderProtectedState")>]
    let readerProtectedState = ReaderProtectedStateBuilder ()
    //
    [<CompiledName("StatefulChoice")>]
    let statefulChoice = StatefulChoiceBuilder ()
    //
    [<CompiledName("AsyncChoice")>]
    let asyncChoice = AsyncChoiceBuilder ()
    //
    [<CompiledName("AsyncReaderChoice")>]
    let asyncReaderChoice = AsyncReaderChoiceBuilder ()
    //
    [<CompiledName("AsyncProtectedState")>]
    let asyncProtectedState = AsyncProtectedStateBuilder ()
    //
    [<CompiledName("AsyncStatefulChoice")>]
    let asyncStatefulChoice = AsyncStatefulChoiceBuilder ()


(*** Workflow helper modules ***)

/// <summary>
/// </summary>
[<RequireQualifiedAccess>]
module State =

    /// Adapts a ProtectedState function for use within a State workflow.
    /// If the ProtectedState function returns an exception instance when executed,
    /// the exception will be raised rather than being passed into the State workflow.
    [<CompiledName("BindChoice")>]
    let inline bindChoice (k : 'T -> StateFunc<'State, 'U>) (m : ProtectedStateFunc<_,_,_>) =
        fun state ->
            match m state with
            | Choice2Of2 ex ->
                raise ex
            | Choice1Of2 (value, state) ->
                k value state


/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ReaderChoice =
    //
    [<CompiledName("LiftReader")>]
    let inline liftReader (readerFunc : ReaderFunc<'Env, 'T>) : ReaderChoiceFunc<'Env, 'T, 'Error> =
        fun env ->
            Choice1Of2 (readerFunc env)

    //
    [<CompiledName("LiftChoice")>]
    let inline liftChoice (choice : Choice<'T, 'Error>) : ReaderChoiceFunc<'Env, 'T, 'Error> =
        fun _ -> choice


/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ProtectedState =
    //
    [<CompiledName("LiftState")>]
    let inline liftState (stateFunc : StateFunc<'State, 'T>)
        : ProtectedStateFunc<'State, 'T, 'Error> =
        fun state ->
            Choice1Of2 (stateFunc state)

    //
    [<CompiledName("LiftChoice")>]
    let inline liftChoice (choice : Choice<'T, 'Error>)
        : ProtectedStateFunc<'State, 'T, 'Error> =
        match choice with
        | Choice2Of2 error ->
            fun _ -> Choice2Of2 error
        | Choice1Of2 value ->
            fun (state : 'State) ->
            Choice1Of2 (value, state)

    /// Adapts a function designed for use with the Reader monad
    /// so it can be used with the ProtectedState monad.
    /// Used for functions which only need to read from the state.
    [<CompiledName("LiftReader")>]
    let inline liftReader (readerFunc : ReaderFunc<'Env, 'T>)
        : ProtectedStateFunc<'Env, 'T, 'Error> =
        fun env ->
            let result = readerFunc env
            Choice1Of2 (result, env)

    //
    [<CompiledName("LiftReaderChoice")>]
    let inline liftReaderChoice (readerChoiceFunc : 'State -> Choice<'T, 'Error>)
        : ProtectedStateFunc<'State, 'T, 'Error> =
        fun state ->
            match readerChoiceFunc state with
            | Choice2Of2 error ->
                Choice2Of2 error
            | Choice1Of2 result ->
                Choice1Of2 (result, state)

    //
    [<CompiledName("SetState")>]
    let inline setState (state : 'State) : Choice<unit * 'State, 'Error> =
        Choice1Of2 ((), state)

    //
    [<CompiledName("GetState")>]
    let inline getState (state : 'State) : Choice<'State * 'State, 'Error> =
        Choice1Of2 (state, state)

    /// Sets an error value in the computation. The monadic equivalent of raising an exception.
    [<CompiledName("SetError")>]
    let inline setError error (_ : 'State) : Choice<'T * 'State, 'Error> =
        Choice2Of2 error

    /// The monadic equivalent of F#'s built-in 'failwith' operator.
    [<CompiledName("Failwith")>]
    let inline failwith (errorMsg : string) (_ : 'State) : Choice<'T * 'State, string> =
        Choice2Of2 errorMsg

    /// Discards the state value.
    /// Useful when the state value is only needed during the computation;
    /// by discarding the state when the computation is complete, the return
    /// value can be adapted to the Choice workflow.
    [<CompiledName("DiscardState")>]
    let inline discardState (protectedStateFunc : ProtectedStateFunc<'State, 'T, 'Error>) =
        fun state ->
            match protectedStateFunc state with
            | Choice2Of2 error ->
                Choice2Of2 error
            | Choice1Of2 (result, _) ->
                Choice1Of2 result



/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StatefulChoice =
    //
    [<CompiledName("LiftState")>]
    let liftState (stateFunc : StateFunc<'State, 'T>) : StatefulChoiceFunc<'State, 'T, 'Error> =
        fun state ->
        let value, state = stateFunc state
        (Choice1Of2 value), state

    //
    [<CompiledName("LiftChoice")>]
    let inline liftChoice (choice : Choice<'T, 'Error>) : StatefulChoiceFunc<'State, 'T, 'Error> =
        fun state ->
        choice, state

    //
    [<CompiledName("SetState")>]
    let setState (state : 'State) : StatefulChoiceFunc<_,_,'Error> =
        fun _ ->
        (Choice1Of2 ()), state

    //
    [<CompiledName("GetState")>]
    let inline getState (state : 'State) =
        (Choice1Of2 state), state

    let private ``return`` value =
        fun state ->
        (Choice1Of2 value), state

    //
    let private bind k m =
        fun state ->
        match m state with
        | (Choice1Of2 value), state ->
            k value state
        | (Choice2Of2 error), state ->
            (Choice2Of2 error), state

    /// Transforms a value in the StatefulChoice workflow by using a specified mapping function.
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'U) (m : StatefulChoiceFunc<'State, 'T, 'Error>)
            : StatefulChoiceFunc<'State, 'U, 'Error> =
        bind (mapping >> ``return``) m

    //
    [<CompiledName("Attempt")>]
    let attempt (generator : unit -> 'T) : StatefulChoiceFunc<'State, 'T, exn> =
        statefulChoice {
        let! state = getState
        return! fun _ -> Choice.attempt generator, state
        }

    //
    [<CompiledName("MapError")>]
    let mapError (map : 'Error1 -> 'Error2) (value : StatefulChoiceFunc<'State, 'T, 'Error1>)
            : StatefulChoiceFunc<'State, 'T, 'Error2> =
        statefulChoice {
        let! state = getState
        let choice, state' = value state
        return! 
            match choice with
            | Choice1Of2 c -> fun _ -> Choice1Of2 c, state'
            | Choice2Of2 error -> fun _ -> Choice2Of2 (map error), state'
        }

/// Functions for working with AsyncChoice workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AsyncChoice =
    open Microsoft.FSharp.Control

    /// Creates an AsyncChoice from an error value.
    [<CompiledName("Error")>]
    let inline error value : AsyncChoice<'T, 'Error> =
        async.Return (Choice2Of2 value)

    /// Creates an AsyncChoice representing an error value.
    /// The error value in the Choice is the specified error message.
    [<CompiledName("FailWith")>]
    let inline failwith errorMsg : AsyncChoice<'T, string> =
        async.Return (Choice2Of2 errorMsg)

    /// <summary>
    /// When the choice value is <c>Choice1Of2(x)</c>, returns <c>Choice1Of2 (f x)</c>.
    /// Otherwise, when the choice value is <c>Choice2Of2(x)</c>, returns <c>Choice2Of2(x)</c>. 
    /// </summary>
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'U) (value : AsyncChoice<'T, 'Error>) : AsyncChoice<'U, 'Error> =
        async {
        // Get the input value.
        let! x = value

        // Apply the mapping function and return the result.
        match x with
        | Choice1Of2 result ->
            return Choice1Of2 (mapping result)
        | Choice2Of2 error ->
            return (Choice2Of2 error)
        }

    /// <summary>
    /// When the choice value is <c>Choice1Of2(x)</c>, returns <c>Choice1Of2 (f x)</c>.
    /// Otherwise, when the choice value is <c>Choice2Of2(x)</c>, returns <c>Choice2Of2(x)</c>. 
    /// </summary>
    [<CompiledName("MapAsync")>]
    let mapAsync (mapping : 'T -> Async<'U>) (value : AsyncChoice<'T, 'Error>) : AsyncChoice<'U, 'Error> =
        async {
        // Get the input value.
        let! x = value

        // Apply the mapping function and return the result.
        match x with
        | Choice1Of2 result ->
            let! mappedResult = mapping result
            return Choice1Of2 mappedResult
        | Choice2Of2 error ->
            return (Choice2Of2 error)
        }
