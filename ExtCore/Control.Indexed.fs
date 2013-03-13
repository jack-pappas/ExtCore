(*

Copyright 2010-2012 TidePowerd Ltd.
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

//
namespace ExtCore.Control.Indexed

open ExtCore


(*** Workflow Monoids ***)

/// <summary>
/// </summary>
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
type StateFuncIndexed<'S1, 'S2, 'T> =
    'S1 -> 'T * 'S2

/// <summary>
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
type ReaderStateFuncIndexed<'Env, 'S1, 'S2, 'T> =
    'Env -> 'S1 -> 'T * 'S2

/// <summary>
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="Writer"></typeparam>
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
type ReaderWriterStateFuncIndexed<'Env, 'Writer, 'S1, 'S2, 'T> =
    //'Env -> 'S1 -> ('T * 'S2) * 'Writer
    'Env -> 'S1 -> 'T * 'S2 * 'Writer

/// <summary>
/// </summary>
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type ProtectedStateFuncIndexed<'S1, 'S2, 'T, 'Error> =
    'S1 -> Choice<'T * 'S2, 'Error>

/// <summary>
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type ReaderProtectedStateFuncIndexed<'Env, 'S1, 'S2, 'T, 'Error> =
    'Env -> 'S1 -> Choice<'T * 'S2, 'Error>

/// <summary>
/// </summary>
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type StatefulChoiceFuncIndexed<'S1, 'S2, 'T, 'Error> =
    'S1 -> Choice<'T, 'Error> * 'S2

/// <summary>
/// </summary>
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="K"></typeparam>
type StateContinuationFuncIndexed<'S1, 'S2, 'T, 'K> =
    'S1 -> ('T * 'S2 -> 'K) -> 'K

/// <summary>
/// </summary>
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
/// <typeparam name="K"></typeparam>
type ProtectedStateContinuationFuncIndexed<'S1, 'S2, 'T, 'Error, 'K> =
    'S1 -> (Choice<'T * 'S2, 'Error> -> 'K) -> 'K


(*** Workflow Builders ***)

/// <summary>
/// </summary>
[<Sealed>]
type StateBuilder () =
    // 'T -> M<'T>
    member inline __.Return (x)
        : StateFuncIndexed<'State, 'State, 'T> =
        fun state ->
        x, state

    // M<'T> -> M<'T>
    member inline __.ReturnFrom func
        : StateFuncIndexed<'S1, 'S2, 'T> =
        func

    // unit -> M<'T>
    member inline this.Zero ()
        : StateFuncIndexed<'State, 'State, unit> =
        this.Return ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (m : StateFuncIndexed<_,'S2,'T>, k : 'T -> StateFuncIndexed<_,_,_>)
        : StateFuncIndexed<'S1, 'S3, 'U> =
        fun state ->
            let result, state = m state
            (k result) state

    // (unit -> M<'T>) -> M<'T>
    member inline this.Delay (f : unit -> StateFuncIndexed<_,_,_>)
        : StateFuncIndexed<'S1, 'S2, 'T> =
        this.Bind (this.Return (), f)

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline this.Combine (r1 : StateFuncIndexed<_,'S2,_>, r2 : StateFuncIndexed<_,_,_>)
        : StateFuncIndexed<'S1, 'S3, 'T> =
        this.Bind (r1, fun () -> r2)

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryWith (body : StateFuncIndexed<_,_,_>, handler : exn -> StateFuncIndexed<_,_,_>)
        : StateFuncIndexed<'S1, 'S2, 'T> =
        fun state ->
            try body state
            with ex ->
                handler ex state

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryFinally (body : StateFuncIndexed<_,_,_>, handler)
        : StateFuncIndexed<'S1, 'S2, 'T> =
        fun state ->
            try body state
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member inline this.Using (resource : ('T :> System.IDisposable), body : 'T -> StateFuncIndexed<_,_,_>)
        : StateFuncIndexed<'S1, 'S2, 'U> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : StateFuncIndexed<_,_,_>)
        : StateFuncIndexed<'State, 'State, unit> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member inline this.For (sequence : seq<_>, body : 'T -> StateFuncIndexed<_,_,_>)
        : StateFuncIndexed<'State, 'State, unit> =
        this.Using (sequence.GetEnumerator (),
            (fun enum ->
                this.While (
                    enum.MoveNext,
                    this.Delay (fun () ->
                        body enum.Current))))


/// <summary>
/// </summary>
[<Sealed>]
type ReaderStateBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : ReaderStateFuncIndexed<'Env, 'State, 'State, 'T> =
        fun env state ->
        value, state

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : ReaderStateFuncIndexed<'Env, 'S1, 'S2, 'T> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : ReaderStateFuncIndexed<'Env, 'State, 'State, unit> =
        this.Return ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : ReaderStateFuncIndexed<_,_,'S2,_>, k : 'T -> ReaderStateFuncIndexed<_,_,_,_>)
        : ReaderStateFuncIndexed<'Env, 'S1, 'S3, 'U> =
        fun env state ->
            let result, state = m env state
            (k result) env state

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> ReaderStateFuncIndexed<_,_,_,_>)
        : ReaderStateFuncIndexed<'Env, 'S1, 'S2, 'T> =
        this.Bind (this.Return (), f)

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : ReaderStateFuncIndexed<_,_,'S2,_>, r2 : ReaderStateFuncIndexed<_,_,_,_>)
        : ReaderStateFuncIndexed<'Env, 'S1, 'S3, 'T> =
        this.Bind (r1, fun () -> r2)

    // M<'T> -> M<'T> -> M<'T>
    member __.TryWith (body : ReaderStateFuncIndexed<_,_,_,_>, handler : exn -> ReaderStateFuncIndexed<_,_,_,_>)
        : ReaderStateFuncIndexed<'Env, 'S1, 'S2, 'T> =
        fun env state ->
            try body env state
            with ex ->
                handler ex env state

    // M<'T> -> M<'T> -> M<'T>
    member __.TryFinally (body : ReaderStateFuncIndexed<_,_,_,_>, handler)
        : ReaderStateFuncIndexed<'Env, 'S1, 'S2, 'T> =
        fun env state ->
            try body env state
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : _ -> ReaderStateFuncIndexed<_,_,_,_>)
        : ReaderStateFuncIndexed<'Env, 'S1, 'S2, 'U> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ReaderStateFuncIndexed<_,_,_,_>)
        : ReaderStateFuncIndexed<'Env, 'State, 'State, unit> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ReaderStateFuncIndexed<_,_,_,_>)
        : ReaderStateFuncIndexed<'Env, 'State, 'State, unit> =
        this.Using (sequence.GetEnumerator (),
            (fun enum ->
                this.While (
                    enum.MoveNext,
                    this.Delay (fun () ->
                        body enum.Current))))

/// <summary>
/// </summary>
[<Sealed>]
type ProtectedStateBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : ProtectedStateFuncIndexed<'State, 'State, 'T, 'Error> =
        fun state ->
        Choice1Of2 (value, state)

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : ProtectedStateFuncIndexed<'S1, 'S2, 'T, 'Error> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : ProtectedStateFuncIndexed<'State, 'State, unit, 'Error> =
        fun state ->
        Choice1Of2 ((), state)

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> ProtectedStateFuncIndexed<_,_,_,_>)
        : ProtectedStateFuncIndexed<'S1, 'S2, 'T, 'Error> =
        fun state -> f () state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : ProtectedStateFuncIndexed<_,'S2,_,_>, k : 'T -> ProtectedStateFuncIndexed<_,_,_,_>)
        : ProtectedStateFuncIndexed<'S1, 'S3, 'U, 'Error> =
        fun state ->
        match m state with
        | Choice2Of2 error ->
            Choice2Of2 error
        | Choice1Of2 (value, state) ->
            k value state

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : ProtectedStateFuncIndexed<_,'S2,_,_>, r2 : ProtectedStateFuncIndexed<_,_,_,_>)
        : ProtectedStateFuncIndexed<'S1, 'S3, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> -> M<'T> -> M<'T>
    member __.TryWith (body : ProtectedStateFuncIndexed<_,_,_,_>, handler : exn -> ProtectedStateFuncIndexed<_,_,_,_>)
        : ProtectedStateFuncIndexed<'S1, 'S2, 'T, 'Error> =
        fun state ->
        try body state
        with ex ->
            handler ex state

    // M<'T> -> M<'T> -> M<'T>
    member __.TryFinally (body : ProtectedStateFuncIndexed<_,_,_,_>, handler)
        : ProtectedStateFuncIndexed<'S1, 'S2, 'T, 'Error> =
        fun state ->
        try body state
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ProtectedStateFuncIndexed<_,_,_,_>)
        : ProtectedStateFuncIndexed<'S1, 'S2, 'U, 'Error> =
        this.TryFinally (body resource, fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ())

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ProtectedStateFuncIndexed<_,_,_,_>)
        : ProtectedStateFuncIndexed<'State, 'State, unit, 'Error> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ProtectedStateFuncIndexed<_,_,_,_>)
        : ProtectedStateFuncIndexed<'State, 'State, unit, 'Error> =
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
        : ReaderProtectedStateFuncIndexed<'Env, 'State, 'State, 'T, 'Error> =
        fun env state ->
        Choice1Of2 (value, state)

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : ReaderProtectedStateFuncIndexed<'Env, 'S1, 'S2, 'T, 'Error> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : ReaderProtectedStateFuncIndexed<'Env, 'State, 'State, unit, 'Error> =
        fun env state ->
        Choice1Of2 ((), state)

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> ReaderProtectedStateFuncIndexed<_,_,_,_,_>)
        : ReaderProtectedStateFuncIndexed<'Env, 'S1, 'S2, 'T, 'Error> =
        fun env state -> f () env state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : ReaderProtectedStateFuncIndexed<_,_,'S2,_,_>, k : 'T -> ReaderProtectedStateFuncIndexed<_,_,_,_,_>)
        : ReaderProtectedStateFuncIndexed<'Env, 'S1, 'S3, 'U, 'Error> =
        fun env state ->
        match m env state with
        | Choice2Of2 error ->
            Choice2Of2 error
        | Choice1Of2 (value, state) ->
            k value env state

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : ReaderProtectedStateFuncIndexed<_,_,'S2,_,_>, r2 : ReaderProtectedStateFuncIndexed<_,_,_,_,_>)
        : ReaderProtectedStateFuncIndexed<'Env, 'S1, 'S3, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> -> M<'T> -> M<'T>
    member __.TryWith (body : ReaderProtectedStateFuncIndexed<_,_,_,_,_>, handler : exn -> ReaderProtectedStateFuncIndexed<_,_,_,_,_>)
        : ReaderProtectedStateFuncIndexed<'Env, 'S1, 'S2, 'T, 'Error> =
        fun state ->
        try body state
        with ex ->
            handler ex state

    // M<'T> -> M<'T> -> M<'T>
    member __.TryFinally (body : ReaderProtectedStateFuncIndexed<_,_,_,_,_>, handler)
        : ReaderProtectedStateFuncIndexed<'Env, 'S1, 'S2, 'T, 'Error> =
        fun state ->
        try body state
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ReaderProtectedStateFuncIndexed<_,_,_,_,_>)
        : ReaderProtectedStateFuncIndexed<'Env, 'S1, 'S2, 'U, 'Error> =
        this.TryFinally (body resource, fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ())

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ReaderProtectedStateFuncIndexed<_,_,_,_,_>)
        : ReaderProtectedStateFuncIndexed<'Env, 'State, 'State, unit, 'Error> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ReaderProtectedStateFuncIndexed<_,_,_,_,_>)
        : ReaderProtectedStateFuncIndexed<'Env, 'State, 'State, unit, 'Error> =
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
        : StatefulChoiceFuncIndexed<'State, 'State, 'T, 'Error> =
        fun state ->
        (Choice1Of2 value), state

    // M<'T> -> M<'T>
    member __.ReturnFrom (func)
        : StatefulChoiceFuncIndexed<_,_,_,_> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : StatefulChoiceFuncIndexed<'State, 'State, unit, 'Error> =
        fun state ->
        (Choice1Of2 ()), state

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> StatefulChoiceFuncIndexed<_,_,_,_>)
        : StatefulChoiceFuncIndexed<'S1, 'S2, 'T, 'Error> =
        fun state -> f () state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (f : StatefulChoiceFuncIndexed<_,_,_,_>, k : 'T -> StatefulChoiceFuncIndexed<_,_,_,_>)
        : StatefulChoiceFuncIndexed<'S1, 'S2, 'U, 'Error> =
        fun state ->
        match f state with
        | (Choice1Of2 value), state ->
            k value state
        | (Choice2Of2 error), state ->
            (Choice2Of2 error), state    
        
    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : StatefulChoiceFuncIndexed<_,_,_,_>, r2 : StatefulChoiceFuncIndexed<_,_,_,_>)
        : StatefulChoiceFuncIndexed<'S1, 'S2, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> -> M<'T> -> M<'T>
    member __.TryWith (body : StatefulChoiceFuncIndexed<_,_,_,_>, handler : exn -> StatefulChoiceFuncIndexed<_,_,_,_>)
        : StatefulChoiceFuncIndexed<'S1, 'S2, 'T, 'Error> =
        fun state ->
        try body state
        with ex ->
            handler ex state

    // M<'T> -> M<'T> -> M<'T>
    member __.TryFinally (body : StatefulChoiceFuncIndexed<_,_,_,_>, handler)
        : StatefulChoiceFuncIndexed<'S1, 'S2, 'T, 'Error> =
        fun state ->
        try body state
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> StatefulChoiceFuncIndexed<_,_,_,_>)
        : StatefulChoiceFuncIndexed<'S1, 'S2, 'U, 'Error> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : StatefulChoiceFuncIndexed<_,_,_,_>)
        : StatefulChoiceFuncIndexed<'State, 'State, _, 'Error> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> StatefulChoiceFuncIndexed<_,_,_,_>)
        : StatefulChoiceFuncIndexed<'State, 'State, _, 'Error> =
        this.Using (sequence.GetEnumerator (),
            (fun enum ->
                this.While (
                    enum.MoveNext,
                    this.Delay (fun () ->
                        body enum.Current))))

/// <summary>
/// </summary>
[<Sealed>]
type StateContinuationBuilder () =
    // 'T -> M<'T>
    member inline __.Return value
        : StateContinuationFuncIndexed<'State, 'State, 'T, 'K> =
        fun state cont ->
            cont (value, state)

    // M<'T> -> M<'T>
    member inline __.ReturnFrom func
        : StateContinuationFuncIndexed<'S1, 'S2, 'T, 'K> =
        func

    // unit -> M<'T>
    member inline __.Zero ()
        : StateContinuationFuncIndexed<'State, 'State, unit, 'K> =
        fun state cont ->
            cont ((), state)

    // (unit -> M<'T>) -> M<'T>
    member __.Delay f
        : StateContinuationFuncIndexed<'S1, 'S2, 'T, 'K> =
        f ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (m : StateContinuationFuncIndexed<_,'S2,_,_>, k : 'T -> StateContinuationFuncIndexed<_,_,_,_>)
        : StateContinuationFuncIndexed<'S1, 'S3, 'U, 'K> =
        fun state cont ->
            m state <| fun (result, state) ->
                k result state cont

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline __.Combine (r1 : StateContinuationFuncIndexed<_,_,_,_>, r2 : StateContinuationFuncIndexed<_,_,_,_>)
        : StateContinuationFuncIndexed<_,_,_,_> =
        fun state cont ->
            r1 state <| fun ((), state) ->
                r2 () state cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryWith (body : StateContinuationFuncIndexed<_,_,_,_>, handler : exn -> StateContinuationFuncIndexed<_,_,_,_>)
        : StateContinuationFuncIndexed<_,_,_,_> =
        fun state cont ->
            try body state cont
            with ex ->
                handler ex state cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryFinally (body : StateContinuationFuncIndexed<_,_,_,_>, handler)
        : StateContinuationFuncIndexed<_,_,_,_> =
        fun state cont ->
            try body state cont
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> StateContinuationFuncIndexed<_,_,_,_>)
        : StateContinuationFuncIndexed<_,_,_,_> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : StateContinuationFuncIndexed<_,_,_,_>)
        : StateContinuationFuncIndexed<_,_,_,_> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> StateContinuationFuncIndexed<_,_,_,_>)
        : StateContinuationFuncIndexed<_,_,_,_> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))

/// <summary>
/// </summary>
[<Sealed>]
type ProtectedStateContinuationBuilder () =
    // 'T -> M<'T>
    member inline __.Return value
        : ProtectedStateContinuationFuncIndexed<'State, 'State, 'T, 'Error, 'K> =
        fun state cont ->
            cont (Choice1Of2 (value, state))

    // M<'T> -> M<'T>
    member inline __.ReturnFrom func
        : ProtectedStateContinuationFuncIndexed<'S1, 'S2, 'T, 'Error, 'K> =
        func

    // unit -> M<'T>
    member inline __.Zero ()
        : ProtectedStateContinuationFuncIndexed<'State, 'State, unit, 'Error, 'K> =
        fun state cont ->
            cont (Choice1Of2 ((), state))

    // (unit -> M<'T>) -> M<'T>
    member __.Delay f
        : ProtectedStateContinuationFuncIndexed<'S1, 'S2, 'T, 'Error, 'K> =
        f ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (m : ProtectedStateContinuationFuncIndexed<_,_,_,_,_>, k : 'T -> ProtectedStateContinuationFuncIndexed<_,_,_,_,_>) =
        fun state cont ->
            m state <| fun result ->
                match result with
                | Choice2Of2 error ->
                    Choice2Of2 error
                    |> cont
                | Choice1Of2 (result, state) ->
                    k result state cont

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline __.Combine (r1 : ProtectedStateContinuationFuncIndexed<_,'S2,_,_,_>, r2 : ProtectedStateContinuationFuncIndexed<_,_,_,_,_>)
        : ProtectedStateContinuationFuncIndexed<'S1, 'S3, 'T, 'Error, 'K>=
        fun state cont ->
            r1 state <| fun result ->
                match result with
                | Choice2Of2 error ->
                    Choice2Of2 error
                    |> cont
                | Choice1Of2 ((), state) ->
                    r2 state cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryWith (body : ProtectedStateContinuationFuncIndexed<_,_,_,_,_>, handler : exn -> ProtectedStateContinuationFuncIndexed<_,_,_,_,_>)
        : ProtectedStateContinuationFuncIndexed<'S1, 'S2, 'T, 'Error, 'K> =
        fun state cont ->
            try body state cont
            with ex ->
                handler ex state cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryFinally (body : ProtectedStateContinuationFuncIndexed<_,_,_,_,_>, handler)
        : ProtectedStateContinuationFuncIndexed<'S1, 'S2, 'T, 'Error, 'K> =
        fun state cont ->
            try body state cont
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ProtectedStateContinuationFuncIndexed<_,_,_,_,_>)
        : ProtectedStateContinuationFuncIndexed<'S1, 'S2, 'U, 'Error, 'K> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ProtectedStateContinuationFuncIndexed<_,_,_,_,_>)
        : ProtectedStateContinuationFuncIndexed<'State, 'State, unit, 'Error, 'K> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ProtectedStateContinuationFuncIndexed<_,_,_,_,_>)
        : ProtectedStateContinuationFuncIndexed<'State, 'State, unit, 'Error, 'K> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))
