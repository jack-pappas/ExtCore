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
type IndexedStateFunc<'S1, 'S2, 'T> =
    'S1 -> 'T * 'S2

/// <summary>
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
type ReaderIndexedStateFunc<'Env, 'S1, 'S2, 'T> =
    'Env -> 'S1 -> 'T * 'S2

/// <summary>
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="Writer"></typeparam>
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
type ReaderWriterIndexedStateFunc<'Env, 'Writer, 'S1, 'S2, 'T> =
    //'Env -> 'S1 -> ('T * 'S2) * 'Writer
    'Env -> 'S1 -> 'T * 'S2 * 'Writer

/// <summary>
/// </summary>
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type ProtectedIndexedStateFunc<'S1, 'S2, 'T, 'Error> =
    'S1 -> Choice<'T * 'S2, 'Error>

/// <summary>
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type ReaderProtectedIndexedStateFunc<'Env, 'S1, 'S2, 'T, 'Error> =
    'Env -> 'S1 -> Choice<'T * 'S2, 'Error>

/// <summary>
/// </summary>
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type IndexedStatefulChoiceFunc<'S1, 'S2, 'T, 'Error> =
    'S1 -> Choice<'T, 'Error> * 'S2


(*** Workflow Builders ***)

/// <summary>
/// </summary>
[<Sealed>]
type IndexedStateBuilder () =
    // 'T -> M<'T>
    member inline __.Return (x)
        : IndexedStateFunc<'State, 'State, 'T> =
        fun state ->
        x, state

    // M<'T> -> M<'T>
    member inline __.ReturnFrom func
        : IndexedStateFunc<'S1, 'S2, 'T> =
        func

    // unit -> M<'T>
    member inline this.Zero ()
        : IndexedStateFunc<'State, 'State, unit> =
        this.Return ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (m : IndexedStateFunc<_,'S2,'T>, k : 'T -> IndexedStateFunc<_,_,_>)
        : IndexedStateFunc<'S1, 'S3, 'U> =
        fun state ->
            let result, state = m state
            (k result) state

    // (unit -> M<'T>) -> M<'T>
    member inline this.Delay (f : unit -> IndexedStateFunc<_,_,_>)
        : IndexedStateFunc<'S1, 'S2, 'T> =
        this.Bind (this.Return (), f)

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline this.Combine (r1 : IndexedStateFunc<_,'S2,_>, r2 : IndexedStateFunc<_,_,_>)
        : IndexedStateFunc<'S1, 'S3, 'T> =
        this.Bind (r1, fun () -> r2)

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryWith (body : IndexedStateFunc<_,_,_>, handler : exn -> IndexedStateFunc<_,_,_>)
        : IndexedStateFunc<'S1, 'S2, 'T> =
        fun state ->
            try body state
            with ex ->
                handler ex state

    // M<'T> * (unit -> unit) -> M<'T>
    member inline __.TryFinally (body : IndexedStateFunc<_,_,_>, handler)
        : IndexedStateFunc<'S1, 'S2, 'T> =
        fun state ->
            try body state
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member inline this.Using (resource : ('T :> System.IDisposable), body : 'T -> IndexedStateFunc<_,_,_>)
        : IndexedStateFunc<'S1, 'S2, 'U> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : IndexedStateFunc<_,_,_>)
        : IndexedStateFunc<'State, 'State, unit> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member inline this.For (sequence : seq<_>, body : 'T -> IndexedStateFunc<_,_,_>)
        : IndexedStateFunc<'State, 'State, unit> =
        this.Using (sequence.GetEnumerator (),
            (fun enum ->
                this.While (
                    enum.MoveNext,
                    this.Delay (fun () ->
                        body enum.Current))))


/// <summary>
/// </summary>
[<Sealed>]
type ReaderIndexedStateBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : ReaderIndexedStateFunc<'Env, 'State, 'State, 'T> =
        fun _ state ->
        value, state

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : ReaderIndexedStateFunc<'Env, 'S1, 'S2, 'T> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : ReaderIndexedStateFunc<'Env, 'State, 'State, unit> =
        this.Return ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : ReaderIndexedStateFunc<_,_,'S2,_>, k : 'T -> ReaderIndexedStateFunc<_,_,_,_>)
        : ReaderIndexedStateFunc<'Env, 'S1, 'S3, 'U> =
        fun env state ->
            let result, state = m env state
            (k result) env state

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> ReaderIndexedStateFunc<_,_,_,_>)
        : ReaderIndexedStateFunc<'Env, 'S1, 'S2, 'T> =
        this.Bind (this.Return (), f)

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : ReaderIndexedStateFunc<_,_,'S2,_>, r2 : ReaderIndexedStateFunc<_,_,_,_>)
        : ReaderIndexedStateFunc<'Env, 'S1, 'S3, 'T> =
        this.Bind (r1, fun () -> r2)

    // M<'T> -> M<'T> -> M<'T>
    member __.TryWith (body : ReaderIndexedStateFunc<_,_,_,_>, handler : exn -> ReaderIndexedStateFunc<_,_,_,_>)
        : ReaderIndexedStateFunc<'Env, 'S1, 'S2, 'T> =
        fun env state ->
            try body env state
            with ex ->
                handler ex env state

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : ReaderIndexedStateFunc<_,_,_,_>, handler)
        : ReaderIndexedStateFunc<'Env, 'S1, 'S2, 'T> =
        fun env state ->
            try body env state
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : _ -> ReaderIndexedStateFunc<_,_,_,_>)
        : ReaderIndexedStateFunc<'Env, 'S1, 'S2, 'U> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ReaderIndexedStateFunc<_,_,_,_>)
        : ReaderIndexedStateFunc<'Env, 'State, 'State, unit> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ReaderIndexedStateFunc<_,_,_,_>)
        : ReaderIndexedStateFunc<'Env, 'State, 'State, unit> =
        this.Using (sequence.GetEnumerator (),
            (fun enum ->
                this.While (
                    enum.MoveNext,
                    this.Delay (fun () ->
                        body enum.Current))))

/// <summary>
/// </summary>
[<Sealed>]
type ProtectedIndexedStateBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : ProtectedIndexedStateFunc<'State, 'State, 'T, 'Error> =
        fun state ->
        Choice1Of2 (value, state)

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : ProtectedIndexedStateFunc<'S1, 'S2, 'T, 'Error> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : ProtectedIndexedStateFunc<'State, 'State, unit, 'Error> =
        fun state ->
        Choice1Of2 ((), state)

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> ProtectedIndexedStateFunc<_,_,_,_>)
        : ProtectedIndexedStateFunc<'S1, 'S2, 'T, 'Error> =
        fun state -> f () state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : ProtectedIndexedStateFunc<_,'S2,_,_>, k : 'T -> ProtectedIndexedStateFunc<_,_,_,_>)
        : ProtectedIndexedStateFunc<'S1, 'S3, 'U, 'Error> =
        fun state ->
        match m state with
        | Choice2Of2 error ->
            Choice2Of2 error
        | Choice1Of2 (value, state) ->
            k value state

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : ProtectedIndexedStateFunc<_,'S2,_,_>, r2 : ProtectedIndexedStateFunc<_,_,_,_>)
        : ProtectedIndexedStateFunc<'S1, 'S3, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> -> M<'T> -> M<'T>
    member __.TryWith (body : ProtectedIndexedStateFunc<_,_,_,_>, handler : exn -> ProtectedIndexedStateFunc<_,_,_,_>)
        : ProtectedIndexedStateFunc<'S1, 'S2, 'T, 'Error> =
        fun state ->
        try body state
        with ex ->
            handler ex state

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : ProtectedIndexedStateFunc<_,_,_,_>, handler)
        : ProtectedIndexedStateFunc<'S1, 'S2, 'T, 'Error> =
        fun state ->
        try body state
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ProtectedIndexedStateFunc<_,_,_,_>)
        : ProtectedIndexedStateFunc<'S1, 'S2, 'U, 'Error> =
        this.TryFinally (body resource, fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ())

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ProtectedIndexedStateFunc<_,_,_,_>)
        : ProtectedIndexedStateFunc<'State, 'State, unit, 'Error> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ProtectedIndexedStateFunc<_,_,_,_>)
        : ProtectedIndexedStateFunc<'State, 'State, unit, 'Error> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))


/// <summary>
/// </summary>
[<Sealed>]
type ReaderProtectedIndexedStateBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : ReaderProtectedIndexedStateFunc<'Env, 'State, 'State, 'T, 'Error> =
        fun _ state ->
        Choice1Of2 (value, state)

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : ReaderProtectedIndexedStateFunc<'Env, 'S1, 'S2, 'T, 'Error> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : ReaderProtectedIndexedStateFunc<'Env, 'State, 'State, unit, 'Error> =
        fun _ state ->
        Choice1Of2 ((), state)

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> ReaderProtectedIndexedStateFunc<_,_,_,_,_>)
        : ReaderProtectedIndexedStateFunc<'Env, 'S1, 'S2, 'T, 'Error> =
        fun env state -> f () env state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : ReaderProtectedIndexedStateFunc<_,_,'S2,_,_>, k : 'T -> ReaderProtectedIndexedStateFunc<_,_,_,_,_>)
        : ReaderProtectedIndexedStateFunc<'Env, 'S1, 'S3, 'U, 'Error> =
        fun env state ->
        match m env state with
        | Choice2Of2 error ->
            Choice2Of2 error
        | Choice1Of2 (value, state) ->
            k value env state

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : ReaderProtectedIndexedStateFunc<_,_,'S2,_,_>, r2 : ReaderProtectedIndexedStateFunc<_,_,_,_,_>)
        : ReaderProtectedIndexedStateFunc<'Env, 'S1, 'S3, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> -> M<'T> -> M<'T>
    member __.TryWith (body : ReaderProtectedIndexedStateFunc<_,_,_,_,_>, handler : exn -> ReaderProtectedIndexedStateFunc<_,_,_,_,_>)
        : ReaderProtectedIndexedStateFunc<'Env, 'S1, 'S2, 'T, 'Error> =
        fun state ->
        try body state
        with ex ->
            handler ex state

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : ReaderProtectedIndexedStateFunc<_,_,_,_,_>, handler)
        : ReaderProtectedIndexedStateFunc<'Env, 'S1, 'S2, 'T, 'Error> =
        fun state ->
        try body state
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ReaderProtectedIndexedStateFunc<_,_,_,_,_>)
        : ReaderProtectedIndexedStateFunc<'Env, 'S1, 'S2, 'U, 'Error> =
        this.TryFinally (body resource, fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ())

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ReaderProtectedIndexedStateFunc<_,_,_,_,_>)
        : ReaderProtectedIndexedStateFunc<'Env, 'State, 'State, unit, 'Error> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ReaderProtectedIndexedStateFunc<_,_,_,_,_>)
        : ReaderProtectedIndexedStateFunc<'Env, 'State, 'State, unit, 'Error> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))


/// <summary>
/// </summary>
[<Sealed>]
type IndexedStatefulChoiceBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : IndexedStatefulChoiceFunc<'State, 'State, 'T, 'Error> =
        fun state ->
        (Choice1Of2 value), state

    // M<'T> -> M<'T>
    member __.ReturnFrom (func)
        : IndexedStatefulChoiceFunc<_,_,_,_> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : IndexedStatefulChoiceFunc<'State, 'State, unit, 'Error> =
        fun state ->
        (Choice1Of2 ()), state

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> IndexedStatefulChoiceFunc<_,_,_,_>)
        : IndexedStatefulChoiceFunc<'S1, 'S2, 'T, 'Error> =
        fun state -> f () state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (f : IndexedStatefulChoiceFunc<_,_,_,_>, k : 'T -> IndexedStatefulChoiceFunc<_,_,_,_>)
        : IndexedStatefulChoiceFunc<'S1, 'S2, 'U, 'Error> =
        fun state ->
        match f state with
        | (Choice1Of2 value), state ->
            k value state
        | (Choice2Of2 error), state ->
            (Choice2Of2 error), state    
        
    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : IndexedStatefulChoiceFunc<_,_,_,_>, r2 : IndexedStatefulChoiceFunc<_,_,_,_>)
        : IndexedStatefulChoiceFunc<'S1, 'S2, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> -> M<'T> -> M<'T>
    member __.TryWith (body : IndexedStatefulChoiceFunc<_,_,_,_>, handler : exn -> IndexedStatefulChoiceFunc<_,_,_,_>)
        : IndexedStatefulChoiceFunc<'S1, 'S2, 'T, 'Error> =
        fun state ->
        try body state
        with ex ->
            handler ex state

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : IndexedStatefulChoiceFunc<_,_,_,_>, handler)
        : IndexedStatefulChoiceFunc<'S1, 'S2, 'T, 'Error> =
        fun state ->
        try body state
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> IndexedStatefulChoiceFunc<_,_,_,_>)
        : IndexedStatefulChoiceFunc<'S1, 'S2, 'U, 'Error> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : IndexedStatefulChoiceFunc<_,_,_,_>)
        : IndexedStatefulChoiceFunc<'State, 'State, _, 'Error> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> IndexedStatefulChoiceFunc<_,_,_,_>)
        : IndexedStatefulChoiceFunc<'State, 'State, _, 'Error> =
        this.Using (sequence.GetEnumerator (),
            (fun enum ->
                this.While (
                    enum.MoveNext,
                    this.Delay (fun () ->
                        body enum.Current))))

