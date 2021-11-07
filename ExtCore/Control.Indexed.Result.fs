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
/// <typeparam name="Error"></typeparam>
type ProtectedIndexedStateFunc<'S1, 'S2, 'T, 'Error> =
    'S1 -> Result<'T * 'S2, 'Error>

/// <summary>
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type ReaderProtectedIndexedStateFunc<'Env, 'S1, 'S2, 'T, 'Error> =
    'Env -> 'S1 -> Result<'T * 'S2, 'Error>

/// <summary>
/// </summary>
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type IndexedStatefulFunc<'S1, 'S2, 'T, 'Error> =
    'S1 -> Result<'T, 'Error> * 'S2


(*** Workflow Builders ***)

/// <summary>
/// </summary>
[<Sealed>]
type ProtectedIndexedStateBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : ProtectedIndexedStateFunc<'State, 'State, 'T, 'Error> =
        fun state ->
        Ok (value, state)

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : ProtectedIndexedStateFunc<'S1, 'S2, 'T, 'Error> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : ProtectedIndexedStateFunc<'State, 'State, unit, 'Error> =
        fun state ->
        Ok ((), state)

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> ProtectedIndexedStateFunc<_,_,_,_>)
        : ProtectedIndexedStateFunc<'S1, 'S2, 'T, 'Error> =
        fun state -> f () state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : ProtectedIndexedStateFunc<_,'S2,_,_>, k : 'T -> ProtectedIndexedStateFunc<_,_,_,_>)
        : ProtectedIndexedStateFunc<'S1, 'S3, 'U, 'Error> =
        fun state ->
        match m state with
        | Error error ->
            Error error
        | Ok (value, state) ->
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
        Ok (value, state)

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : ReaderProtectedIndexedStateFunc<'Env, 'S1, 'S2, 'T, 'Error> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : ReaderProtectedIndexedStateFunc<'Env, 'State, 'State, unit, 'Error> =
        fun _ state ->
        Ok ((), state)

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> ReaderProtectedIndexedStateFunc<_,_,_,_,_>)
        : ReaderProtectedIndexedStateFunc<'Env, 'S1, 'S2, 'T, 'Error> =
        fun env state -> f () env state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : ReaderProtectedIndexedStateFunc<_,_,'S2,_,_>, k : 'T -> ReaderProtectedIndexedStateFunc<_,_,_,_,_>)
        : ReaderProtectedIndexedStateFunc<'Env, 'S1, 'S3, 'U, 'Error> =
        fun env state ->
        match m env state with
        | Error error ->
            Error error
        | Ok (value, state) ->
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
type IndexedStatefulBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : IndexedStatefulFunc<'State, 'State, 'T, 'Error> =
        fun state ->
        (Ok value), state

    // M<'T> -> M<'T>
    member __.ReturnFrom (func)
        : IndexedStatefulFunc<_,_,_,_> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : IndexedStatefulFunc<'State, 'State, unit, 'Error> =
        fun state ->
        (Ok ()), state

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> IndexedStatefulFunc<_,_,_,_>)
        : IndexedStatefulFunc<'S1, 'S2, 'T, 'Error> =
        fun state -> f () state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (f : IndexedStatefulFunc<_,_,_,_>, k : 'T -> IndexedStatefulFunc<_,_,_,_>)
        : IndexedStatefulFunc<'S1, 'S2, 'U, 'Error> =
        fun state ->
        match f state with
        | (Ok value), state ->
            k value state
        | (Error error), state ->
            (Error error), state    
        
    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : IndexedStatefulFunc<_,_,_,_>, r2 : IndexedStatefulFunc<_,_,_,_>)
        : IndexedStatefulFunc<'S1, 'S2, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> -> M<'T> -> M<'T>
    member __.TryWith (body : IndexedStatefulFunc<_,_,_,_>, handler : exn -> IndexedStatefulFunc<_,_,_,_>)
        : IndexedStatefulFunc<'S1, 'S2, 'T, 'Error> =
        fun state ->
        try body state
        with ex ->
            handler ex state

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : IndexedStatefulFunc<_,_,_,_>, handler)
        : IndexedStatefulFunc<'S1, 'S2, 'T, 'Error> =
        fun state ->
        try body state
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> IndexedStatefulFunc<_,_,_,_>)
        : IndexedStatefulFunc<'S1, 'S2, 'U, 'Error> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : IndexedStatefulFunc<_,_,_,_>)
        : IndexedStatefulFunc<'State, 'State, _, 'Error> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> IndexedStatefulFunc<_,_,_,_>)
        : IndexedStatefulFunc<'State, 'State, _, 'Error> =
        this.Using (sequence.GetEnumerator (),
            (fun enum ->
                this.While (
                    enum.MoveNext,
                    this.Delay (fun () ->
                        body enum.Current))))


/// Indexed-state workflows.
[<RequireQualifiedAccess>]
module Indexed =
    //
    [<CompiledName("State")>]
    let state = IndexedStateBuilder ()

    //
    [<CompiledName("ReaderState")>]
    let readerState = ReaderIndexedStateBuilder ()

    //
    [<CompiledName("ProtectedState")>]
    let protectedState = ProtectedIndexedStateBuilder ()

    //
    [<CompiledName("ReaderProtectedState")>]
    let readerProtectedState = ReaderProtectedIndexedStateBuilder ()

    //
    [<CompiledName("StatefulResult")>]
    let statefulResult = IndexedStatefulBuilder ()

