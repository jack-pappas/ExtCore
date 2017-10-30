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
type ProtectedIndexedResultStateFunc<'S1, 'S2, 'T, 'Error> =
    'S1 -> Result<'T * 'S2, 'Error>

/// <summary>
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type ReaderProtectedIndexedResultStateFunc<'Env, 'S1, 'S2, 'T, 'Error> =
    'Env -> 'S1 -> Result<'T * 'S2, 'Error>

/// <summary>
/// </summary>
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type IndexedStatefulResultFunc<'S1, 'S2, 'T, 'Error> =
    'S1 -> Result<'T, 'Error> * 'S2


(*** Workflow Builders ***)

/// <summary>
/// </summary>
[<Sealed>]
type ProtectedIndexedResultStateBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : ProtectedIndexedResultStateFunc<'State, 'State, 'T, 'Error> =
        fun state ->
        Ok (value, state)

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : ProtectedIndexedResultStateFunc<'S1, 'S2, 'T, 'Error> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : ProtectedIndexedResultStateFunc<'State, 'State, unit, 'Error> =
        fun state ->
        Ok ((), state)

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> ProtectedIndexedResultStateFunc<_,_,_,_>)
        : ProtectedIndexedResultStateFunc<'S1, 'S2, 'T, 'Error> =
        fun state -> f () state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : ProtectedIndexedResultStateFunc<_,'S2,_,_>, k : 'T -> ProtectedIndexedResultStateFunc<_,_,_,_>)
        : ProtectedIndexedResultStateFunc<'S1, 'S3, 'U, 'Error> =
        fun state ->
        match m state with
        | Error error ->
            Error error
        | Ok (value, state) ->
            k value state

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : ProtectedIndexedResultStateFunc<_,'S2,_,_>, r2 : ProtectedIndexedResultStateFunc<_,_,_,_>)
        : ProtectedIndexedResultStateFunc<'S1, 'S3, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> -> M<'T> -> M<'T>
    member __.TryWith (body : ProtectedIndexedResultStateFunc<_,_,_,_>, handler : exn -> ProtectedIndexedResultStateFunc<_,_,_,_>)
        : ProtectedIndexedResultStateFunc<'S1, 'S2, 'T, 'Error> =
        fun state ->
        try body state
        with ex ->
            handler ex state

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : ProtectedIndexedResultStateFunc<_,_,_,_>, handler)
        : ProtectedIndexedResultStateFunc<'S1, 'S2, 'T, 'Error> =
        fun state ->
        try body state
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ProtectedIndexedResultStateFunc<_,_,_,_>)
        : ProtectedIndexedResultStateFunc<'S1, 'S2, 'U, 'Error> =
        this.TryFinally (body resource, fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ())

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ProtectedIndexedResultStateFunc<_,_,_,_>)
        : ProtectedIndexedResultStateFunc<'State, 'State, unit, 'Error> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ProtectedIndexedResultStateFunc<_,_,_,_>)
        : ProtectedIndexedResultStateFunc<'State, 'State, unit, 'Error> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))


/// <summary>
/// </summary>
[<Sealed>]
type ReaderProtectedIndexedResultStateBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : ReaderProtectedIndexedResultStateFunc<'Env, 'State, 'State, 'T, 'Error> =
        fun _ state ->
        Ok (value, state)

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : ReaderProtectedIndexedResultStateFunc<'Env, 'S1, 'S2, 'T, 'Error> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : ReaderProtectedIndexedResultStateFunc<'Env, 'State, 'State, unit, 'Error> =
        fun _ state ->
        Ok ((), state)

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> ReaderProtectedIndexedResultStateFunc<_,_,_,_,_>)
        : ReaderProtectedIndexedResultStateFunc<'Env, 'S1, 'S2, 'T, 'Error> =
        fun env state -> f () env state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : ReaderProtectedIndexedResultStateFunc<_,_,'S2,_,_>, k : 'T -> ReaderProtectedIndexedResultStateFunc<_,_,_,_,_>)
        : ReaderProtectedIndexedResultStateFunc<'Env, 'S1, 'S3, 'U, 'Error> =
        fun env state ->
        match m env state with
        | Error error ->
            Error error
        | Ok (value, state) ->
            k value env state

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : ReaderProtectedIndexedResultStateFunc<_,_,'S2,_,_>, r2 : ReaderProtectedIndexedResultStateFunc<_,_,_,_,_>)
        : ReaderProtectedIndexedResultStateFunc<'Env, 'S1, 'S3, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> -> M<'T> -> M<'T>
    member __.TryWith (body : ReaderProtectedIndexedResultStateFunc<_,_,_,_,_>, handler : exn -> ReaderProtectedIndexedResultStateFunc<_,_,_,_,_>)
        : ReaderProtectedIndexedResultStateFunc<'Env, 'S1, 'S2, 'T, 'Error> =
        fun state ->
        try body state
        with ex ->
            handler ex state

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : ReaderProtectedIndexedResultStateFunc<_,_,_,_,_>, handler)
        : ReaderProtectedIndexedResultStateFunc<'Env, 'S1, 'S2, 'T, 'Error> =
        fun state ->
        try body state
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ReaderProtectedIndexedResultStateFunc<_,_,_,_,_>)
        : ReaderProtectedIndexedResultStateFunc<'Env, 'S1, 'S2, 'U, 'Error> =
        this.TryFinally (body resource, fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ())

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ReaderProtectedIndexedResultStateFunc<_,_,_,_,_>)
        : ReaderProtectedIndexedResultStateFunc<'Env, 'State, 'State, unit, 'Error> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ReaderProtectedIndexedResultStateFunc<_,_,_,_,_>)
        : ReaderProtectedIndexedResultStateFunc<'Env, 'State, 'State, unit, 'Error> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))


/// <summary>
/// </summary>
[<Sealed>]
type IndexedStatefulResultBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : IndexedStatefulResultFunc<'State, 'State, 'T, 'Error> =
        fun state ->
        (Ok value), state

    // M<'T> -> M<'T>
    member __.ReturnFrom (func)
        : IndexedStatefulResultFunc<_,_,_,_> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : IndexedStatefulResultFunc<'State, 'State, unit, 'Error> =
        fun state ->
        (Ok ()), state

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> IndexedStatefulResultFunc<_,_,_,_>)
        : IndexedStatefulResultFunc<'S1, 'S2, 'T, 'Error> =
        fun state -> f () state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (f : IndexedStatefulResultFunc<_,_,_,_>, k : 'T -> IndexedStatefulResultFunc<_,_,_,_>)
        : IndexedStatefulResultFunc<'S1, 'S2, 'U, 'Error> =
        fun state ->
        match f state with
        | (Ok value), state ->
            k value state
        | (Error error), state ->
            (Error error), state    
        
    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : IndexedStatefulResultFunc<_,_,_,_>, r2 : IndexedStatefulResultFunc<_,_,_,_>)
        : IndexedStatefulResultFunc<'S1, 'S2, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> -> M<'T> -> M<'T>
    member __.TryWith (body : IndexedStatefulResultFunc<_,_,_,_>, handler : exn -> IndexedStatefulResultFunc<_,_,_,_>)
        : IndexedStatefulResultFunc<'S1, 'S2, 'T, 'Error> =
        fun state ->
        try body state
        with ex ->
            handler ex state

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : IndexedStatefulResultFunc<_,_,_,_>, handler)
        : IndexedStatefulResultFunc<'S1, 'S2, 'T, 'Error> =
        fun state ->
        try body state
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> IndexedStatefulResultFunc<_,_,_,_>)
        : IndexedStatefulResultFunc<'S1, 'S2, 'U, 'Error> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : IndexedStatefulResultFunc<_,_,_,_>)
        : IndexedStatefulResultFunc<'State, 'State, _, 'Error> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> IndexedStatefulResultFunc<_,_,_,_>)
        : IndexedStatefulResultFunc<'State, 'State, _, 'Error> =
        this.Using (sequence.GetEnumerator (),
            (fun enum ->
                this.While (
                    enum.MoveNext,
                    this.Delay (fun () ->
                        body enum.Current))))


/// Indexed-state workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Indexed =
    //
    [<CompiledName("State")>]
    let state = IndexedStateBuilder ()

    //
    [<CompiledName("ReaderState")>]
    let readerState = ReaderIndexedStateBuilder ()

    //
    [<CompiledName("ProtectedState")>]
    let protectedState = ProtectedIndexedResultStateBuilder ()

    //
    [<CompiledName("ReaderProtectedState")>]
    let readerProtectedState = ReaderProtectedIndexedResultStateBuilder ()

    //
    [<CompiledName("StatefulResult")>]
    let statefulResult = IndexedStatefulResultBuilder ()

