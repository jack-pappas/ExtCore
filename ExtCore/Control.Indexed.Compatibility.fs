//
namespace ExtCore.Control.Indexed.Compatibility

(*** Workflow Monoids ***)

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


open ExtCore
open ExtCore.Control.Indexed
/// Indexed-state workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Indexed =
    //
    [<CompiledName("ProtectedState")>]
    let protectedState = ProtectedIndexedStateBuilder ()
    //
    [<CompiledName("ReaderProtectedState")>]
    let readerProtectedState = ReaderProtectedIndexedStateBuilder ()
    //
    [<CompiledName("StatefulChoice")>]
    let statefulChoice = IndexedStatefulChoiceBuilder ()
