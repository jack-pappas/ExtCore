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
namespace ExtCore.Control.Cps

open ExtCore


(*** Workflow Monoids ***)

/// <summary>
/// </summary>
/// <typeparam name="T">The type of the result value.</typeparam>
/// <typeparam name="Error">The type of the error value.</typeparam>
type TransactionStatus<'T, 'Error> =
    //
    | Begin
    /// Reverted state, returned an error.
    | Abort of 'Error option
    /// Committed state, returned an error.
    | Dirty of 'Error option
    /// Reverted state, returned a result.
    | Rollback of 'T
    /// Committed state, returned a result.
    | Commit of 'T

/// <summary>
/// </summary>
/// <typeparam name="State">The type of the state value.</typeparam>
/// <typeparam name="T">The type of the result value.</typeparam>
/// <typeparam name="Error"></typeparam>
/// <typeparam name="K"></typeparam>
type TransactionFunc<'State, 'T, 'Error, 'K> =
    'State -> ('State -> TransactionStatus<'T, 'Error> -> 'K) -> 'K

/// <summary>
/// </summary>
/// <typeparam name="T"></typeparam>
/// <typeparam name="K"></typeparam>
type ContinuationFunc<'T, 'K> =
    ('T -> 'K) -> 'K

/// <summary>
/// </summary>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="K"></typeparam>
type StateContinuationFunc<'State, 'T, 'K> =
    'State -> ('T * 'State -> 'K) -> 'K

/// <summary>
/// </summary>
/// <typeparam name="T"></typeparam>
/// <typeparam name="K"></typeparam>
type MaybeContinuationFunc<'T, 'K> =
    ContinuationFunc<'T option, 'K>

/// <summary>
/// </summary>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
/// <typeparam name="K"></typeparam>
type ChoiceContinuationFunc<'T, 'Error, 'K> =
    ContinuationFunc<Choice<'T, 'Error>, 'K>

/// <summary>
/// </summary>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
/// <typeparam name="K"></typeparam>
type ProtectedStateContinuationFunc<'State, 'T, 'Error, 'K> =
    'State -> (Choice<'T * 'State, 'Error> -> 'K) -> 'K



(*** Workflow Builders ***)

(*
/// <summary>
/// </summary>
[<Sealed>]
type TransactionBuilder () =
    // 'T -> M<'T>
    member __.Return value =
        notImpl "Return"

    // M<'T> -> M<'T>
    member __.ReturnFrom value =
        notImpl "ReturnFrom"

    // unit -> M<'T>
    member __.Zero () =
        notImpl "Zero"

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (value, f) =
        notImpl "Bind"

    // (unit -> M<'T>) -> M<'T>
    member __.Delay f =
        notImpl "Delay"

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1, r2) =
        //this.Bind (r1, (fun () -> r2))
        notImpl "TryWith"

    // M<'T> -> M<'T> -> M<'T>
    member __.TryWith (body, handler) =
        notImpl "TryWith"

    // M<'T> -> M<'T> -> M<'T>
    member __.TryFinally (body, handler) =
        notImpl "TryFinally"

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member __.Using (resource : ('T :> System.IDisposable), body) =
        notImpl "Using"

    // (unit -> bool) * M<'T> -> M<'T>
    member __.While (guard, body) =
        notImpl "While"

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member __.For (sequence : seq<_>, body) =
        notImpl "For"
*)

/// <summary>
/// </summary>
[<Sealed>]
type ContinuationBuilder () =
    // 'T -> M<'T>
    member inline __.Return value
        : ContinuationFunc<'T, 'K> =
        fun k -> k value

    // M<'T> -> M<'T>
    member inline __.ReturnFrom func
        : ContinuationFunc<'T, 'K> =
        func

    // unit -> M<'T>
    member inline __.Zero ()
        : ContinuationFunc<unit, 'K> =
        fun k -> k ()

    // (unit -> M<'T>) -> M<'T>
    member __.Delay f
        : ContinuationFunc<'T, 'K> =
        f ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (m : ContinuationFunc<_,_>, f : 'T -> ContinuationFunc<_,_>)
        : ContinuationFunc<'U, 'K> =
        fun cont ->
            m <| fun result ->
                f result cont

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline __.Combine (r1 : ContinuationFunc<unit, 'K>, r2 : ContinuationFunc<'T, 'K>)
        : ContinuationFunc<'T, 'K> =
        fun cont ->
            r1 <| fun () ->
                r2 cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryWith (body : ContinuationFunc<_,_>, handler : exn -> ContinuationFunc<_,_>)
        : ContinuationFunc<'T, 'K> =
        fun state ->
            try body state
            with ex ->
                handler ex state

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryFinally (body : ContinuationFunc<_,_>, handler)
        : ContinuationFunc<'T, 'K> =
        fun state ->
            try body state
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ContinuationFunc<_,_>)
        : ContinuationFunc<'U, 'K> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ContinuationFunc<_,_>)
        : ContinuationFunc<unit, 'K> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ContinuationFunc<_,_>)
        : ContinuationFunc<unit, 'K> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))


/// <summary>
/// </summary>
[<Sealed>]
type StateContinuationBuilder () =
    // 'T -> M<'T>
    member inline __.Return value
        : StateContinuationFunc<'State, 'T, 'K> =
        fun state cont ->
            cont (value, state)

    // M<'T> -> M<'T>
    member inline __.ReturnFrom func
        : StateContinuationFunc<'State, 'T, 'K> =
        func

    // unit -> M<'T>
    member inline __.Zero ()
        : StateContinuationFunc<'State, unit, 'K> =
        fun state cont ->
            cont ((), state)

    // (unit -> M<'T>) -> M<'T>
    member __.Delay f
        : StateContinuationFunc<'State, 'T, 'K> =
        f ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (m : StateContinuationFunc<_,_,_>, k : 'T -> StateContinuationFunc<_,_,_>)
        : StateContinuationFunc<'State, 'U, 'K> =
        fun state cont ->
            m state <| fun (result, state) ->
                k result state cont

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline __.Combine (r1 : StateContinuationFunc<'State, unit, _>, r2 : StateContinuationFunc<_,_,_>)
        : StateContinuationFunc<'State, 'T, 'K> =
        fun (state : 'State) cont ->
            r1 state <| fun ((), state) ->
                r2 state cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryWith (body : StateContinuationFunc<_,_,_>, handler : exn -> StateContinuationFunc<_,_,_>)
        : StateContinuationFunc<'State, 'T, 'K> =
        fun state cont ->
            try body state cont
            with ex ->
                handler ex state cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryFinally (body : StateContinuationFunc<_,_,_>, handler)
        : StateContinuationFunc<'State, 'T, 'K> =
        fun state cont ->
            try body state cont
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> StateContinuationFunc<_,_,_>)
        : StateContinuationFunc<'State, 'U, 'K> =
        fun state cont ->
            try body resource state cont
            finally
                if not <| isNull (box resource) then
                    resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : StateContinuationFunc<_,_,_>)
        : StateContinuationFunc<'State, unit, 'K> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> StateContinuationFunc<_,_,_>)
        : StateContinuationFunc<'State, unit, 'K> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))


/// <summary>
/// </summary>
[<Sealed>]
type MaybeContinuationBuilder () =
    // 'T -> M<'T>
    member inline __.Return value
        : MaybeContinuationFunc<'T, 'K> =
        fun cont ->
            cont (Some value)

    // M<'T> -> M<'T>
    member inline __.ReturnFrom func
        : MaybeContinuationFunc<'T, 'K> =
        func

    // unit -> M<'T>
    member inline __.Zero ()
        : MaybeContinuationFunc<unit, 'K> =
        fun cont ->
            cont (Some ())

    // (unit -> M<'T>) -> M<'T>
    member __.Delay f
        : MaybeContinuationFunc<_,_> =
        f ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (m : MaybeContinuationFunc<_,_>, k : 'T -> MaybeContinuationFunc<_,_>)
        : MaybeContinuationFunc<'U, 'K> =
        fun cont ->
            m <| fun result ->
            match result with
            | None ->
                cont None
            | Some value ->
                k value cont

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline __.Combine (r1 : MaybeContinuationFunc<_,_>, r2 : MaybeContinuationFunc<_,_>)
        : MaybeContinuationFunc<'T, 'K> =
        fun cont ->
            r1 <| fun result ->
            match result with
            | None ->
                cont None
            | Some () ->
                r2 cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryWith (body : MaybeContinuationFunc<_,_>, handler : exn -> MaybeContinuationFunc<_,_>)
        : MaybeContinuationFunc<'T, 'K> =
        fun cont ->
            try body cont
            with ex ->
                handler ex cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryFinally (body : MaybeContinuationFunc<_,_>, handler)
        : MaybeContinuationFunc<'T, 'K> =
        fun cont ->
            try body cont
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> MaybeContinuationFunc<_,_>)
        : MaybeContinuationFunc<'U, 'K> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : MaybeContinuationFunc<_,_>)
        : MaybeContinuationFunc<unit, 'K> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> MaybeContinuationFunc<_,_>)
        : MaybeContinuationFunc<unit, 'K> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))


/// <summary>
/// </summary>
[<Sealed>]
type ChoiceContinuationBuilder () =
    // 'T -> M<'T>
    member inline __.Return value
        : ChoiceContinuationFunc<'T, 'Error, 'K> =
        fun cont ->
            cont (Choice1Of2 value)

    // M<'T> -> M<'T>
    member inline __.ReturnFrom func
        : ChoiceContinuationFunc<'T, 'Error, 'K> =
        func

    // unit -> M<'T>
    member inline __.Zero ()
        : ChoiceContinuationFunc<unit, 'Error, 'K> =
        fun cont ->
            cont (Choice1Of2 ())

    // (unit -> M<'T>) -> M<'T>
    member __.Delay f
        : ChoiceContinuationFunc<'T, 'Error, 'K> =
        f ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (m : ChoiceContinuationFunc<_,_,_>, k : 'T -> ChoiceContinuationFunc<_,_,_>)
        : ChoiceContinuationFunc<'U, 'Error, 'K> =
        fun cont ->
            m <| fun result ->
            match result with
            | Choice2Of2 error ->
                Choice2Of2 error
                |> cont
            | Choice1Of2 value ->
                k value cont    

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline __.Combine (r1 : ChoiceContinuationFunc<_,_,_>, r2 : ChoiceContinuationFunc<_,_,_>)
        : ChoiceContinuationFunc<'T, 'Error, 'K> =
        fun cont ->
            r1 <| fun result ->
            match result with
            | Choice2Of2 error ->
                Choice2Of2 error
                |> cont
            | Choice1Of2 () ->
                r2 cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryWith (body : ChoiceContinuationFunc<_,_,_>, handler : exn -> ChoiceContinuationFunc<_,_,_>)
        : ChoiceContinuationFunc<'T, 'Error, 'K> =
        fun cont ->
            try body cont
            with ex ->
                handler ex cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryFinally (body : ChoiceContinuationFunc<_,_,_>, handler)
        : ChoiceContinuationFunc<'T, 'Error, 'K> =
        fun cont ->
            try body cont
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ChoiceContinuationFunc<_,_,_>)
        : ChoiceContinuationFunc<'U, 'Error, 'K> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ChoiceContinuationFunc<_,_,_>)
        : ChoiceContinuationFunc<unit, 'Error, 'K> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ChoiceContinuationFunc<_,_,_>)
        : ChoiceContinuationFunc<unit, 'Error, 'K> =
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
        : ProtectedStateContinuationFunc<'State, 'T, 'Error, 'K> =
        fun state cont ->
            cont (Choice1Of2 (value, state))

    // M<'T> -> M<'T>
    member inline __.ReturnFrom func
        : ProtectedStateContinuationFunc<'State, 'T, 'Error, 'K> =
        func

    // unit -> M<'T>
    member inline __.Zero ()
        : ProtectedStateContinuationFunc<'State, unit, 'Error, 'K> =
        fun state cont ->
            cont (Choice1Of2 ((), state))

    // (unit -> M<'T>) -> M<'T>
    member __.Delay f
        : ProtectedStateContinuationFunc<'State, 'T, 'Error, 'K> =
        f ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (m : ProtectedStateContinuationFunc<_,_,_,_>, k : 'T -> ProtectedStateContinuationFunc<_,_,_,_>) =
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
    member inline __.Combine (r1 : ProtectedStateContinuationFunc<_,_,_,_>, r2 : ProtectedStateContinuationFunc<_,_,_,_>)
        : ProtectedStateContinuationFunc<'State, 'T, 'Error, 'K>=
        fun state cont ->
            r1 state <| fun result ->
                match result with
                | Choice2Of2 error ->
                    Choice2Of2 error
                    |> cont
                | Choice1Of2 ((), state) ->
                    r2 state cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryWith (body : ProtectedStateContinuationFunc<_,_,_,_>, handler : exn -> ProtectedStateContinuationFunc<_,_,_,_>)
        : ProtectedStateContinuationFunc<'State, 'T, 'Error, 'K> =
        fun state cont ->
            try body state cont
            with ex ->
                handler ex state cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryFinally (body : ProtectedStateContinuationFunc<_,_,_,_>, handler)
        : ProtectedStateContinuationFunc<'State, 'T, 'Error, 'K> =
        fun state cont ->
            try body state cont
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ProtectedStateContinuationFunc<_,_,_,_>)
        : ProtectedStateContinuationFunc<'State, 'U, 'Error, 'K> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ProtectedStateContinuationFunc<_,_,_,_>)
        : ProtectedStateContinuationFunc<'State, unit, 'Error, 'K> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ProtectedStateContinuationFunc<_,_,_,_>)
        : ProtectedStateContinuationFunc<'State, unit, 'Error, 'K> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))


/// <summary>
/// </summary>
[<RequireQualifiedAccess>]
module Cps =
//    //
//    [<CompiledName("Transaction")>]
//    let transaction = TransactionBuilder ()
    //
    [<CompiledName("Cont")>]
    let cont = ContinuationBuilder ()
    //
    [<CompiledName("State")>]
    let state = StateContinuationBuilder ()
    //
    [<CompiledName("Maybe")>]
    let maybe = MaybeContinuationBuilder ()
    //
    [<CompiledName("Choice")>]
    let choice = ChoiceContinuationBuilder ()
    //
    [<CompiledName("ProtectedState")>]
    let protectedState = ProtectedStateContinuationBuilder ()


(*
/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Transaction =
    //
    let dummy () = ()


/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Continuation =
    /// Call with current continuation.
    [<CompiledName("CallCC")>]
    let inline callCC kk k =
        kk (fun x _ -> k x) k
        

/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StateContinuation =
    //
    let dummy () = ()


/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ProtectedStateContinuation =
    //
    let dummy () = ()
*)


(*

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
    member inline __.Combine (r1 : StateContinuationFuncIndexed<_,'S2,_,_>, r2 : StateContinuationFuncIndexed<_,_,_,_>)
        : StateContinuationFuncIndexed<'S1, 'S3, 'T, 'K> =
        fun state cont ->
            r1 state <| fun ((), state) ->
                r2 state cont

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

*)
