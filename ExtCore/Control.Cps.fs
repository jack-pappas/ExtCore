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
/// Incomplete continuation function
/// </summary>
/// <typeparam name="T">The type of the result value</typeparam>
/// <typeparam name="K">The type of the continuation function</typeparam>
type ContFunc<'T, 'K> =
    ('T -> 'K) -> 'K

/// <summary>
/// </summary>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="K"></typeparam>
type StateContFunc<'State, 'T, 'K> =
    'State -> ('T * 'State -> 'K) -> 'K

/// <summary>
/// </summary>
/// <typeparam name="T"></typeparam>
/// <typeparam name="K"></typeparam>
type MaybeContFunc<'T, 'K> =
    ContFunc<'T option, 'K>

/// <summary>
/// </summary>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
/// <typeparam name="K"></typeparam>
type ChoiceContFunc<'T, 'Error, 'K> =
    ContFunc<Choice<'T, 'Error>, 'K>

/// <summary>
/// </summary>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
/// <typeparam name="K"></typeparam>
type ProtectedStateContFunc<'State, 'T, 'Error, 'K> =
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

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member __.TryWith (body, handler) =
        notImpl "TryWith"

    // M<'T> * (unit -> unit) -> M<'T>
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
type ContBuilder () =
    // 'T -> M<'T>
    member inline __.Return value
        : ContFunc<'T, 'K> =
        fun k -> k value

    // M<'T> -> M<'T>
    member inline __.ReturnFrom func
        : ContFunc<'T, 'K> =
        func

    // unit -> M<'T>
    member inline __.Zero ()
        : ContFunc<unit, 'K> =
        fun k -> k ()

    // (unit -> M<'T>) -> M<'T>
    member __.Delay f
        : ContFunc<'T, 'K> =
        f ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (m : ContFunc<_,_>, f : 'T -> ContFunc<_,_>)
        : ContFunc<'U, 'K> =
        fun cont ->
            m <| fun result ->
                f result cont

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline __.Combine (r1 : ContFunc<unit, 'K>, r2 : ContFunc<'T, 'K>)
        : ContFunc<'T, 'K> =
        fun cont ->
            r1 <| fun () ->
                r2 cont

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member inline __.TryWith (body : ContFunc<_,_>, handler : exn -> ContFunc<_,_>)
        : ContFunc<'T, 'K> =
        fun state ->
            try body state
            with ex ->
                handler ex state

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryFinally (body : ContFunc<_,_>, handler)
        : ContFunc<'T, 'K> =
        fun state ->
            try body state
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ContFunc<_,_>)
        : ContFunc<'U, 'K> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ContFunc<_,_>)
        : ContFunc<unit, 'K> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ContFunc<_,_>)
        : ContFunc<unit, 'K> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))


/// <summary>
/// </summary>
[<Sealed>]
type StateContBuilder () =
    // 'T -> M<'T>
    member inline __.Return value
        : StateContFunc<'State, 'T, 'K> =
        fun state cont ->
            cont (value, state)

    // M<'T> -> M<'T>
    member inline __.ReturnFrom func
        : StateContFunc<'State, 'T, 'K> =
        func

    // unit -> M<'T>
    member inline __.Zero ()
        : StateContFunc<'State, unit, 'K> =
        fun state cont ->
            cont ((), state)

    // (unit -> M<'T>) -> M<'T>
    member __.Delay f
        : StateContFunc<'State, 'T, 'K> =
        f ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (m : StateContFunc<_,_,_>, k : 'T -> StateContFunc<_,_,_>)
        : StateContFunc<'State, 'U, 'K> =
        fun state cont ->
            m state <| fun (result, state) ->
                k result state cont

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline __.Combine (r1 : StateContFunc<'State, unit, _>, r2 : StateContFunc<_,_,_>)
        : StateContFunc<'State, 'T, 'K> =
        fun (state : 'State) cont ->
            r1 state <| fun ((), state) ->
                r2 state cont

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member inline __.TryWith (body : StateContFunc<_,_,_>, handler : exn -> StateContFunc<_,_,_>)
        : StateContFunc<'State, 'T, 'K> =
        fun state cont ->
            try body state cont
            with ex ->
                handler ex state cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryFinally (body : StateContFunc<_,_,_>, handler)
        : StateContFunc<'State, 'T, 'K> =
        fun state cont ->
            try body state cont
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> StateContFunc<_,_,_>)
        : StateContFunc<'State, 'U, 'K> =
        fun state cont ->
            try body resource state cont
            finally
                if not <| isNull (box resource) then
                    resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : StateContFunc<_,_,_>)
        : StateContFunc<'State, unit, 'K> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> StateContFunc<_,_,_>)
        : StateContFunc<'State, unit, 'K> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))


/// <summary>
/// </summary>
[<Sealed>]
type MaybeContBuilder () =
    // 'T -> M<'T>
    member inline __.Return value
        : MaybeContFunc<'T, 'K> =
        fun cont ->
            cont (Some value)

    // M<'T> -> M<'T>
    member inline __.ReturnFrom func
        : MaybeContFunc<'T, 'K> =
        func

    // unit -> M<'T>
    member inline __.Zero ()
        : MaybeContFunc<unit, 'K> =
        fun cont ->
            cont (Some ())

    // (unit -> M<'T>) -> M<'T>
    member __.Delay f
        : MaybeContFunc<_,_> =
        f ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (m : MaybeContFunc<_,_>, k : 'T -> MaybeContFunc<_,_>)
        : MaybeContFunc<'U, 'K> =
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
    member inline __.Combine (r1 : MaybeContFunc<_,_>, r2 : MaybeContFunc<_,_>)
        : MaybeContFunc<'T, 'K> =
        fun cont ->
            r1 <| fun result ->
            match result with
            | None ->
                cont None
            | Some () ->
                r2 cont

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member inline __.TryWith (body : MaybeContFunc<_,_>, handler : exn -> MaybeContFunc<_,_>)
        : MaybeContFunc<'T, 'K> =
        fun cont ->
            try body cont
            with ex ->
                handler ex cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryFinally (body : MaybeContFunc<_,_>, handler)
        : MaybeContFunc<'T, 'K> =
        fun cont ->
            try body cont
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> MaybeContFunc<_,_>)
        : MaybeContFunc<'U, 'K> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : MaybeContFunc<_,_>)
        : MaybeContFunc<unit, 'K> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> MaybeContFunc<_,_>)
        : MaybeContFunc<unit, 'K> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))


/// <summary>
/// </summary>
[<Sealed>]
type ChoiceContBuilder () =
    // 'T -> M<'T>
    member inline __.Return value
        : ChoiceContFunc<'T, 'Error, 'K> =
        fun cont ->
            cont (Choice1Of2 value)

    // M<'T> -> M<'T>
    member inline __.ReturnFrom func
        : ChoiceContFunc<'T, 'Error, 'K> =
        func

    // unit -> M<'T>
    member inline __.Zero ()
        : ChoiceContFunc<unit, 'Error, 'K> =
        fun cont ->
            cont (Choice1Of2 ())

    // (unit -> M<'T>) -> M<'T>
    member __.Delay f
        : ChoiceContFunc<'T, 'Error, 'K> =
        f ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (m : ChoiceContFunc<_,_,_>, k : 'T -> ChoiceContFunc<_,_,_>)
        : ChoiceContFunc<'U, 'Error, 'K> =
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
    member inline __.Combine (r1 : ChoiceContFunc<_,_,_>, r2 : ChoiceContFunc<_,_,_>)
        : ChoiceContFunc<'T, 'Error, 'K> =
        fun cont ->
            r1 <| fun result ->
            match result with
            | Choice2Of2 error ->
                Choice2Of2 error
                |> cont
            | Choice1Of2 () ->
                r2 cont

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member inline __.TryWith (body : ChoiceContFunc<_,_,_>, handler : exn -> ChoiceContFunc<_,_,_>)
        : ChoiceContFunc<'T, 'Error, 'K> =
        fun cont ->
            try body cont
            with ex ->
                handler ex cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryFinally (body : ChoiceContFunc<_,_,_>, handler)
        : ChoiceContFunc<'T, 'Error, 'K> =
        fun cont ->
            try body cont
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ChoiceContFunc<_,_,_>)
        : ChoiceContFunc<'U, 'Error, 'K> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ChoiceContFunc<_,_,_>)
        : ChoiceContFunc<unit, 'Error, 'K> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ChoiceContFunc<_,_,_>)
        : ChoiceContFunc<unit, 'Error, 'K> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))


/// <summary>
/// </summary>
[<Sealed>]
type ProtectedStateContBuilder () =
    // 'T -> M<'T>
    member inline __.Return value
        : ProtectedStateContFunc<'State, 'T, 'Error, 'K> =
        fun state cont ->
            cont (Choice1Of2 (value, state))

    // M<'T> -> M<'T>
    member inline __.ReturnFrom func
        : ProtectedStateContFunc<'State, 'T, 'Error, 'K> =
        func

    // unit -> M<'T>
    member inline __.Zero ()
        : ProtectedStateContFunc<'State, unit, 'Error, 'K> =
        fun state cont ->
            cont (Choice1Of2 ((), state))

    // (unit -> M<'T>) -> M<'T>
    member __.Delay f
        : ProtectedStateContFunc<'State, 'T, 'Error, 'K> =
        f ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (m : ProtectedStateContFunc<_,_,_,_>, k : 'T -> ProtectedStateContFunc<_,_,_,_>) =
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
    member inline __.Combine (r1 : ProtectedStateContFunc<_,_,_,_>, r2 : ProtectedStateContFunc<_,_,_,_>)
        : ProtectedStateContFunc<'State, 'T, 'Error, 'K>=
        fun state cont ->
            r1 state <| fun result ->
                match result with
                | Choice2Of2 error ->
                    Choice2Of2 error
                    |> cont
                | Choice1Of2 ((), state) ->
                    r2 state cont

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member inline __.TryWith (body : ProtectedStateContFunc<_,_,_,_>, handler : exn -> ProtectedStateContFunc<_,_,_,_>)
        : ProtectedStateContFunc<'State, 'T, 'Error, 'K> =
        fun state cont ->
            try body state cont
            with ex ->
                handler ex state cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryFinally (body : ProtectedStateContFunc<_,_,_,_>, handler)
        : ProtectedStateContFunc<'State, 'T, 'Error, 'K> =
        fun state cont ->
            try body state cont
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ProtectedStateContFunc<_,_,_,_>)
        : ProtectedStateContFunc<'State, 'U, 'Error, 'K> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ProtectedStateContFunc<_,_,_,_>)
        : ProtectedStateContFunc<'State, unit, 'Error, 'K> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ProtectedStateContFunc<_,_,_,_>)
        : ProtectedStateContFunc<'State, unit, 'Error, 'K> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))


/// <summary>
/// </summary>
[<AutoOpen>]
module WorkflowBuilders =
//    //
//    [<CompiledName("Transaction")>]
//    let transaction = TransactionBuilder ()
    //
    [<CompiledName("Cont")>]
    let cont = ContBuilder ()
    //
    [<CompiledName("StateCont")>]
    let stateCont = StateContBuilder ()
    //
    [<CompiledName("MaybeCont")>]
    let maybeCont = MaybeContBuilder ()
    //
    [<CompiledName("ChoiceCont")>]
    let choiceCont = ChoiceContBuilder ()
    //
    [<CompiledName("ProtectedStateCont")>]
    let protectedStateCont = ProtectedStateContBuilder ()


(*
/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Transaction =
    //
    let dummy () = ()
*)

/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Cont =
    /// Call with current continuation.
    [<CompiledName("CallCC")>]
    let inline callCC (kk : ContFunc<_,_>) (k : ContFunc<'T, 'U>) =
        kk (fun x _ -> k x) k
        

/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StateCont =
    //
    [<CompiledName("Run")>]
    let inline run (stateFunc : StateContFunc<'State, 'T, 'K>) initialState cont =
        stateFunc initialState cont
    
    //
    [<CompiledName("Evaluate")>]
    let evaluate (stateFunc : StateContFunc<'State, 'T, 'K>) initialState cont =
        // "Run" the state function, starting with the initial state.
        // Discard the final state value and return the final result value.
        stateFunc initialState (fst >> cont)

    //
    [<CompiledName("Execute")>]
    let execute (stateFunc : StateContFunc<'State, 'T, 'K>) initialState cont =
        // "Run" the state function, starting with the initial state.
        // Discard the final result value and return the final state value.
        stateFunc initialState (snd >> cont)

    //
    [<CompiledName("GetState")>]
    let inline getState (state : 'State) cont : 'K =
        cont (state, state)

    //
    [<CompiledName("SetState")>]
    let inline setState (state : 'State) (_ : 'State) cont : 'K =
        cont ((), state)

(*
/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MaybeCont =
    //
    let dummy () = ()
*)

/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ChoiceCont =
    //
    [<CompiledName("SetError")>]
    let inline setError (error : 'Error) (cont : Choice<'T, 'Error> -> ChoiceContFunc<'T, 'Error, 'K>)
        : ChoiceContFunc<'T, 'Error, 'K> =
        Choice2Of2 error
        |> cont

    //
    [<CompiledName("Failwith")>]
    let inline failwith (errorMsg : string) (cont : Choice<'T, string> -> ChoiceContFunc<'T, string, 'K>)
        : ChoiceContFunc<'T, string, 'K> =
        Choice2Of2 errorMsg
        |> cont

(*
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
type StateContFuncIndexed<'S1, 'S2, 'T, 'K> =
    'S1 -> ('T * 'S2 -> 'K) -> 'K

/// <summary>
/// </summary>
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
/// <typeparam name="K"></typeparam>
type ProtectedStateContFuncIndexed<'S1, 'S2, 'T, 'Error, 'K> =
    'S1 -> (Choice<'T * 'S2, 'Error> -> 'K) -> 'K

/// <summary>
/// </summary>
[<Sealed>]
type StateContBuilder () =
    // 'T -> M<'T>
    member inline __.Return value
        : StateContFuncIndexed<'State, 'State, 'T, 'K> =
        fun state cont ->
            cont (value, state)

    // M<'T> -> M<'T>
    member inline __.ReturnFrom func
        : StateContFuncIndexed<'S1, 'S2, 'T, 'K> =
        func

    // unit -> M<'T>
    member inline __.Zero ()
        : StateContFuncIndexed<'State, 'State, unit, 'K> =
        fun state cont ->
            cont ((), state)

    // (unit -> M<'T>) -> M<'T>
    member __.Delay f
        : StateContFuncIndexed<'S1, 'S2, 'T, 'K> =
        f ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (m : StateContFuncIndexed<_,'S2,_,_>, k : 'T -> StateContFuncIndexed<_,_,_,_>)
        : StateContFuncIndexed<'S1, 'S3, 'U, 'K> =
        fun state cont ->
            m state <| fun (result, state) ->
                k result state cont

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline __.Combine (r1 : StateContFuncIndexed<_,'S2,_,_>, r2 : StateContFuncIndexed<_,_,_,_>)
        : StateContFuncIndexed<'S1, 'S3, 'T, 'K> =
        fun state cont ->
            r1 state <| fun ((), state) ->
                r2 state cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryWith (body : StateContFuncIndexed<_,_,_,_>, handler : exn -> StateContFuncIndexed<_,_,_,_>)
        : StateContFuncIndexed<_,_,_,_> =
        fun state cont ->
            try body state cont
            with ex ->
                handler ex state cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryFinally (body : StateContFuncIndexed<_,_,_,_>, handler)
        : StateContFuncIndexed<_,_,_,_> =
        fun state cont ->
            try body state cont
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> StateContFuncIndexed<_,_,_,_>)
        : StateContFuncIndexed<_,_,_,_> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : StateContFuncIndexed<_,_,_,_>)
        : StateContFuncIndexed<_,_,_,_> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> StateContFuncIndexed<_,_,_,_>)
        : StateContFuncIndexed<_,_,_,_> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))

/// <summary>
/// </summary>
[<Sealed>]
type ProtectedStateContBuilder () =
    // 'T -> M<'T>
    member inline __.Return value
        : ProtectedStateContFuncIndexed<'State, 'State, 'T, 'Error, 'K> =
        fun state cont ->
            cont (Choice1Of2 (value, state))

    // M<'T> -> M<'T>
    member inline __.ReturnFrom func
        : ProtectedStateContFuncIndexed<'S1, 'S2, 'T, 'Error, 'K> =
        func

    // unit -> M<'T>
    member inline __.Zero ()
        : ProtectedStateContFuncIndexed<'State, 'State, unit, 'Error, 'K> =
        fun state cont ->
            cont (Choice1Of2 ((), state))

    // (unit -> M<'T>) -> M<'T>
    member __.Delay f
        : ProtectedStateContFuncIndexed<'S1, 'S2, 'T, 'Error, 'K> =
        f ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (m : ProtectedStateContFuncIndexed<_,_,_,_,_>, k : 'T -> ProtectedStateContFuncIndexed<_,_,_,_,_>) =
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
    member inline __.Combine (r1 : ProtectedStateContFuncIndexed<_,'S2,_,_,_>, r2 : ProtectedStateContFuncIndexed<_,_,_,_,_>)
        : ProtectedStateContFuncIndexed<'S1, 'S3, 'T, 'Error, 'K>=
        fun state cont ->
            r1 state <| fun result ->
                match result with
                | Choice2Of2 error ->
                    Choice2Of2 error
                    |> cont
                | Choice1Of2 ((), state) ->
                    r2 state cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryWith (body : ProtectedStateContFuncIndexed<_,_,_,_,_>, handler : exn -> ProtectedStateContFuncIndexed<_,_,_,_,_>)
        : ProtectedStateContFuncIndexed<'S1, 'S2, 'T, 'Error, 'K> =
        fun state cont ->
            try body state cont
            with ex ->
                handler ex state cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryFinally (body : ProtectedStateContFuncIndexed<_,_,_,_,_>, handler)
        : ProtectedStateContFuncIndexed<'S1, 'S2, 'T, 'Error, 'K> =
        fun state cont ->
            try body state cont
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ProtectedStateContFuncIndexed<_,_,_,_,_>)
        : ProtectedStateContFuncIndexed<'S1, 'S2, 'U, 'Error, 'K> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ProtectedStateContFuncIndexed<_,_,_,_,_>)
        : ProtectedStateContFuncIndexed<'State, 'State, unit, 'Error, 'K> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ProtectedStateContFuncIndexed<_,_,_,_,_>)
        : ProtectedStateContFuncIndexed<'State, 'State, unit, 'Error, 'K> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))

*)
