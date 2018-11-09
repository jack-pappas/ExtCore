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

namespace ExtCore.Control.Cps.Compatibility

open ExtCore
open ExtCore.Control.Cps

(*** Workflow Monoids ***)

/// <summary>
/// </summary>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
/// <typeparam name="K"></typeparam>
type ProtectedStateContFunc<'State, 'T, 'Error, 'K> =
    'State -> (Choice<'T * 'State, 'Error> -> 'K) -> 'K

/// <summary>
/// </summary>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
/// <typeparam name="K"></typeparam>
type ChoiceContFunc<'T, 'Error, 'K> =
    ContFunc<Choice<'T, 'Error>, 'K>

(*** Workflow Builders ***)

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
[<AutoOpen>]
module WorkflowBuilders =
    //
    [<CompiledName("ProtectedStateCont")>]
    let protectedStateCont = ProtectedStateContBuilder ()
    //
    [<CompiledName("ChoiceCont")>]
    let choiceCont = ChoiceContBuilder ()


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

