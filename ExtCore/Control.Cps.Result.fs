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
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
/// <typeparam name="K"></typeparam>
type ResultContFunc<'T, 'Error, 'K> =
    ContFunc<Result<'T, 'Error>, 'K>

/// <summary>
/// </summary>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
/// <typeparam name="K"></typeparam>
type ProtectedResultStateContFunc<'State, 'T, 'Error, 'K> =
    'State -> (Result<'T * 'State, 'Error> -> 'K) -> 'K

(*** Workflow Builders ***)


/// <summary>
/// </summary>
[<Sealed>]
type ResultContBuilder () =
    // 'T -> M<'T>
    member inline __.Return value
        : ResultContFunc<'T, 'Error, 'K> =
        fun cont ->
            cont (Ok value)

    // M<'T> -> M<'T>
    member inline __.ReturnFrom func
        : ResultContFunc<'T, 'Error, 'K> =
        func

    // unit -> M<'T>
    member inline __.Zero ()
        : ResultContFunc<unit, 'Error, 'K> =
        fun cont ->
            cont (Ok ())

    // (unit -> M<'T>) -> M<'T>
    member __.Delay f
        : ResultContFunc<'T, 'Error, 'K> =
        f ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (m : ResultContFunc<_,_,_>, k : 'T -> ResultContFunc<_,_,_>)
        : ResultContFunc<'U, 'Error, 'K> =
        fun cont ->
            m <| fun result ->
            match result with
            | Error error ->
                Error error
                |> cont
            | Ok value ->
                k value cont    

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline __.Combine (r1 : ResultContFunc<_,_,_>, r2 : ResultContFunc<_,_,_>)
        : ResultContFunc<'T, 'Error, 'K> =
        fun cont ->
            r1 <| fun result ->
            match result with
            | Error error ->
                Error error
                |> cont
            | Ok () ->
                r2 cont

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member inline __.TryWith (body : ResultContFunc<_,_,_>, handler : exn -> ResultContFunc<_,_,_>)
        : ResultContFunc<'T, 'Error, 'K> =
        fun cont ->
            try body cont
            with ex ->
                handler ex cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryFinally (body : ResultContFunc<_,_,_>, handler)
        : ResultContFunc<'T, 'Error, 'K> =
        fun cont ->
            try body cont
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ResultContFunc<_,_,_>)
        : ResultContFunc<'U, 'Error, 'K> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ResultContFunc<_,_,_>)
        : ResultContFunc<unit, 'Error, 'K> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ResultContFunc<_,_,_>)
        : ResultContFunc<unit, 'Error, 'K> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))


/// <summary>
/// </summary>
[<Sealed>]
type ProtectedResultStateContBuilder () =
    // 'T -> M<'T>
    member inline __.Return value
        : ProtectedResultStateContFunc<'State, 'T, 'Error, 'K> =
        fun state cont ->
            cont (Ok (value, state))

    // M<'T> -> M<'T>
    member inline __.ReturnFrom func
        : ProtectedResultStateContFunc<'State, 'T, 'Error, 'K> =
        func

    // unit -> M<'T>
    member inline __.Zero ()
        : ProtectedResultStateContFunc<'State, unit, 'Error, 'K> =
        fun state cont ->
            cont (Ok ((), state))

    // (unit -> M<'T>) -> M<'T>
    member __.Delay f
        : ProtectedResultStateContFunc<'State, 'T, 'Error, 'K> =
        f ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (m : ProtectedResultStateContFunc<_,_,_,_>, k : 'T -> ProtectedResultStateContFunc<_,_,_,_>) =
        fun state cont ->
            m state <| fun result ->
                match result with
                | Error error ->
                    Error error
                    |> cont
                | Ok (result, state) ->
                    k result state cont

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline __.Combine (r1 : ProtectedResultStateContFunc<_,_,_,_>, r2 : ProtectedResultStateContFunc<_,_,_,_>)
        : ProtectedResultStateContFunc<'State, 'T, 'Error, 'K>=
        fun state cont ->
            r1 state <| fun result ->
                match result with
                | Error error ->
                    Error error
                    |> cont
                | Ok ((), state) ->
                    r2 state cont

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member inline __.TryWith (body : ProtectedResultStateContFunc<_,_,_,_>, handler : exn -> ProtectedResultStateContFunc<_,_,_,_>)
        : ProtectedResultStateContFunc<'State, 'T, 'Error, 'K> =
        fun state cont ->
            try body state cont
            with ex ->
                handler ex state cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryFinally (body : ProtectedResultStateContFunc<_,_,_,_>, handler)
        : ProtectedResultStateContFunc<'State, 'T, 'Error, 'K> =
        fun state cont ->
            try body state cont
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ProtectedResultStateContFunc<_,_,_,_>)
        : ProtectedResultStateContFunc<'State, 'U, 'Error, 'K> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ProtectedResultStateContFunc<_,_,_,_>)
        : ProtectedResultStateContFunc<'State, unit, 'Error, 'K> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ProtectedResultStateContFunc<_,_,_,_>)
        : ProtectedResultStateContFunc<'State, unit, 'Error, 'K> =
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
    [<CompiledName("ResultCont")>]
    let resultCont = ResultContBuilder ()
    //
    [<CompiledName("ProtectedResultStateCont")>]
    let protectedResultStateCont = ProtectedResultStateContBuilder ()


/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ResultCont =
    //
    [<CompiledName("SetError")>]
    let inline setError (error : 'Error) (cont : Result<'T, 'Error> -> ResultContFunc<'T, 'Error, 'K>)
        : ResultContFunc<'T, 'Error, 'K> =
        Error error
        |> cont

    //
    [<CompiledName("Failwith")>]
    let inline failwith (errorMsg : string) (cont : Result<'T, string> -> ResultContFunc<'T, string, 'K>)
        : ResultContFunc<'T, string, 'K> =
        Error errorMsg
        |> cont

