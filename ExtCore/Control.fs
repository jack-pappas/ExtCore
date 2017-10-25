﻿(*

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

namespace ExtCore.Control

open ExtCore


/// <summary>
/// Type extensions for the <see cref="Microsoft.FSharp.Core.Async&lt;T&gt;"/> type.
/// </summary>
[<AutoOpen>]
module AsyncExtensions =
    open ExtCore.Control.Agents

    type Microsoft.FSharp.Control.Async with
        /// Creates an asynchronous workflow that runs the asynchronous workflow
        /// given as an argument at most once. When the returned workflow is
        /// started for the second time, it reuses the result of the
        /// previous execution.
        static member Cache (input : Async<'T>) =
            // Preconditions
            checkNonNull "input" input

            let agent = Agent<AsyncReplyChannel<_>>.Start <| fun agent ->
                async {
                let! repl = agent.Receive ()
                let! res = input
                repl.Reply res
                while true do
                    let! repl = agent.Receive ()
                    repl.Reply res }

            async { return! agent.PostAndAsyncReply id }

        /// Starts the specified operation using a new CancellationToken and returns
        /// IDisposable object that cancels the computation. This method can be used
        /// when implementing the Subscribe method of IObservable interface.
        static member StartDisposable (op : Async<unit>) =
            // Preconditions
            checkNonNull "op" op

            let ct = new System.Threading.CancellationTokenSource ()
            Async.Start (op, ct.Token)
            { new System.IDisposable with
                member x.Dispose () =
                    ct.Cancel () }


(*** Workflow Monoids ***)

/// <summary>
/// </summary>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
type StateFunc<'State, 'T> =
    'State -> 'T * 'State

/// <summary>
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="T"></typeparam>
type ReaderFunc<'Env, 'T> =
    'Env -> 'T

/// <summary>
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
type ReaderStateFunc<'Env, 'State, 'T> =
    'Env -> 'State -> 'T * 'State

/// <summary>
/// </summary>
/// <typeparam name="Output"></typeparam>
type IWriter<'Output> =
    //
    abstract member Zero : 'Output
        with get

    //
    abstract member Combine :
        'Output * 'Output -> 'Output

/// <summary>
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="Writer"></typeparam>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
type ReaderWriterStateFunc<'Env, 'Writer, 'State, 'T> =
    //'Env -> 'State -> ('T * 'State) * 'Writer
    'Env -> 'State -> 'T * 'State * 'Writer

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
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type AsyncResult<'T, 'Error> =
    Async<Result<'T, 'Error>>

/// <summary>
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="T"></typeparam>
type AsyncReaderFunc<'Env, 'T> =
    'Env -> Async<'T>

/// <summary>
/// </summary>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
type AsyncStateFunc<'State, 'T> =
    'State -> Async<'T * 'State>

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
type LazyBuilder () =
    // 'T -> M<'T>
    member inline __.Return (x)
        : Lazy<'T> =
        notlazy x

    // M<'T> -> M<'T>
    member inline __.ReturnFrom (lazyValue : Lazy<'T>)
        : Lazy<'T> =
        lazyValue

    // unit -> M<'T>
    member inline this.Zero ()
        : Lazy<unit> =
        notlazy ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (lazyValue : Lazy<'T>, binder : 'T -> Lazy<'U>)
        : Lazy<'U> =
        Lazy.bind binder lazyValue

    // (unit -> M<'T>) -> M<'T>
    member inline this.Delay (generator : unit -> Lazy<_>)
        : Lazy<'T> =
        this.Bind (this.Return (), generator)

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline this.Combine (r1 : Lazy<_>, r2 : Lazy<_>)
        : Lazy<'T> =
        this.Bind (r1, fun () -> r2)

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member (*inline*) __.TryWith (body : Lazy<'T>, handler : exn -> 'T)
        : Lazy<'T> =
        lazy
            try
                Lazy.force body
            with ex ->
                handler ex

    // M<'T> * (unit -> unit) -> M<'T>
    member (*inline*) __.TryFinally (body : Lazy<_>, handler)
        : Lazy<'T> =
        lazy
            try
                Lazy.force body
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member (*inline*) this.Using (resource : ('T :> System.IDisposable), binding : 'T -> Lazy<_>)
        : Lazy<'U> =
        Lazy<'T>.Create <| fun () ->
            try
                let result = binding resource
                result.Force ()
            finally
                if not <| isNull (box resource) then
                    resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : Lazy<_>)
        : Lazy<_> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member (*inline*) this.For (sequence : seq<_>, body : 'T -> Lazy<_>)
        : Lazy<_> =
        this.Using (sequence.GetEnumerator (),
            (fun enum ->
                this.While (
                    enum.MoveNext,
                    this.Delay (fun () ->
                        body enum.Current))))


/// <summary>
/// </summary>
[<Sealed>]
type StateBuilder () =
    // 'T -> M<'T>
    member inline __.Return value
        : StateFunc<'State, 'T> =
        fun state ->
        value, state

    // M<'T> -> M<'T>
    member inline __.ReturnFrom func
        : StateFunc<'State, 'T> =
        func

    // unit -> M<'T>
    member inline this.Zero ()
        : StateFunc<'State, unit> =
        this.Return ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (computation : StateFunc<_, 'T>, binder : 'T -> StateFunc<_,_>)
        : StateFunc<'State, 'U> =
        fun state ->
            let result, state = computation state
            (binder result) state

    // (unit -> M<'T>) -> M<'T>
    member inline this.Delay (generator : unit -> StateFunc<_,_>)
        : StateFunc<'State, 'T> =
        this.Bind (this.Return (), generator)

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline this.Combine (r1 : StateFunc<_,_>, r2 : StateFunc<_,_>)
        : StateFunc<'State, 'T> =
        this.Bind (r1, fun () -> r2)

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member inline __.TryWith (computation : StateFunc<_,_>, catchHandler : exn -> StateFunc<_,_>)
        : StateFunc<'State, 'T> =
        fun state ->
            try computation state
            with ex ->
                catchHandler ex state

    // M<'T> * (unit -> unit) -> M<'T>
    member inline __.TryFinally (computation : StateFunc<_,_>, compensation)
        : StateFunc<'State, 'T> =
        fun state ->
            try computation state
            finally
                compensation ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), binder : 'T -> StateFunc<_,_>)
        : StateFunc<'State, 'U> =
        fun state ->
            try binder resource state
            finally
                if not <| isNull (box resource) then
                    resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : StateFunc<'State, unit>)
        : StateFunc<'State, unit> =
        fun state ->
            let mutable state = state
            while guard () do
                state <- snd <| body state
            (), state

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member inline this.For (sequence : seq<_>, body : 'T -> StateFunc<_,_>)
        : StateFunc<'State, unit> =
        this.Using (sequence.GetEnumerator (),
            (fun enum ->
                this.While (
                    enum.MoveNext,
                    this.Delay (fun () ->
                        body enum.Current))))


/// <summary>
/// </summary>
[<Sealed>]
type ReaderBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : ReaderFunc<'Env, 'T> =
        fun _ -> value

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : ReaderFunc<'Env, 'T> =
        func

    // unit -> M<'T>
    member __.Zero ()
        : ReaderFunc<'Env, unit> =
        fun _ -> ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (f : ReaderFunc<_,_>, binder : 'T -> ReaderFunc<_,_>)
        : ReaderFunc<'Env, 'U> =
        fun env ->
            let result = f env
            binder result env

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (generator : unit -> ReaderFunc<_,_>)
        : ReaderFunc<'Env, 'T> =
        this.Bind (this.Zero (), generator)

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : ReaderFunc<_,_>, r2 : ReaderFunc<_,_>)
        : ReaderFunc<'Env, 'T> =
        this.Bind (r1, fun () -> r2)

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member __.TryWith (body : ReaderFunc<_,_>, handler : exn -> ReaderFunc<_,_>)
        : ReaderFunc<'Env, 'T> =
        fun env ->
            try body env
            with ex ->
                handler ex env

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : ReaderFunc<_,_>, handler)
        : ReaderFunc<'Env, 'T> =
        fun env ->
            try body env
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), binder : 'T -> ReaderFunc<_,_>)
        : ReaderFunc<'Env, 'U> =
        fun env ->
            try binder resource env
            finally
                if not <| isNull (box resource) then
                    resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ReaderFunc<_,_>)
        : ReaderFunc<'Env, unit> =
        fun env ->
            while guard () do
                body env

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ReaderFunc<_,_>)
        : ReaderFunc<'Env, unit> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () -> body enum.Current)))
        // OPTIMIZE : Could this be replaced with Seq.map?
        (*
        fun env ->
            sequence
            |> Seq.iter (fun el ->
                body el env)
        *)


/// <summary>
/// </summary>
[<Sealed>]
type ReaderStateBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : ReaderStateFunc<'Env, 'State, 'T> =
        fun _ state ->
        value, state

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : ReaderStateFunc<'Env, 'State, 'T> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : ReaderStateFunc<'Env, 'State, unit> =
        this.Return ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : ReaderStateFunc<_,_,_>, binder : 'T -> ReaderStateFunc<_,_,_>)
        : ReaderStateFunc<'Env, 'State, 'U> =
        fun env state ->
            let result, state = m env state
            (binder result) env state

    // (unit -> M<'T>) -> M<'T>
    member __.Delay (generator : unit -> ReaderStateFunc<_,_,_>)
        : ReaderStateFunc<'Env, 'State, 'T> =
        generator ()

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : ReaderStateFunc<_,_,_>, r2 : ReaderStateFunc<_,_,_>)
        : ReaderStateFunc<'Env, 'State, 'T> =
        this.Bind (r1, fun () -> r2)

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member __.TryWith (body : ReaderStateFunc<_,_,_>, handler : exn -> ReaderStateFunc<_,_,_>)
        : ReaderStateFunc<'Env, 'State, 'T> =
        fun env state ->
            try body env state
            with ex ->
                handler ex env state

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : ReaderStateFunc<_,_,_>, handler)
        : ReaderStateFunc<'Env, 'State, 'T> =
        fun env state ->
            try body env state
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : _ -> ReaderStateFunc<_,_,_>)
        : ReaderStateFunc<'Env, 'State, 'U> =
        fun env state ->
            try body resource env state
            finally
                if not <| isNull (box resource) then
                    resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ReaderStateFunc<_,_,_>)
        : ReaderStateFunc<'Env, 'State, unit> =
        fun env state ->
            let mutable state = state
            while guard () do
                state <- snd <| body env state
            (), state

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> ReaderStateFunc<_,_,_>)
        : ReaderStateFunc<'Env, 'State, unit> =
        this.Using (sequence.GetEnumerator (),
            (fun enum ->
                this.While (
                    enum.MoveNext,
                    this.Delay (fun () ->
                        body enum.Current))))


/// <summary>
/// </summary>
/// <typeparam name="writer"></typeparam>
[<Sealed>]
type WriterBuilder<'Writer
    when 'Writer :> IWriter<'Writer>
    and 'Writer : (new : unit -> 'Writer)> (writer : 'Writer) =
    // 'T -> M<'T>
    member __.Return value
        : 'T * 'Writer =
        value, writer

    // M<'T> -> M<'T>
    member __.ReturnFrom writer
        : 'T * 'Writer =
        writer

    // unit -> M<'T>
    member __.Zero ()
        : unit * 'Writer =
        (), writer

    // (unit -> M<'T>) -> M<'T>
    member __.Delay generator
        : 'T * 'Writer =
        generator ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : 'T * 'Writer, k : 'T -> 'U * 'Writer)
        : 'U * 'Writer =
        let value, writer1 = m
        let result, writer2 = k value
        result, writer.Combine (writer1, writer2)

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member __.Combine (r1 : unit * _, r2)
        : 'T * 'Writer =
        let writer1 = snd r1
        let result, writer2 = r2
        result, writer.Combine (writer1, writer2)

//    // M<'T> * (exn -> M<'T>) -> M<'T>
//    member inline __.TryWith (body, handler)
//        : 'T * 'W =
//        notImpl "TryWith"
//
//    // M<'T> * (unit -> unit) -> M<'T>
//    member inline __.TryFinally (body, handler)
//        : 'T * 'W =
//        notImpl "TryFinally"
//
//    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
//    member this.Using (resource : ('T :> System.IDisposable), body) =
//        try body resource
//        finally
//            if not <| isNull (box resource) then
//                resource.Dispose ()
//
//    // (unit -> bool) * M<'T> -> M<'T>
//    member this.While (guard, body)
//        : unit * 'W =
//        if guard () then
//            this.Bind (body, (fun () ->
//                this.While (guard, body)))
//        else
//            this.Zero ()
//
//    // seq<'T> * ('T -> M<'U>) -> M<'U>
//    // or
//    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
//    member this.For (sequence : seq<_>, body : 'T -> _)
//        : unit * 'W =
//        this.Using (sequence.GetEnumerator (), fun enum ->
//            this.While (
//                enum.MoveNext,
//                this.Delay (fun () ->
//                    body enum.Current)))


///// <summary>
///// </summary>
//[<Sealed>]
//type ReaderWriterStateBuilder () =
//    // 'T -> M<'T>
//    member __.Return value
//        : ReaderWriterStateFuncClamped<'Env, 'Output, 'State, 'T> =
//        notImpl "Return"
//
//    // M<'T> -> M<'T>
//    member __.ReturnFrom value
//        : ReaderWriterStateFunc<'Env, 'Output, 'S1, 'S2, 'T> =
//        notImpl "ReturnFrom"
//
//    // unit -> M<'T>
//    member __.Zero ()
//        : ReaderWriterStateFunc<'Env, 'Output, 'S1, 'S2, unit> =
//        notImpl "Zero"
//
//    // M<'T> * ('T -> M<'U>) -> M<'U>
//    member __.Bind (f : ReaderWriterStateFunc<'Env, 'O1, 'S1, 'S2, 'T>, k : ReaderWriterStateFunc<'Env, 'O2, 'S2, 'S3, 'U>)
//        : ReaderWriterStateFunc<'Env, 'O2, 'S1, 'S3, 'U> =
//        notImpl "Bind"
//
//    // (unit -> M<'T>) -> M<'T>
//    member __.Delay (generator : unit -> ReaderWriterStateFunc<'Env, 'Output, 'S1, 'S2, 'T>)
//        : ReaderWriterStateFunc<'Env, 'Output, 'S1, 'S2, 'T> =
//        notImpl "Delay"
//
//    // M<'T> -> M<'T> -> M<'T>
//    // or
//    // M<unit> -> M<'T> -> M<'T>
//    member this.Combine (r1 : ReaderWriterStateFunc<_,_,_,_,_>, r2 : ReaderWriterStateFunc<_,_,_,_,_>)
//        : ReaderWriterStateFunc<_,_,_,_,_> =
//        //this.Bind (r1, fun () -> r2)
//        notImpl "Combine"
//
//    // M<'T> * (exn -> M<'T>) -> M<'T>
//    member __.TryWith (body : ReaderWriterStateFunc<_,_,_,_,_>, handler : exn -> ReaderWriterStateFunc<'Env, 'Output, 'S1, 'S2, 'T>)
//        : ReaderWriterStateFunc<'Env, 'Output, 'S1, 'S2, 'T> =
//        fun env state ->
//        try body env state
//        with ex ->
//            handler ex env state
//
//    // M<'T> * (unit -> unit) -> M<'T>
//    member __.TryFinally (body : ReaderWriterStateFunc<_,_,_,_,_>, handler)
//        : ReaderWriterStateFunc<'Env, 'Output, 'S1, 'S2, 'T> =
//        fun env state ->
//        try body env state
//        finally
//            handler ()
//
//    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
//    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ReaderWriterStateFunc<_,_,_,_,_>)
//        : ReaderWriterStateFunc<'Env, 'Output, 'S1, 'S2, unit> =
//        try body resource
//        finally
//            if not <| isNull (box resource) then
//                resource.Dispose ()
//
//    // (unit -> bool) * M<'T> -> M<'T>
//    member this.While (guard, body : ReaderWriterStateFunc<_,_,_,_,_>)
//        : ReaderWriterStateFunc<_,_,_,_,_> =
//        if guard () then
//            this.Bind (body, (fun () ->
//                this.While (guard, body)))
//        else
//            this.Zero ()
//
//    // seq<'T> * ('T -> M<'U>) -> M<'U>
//    // or
//    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
//    member this.For (sequence : seq<_>, body : 'T -> ReaderWriterStateFunc<_,_,_,_,_>)
//        : ReaderWriterStateFunc<_,_,_,_,_> =
//        this.Using (sequence.GetEnumerator (), fun enum ->
//            this.While (
//                enum.MoveNext,
//                this.Delay (fun () ->
//                    body enum.Current)))


/// <summary>
/// </summary>
[<Sealed>]
type MaybeBuilder () =
    // 'T -> M<'T>
#if DEBUG
    member __.Return
#else
    member inline __.Return
#endif
        value : 'T option =
        Some value

    // M<'T> -> M<'T>
#if DEBUG
    member __.ReturnFrom
#else
    member inline __.ReturnFrom
#endif
        value : 'T option =
        value

    // unit -> M<'T>
#if DEBUG
    member __.Zero () : 'T option =
#else
    member inline __.Zero () : 'T option =
#endif
        None

    // (unit -> M<'T>) -> M<'T>
#if DEBUG
    member __.Delay (generator : unit -> 'T) =
#else
    member inline __.Delay (generator : unit -> 'T) =
#endif
        generator

    // The signature of Run corresponds closely to that of Delay.
    // This computation builder needs to define Run because Delay doesn't force evaluation
    // of the thunk it's given (it simply returns it).
    member __.Run (generator) : 'T option =
        generator ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
#if DEBUG
    member __.Bind
#else
    member inline __.Bind
#endif
        (value, binder : 'T -> 'U option) : 'U option =
        Option.bind binder value

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
#if DEBUG
    member __.Combine
#else
    member inline __.Combine
#endif
        (r1, r2 : 'T option) : 'T option =
        match r1 with
        | None ->
            None
        | Some () ->
            r2

    // This overload of 'Combine' is needed for the implementations of some of the
    // other builder methods (e.g., While, TryWith, TryFinally) to work correctly.
#if DEBUG
    member this.Combine
#else
    member inline this.Combine
#endif
        (r1 : 'T option, r2) : 'U option =
        this.Bind (r1, r2)

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member __.TryWith (body, handler) : 'T option =
        try body ()
        with ex ->
            handler ex

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : unit -> 'T option, handler) : 'T option =
        try body ()
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member __.Using (resource : ('T :> System.IDisposable), body : _ -> _ option) : 'U option =
        try body resource
        finally
            if not <| isNull (box resource) then
                resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : unit -> unit option) : _ option =
#if false
        if guard () then
            match body () with
            | None -> None
            | Some _ ->
                this.While (guard, body)
        else
            // Return Some () to indicate success when the loop
            // finishes normally (because the guard returned false).
            Some ()
#else
        while guard () do body () |> ignore
        // Return Some () to indicate success when the loop
        // finishes normally (because the guard returned false).
        Some ()
#endif

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member __.For (sequence : seq<_>, body : 'T -> unit option) : _ option =
#if false
        use enumerator = sequence.GetEnumerator ()

        let mutable foundNone = false
        while enumerator.MoveNext () && not foundNone do
            if Option.isNone (body enumerator.Current) then
                foundNone <- true

        // If we broke out of the loop early because the 'body' function
        // return None for some element, return None (to propagate the failure).
        // Otherwise, return the 'zero' value (representing a 'success' which carries no value).
        if foundNone then None else Some ()
#else
        // Apply the body function to each element of the sequence.
        // It seems like we should be checking the return value of the body
        // and short-circuiting if it returns None (see above).
        // However, the F# compiler emits a call to Zero () at the end of a
        // 'for' loop body so checking the result causes loops to only ever
        // evaluate the body for the first element of the sequence then stop.
        for x in sequence do
            body x |> ignore
        Some ()

#endif


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
type AsyncStateBuilder () =
    // 'T -> M<'T>
    member __.Return (value : 'T)
        : AsyncStateFunc<'State, 'T> =
        fun state ->
            async.Return (value, state)

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : AsyncStateFunc<'State, 'T> =
        func

    // unit -> M<'T>
    member inline this.Zero ()
        : AsyncStateFunc<'State, unit> =
        this.Return ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : AsyncStateFunc<_, 'T>, binder : 'T -> AsyncStateFunc<_,_>)
        : AsyncStateFunc<'State, 'U> =
        fun state ->
            async {
            let! result, state = m state
            return! (binder result) state
            }

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (generator : unit -> AsyncStateFunc<_,_>)
        : AsyncStateFunc<'State, 'T> =
        this.Bind (this.Zero (), generator)

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : AsyncStateFunc<_,_>, r2 : AsyncStateFunc<_,_>)
        : AsyncStateFunc<'State, 'T> =
        this.Bind (r1, fun () -> r2)

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member __.TryWith (body : AsyncStateFunc<_,_>, handler : exn -> AsyncStateFunc<_,_>)
        : AsyncStateFunc<'State, 'T> =
        fun state ->
            async.TryWith (
                async.Delay (fun () -> body state),
                fun ex ->
                    async.Delay (fun () -> handler ex state))

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : AsyncStateFunc<_,_>, handler)
        : AsyncStateFunc<'State, 'T> =
        fun state ->
            async.TryFinally (
                async.Delay (fun () -> body state),
                handler)

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> AsyncStateFunc<_,_>)
        : AsyncStateFunc<'State, 'U> =
        this.TryFinally (
            this.Delay (fun () ->
                body resource),
            fun () ->
                if not <| isNull (box resource) then
                    resource.Dispose ())

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : AsyncStateFunc<_,_>)
        : AsyncStateFunc<'State, unit> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> AsyncStateFunc<_,_>)
        : AsyncStateFunc<'State, unit> =
        this.Using (sequence.GetEnumerator (),
            (fun enum ->
                this.While (
                    enum.MoveNext,
                    this.Delay (fun () ->
                        body enum.Current))))

/// <summary>
/// </summary>
[<Sealed>]
type AsyncMaybeBuilder () =
    // 'T -> M<'T>
    member (*inline*) __.Return value : Async<'T option> =
        async.Return <| Some value

    // M<'T> -> M<'T>
    member (*inline*) __.ReturnFrom value : Async<'T option> =
        value

    // unit -> M<'T>
    member inline this.Zero () : Async<unit option> =
        this.Return ()

    // (unit -> M<'T>) -> M<'T>
    member __.Delay (generator : unit -> Async<'T option>) : Async<'T option> =
        async.Delay generator

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member (*inline*) __.Combine (r1, r2 : Async<'T option>) : Async<'T option> =
        async {
        let! r1' = r1
        match r1' with
        | None ->
            return None
        | Some () ->
            return! r2
        }

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member (*inline*) __.Bind (value, binder : 'T -> Async<'U option>) : Async<'U option> =
        async {
        let! value' = value
        match value' with
        | None ->
            return None
        | Some result ->
            return! binder result
        }

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member inline __.TryWith (computation : Async<'T option>, catchHandler : exn -> Async<'T option>) : Async<'T option> =
        async.TryWith (computation, catchHandler)

    // M<'T> * (unit -> unit) -> M<'T>
    member inline __.TryFinally (computation : Async<'T option>, compensation : unit -> unit) : Async<'T option> =
        async.TryFinally (computation, compensation)

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member inline __.Using (resource : ('T :> System.IDisposable), binder : 'T -> Async<'U option>) : Async<'U option> =
        async.Using (resource, binder)

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : Async<_ option>) : Async<_ option> =
        if guard () then
            // OPTIMIZE : This could be simplified so we don't need to make calls to Bind and While.
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> Async<unit option>) : Async<_ option> =
        // OPTIMIZE : This could be simplified so we don't need to make calls to Using, While, Delay.
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
type AsyncResultBuilder () =
    // 'T -> M<'T>
    member (*inline*) __.Return value : Async<Result<'T, 'Error>> =
        Result.Ok value
        |> async.Return

    // M<'T> -> M<'T>
    member (*inline*) __.ReturnFrom (asyncChoice : Async<Result<'T, 'Error>>) =
        asyncChoice

    // unit -> M<'T>
    member inline this.Zero () : Async<Result<unit, 'Error>> =
        this.Return ()

    // (unit -> M<'T>) -> M<'T>
    member inline this.Delay (generator : unit -> Async<Result<'T, 'Error>>) : Async<Result<'T, 'Error>> =
        async.Delay generator

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member (*inline*) __.Combine (r1, r2) : Async<Result<'T, 'Error>> =
        async {
        let! r1' = r1
        match r1' with
        | Result.Error error ->
            return Result.Error error
        | Result.Ok () ->
            return! r2
        }

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member (*inline*) __.Bind (value : Async<Result<'T, 'Error>>, binder : 'T -> Async<Result<'U, 'Error>>)
        : Async<Result<'U, 'Error>> =
        async {
        let! value' = value
        match value' with
        | Result.Error error ->
            return Result.Error error
        | Result.Ok x ->
            return! binder x
        }

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member inline __.TryWith (computation : Async<Result<'T, 'Error>>, catchHandler : exn -> Async<Result<'T, 'Error>>)
        : Async<Result<'T, 'Error>> =
        async.TryWith(computation, catchHandler)

    // M<'T> * (unit -> unit) -> M<'T>
    member inline __.TryFinally (computation : Async<Result<'T, 'Error>>, compensation : unit -> unit)
        : Async<Result<'T, 'Error>> =
        async.TryFinally (computation, compensation)

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member inline __.Using (resource : ('T :> System.IDisposable), binder : _ -> Async<Result<'U, 'Error>>)
        : Async<Result<'U, 'Error>> =
        async.Using (resource, binder)

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : Async<Result<unit, 'Error>>) : Async<Result<_,_>> =
        if guard () then
            // OPTIMIZE : This could be simplified so we don't need to make calls to Bind and While.
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> Async<Result<unit, 'Error>>) =
        // OPTIMIZE : This could be simplified so we don't need to make calls to Using, While, Delay.
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))

/// <summary>
/// </summary>
[<Sealed>]
type AsyncReaderBuilder () =
    // 'T -> M<'T>
    member __.Return value
        : AsyncReaderFunc<'Env, 'T> =
        fun _ ->
            async.Return value

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : AsyncReaderFunc<'Env, 'T> =
        func

    // unit -> M<'T>
    member inline this.Zero ()
        : AsyncReaderFunc<'Env, unit> =
        this.Return ()

    // (unit -> M<'T>) -> M<'T>
    member __.Delay (generator : unit -> AsyncReaderFunc<_,_>)
        : AsyncReaderFunc<'Env, 'T> =
        fun env -> generator () env

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : AsyncReaderFunc<_,_>, k : 'T -> AsyncReaderFunc<_,_>)
        : AsyncReaderFunc<'Env, 'U> =
        fun env ->
            async {
            let! result = m env
            return! k result env
            }

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : AsyncReaderFunc<_,_>, r2 : AsyncReaderFunc<_,_>)
        : AsyncReaderFunc<'Env, 'T> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member __.TryWith (body : AsyncReaderFunc<_,_>, handler : exn -> AsyncReaderFunc<_,_>)
        : AsyncReaderFunc<'Env, 'T> =
        fun env ->
            async.TryWith (
                async.Delay (fun () -> body env),
                fun ex ->
                    async.Delay (fun () -> handler ex env))

    // M<'T> * (unit -> unit) -> M<'T>
    member __.TryFinally (body : AsyncReaderFunc<_,_>, handler)
        : AsyncReaderFunc<'Env, 'T> =
        fun env ->
            async.TryFinally (
                async.Delay (fun () -> body env),
                handler)

    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> AsyncReaderFunc<_,_>)
        : AsyncReaderFunc<'Env, 'U> =
        this.TryFinally (
            this.Delay (fun () ->
                body resource),
            fun () ->
                if not <| isNull (box resource) then
                    resource.Dispose ())

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : AsyncReaderFunc<_,_>)
        : AsyncReaderFunc<'Env, unit> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> AsyncReaderFunc<_,_>)
        : AsyncReaderFunc<'Env, unit> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))

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
    [<CompiledName("Lazy")>]
    let lazily = LazyBuilder ()
    //
    [<CompiledName("State")>]
    let state = StateBuilder ()
    //
    [<CompiledName("Reader")>]
    let reader = ReaderBuilder ()
    //
    [<CompiledName("ReaderState")>]
    let readerState = ReaderStateBuilder ()
//    /// <summary>
//    /// </summary>
//    /// <typeparam name="Writer"></typeparam>
//    [<CompiledName("Writer")>]
//    let writer<'Writer
//        when 'Writer :> IWriter<'Writer>
//        and 'Writer : (new : unit -> 'Writer)> =
//        WriterBuilder<'Writer> (new 'Writer())
    //
    //[<CompiledName("Rws")>]
    //let rws = ReaderWriterStateBuilder ()
    //
    [<CompiledName("Maybe")>]
    let maybe = MaybeBuilder ()
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
    [<CompiledName("AsyncReader")>]
    let asyncReader = AsyncReaderBuilder ()
    //
    [<CompiledName("AsyncMaybe")>]
    let asyncMaybe = AsyncMaybeBuilder ()
    //
    [<CompiledName("AsyncChoice")>]
    let asyncChoice = AsyncChoiceBuilder ()
    //
    [<CompiledName("AsyncResult")>]
    let asyncResult = AsyncResultBuilder ()
    //
    [<CompiledName("AsyncReaderChoice")>]
    let asyncReaderChoice = AsyncReaderChoiceBuilder ()
    //
    [<CompiledName("AsyncState")>]
    let asyncState = AsyncStateBuilder ()
    //
    [<CompiledName("AsyncProtectedState")>]
    let asyncProtectedState = AsyncProtectedStateBuilder ()
    //
    [<CompiledName("AsyncStatefulChoice")>]
    let asyncStatefulChoice = AsyncStatefulChoiceBuilder ()


(*** Workflow helper modules ***)

/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State =
    //
    [<CompiledName("Run")>]
    let inline run (stateFunc : StateFunc<'State, 'T>) initialState =
        stateFunc initialState

    //
    [<CompiledName("Evaluate")>]
    let evaluate (stateFunc : StateFunc<'State, 'T>) initialState =
        // "Run" the state function, starting with the initial state.
        // Discard the final state value and return the final result value.
        fst <| stateFunc initialState

    //
    [<CompiledName("Execute")>]
    let execute (stateFunc : StateFunc<'State, 'T>) initialState =
        // "Run" the state function, starting with the initial state.
        // Discard the final result value and return the final state value.
        snd <| stateFunc initialState

    //
    [<CompiledName("GetState")>]
    let inline getState (state : 'State) =
        state, state

    //
    [<CompiledName("SetState")>]
    let inline setState (state : 'State) =
        fun (_ : 'State) -> ((), state)

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

    /// Adapts a function designed for use with the Reader workflow
    /// so it can be used with the State workflow.
    /// Used when a function which only reads the state needs to be
    /// called from within the State workflow.
    [<CompiledName("Readonly")>]
    let inline readonly (reader : 'State -> 'T) : StateFunc<'State, 'T> =
        fun (state : 'State) ->
            let result = reader state
            result, state

    /// Applies a function to the result of a State function to transform it
    /// without affecting the current state value.
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'U) (stateFunc : StateFunc<'State, 'T>)
        : StateFunc<'State, 'U> =
        fun state ->
            // "Run" the provided state function by applying the given state.
            let result, state = stateFunc state

            // Map the result value using the mapping function.
            let result = mapping result

            // Return the mapped result value and the new state value.
            result, state


/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Reader =
    //
    [<CompiledName("Run")>]
    let inline run (readerFunc : ReaderFunc<'Env, 'T>) env =
        readerFunc env

    //
    [<CompiledName("Read")>]
    let inline read (env : 'Env) = env


/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ReaderState =
    /// Adapts a function designed for use with the State monad
    /// so it can be used with the ReaderState monad.
    [<CompiledName("LiftReader")>]
    let inline liftReader (readerFunc : ReaderFunc<'Env, 'T>)
        env (_ : 'State) =
        readerFunc env, state

    /// Adapts a function designed for use with the State monad
    /// so it can be used with the ReaderState monad.
    [<CompiledName("LiftState")>]
    let inline liftState (stateFunc : StateFunc<'State, 'T>)
        (_ : 'Env) state =
        stateFunc state

    //
    [<CompiledName("Run")>]
    let inline run (readerStateFunc : ReaderStateFunc<'Env, 'State, 'T>)
        env initialState =
        readerStateFunc env initialState

    //
    [<CompiledName("Evaluate")>]
    let evaluate (readerStateFunc : ReaderStateFunc<'Env, 'State, 'T>)
        env initialState =
        // "Run" the state function, starting with the initial state.
        // Discard the final state value and return the final result value.
        fst <| readerStateFunc env initialState

    //
    [<CompiledName("Execute")>]
    let execute (readerStateFunc : ReaderStateFunc<'Env, 'State, 'T>)
        env initialState =
        // "Run" the state function, starting with the initial state.
        // Discard the final result value and return the final state value.
        snd <| readerStateFunc env initialState

    //
    [<CompiledName("Read")>]
    let inline read (env : 'Env) (state : 'State) =
        env, state

    //
    [<CompiledName("GetState")>]
    let inline getState (_ : 'Env) (state : 'State) =
        state, state

    //
    [<CompiledName("SetState")>]
    let inline setState (state : 'State) =
        fun (_ : 'Env) (_ : 'State) -> ((), state)

(*
/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Writer =
    //
    let dummy () = ()


/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ReaderWriterState =
    //
    let dummy () = ()


/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Maybe =
    //
    let dummy () = ()
*)

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

(*
/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ReaderProtectedState =
    //
    let dummy () = ()
*)

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


/// Functions for working with F# Async workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Async =
    open Microsoft.FSharp.Control

    /// Transforms an Async value using the specified function.
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'U) (value : Async<'T>) : Async<'U> =
        async {
        // Get the input value.
        let! x = value

        // Apply the mapping function and return the result.
        return mapping x
        }

    //
    [<CompiledName("Bind2")>]
    let bind2 (binding : 'T1 -> 'T2 -> Async<'U>) (value1 : Async<'T1>) (value2 : Async<'T2>) : Async<'U> =
        async {
        // Get the input values, asynchronously.
        let! v1 = value1
        let! v2 = value2

        // Apply the binding function and return the result, asynchronously.
        return! binding v1 v2
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

/// Functions for working with AsyncResult workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AsyncResult =
    open Microsoft.FSharp.Control

    /// Creates an AsyncResult from an error value.
    [<CompiledName("Error")>]
    let inline error value : AsyncResult<'T, 'Error> =
        async.Return (Result.Error value)

    /// Creates an AsyncResult representing an error value.
    /// The error value in the Choice is the specified error message.
    [<CompiledName("FailWith")>]
    let inline failwith errorMsg : AsyncResult<'T, string> =
        async.Return (Result.Error errorMsg)

    /// <summary>
    /// When the choice value is <c>Ok(x)</c>, returns <c>Ok (f x)</c>.
    /// Otherwise, when the choice value is <c>Error(x)</c>, returns <c>Error(x)</c>.
    /// </summary>
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'U) (value : AsyncResult<'T, 'Error>) : AsyncResult<'U, 'Error> =
        async {
        // Get the input value.
        let! x = value

        // Apply the mapping function and return the result.
        match x with
        | Result.Ok result ->
            return Result.Ok (mapping result)
        | Result.Error error ->
            return (Result.Error error)
        }

    /// <summary>
    /// When the choice value is <c>Ok(x)</c>, returns <c>Ok (f x)</c>.
    /// Otherwise, when the choice value is <c>Error(x)</c>, returns <c>Error(x)</c>.
    /// </summary>
    [<CompiledName("MapAsync")>]
    let mapAsync (mapping : 'T -> Async<'U>) (value : AsyncResult<'T, 'Error>) : AsyncResult<'U, 'Error> =
        async {
        // Get the input value.
        let! x = value

        // Apply the mapping function and return the result.
        match x with
        | Result.Ok result ->
            let! mappedResult = mapping result
            return Ok mappedResult
        | Result.Error error ->
            return (Result.Error error)
        }
