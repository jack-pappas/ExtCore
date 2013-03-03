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
namespace ExtCore.Control

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
/// Synonym for StateFuncIndexed where the initial and final state are "clamped" to the
/// same type. In other words, the function does not change the type of the state value.
/// </summary>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
type StateFunc<'State, 'T> =
    StateFuncIndexed<'State, 'State, 'T>

/// <summary>
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="T"></typeparam>
type ReaderFunc<'Env, 'T> =
    'Env -> 'T

/// <summary>
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
type ReaderStateFuncIndexed<'Env, 'S1, 'S2, 'T> =
    'Env -> 'S1 -> 'T * 'S2

/// <summary>
/// Synonym for ReaderStateFuncIndexed where the initial and final state are "clamped" to the
/// same type. In other words, the function does not change the type of the state value.
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
type ReaderStateFunc<'Env, 'State, 'T> =
    ReaderStateFuncIndexed<'Env, 'State, 'State, 'T>

/// <summary>
/// 
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
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
type ReaderWriterStateFuncIndexed<'Env, 'Writer, 'S1, 'S2, 'T> =
    //'Env -> 'S1 -> ('T * 'S2) * 'Writer
    'Env -> 'S1 -> 'T * 'S2 * 'Writer

/// <summary>
/// Synonym for ReaderWriterStateFuncIndexed where the initial and final state are "clamped" to the
/// same type. In other words, the function does not change the type of the state value.
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="Writer"></typeparam>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
type ReaderWriterStateFunc<'Env, 'Writer, 'State, 'T> =
    ReaderWriterStateFuncIndexed<'Env, 'Writer, 'State, 'State, 'T>

/// <summary>
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type ReaderChoiceFunc<'Env, 'T, 'Error> =
    'Env -> Choice<'T, 'Error>

/// <summary>
/// </summary>
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type ProtectedStateFuncIndexed<'S1, 'S2, 'T, 'Error> =
    'S1 -> Choice<'T * 'S2, 'Error>

/// <summary>
/// Synonym for ProtectedStateFuncIndexed where the initial and final state are "clamped" to the
/// same type. In other words, the function does not change the type of the state value.
/// </summary>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type ProtectedStateFunc<'State, 'T, 'Error> =
    ProtectedStateFuncIndexed<'State, 'State, 'T, 'Error>

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
/// Synonym for ReaderProtectedStateFuncIndexed where the initial and final state are "clamped" to the
/// same type. In other words, the function does not change the type of the state value.
/// </summary>
/// <typeparam name="Env"></typeparam>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type ReaderProtectedStateFunc<'Env, 'State, 'T, 'Error> =
    ReaderProtectedStateFuncIndexed<'Env, 'State, 'State, 'T, 'Error>

/// <summary>
/// </summary>
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type StatefulChoiceFuncIndexed<'S1, 'S2, 'T, 'Error> =
    'S1 -> Choice<'T, 'Error> * 'S2

/// <summary>
/// Synonym for StatefulChoiceFuncIndexed where the initial and final state are "clamped" to the
/// same type. In other words, the function does not change the type of the state value.
/// </summary>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
type StatefulChoiceFunc<'State, 'T, 'Error> =
    StatefulChoiceFuncIndexed<'State, 'State, 'T, 'Error>

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
/// <typeparam name="T">The type of the result value.</typeparam>
/// <typeparam name="State">The type of the state value.</typeparam>
/// <typeparam name="a"></typeparam>
type TransactionFunc<'T, 'State, 'a> =
    'State -> ('State -> 'a -> 'T) -> 'T

/// <summary>
/// </summary>
/// <typeparam name="T"></typeparam>
/// <typeparam name="K"></typeparam>
type ContinuationFunc<'T, 'K> =
    ('T -> 'K) -> 'K

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
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="K"></typeparam>
type StateContinuationFunc<'State, 'T, 'K> =
    StateContinuationFuncIndexed<'State, 'State, 'T, 'K>

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
/// <typeparam name="S1"></typeparam>
/// <typeparam name="S2"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
/// <typeparam name="K"></typeparam>
type ProtectedStateContinuationFuncIndexed<'S1, 'S2, 'T, 'Error, 'K> =
    'S1 -> (Choice<'T * 'S2, 'Error> -> 'K) -> 'K

/// <summary>
/// </summary>
/// <typeparam name="State"></typeparam>
/// <typeparam name="T"></typeparam>
/// <typeparam name="Error"></typeparam>
/// <typeparam name="K"></typeparam>
type ProtectedStateContinuationFunc<'State, 'T, 'Error, 'K> =
    ProtectedStateContinuationFuncIndexed<'State, 'State, 'T, 'Error, 'K>



(*** Workflow Builders ***)

/// <summary>
/// </summary>
[<Sealed>]
type StateBuilder () =
    // 'T -> M<'T>
    member inline __.Return (x)
        : StateFunc<'State, 'T> =
        fun state ->
        x, state

    // M<'T> -> M<'T>
    member inline __.ReturnFrom func
        : StateFuncIndexed<'S1, 'S2, 'T> =
        func

    // unit -> M<'T>
    member inline this.Zero ()
        : StateFunc<'State, unit> =
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
    member this.While (guard, body : StateFunc<_,_>)
        : StateFunc<'State, unit> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

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
    member __.Bind (f : ReaderFunc<_,_>, k : 'T -> ReaderFunc<_,_>)
        : ReaderFunc<'Env, 'U> =
        fun env ->
            let result = f env
            k result env

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> ReaderFunc<_,_>)
        : ReaderFunc<'Env, 'T> =
        this.Bind (this.Zero (), f)

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : ReaderFunc<_,_>, r2 : ReaderFunc<_,_>)
        : ReaderFunc<'Env, 'T> =
        this.Bind (r1, fun () -> r2)

    // M<'T> -> M<'T> -> M<'T>
    member __.TryWith (body : ReaderFunc<_,_>, handler : exn -> ReaderFunc<_,_>)
        : ReaderFunc<'Env, 'T> =
        fun env ->
            try body env
            with ex ->
                handler ex env

    // M<'T> -> M<'T> -> M<'T>
    member __.TryFinally (body : ReaderFunc<_,_>, handler)
        : ReaderFunc<'Env, 'T> =
        fun env ->
            try body env
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ReaderFunc<_,_>)
        : ReaderFunc<'Env, 'U> =
        try body resource
        finally
            if not <| isNull (box resource) then
                resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : ReaderFunc<_,_>)
        : ReaderFunc<'Env, unit> =
        if guard () then
            // OPTIMIZE : Could we manually invoke 'body' here, then call this.While recursively?
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

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
        fun env state ->
        value, state

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : ReaderStateFuncIndexed<'Env, 'S1, 'S2, 'T> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : ReaderStateFunc<'Env, 'State, unit> =
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
    member this.While (guard, body : ReaderStateFunc<_,_,_>)
        : ReaderStateFunc<'Env, 'State, unit> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

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
    member __.Delay f
        : 'T * 'Writer =
        f ()

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

//    // M<'T> -> M<'T> -> M<'T>
//    member inline __.TryWith (body, handler)
//        : 'T * 'W =
//        notImpl "TryWith"
//
//    // M<'T> -> M<'T> -> M<'T>
//    member inline __.TryFinally (body, handler)
//        : 'T * 'W =
//        notImpl "TryFinally"
//
//    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
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
//    member __.Delay (f : unit -> ReaderWriterStateFunc<'Env, 'Output, 'S1, 'S2, 'T>)
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
//    // M<'T> -> M<'T> -> M<'T>
//    member __.TryWith (body : ReaderWriterStateFunc<_,_,_,_,_>, handler : exn -> ReaderWriterStateFunc<'Env, 'Output, 'S1, 'S2, 'T>)
//        : ReaderWriterStateFunc<'Env, 'Output, 'S1, 'S2, 'T> =
//        fun env state ->
//        try body env state
//        with ex ->
//            handler ex env state
//
//    // M<'T> -> M<'T> -> M<'T>
//    member __.TryFinally (body : ReaderWriterStateFunc<_,_,_,_,_>, handler)
//        : ReaderWriterStateFunc<'Env, 'Output, 'S1, 'S2, 'T> =
//        fun env state ->
//        try body env state
//        finally
//            handler ()
//
//    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
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
    member inline __.Return value : 'T option =
        Some value

    // M<'T> -> M<'T>
    member inline __.ReturnFrom value : 'T option =
        value

    // unit -> M<'T>
    member inline __.Zero () : unit option =
        Some ()     // TODO : Should this be None?

    // (unit -> M<'T>) -> M<'T>
    member __.Delay (f : unit -> 'T option) : 'T option =
        f ()

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline __.Combine (r1, r2 : 'T option) : 'T option =
        match r1 with
        | None ->
            None
        | Some () ->
            r2

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (value, f : 'T -> 'U option) : 'U option =
        Option.bind f value

//    // M<'T> -> M<'T> -> M<'T>
//    member this.TryWith (body, handler) : _ option =
//        notImpl "TryWith"
//
//    // M<'T> -> M<'T> -> M<'T>
//    member this.TryFinally (body, handler) : _ option =
//        notImpl "TryFinally"
//
//    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
//    member this.Using (resource : ('T :> System.IDisposable), body) : _ option =
//        notImpl "Using"
//
//    // (unit -> bool) * M<'T> -> M<'T>
//    member this.While (guard, body) : _ option =
//        notImpl "While"
//
//    // seq<'T> * ('T -> M<'U>) -> M<'U>
//    // or
//    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
//    member this.For (sequence : seq<_>, body : 'T -> unit option) : _ option =
//        notImpl "For"


/// <summary>
/// </summary>
[<Sealed>]
type ChoiceBuilder () =
    // 'T -> M<'T>
    member inline __.Return value : Choice<'T, 'Error> =
        Choice1Of2 value

    // M<'T> -> M<'T>
    member inline __.ReturnFrom (m : Choice<'T, 'Error>) =
        m

    // unit -> M<'T>
    member inline __.Zero () : Choice<unit, 'Error> =
        Choice1Of2 ()

    // (unit -> M<'T>) -> M<'T>
    member __.Delay (f : unit -> Choice<'T, 'Error>) : Choice<'T, 'Error> =
        f ()

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline __.Combine (r1, r2) : Choice<'T, 'Error> =
        match r1 with
        | Choice2Of2 error ->
            Choice2Of2 error
        | Choice1Of2 () ->
            r2

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (value, f : 'T -> Choice<'U, 'Error>) : Choice<'U, 'Error> =
        match value with
        | Choice2Of2 error ->
            Choice2Of2 error
        | Choice1Of2 x ->
            f x

//    // M<'T> -> M<'T> -> M<'T>
//    member inline __.TryWith (body : 'T -> Choice<'U, 'Error>, handler) =
//        fun value ->
//            try body value
//            with ex ->
//                handler ex
//
//    // M<'T> -> M<'T> -> M<'T>
//    member inline __.TryFinally (body : 'T -> Choice<'U, 'Error>, handler) =
//        fun value ->
//            try body value
//            finally
//                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : _ -> Choice<_,_>)
        : Choice<'U, 'Error> =
        try body resource
        finally
            if not <| isNull (box resource) then
                resource.Dispose ()

//    // (unit -> bool) * M<'T> -> M<'T>
//    member this.While (guard, body : Choice<unit, 'Error>) : Choice<_,_> =
//        if guard () then
//            // OPTIMIZE : This could be simplified so we don't need to make calls to Bind and While.
//            this.Bind (body, (fun () -> this.While (guard, body)))
//        else
//            this.Zero ()

//    // seq<'T> * ('T -> M<'U>) -> M<'U>
//    // or
//    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
//    member this.For (sequence : seq<_>, body : 'T -> Choice<unit, 'Error>) =
//        // OPTIMIZE : This could be simplified so we don't need to make calls to Using, While, Delay.
//        this.Using (sequence.GetEnumerator (), fun enum ->
//            this.While (
//                enum.MoveNext,
//                this.Delay (fun () ->
//                    body enum.Current)))


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
    member __.Delay (f : unit -> ReaderChoiceFunc<'Env, 'T, 'Error>)
        : ReaderChoiceFunc<'Env, 'T, 'Error> =
        f ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (f : ReaderChoiceFunc<_,_,_>, k : 'T -> ReaderChoiceFunc<_,_,_>)
        : ReaderChoiceFunc<'Env, 'U, 'Error> =
        fun env ->
        match f env with
        | Choice2Of2 error ->
            Choice2Of2 error
        | Choice1Of2 result ->
            k result env

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

    // M<'T> -> M<'T> -> M<'T>
    member __.TryWith (body : ReaderChoiceFunc<_,_,_>, handler : exn -> ReaderChoiceFunc<_,_,_>)
        : ReaderChoiceFunc<'Env, 'T, 'Error> =
        fun env ->
        try body env
        with ex ->
            handler ex env

    // M<'T> -> M<'T> -> M<'T>
    member __.TryFinally (body : ReaderChoiceFunc<_,_,_>, handler)
        : ReaderChoiceFunc<'Env, 'T, 'Error> =
        fun env ->
        try body env
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member __.Using (resource : ('T :> System.IDisposable), body : 'T -> ReaderChoiceFunc<_,_,_>)
        : ReaderChoiceFunc<'Env, 'U, 'Error> =
        try body resource
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
        : ProtectedStateFuncIndexed<'S1, 'S2, 'T, 'Error> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : ProtectedStateFunc<'State, unit, 'Error> =
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
        fun env state ->
        Choice1Of2 (value, state)

    // M<'T> -> M<'T>
    member __.ReturnFrom func
        : ReaderProtectedStateFuncIndexed<'Env, 'S1, 'S2, 'T, 'Error> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : ReaderProtectedStateFunc<'Env, 'State, unit, 'Error> =
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
        : StatefulChoiceFuncIndexed<_,_,_,_> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : StatefulChoiceFunc<'State, unit, 'Error> =
        fun state ->
        (Choice1Of2 ()), state

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> StatefulChoiceFuncIndexed<_,_,_,_>)
        : StatefulChoiceFuncIndexed<'S1, 'S2, 'T, 'Error> =
        fun state -> f () state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (f : StatefulChoiceFuncIndexed<_,_,_,_>, k : 'T -> StatefulChoiceFunc<_,_,_>)
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
    member this.Combine (r1 : StatefulChoiceFuncIndexed<_,_,_,_>, r2 : StatefulChoiceFunc<_,_,_>)
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
        : StateContinuationFuncIndexed<'S1, 'S2, 'T, 'K> =
        func

    // unit -> M<'T>
    member inline __.Zero ()
        : StateContinuationFunc<'State, unit, 'K> =
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
    member this.While (guard, body : StateContinuationFunc<_,_,_>)
        : StateContinuationFunc<_,_,_> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> StateContinuationFunc<_,_,_>)
        : StateContinuationFunc<_,_,_> =
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
        : MaybeContinuationFunc<_,_> =
        fun cont ->
            r1 <| fun result ->
            match result with
            | None ->
                cont None
            | Some () ->
                r2 cont

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryWith (body : MaybeContinuationFunc<_,_>, handler : exn -> MaybeContinuationFunc<_,_>)
        : MaybeContinuationFunc<_,_> =
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
        : MaybeContinuationFunc<_,_> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : MaybeContinuationFunc<_,_>)
        : MaybeContinuationFunc<_,_> =
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
        : ProtectedStateContinuationFuncIndexed<'S1, 'S2, 'T, 'Error, 'K> =
        func

    // unit -> M<'T>
    member inline __.Zero ()
        : ProtectedStateContinuationFunc<'State, unit, 'Error, 'K> =
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
[<AutoOpen>]
module WorkflowBuilders =
    //
    let state = StateBuilder ()
    //
    let reader = ReaderBuilder ()
    //
    let readerState = ReaderStateBuilder ()
    /// <summary>
    /// </summary>
    /// <typeparam name="Writer"></typeparam>
    [<GeneralizableValue>]
    let writer<'Writer
        when 'Writer :> IWriter<'Writer>
        and 'Writer : (new : unit -> 'Writer)> =
        WriterBuilder<'Writer> (new 'Writer())
    //
    //let rws = ReaderWriterStateBuilder ()
    //
    let maybe = MaybeBuilder ()
    //
    let choice = ChoiceBuilder ()
    //
    let readerChoice = ReaderChoiceBuilder ()
    //
    let protectedState = ProtectedStateBuilder ()
    //
    let readerProtectedState = ReaderProtectedStateBuilder ()
    //
    let statefulChoice = StatefulChoiceBuilder ()
    //
    let transaction = TransactionBuilder ()

    /// <summary>
    /// </summary>
    [<RequireQualifiedAccess>]
    module Cps =
        //
        let cont = ContinuationBuilder ()
        //
        let state = StateContinuationBuilder ()
        //
        let maybe = MaybeContinuationBuilder ()
        //
        let choice = ChoiceContinuationBuilder ()
        //
        let protectedState = ProtectedStateContinuationBuilder ()



(*** Workflow helper modules ***)

/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State =
    //
    let inline run (stateFunc : StateFuncIndexed<'S1, 'S2, 'T>) initialState =
        stateFunc initialState
    
    //
    let evaluate (stateFunc : StateFuncIndexed<'S1, 'S2, 'T>) initialState =
        // "Run" the state function, starting with the initial state.
        // Discard the final state value and return the final result value.
        fst <| stateFunc initialState

    //
    let execute (stateFunc : StateFuncIndexed<'S1, 'S2, 'T>) initialState =
        // "Run" the state function, starting with the initial state.
        // Discard the final result value and return the final state value.
        snd <| stateFunc initialState

    //
    let inline getState (state : 'State) =
        state, state

    //
    let inline setState (state : 'State) =
        fun _ -> ((), state)

    //
    let inline bindChoice (k : 'T -> StateFuncIndexed<'S2, 'S3, 'U>) (m : ProtectedStateFuncIndexed<_,_,_,_>) =
        fun (state : 'S1) ->
            match m state with
            | Choice2Of2 ex ->
                raise ex
            | Choice1Of2 (value, state) ->
                k value state

    /// Adapts a function designed for use with the Reader workflow
    /// so it can be used with the State workflow.
    /// Used when a function which only reads the state needs to be
    /// called from within the State workflow.
    let inline readonly (reader : 'State -> 'T) : StateFunc<'State, 'T> =
        fun (state : 'State) ->
            let result = reader state
            result, state


/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Reader =
    //
    let dummy () = ()


/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ReaderState =
    //
    let dummy () = ()


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


/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Choice =
    /// Transforms a Choice's first value by using a specified mapping function.
    let inline map (f : 'T -> 'U) (x : Choice<'T, 'Error>) =
        match x with
        | Choice2Of2 x ->
            Choice2Of2 x
        | Choice1Of2 x ->
            f x
            |> Choice1Of2

    //
    let inline bindOrRaise (x : Choice<'T, #exn>) =
        match x with
        | Choice2Of2 e ->
            raise e
        | Choice1Of2 r ->
            r

    //
    let inline bindOrFail (x : Choice<'T, string>) =
        match x with
        | Choice1Of2 r -> r
        | Choice2Of2 msg ->
            raise <| System.Exception msg

    //
    let inline setError error : Choice<'T, 'Error> =
        Choice2Of2 error

    //
    let inline failwith errorMsg : Choice<'T, string> =
        Choice2Of2 errorMsg


/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ReaderChoice =
    //
    let dummy () = ()


/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ProtectedState =
    //
    let inline bind k m =
        fun state ->
        match m state with
        | Choice2Of2 error ->
            Choice2Of2 error
        | Choice1Of2 (value, state) ->
            k value state

    //
    let inline delay f =
        bind f <| fun state ->
            Choice1Of2 ((), state)

    //
    let inline combine (r1, r2) =
        bind (fun () -> r2) r1

    //
    let inline liftState (stateFunc : StateFuncIndexed<'S1, 'S2, 'T>) : ProtectedStateFuncIndexed<'S1, 'S2, 'T, 'Error> =
        stateFunc >> Choice1Of2

    //
    let inline liftEither (valueOrError : Choice<'T, 'Error>) =
        match valueOrError with
        | Choice2Of2 error ->
            fun _ -> Choice2Of2 error
        | Choice1Of2 value ->
            fun (state : 'State) ->
            Choice1Of2 (value, state)

    /// Adapts a function designed for use with the Reader monad
    /// so it can be used with the ProtectedState monad.
    /// Used for functions which only need to read from the state.
    let inline liftReader (readerM : 'State -> 'T) : ProtectedStateFunc<'State, 'T, 'Error> =
        fun state ->
            let result = readerM state
            Choice1Of2 (result, state)

    //
    let inline liftReaderChoice (readerChoiceFunc : 'State -> Choice<'T, 'Error>) : ProtectedStateFunc<'State, 'T, 'Error> =
        fun state ->
            match readerChoiceFunc state with
            | Choice2Of2 error ->
                Choice2Of2 error
            | Choice1Of2 result ->
                Choice1Of2 (result, state)

    //
    let inline setProtectedState (state : 'State) =
        Choice1Of2 ((), state)

    //
    let inline getProtectedState (state : 'State) =
        Choice1Of2 (state, state)

    /// Sets an error value in the computation. The monadic equivalent of raising an exception.
    let inline setError error (_ : 'State) : Choice<'T * 'State, 'Error> =
        Choice2Of2 error

    /// The monadic equivalent of F#'s built-in 'failwith' operator.
    let inline failwith (errorMsg : string) (_ : 'State) : Choice<'T * 'State, string> =
        Choice2Of2 errorMsg

    /// Discards the state value.
    /// Useful when the state value is only needed during the computation;
    /// by discarding the state when the computation is complete, the return
    /// value can be adapted to the Either workflow.
    let inline discardState (protectedStateM : 'State1 -> Choice<'T * 'State2, 'Error>) =
        fun state ->
            match protectedStateM state with
            | Choice2Of2 error ->
                Choice2Of2 error
            | Choice1Of2 (result, _) ->
                Choice1Of2 result


/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ReaderProtectedState =
    //
    let dummy () = ()


/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StatefulChoice =
    //
    let liftState (stateFunc : StateFuncIndexed<'S1, 'S2, 'T>) : StatefulChoiceFuncIndexed<'S1, 'S2, 'T, 'Error> =
        fun state ->
        let value, state = stateFunc state
        (Choice1Of2 value), state

    //
    let inline liftEither (valueOrError : Choice<'T, 'Error>) : StatefulChoiceFunc<'State, 'T, 'Error> =
        fun state ->
        valueOrError, state

    //
    let setState (state : 'State) : StatefulChoiceFuncIndexed<_,_,unit,'Error> =
        // TODO : Instead of using unit input and output here, should we accept
        // any input value and pass it through?
        fun () ->
        (Choice1Of2 ()), state

    //
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
    let map (mapping : 'T -> 'U) (m : StatefulChoiceFuncIndexed<'S1, 'S2, 'T, 'Error>)
            : StatefulChoiceFuncIndexed<'S1, 'S2, 'U, 'Error> =
        bind (mapping >> ``return``) m


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
    //
    let dummy () = ()


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

