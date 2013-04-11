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
        : StateFunc<'State, 'T> =
        func

    // unit -> M<'T>
    member inline this.Zero ()
        : StateFunc<'State, unit> =
        this.Return ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (m : StateFunc<_, 'T>, k : 'T -> StateFunc<_,_>)
        : StateFunc<'State, 'U> =
        fun state ->
            let result, state = m state
            (k result) state

    // (unit -> M<'T>) -> M<'T>
    member inline this.Delay (f : unit -> StateFunc<_,_>)
        : StateFunc<'State, 'T> =
        this.Bind (this.Return (), f)

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline this.Combine (r1 : StateFunc<_,_>, r2 : StateFunc<_,_>)
        : StateFunc<'State, 'T> =
        this.Bind (r1, fun () -> r2)

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryWith (body : StateFunc<_,_>, handler : exn -> StateFunc<_,_>)
        : StateFunc<'State, 'T> =
        fun state ->
            try body state
            with ex ->
                handler ex state

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryFinally (body : StateFunc<_,_>, handler)
        : StateFunc<'State, 'T> =
        fun state ->
            try body state
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member inline this.Using (resource : ('T :> System.IDisposable), body : 'T -> StateFunc<_,_>)
        : StateFunc<'State, 'U> =
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
        : ReaderStateFunc<'Env, 'State, 'T> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : ReaderStateFunc<'Env, 'State, unit> =
        this.Return ()

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : ReaderStateFunc<_,_,_>, k : 'T -> ReaderStateFunc<_,_,_>)
        : ReaderStateFunc<'Env, 'State, 'U> =
        fun env state ->
            let result, state = m env state
            (k result) env state

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> ReaderStateFunc<_,_,_>)
        : ReaderStateFunc<'Env, 'State, 'T> =
        this.Bind (this.Return (), f)

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : ReaderStateFunc<_,_,_>, r2 : ReaderStateFunc<_,_,_>)
        : ReaderStateFunc<'Env, 'State, 'T> =
        this.Bind (r1, fun () -> r2)

    // M<'T> -> M<'T> -> M<'T>
    member __.TryWith (body : ReaderStateFunc<_,_,_>, handler : exn -> ReaderStateFunc<_,_,_>)
        : ReaderStateFunc<'Env, 'State, 'T> =
        fun env state ->
            try body env state
            with ex ->
                handler ex env state

    // M<'T> -> M<'T> -> M<'T>
    member __.TryFinally (body : ReaderStateFunc<_,_,_>, handler)
        : ReaderStateFunc<'Env, 'State, 'T> =
        fun env state ->
            try body env state
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : _ -> ReaderStateFunc<_,_,_>)
        : ReaderStateFunc<'Env, 'State, 'U> =
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
//        fun value ->
//            try body value
//            with ex ->
//                handler ex
//
//    // M<'T> -> M<'T> -> M<'T>
//    member this.TryFinally (body, handler) : _ option =
//        fun value ->
//            try body value
//            finally
//                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : _ -> _ option) : _ option =
        try body resource
        finally
            if not <| isNull (box resource) then
                resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : _ option) : _ option =
        if guard () then
            // OPTIMIZE : This could be simplified so we don't need to make calls to Bind and While.
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> unit option) : _ option =
        // OPTIMIZE : This could be simplified so we don't need to make calls to Using, While, Delay.
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))


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

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryWith (body : 'T -> Choice<'U, 'Error>, handler) =
        fun value ->
            try body value
            with ex ->
                handler ex

    // M<'T> -> M<'T> -> M<'T>
    member inline __.TryFinally (body : 'T -> Choice<'U, 'Error>, handler) =
        fun value ->
            try body value
            finally
                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : _ -> Choice<_,_>)
        : Choice<'U, 'Error> =
        try body resource
        finally
            if not <| isNull (box resource) then
                resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : Choice<unit, 'Error>) : Choice<_,_> =
        if guard () then
            // OPTIMIZE : This could be simplified so we don't need to make calls to Bind and While.
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> Choice<unit, 'Error>) =
        // OPTIMIZE : This could be simplified so we don't need to make calls to Using, While, Delay.
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))


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
        : ProtectedStateFunc<'State, 'T, 'Error> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : ProtectedStateFunc<'State, unit, 'Error> =
        fun state ->
        Choice1Of2 ((), state)

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> ProtectedStateFunc<_,_,_>)
        : ProtectedStateFunc<'State, 'T, 'Error> =
        fun state -> f () state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : ProtectedStateFunc<_,_,_>, k : 'T -> ProtectedStateFunc<_,_,_>)
        : ProtectedStateFunc<'State, 'U, 'Error> =
        fun state ->
        match m state with
        | Choice2Of2 error ->
            Choice2Of2 error
        | Choice1Of2 (value, state) ->
            k value state

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : ProtectedStateFunc<_,_,_>, r2 : ProtectedStateFunc<_,_,_>)
        : ProtectedStateFunc<'State, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> -> M<'T> -> M<'T>
    member __.TryWith (body : ProtectedStateFunc<_,_,_>, handler : exn -> ProtectedStateFunc<_,_,_>)
        : ProtectedStateFunc<'State, 'T, 'Error> =
        fun state ->
        try body state
        with ex ->
            handler ex state

    // M<'T> -> M<'T> -> M<'T>
    member __.TryFinally (body : ProtectedStateFunc<_,_,_>, handler)
        : ProtectedStateFunc<'State, 'T, 'Error> =
        fun state ->
        try body state
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ProtectedStateFunc<_,_,_>)
        : ProtectedStateFunc<'State, 'U, 'Error> =
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
        : ReaderProtectedStateFunc<'Env, 'State, 'T, 'Error> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : ReaderProtectedStateFunc<'Env, 'State, unit, 'Error> =
        fun env state ->
        Choice1Of2 ((), state)

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> ReaderProtectedStateFunc<_,_,_,_>)
        : ReaderProtectedStateFunc<'Env, 'State, 'T, 'Error> =
        fun env state -> f () env state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (m : ReaderProtectedStateFunc<_,_,_,_>, k : 'T -> ReaderProtectedStateFunc<_,_,_,_>)
        : ReaderProtectedStateFunc<'Env, 'State, 'U, 'Error> =
        fun env state ->
        match m env state with
        | Choice2Of2 error ->
            Choice2Of2 error
        | Choice1Of2 (value, state) ->
            k value env state

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : ReaderProtectedStateFunc<_,_,_,_>, r2 : ReaderProtectedStateFunc<_,_,_,_>)
        : ReaderProtectedStateFunc<'Env, 'State, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> -> M<'T> -> M<'T>
    member __.TryWith (body : ReaderProtectedStateFunc<_,_,_,_>, handler : exn -> ReaderProtectedStateFunc<_,_,_,_>)
        : ReaderProtectedStateFunc<'Env, 'State, 'T, 'Error> =
        fun state ->
        try body state
        with ex ->
            handler ex state

    // M<'T> -> M<'T> -> M<'T>
    member __.TryFinally (body : ReaderProtectedStateFunc<_,_,_,_>, handler)
        : ReaderProtectedStateFunc<'Env, 'State, 'T, 'Error> =
        fun state ->
        try body state
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> ReaderProtectedStateFunc<_,_,_,_>)
        : ReaderProtectedStateFunc<'Env, 'State, 'U, 'Error> =
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
        : StatefulChoiceFunc<_,_,_> =
        func

    // unit -> M<'T>
    member this.Zero ()
        : StatefulChoiceFunc<'State, unit, 'Error> =
        fun state ->
        (Choice1Of2 ()), state

    // (unit -> M<'T>) -> M<'T>
    member this.Delay (f : unit -> StatefulChoiceFunc<_,_,_>)
        : StatefulChoiceFunc<'State, 'T, 'Error> =
        fun state -> f () state

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member __.Bind (f : StatefulChoiceFunc<_,_,_>, k : 'T -> StatefulChoiceFunc<_,_,_>)
        : StatefulChoiceFunc<'State, 'U, 'Error> =
        fun state ->
        match f state with
        | (Choice1Of2 value), state ->
            k value state
        | (Choice2Of2 error), state ->
            (Choice2Of2 error), state    
        
    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member this.Combine (r1 : StatefulChoiceFunc<_,_,_>, r2 : StatefulChoiceFunc<_,_,_>)
        : StatefulChoiceFunc<'State, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    // M<'T> -> M<'T> -> M<'T>
    member __.TryWith (body : StatefulChoiceFunc<_,_,_>, handler : exn -> StatefulChoiceFunc<_,_,_>)
        : StatefulChoiceFunc<'State, 'T, 'Error> =
        fun state ->
        try body state
        with ex ->
            handler ex state

    // M<'T> -> M<'T> -> M<'T>
    member __.TryFinally (body : StatefulChoiceFunc<_,_,_>, handler)
        : StatefulChoiceFunc<'State, 'T, 'Error> =
        fun state ->
        try body state
        finally
            handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> StatefulChoiceFunc<_,_,_>)
        : StatefulChoiceFunc<'State, 'U, 'Error> =
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
[<AutoOpen>]
module WorkflowBuilders =
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
    [<CompiledName("GetState")>]
    let inline getState (env : 'Env) (state : 'State) =
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
module Choice =
    //
    [<CompiledName("BindOrRaise")>]
    let inline bindOrRaise (x : Choice<'T, #exn>) : 'T =
        match x with
        | Choice2Of2 e ->
            raise e
        | Choice1Of2 r ->
            r

    //
    [<CompiledName("BindOrFail")>]
    let inline bindOrFail (x : Choice<'T, string>) : 'T =
        match x with
        | Choice1Of2 r -> r
        | Choice2Of2 msg ->
            raise <| exn msg

    //
    [<CompiledName("SetError")>]
    let inline setError error : Choice<'T, 'Error> =
        Choice2Of2 error

    //
    [<CompiledName("Failwith")>]
    let inline failwith errorMsg : Choice<'T, string> =
        Choice2Of2 errorMsg

(*
/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ReaderChoice =
    //
    let dummy () = ()
*)

/// <summary>
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ProtectedState =
    //
    [<CompiledName("LiftState")>]
    let inline liftState (stateFunc : StateFunc<'State, 'T>)
        : ProtectedStateFunc<'State, 'T, 'Error> =
        stateFunc >> Choice1Of2

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


/// Functions for working with F# Async workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Async =
    open Microsoft.FSharp.Control

    //
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'U) (value : Async<'T>) : Async<'U> =
        async {
        // Get the input value.
        let! x = value

        // Apply the mapping function and return the result.
        return mapping x
        }

