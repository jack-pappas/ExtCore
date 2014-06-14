(*

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

namespace ExtCore.Collections

open System
open System.Threading
open System.IO
open ExtCore
open ExtCore.Control
open ExtCore.Control.Agents


/// The internal type that represents a value returned as a result of
/// evaluating a step of an asynchronous sequence.
//[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type AsyncSeqItem<'T> =
    private
    | Nil
    | Cons of 'T * AsyncSeq<'T>

/// An asynchronous sequence represents a delayed computation that can be
/// started to produce either Cons value consisting of the next element of the 
/// sequence (head) together with the next asynchronous sequence (tail) or a 
/// special value representing the end of the sequence (Nil)
and AsyncSeq<'T> = Async<AsyncSeqItem<'T>>

/// Computation builder that allows creating of asynchronous
/// sequences using the 'asyncSeq { ... }' syntax.
type AsyncSeqBuilder () =
    //
    member __.Zero () : AsyncSeq<'T> =
        async { return Nil }

    // This looks weird, but it is needed to allow:
    //
    //   while foo do
    //       do! something
    //
    // because F# translates body as Bind(something, fun () -> Return())
    member this.Return () : AsyncSeq<'T> =
        this.Zero ()

    //
    member (*inline*) __.Bind (source : Async<'T>, body : 'T -> AsyncSeq<'U>) : AsyncSeq<'U> =
        async.Bind (source, body)

    //
    member (*inline*) __.Delay (generator : unit -> AsyncSeq<'T>) : AsyncSeq<_> =
        async.Delay generator
        
    //
    member this.Yield (value : 'T) : AsyncSeq<_> =
        async { return Cons (value, this.Zero ()) }
        
    member __.YieldFrom source : AsyncSeq<'T> = source

    //
    member this.Combine (source1 : AsyncSeq<'T>, source2 : AsyncSeq<'T>) : AsyncSeq<_> =
        async {
        let! v1 = source1
        match v1 with
        | Nil ->
            return! source2
        | Cons (hd, tl) ->
            return Cons (hd, this.Combine (tl, source2))
        }

    //
    member this.While (guard, source : AsyncSeq<'T>) : AsyncSeq<_> =
        if guard () then
            this.Combine (source, this.Delay (fun () -> this.While (guard, source)))
        else this.Zero ()

//
[<AutoOpen>]
module AsyncSeqExtensions =
    // Add asynchronous for loop to the built-in 'async' computation builder.
    type Microsoft.FSharp.Control.AsyncBuilder with
        //
        member this.For (source : AsyncSeq<'T>, action : 'T -> Async<unit>) =
            async.Bind (source, function
                | Nil ->
                    async.Zero ()
                | Cons (hd, tl) ->
                    async.Combine (action hd, this.For (tl, action)))
    
    /// Builds an asynchronous sequence using the computation builder syntax.
    let asyncSeq = AsyncSeqBuilder ()

    /// Tries to get the next element of an asynchronous sequence
    /// and returns either the value or an exception.
    // TODO : Simplify this to use the built-in Async.Catch method.
    let private tryNext (source : AsyncSeq<_>) =
        async {
        try
            let! v = source
            return Choice1Of2 v
        with ex ->
            return Choice2Of2 ex
        }

    // Add additional methods to the 'asyncSeq' computation builder
    type AsyncSeqBuilder with
        //
        member this.TryFinally (source : AsyncSeq<'T>, compensation) =
            asyncSeq {
            let! v = tryNext source
            match v with
            | Choice1Of2 Nil ->
                compensation ()
            | Choice1Of2 (Cons (hd, tl)) ->
                yield hd
                yield! this.TryFinally (tl, compensation)
            | Choice2Of2 e ->
                compensation ()
                yield! raise e
            }

        //
        member this.TryWith (source : AsyncSeq<_>, handler : exn -> AsyncSeq<_>) =
            asyncSeq {
            let! v = tryNext source
            match v with
            | Choice1Of2 Nil -> ()
            | Choice1Of2 (Cons (hd, tl)) ->
                yield hd
                yield! this.TryWith (tl, handler)
            | Choice2Of2 rest ->
                yield! handler rest
            }

        //
        member this.Using (resource : #IDisposable, binder) =
            this.TryFinally (binder resource, fun () ->
                if box resource <> null then resource.Dispose ())

        /// For loop that iterates over a synchronous sequence (and generates
        /// all elements generated by the asynchronous body)
        member this.For (source : seq<'T>, action : 'T -> AsyncSeq<'TResult>) =
            let enum = source.GetEnumerator ()
            this.TryFinally (
                this.While (
                    (fun () -> enum.MoveNext ()),
                    this.Delay (fun () -> action enum.Current)),
                (fun () -> if enum <> null then enum.Dispose ()))

        /// Asynchronous for loop - for all elements from the input sequence,
        /// generate all elements produced by the body (asynchronously). See
        /// also the AsyncSeq.collect function.
        member this.For (source : AsyncSeq<'T>, mapping : 'T -> AsyncSeq<'TResult>) =
            asyncSeq {
            let! v = source
            match v with
            | Nil -> ()
            | Cons (hd, tl) ->
                yield! mapping hd
                yield! this.For (tl, mapping)
            }


/// Module with helper functions for working with asynchronous sequences
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AsyncSeq =
    /// Creates an empty asynchronous sequence that immediately ends
    [<GeneralizableValue>]
    [<CompiledName("Empty")>]
    let empty<'T> : AsyncSeq<'T> =
        asyncSeq.Zero ()

    /// Creates an asynchronous sequence that generates a single element and then ends
    [<CompiledName("Singleton")>]
    let singleton (value : 'T) : AsyncSeq<'T> =
        asyncSeq.Yield value

    /// Asynchronously returns the first element that was generated by the
    /// given asynchronous sequence (or the specified default value).
    [<CompiledName("FirstOrDefault")>]
    let firstOrDefault defaultValue (source : AsyncSeq<'T>) =
        // Preconditions
        checkNonNull "source" source

        async {
        let! v = source
        match v with
        | Nil ->
            return defaultValue
        | Cons (hd, _) ->
            return hd }

    /// Asynchronously returns the last element that was generated by the
    /// given asynchronous sequence (or the specified default value).
    [<CompiledName("LastOrDefault")>]
    let rec lastOrDefault defaultValue (source : AsyncSeq<'T>) =
        // Preconditions
        checkNonNull "source" source

        async {
        let! v = source
        match v with
        | Nil ->
            return defaultValue
        | Cons (hd, tl) ->
            return! lastOrDefault hd tl }

    /// Yields all elements of the first asynchronous sequence and then 
    /// all elements of the second asynchronous sequence.
    [<CompiledName("Append")>]
    let rec append (source1 : AsyncSeq<'T>) (source2 : AsyncSeq<'T>) : AsyncSeq<'T> =
        // Preconditions
        checkNonNull "source1" source1
        checkNonNull "source2" source2

        asyncSeq.Combine (source1, source2)

    /// Creates an asynchronous sequence that iterates over the given input sequence.
    /// For every input element, it calls the the specified function and iterates
    /// over all elements generated by that asynchronous sequence.
    /// This is the 'bind' operation of the computation expression (exposed using
    /// the 'for' keyword in asyncSeq computation).
    [<CompiledName("Collect")>]
    let rec collect mapping (source : AsyncSeq<'T>) : AsyncSeq<'U> =
        // Preconditions
        checkNonNull "source" source

        asyncSeq.For (source, mapping)

    // --------------------------------------------------------------------------
    // Additional combinators (implemented as async/asyncSeq computations)

    /// Builds a new asynchronous sequence whose elements are generated by 
    /// applying the specified function to all elements of the input sequence.
    ///
    /// The specified function is asynchronous (and the input sequence will
    /// be asked for the next element after the processing of an element completes).
    [<CompiledName("Map")>]
    let map mapping (source : AsyncSeq<'T>) : AsyncSeq<'U> =
        // Preconditions
        checkNonNull "source" source

        asyncSeq {
        for itm in source do
            let! v = mapping itm
            yield v
        }

    /// Asynchronously iterates over the input sequence and generates 'x' for 
    /// every input element for which the specified asynchronous function 
    /// returned 'Some(x)' 
    ///
    /// The specified function is asynchronous (and the input sequence will
    /// be asked for the next element after the processing of an element completes).
    [<CompiledName("Choose")>]
    let choose chooser (source : AsyncSeq<'T>) : AsyncSeq<'U> =
        // Preconditions
        checkNonNull "source" source

        asyncSeq {
        for itm in source do
            let! v = chooser itm
            match v with
            | None -> ()
            | Some v ->
                yield v
        }

    /// Builds a new asynchronous sequence whose elements are those from the
    /// input sequence for which the specified function returned true.
    ///
    /// The specified function is asynchronous (and the input sequence will
    /// be asked for the next element after the processing of an element completes).
    [<CompiledName("Filter")>]
    let filter predicate (source : AsyncSeq<'T>) : AsyncSeq<'T> =
        // Preconditions
        checkNonNull "source" source

        asyncSeq {
        for v in source do
            let! b = predicate v
            if b then
                yield v }

    /// Aggregates the elements of the input asynchronous sequence using the
    /// specified 'aggregation' function. The result is an asynchronous
    /// sequence of intermediate aggregation result.
    ///
    /// The aggregation function is asynchronous (and the input sequence will
    /// be asked for the next element after the processing of an element completes).
    [<CompiledName("Scan")>]
    let rec scan folder (state : 'State) (source : AsyncSeq<'T>) : AsyncSeq<'State> =
        // Preconditions
        checkNonNull "source" source

        asyncSeq {
        let! v = source
        match v with
        | Nil -> ()
        | Cons (hd, tl) ->
            let! v = folder state hd
            yield v
            yield! scan folder v tl }

    /// Iterates over the input sequence and calls the specified function for
    /// every value (to perform some side-effect asynchronously).
    ///
    /// The specified function is asynchronous (and the input sequence will
    /// be asked for the next element after the processing of an element completes).
    [<CompiledName("Iter")>]
    let rec iter action (source : AsyncSeq<'T>) =
        // Preconditions
        checkNonNull "source" source

        async {
        for itm in source do
            do! action itm }

    /// Aggregates the elements of the input asynchronous sequence using the
    /// specified 'aggregation' function. The result is an asynchronous 
    /// workflow that returns the final result.
    ///
    /// The aggregation function is asynchronous (and the input sequence will
    /// be asked for the next element after the processing of an element completes).
    [<CompiledName("Fold")>]
    let rec fold folder (state : 'State) (source : AsyncSeq<'T>) =
        // Preconditions
        checkNonNull "source" source

        source |> scan folder state |> lastOrDefault state

    /// Returns an asynchronous sequence that returns pairs containing an element
    /// from the input sequence and its predecessor. Empty sequence is returned for
    /// singleton input sequence.
    [<CompiledName("Pairwise")>]
    let rec pairwise (source : AsyncSeq<'T>) : AsyncSeq<'T * 'T> =
        // Preconditions
        checkNonNull "source" source

        asyncSeq {
        let! v = source
        match v with
        | Nil -> ()
        | Cons (hd, tl) ->
            let prev = ref hd
            for v in tl do
                yield (!prev, v)
                prev := v }

    /// Combines two (2) asynchronous sequences into a sequence of pairs. 
    /// The values from sequences are retrieved in parallel.
    [<CompiledName("Zip")>]
    let rec zip (source1 : AsyncSeq<'T1>) (source2 : AsyncSeq<'T2>) : AsyncSeq<_> =
        // Preconditions
        checkNonNull "source1" source1
        checkNonNull "source2" source2

        async {
        let! ft = Async.StartChild source1
        let! s = source2
        let! f = ft
        match f, s with
        | Cons (h1, t1), Cons (h2, t2) ->
            return Cons ((h1, h2), zip t1 t2)
        | _ -> return Nil }

    /// Combines three (3) asynchronous sequences into a sequence of triples. 
    /// The values from sequences are retrieved in parallel.
    [<CompiledName("Zip3")>]
    let rec zip3 (source1 : AsyncSeq<'T1>) (source2 : AsyncSeq<'T2>) (source3 : AsyncSeq<'T3>) : AsyncSeq<_> =
        // Preconditions
        checkNonNull "source1" source1
        checkNonNull "source2" source2
        checkNonNull "source3" source3

        async {
        let! ft = Async.StartChild source1
        let! s = source2
        let! t = source3
        let! f = ft
        match f, s, t with
        | Cons (h1, t1), Cons (h2, t2), Cons (h3, t3) ->
            return Cons ((h1, h2, h3), zip3 t1 t2 t3)
        | _ -> return Nil }

    /// Returns the first N elements of an asynchronous sequence
    [<CompiledName("Take")>]
    let rec take count (source : AsyncSeq<'T>) : AsyncSeq<'T> =
        // Preconditions
        checkNonNull "source" source

        async {
        if count > 0 then
            let! v = source
            match v with
            | Nil ->
                return Nil
            | Cons (hd, tl) ->
                return Cons (hd, take (count - 1) tl)
        else return Nil }

    /// Returns elements from an asynchronous sequence while the specified 
    /// predicate holds. The predicate is evaluated asynchronously.
    [<CompiledName("TakeWhile")>]
    let rec takeWhile predicate (source : AsyncSeq<'T>) : AsyncSeq<'T> =
        // Preconditions
        checkNonNull "source" source

        async {
        let! v = source
        match v with
        | Nil ->
            return Nil
        | Cons (hd, tl) ->
            let! res = predicate hd
            if res then
                return Cons (hd, takeWhile predicate tl)
            else return Nil }

    /// Skips the first N elements of an asynchronous sequence and
    /// then returns the rest of the sequence unmodified.
    [<CompiledName("Skip")>]
    let rec skip count (source : AsyncSeq<'T>) : AsyncSeq<'T> =
        // Preconditions
        checkNonNull "source" source

        async {
        if count > 0 then
            let! v = source
            match v with
            | Nil ->
                return Nil
            | Cons (_, tl) ->
                return! skip (count - 1) tl
        else return! source }

    /// Skips elements from an asynchronous sequence while the specified 
    /// predicate holds and then returns the rest of the sequence. The 
    /// predicate is evaluated asynchronously.
    [<CompiledName("SkipWhile")>]
    let rec skipWhile predicate (source : AsyncSeq<'T>) : AsyncSeq<'T> =
        // Preconditions
        checkNonNull "source" source

        async {
        let! v = source
        match v with
        | Nil ->
            return Nil
        | Cons (hd, tl) ->
            let! res = predicate hd
            if res then
                return! skipWhile predicate tl
            else return! tl }

    /// Create a new asynchronous sequence that caches all elements of the 
    /// sequence specified as the input. When accessing the resulting sequence
    /// multiple times, the input will still be evaluated only once
    [<CompiledName("Cache")>]
    let rec cache (source : AsyncSeq<'T>) : AsyncSeq<'T> =
        // Preconditions
        checkNonNull "source" source

        let agent = Agent<AsyncReplyChannel<_>>.Start <| fun agent ->
            async {
            let! repl = agent.Receive ()
            let! next = source
            let res =
                match next with 
                | Nil -> Nil
                | Cons (hd, tl) ->
                    Cons (hd, cache tl)
            repl.Reply res
            while true do
                let! repl = agent.Receive ()
                repl.Reply res }

        //
        async { return! agent.PostAndAsyncReply id }

    // --------------------------------------------------------------------------
    // Converting from/to synchronous sequences or IObservables

    /// Creates an asynchronous sequence that lazily takes element from an
    /// input synchronous sequence and returns them one-by-one.
    [<CompiledName("OfSeq")>]
    let ofSeq (source : seq<'T>) : AsyncSeq<'T> =
        // Preconditions
        checkNonNull "source" source

        asyncSeq {
        for el in source do
            yield el }

    /// Converts asynchronous sequence to a synchronous blocking sequence.
    /// The elements of the asynchronous sequence are consumed lazily.
    [<CompiledName("ToSeq")>]
    let toSeq (source : AsyncSeq<'T>) =
        // Preconditions
        checkNonNull "source" source

        // Write all elements to a blocking buffer and then add None to denote end
        let buf = BlockingQueueAgent<_> (1)
        async {
            do! iter (Some >> buf.AsyncAdd) source
            do! buf.AsyncAdd None }
        |> Async.Start

        // Read elements from the blocking buffer & return a sequences
        let rec loop () =
            seq {
            match buf.Get () with
            | None -> ()
            | Some value ->
                yield value
                yield! loop () }
        loop ()

    /// A helper type for implementation of buffering when converting 
    /// observable to an asynchronous sequence
    type private BufferMessage<'T> =
        | Get of AsyncReplyChannel<'T>
        | Put of 'T

    /// Converts observable to an asynchronous sequence using an agent with
    /// a body specified as the argument. The returnd async sequence repeatedly 
    /// sends 'Get' message to the agent to get the next element. The observable
    /// sends 'Put' message to the agent (as new inputs are generated).
    let private ofObservableUsingAgent (input : System.IObservable<'T>) body : AsyncSeq<'T> =
        // Preconditions
        checkNonNull "input" input

        asyncSeq {
        use agent = AutoCancelAgent.Start body
        use d =
            input
            |> Observable.asUpdates
            |> Observable.subscribe (Put >> agent.Post)
      
        let rec loop () =
            asyncSeq {
            let! msg = agent.PostAndAsyncReply Get
            match msg with
            | Notification.Completed -> ()
            | Notification.Error e ->
                raise e
            | Notification.Next v ->
                yield v
                yield! loop() }

        yield! loop() }

    /// Converts observable to an asynchronous sequence. Values that are produced
    /// by the observable while the asynchronous sequence is blocked are stored to 
    /// an unbounded buffer and are returned as next elements of the async sequence.
    [<CompiledName("FromObservableBuffered")>]
    let ofObservableBuffered (input : System.IObservable<'T>) : AsyncSeq<'T> =
        // Preconditions
        checkNonNull "input" input

        ofObservableUsingAgent input <| fun mbox ->
            async {
            let buffer = System.Collections.Generic.Queue<_> ()
            let replies = System.Collections.Generic.Queue<_> ()
            while true do
                // Receive next message (when observable ends, caller will
                // cancel the agent, so we need timeout to allow cancleation)
                let! msg = mbox.TryReceive 200
                match msg with
                | None -> ()
                | Some (Put value) ->
                    buffer.Enqueue value
                | Some (Get reply) ->
                    replies.Enqueue reply
                
                // Process matching calls from buffers
                while buffer.Count > 0 && replies.Count > 0 do
                    buffer.Dequeue ()
                    |> replies.Dequeue().Reply
            }

    /// Converts observable to an asynchronous sequence. Values that are produced
    /// by the observable while the asynchronous sequence is blocked are discarded
    /// (this function doesn't guarantee that asynchronou ssequence will return 
    /// all values produced by the observable)
    [<CompiledName("FromObservable")>]
    let ofObservable (input : System.IObservable<'T>) : AsyncSeq<'T> =
        // Preconditions
        checkNonNull "input" input

        ofObservableUsingAgent input <| fun mbox ->
            async {
            while true do
                // Allow timeout (when the observable ends, caller will
                // cancel the agent, so we need timeout to allow cancellation)
                let! msg = mbox.TryReceive 200
                match msg with
                | None
                | Some (Put _) ->
                    ()  // Ignore put or no message.
                | Some (Get repl) ->
                    // Reader is blocked, so next will be Put
                    // (caller will not stop the agent at this point,
                    // so timeout is not necessary)
                    let! v = mbox.Receive ()
                    match v with
                    | Put v ->
                        repl.Reply v
                    | _ ->
                        failwith "Unexpected Get" }

    /// Converts asynchronous sequence to an IObservable<_>. When the client subscribes
    /// to the observable, a new copy of asynchronous sequence is started and is 
    /// sequentially iterated over (at the maximal possible speed). Disposing of the 
    /// observer cancels the iteration over asynchronous sequence.
    [<CompiledName("ToObservable")>]
    let toObservable (source : AsyncSeq<'T>) =
        // Preconditions
        checkNonNull "source" source

        let start (obs : IObserver<_>) =
            async {
            try
                for v in source do
                    obs.OnNext v
                obs.OnCompleted ()
            with ex ->
                obs.OnError ex }
            |> Async.StartDisposable

        { new IObservable<_> with
            member x.Subscribe observer =
                // Preconditions
                checkNonNull "observer" observer

                start observer }

(*
//
module Seq = 
    open ExtCore.Control

    /// Converts asynchronous sequence to a synchronous blocking sequence.
    /// The elements of the asynchronous sequence are consumed lazily.
    let ofAsyncSeq (input : AsyncSeq<'T>) =
        AsyncSeq.toBlockingSeq input

*)
