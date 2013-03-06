(*

Copyright 1994 Chris Okasaki
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
namespace ExtCore.Collections

open LanguagePrimitives
open OptimizedClosures
open ExtCore


(* INVARIANTS                                        *)
(*   1. length front >= length rear                  *)
(*   2. length pending = length front - length rear  *)
(*   3. (in the absence of insertf's)                *)
(*      pending = nthtail (front, length rear)       *)
//
type Queue<'T> private
    (front : LazyList<'T>, rear : 'T list, pending : LazyList<'T>) =
    //
    static let emptyQueue : Queue<'T> =
        Queue (
            LazyList.empty,
            [],
            LazyList.empty)

    //
    static member Empty
        with get () = emptyQueue

    /// Returns true if the given queue is empty; otherwise, false.
    member __.IsEmpty
        with get () =
            (* by Invariant 1, front = empty implies rear = [] *)
            LazyList.isEmpty front

    /// The number of elements in the queue.
    member __.GetLength () =
        (* = LazyList.length front + length rear -- by Invariant 2 *)
        LazyList.length pending + 2 * List.length rear

    /// take from front of queue
    member __.Dequeue () =
        // Preconditions
        if LazyList.isEmpty front then
            invalidOp "The queue is empty."

        LazyList.head front,
        Queue<_>.CreateQueue (
            LazyList.tail front,
            rear,
            pending)

    /// add to rear of queue
    member __.Enqueue value =
        Queue<_>.CreateQueue (
            front,
            value :: rear,
            pending)

    /// add to front of queue
    member __.EnqueueFront value =
        Queue (
            LazyList.cons value front,
            rear,
            LazyList.cons value pending)

    //
    member this.ToArray () : 'T[] =
        // OPTIMIZATION : If the queue is empty return immediately.
        if this.IsEmpty then
            Array.empty
        else
            // Instead of creating a fixed-size array with
                // Array.zeroCreate <| size queue
            // use a ResizeArray<_> so we only need to traverse the queue once.
            // TODO : Tune this for best average-case performance.
            let result = ResizeArray<_> ()

            let mutable queue = this
            while not <| queue.IsEmpty do
                let item, queue' = queue.Dequeue ()
                result.Add item
                queue <- queue'

            result.ToArray ()

    //
    member this.ToList () : 'T list =
        // OPTIMIZATION : If the queue is empty return immediately.
        if this.IsEmpty then
            List.empty
        else
            let mutable resultList = []
            let mutable queue = this
            while not <| queue.IsEmpty do
                let item, queue' = queue.Dequeue ()
                resultList <- item :: resultList
                queue <- queue'

            List.rev resultList

    //
    static member private Rotate (xs : LazyList<'T>, ys : 'T list, rys : LazyList<'T>) =
        match ys with
        | [] ->
            // This should never happen -- it's only here to satisfy
            // the F# pattern-match exhaustivity checker.
            failwith "Invariant failed."
        | y :: ys ->
            if LazyList.isEmpty xs then
                LazyList.cons y rys
            else
                LazyList.consDelayed (LazyList.head xs) <| fun () ->
                    Queue<_>.Rotate (LazyList.tail xs, ys, LazyList.cons y rys)

    //
    static member private CreateQueue (front, rear, pending) =
        if LazyList.isEmpty pending then
            (* length rear = length front + 1 *)
            let front = Queue<_>.Rotate (front, rear, LazyList.empty)
            Queue (front, [], front)
        else
            Queue (
                front,
                rear,
                LazyList.tail pending)

    // TODO
    // functions for converting to/from System.Collections.Generic.Queue?

//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Queue =
    /// Returns an empty queue of the given type.
    [<CompiledName("Empty")>]
    [<GeneralizableValue>]
    let empty<'T> : Queue<'T> =
        Queue<'T>.Empty

    /// Returns true if the given queue is empty; otherwise, false.
    let inline isEmpty (queue : Queue<'T>) =
        // Preconditions
        checkNonNull "queue" queue

        queue.IsEmpty

    /// The number of elements in the queue.
    let inline length (queue : Queue<'T>) =
        // Preconditions
        checkNonNull "queue" queue

        queue.GetLength ()

    /// add to rear of queue
    let inline enqueue value (queue : Queue<'T>) =
        // Preconditions
        checkNonNull "queue" queue

        queue.Enqueue value

    /// add to front of queue
    let inline enqueuef value (queue : Queue<'T>) =
        // Preconditions
        checkNonNull "queue" queue

        queue.EnqueueFront value

    /// take from front of queue
    let inline dequeue (queue : Queue<'T>) =
        // Preconditions
        checkNonNull "queue" queue

        queue.Dequeue ()

    //
    let inline toList (queue : Queue<'T>) : 'T list =
        // Preconditions
        checkNonNull "queue" queue
        
        queue.ToList ()

    //
    let inline toArray (queue : Queue<'T>) : 'T[] =
        // Preconditions
        checkNonNull "queue" queue
        
        queue.ToArray ()

