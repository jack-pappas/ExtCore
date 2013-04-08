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

//open System.Collections
//open System.Collections.Generic
//open System.Diagnostics.Contracts
open ExtCore


(* INVARIANTS                                        *)
(*   1. length front >= length rear                  *)
(*   2. length pending = length front - length rear  *)
(*   3. (in the absence of insertf's)                *)
(*      pending = nthtail (front, length rear)       *)
/// An immutable queue representing a first-in, first-out (FIFO) collection of objects.
[<Sealed>]
[<CompiledName("FSharpQueue`1")>]
//[<StructuredFormatDisplay("")>]
type Queue<'T> private
    (front : LazyList<'T>, rear : 'T list, pending : LazyList<'T>) =
    /// The empty queue instance.
    /// This avoids creating unnecessary instances, since all empty queues are equivalent.
    static let emptyQueue : Queue<'T> =
        Queue (
            LazyList.empty,
            [],
            LazyList.empty)

    /// The empty queue.
    static member Empty
        with get () = emptyQueue

    /// Returns true if the given Queue is empty; otherwise, false.
    member __.IsEmpty
        with get () =
            (* by Invariant 1, front = empty implies rear = [] *)
            LazyList.isEmpty front

    /// The number of elements in the Queue.
    member __.GetLength () =
        (* = LazyList.length front + length rear -- by Invariant 2 *)
        LazyList.length pending + 2 * List.length rear

    /// Removes and returns the object at the beginning of the Queue.
    member __.Dequeue () =
        // Preconditions
        if LazyList.isEmpty front then
            invalidOp "The queue is empty."

        LazyList.head front,
        Queue<_>.CreateQueue (
            LazyList.tail front,
            rear,
            pending)

    /// Returns a new Queue with the object added to the end of the Queue.
    member __.Enqueue value =
        Queue<_>.CreateQueue (
            front,
            value :: rear,
            pending)

    /// Returns a new Queue with the object added to the front of the Queue.
    member __.EnqueueFront value =
        Queue (
            LazyList.cons value front,
            rear,
            LazyList.cons value pending)

    /// Create an array containing the elements of the Queue in order.
    member this.ToArray () : 'T[] =
        // OPTIMIZATION : If the queue is empty return immediately.
        if this.IsEmpty then
            Array.empty
        else
            // OPTIMIZE : Tune this for best average-case performance.
            // Perhaps we can use the LazyList.LazyLength property to
            // estimate a better initial capacity for the ResizeArray.
            let result = ResizeArray<_> ()

            let mutable queue = this
            while not <| queue.IsEmpty do
                let item, queue' = queue.Dequeue ()
                result.Add item
                queue <- queue'

            ResizeArray.toArray result

    /// Create a list containing the elements of the Queue in order.
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

    /// Creates a Queue from the elements of a ResizeQueue.
    static member OfResizeQueue (resizeQueue : ResizeQueue<'T>) : Queue<'T> =
        // Preconditions
        checkNonNull "resizeQueue" resizeQueue

        /// A LazyList containing the elements copied from the queue.
        let queueElements =
            // Lock the ResizeQueue while copying to ensure it doesn't change.
            lock ((resizeQueue :> System.Collections.ICollection).SyncRoot) <| fun () ->
                (LazyList.empty, resizeQueue)
                ||> Seq.fold (fun lazyList el ->
                    LazyList.cons el lazyList)

        // Create a new Queue from the copied elements.
        Queue (queueElements, [], LazyList.empty)

    /// Creates a ResizeQueue from the elements of the Queue.
    member this.ToResizeQueue () : ResizeQueue<'T> =
        // OPTIMIZATION : If the queue is empty return immediately.
        if this.IsEmpty then
            ResizeQueue ()
        else
            let resizeQueue = ResizeQueue ()
            let mutable queue = this
            while not <| queue.IsEmpty do
                let item, queue' = queue.Dequeue ()
                resizeQueue.Enqueue item
                queue <- queue'

            resizeQueue

//    /// Used by Code Contracts to check that invariant contracts are met.
//    [<ContractInvariantMethod>]
//    member private this.ObjectInvariant () : unit =
//        (*   1. length front >= length rear                  *)
//        Contract.Invariant (
//            LazyList.length front >= rear.Length)
//        (*   2. length pending = length front - length rear  *)
//        Contract.Invariant (
//            LazyList.length pending = (LazyList.length front - rear.Length))
//        (*   3. (in the absence of insertf's)                *)
//        (*      pending = nthtail (front, length rear)       *)
//        // TODO : Determine the best way to implement this last invariant condition.
//        //

    /// Performs the 'rotate' operation on a Queue, given the private fields
    /// holding the queue elements.
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

    /// Creates a new Queue with the given field values.
    static member private CreateQueue (front, rear, pending) =
        if LazyList.isEmpty pending then
            (* length rear = length front + 1 *)
            let front = Queue<'T>.Rotate (front, rear, LazyList.empty)
            Queue (front, [], front)
        else
            Queue (
                front,
                rear,
                LazyList.tail pending)

/// Functional operators related to the Queue type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Queue =
    //open OptimizedClosures

    /// Returns an empty queue of the given type.
    [<CompiledName("Empty")>]
    let empty<'T> : Queue<'T> =
        Queue<'T>.Empty

    /// Returns true if the given queue is empty; otherwise, false.
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (queue : Queue<'T>) =
        // Preconditions
        checkNonNull "queue" queue

        queue.IsEmpty

    /// The number of elements in the queue.
    [<CompiledName("Length")>]
    let inline length (queue : Queue<'T>) =
        // Preconditions
        checkNonNull "queue" queue

        queue.GetLength ()

    /// Returns a new Queue with the object added to the end of the given Queue.
    [<CompiledName("Enqueue")>]
    let inline enqueue value (queue : Queue<'T>) =
        // Preconditions
        checkNonNull "queue" queue
        
        queue.Enqueue value

    /// Returns a new Queue with the object added to the front of the given Queue.
    [<CompiledName("EnqueueFront")>]
    let inline enqueueFront value (queue : Queue<'T>) =
        // Preconditions
        checkNonNull "queue" queue

        queue.EnqueueFront value

    /// Removes and returns the object at the beginning of the Queue.
    [<CompiledName("Dequeue")>]
    let inline dequeue (queue : Queue<'T>) =
        // Preconditions
        checkNonNull "queue" queue

        queue.Dequeue ()

    /// Create a list containing the elements of the Queue in order.
    [<CompiledName("ToList")>]
    let inline toList (queue : Queue<'T>) : 'T list =
        // Preconditions
        checkNonNull "queue" queue
        
        queue.ToList ()

    /// Create an array containing the elements of the Queue in order.
    [<CompiledName("ToArray")>]
    let inline toArray (queue : Queue<'T>) : 'T[] =
        // Preconditions
        checkNonNull "queue" queue
        
        queue.ToArray ()

    /// Creates a Queue from the elements of a ResizeQueue.
    [<CompiledName("OfResizeQueue")>]
    let inline ofResizeQueue (resizeQueue : ResizeQueue<'T>) : Queue<'T> =
        // Preconditions are checked within the method being called.
        Queue.OfResizeQueue resizeQueue

    /// Creates a ResizeQueue from the elements of the Queue.
    [<CompiledName("ToResizeQueue")>]
    let inline toResizeQueue (queue : Queue<'T>) : ResizeQueue<'T> =
        // Preconditions
        checkNonNull "queue" queue

        queue.ToResizeQueue ()

