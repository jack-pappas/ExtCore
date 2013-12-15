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

namespace ExtCore.Collections

//open System.Collections
//open System.Collections.Generic
//open System.Diagnostics.Contracts
open ExtCore


/// <summary>An immutable queue representing a first-in, first-out (FIFO) collection of objects.</summary>
/// <typeparam name="T">The type of elements in the queue.</typeparam>
[<Sealed>]
//[<StructuredFormatDisplay("")>]
type Queue<'T> private (front : LazyList<'T>, rear : 'T list, pending : LazyList<'T>) =
    /// The empty queue instance.
    /// This avoids creating unnecessary instances, since all empty queues are equivalent.
    static let empty : Queue<'T> =
        Queue (LazyList.empty, [], LazyList.empty)

    /// The empty queue.
    static member Empty
        with get () = empty

    /// <summary>Returns <c>true</c> if the given Queue is empty; otherwise, <c>false</c>.</summary>
    member __.IsEmpty
        with get () =
            (* by Invariant 1, front = empty implies rear = [] *)
            LazyList.isEmpty front

    /// <summary>The number of elements in the Queue.</summary>
    /// <returns></returns>
    member __.GetLength () =
        (* = LazyList.length front + length rear -- by Invariant 2 *)
        LazyList.length pending + 2 * List.length rear

    /// <summary>Removes and returns the object at the beginning of the Queue.</summary>
    /// <returns></returns>
    member __.Dequeue () =
        // Preconditions
        if LazyList.isEmpty front then
            invalidOp "The queue is empty."

        LazyList.head front,
        Queue<_>.CreateQueue (
            LazyList.tail front,
            rear,
            pending)

    /// <summary>Returns a new Queue with the object added to the end of the Queue.</summary>
    /// <param name="value"></param>
    /// <returns></returns>
    member __.Enqueue value =
        Queue<_>.CreateQueue (
            front,
            value :: rear,
            pending)

    /// <summary>Returns a new Queue with the object added to the front of the Queue.</summary>
    /// <param name="value"></param>
    /// <returns></returns>
    member __.EnqueueFront value =
        Queue (
            LazyList.cons value front,
            rear,
            LazyList.cons value pending)

    /// <summary>Creates a Queue from the elements of a list.</summary>
    /// <param name="list"></param>
    /// <returns></returns>
    static member OfList list : Queue<'T> =
        // Preconditions
        checkNonNull "list" list

        (empty, list)
        ||> List.fold (fun queue el ->
            queue.Enqueue el)

    /// <summary>Creates a Queue from the elements of an array.</summary>
    /// <param name="array"></param>
    /// <returns></returns>
    static member OfArray array : Queue<'T> =
        // Preconditions
        checkNonNull "array" array

        (empty, array)
        ||> Array.fold (fun queue el ->
            queue.Enqueue el)

    /// <summary>Create a sequence containing the elements of the Queue in order.</summary>
    /// <returns></returns>
    member this.ToSeq () : seq<'T> =
        // OPTIMIZATION : If the queue is empty return immediately.
        if this.IsEmpty then
            Seq.empty
        else
            this
            |> Seq.unfold (fun queue ->
                if queue.IsEmpty then None
                else
                    Some (queue.Dequeue ()))

    /// <summary>Create a list containing the elements of the Queue in order.</summary>
    /// <returns></returns>
    member this.ToList () : 'T list =
        // OPTIMIZATION : If the queue is empty return immediately.
        if this.IsEmpty then
            List.empty
        else
            // This is implemented using local mutability for maximum performance.
            // We dequeue the items into a ResizeArray, then traverse the ResizeArray backwards
            // to build up the resulting list.
            let result = ResizeArray<_> ()

            let mutable queue = this
            while not <| queue.IsEmpty do
                let item, queue' = queue.Dequeue ()
                result.Add item
                queue <- queue'

            let mutable resultList = []
            for i = result.Count - 1 downto 0 do
                resultList <- result.[i] :: resultList

            resultList

    /// <summary>Create an array containing the elements of the Queue in order.</summary>
    /// <returns></returns>
    member this.ToArray () : 'T[] =
        // OPTIMIZATION : If the queue is empty return immediately.
        if this.IsEmpty then
            Array.empty
        else
            let result = ResizeArray<_> ()

            let mutable queue = this
            while not <| queue.IsEmpty do
                let item, queue' = queue.Dequeue ()
                result.Add item
                queue <- queue'

            ResizeArray.toArray result

//    /// Used by Code Contracts to check that invariant contracts are met.
//    [<ContractInvariantMethod>]
//    member private this.ObjectInvariant () : unit =
//        (*   1. length front >= length rear                  *)
//        Contract.Invariant (LazyList.length front >= rear.Length)
//        (*   2. length pending = length front - length rear  *)
//        Contract.Invariant (LazyList.length pending = (LazyList.length front - rear.Length))
//        (*   3. (in the absence of insertf's)                *)
//        (*      pending = nthtail (front, length rear)       *)
//        // TODO : Determine the best way to implement this last invariant condition.
//        //

    /// Performs the 'rotate' operation on a Queue, given the private fields holding the queue elements.
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
            Queue (front, rear, LazyList.tail pending)

    interface System.Collections.IEnumerable with
        /// <inherit />
        member this.GetEnumerator () =
            this.ToSeq().GetEnumerator() :> System.Collections.IEnumerator

    interface System.Collections.Generic.IEnumerable<'T> with
        /// <inherit />
        member this.GetEnumerator () =
            this.ToSeq().GetEnumerator()


/// Functional operators related to the Queue type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Queue =
    //open OptimizedClosures

    /// <summary>Returns an empty queue of the given type.</summary>
    [<CompiledName("Empty")>]
    [<GeneralizableValue>]
    let empty<'T> : Queue<'T> =
        Queue<'T>.Empty

    /// <summary>Returns true if the given queue is empty; otherwise, false.</summary>
    /// <param name="queue"></param>
    /// <returns></returns>
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (queue : Queue<'T>) =
        // Preconditions
        checkNonNull "queue" queue

        queue.IsEmpty

    /// <summary>The number of elements in the queue.</summary>
    /// <param name="queue"></param>
    /// <returns></returns>
    [<CompiledName("Length")>]
    let inline length (queue : Queue<'T>) =
        // Preconditions
        checkNonNull "queue" queue

        queue.GetLength ()

    /// <summary>Returns a new Queue with the object added to the end of the given Queue.</summary>
    /// <param name="value"></param>
    /// <param name="queue"></param>
    /// <returns></returns>
    [<CompiledName("Enqueue")>]
    let inline enqueue value (queue : Queue<'T>) =
        // Preconditions
        checkNonNull "queue" queue
        
        queue.Enqueue value

    /// <summary>Returns a new Queue with the object added to the front of the given Queue.</summary>
    /// <param name="value"></param>
    /// <param name="queue"></param>
    /// <returns></returns>
    [<CompiledName("EnqueueFront")>]
    let inline enqueueFront value (queue : Queue<'T>) =
        // Preconditions
        checkNonNull "queue" queue

        queue.EnqueueFront value

    /// <summary>Removes and returns the object at the beginning of the Queue.</summary>
    /// <param name="queue"></param>
    /// <returns></returns>
    [<CompiledName("Dequeue")>]
    let inline dequeue (queue : Queue<'T>) =
        // Preconditions
        checkNonNull "queue" queue

        queue.Dequeue ()

    /// <summary>Creates a Queue from the elements of a list.</summary>
    /// <param name="list"></param>
    /// <returns></returns>
    [<CompiledName("OfList")>]
    let inline ofList list : Queue<'T> =
        // Preconditions are checked within the method being called.
        Queue.OfList list

    /// <summary>Creates a Queue from the elements of an array.</summary>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("OfArray")>]
    let inline ofArray array : Queue<'T> =
        // Preconditions are checked within the method being called.
        Queue.OfArray array

    /// <summary>Create a sequence containing the elements of the Queue in order.</summary>
    /// <param name="queue"></param>
    /// <returns></returns>
    [<CompiledName("ToSeq")>]
    let inline toSeq (queue : Queue<'T>) : seq<'T> =
        // Preconditions
        checkNonNull "queue" queue
        
        queue.ToSeq ()

    /// <summary>Create a list containing the elements of the Queue in order.</summary>
    /// <param name="queue"></param>
    /// <returns></returns>
    [<CompiledName("ToList")>]
    let inline toList (queue : Queue<'T>) : 'T list =
        // Preconditions
        checkNonNull "queue" queue
        
        queue.ToList ()

    /// <summary>Create an array containing the elements of the Queue in order.</summary>
    /// <param name="queue"></param>
    /// <returns></returns>
    [<CompiledName("ToArray")>]
    let inline toArray (queue : Queue<'T>) : 'T[] =
        // Preconditions
        checkNonNull "queue" queue
        
        queue.ToArray ()
