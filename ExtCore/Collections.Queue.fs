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
type internal QueueData<'T> = {
    Front : LazyList<'T>;
    Rear : 'T list;
    Pending : LazyList<'T>;
}

//
type Queue<'T> private (data : QueueData<'T>) =
    //
    static let emptyQueue : Queue<'T> =
        Queue {
            Front = LazyList.empty;
            Rear = [];
            Pending = LazyList.empty; }

    //
    static member Empty
        with get () = emptyQueue

    /// Returns true if the given queue is empty; otherwise, false.
    member __.IsEmpty
        with get () =
            (* by Invariant 1, front = empty implies rear = [] *)
            LazyList.isEmpty data.Front

    /// The number of elements in the queue.
    member __.GetLength () =
        (* = LazyList.length front + length rear -- by Invariant 2 *)
        LazyList.length data.Pending + 2 * List.length data.Rear

    /// take from front of queue
    member __.Dequeue () =
        // Preconditions
        if LazyList.isEmpty data.Front then
            invalidArg "front" "The queue is empty."

        LazyList.head data.Front,
        Queue<_>.CreateQueue
            { data with
                Front = LazyList.tail data.Front; }

    /// add to rear of queue
    member __.Enqueue value =
        Queue<_>.CreateQueue
            { data with
                Rear = value :: data.Rear; }

    /// add to front of queue
    member __.EnqueueFront value =
        Queue {
            Front = LazyList.cons value data.Front;
            Rear = data.Rear;
            Pending = LazyList.cons value data.Pending; }

    //
    member this.ToArray () : 'T[] =
        // OPTIMIZATION : If the queue is empty, return an empty array.
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
    static member private CreateQueue { Front = front; Rear = rear; Pending = pending; } =
        if LazyList.isEmpty pending then
            (* length rear = length front + 1 *)
            let front = Queue<_>.Rotate (front, rear, LazyList.empty)
            Queue {
                Front = front;
                Rear = [];
                Pending = front; }
        else
            Queue {
                Front = front;
                Rear = rear;
                Pending = LazyList.tail pending; }

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
    let inline size (queue : Queue<'T>) =
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
    let inline toArray (queue : Queue<'T>) : 'T[] =
        // Preconditions
        checkNonNull "queue" queue
        
        queue.ToArray ()

