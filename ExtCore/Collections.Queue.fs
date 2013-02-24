(*

Copyright ____ Chris Okasaki
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


//
type QueueData<'T> = {
    Front : Stream<'T>;
    Rear : 'T list;
    Pending : Stream<'T>;
}

//
type Queue<'T> = Queue of QueueData<'T>

//
module Queue =
    open Stream

    (* INVARIANTS                                        *)
    (*   1. length front >= length rear                  *)
    (*   2. length pending = length front - length rear  *)
    (*   3. (in the absence of insertf's)                *)
    (*      pending = nthtail (front, length rear)       *)

    //
    let rec private rotate (xs, ys, rys) =
        match ys with
        | [] ->
            // This should never happen -- it's only here to satisfy
            // the F# pattern-match exhaustivity checker.
            failwith "Invariant failed."
        | y::ys ->
            if Stream.isEmpty xs then
                cons (y,rys)
            else lcons (head xs,
                          fun () -> rotate (tail xs, ys, cons (y, rys)))

    (* Psuedo-constructor that enforces invariant *)
    (*   always called with length pending = length front - length rear + 1 *)
    let private queue { Front = front; Rear = rear; Pending = pending; } =
        if Stream.isEmpty pending then
            (* length rear = length front + 1 *)
            let front = rotate (front, rear, Stream.empty)
            Queue { Front = front; Rear = []; Pending = front; }
        else
            Queue { Front = front; Rear = rear; Pending = tail pending; }

    
    exception Empty

    /// Returns an empty queue of the given type.
    let empty : Queue<'T> =
        Queue { Front = Stream.empty; Rear = []; Pending = Stream.empty; }

    /// Returns true if the given queue is empty; otherwise, false.
    let isEmpty (Queue { Front = front } : Queue<'T>) =
        (* by Invariant 1, front = empty implies rear = [] *)
        Stream.isEmpty front

    let size (Queue { Front = front; Rear = rear; Pending = pending; } : Queue<'T>) =
        (* = Stream.size front + length rear -- by Invariant 2 *)
        Stream.size pending + 2 * List.length rear

    /// add to rear of queue
    let enqueue x (Queue { Front = front; Rear = rear; Pending = pending; } : Queue<'T>) =
        queue { Front = front; Rear = x :: rear; Pending = pending; }

    /// add to front of queue
    let enqueuef x (Queue { Front = front; Rear = rear; Pending = pending; } : Queue<'T>) =
        Queue { Front = cons (x, front); Rear = rear; Pending = cons (x, pending); }

    /// take from front of queue
    let dequeue (Queue { Front = front; Rear = rear; Pending = pending; } : Queue<'T>) =
        if Stream.isEmpty front then
            raise Empty
        else
            head front,
            queue { Front = tail front; Rear = rear; Pending = pending; }

    //
    let toArray (queue : Queue<'T>) : 'T[] =
        // OPTIMIZATION : If the queue is empty, return an empty array.
        if isEmpty queue then
            Array.empty
        else
            // Instead of creating a fixed-size array with
                // Array.zeroCreate <| size queue
            // use a ResizeArray<_> so we only need to traverse the queue once.
            // TODO : Tune this for best average-case performance.
            let result = ResizeArray<_> ()

            let mutable queue = queue
            while not <| isEmpty queue do
                let item, queue' = dequeue queue
                result.Add item
                queue <- queue'

            result.ToArray ()

