(*

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

/// Unit tests for the ExtCore.Collections.Queue type and module.
module Tests.ExtCore.Collections.Queue

open NUnit.Framework
open FsUnit
//open FsCheck


[<Test>]
let isEmpty () : unit =
    Queue.empty
    |> Queue.isEmpty
    |> should be True

    Queue.empty
    |> Queue.enqueue "Hello"
    |> Queue.enqueue "World!"
    |> Queue.isEmpty
    |> should be False

[<Test>]
let length () : unit =
    Queue.empty
    |> Queue.length
    |> assertEqual 0

    Queue.empty
    |> Queue.enqueue "foo"
    |> Queue.enqueue "bar"
    |> Queue.enqueue "baz"
    |> Queue.length
    |> assertEqual 3

[<Test>]
let enqueue () : unit =
    Queue.empty
    |> Queue.enqueue "Hello"
    |> Queue.toArray
    |> assertEqual
        [| "Hello" |]

    [| "foo"; "bar"; "baz"; |]
    |> Queue.ofArray
    |> Queue.enqueue "cdr"
    |> Queue.enqueue "car"
    |> Queue.enqueue "bar"
    |> Queue.enqueue "bar"
    |> Queue.toArray
    |> assertEqual
        [| "foo"; "bar"; "baz"; "cdr"; "car"; "bar"; "bar"; |]

    // Test case for checking that the Queue is persistent as expected.
    do
        let queue = Queue.ofArray [| "foo"; "bar"; "baz"; |]

        queue
        |> Queue.enqueue "Hello"
        |> Queue.enqueue "World"
        |> Queue.toArray
        |> assertEqual
            [| "foo"; "bar"; "baz"; "Hello"; "World"; |]

        queue
        |> Queue.enqueue "cdr"
        |> Queue.enqueue "car"
        |> Queue.toArray
        |> assertEqual
            [| "foo"; "bar"; "baz"; "cdr"; "car"; |]

[<Test>]
let enqueueFront () : unit =
    Queue.empty
    |> Queue.enqueueFront "Hello"
    |> Queue.toArray
    |> assertEqual
        [| "Hello" |]
    
    Queue.empty
    |> Queue.enqueue "foo"
    |> Queue.enqueue "bar"
    |> Queue.enqueueFront "baz"
    |> Queue.enqueue "cdr"
    |> Queue.enqueueFront "car"
    |> Queue.toArray
    |> assertEqual
        [| "car"; "baz"; "foo"; "bar"; "cdr"; |]

    // Test case for checking that the Queue is persistent as expected.
    do
        let queue = Queue.ofArray [| "foo"; "bar"; "baz"; |]

        queue
        |> Queue.enqueueFront "Hello"
        |> Queue.enqueueFront "World"
        |> Queue.toArray
        |> assertEqual
            [| "World"; "Hello"; "foo"; "bar"; "baz"; |]

        queue
        |> Queue.enqueueFront "cdr"
        |> Queue.enqueueFront "car"
        |> Queue.toArray
        |> assertEqual
            [| "car"; "cdr"; "foo"; "bar"; "baz"; |]

[<Test>]
let dequeue () : unit =
    do
        let queue = Queue.ofArray [| "car"; "baz"; "foo"; "bar"; "cdr"; |]

        let result, queue = Queue.dequeue queue
        result |> assertEqual "car"
        Queue.length queue |> assertEqual 4

        let result, queue = Queue.dequeue queue
        result |> assertEqual "baz"
        Queue.length queue |> assertEqual 3

        let result, queue = Queue.dequeue queue
        result |> assertEqual "foo"
        Queue.length queue |> assertEqual 2

        let result, queue = Queue.dequeue queue
        result |> assertEqual "bar"
        Queue.length queue |> assertEqual 1

        let result, queue = Queue.dequeue queue
        result |> assertEqual "cdr"
        Queue.length queue |> assertEqual 0

    // Test case for checking that the Queue is persistent as expected.
    do
        let queue = Queue.ofArray [| "car"; "baz"; "foo"; "bar"; "cdr"; |]

        // Run a simple dequeue test.
        let result, queue = Queue.dequeue queue
        result |> assertEqual "car"
        Queue.length queue |> assertEqual 4

        let result, queue = Queue.dequeue queue
        result |> assertEqual "baz"
        Queue.length queue |> assertEqual 3

        // Change to queue' here, so we can use the "partial" queue later.
        let result, queue' = Queue.dequeue queue
        result |> assertEqual "foo"
        Queue.length queue' |> assertEqual 2

        let result, queue' = Queue.dequeue queue'
        result |> assertEqual "bar"
        Queue.length queue' |> assertEqual 1

        let result, queue' = Queue.dequeue queue'
        result |> assertEqual "cdr"
        Queue.length queue' |> assertEqual 0

        // Now re-run the test for the last few elements using the partial queue.
        let result, queue = Queue.dequeue queue
        result |> assertEqual "foo"
        Queue.length queue |> assertEqual 2

        let result, queue = Queue.dequeue queue
        result |> assertEqual "bar"
        Queue.length queue |> assertEqual 1

        let result, queue = Queue.dequeue queue
        result |> assertEqual "cdr"
        Queue.length queue |> assertEqual 0

[<Test; ExpectedException(typeof<System.InvalidOperationException>)>]
let ``dequeue raises exn when queue is empty`` () : unit =
    Queue.empty
    |> Queue.dequeue
    |> ignore

[<Test>]
let ofList () : unit =
    List.empty
    |> Queue.ofList
    |> Queue.isEmpty
    |> should be True

    ["foo"; "bar"; "baz"; "cdr"; "car"]
    |> Queue.ofList
    |> Queue.toSeq
    |> Seq.toArray
    |> assertEqual
        [| "foo"; "bar"; "baz"; "cdr"; "car"; |]

[<Test>]
let ofArray () : unit =
    List.empty
    |> Queue.ofList
    |> Queue.isEmpty
    |> should be True

    [| "foo"; "bar"; "baz"; "cdr"; "car"; |]
    |> Queue.ofArray
    |> Queue.toSeq
    |> Seq.toArray
    |> assertEqual
        [| "foo"; "bar"; "baz"; "cdr"; "car"; |]

[<Test>]
let toSeq () : unit =
    Queue.empty
    |> Queue.toSeq
    |> Seq.isEmpty
    |> should be True

    Queue.empty
    |> Queue.enqueue "foo"
    |> Queue.enqueue "bar"
    |> Queue.enqueue "baz"
    |> Queue.dequeue
    |> snd
    |> Queue.enqueue "cdr"
    |> Queue.enqueue "car"
    |> Queue.toSeq
    |> Seq.toArray
    |> assertEqual
        [| "bar"; "baz"; "cdr"; "car"; |]

[<Test>]
let toList () : unit =
    Queue.empty
    |> Queue.toList
    |> List.isEmpty
    |> should be True

    Queue.empty
    |> Queue.enqueue "foo"
    |> Queue.enqueue "bar"
    |> Queue.enqueue "baz"
    |> Queue.dequeue
    |> snd
    |> Queue.enqueue "cdr"
    |> Queue.enqueue "car"
    |> Queue.toList
    |> assertEqual
        ["bar"; "baz"; "cdr"; "car"]

[<Test>]
let toArray () : unit =
    Queue.empty
    |> Queue.toArray
    |> Array.isEmpty
    |> should be True

    Queue.empty
    |> Queue.enqueue "foo"
    |> Queue.enqueue "bar"
    |> Queue.enqueue "baz"
    |> Queue.dequeue
    |> snd
    |> Queue.enqueue "cdr"
    |> Queue.enqueue "car"
    |> Queue.toArray
    |> assertEqual
        [| "bar"; "baz"; "cdr"; "car"; |]
