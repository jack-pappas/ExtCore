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
module ExtCore.Collections.Queue.Tests

open NUnit.Framework
open FsUnit
//open FsCheck


[<TestCase>]
let isEmpty () : unit =
    Queue.empty
    |> Queue.isEmpty
    |> should be True

    Queue.empty
    |> Queue.enqueue "Hello"
    |> Queue.enqueue "World!"
    |> Queue.isEmpty
    |> should be False

[<TestCase>]
let length () : unit =
    Queue.empty
    |> Queue.length
    |> should equal 0

    Queue.empty
    |> Queue.enqueue "foo"
    |> Queue.enqueue "bar"
    |> Queue.enqueue "baz"
    |> Queue.length
    |> should equal 3

[<TestCase>]
let enqueue () : unit =
    Queue.empty
    |> Queue.enqueue "Hello"
    |> Queue.toArray
    |> should equal
        [| "Hello" |]

    [| "foo"; "bar"; "baz"; |]
    |> Queue.ofArray
    |> Queue.enqueue "cdr"
    |> Queue.enqueue "car"
    |> Queue.enqueue "bar"
    |> Queue.enqueue "bar"
    |> Queue.toArray
    |> should equal
        [| "foo"; "bar"; "baz"; "cdr"; "car"; "bar"; "bar"; |]

[<TestCase>]
let enqueueFront () : unit =
    Queue.empty
    |> Queue.enqueueFront "Hello"
    |> Queue.toArray
    |> should equal
        [| "Hello" |]
    
    Queue.empty
    |> Queue.enqueue "foo"
    |> Queue.enqueue "bar"
    |> Queue.enqueueFront "baz"
    |> Queue.enqueue "cdr"
    |> Queue.enqueueFront "car"
    |> Queue.toArray
    |> should equal
        [| "car"; "baz"; "foo"; "bar"; "cdr"; |]

[<TestCase>]
let dequeue () : unit =
    do
        let queue = Queue.ofArray [| "car"; "baz"; "foo"; "bar"; "cdr"; |]

        let result, queue = Queue.dequeue queue
        result |> should equal "car"
        Queue.length queue |> should equal 4

        let result, queue = Queue.dequeue queue
        result |> should equal "baz"
        Queue.length queue |> should equal 3

        let result, queue = Queue.dequeue queue
        result |> should equal "foo"
        Queue.length queue |> should equal 2

        let result, queue = Queue.dequeue queue
        result |> should equal "bar"
        Queue.length queue |> should equal 1

        let result, queue = Queue.dequeue queue
        result |> should equal "cdr"
        Queue.length queue |> should equal 0

[<TestCase; ExpectedException(typeof<System.InvalidOperationException>)>]
let ``dequeue raises exn when queue is empty`` () : unit =
    Queue.empty
    |> Queue.dequeue
    |> ignore

[<TestCase>]
let ofList () : unit =
    List.empty
    |> Queue.ofList
    |> Queue.isEmpty
    |> should be True

    ["foo"; "bar"; "baz"; "cdr"; "car"]
    |> Queue.ofList
    |> Queue.toSeq
    |> Seq.toArray
    |> should equal
        [| "foo"; "bar"; "baz"; "cdr"; "car"; |]

[<TestCase>]
let ofArray () : unit =
    List.empty
    |> Queue.ofList
    |> Queue.isEmpty
    |> should be True

    [| "foo"; "bar"; "baz"; "cdr"; "car"; |]
    |> Queue.ofArray
    |> Queue.toSeq
    |> Seq.toArray
    |> should equal
        [| "foo"; "bar"; "baz"; "cdr"; "car"; |]

[<TestCase>]
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
    |> should equal
        [| "bar"; "baz"; "cdr"; "car"; |]

[<TestCase>]
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
    |> should equal
        ["bar"; "baz"; "cdr"; "car"]

[<TestCase>]
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
    |> should equal
        [| "bar"; "baz"; "cdr"; "car"; |]
