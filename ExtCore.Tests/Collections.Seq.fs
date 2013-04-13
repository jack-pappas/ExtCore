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

/// Unit tests for the ExtCore.Collections.Seq module.
module Tests.ExtCore.Collections.Seq

open NUnit.Framework
open FsUnit
//open FsCheck


[<TestCase>]
let appendSingleton () : unit =
    Seq.empty
    |> Seq.appendSingleton 123
    |> Seq.length
    |> should equal 1

    seq { 0 .. 4 }
    |> Seq.appendSingleton 10
    |> should equal [0; 1; 2; 3; 4; 10]

[<TestCase>]
let projectValues () : unit =
    seq { 'a' .. 'e' }
    |> Seq.projectValues (fun asciiChar ->
        asciiChar.ToString().ToUpper())
    |> Seq.toList
    |> should equal
       ['a', "A";
        'b', "B";
        'c', "C";
        'd', "D";
        'e', "E"]

[<TestCase>]
let projectKeys () : unit =
    seq {
        yield "Red"
        yield "Blue"
        yield "Green"
        yield "Yellow" }
    |> Seq.projectKeys String.length
    |> Seq.toList
    |> should equal
       [3, "Red";
        4, "Blue";
        5, "Green";
        6, "Yellow"]

[<TestCase>]
let replicate () : unit =
    // Replicating an empty sequence should return an empty sequence.
    Seq.replicate 4 Seq.empty
    |> Seq.isEmpty
    |> should be True

    // Basic usage test.
    seq {
        yield "Red"
        yield "Blue"
        yield "Green"
        yield "Yellow" }
    |> Seq.replicate 4
    |> Seq.toArray
    |> should equal
        [| "Red"; "Blue"; "Green"; "Yellow";
           "Red"; "Blue"; "Green"; "Yellow";
           "Red"; "Blue"; "Green"; "Yellow";
           "Red"; "Blue"; "Green"; "Yellow"; |]

    // Make sure the input sequence is only evaluated once.
    do
        let elementEvalCount = ref 0

        seq {
            incr elementEvalCount
            yield "Red"
            incr elementEvalCount
            yield "Blue"
            incr elementEvalCount
            yield "Green"
            incr elementEvalCount
            yield "Yellow" }
        |> Seq.replicate 4
        |> Seq.toArray
        |> should equal
            [| "Red"; "Blue"; "Green"; "Yellow";
               "Red"; "Blue"; "Green"; "Yellow";
               "Red"; "Blue"; "Green"; "Yellow";
               "Red"; "Blue"; "Green"; "Yellow"; |]

        !elementEvalCount
        |> should equal 4

[<TestCase>]
let repeat () : unit =
    // Basic usage test.
    Seq.repeat "Hello World!"
    |> Seq.take 5
    |> Seq.toArray
    |> should equal
        [| "Hello World!"; "Hello World!"; "Hello World!"; "Hello World!"; "Hello World!"; |]

[<TestCase>]
let cycle () : unit =
    // Basic usage test.
    Seq.cycle 5 (fun i ->
        4 - i)
    |> Seq.take 23
    |> Seq.toArray
    |> should equal
        [| 4; 3; 2; 1; 0;
           4; 3; 2; 1; 0;
           4; 3; 2; 1; 0;
           4; 3; 2; 1; 0;
           4; 3; 2; |]

    // Make sure the generator function is only called once per element.
    do
        let elementEvalCount = ref 0

        Seq.cycle 5 (fun i ->
            incr elementEvalCount
            4 - i)
        |> Seq.take 23
        |> Seq.toArray
        |> should equal
            [| 4; 3; 2; 1; 0;
               4; 3; 2; 1; 0;
               4; 3; 2; 1; 0;
               4; 3; 2; 1; 0;
               4; 3; 2; |]

        !elementEvalCount
        |> should equal 5

