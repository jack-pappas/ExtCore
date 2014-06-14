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
//open FsCheck


[<Test>]
let appendSingleton () : unit =
    Seq.empty
    |> Seq.appendSingleton 123
    |> Seq.length
    |> assertEqual 1

    seq { 0 .. 4 }
    |> Seq.appendSingleton 10
    |> Seq.toList
    |> Collection.assertEqual [0; 1; 2; 3; 4; 10]

[<Test>]
let projectValues () : unit =
    seq { 'a' .. 'e' }
    |> Seq.projectValues (fun asciiChar ->
        asciiChar.ToString().ToUpper())
    |> Seq.toList
    |> Collection.assertEqual
       ['a', "A";
        'b', "B";
        'c', "C";
        'd', "D";
        'e', "E"]

[<Test>]
let projectKeys () : unit =
    seq {
        yield "Red"
        yield "Blue"
        yield "Green"
        yield "Yellow" }
    |> Seq.projectKeys String.length
    |> Seq.toList
    |> Collection.assertEqual
       [3, "Red";
        4, "Blue";
        5, "Green";
        6, "Yellow"]

[<Test>]
let replicate () : unit =
    // Replicating an empty sequence should return an empty sequence.
    Seq.replicate 4 Seq.empty
    |> Seq.isEmpty
    |> assertTrue

    // Basic usage test.
    seq {
        yield "Red"
        yield "Blue"
        yield "Green"
        yield "Yellow" }
    |> Seq.replicate 4
    |> Seq.toArray
    |> Collection.assertEqual
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
        |> Collection.assertEqual
            [| "Red"; "Blue"; "Green"; "Yellow";
               "Red"; "Blue"; "Green"; "Yellow";
               "Red"; "Blue"; "Green"; "Yellow";
               "Red"; "Blue"; "Green"; "Yellow"; |]

        !elementEvalCount
        |> assertEqual 4

[<Test>]
let repeat () : unit =
    // Basic usage test.
    Seq.repeat "Hello World!"
    |> Seq.take 5
    |> Seq.toArray
    |> Collection.assertEqual
        [| "Hello World!"; "Hello World!"; "Hello World!"; "Hello World!"; "Hello World!"; |]

[<Test>]
let cycle () : unit =
    // Basic usage test.
    Seq.cycle 5 (fun i ->
        4 - i)
    |> Seq.take 23
    |> Seq.toArray
    |> Collection.assertEqual
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
        |> Collection.assertEqual
            [| 4; 3; 2; 1; 0;
               4; 3; 2; 1; 0;
               4; 3; 2; 1; 0;
               4; 3; 2; 1; 0;
               4; 3; 2; |]

        !elementEvalCount
        |> assertEqual 5

[<Test>]
let countWith () : unit =
    // Test case for an empty sequence.
    Array.empty
    |> Seq.ofArray
    |> Seq.countWith (fun x ->
        x % 7 = 0)
    |> assertEqual 0L

    // Sample usage test cases.
    [| 0; 1; 2; 3; 4; 5; 6; 8; 9; 10; 16 |]
    |> Seq.ofArray
    |> Seq.countWith (fun x ->
        x < 0)
    |> assertEqual 0L

    [| 0; 1; 2; 3; 4; 5; 6; 8; 9; 10; 16 |]
    |> Seq.ofArray
    |> Seq.countWith (fun x ->
        x % 7 = 0)
    |> assertEqual 1L

    [| 0; 1; 2; 3; 4; 5; 6; 8; 9; 10; 16 |]
    |> Seq.ofArray
    |> Seq.countWith (fun x ->
        x % 3 = 0)
    |> assertEqual 4L

[<Test>]
let sample () : unit =
    // Test case for an empty sequence
    Seq.empty
    |> Seq.sample 3
    |> Seq.isEmpty
    |> assertTrue

    // Sample usage test cases.
    seq { 0 .. 100 }
    |> Seq.sample 3
    |> Seq.toArray
    |> Collection.assertEqual
        [| 0; 3; 6; 9; 12; 15; 18; 21; 24; 27; 30; 33; 36; 39; 42; 45; 48; 51;
           54; 57; 60; 63; 66; 69; 72; 75; 78; 81; 84; 87; 90; 93; 96; 99; |]

[<Test>]
let fold2 () : unit =
    // Test case for when one of the input sequences is empty.
    (Seq.empty, [| "a"; "b"; "c"|])
    ||> Seq.fold2 (fun state x y ->
        System.String.Concat (state, x, y)) "xyz"
    |> assertEqual "xyz"

    // Make sure the function stops once one of the input sequence is empty.
    ([| "a"; "b"; "c" |], [| "A"; "B"; "C"; "D"; "E" |])
    ||> Seq.fold2 (fun state x y ->
        System.String.Concat (state, x, y)) ""
    |> assertEqual "aAbBcC"

    // Test case for when the input sequences have the same length.
    ([| "a"; "b"; "c"; "d"; "e" |], [| "A"; "B"; "C"; "D"; "E" |])
    ||> Seq.fold2 (fun state x y ->
        System.String.Concat (state, x, y)) ""
    |> assertEqual "aAbBcCdDeE"

[<Test>]
let segmentBy () : unit =
    // Test case for an empty input sequence.
    Seq.empty
    |> Seq.segmentBy fst
    |> Seq.isEmpty
    |> assertTrue

    // Sample usage test cases.
    seq { 'a' .. 'k' }
    |> Seq.segmentBy (fun c ->
        int c / 3)
    |> Collection.assertEqual
     <| seq {
        yield seq { 'a' .. 'c' }
        yield seq { 'd' .. 'f' }
        yield seq { 'g' .. 'i' }
        yield seq { 'j' .. 'k' }
        }

[<Test>]
let segmentWith () : unit =
    // Test case for an empty input sequence.
    Seq.empty
    |> Seq.segmentWith (fun _ _ -> true)
    |> Seq.isEmpty
    |> assertTrue

    // Sample usage test cases.
    seq { 'a' .. 'k' }
    |> Seq.segmentWith (fun x y ->
        (int x / 3) = (int y / 3))
    |> Collection.assertEqual
     <| seq {
        yield seq { 'a' .. 'c' }
        yield seq { 'd' .. 'f' }
        yield seq { 'g' .. 'i' }
        yield seq { 'j' .. 'k' }
        }
