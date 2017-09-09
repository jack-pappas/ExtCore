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

/// Creates a reference-counting wrapper over a sequence.
let private refCounter<'T, 'TSeq when 'TSeq :> seq<'T>> (source : 'TSeq) =
    Tests.ExtCore.RefCountEnumerable<'T> (source)

let [<Literal>] sourceRefCountNonZeroMsg =
    "One or more of the enumerators created for the source sequence was not disposed."


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
    do
        let source = refCounter <|  seq { 'a' .. 'e' }

        source
        |> Seq.projectValues (fun asciiChar ->
            asciiChar.ToString().ToUpper())
        |> Seq.toList
        |> Collection.assertEqual
           ['a', "A";
            'b', "B";
            'c', "C";
            'd', "D";
            'e', "E"]

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

[<Test>]
let projectKeys () : unit =
    do
        let source = refCounter <| seq {
            yield "Red"
            yield "Blue"
            yield "Green"
            yield "Yellow" }

        source
        |> Seq.projectKeys String.length
        |> Seq.toList
        |> Collection.assertEqual
           [3, "Red";
            4, "Blue";
            5, "Green";
            6, "Yellow"]

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

[<Test>]
let replicate () : unit =
    // Replicating an empty sequence should return an empty sequence.
    do
        let source = refCounter Seq.empty

        source
        |> Seq.replicate 4
        |> Seq.isEmpty
        |> assertTrue

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

    // Basic usage test.
    do
        let source = refCounter <| seq {
            yield "Red"
            yield "Blue"
            yield "Green"
            yield "Yellow" }

        source
        |> Seq.replicate 4
        |> Seq.toArray
        |> Collection.assertEqual
            [| "Red"; "Blue"; "Green"; "Yellow";
               "Red"; "Blue"; "Green"; "Yellow";
               "Red"; "Blue"; "Green"; "Yellow";
               "Red"; "Blue"; "Green"; "Yellow"; |]

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

    // Make sure the input sequence is only evaluated once.
    do
        let elementEvalCount = ref 0

        let source = refCounter <| seq {
            incr elementEvalCount
            yield "Red"
            incr elementEvalCount
            yield "Blue"
            incr elementEvalCount
            yield "Green"
            incr elementEvalCount
            yield "Yellow" }

        source
        |> Seq.replicate 4
        |> Seq.toArray
        |> Collection.assertEqual
            [| "Red"; "Blue"; "Green"; "Yellow";
               "Red"; "Blue"; "Green"; "Yellow";
               "Red"; "Blue"; "Green"; "Yellow";
               "Red"; "Blue"; "Green"; "Yellow"; |]

        !elementEvalCount
        |> assertEqual 4

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

[<Test>]
let repeat () : unit =
    // Basic usage test.
    do
        let source = refCounter <| Seq.repeat "Hello World!"

        source
        |> Seq.take 5
        |> Seq.toArray
        |> Collection.assertEqual
            [| "Hello World!"; "Hello World!"; "Hello World!"; "Hello World!"; "Hello World!"; |]

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

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
    do
        let source = refCounter Array.empty

        source
        |> Seq.countWith (fun x ->
            x % 7 = 0)
        |> assertEqual 0L

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

    // Sample usage test cases.
    do
        let source = refCounter [| 0; 1; 2; 3; 4; 5; 6; 8; 9; 10; 16 |]

        source
        |> Seq.countWith (fun x ->
            x < 0)
        |> assertEqual 0L

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

    do
        let source = refCounter [| 0; 1; 2; 3; 4; 5; 6; 8; 9; 10; 16 |]

        source
        |> Seq.countWith (fun x ->
            x % 7 = 0)
        |> assertEqual 1L

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

    do
        let source = refCounter [| 0; 1; 2; 3; 4; 5; 6; 8; 9; 10; 16 |]

        source
        |> Seq.countWith (fun x ->
            x % 3 = 0)
        |> assertEqual 4L

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

[<Test>]
let sample () : unit =
    // Test case for an empty sequence
    do
        let source = refCounter Seq.empty

        source
        |> Seq.sample 3
        |> Seq.isEmpty
        |> assertTrue

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

    // Sample usage test cases.
    do
        let source = refCounter <| seq { 0 .. 100 }

        source
        |> Seq.sample 3
        |> Seq.toArray
        |> Collection.assertEqual
            [| 0; 3; 6; 9; 12; 15; 18; 21; 24; 27; 30; 33; 36; 39; 42; 45; 48; 51;
               54; 57; 60; 63; 66; 69; 72; 75; 78; 81; 84; 87; 90; 93; 96; 99; |]

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

[<Test>]
let fold2 () : unit =
    // Test case for when one of the input sequences is empty.
    do
        let source1 = refCounter Seq.empty
        let source2 = refCounter [| "a"; "b"; "c"|]

        (source1, source2)
        ||> Seq.fold2 (fun state x y ->
            System.String.Concat (state, x, y)) "xyz"
        |> assertEqual "xyz"

        Assert.AreEqual (0, source1.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)
        Assert.AreEqual (0, source2.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

    // Make sure the function stops once one of the input sequence is empty.
    do
        let source1 = refCounter [| "a"; "b"; "c" |]
        let source2 = refCounter [| "A"; "B"; "C"; "D"; "E" |]

        ([| "a"; "b"; "c" |], [| "A"; "B"; "C"; "D"; "E" |])
        ||> Seq.fold2 (fun state x y ->
            System.String.Concat (state, x, y)) ""
        |> assertEqual "aAbBcC"

        Assert.AreEqual (0, source1.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)
        Assert.AreEqual (0, source2.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

    // Test case for when the input sequences have the same length.
    do
        let source1 = refCounter [| "a"; "b"; "c"; "d"; "e" |]
        let source2 = refCounter [| "A"; "B"; "C"; "D"; "E" |]

        (source1, source2)
        ||> Seq.fold2 (fun state x y ->
            System.String.Concat (state, x, y)) ""
        |> assertEqual "aAbBcCdDeE"

        Assert.AreEqual (0, source1.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)
        Assert.AreEqual (0, source2.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

[<Test>]
let segmentBy () : unit =
    // Test case for an empty input sequence.
    do
        let source = refCounter Seq.empty

        source
        |> Seq.segmentBy fst
        |> Seq.isEmpty
        |> assertTrue

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

    (* Tests for special cases.
       Cast these seqs to arrays before comparing, otherwise assertion failure messages are incomprehensible. *)

    // Single-element (singleton) sequence.
    do
        let source = refCounter <| Seq.singleton "Foo"

        source
        |> Seq.segmentBy id
        // Convert to array of arrays
        |> Seq.map Seq.toArray
        |> Seq.toArray
        |> Collection.assertEqual
            [| [| "Foo" |] |]

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

    // No segments with multiple elements.
    do
        let source = refCounter <| seq { 0 .. 10 }

        source
        |> Seq.segmentBy id
        // Convert to array of arrays
        |> Seq.map Seq.toArray
        |> Seq.toArray
        |> Collection.assertEqual
            [| [| 0 |]; [| 1 |]; [| 2 |]; [| 3 |]; [| 4 |]; [| 5 |]; [| 6 |]; [| 7 |]; [| 8 |]; [| 9 |]; [| 10 |]; |]

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

    // The first segment has a single element.
    do
        let source = refCounter <| seq { 9 .. 19 }

        source
        |> Seq.segmentBy (fun x ->
            x / 10)
        // Convert to array of arrays
        |> Seq.map Seq.toArray
        |> Seq.toArray
        |> Collection.assertEqual
            [| [| 9 |]; [| 10 .. 19 |]; |]

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

    // The last segment has a single element.
    do
        let source = refCounter <| seq { 0 .. 10 }

        source
        |> Seq.segmentBy (fun x ->
            x / 10)
        // Convert to array of arrays
        |> Seq.map Seq.toArray
        |> Seq.toArray
        |> Collection.assertEqual
            [| [| 0 .. 9 |]; [| 10 |]; |]

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

    (* Sample usage test cases. *)
    do
        let source = refCounter <| seq { 'a' .. 'k' }

        source
        |> Seq.segmentBy (fun c ->
            int c / 3)
        // Convert to array of arrays
        |> Seq.map Seq.toArray
        |> Seq.toArray
        |> Collection.assertEqual
         <|(seq {
            yield seq { 'a' .. 'b' }
            yield seq { 'c' .. 'e' }
            yield seq { 'f' .. 'h' }
            yield seq { 'i' .. 'k' }
            }
            |> Seq.map Seq.toArray
            |> Seq.toArray)

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

[<Test>]
let segmentWith () : unit =
    // Test case for an empty input sequence.
    do
        let source = refCounter Seq.empty

        source
        |> Seq.segmentWith (fun _ _ -> true)
        |> Seq.isEmpty
        |> assertTrue

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

    (* Tests for special cases.
       Cast these seqs to arrays before comparing, otherwise assertion failure messages are incomprehensible. *)

    // Single-element (singleton) sequence.
    do
        let source = refCounter <| Seq.singleton "Foo"

        source
        |> Seq.segmentWith (fun _ _ ->
            Assert.Fail "The predicate function should not be called for a single-element sequence."
            false)
        // Convert to array of arrays
        |> Seq.map Seq.toArray
        |> Seq.toArray
        |> Collection.assertEqual
            [| [| "Foo" |] |]

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

    // No segments with multiple elements.
    do
        let source = refCounter <| seq { 0 .. 10 }

        source
        |> Seq.segmentWith (fun _ _ -> false)
        // Convert to array of arrays
        |> Seq.map Seq.toArray
        |> Seq.toArray
        |> Collection.assertEqual
            [| [| 0 |]; [| 1 |]; [| 2 |]; [| 3 |]; [| 4 |]; [| 5 |]; [| 6 |]; [| 7 |]; [| 8 |]; [| 9 |]; [| 10 |]; |]

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

    // The first segment has a single element.
    do
        let source = refCounter <| seq { 9 .. 19 }

        source
        |> Seq.segmentWith (fun x y ->
            x / 10 = y / 10)
        // Convert to array of arrays
        |> Seq.map Seq.toArray
        |> Seq.toArray
        |> Collection.assertEqual
            [| [| 9 |]; [| 10 .. 19 |]; |]

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

    // The last segment has a single element.
    do
        let source = refCounter <| seq { 0 .. 10 }

        source
        |> Seq.segmentWith (fun x y ->
            x / 10 = y / 10)
        // Convert to array of arrays
        |> Seq.map Seq.toArray
        |> Seq.toArray
        |> Collection.assertEqual
            [| [| 0 .. 9 |]; [| 10 |]; |]

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)

    (* Sample usage test cases. *)
    do
        let source = refCounter <| seq { 'a' .. 'k' }

        source
        |> Seq.segmentWith (fun x y ->
            (int x / 3) = (int y / 3))
        // Convert to array of arrays
        |> Seq.map Seq.toArray
        |> Seq.toArray
        |> Collection.assertEqual
         <|(seq {
            yield seq { 'a' .. 'b' }
            yield seq { 'c' .. 'e' }
            yield seq { 'f' .. 'h' }
            yield seq { 'i' .. 'k' }
            }
            |> Seq.map Seq.toArray
            |> Seq.toArray)

        Assert.AreEqual (0, source.EnumeratorReferenceCount, sourceRefCountNonZeroMsg)
