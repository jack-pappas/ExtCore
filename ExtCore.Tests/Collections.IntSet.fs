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

/// Unit tests for the ExtCore.Collections.IntSet type and module.
module Tests.ExtCore.Collections.IntSet

open System.Collections.Generic
open NUnit.Framework
open FsUnit


[<Test>]
let isEmpty () : unit =
    IntSet.empty
    |> IntSet.isEmpty
    |> should be True
    
    IntSet.singleton 5
    |> IntSet.isEmpty
    |> should be False

[<Test>]
let count () : unit =
    IntSet.empty
    |> IntSet.count
    |> assertEqual 0

    IntSet.singleton 4
    |> IntSet.count
    |> assertEqual 1

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.count
    |> assertEqual 8

[<Test>]
let singleton () : unit =
    IntSet.singleton 6
    |> assertEqual (
        IntSet.empty
        |> IntSet.add 6)

[<Test>]
let contains () : unit =
    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.contains 11
    |> should be True

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.contains 6
    |> should be False

[<Test>]
let minElement () : unit =
    // Simple test case.
    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.minElement
    |> assertEqual 2

    // Test case for minElement (unsigned) when a negative value is in the set.
    [| 5; 3; 11; 2; 17; 4; 12; 14; -1 |]
    |> IntSet.ofArray
    |> IntSet.minElement
    |> assertEqual 2

    // Test case for minElement (unsigned) only negative values are in the set.
    [| -5; -3; -11; -2; -17; -4; -12; -14 |]
    |> IntSet.ofArray
    |> IntSet.minElement
    |> assertEqual -17

[<Test; ExpectedException(typeof<System.ArgumentException>)>]
let ``minElement raises exn for empty set`` () : unit =
    IntSet.minElement IntSet.empty |> ignore

[<Test>]
let minElementSigned () : unit =
    // Simple test case.
    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.minElementSigned
    |> assertEqual 2

    // Test case for minElementSigned when a negative value is in the set.
    [| 5; 3; 11; 2; 17; 4; 12; 14; -1 |]
    |> IntSet.ofArray
    |> IntSet.minElementSigned
    |> assertEqual -1

    // Test case for minElementSigned only negative values are in the set.
    [| -5; -3; -11; -2; -17; -4; -12; -14 |]
    |> IntSet.ofArray
    |> IntSet.minElementSigned
    |> assertEqual -17

[<Test; ExpectedException(typeof<System.ArgumentException>)>]
let ``minElementSigned raises exn for empty set`` () : unit =
    IntSet.minElementSigned IntSet.empty |> ignore

[<Test>]
let maxElement () : unit =
    // Simple test case.
    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.maxElement
    |> assertEqual 17

    // Test case for maxElement (unsigned) when a negative value is in the set.
    [| 5; 3; 11; 2; 17; 4; 12; 14; -1 |]
    |> IntSet.ofArray
    |> IntSet.maxElement
    |> assertEqual -1

    // Test case for maxElement (unsigned) only negative values are in the set.
    [| -5; -3; -11; -2; -17; -4; -12; -14 |]
    |> IntSet.ofArray
    |> IntSet.maxElement
    |> assertEqual -2

[<Test; ExpectedException(typeof<System.ArgumentException>)>]
let ``maxElement raises exn for empty set`` () : unit =
    IntSet.maxElement IntSet.empty |> ignore

[<Test>]
let maxElementSigned () : unit =
    // Simple test case.
    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.maxElementSigned
    |> assertEqual 17

    // Test case for maxElementSigned when a negative value is in the set.
    [| 5; 3; 11; 2; 17; 4; 12; 14; -1 |]
    |> IntSet.ofArray
    |> IntSet.maxElementSigned
    |> assertEqual 17

    // Test case for maxElementSigned only negative values are in the set.
    [| -5; -3; -11; -2; -17; -4; -12; -14 |]
    |> IntSet.ofArray
    |> IntSet.maxElementSigned
    |> assertEqual -2

[<Test; ExpectedException(typeof<System.ArgumentException>)>]
let ``maxElementSigned raises exn for empty set`` () : unit =
    IntSet.maxElementSigned IntSet.empty |> ignore

[<Test>]
let add () : unit =
    IntSet.empty
    |> IntSet.add 5
    |> assertEqual (
        IntSet.singleton 5)

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.add 5
    |> assertEqual (IntSet.ofArray
        [| 5; 3; 11; 2; 17; 4; 12; 14 |])

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.add 8
    |> assertEqual (IntSet.ofArray
        [| 5; 3; 11; 2; 17; 4; 12; 14; 8 |])

[<Test>]
let remove () : unit =
    IntSet.singleton 6
    |> IntSet.remove 6
    |> assertEqual IntSet.empty

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.remove 5
    |> assertEqual (IntSet.ofArray
        [| 3; 11; 2; 17; 4; 12; 14 |])

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.remove 8
    |> assertEqual (IntSet.ofArray
        [| 5; 3; 11; 2; 17; 4; 12; 14 |])

[<Test>]
let union () : unit =
    IntSet.union
        (IntSet.ofArray [| 3; 11; 2; 4; 12 |])
        (IntSet.ofArray [| 5; 11; 17; 4; 14 |])
    |> assertEqual
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])

[<Test>]
let unionMany () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let intersect () : unit =
    IntSet.intersect
        (IntSet.ofArray [| 5; 11; 17; 4; 14 |])
        IntSet.empty
    |> assertEqual IntSet.empty

    IntSet.intersect
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (IntSet.ofArray [| 5; 11; 17; 4; 14 |])
    |> assertEqual
        (IntSet.ofArray [| 5; 11; 17; 4; 14 |])

[<Test>]
let intersectMany () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let difference () : unit =
    IntSet.difference
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        IntSet.empty
    |> assertEqual
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])

    IntSet.difference
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (IntSet.ofArray [| 5; 11; 17; 4; 14 |])
    |> assertEqual
        (IntSet.ofArray [| 3; 2; 12 |])

[<Test>]
let isSubset () : unit =
    // The empty set is always a subset of any other set.
    IntSet.isSubset
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        IntSet.empty
    |> should be True

    IntSet.isSubset
        IntSet.empty IntSet.empty
    |> should be True

    // A set is a subset of itself (this distinguishes isSubset from isProperSubset).
    IntSet.isSubset
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be True

    // Basic tests.
    IntSet.isSubset
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (IntSet.ofArray [| 5; 3; 11; 12; 14 |])
    |> should be True

    IntSet.isSubset
        (IntSet.ofArray [| 5; 3; 11; 12; 14 |])
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be False

    // Partially-overlapping sets.
    IntSet.isSubset
        (IntSet.ofArray [| 5; 3; 11; 12; 14; 22; 42; 25; |])
        (IntSet.ofArray [| 5; 3; 11; 12; 14; 32; 57; 53; |])
    |> should be False

    // Disjoint sets.
    IntSet.isSubset
        (IntSet.ofArray [| 1..5 |])
        (IntSet.ofArray [| 6..10 |])
    |> should be False

[<Test>]
let isProperSubset () : unit =
    // The empty set is a proper subset of any set except itself.
    IntSet.isProperSubset
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        IntSet.empty
    |> should be True

    IntSet.isProperSubset
        IntSet.empty IntSet.empty
    |> should be False

    // A set is a subset of itself (this distinguishes isSubset from isProperSubset).
    IntSet.isProperSubset
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be False

    // Basic tests.
    IntSet.isProperSubset
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (IntSet.ofArray [| 5; 3; 11; 12; 14 |])
    |> should be True

    IntSet.isProperSubset
        (IntSet.ofArray [| 5; 3; 11; 12; 14 |])
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be False

    // Partially-overlapping sets.
    IntSet.isProperSubset
        (IntSet.ofArray [| 5; 3; 11; 12; 14; 22; 42; 25; |])
        (IntSet.ofArray [| 5; 3; 11; 12; 14; 32; 57; 53; |])
    |> should be False

    // Disjoint sets.
    IntSet.isProperSubset
        (IntSet.ofArray [| 1..5 |])
        (IntSet.ofArray [| 6..10 |])
    |> should be False

[<Test>]
let isSuperset () : unit =
    // The empty set is never a superset of any other set except itself.
    IntSet.isSuperset
        IntSet.empty
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be False

    IntSet.isSuperset
        IntSet.empty IntSet.empty
    |> should be True

    // A set is a superset of itself (this distinguishes isSuperset from isProperSuperset).
    IntSet.isSuperset
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be True

    // Basic tests.
    IntSet.isSuperset
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (IntSet.ofArray [| 5; 3; 11; 12; 14 |])
    |> should be False

    IntSet.isSuperset
        (IntSet.ofArray [| 5; 3; 11; 12; 14 |])
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be True

    // Partially-overlapping sets.
    IntSet.isSuperset
        (IntSet.ofArray [| 5; 3; 11; 12; 14; 22; 42; 25; |])
        (IntSet.ofArray [| 5; 3; 11; 12; 14; 32; 57; 53; |])
    |> should be False

    // Disjoint sets.
    IntSet.isSuperset
        (IntSet.ofArray [| 1..5 |])
        (IntSet.ofArray [| 6..10 |])
    |> should be False

[<Test>]
let isProperSuperset () : unit =
    // The empty set is never a proper superset of any set.
    IntSet.isProperSuperset
        IntSet.empty
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be False

    IntSet.isProperSuperset
        IntSet.empty IntSet.empty
    |> should be False

    // A set is a superset of itself (this distinguishes isSuperset from isProperSuperset).
    IntSet.isProperSuperset
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be False

    // Basic tests.
    IntSet.isProperSuperset
        (IntSet.ofArray [| 5; 3; 11; 12; 14 |])
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be False

    IntSet.isProperSuperset
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (IntSet.ofArray [| 5; 3; 11; 12; 14 |])
    |> should be True

    // Partially-overlapping sets.
    IntSet.isProperSuperset
        (IntSet.ofArray [| 5; 3; 11; 12; 14; 22; 42; 25; |])
        (IntSet.ofArray [| 5; 3; 11; 12; 14; 32; 57; 53; |])
    |> should be False

    // Disjoint sets.
    IntSet.isProperSuperset
        (IntSet.ofArray [| 1..5 |])
        (IntSet.ofArray [| 6..10 |])
    |> should be False

[<Test>]
let ofSeq () : unit =
    Seq.empty
    |> IntSet.ofSeq
    |> assertEqual IntSet.empty
    
    seq {
        yield! seq { 2 .. 5 }
        yield 11
        yield 12
        yield 14
        yield 17 }
    |> IntSet.ofSeq
    |> assertEqual (
        IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])

[<Test>]
let ofList () : unit =
    List.empty
    |> IntSet.ofList
    |> assertEqual IntSet.empty

    [5; 3; 11; 2; 17; 4; 12; 14]
    |> IntSet.ofList
    |> assertEqual (
        IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])

[<Test>]
let ofArray () : unit =
    Array.empty
    |> IntSet.ofArray
    |> assertEqual IntSet.empty

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> assertEqual (
        IntSet.empty
        |> IntSet.add 2
        |> IntSet.add 3
        |> IntSet.add 4
        |> IntSet.add 5
        |> IntSet.add 11
        |> IntSet.add 12
        |> IntSet.add 14
        |> IntSet.add 17)

[<Test>]
let ofSet () : unit =
    Set.empty
    |> IntSet.ofSet
    |> assertEqual IntSet.empty

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> Set.ofArray
    |> IntSet.ofSet
    |> assertEqual (
        IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])

[<Test>]
let toSeq () : unit =
    IntSet.empty
    |> IntSet.toSeq
    |> Seq.isEmpty
    |> should be True

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.toSeq
    |> Seq.toArray
    |> assertEqual
        [|2; 3; 4; 5; 11; 12; 14; 17|]

[<Test>]
let toList () : unit =
    IntSet.empty
    |> IntSet.toList
    |> assertEqual List.empty<int>

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.toList
    |> assertEqual
        [2; 3; 4; 5; 11; 12; 14; 17]

[<Test>]
let toArray () : unit =
    IntSet.empty
    |> IntSet.toArray
    |> assertEqual Array.empty<int>

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.toArray
    |> assertEqual
        [|2; 3; 4; 5; 11; 12; 14; 17|]

[<Test>]
let toSet () : unit =
    IntSet.empty
    |> IntSet.toSet
    |> assertEqual Set.empty<int>

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.toSet
    |> assertEqual
        (Set.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])

[<Test>]
let iter () : unit =
    let elements = ResizeArray ()

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.iter (fun el ->
        elements.Add (el + 2))

    elements
    |> ResizeArray.toArray
    |> assertEqual
        [|4; 5; 6; 7; 13; 14; 16; 19|]

[<Test>]
let iterBack () : unit =
    let elements = ResizeArray ()

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.iterBack (fun el ->
        elements.Add (el + 2))

    elements
    |> ResizeArray.toArray
    |> assertEqual
        [|19; 16; 14; 13; 7; 6; 5; 4|]

[<Test>]
let fold () : unit =
    do
        let elements = ResizeArray ()

        (0, IntSet.empty)
        ||> IntSet.fold (fun counter el ->
            elements.Add (counter + el + 2)
            counter + 1)
        |> assertEqual 0

        elements
        |> ResizeArray.isEmpty
        |> should be True

    do
        let elements = ResizeArray ()

        let testSet =
            [| 5; 3; 11; 2; 17; 4; 12; 14 |]
            |> IntSet.ofArray

        (0, testSet)
        ||> IntSet.fold (fun counter el ->
            elements.Add (counter + el + 2)
            counter + 1)
        |> assertEqual (IntSet.count testSet)

        elements
        |> ResizeArray.toArray
        |> assertEqual
            [|4; 6; 8; 10; 17; 19; 22; 26|]

[<Test>]
let foldBack () : unit =
    do
        let elements = ResizeArray ()

        (IntSet.empty, 0)
        ||> IntSet.foldBack (fun el counter ->
            elements.Add (counter + el + 2)
            counter + 1)
        |> assertEqual 0

        elements
        |> ResizeArray.isEmpty
        |> should be True

    do
        let elements = ResizeArray ()

        let testSet =
            [| 5; 3; 11; 2; 17; 4; 12; 14 |]
            |> IntSet.ofArray

        (testSet, 0)
        ||> IntSet.foldBack (fun el counter ->
            elements.Add (counter + el + 2)
            counter + 1)
        |> assertEqual (IntSet.count testSet)

        elements
        |> ResizeArray.toArray
        |> assertEqual
            [|19; 17; 16; 16; 11; 11; 11; 11|]

[<Test>]
let choose () : unit =
    IntSet.empty
    |> IntSet.choose (fun el ->
        if el % 2 = 0 then
            Some (el + 1)
        else None)
    |> assertEqual IntSet.empty

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.choose (fun el ->
        if el % 2 = 0 then
            Some (el + 1)
        else None)
    |> assertEqual
        (IntSet.ofArray [|3; 5; 13; 15|])

[<Test>]
let filter () : unit =
    IntSet.empty
    |> IntSet.filter (fun el ->
        el % 2 <> 0)
    |> IntSet.isEmpty
    |> should be True

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.filter (fun el ->
        el % 2 <> 0)
    |> assertEqual
        (IntSet.ofArray [|5; 3; 11; 17|])

[<Test>]
let map () : unit =
    IntSet.empty
    |> IntSet.map (fun el ->
        el * 2)
    |> IntSet.isEmpty
    |> should be True

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.map (fun el ->
        el * 2)
    |> assertEqual (
        [| 5; 3; 11; 2; 17; 4; 12; 14 |]
        |> Set.ofArray
        |> ExtCore.Collections.Set.mapToArray (fun el ->
            el * 2)
        |> IntSet.ofArray)

[<Test>]
let partition () : unit =
    do
        let evens, odds =
            IntSet.empty
            |> IntSet.partition (fun el ->
                el % 2 = 0)

        evens
        |> IntSet.isEmpty
        |> should be True

        odds
        |> IntSet.isEmpty
        |> should be True

    do
        let evens, odds =
            [| 5; 3; 11; 2; 17; 4; 12; 14 |]
            |> IntSet.ofArray
            |> IntSet.partition (fun el ->
                el % 2 = 0)

        evens
        |> assertEqual
            (IntSet.ofArray [|2; 4; 12; 14|])

        odds
        |> assertEqual
            (IntSet.ofArray [|5; 3; 11; 17|])

[<Test>]
let exists () : unit =
    IntSet.empty
    |> IntSet.exists (fun el ->
        el % 7 = 0)
    |> should be False

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.exists (fun el ->
        el = 6)
    |> should be False

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.exists (fun el ->
        el % 7 = 0)
    |> should be True

[<Test>]
let forall () : unit =
    IntSet.empty
    |> IntSet.forall (fun el ->
        el % 7 = 0)
    |> should be True

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.forall (fun el ->
        el < 100)
    |> should be True

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.forall (fun el ->
        el % 2 = 0)
    |> should be False

[<Test>]
let tryPick () : unit =
    // Test against an empty set.
    IntSet.empty
    |> IntSet.tryPick (fun el ->
        if el % 7 = 0 then
            Some (el + 2)
        else None)
    |> assertEqual (None : int option)

    // Test for case where the set does not contain a matching element.
    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.tryPick (fun el ->
        if el > 30 then
            Some (el - 10)
        else None)
    |> assertEqual (None : int option)

    // Test for case where the set contains a single matching element.
    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.tryPick (fun el ->
        if el % 7 = 0 then
            Some (el - 2)
        else None)
    |> assertEqual (Some 12)

    // Test for case where the set contains multiple matching elements.
    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.tryPick (fun el ->
        if el % 3 = 2 then
            Some (el + 1)
        else None)
    |> assertEqual (Some 3)

[<Test>]
let pick () : unit =
    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.pick (fun el ->
        if el % 3 = 2 then
            Some (el + 1)
        else None)
    |> assertEqual 3

[<Test; ExpectedException(typeof<KeyNotFoundException>)>]
let ``pick raises exn on empty input`` () : unit =
    IntSet.empty
    |> IntSet.pick (fun el ->
        if el % 3 = 2 then
            Some (el + 1)
        else None)
    |> ignore


open FsCheck

(* TODO : Implement FsCheck tests. *)
