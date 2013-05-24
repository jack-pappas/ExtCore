(*

Copyright 2005-2009 Microsoft Corporation
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

open System
open System.Collections
open System.Collections.Generic
open FsUnit
open NUnit.Framework


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
        IntSet.empty
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
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
        (IntSet.ofArray [| 5; 3; 11; 12; 14 |])
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be True

    IntSet.isSubset
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (IntSet.ofArray [| 5; 3; 11; 12; 14 |])
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
        IntSet.empty
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be True

    IntSet.isProperSubset
        IntSet.empty IntSet.empty
    |> should be False

    // A set is not a proper subset of itself (this distinguishes isSubset from isProperSubset).
    IntSet.isProperSubset
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be False

    // Basic tests.
    IntSet.isProperSubset
        (IntSet.ofArray [| 5; 3; 11; 12; 14 |])
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be True

    IntSet.isProperSubset
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (IntSet.ofArray [| 5; 3; 11; 12; 14 |])
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
        (IntSet.ofArray [| 5; 3; 11; 12; 14 |])
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be False

    IntSet.isSuperset
        (IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (IntSet.ofArray [| 5; 3; 11; 12; 14 |])
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


(* SetModule and SetType tests from the F# source distribution (in FSharp.Core.Unittests). *)

(*
[Test Strategy]
Make sure each method works on:
* Empty set
* Single-element set
* Sets with 4 more more elements
*)

module SetType =
    // Interfaces
    [<Test>]
    let IEnumerable () : unit =        
        // Legit IE
        let ie = (new IntSet([1; 2; 3])) :> IEnumerable
        //let alphabet = new IntSet<char>([| 'a' .. 'z' |])
        let enum = ie.GetEnumerator()
        
        let testStepping () : unit =
            checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, 1)
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, 2)
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, 3)
            Assert.AreEqual(enum.MoveNext(), false)
            checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
    
        testStepping()
        enum.Reset()
        testStepping()
    
        // Empty IE
        let ie = (new IntSet([])) :> IEnumerable  // Note no type args
        let enum = ie.GetEnumerator()
        
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        Assert.AreEqual(enum.MoveNext(), false)
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)

    [<Test>]
    let IEnumerable_T () : unit =        
        // Legit IE
        let ie =(new IntSet([1; 2; 3])) :> IEnumerable<int>
        let enum = ie.GetEnumerator()
        
        let testStepping () : unit =
            checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, 1)
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, 2)
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, 3)
            Assert.AreEqual(enum.MoveNext(), false)
            checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        
        testStepping()
        enum.Reset()
        testStepping()
    
        // Empty IE
        let ie = (new IntSet([])) :> IEnumerable<int>  
        let enum = ie.GetEnumerator()
        
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        Assert.AreEqual(enum.MoveNext(), false)
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        
    [<Test>]
    let ICollection () : unit =        
        // Legit IC        
        let ic = (new IntSet([1;2;3;4])) :> ICollection<int>
        let st = new IntSet([1;2;3;4])        
        
        Assert.IsTrue(ic.Contains(3)) 
        let newArr = Array.create 5 0
        ic.CopyTo(newArr,0) 
        Assert.IsTrue(ic.IsReadOnly)       
            
        // Empty IC
        let ic = (new IntSet([])) :> ICollection<int>
        Assert.IsFalse(ic.Contains(123) )     
        let newArr = Array.create 5 -1
        ic.CopyTo(newArr,0) 
    
    [<Test>]
    let IComparable () : unit =        
        // Legit IC
        let ic = (new IntSet([1;2;3;4])) :> IComparable    
        Assert.AreEqual(ic.CompareTo(new IntSet([1;2;3;4])),0) 
        
        // Empty IC
        let ic = (new IntSet([])) :> IComparable   
        Assert.AreEqual(ic.CompareTo(IntSet.empty),0)
        
        
    // Base class methods
    [<Test>]
    let ObjectGetHashCode () : unit =
        // Verify order added is independent
        let x = IntSet.ofList [1; 2; 3]
        let y = IntSet.ofList [3; 2; 1]
        Assert.AreEqual(x.GetHashCode(), y.GetHashCode())
    
    [<Test>]
    let ObjectToString () : unit =
        Assert.AreEqual("intSet [1; 2; 3; ... ]", (new IntSet([1;2;3;4])).ToString())
        Assert.AreEqual("intSet []", (IntSet.empty).ToString())
        Assert.AreEqual("intSet [1; 3]", (new IntSet([1;3])).ToString())
        
    
    [<Test>]
    let ObjectEquals () : unit =
        // All three are different references, but equality has been
        // provided by the F# compiler.
        let a = new IntSet([1;2;3])
        let b = new IntSet([1..3])
        let c = new IntSet(seq{1..3})
        Assert.IsTrue( (a = b) )
        Assert.IsTrue( (b = c) )
        Assert.IsTrue( (c = a) )
        Assert.IsTrue( a.Equals(b) ); Assert.IsTrue( b.Equals(a) )
        Assert.IsTrue( b.Equals(c) ); Assert.IsTrue( c.Equals(b) )
        Assert.IsTrue( c.Equals(a) ); Assert.IsTrue( a.Equals(c) )
        
        // Self equality
        let a = new IntSet([1])
        Assert.IsTrue( (a = a) )
        Assert.IsTrue(a.Equals(a))
        
        // Null
        Assert.IsFalse(a.Equals(null))  
        
        
    // Instance methods
    [<Test>]
    let Add () : unit =    
        let l = new IntSet([1 .. 10])
        let ad = l.Add 88
        Assert.IsTrue(ad.Contains(88))
    
        let e : IntSet = IntSet.empty
        let ade = e.Add 123
        Assert.IsTrue(ade.Contains(123))
        
        let s = IntSet.singleton 168
        let ads = s.Add 100
        Assert.IsTrue(ads.Contains(100))
        
    [<Test>]
    let Contains () : unit =    
        let i = new IntSet([1 .. 10])
        Assert.IsTrue(i.Contains(8))
    
        let e : IntSet = IntSet.empty
        Assert.IsFalse(e.Contains(123))
        
        let s = IntSet.singleton 168
        Assert.IsTrue(s.Contains(168))
    
    [<Test>]
    let Count () : unit =    
        let l = new IntSet([1 .. 10])
        Assert.AreEqual(l.Count, 10)
    
        let e : IntSet = IntSet.empty
        Assert.AreEqual(e.Count, 0)
        
        let s = IntSet.singleton 123
        Assert.AreEqual(s.Count, 1)        
        
    [<Test>]
    let IsEmpty () : unit =
        let i = new IntSet([1 .. 10])
        Assert.IsFalse(i.IsEmpty)
    
        let e : IntSet = IntSet.empty
        Assert.IsTrue(e.IsEmpty)
        
        let s = IntSet.singleton 168
        Assert.IsFalse(s.IsEmpty)   
        
    [<Test>]
    let IsSubsetOf () : unit =
        let fir = new IntSet([1 .. 20])
        let sec = new IntSet([1 .. 10])
        Assert.IsTrue(sec.IsSubsetOf(fir))
        Assert.IsTrue(IntSet.isSubset sec fir)
    
        let e : IntSet = IntSet.empty
        Assert.IsTrue(e.IsSubsetOf(fir))
        Assert.IsTrue(IntSet.isSubset e fir)
        
        let s = IntSet.singleton 8
        Assert.IsTrue(s.IsSubsetOf(fir)) 
        Assert.IsTrue(IntSet.isSubset s fir)
        
        let s100 = set [0..100]
        let s101 = set [0..101]
        for i = 0 to 100 do 
            Assert.IsFalse( (set [-1..i]).IsSubsetOf s100)
            Assert.IsTrue( (set [0..i]).IsSubsetOf s100)
            Assert.IsTrue( (set [0..i]).IsProperSubsetOf s101)
           
        
    [<Test>]
    let IsSupersetOf () : unit =
        let fir = new IntSet([1 .. 10])
        let sec = new IntSet([1 .. 20])
        Assert.IsTrue(sec.IsSupersetOf(fir))
        Assert.IsTrue(IntSet.isSuperset sec fir)
    
        let e : IntSet = IntSet.empty
        Assert.IsFalse(e.IsSupersetOf(fir))
        Assert.IsFalse(IntSet.isSuperset e fir)
        
        let s = IntSet.singleton 168
        Assert.IsFalse(s.IsSupersetOf(fir))  
        Assert.IsFalse(IntSet.isSuperset s fir)

        let s100 = set [0..100]
        let s101 = set [0..101]
        for i = 0 to 100 do 
            Assert.IsFalse( s100.IsSupersetOf (set [-1..i]))
            Assert.IsTrue( s100.IsSupersetOf (set [0..i]))
            Assert.IsTrue( s101.IsSupersetOf (set [0..i]))
        
    [<Test>]
    let Remove () : unit =    
        let i = new IntSet([1;2;3;4])
        Assert.AreEqual(i.Remove 3,(new IntSet([1;2;4])))
    
        let e : IntSet = IntSet.empty
        Assert.AreEqual(e.Remove 123, e)
        
        let s = IntSet.singleton 168
        Assert.AreEqual(s.Remove 168, IntSet.empty)

    [<Test>]
    let MinimumElement () : unit =
        let fir = new IntSet([1..6])
        let sec = new IntSet([2;4;6])
        Assert.AreEqual(fir.MinimumElement, 1)
        Assert.AreEqual(sec.MinimumElement, 2)
        Assert.AreEqual(IntSet.minElement fir, 1)
        Assert.AreEqual(IntSet.minElement sec, 2)
        

    [<Test>]
    let MaximumElement () : unit =
        let fir = new IntSet([1..6])
        let sec = new IntSet([2;4;7])
        Assert.AreEqual(fir.MaximumElement, 6)
        Assert.AreEqual(sec.MaximumElement, 7)
        Assert.AreEqual(IntSet.maxElement fir, 6)
        Assert.AreEqual(IntSet.maxElement sec, 7)
        
        
    // Static methods
    [<Test>]
    let Addition () : unit =
        let fir = new IntSet([1;3;5])
        let sec = new IntSet([2;4;6])
        Assert.AreEqual(fir + sec, new IntSet([1;2;3;4;5;6]))
        Assert.AreEqual(IntSet.op_Addition(fir,sec), new IntSet([1;2;3;4;5;6]))
    
        let e : IntSet = IntSet.empty
        Assert.AreEqual(e + e, e)
        Assert.AreEqual(IntSet.op_Addition(e,e),e)
        
        let s1 = IntSet.singleton 8
        let s2 = IntSet.singleton 6
        Assert.AreEqual(s1 + s2, new IntSet([8;6]))
        Assert.AreEqual(IntSet.op_Addition(s1,s2), new IntSet([8;6]))
        

    [<Test>]
    let Subtraction () : unit =
        let fir = new IntSet([1..6])
        let sec = new IntSet([2;4;6])
        Assert.AreEqual(fir - sec, new IntSet([1;3;5]))
        Assert.AreEqual(IntSet.difference fir sec, new IntSet([1;3;5]))
        Assert.AreEqual(IntSet.op_Subtraction(fir,sec), new IntSet([1;3;5]))
    
        let e : IntSet = IntSet.empty
        Assert.AreEqual(e - e, e)
        Assert.AreEqual(IntSet.difference e e, e)
        Assert.AreEqual(IntSet.op_Subtraction(e,e),e)
        
        let s1 = IntSet.singleton 8
        let s2 = IntSet.singleton 6
        Assert.AreEqual(s1 - s2, new IntSet([8]))
        Assert.AreEqual(IntSet.difference s1 s2, new IntSet([8]))
        Assert.AreEqual(IntSet.op_Subtraction(s1,s2), new IntSet([8]))


(*
[Test Strategy]
Make sure each method works on:
* Empty set
* Single-element set
* Sets with 4 more more elements
*)
(*
module SetModule =
    [<Test>]
    let empty () : unit =
        let emptySet = IntSet.empty
        if IntSet.count emptySet <> 0 then Assert.Fail()    
        
        let c : IntSet    = IntSet.empty
        ()

    [<Test>]
    let singleton () : unit =
        let intSingleton = IntSet.singleton 5
        Assert.IsTrue(intSingleton.Count = 1)
        Assert.IsTrue(intSingleton.Contains(5))
        
    [<Test>]
    let add () : unit =
        let empty = IntSet.empty
        let x     = IntSet.add 123 empty
        let xy    = IntSet.add 456 x
        let xyz   = IntSet.add 789 xy
        let wxyz  = IntSet.add 10 xyz
        
        Assert.IsTrue(IntSet.count xy   = 2)
        Assert.IsTrue(IntSet.count xyz  = 3)
        Assert.IsTrue(IntSet.count wxyz = 4)
        
    [<Test>]
    let contains () : unit =
        // Empty set searching for null = false
        if IntSet.contains null (IntSet.empty) <> false then Assert.Fail()

        // Single element set (of tuple) = true
        let digits = new IntSet<string * int>([("one", 1)])
        if IntSet.contains ("one", 1) digits <> true then Assert.Fail()

        let odds = new IntSet([1 .. 2 .. 11])
        if IntSet.contains 6 odds <> false then Assert.Fail()
        ()
        
    [<Test>]
    let count () : unit = 
        let empty = IntSet.empty
        if IntSet.count empty <> 0 then Assert.Fail()
        
        let one = IntSet.add 1 empty
        if IntSet.count one <> 1 then Assert.Fail()
        
        let multi = new IntSet<char>([| 'a' .. 'z' |])
        if IntSet.count multi <> 26 then Assert.Fail()
        ()
        
    [<Test>]
    let diff () : unit = 
        // Given a large set and removing 0, 1, x elements...
        let alphabet = new IntSet<char>([| 'a' .. 'z' |])
        let emptyChar = IntSet.empty : IntSet<char>
        
        let removeEmpty = alphabet - emptyChar
        if (alphabet = removeEmpty) <> true then Assert.Fail()
        
        let number = IntSet.singleton '1'
        let removeNumber = alphabet - number
        if (alphabet = removeNumber) <> true then Assert.Fail()
        
        let vowels = new IntSet<char>([| 'a'; 'e'; 'i'; 'o'; 'u' |])
        let noVowels = alphabet - vowels
        if noVowels.Count <> 21 then Assert.Fail()
        
        // Give a set of 0, 1, x elements remove some other set
        let odds  = new IntSet([1 .. 2 .. 10])
        let evens = new IntSet([2 .. 2 .. 10])
        
        let emptyNum = IntSet.empty : IntSet
        let removeOddsFromEmpty = emptyNum - odds 
        if (emptyNum = removeOddsFromEmpty) <> true then Assert.Fail()
        
        let one = IntSet.singleton 1
        let removeOddsFrom1 = one - odds
        if (removeOddsFrom1 = emptyNum) <> true then Assert.Fail()
        
        let evensSansOdds = evens - odds
        if (evensSansOdds = evens) <> true then Assert.Fail()
        ()

    [<Test>]
    let equal () : unit =
        let emptySet1 : IntSet<string> = IntSet.empty
        let emptySet2 : IntSet<string> = IntSet.empty
        if (emptySet1 = emptySet2) <> true then Assert.Fail()
        
        let a  = new IntSet([1; 2; 3; 4; 5])
        let b = new IntSet([1; 3; 5])
        
        if (a = b) <> false then Assert.Fail()
        
        let a = a |> IntSet.remove 2 |> IntSet.remove 4
        if (a = b) <> true then Assert.Fail()
        ()
        
    [<Test>]
    let compare () : unit =
        // Comparing empty sets
        let emptyString1 = IntSet.empty : IntSet<string>
        let emptyString2 = IntSet.empty : IntSet<string>
        
        if compare emptyString1 emptyString1 <> 0 then Assert.Fail()
        if compare emptyString1 emptyString2 <> 0 then Assert.Fail()

        // Comparing single-element sets
        let one = IntSet.singleton 1
        let two = IntSet.singleton 2
        if compare one two <> -1 then Assert.Fail()
        if compare one one <> 0  then Assert.Fail()
        if compare two two <> 0  then Assert.Fail()
        if compare two one <> 1  then Assert.Fail()

        // Comparing multi-element sets
        let alphabet = new IntSet<char>(['a' .. 'z'])
        let vowels   = new IntSet<char>(['a'; 'e'; 'i'; 'o'; 'u'])
        
        let noVowelAlpa = alphabet - vowels
        if compare noVowelAlpa alphabet     <> 1  then Assert.Fail()
        if compare alphabet alphabet        <> 0  then Assert.Fail()
        if compare noVowelAlpa noVowelAlpa  <> 0  then Assert.Fail()
        if compare alphabet noVowelAlpa     <> -1 then Assert.Fail()
        ()

    [<Test>]
    let exists () : unit =
        
        let emptyInt = IntSet.empty : IntSet
        if IntSet.exists (fun _ -> true) emptyInt <> false then Assert.Fail()
        
        let x = IntSet.singleton 'x'
        if IntSet.exists (fun c -> c = 'x') x  <> true  then Assert.Fail()
        if IntSet.exists (fun c -> c <> 'x') x <> false then Assert.Fail()
        
        let letNumPairs = new IntSet<string * int>([("one", 1); ("two", 2); ("three", 3)])
        if IntSet.exists (fun (text, num) -> text = "one" && num = 1) letNumPairs <> true then Assert.Fail()
        if IntSet.exists (fun (text, num) -> text = "four") letNumPairs           <> false then Assert.Fail()
        ()
        
    [<Test>]
    let filter () : unit =
        let emptyComplex = IntSet.empty : IntSet<int * List<string * IntSet<decimal>> * IntSet<int * string * (char * char * char)>>
        let fileredEmpty = IntSet.filter (fun _ -> false) emptyComplex 
        if (fileredEmpty = emptyComplex) <> true then Assert.Fail()
        
        let nullSet = IntSet.singleton null
        if nullSet.Count <> 1 then Assert.Fail()
        let filteredNull = IntSet.filter (fun x -> x <> null) nullSet
        if filteredNull.Count <> 0 then Assert.Fail()
        
        let digits = new IntSet([1 .. 10])
        let evens  = new IntSet([2 .. 2 .. 10])
        let filteredDigits = IntSet.filter(fun i -> i % 2 = 0) digits
        if (filteredDigits = evens) <> true then Assert.Fail()
        ()
        

    [<Test>]
    let map () : unit =
        let emptySet : IntSet<string> = IntSet.empty
        
        let result = IntSet.map (fun _ -> Assert.Fail(); "") emptySet
        if (emptySet = result) <> true then Assert.Fail()
        
        let alphabet = new IntSet(['a' .. 'z'])
        let capped = IntSet.map (fun c -> Char.ToUpper(c)) alphabet
        
        if IntSet.exists (fun c -> c = Char.ToLower(c)) capped then Assert.Fail()
        ()

    [<Test>]
    let fold () : unit =
        
        let emptySet : IntSet<decimal> = IntSet.empty
        let result = IntSet.fold (fun _ _ -> Assert.Fail(); -1I) 0I emptySet
        if result <> 0I then Assert.Fail()
        
        let callOrder = ref ([] : (int * int) list)
        let input = new IntSet([1; 2; 3; 4; 5])
        
        let result = IntSet.fold 
                            (fun acc i -> callOrder := (acc, i) :: !callOrder; acc + i) 
                            0 
                            input
        if result    <> 15 then Assert.Fail()
        if !callOrder <> [(10, 5); (6, 4); (3, 3); (1, 2); (0, 1)] then Assert.Fail()
        ()
        
    [<Test>]
    let foldBack () : unit =
        
        let emptySet : IntSet<decimal> = IntSet.empty
        let result = IntSet.foldBack (fun _ _ -> Assert.Fail(); -1I) emptySet 0I
        if result <> 0I then Assert.Fail()
        
        let callOrder = ref ([] : (int * int) list)
        let input = new IntSet([1; 2; 3; 4; 5])
        
        let result = IntSet.foldBack
                            (fun i acc -> callOrder := (acc, i) :: !callOrder; acc + i) 
                            input
                            0
        if result    <> 15 then Assert.Fail()
        if !callOrder <> [(14, 1); (12, 2); (9, 3); (5, 4); (0, 5)] then Assert.Fail()
        ()

    [<Test>]
    let forall () : unit =

        let emptySet : IntSet<string> = IntSet.empty
        let result = IntSet.forall (fun x -> Assert.Fail(); false) emptySet
        if result <> true then Assert.Fail()
        
        let seta = new IntSet( [1 .. 99] |> List.map (fun i -> i.ToString()) )
        let result = seta |> IntSet.forall (fun str -> str.Length < 3)
        Assert.IsTrue(result)

        let setb = new IntSet( [50 .. 150] |> List.map (fun i -> i.ToString()) )
        let result = setb |> IntSet.forall (fun str -> str.Length < 3)
        Assert.IsFalse(result)
        ()

    [<Test>]
    let intersect () : unit =
        
        let emptySet1 : IntSet = IntSet.empty
        let emptySet2 : IntSet = IntSet.empty
        let four                = IntSet.singleton 4
       
        let emptyInterEmpty = IntSet.intersect emptySet1 emptySet2
        Assert.IsTrue( (emptyInterEmpty = emptySet1) )
        
        let xInterEmpty = IntSet.intersect four emptySet1
        Assert.IsFalse( (four = xInterEmpty) )
        
        let emptyInterX = IntSet.intersect emptySet1 four
        Assert.IsFalse( (four = emptyInterX) )
        ()
    
    [<Test>]
    let intersect2 () : unit =
        let a = new IntSet([3; 4; 5; 6])
        let b = new IntSet([5; 6; 7; 8])
        
        let intersection   = IntSet.intersect a b
        let expectedResult = new IntSet([5; 6])
        Assert.IsTrue( (intersection = expectedResult) )

    
    [<Test>]
    let intersectMany () : unit =
        (* IntersectAll
            1234567
             234567
              34567
               4567
                567
                 67 *)
        let setsToIntersect = 
            [
                for i = 1 to 6 do
                    yield new IntSet([i .. 7])
            ]
            
        let result : IntSet = IntSet.intersectMany setsToIntersect
        Assert.IsTrue(result.Count = 2)
        
        let contains x s = s |> IntSet.exists (fun i -> i = x) 
        Assert.IsTrue(contains 6 result)
        Assert.IsTrue(contains 7 result)
                  
    [<Test>]
    let intersectMany2 () : unit =
        let all   = new IntSet([1 .. 10])
        let odds  = new IntSet([1 .. 2 .. 10])
        let evens = new IntSet([2 .. 2 .. 10])
        
        let result = IntSet.intersectMany [odds; evens; all]
        Assert.IsTrue(IntSet.count result = 0)

    [<Test>]
    let intersectMany3 () : unit =
        let all   = new IntSet([1 .. 10])
        let empty = IntSet.empty : IntSet
        
        let result = IntSet.intersectMany [all; empty; all]
        Assert.IsTrue(IntSet.count result = 0)
        
        
    [<Test>]
    let intersectMany4 () : unit =
        checkThrowsArgumentException (fun () -> IntSet.intersectMany (Seq.empty : seq<IntSet>) |> ignore)
        ()

    [<Test>]
    let union () : unit =
        let emptySet1 : IntSet = IntSet.empty
        let emptySet2 : IntSet = IntSet.empty
        let four                 = IntSet.singleton 4
       
        let emptyUnionEmpty = IntSet.union emptySet1 emptySet2
        Assert.IsTrue( (emptyUnionEmpty = emptySet1) )
        
        let xUnionEmpty = IntSet.union four emptySet1
        Assert.IsTrue( (four = xUnionEmpty) )
        
        let emptyUnionX = IntSet.union emptySet1 four
        Assert.IsTrue( (four = emptyUnionX) )
        ()
    
    [<Test>]
    let union2 () : unit =
        let a = new IntSet([1; 2; 3; 4])
        let b = new IntSet([5; 6; 7; 8])
        
        let union = IntSet.union a b
        let expectedResult = new IntSet([1 .. 8])
        Assert.IsTrue( (union = expectedResult) )

    [<Test>]
    let union3 () : unit =
        let x : IntSet = 
            IntSet.singleton 1
            |> IntSet.union (IntSet.singleton 1)
            |> IntSet.union (IntSet.singleton 1)
            |> IntSet.union (IntSet.singleton 1)
            
        Assert.IsTrue(x.Count = 1)
        
    [<Test>]
    let unionMany () : unit =
        let odds  = new IntSet([1 .. 2 .. 10])
        let evens = new IntSet([2 .. 2 .. 10])
        let empty = IntSet.empty : IntSet
        let rest  = new IntSet([11 .. 19])
        let zero  = IntSet.singleton 0
        
        let result : IntSet = IntSet.unionMany [odds; evens; empty; rest; zero]
        Assert.IsTrue(result.Count = 20)

    [<Test>]
    let unionMany2 () : unit =
        let result : IntSet = IntSet.unionMany (Seq.empty : seq<IntSet>)
        Assert.IsTrue(result.Count = 0)
        
    [<Test>]
    let isEmpty () : unit =
        let zero  = IntSet.empty : IntSet
        let zero2 = new IntSet([])
        let one   = IntSet.singleton 123
        let n     = new IntSet( [1 .. 10] )
        
        Assert.IsTrue(IntSet.isEmpty zero)
        Assert.IsTrue(IntSet.isEmpty zero2)
        
        Assert.IsFalse(IntSet.isEmpty one)
        Assert.IsFalse(IntSet.isEmpty n)
        
    [<Test>]
    let iter () : unit =

        // Empty set
        IntSet.empty |> IntSet.iter (fun _ -> Assert.Fail())

        // Full set
        let elements = [| for i = 3 to 12 do yield false |]
        
        let set = new IntSet([3 .. 12])
        IntSet.iter (fun c ->
            let i = int c - 3
            elements.[i] <- true) set
        
        Assert.IsTrue (Array.forall ( (=) true ) elements)

    [<Test>]
    let partition () : unit =
        
        // Empty
        let resulta, resultb = IntSet.partition (fun (x : int) -> Assert.Fail(); false) IntSet.empty
        Assert.IsTrue(resulta.Count = 0 && resultb.Count = 0)

        // One
        let single = IntSet.singleton "foo"
        
        let resulta, resultb = IntSet.partition (fun (str : string) -> str.Length <> 3) single
        Assert.IsTrue(resulta.Count = 0 && resultb.Count = 1)
        
        let resulta, resultb = IntSet.partition (fun (str : string) -> str.Length = 3) single
        Assert.IsTrue(resulta.Count = 1 && resultb.Count = 0)

        // Multi
        let alphabet = IntSet.ofList ['a' .. 'z']
        let isVowel = function |'a' | 'e' | 'i' | 'o' | 'u' -> true
                               | _ -> false

        let resulta, resultb = IntSet.partition isVowel alphabet
        Assert.IsTrue(resulta.Count = 5 && resultb.Count = 21)

    [<Test>]
    let remove () : unit =
        
        let emptySet : IntSet = IntSet.empty
        let result = IntSet.remove 42 emptySet
        Assert.IsTrue(result.Count = 0)
        
        // One
        let single = IntSet.singleton 100I
        let resulta = IntSet.remove 100I single
        let resultb = IntSet.remove   1I single
        
        Assert.IsTrue (resulta.Count = 0)
        Assert.IsTrue (resultb.Count = 1)
        
        // Multi
        let a = new IntSet([1 .. 5])
        Assert.IsTrue(a.Count = 5)
        
        let b = IntSet.remove 3 a
        Assert.IsTrue(b.Count = 4)
        // Call again, double delete
        let c = IntSet.remove 3 b
        Assert.IsTrue(c.Count = 4)
        
        Assert.IsFalse(IntSet.exists ( (=) 3 ) c)

    [<Test>]
    let ofList () : unit =
        
        // Empty
        let emptySet = IntSet.ofList ([] : (string * int * IntSet) list)
        Assert.IsTrue(IntSet.isEmpty emptySet)
        
        // Single
        let single = IntSet.ofList [1]
        Assert.IsTrue(single.Count = 1)
        Assert.IsTrue(IntSet.exists ( (=) 1 ) single)
        
        // Multi
        let multi = IntSet.ofList ["mon"; "tue"; "wed"; "thu"; "fri"]
        Assert.IsTrue(multi.Count = 5)
        let expected = new IntSet(["mon"; "tue"; "wed"; "thu"; "fri"])
        Assert.IsTrue( (multi = expected) )

    [<Test>]
    let toList () : unit =

        // Empty
        let emptySet : IntSet<byte> = IntSet.empty
        Assert.IsTrue(IntSet.toList emptySet = [])
        
        // Single
        let single = IntSet.singleton "stuff"
        Assert.IsTrue(IntSet.toList single = ["stuff"])
        
        // Multi
        let multi = new IntSet([5; 2; 3; 1; 4])
        Assert.IsTrue(IntSet.toList multi = [1; 2; 3; 4; 5])

    [<Test>]
    let ofArray () : unit =
        
        // Empty
        let emptySet = IntSet.ofArray ([| |] : (string * int * IntSet) [])
        Assert.IsTrue(IntSet.isEmpty emptySet)
        
        // Single
        let single = IntSet.ofArray [| 1 |]
        Assert.IsTrue(single.Count = 1)
        Assert.IsTrue(IntSet.exists ( (=) 1 ) single)
        
        // Multi
        let multi = IntSet.ofArray [| "mon"; "tue"; "wed"; "thu"; "fri" |]
        Assert.IsTrue(multi.Count = 5)
        let expected = new IntSet(["mon"; "tue"; "wed"; "thu"; "fri"])
        Assert.IsTrue( (multi = expected) )

    [<Test>]
    let toArray () : unit =

        // Empty
        let emptySet : IntSet<byte> = IntSet.empty
        Assert.IsTrue(IntSet.toArray emptySet = [| |])
        
        // Single
        let single = IntSet.singleton "stuff"
        Assert.IsTrue(IntSet.toArray single = [| "stuff" |])
        
        // Multi
        let multi = new IntSet([5; 2; 3; 1; 4])
        Assert.IsTrue(IntSet.toArray multi = [| 1; 2; 3; 4; 5 |])


    [<Test>]
    let ofSeq () : unit =
        
        // Empty
        let emptySet = IntSet.ofSeq ([| |] : (string * int * IntSet) [])
        Assert.IsTrue(IntSet.isEmpty emptySet)
        
        // Single
        let single = IntSet.ofSeq [ 1 ]
        Assert.IsTrue(single.Count = 1)
        Assert.IsTrue(IntSet.exists ( (=) 1 ) single)
        
        // Multi
        let multi = IntSet.ofSeq [| "mon"; "tue"; "wed"; "thu"; "fri" |]
        Assert.IsTrue(multi.Count = 5)
        let expected = new IntSet(["mon"; "tue"; "wed"; "thu"; "fri"])
        Assert.IsTrue( (multi = expected) )

    [<Test>]
    let toSeq () : unit =

        // Empty
        let emptySet : IntSet<byte> = IntSet.empty
        let emptySeq = IntSet.toSeq emptySet
        Assert.IsTrue (Seq.length emptySeq = 0)
        
        // Single
        let single = IntSet.singleton "stuff"
        let singleSeq = IntSet.toSeq single
        Assert.IsTrue(Seq.toList singleSeq = [ "stuff" ])
        
        // Multi
        let multi = new IntSet([5; 2; 3; 1; 4])
        let multiSeq = IntSet.toSeq multi
        Assert.IsTrue(Seq.toList multiSeq = [ 1; 2; 3; 4; 5 ])
        
    [<Test>]
    let minElement () : unit =
        // Check for an argument exception "Set contains no members"
        checkThrowsArgumentException(fun () -> IntSet.minElement IntSet.empty |> ignore)
        
        let set1 = IntSet.ofList [10; 8; 100; 1; 50]
        Assert.AreEqual(IntSet.minElement set1, 1)
        
        let set2 = IntSet.ofList ["abcd"; "a"; "abc"; "ab"]
        Assert.AreEqual(IntSet.minElement set2, "a")
        
    [<Test>]
    let maxElement () : unit =
        // Check for an argument exception "Set contains no members"
        checkThrowsArgumentException(fun () -> IntSet.maxElement IntSet.empty |> ignore)
        
        let set1 = IntSet.ofList [10; 8; 100; 1; 50]
        Assert.AreEqual(IntSet.maxElement set1, 100)
        
        let set2 = IntSet.ofList ["abcd"; "a"; "abc"; "ab"]
        Assert.AreEqual(IntSet.maxElement set2, "abcd")


    [<Test>]
    let isProperSubset () : unit =
        let set1 = IntSet.ofList [10; 8; 100]
        let set2 = IntSet.ofList [100]
        Assert.IsTrue(IntSet.isProperSubset set2 set1)
        Assert.IsTrue(IntSet.isProperSubset IntSet.empty set2)
        Assert.IsFalse(IntSet.isProperSubset IntSet.empty IntSet.empty)
        Assert.IsFalse(IntSet.isProperSubset set1 set2)

    [<Test>]
    let isProperSuperset () : unit =
        let set1 = IntSet.ofList [10; 8; 100]
        let set2 = IntSet.ofList [100; 8]
        Assert.IsTrue(IntSet.isProperSuperset set1 set2)
        Assert.IsTrue(IntSet.isProperSuperset set2 IntSet.empty)
        Assert.IsFalse(IntSet.isProperSuperset IntSet.empty IntSet.empty)
        Assert.IsFalse(IntSet.isProperSuperset set1 set1)
        Assert.IsFalse(IntSet.isProperSuperset set2 set1)
        
    // ----- Not associated with a module function -----

    [<Test>]
    let ``General Test #1`` () : unit =
        // Retruns a random permutation of integers between the two bounds.
        let randomPermutation lowerBound upperBound = 
            let items = ResizeArray<_>([lowerBound .. upperBound])
            let rng = new Random()
            
            let randomPermutation = ResizeArray<int>()
            while items.Count > 0 do
                let idx = rng.Next() % items.Count
                let i = items.[idx]
                items.RemoveAt(idx)
                randomPermutation.Add(i)
            
            randomPermutation.ToArray()
        
        for i in 0..50 do
            let permutation = randomPermutation 0 i
            
            let set : IntSet ref = ref IntSet.empty
            // Add permutation items to set in order
            Array.iter (fun i -> set := IntSet.add i !set) permutation
            // Check that the set equals the full list
            Assert.IsTrue(IntSet.toList !set = [0 .. i])
            // Remove items in permutation order, ensuring set is delt with correctly
            Array.iteri
                (fun idx i -> set := IntSet.remove i !set
                              // Verify all elements have been correctly removed
                              let removedElements = Array.sub permutation 0 (idx + 1) |> IntSet.ofSeq
                              let inter : IntSet = IntSet.intersect !set removedElements
                              Assert.IsTrue(inter.Count = 0))
                permutation
        ()
*)


open FsCheck

(* TODO : Implement FsCheck tests. *)

