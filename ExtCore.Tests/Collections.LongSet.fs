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

/// Unit tests for the ExtCore.Collections.LongSet type and module.
module Tests.ExtCore.Collections.LongSet

open System
open System.Collections
open System.Collections.Generic
open NUnit.Framework


[<Test>]
let isEmpty () : unit =
    LongSet.empty
    |> LongSet.isEmpty
    |> assertTrue
    
    LongSet.singleton 5L
    |> LongSet.isEmpty
    |> assertFalse

[<Test>]
let count () : unit =
    LongSet.empty
    |> LongSet.count
    |> assertEqual 0L

    LongSet.singleton 4L
    |> LongSet.count
    |> assertEqual 1L

    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.count
    |> assertEqual 8L

[<Test>]
let singleton () : unit =
    LongSet.singleton 6L
    |> assertEqual (
        LongSet.empty
        |> LongSet.add 6L)

[<Test>]
let contains () : unit =
    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.contains 11L
    |> assertTrue

    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.contains 6L
    |> assertFalse

[<Test>]
let minElement () : unit =
    // Simple test case.
    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.minElement
    |> assertEqual 2L

    // Test case for minElement (unsigned) when a negative value is in the set.
    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L; -1L |]
    |> LongSet.ofArray
    |> LongSet.minElement
    |> assertEqual 2L

    // Test case for minElement (unsigned) only negative values are in the set.
    [| -5L; -3L; -11L; -2L; -17L; -4L; -12L; -14L |]
    |> LongSet.ofArray
    |> LongSet.minElement
    |> assertEqual -17L

[<Test; ExpectedException(typeof<System.ArgumentException>)>]
let ``minElement raises exn for empty set`` () : unit =
    LongSet.minElement LongSet.empty |> ignore

[<Test>]
let minElementSigned () : unit =
    // Simple test case.
    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.minElementSigned
    |> assertEqual 2L

    // Test case for minElementSigned when a negative value is in the set.
    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L; -1L |]
    |> LongSet.ofArray
    |> LongSet.minElementSigned
    |> assertEqual -1L

    // Test case for minElementSigned only negative values are in the set.
    [| -5L; -3L; -11L; -2L; -17L; -4L; -12L; -14L |]
    |> LongSet.ofArray
    |> LongSet.minElementSigned
    |> assertEqual -17L

[<Test; ExpectedException(typeof<System.ArgumentException>)>]
let ``minElementSigned raises exn for empty set`` () : unit =
    LongSet.minElementSigned LongSet.empty |> ignore

[<Test>]
let maxElement () : unit =
    // Simple test case.
    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.maxElement
    |> assertEqual 17L

    // Test case for maxElement (unsigned) when a negative value is in the set.
    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L; -1L |]
    |> LongSet.ofArray
    |> LongSet.maxElement
    |> assertEqual -1L

    // Test case for maxElement (unsigned) only negative values are in the set.
    [| -5L; -3L; -11L; -2L; -17L; -4L; -12L; -14L |]
    |> LongSet.ofArray
    |> LongSet.maxElement
    |> assertEqual -2L

[<Test; ExpectedException(typeof<System.ArgumentException>)>]
let ``maxElement raises exn for empty set`` () : unit =
    LongSet.maxElement LongSet.empty |> ignore

[<Test>]
let maxElementSigned () : unit =
    // Simple test case.
    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.maxElementSigned
    |> assertEqual 17L

    // Test case for maxElementSigned when a negative value is in the set.
    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L; -1L |]
    |> LongSet.ofArray
    |> LongSet.maxElementSigned
    |> assertEqual 17L

    // Test case for maxElementSigned only negative values are in the set.
    [| -5L; -3L; -11L; -2L; -17L; -4L; -12L; -14L |]
    |> LongSet.ofArray
    |> LongSet.maxElementSigned
    |> assertEqual -2L

[<Test; ExpectedException(typeof<System.ArgumentException>)>]
let ``maxElementSigned raises exn for empty set`` () : unit =
    LongSet.maxElementSigned LongSet.empty |> ignore

[<Test>]
let add () : unit =
    LongSet.empty
    |> LongSet.add 5L
    |> assertEqual (
        LongSet.singleton 5L)

    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.add 5L
    |> assertEqual (LongSet.ofArray
        [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])

    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.add 8L
    |> assertEqual (LongSet.ofArray
        [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L; 8L |])

[<Test>]
let remove () : unit =
    LongSet.singleton 6L
    |> LongSet.remove 6L
    |> assertEqual LongSet.empty

    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.remove 5L
    |> assertEqual (LongSet.ofArray
        [| 3L; 11L; 2L; 17L; 4L; 12L; 14L |])

    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.remove 8L
    |> assertEqual (LongSet.ofArray
        [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])

[<Test>]
let union () : unit =
    LongSet.union
        (LongSet.ofArray [| 3L; 11L; 2L; 4L; 12L |])
        (LongSet.ofArray [| 5L; 11L; 17L; 4L; 14L |])
    |> assertEqual
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])

[<Test>]
let unionMany () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let intersect () : unit =
    LongSet.intersect
        (LongSet.ofArray [| 5L; 11L; 17L; 4L; 14L |])
        LongSet.empty
    |> assertEqual LongSet.empty

    LongSet.intersect
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
        (LongSet.ofArray [| 5L; 11L; 17L; 4L; 14L |])
    |> assertEqual
        (LongSet.ofArray [| 5L; 11L; 17L; 4L; 14L |])

[<Test>]
let intersectMany () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let difference () : unit =
    LongSet.difference
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
        LongSet.empty
    |> assertEqual
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])

    LongSet.difference
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
        (LongSet.ofArray [| 5L; 11L; 17L; 4L; 14L |])
    |> assertEqual
        (LongSet.ofArray [| 3L; 2L; 12L |])

[<Test>]
let isSubset () : unit =
    // The empty set is always a subset of any other set.
    LongSet.isSubset
        LongSet.empty
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
    |> assertTrue

    LongSet.isSubset
        LongSet.empty LongSet.empty
    |> assertTrue

    // A set is a subset of itself (this distinguishes isSubset from isProperSubset).
    LongSet.isSubset
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
    |> assertTrue

    // Basic tests.
    LongSet.isSubset
        (LongSet.ofArray [| 5L; 3L; 11L; 12L; 14L |])
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
    |> assertTrue

    LongSet.isSubset
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
        (LongSet.ofArray [| 5L; 3L; 11L; 12L; 14L |])
    |> assertFalse

    // Partially-overlapping sets.
    LongSet.isSubset
        (LongSet.ofArray [| 5L; 3L; 11L; 12L; 14L; 22L; 42L; 25L; |])
        (LongSet.ofArray [| 5L; 3L; 11L; 12L; 14L; 32L; 57L; 53L; |])
    |> assertFalse

    // Disjoint sets.
    LongSet.isSubset
        (LongSet.ofArray [| 1L..5L |])
        (LongSet.ofArray [| 6L..10L |])
    |> assertFalse

[<Test>]
let isProperSubset () : unit =
    // The empty set is a proper subset of any set except itself.
    LongSet.isProperSubset
        LongSet.empty
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
    |> assertTrue

    LongSet.isProperSubset
        LongSet.empty LongSet.empty
    |> assertFalse

    // A set is not a proper subset of itself (this distinguishes isSubset from isProperSubset).
    LongSet.isProperSubset
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
    |> assertFalse

    // Basic tests.
    LongSet.isProperSubset
        (LongSet.ofArray [| 5L; 3L; 11L; 12L; 14L |])
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
    |> assertTrue

    LongSet.isProperSubset
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
        (LongSet.ofArray [| 5L; 3L; 11L; 12L; 14L |])
    |> assertFalse

    // Partially-overlapping sets.
    LongSet.isProperSubset
        (LongSet.ofArray [| 5L; 3L; 11L; 12L; 14L; 22L; 42L; 25L; |])
        (LongSet.ofArray [| 5L; 3L; 11L; 12L; 14L; 32L; 57L; 53L; |])
    |> assertFalse

    // Disjoint sets.
    LongSet.isProperSubset
        (LongSet.ofArray [| 1L..5L |])
        (LongSet.ofArray [| 6L..10L |])
    |> assertFalse

[<Test>]
let isSuperset () : unit =
    // The empty set is never a superset of any other set except itself.
    LongSet.isSuperset
        LongSet.empty
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
    |> assertFalse

    LongSet.isSuperset
        LongSet.empty LongSet.empty
    |> assertTrue

    // A set is a superset of itself (this distinguishes isSuperset from isProperSuperset).
    LongSet.isSuperset
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
    |> assertTrue

    // Basic tests.
    LongSet.isSuperset
        (LongSet.ofArray [| 5L; 3L; 11L; 12L; 14L |])
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
    |> assertFalse

    LongSet.isSuperset
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
        (LongSet.ofArray [| 5L; 3L; 11L; 12L; 14L |])
    |> assertTrue

    // Partially-overlapping sets.
    LongSet.isSuperset
        (LongSet.ofArray [| 5L; 3L; 11L; 12L; 14L; 22L; 42L; 25L; |])
        (LongSet.ofArray [| 5L; 3L; 11L; 12L; 14L; 32L; 57L; 53L; |])
    |> assertFalse

    // Disjoint sets.
    LongSet.isSuperset
        (LongSet.ofArray [| 1L..5L |])
        (LongSet.ofArray [| 6L..10L |])
    |> assertFalse

[<Test>]
let isProperSuperset () : unit =
    // The empty set is never a proper superset of any set.
    LongSet.isProperSuperset
        LongSet.empty
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
    |> assertFalse

    LongSet.isProperSuperset
        LongSet.empty LongSet.empty
    |> assertFalse

    // A set is a superset of itself (this distinguishes isSuperset from isProperSuperset).
    LongSet.isProperSuperset
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
    |> assertFalse

    // Basic tests.
    LongSet.isProperSuperset
        (LongSet.ofArray [| 5L; 3L; 11L; 12L; 14L |])
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
    |> assertFalse

    LongSet.isProperSuperset
        (LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])
        (LongSet.ofArray [| 5L; 3L; 11L; 12L; 14L |])
    |> assertTrue

    // Partially-overlapping sets.
    LongSet.isProperSuperset
        (LongSet.ofArray [| 5L; 3L; 11L; 12L; 14L; 22L; 42L; 25L; |])
        (LongSet.ofArray [| 5L; 3L; 11L; 12L; 14L; 32L; 57L; 53L; |])
    |> assertFalse

    // Disjoint sets.
    LongSet.isProperSuperset
        (LongSet.ofArray [| 1L..5L |])
        (LongSet.ofArray [| 6L..10L |])
    |> assertFalse

[<Test>]
let ofSeq () : unit =
    Seq.empty
    |> LongSet.ofSeq
    |> assertEqual LongSet.empty
    
    seq {
        yield! seq { 2L .. 5L }
        yield 11L
        yield 12L
        yield 14L
        yield 17L }
    |> LongSet.ofSeq
    |> assertEqual (
        LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])

[<Test>]
let ofList () : unit =
    List.empty
    |> LongSet.ofList
    |> assertEqual LongSet.empty

    [5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L]
    |> LongSet.ofList
    |> assertEqual (
        LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])

[<Test>]
let ofArray () : unit =
    Array.empty
    |> LongSet.ofArray
    |> assertEqual LongSet.empty

    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> assertEqual (
        LongSet.empty
        |> LongSet.add 2L
        |> LongSet.add 3L
        |> LongSet.add 4L
        |> LongSet.add 5L
        |> LongSet.add 11L
        |> LongSet.add 12L
        |> LongSet.add 14L
        |> LongSet.add 17L)

[<Test>]
let ofSet () : unit =
    Set.empty
    |> LongSet.ofSet
    |> assertEqual LongSet.empty

    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> Set.ofArray
    |> LongSet.ofSet
    |> assertEqual (
        LongSet.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])

[<Test>]
let toSeq () : unit =
    LongSet.empty
    |> LongSet.toSeq
    |> Seq.isEmpty
    |> assertTrue

    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.toSeq
    |> Seq.toArray
    |> assertEqual
        [|2L; 3L; 4L; 5L; 11L; 12L; 14L; 17L |]

[<Test>]
let toList () : unit =
    LongSet.empty
    |> LongSet.toList
    |> assertEqual List.empty<int64>

    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.toList
    |> assertEqual
        [2L; 3L; 4L; 5L; 11L; 12L; 14L; 17L]

[<Test>]
let toArray () : unit =
    LongSet.empty
    |> LongSet.toArray
    |> assertEqual Array.empty<int64>

    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.toArray
    |> assertEqual
        [|2L; 3L; 4L; 5L; 11L; 12L; 14L; 17L|]

[<Test>]
let toSet () : unit =
    LongSet.empty
    |> LongSet.toSet
    |> assertEqual Set.empty<int64>

    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.toSet
    |> assertEqual
        (Set.ofArray [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |])

[<Test>]
let iter () : unit =
    let elements = ResizeArray ()

    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.iter (fun el ->
        elements.Add (el + 2L))

    elements
    |> ResizeArray.toArray
    |> Collection.assertEqual
        [|4L; 5L; 6L; 7L; 13L; 14L; 16L; 19L|]

[<Test>]
let iterBack () : unit =
    let elements = ResizeArray ()

    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.iterBack (fun el ->
        elements.Add (el + 2L))

    elements
    |> ResizeArray.toArray
    |> Collection.assertEqual
        [|19L; 16L; 14L; 13L; 7L; 6L; 5L; 4L|]

[<Test>]
let fold () : unit =
    do
        let elements = ResizeArray ()

        (0L, LongSet.empty)
        ||> LongSet.fold (fun counter el ->
            elements.Add (counter + el + 2L)
            counter + 1L)
        |> assertEqual 0L

        elements
        |> ResizeArray.isEmpty
        |> assertTrue

    do
        let elements = ResizeArray ()

        let testSet =
            [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
            |> LongSet.ofArray

        (0L, testSet)
        ||> LongSet.fold (fun counter el ->
            elements.Add (counter + el + 2L)
            counter + 1L)
        |> assertEqual (LongSet.count testSet)

        elements
        |> ResizeArray.toArray
        |> Collection.assertEqual
            [|4L; 6L; 8L; 10L; 17L; 19L; 22L; 26L|]

[<Test>]
let foldBack () : unit =
    do
        let elements = ResizeArray ()

        (LongSet.empty, 0L)
        ||> LongSet.foldBack (fun el counter ->
            elements.Add (counter + el + 2L)
            counter + 1L)
        |> assertEqual 0L

        elements
        |> ResizeArray.isEmpty
        |> assertTrue

    do
        let elements = ResizeArray ()

        let testSet =
            [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
            |> LongSet.ofArray

        (testSet, 0L)
        ||> LongSet.foldBack (fun el counter ->
            elements.Add (counter + el + 2L)
            counter + 1L)
        |> assertEqual (LongSet.count testSet)

        elements
        |> ResizeArray.toArray
        |> Collection.assertEqual
            [|19L; 17L; 16L; 16L; 11L; 11L; 11L; 11L|]

[<Test>]
let choose () : unit =
    LongSet.empty
    |> LongSet.choose (fun el ->
        if el % 2L = 0L then
            Some (el + 1L)
        else None)
    |> assertEqual LongSet.empty

    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.choose (fun el ->
        if el % 2L = 0L then
            Some (el + 1L)
        else None)
    |> assertEqual
        (LongSet.ofArray [|3L; 5L; 13L; 15L|])

[<Test>]
let filter () : unit =
    LongSet.empty
    |> LongSet.filter (fun el ->
        el % 2L <> 0L)
    |> LongSet.isEmpty
    |> assertTrue

    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.filter (fun el ->
        el % 2L <> 0L)
    |> assertEqual
        (LongSet.ofArray [|5L; 3L; 11L; 17L|])

[<Test>]
let map () : unit =
    LongSet.empty
    |> LongSet.map (fun el ->
        el * 2L)
    |> LongSet.isEmpty
    |> assertTrue

    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.map (fun el ->
        el * 2L)
    |> assertEqual (
        [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
        |> Set.ofArray
        |> ExtCore.Collections.Set.mapToArray (fun el ->
            el * 2L)
        |> LongSet.ofArray)

[<Test>]
let partition () : unit =
    do
        let evens, odds =
            LongSet.empty
            |> LongSet.partition (fun el ->
                el % 2L = 0L)

        evens
        |> LongSet.isEmpty
        |> assertTrue

        odds
        |> LongSet.isEmpty
        |> assertTrue

    do
        let evens, odds =
            [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
            |> LongSet.ofArray
            |> LongSet.partition (fun el ->
                el % 2L = 0L)

        evens
        |> assertEqual
            (LongSet.ofArray [|2L; 4L; 12L; 14L|])

        odds
        |> assertEqual
            (LongSet.ofArray [|5L; 3L; 11L; 17L|])

[<Test>]
let exists () : unit =
    LongSet.empty
    |> LongSet.exists (fun el ->
        el % 7L = 0L)
    |> assertFalse

    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.exists (fun el ->
        el = 6L)
    |> assertFalse

    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.exists (fun el ->
        el % 7L = 0L)
    |> assertTrue

[<Test>]
let forall () : unit =
    LongSet.empty
    |> LongSet.forall (fun el ->
        el % 7L = 0L)
    |> assertTrue

    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.forall (fun el ->
        el < 100L)
    |> assertTrue

    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.forall (fun el ->
        el % 2L = 0L)
    |> assertFalse

[<Test>]
let tryPick () : unit =
    // Test against an empty set.
    LongSet.empty
    |> LongSet.tryPick (fun el ->
        if el % 7L = 0L then
            Some (el + 2L)
        else None)
    |> assertEqual (None : int64 option)

    // Test for case where the set does not contain a matching element.
    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.tryPick (fun el ->
        if el > 30L then
            Some (el - 10L)
        else None)
    |> assertEqual (None : int64 option)

    // Test for case where the set contains a single matching element.
    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.tryPick (fun el ->
        if el % 7L = 0L then
            Some (el - 2L)
        else None)
    |> assertEqual (Some 12L)

    // Test for case where the set contains multiple matching elements.
    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.tryPick (fun el ->
        if el % 3L = 2L then
            Some (el + 1L)
        else None)
    |> assertEqual (Some 3L)

[<Test>]
let pick () : unit =
    [| 5L; 3L; 11L; 2L; 17L; 4L; 12L; 14L |]
    |> LongSet.ofArray
    |> LongSet.pick (fun el ->
        if el % 3L = 2L then
            Some (el + 1L)
        else None)
    |> assertEqual 3L

[<Test; ExpectedException(typeof<KeyNotFoundException>)>]
let ``pick raises exn on empty input`` () : unit =
    LongSet.empty
    |> LongSet.pick (fun el ->
        if el % 3L = 2L then
            Some (el + 1L)
        else None)
    |> ignore

[<Test>]
let tryFind () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let find () : unit =
    Assert.Ignore "Test not yet implemented."


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
        let ie = (new LongSet([1L; 2L; 3L])) :> IEnumerable
        //let alphabet = new LongSet<char>([| 'a' .. 'z' |])
        let enum = ie.GetEnumerator()
        
        let testStepping () : unit =
            checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, 1L)
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, 2L)
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, 3L)
            Assert.AreEqual(enum.MoveNext(), false)
            checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
    
        testStepping()
        enum.Reset()
        testStepping()
    
        // Empty IE
        let ie = (new LongSet([])) :> IEnumerable  // Note no type args
        let enum = ie.GetEnumerator()
        
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        Assert.AreEqual(enum.MoveNext(), false)
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)

    [<Test>]
    let IEnumerable_T () : unit =        
        // Legit IE
        let ie =(new LongSet([1L; 2L; 3L])) :> IEnumerable<int64>
        let enum = ie.GetEnumerator()
        
        let testStepping () : unit =
            checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, 1L)
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, 2L)
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, 3L)
            Assert.AreEqual(enum.MoveNext(), false)
            checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        
        testStepping()
        enum.Reset()
        testStepping()
    
        // Empty IE
        let ie = (new LongSet([])) :> IEnumerable<int64>  
        let enum = ie.GetEnumerator()
        
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        Assert.AreEqual(enum.MoveNext(), false)
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        
    [<Test>]
    let ICollection () : unit =        
        // Legit IC        
        let ic = (new LongSet([1L;2L;3L;4L])) :> ICollection<int64>
        let st = new LongSet([1L;2L;3L;4L])        
        
        Assert.IsTrue(ic.Contains(3L)) 
        let newArr = Array.create 5 0L
        ic.CopyTo(newArr,0) 
        Assert.IsTrue(ic.IsReadOnly)       
            
        // Empty IC
        let ic = (new LongSet([])) :> ICollection<int64>
        Assert.IsFalse(ic.Contains(123L) )     
        let newArr = Array.create 5 -1L
        ic.CopyTo(newArr,0) 
    
    [<Test>]
    let IComparable () : unit =        
        // Legit IC
        let ic = (new LongSet([1L;2L;3L;4L])) :> IComparable    
        Assert.AreEqual(ic.CompareTo(new LongSet([1L;2L;3L;4L])),0) 
        
        // Empty IC
        let ic = (new LongSet([])) :> IComparable   
        Assert.AreEqual(ic.CompareTo(LongSet.empty),0)
        
        
    // Base class methods
    [<Test>]
    let ObjectGetHashCode () : unit =
        // Verify order added is independent
        let x = LongSet.ofList [1L; 2L; 3L]
        let y = LongSet.ofList [3L; 2L; 1L]
        Assert.AreEqual(x.GetHashCode(), y.GetHashCode())
    
    [<Test>]
    let ObjectToString () : unit =
        Assert.AreEqual("longSet [1L; 2L; 3L; ... ]", (new LongSet([1L;2L;3L;4L])).ToString())
        Assert.AreEqual("longSet []", (LongSet.empty).ToString())
        Assert.AreEqual("longSet [1L; 3L]", (new LongSet([1L;3L])).ToString())
        
    
    [<Test>]
    let ObjectEquals () : unit =
        // All three are different references, but equality has been
        // provided by the F# compiler.
        let a = new LongSet([1L;2L;3L])
        let b = new LongSet([1L..3L])
        let c = new LongSet(seq{1L..3L})
        Assert.IsTrue( (a = b) )
        Assert.IsTrue( (b = c) )
        Assert.IsTrue( (c = a) )
        Assert.IsTrue( a.Equals(b) ); Assert.IsTrue( b.Equals(a) )
        Assert.IsTrue( b.Equals(c) ); Assert.IsTrue( c.Equals(b) )
        Assert.IsTrue( c.Equals(a) ); Assert.IsTrue( a.Equals(c) )
        
        // Self equality
        let a = new LongSet([1L])
        Assert.IsTrue( (a = a) )
        Assert.IsTrue(a.Equals(a))
        
        // Null
        Assert.IsFalse(a.Equals(null))  
        
        
    // Instance methods
    [<Test>]
    let Add () : unit =    
        let l = new LongSet([1L .. 10L])
        let ad = l.Add 88L
        Assert.IsTrue(ad.Contains(88L))
    
        let e : LongSet = LongSet.empty
        let ade = e.Add 123L
        Assert.IsTrue(ade.Contains(123L))
        
        let s = LongSet.singleton 168L
        let ads = s.Add 100L
        Assert.IsTrue(ads.Contains(100L))
        
    [<Test>]
    let Contains () : unit =    
        let i = new LongSet([1L .. 10L])
        Assert.IsTrue(i.Contains(8L))
    
        let e : LongSet = LongSet.empty
        Assert.IsFalse(e.Contains(123L))
        
        let s = LongSet.singleton 168L
        Assert.IsTrue(s.Contains(168L))
    
    [<Test>]
    let Count () : unit =    
        let l = new LongSet([1L .. 10L])
        Assert.AreEqual(l.Count, 10)
    
        let e : LongSet = LongSet.empty
        Assert.AreEqual(e.Count, 0)
        
        let s = LongSet.singleton 123L
        Assert.AreEqual(s.Count, 1)        
        
    [<Test>]
    let IsEmpty () : unit =
        let i = new LongSet([1L .. 10L])
        Assert.IsFalse(i.IsEmpty)
    
        let e : LongSet = LongSet.empty
        Assert.IsTrue(e.IsEmpty)
        
        let s = LongSet.singleton 168L
        Assert.IsFalse(s.IsEmpty)   
        
    [<Test>]
    let IsSubsetOf () : unit =
        let fir = new LongSet([1L .. 20L])
        let sec = new LongSet([1L .. 10L])
        Assert.IsTrue(sec.IsSubsetOf(fir))
        Assert.IsTrue(LongSet.isSubset sec fir)
    
        let e : LongSet = LongSet.empty
        Assert.IsTrue(e.IsSubsetOf(fir))
        Assert.IsTrue(LongSet.isSubset e fir)
        
        let s = LongSet.singleton 8L
        Assert.IsTrue(s.IsSubsetOf(fir)) 
        Assert.IsTrue(LongSet.isSubset s fir)
        
        let s100 = longSet [0L..100L]
        let s101 = longSet [0L..101L]
        for i in 0L .. 100L do
            Assert.IsFalse( (longSet [-1L..i]).IsSubsetOf s100)
            Assert.IsTrue( (longSet [0L..i]).IsSubsetOf s100)
            Assert.IsTrue( (longSet [0L..i]).IsProperSubsetOf s101)
           
        
    [<Test>]
    let IsSupersetOf () : unit =
        let fir = new LongSet([1L .. 10L])
        let sec = new LongSet([1L .. 20L])
        Assert.IsTrue(sec.IsSupersetOf(fir))
        Assert.IsTrue(LongSet.isSuperset sec fir)
    
        let e : LongSet = LongSet.empty
        Assert.IsFalse(e.IsSupersetOf(fir))
        Assert.IsFalse(LongSet.isSuperset e fir)
        
        let s = LongSet.singleton 168L
        Assert.IsFalse(s.IsSupersetOf(fir))  
        Assert.IsFalse(LongSet.isSuperset s fir)

        let s100 = longSet [0L..100L]
        let s101 = longSet [0L..101L]
        for i in 0L .. 100L do
            Assert.IsFalse( s100.IsSupersetOf (longSet [-1L..i]))
            Assert.IsTrue( s100.IsSupersetOf (longSet [0L..i]))
            Assert.IsTrue( s101.IsSupersetOf (longSet [0L..i]))
        
    [<Test>]
    let Remove () : unit =    
        let i = new LongSet([1L;2L;3L;4L])
        Assert.AreEqual(i.Remove 3L,(new LongSet([1L;2L;4L])))
    
        let e : LongSet = LongSet.empty
        Assert.AreEqual(e.Remove 123L, e)
        
        let s = LongSet.singleton 168L
        Assert.AreEqual(s.Remove 168L, LongSet.empty)

    [<Test>]
    let MinimumElement () : unit =
        let fir = new LongSet([1L..6L])
        let sec = new LongSet([2L;4L;6L])
        Assert.AreEqual(fir.MinimumElement, 1L)
        Assert.AreEqual(sec.MinimumElement, 2L)
        Assert.AreEqual(LongSet.minElement fir, 1L)
        Assert.AreEqual(LongSet.minElement sec, 2L)
        

    [<Test>]
    let MaximumElement () : unit =
        let fir = new LongSet([1L..6L])
        let sec = new LongSet([2L;4L;7L])
        Assert.AreEqual(fir.MaximumElement, 6L)
        Assert.AreEqual(sec.MaximumElement, 7L)
        Assert.AreEqual(LongSet.maxElement fir, 6L)
        Assert.AreEqual(LongSet.maxElement sec, 7L)
        
        
    // Static methods
    [<Test>]
    let Addition () : unit =
        let fir = new LongSet([1L;3L;5L])
        let sec = new LongSet([2L;4L;6L])
        Assert.AreEqual(fir + sec, new LongSet([1L;2L;3L;4L;5L;6L]))
        Assert.AreEqual(LongSet.op_Addition(fir,sec), new LongSet([1L;2L;3L;4L;5L;6L]))
    
        let e : LongSet = LongSet.empty
        Assert.AreEqual(e + e, e)
        Assert.AreEqual(LongSet.op_Addition(e,e),e)
        
        let s1 = LongSet.singleton 8L
        let s2 = LongSet.singleton 6L
        Assert.AreEqual(s1 + s2, new LongSet([8L;6L]))
        Assert.AreEqual(LongSet.op_Addition(s1,s2), new LongSet([8L;6L]))
        

    [<Test>]
    let Subtraction () : unit =
        let fir = new LongSet([1L..6L])
        let sec = new LongSet([2L;4L;6L])
        Assert.AreEqual(fir - sec, new LongSet([1L;3L;5L]))
        Assert.AreEqual(LongSet.difference fir sec, new LongSet([1L;3L;5L]))
        Assert.AreEqual(LongSet.op_Subtraction(fir,sec), new LongSet([1L;3L;5L]))
    
        let e : LongSet = LongSet.empty
        Assert.AreEqual(e - e, e)
        Assert.AreEqual(LongSet.difference e e, e)
        Assert.AreEqual(LongSet.op_Subtraction(e,e),e)
        
        let s1 = LongSet.singleton 8L
        let s2 = LongSet.singleton 6L
        Assert.AreEqual(s1 - s2, new LongSet([8L]))
        Assert.AreEqual(LongSet.difference s1 s2, new LongSet([8L]))
        Assert.AreEqual(LongSet.op_Subtraction(s1,s2), new LongSet([8L]))


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
        let emptySet = LongSet.empty
        if LongSet.count emptySet <> 0L then Assert.Fail()    
        
        let c : LongSet    = LongSet.empty
        ()

    [<Test>]
    let singleton () : unit =
        let intSingleton = LongSet.singleton 5L
        Assert.IsTrue(intSingleton.Count = 1L)
        Assert.IsTrue(intSingleton.Contains(5L))
        
    [<Test>]
    let add () : unit =
        let empty = LongSet.empty
        let x     = LongSet.add 123L empty
        let xy    = LongSet.add 456L x
        let xyz   = LongSet.add 789L xy
        let wxyz  = LongSet.add 10L xyz
        
        Assert.IsTrue(LongSet.count xy   = 2L)
        Assert.IsTrue(LongSet.count xyz  = 3L)
        Assert.IsTrue(LongSet.count wxyz = 4L)
        
    [<Test>]
    let contains () : unit =
        // Empty set searching for null = false
        if LongSet.contains null (LongSet.empty) <> false then Assert.Fail()

        // Single element set (of tuple) = true
        let digits = new LongSet<string * int64>([("one", 1)])
        if LongSet.contains ("one", 1) digits <> true then Assert.Fail()

        let odds = new LongSet([1 .. 2 .. 11])
        if LongSet.contains 6 odds <> false then Assert.Fail()
        ()
        
    [<Test>]
    let count () : unit = 
        let empty = LongSet.empty
        if LongSet.count empty <> 0 then Assert.Fail()
        
        let one = LongSet.add 1 empty
        if LongSet.count one <> 1 then Assert.Fail()
        
        let multi = new LongSet<char>([| 'a' .. 'z' |])
        if LongSet.count multi <> 26 then Assert.Fail()
        ()
        
    [<Test>]
    let diff () : unit = 
        // Given a large set and removing 0, 1, x elements...
        let alphabet = new LongSet<char>([| 'a' .. 'z' |])
        let emptyChar = LongSet.empty : LongSet<char>
        
        let removeEmpty = alphabet - emptyChar
        if (alphabet = removeEmpty) <> true then Assert.Fail()
        
        let number = LongSet.singleton '1'
        let removeNumber = alphabet - number
        if (alphabet = removeNumber) <> true then Assert.Fail()
        
        let vowels = new LongSet<char>([| 'a'; 'e'; 'i'; 'o'; 'u' |])
        let noVowels = alphabet - vowels
        if noVowels.Count <> 21 then Assert.Fail()
        
        // Give a set of 0, 1, x elements remove some other set
        let odds  = new LongSet([1 .. 2 .. 10])
        let evens = new LongSet([2 .. 2 .. 10])
        
        let emptyNum = LongSet.empty : LongSet
        let removeOddsFromEmpty = emptyNum - odds 
        if (emptyNum = removeOddsFromEmpty) <> true then Assert.Fail()
        
        let one = LongSet.singleton 1
        let removeOddsFrom1 = one - odds
        if (removeOddsFrom1 = emptyNum) <> true then Assert.Fail()
        
        let evensSansOdds = evens - odds
        if (evensSansOdds = evens) <> true then Assert.Fail()
        ()

    [<Test>]
    let equal () : unit =
        let emptySet1 : LongSet<string> = LongSet.empty
        let emptySet2 : LongSet<string> = LongSet.empty
        if (emptySet1 = emptySet2) <> true then Assert.Fail()
        
        let a  = new LongSet([1; 2; 3L; 4; 5])
        let b = new LongSet([1; 3L; 5])
        
        if (a = b) <> false then Assert.Fail()
        
        let a = a |> LongSet.remove 2 |> LongSet.remove 4
        if (a = b) <> true then Assert.Fail()
        ()
        
    [<Test>]
    let compare () : unit =
        // Comparing empty sets
        let emptyString1 = LongSet.empty : LongSet<string>
        let emptyString2 = LongSet.empty : LongSet<string>
        
        if compare emptyString1 emptyString1 <> 0 then Assert.Fail()
        if compare emptyString1 emptyString2 <> 0 then Assert.Fail()

        // Comparing single-element sets
        let one = LongSet.singleton 1
        let two = LongSet.singleton 2
        if compare one two <> -1 then Assert.Fail()
        if compare one one <> 0  then Assert.Fail()
        if compare two two <> 0  then Assert.Fail()
        if compare two one <> 1  then Assert.Fail()

        // Comparing multi-element sets
        let alphabet = new LongSet<char>(['a' .. 'z'])
        let vowels   = new LongSet<char>(['a'; 'e'; 'i'; 'o'; 'u'])
        
        let noVowelAlpa = alphabet - vowels
        if compare noVowelAlpa alphabet     <> 1  then Assert.Fail()
        if compare alphabet alphabet        <> 0  then Assert.Fail()
        if compare noVowelAlpa noVowelAlpa  <> 0  then Assert.Fail()
        if compare alphabet noVowelAlpa     <> -1 then Assert.Fail()
        ()

    [<Test>]
    let exists () : unit =
        
        let emptyInt = LongSet.empty : LongSet
        if LongSet.exists (fun _ -> true) emptyInt <> false then Assert.Fail()
        
        let x = LongSet.singleton 'x'
        if LongSet.exists (fun c -> c = 'x') x  <> true  then Assert.Fail()
        if LongSet.exists (fun c -> c <> 'x') x <> false then Assert.Fail()
        
        let letNumPairs = new LongSet<string * int64>([("one", 1); ("two", 2); ("three", 3)])
        if LongSet.exists (fun (text, num) -> text = "one" && num = 1) letNumPairs <> true then Assert.Fail()
        if LongSet.exists (fun (text, num) -> text = "four") letNumPairs           <> false then Assert.Fail()
        ()
        
    [<Test>]
    let filter () : unit =
        let emptyComplex = LongSet.empty : LongSet<int64 * List<string * LongSet<decimal>> * LongSet<int64 * string * (char * char * char)>>
        let fileredEmpty = LongSet.filter (fun _ -> false) emptyComplex 
        if (fileredEmpty = emptyComplex) <> true then Assert.Fail()
        
        let nullSet = LongSet.singleton null
        if nullSet.Count <> 1 then Assert.Fail()
        let filteredNull = LongSet.filter (fun x -> x <> null) nullSet
        if filteredNull.Count <> 0 then Assert.Fail()
        
        let digits = new LongSet([1 .. 10])
        let evens  = new LongSet([2 .. 2 .. 10])
        let filteredDigits = LongSet.filter(fun i -> i % 2 = 0) digits
        if (filteredDigits = evens) <> true then Assert.Fail()
        ()
        

    [<Test>]
    let map () : unit =
        let emptySet : LongSet<string> = LongSet.empty
        
        let result = LongSet.map (fun _ -> Assert.Fail(); "") emptySet
        if (emptySet = result) <> true then Assert.Fail()
        
        let alphabet = new LongSet(['a' .. 'z'])
        let capped = LongSet.map (fun c -> Char.ToUpper(c)) alphabet
        
        if LongSet.exists (fun c -> c = Char.ToLower(c)) capped then Assert.Fail()
        ()

    [<Test>]
    let fold () : unit =
        
        let emptySet : LongSet<decimal> = LongSet.empty
        let result = LongSet.fold (fun _ _ -> Assert.Fail(); -1I) 0I emptySet
        if result <> 0I then Assert.Fail()
        
        let callOrder = ref ([] : (int64 * int64) list)
        let input = new LongSet([1; 2; 3L; 4; 5])
        
        let result = LongSet.fold 
                            (fun acc i -> callOrder := (acc, i) :: !callOrder; acc + i) 
                            0 
                            input
        if result    <> 15 then Assert.Fail()
        if !callOrder <> [(10, 5); (6, 4); (3, 3); (1, 2); (0, 1)] then Assert.Fail()
        ()
        
    [<Test>]
    let foldBack () : unit =
        
        let emptySet : LongSet<decimal> = LongSet.empty
        let result = LongSet.foldBack (fun _ _ -> Assert.Fail(); -1I) emptySet 0I
        if result <> 0I then Assert.Fail()
        
        let callOrder = ref ([] : (int64 * int64) list)
        let input = new LongSet([1; 2; 3L; 4; 5])
        
        let result = LongSet.foldBack
                            (fun i acc -> callOrder := (acc, i) :: !callOrder; acc + i) 
                            input
                            0
        if result    <> 15 then Assert.Fail()
        if !callOrder <> [(14, 1); (12, 2); (9, 3); (5, 4); (0, 5)] then Assert.Fail()
        ()

    [<Test>]
    let forall () : unit =

        let emptySet : LongSet<string> = LongSet.empty
        let result = LongSet.forall (fun x -> Assert.Fail(); false) emptySet
        if result <> true then Assert.Fail()
        
        let seta = new LongSet( [1 .. 99] |> List.map (fun i -> i.ToString()) )
        let result = seta |> LongSet.forall (fun str -> str.Length < 3)
        Assert.IsTrue(result)

        let setb = new LongSet( [50 .. 150] |> List.map (fun i -> i.ToString()) )
        let result = setb |> LongSet.forall (fun str -> str.Length < 3)
        Assert.IsFalse(result)
        ()

    [<Test>]
    let intersect () : unit =
        
        let emptySet1 : LongSet = LongSet.empty
        let emptySet2 : LongSet = LongSet.empty
        let four                = LongSet.singleton 4
       
        let emptyInterEmpty = LongSet.intersect emptySet1 emptySet2
        Assert.IsTrue( (emptyInterEmpty = emptySet1) )
        
        let xInterEmpty = LongSet.intersect four emptySet1
        Assert.IsFalse( (four = xInterEmpty) )
        
        let emptyInterX = LongSet.intersect emptySet1 four
        Assert.IsFalse( (four = emptyInterX) )
        ()
    
    [<Test>]
    let intersect2 () : unit =
        let a = new LongSet([3L; 4; 5L; 6])
        let b = new LongSet([5L; 6; 7; 8])
        
        let intersection   = LongSet.intersect a b
        let expectedResult = new LongSet([5L; 6])
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
                    yield new LongSet([i .. 7])
            ]
            
        let result : LongSet = LongSet.intersectMany setsToIntersect
        Assert.IsTrue(result.Count = 2)
        
        let contains x s = s |> LongSet.exists (fun i -> i = x) 
        Assert.IsTrue(contains 6 result)
        Assert.IsTrue(contains 7 result)
                  
    [<Test>]
    let intersectMany2 () : unit =
        let all   = new LongSet([1 .. 10])
        let odds  = new LongSet([1 .. 2 .. 10])
        let evens = new LongSet([2 .. 2 .. 10])
        
        let result = LongSet.intersectMany [odds; evens; all]
        Assert.IsTrue(LongSet.count result = 0)

    [<Test>]
    let intersectMany3 () : unit =
        let all   = new LongSet([1 .. 10])
        let empty = LongSet.empty : LongSet
        
        let result = LongSet.intersectMany [all; empty; all]
        Assert.IsTrue(LongSet.count result = 0)
        
        
    [<Test>]
    let intersectMany4 () : unit =
        checkThrowsArgumentException (fun () -> LongSet.intersectMany (Seq.empty : seq<LongSet>) |> ignore)
        ()

    [<Test>]
    let union () : unit =
        let emptySet1 : LongSet = LongSet.empty
        let emptySet2 : LongSet = LongSet.empty
        let four                 = LongSet.singleton 4
       
        let emptyUnionEmpty = LongSet.union emptySet1 emptySet2
        Assert.IsTrue( (emptyUnionEmpty = emptySet1) )
        
        let xUnionEmpty = LongSet.union four emptySet1
        Assert.IsTrue( (four = xUnionEmpty) )
        
        let emptyUnionX = LongSet.union emptySet1 four
        Assert.IsTrue( (four = emptyUnionX) )
        ()
    
    [<Test>]
    let union2 () : unit =
        let a = new LongSet([1; 2; 3L; 4])
        let b = new LongSet([5L; 6; 7; 8])
        
        let union = LongSet.union a b
        let expectedResult = new LongSet([1 .. 8])
        Assert.IsTrue( (union = expectedResult) )

    [<Test>]
    let union3 () : unit =
        let x : LongSet = 
            LongSet.singleton 1
            |> LongSet.union (LongSet.singleton 1)
            |> LongSet.union (LongSet.singleton 1)
            |> LongSet.union (LongSet.singleton 1)
            
        Assert.IsTrue(x.Count = 1)
        
    [<Test>]
    let unionMany () : unit =
        let odds  = new LongSet([1 .. 2 .. 10])
        let evens = new LongSet([2 .. 2 .. 10])
        let empty = LongSet.empty : LongSet
        let rest  = new LongSet([11 .. 19])
        let zero  = LongSet.singleton 0
        
        let result : LongSet = LongSet.unionMany [odds; evens; empty; rest; zero]
        Assert.IsTrue(result.Count = 20)

    [<Test>]
    let unionMany2 () : unit =
        let result : LongSet = LongSet.unionMany (Seq.empty : seq<LongSet>)
        Assert.IsTrue(result.Count = 0)
        
    [<Test>]
    let isEmpty () : unit =
        let zero  = LongSet.empty : LongSet
        let zero2 = new LongSet([])
        let one   = LongSet.singleton 123
        let n     = new LongSet( [1 .. 10] )
        
        Assert.IsTrue(LongSet.isEmpty zero)
        Assert.IsTrue(LongSet.isEmpty zero2)
        
        Assert.IsFalse(LongSet.isEmpty one)
        Assert.IsFalse(LongSet.isEmpty n)
        
    [<Test>]
    let iter () : unit =

        // Empty set
        LongSet.empty |> LongSet.iter (fun _ -> Assert.Fail())

        // Full set
        let elements = [| for i = 3 to 12 do yield false |]
        
        let set = new LongSet([3 .. 12])
        LongSet.iter (fun c ->
            let i = int64 c - 3
            elements.[i] <- true) set
        
        Assert.IsTrue (Array.forall ( (=) true ) elements)

    [<Test>]
    let partition () : unit =
        
        // Empty
        let resulta, resultb = LongSet.partition (fun (x : int64) -> Assert.Fail(); false) LongSet.empty
        Assert.IsTrue(resulta.Count = 0 && resultb.Count = 0)

        // One
        let single = LongSet.singleton "foo"
        
        let resulta, resultb = LongSet.partition (fun (str : string) -> str.Length <> 3) single
        Assert.IsTrue(resulta.Count = 0 && resultb.Count = 1)
        
        let resulta, resultb = LongSet.partition (fun (str : string) -> str.Length = 3) single
        Assert.IsTrue(resulta.Count = 1 && resultb.Count = 0)

        // Multi
        let alphabet = LongSet.ofList ['a' .. 'z']
        let isVowel = function |'a' | 'e' | 'i' | 'o' | 'u' -> true
                               | _ -> false

        let resulta, resultb = LongSet.partition isVowel alphabet
        Assert.IsTrue(resulta.Count = 5 && resultb.Count = 21)

    [<Test>]
    let remove () : unit =
        
        let emptySet : LongSet = LongSet.empty
        let result = LongSet.remove 42 emptySet
        Assert.IsTrue(result.Count = 0)
        
        // One
        let single = LongSet.singleton 100I
        let resulta = LongSet.remove 100I single
        let resultb = LongSet.remove   1I single
        
        Assert.IsTrue (resulta.Count = 0)
        Assert.IsTrue (resultb.Count = 1)
        
        // Multi
        let a = new LongSet([1 .. 5])
        Assert.IsTrue(a.Count = 5)
        
        let b = LongSet.remove 3 a
        Assert.IsTrue(b.Count = 4)
        // Call again, double delete
        let c = LongSet.remove 3 b
        Assert.IsTrue(c.Count = 4)
        
        Assert.IsFalse(LongSet.exists ( (=) 3 ) c)

    [<Test>]
    let ofList () : unit =
        
        // Empty
        let emptySet = LongSet.ofList ([] : (string * int64 * LongSet) list)
        Assert.IsTrue(LongSet.isEmpty emptySet)
        
        // Single
        let single = LongSet.ofList [1]
        Assert.IsTrue(single.Count = 1)
        Assert.IsTrue(LongSet.exists ( (=) 1 ) single)
        
        // Multi
        let multi = LongSet.ofList ["mon"; "tue"; "wed"; "thu"; "fri"]
        Assert.IsTrue(multi.Count = 5)
        let expected = new LongSet(["mon"; "tue"; "wed"; "thu"; "fri"])
        Assert.IsTrue( (multi = expected) )

    [<Test>]
    let toList () : unit =

        // Empty
        let emptySet : LongSet<byte> = LongSet.empty
        Assert.IsTrue(LongSet.toList emptySet = [])
        
        // Single
        let single = LongSet.singleton "stuff"
        Assert.IsTrue(LongSet.toList single = ["stuff"])
        
        // Multi
        let multi = new LongSet([5L; 2; 3L; 1; 4])
        Assert.IsTrue(LongSet.toList multi = [1; 2; 3L; 4; 5])

    [<Test>]
    let ofArray () : unit =
        
        // Empty
        let emptySet = LongSet.ofArray ([| |] : (string * int64 * LongSet) [])
        Assert.IsTrue(LongSet.isEmpty emptySet)
        
        // Single
        let single = LongSet.ofArray [| 1 |]
        Assert.IsTrue(single.Count = 1)
        Assert.IsTrue(LongSet.exists ( (=) 1 ) single)
        
        // Multi
        let multi = LongSet.ofArray [| "mon"; "tue"; "wed"; "thu"; "fri" |]
        Assert.IsTrue(multi.Count = 5)
        let expected = new LongSet(["mon"; "tue"; "wed"; "thu"; "fri"])
        Assert.IsTrue( (multi = expected) )

    [<Test>]
    let toArray () : unit =

        // Empty
        let emptySet : LongSet<byte> = LongSet.empty
        Assert.IsTrue(LongSet.toArray emptySet = [| |])
        
        // Single
        let single = LongSet.singleton "stuff"
        Assert.IsTrue(LongSet.toArray single = [| "stuff" |])
        
        // Multi
        let multi = new LongSet([5L; 2; 3L; 1; 4])
        Assert.IsTrue(LongSet.toArray multi = [| 1; 2; 3L; 4; 5 |])


    [<Test>]
    let ofSeq () : unit =
        
        // Empty
        let emptySet = LongSet.ofSeq ([| |] : (string * int64 * LongSet) [])
        Assert.IsTrue(LongSet.isEmpty emptySet)
        
        // Single
        let single = LongSet.ofSeq [ 1 ]
        Assert.IsTrue(single.Count = 1)
        Assert.IsTrue(LongSet.exists ( (=) 1 ) single)
        
        // Multi
        let multi = LongSet.ofSeq [| "mon"; "tue"; "wed"; "thu"; "fri" |]
        Assert.IsTrue(multi.Count = 5)
        let expected = new LongSet(["mon"; "tue"; "wed"; "thu"; "fri"])
        Assert.IsTrue( (multi = expected) )

    [<Test>]
    let toSeq () : unit =

        // Empty
        let emptySet : LongSet<byte> = LongSet.empty
        let emptySeq = LongSet.toSeq emptySet
        Assert.IsTrue (Seq.length emptySeq = 0)
        
        // Single
        let single = LongSet.singleton "stuff"
        let singleSeq = LongSet.toSeq single
        Assert.IsTrue(Seq.toList singleSeq = [ "stuff" ])
        
        // Multi
        let multi = new LongSet([5L; 2; 3L; 1; 4])
        let multiSeq = LongSet.toSeq multi
        Assert.IsTrue(Seq.toList multiSeq = [ 1; 2; 3L; 4; 5 ])
        
    [<Test>]
    let minElement () : unit =
        // Check for an argument exception "Set contains no members"
        checkThrowsArgumentException(fun () -> LongSet.minElement LongSet.empty |> ignore)
        
        let set1 = LongSet.ofList [10; 8; 100; 1; 50]
        Assert.AreEqual(LongSet.minElement set1, 1)
        
        let set2 = LongSet.ofList ["abcd"; "a"; "abc"; "ab"]
        Assert.AreEqual(LongSet.minElement set2, "a")
        
    [<Test>]
    let maxElement () : unit =
        // Check for an argument exception "Set contains no members"
        checkThrowsArgumentException(fun () -> LongSet.maxElement LongSet.empty |> ignore)
        
        let set1 = LongSet.ofList [10; 8; 100; 1; 50]
        Assert.AreEqual(LongSet.maxElement set1, 100)
        
        let set2 = LongSet.ofList ["abcd"; "a"; "abc"; "ab"]
        Assert.AreEqual(LongSet.maxElement set2, "abcd")


    [<Test>]
    let isProperSubset () : unit =
        let set1 = LongSet.ofList [10; 8; 100]
        let set2 = LongSet.ofList [100]
        Assert.IsTrue(LongSet.isProperSubset set2 set1)
        Assert.IsTrue(LongSet.isProperSubset LongSet.empty set2)
        Assert.IsFalse(LongSet.isProperSubset LongSet.empty LongSet.empty)
        Assert.IsFalse(LongSet.isProperSubset set1 set2)

    [<Test>]
    let isProperSuperset () : unit =
        let set1 = LongSet.ofList [10; 8; 100]
        let set2 = LongSet.ofList [100; 8]
        Assert.IsTrue(LongSet.isProperSuperset set1 set2)
        Assert.IsTrue(LongSet.isProperSuperset set2 LongSet.empty)
        Assert.IsFalse(LongSet.isProperSuperset LongSet.empty LongSet.empty)
        Assert.IsFalse(LongSet.isProperSuperset set1 set1)
        Assert.IsFalse(LongSet.isProperSuperset set2 set1)
        
    // ----- Not associated with a module function -----

    [<Test>]
    let ``General Test #1`` () : unit =
        // Retruns a random permutation of integers between the two bounds.
        let randomPermutation lowerBound upperBound = 
            let items = ResizeArray<_>([lowerBound .. upperBound])
            let rng = new Random()
            
            let randomPermutation = ResizeArray<int64>()
            while items.Count > 0 do
                let idx = rng.Next() % items.Count
                let i = items.[idx]
                items.RemoveAt(idx)
                randomPermutation.Add(i)
            
            randomPermutation.ToArray()
        
        for i in 0..50 do
            let permutation = randomPermutation 0 i
            
            let set : LongSet ref = ref LongSet.empty
            // Add permutation items to set in order
            Array.iter (fun i -> set := LongSet.add i !set) permutation
            // Check that the set equals the full list
            Assert.IsTrue(LongSet.toList !set = [0 .. i])
            // Remove items in permutation order, ensuring set is delt with correctly
            Array.iteri
                (fun idx i -> set := LongSet.remove i !set
                              // Verify all elements have been correctly removed
                              let removedElements = Array.sub permutation 0 (idx + 1) |> LongSet.ofSeq
                              let inter : LongSet = LongSet.intersect !set removedElements
                              Assert.IsTrue(inter.Count = 0))
                permutation
        ()
*)


open FsCheck

(* TODO : Implement FsCheck tests. *)

