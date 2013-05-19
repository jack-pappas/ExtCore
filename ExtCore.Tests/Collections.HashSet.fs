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

/// Unit tests for the ExtCore.Collections.HashSet type and module.
module Tests.ExtCore.Collections.HashSet

open System
open System.Collections
open System.Collections.Generic
open NUnit.Framework
open FsUnit


[<Test>]
let isEmpty () : unit =
    HashSet.empty
    |> HashSet.isEmpty
    |> should be True
    
    HashSet.singleton 5
    |> HashSet.isEmpty
    |> should be False

[<Test>]
let count () : unit =
    HashSet.empty
    |> HashSet.count
    |> assertEqual 0

    HashSet.singleton 4
    |> HashSet.count
    |> assertEqual 1

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.count
    |> assertEqual 8

[<Test>]
let singleton () : unit =
    HashSet.singleton 6
    |> assertEqual (
        HashSet.empty
        |> HashSet.add 6)

[<Test>]
let contains () : unit =
    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.contains 11
    |> should be True

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.contains 6
    |> should be False
(*
[<Test>]
let minElement () : unit =
    // Simple test case.
    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.minElement
    |> assertEqual 2

    // Test case for minElement (unsigned) when a negative value is in the set.
    [| 5; 3; 11; 2; 17; 4; 12; 14; -1 |]
    |> HashSet.ofArray
    |> HashSet.minElement
    |> assertEqual 2

    // Test case for minElement (unsigned) only negative values are in the set.
    [| -5; -3; -11; -2; -17; -4; -12; -14 |]
    |> HashSet.ofArray
    |> HashSet.minElement
    |> assertEqual -17

[<Test; ExpectedException(typeof<System.ArgumentException>)>]
let ``minElement raises exn for empty set`` () : unit =
    HashSet.minElement HashSet.empty |> ignore

[<Test>]
let maxElement () : unit =
    // Simple test case.
    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.maxElement
    |> assertEqual 17

    // Test case for maxElement (unsigned) when a negative value is in the set.
    [| 5; 3; 11; 2; 17; 4; 12; 14; -1 |]
    |> HashSet.ofArray
    |> HashSet.maxElement
    |> assertEqual -1

    // Test case for maxElement (unsigned) only negative values are in the set.
    [| -5; -3; -11; -2; -17; -4; -12; -14 |]
    |> HashSet.ofArray
    |> HashSet.maxElement
    |> assertEqual -2

[<Test; ExpectedException(typeof<System.ArgumentException>)>]
let ``maxElement raises exn for empty set`` () : unit =
    HashSet.maxElement HashSet.empty |> ignore
*)
[<Test>]
let add () : unit =
    HashSet.empty
    |> HashSet.add 5
    |> assertEqual (
        HashSet.singleton 5)

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.add 5
    |> assertEqual (HashSet.ofArray
        [| 5; 3; 11; 2; 17; 4; 12; 14 |])

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.add 8
    |> assertEqual (HashSet.ofArray
        [| 5; 3; 11; 2; 17; 4; 12; 14; 8 |])

[<Test>]
let remove () : unit =
    HashSet.singleton 6
    |> HashSet.remove 6
    |> assertEqual HashSet.empty

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.remove 5
    |> assertEqual (HashSet.ofArray
        [| 3; 11; 2; 17; 4; 12; 14 |])

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.remove 8
    |> assertEqual (HashSet.ofArray
        [| 5; 3; 11; 2; 17; 4; 12; 14 |])
(*
[<Test>]
let union () : unit =
    HashSet.union
        (HashSet.ofArray [| 3; 11; 2; 4; 12 |])
        (HashSet.ofArray [| 5; 11; 17; 4; 14 |])
    |> assertEqual
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])

[<Test>]
let unionMany () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let intersect () : unit =
    HashSet.intersect
        (HashSet.ofArray [| 5; 11; 17; 4; 14 |])
        HashSet.empty
    |> assertEqual HashSet.empty

    HashSet.intersect
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (HashSet.ofArray [| 5; 11; 17; 4; 14 |])
    |> assertEqual
        (HashSet.ofArray [| 5; 11; 17; 4; 14 |])

[<Test>]
let intersectMany () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let difference () : unit =
    HashSet.difference
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        HashSet.empty
    |> assertEqual
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])

    HashSet.difference
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (HashSet.ofArray [| 5; 11; 17; 4; 14 |])
    |> assertEqual
        (HashSet.ofArray [| 3; 2; 12 |])
*)
(*
[<Test>]
let isSubset () : unit =
    // The empty set is always a subset of any other set.
    HashSet.isSubset
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        HashSet.empty
    |> should be True

    HashSet.isSubset
        HashSet.empty HashSet.empty
    |> should be True

    // A set is a subset of itself (this distinguishes isSubset from isProperSubset).
    HashSet.isSubset
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be True

    // Basic tests.
    HashSet.isSubset
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 12; 14 |])
    |> should be True

    HashSet.isSubset
        (HashSet.ofArray [| 5; 3; 11; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be False

    // Partially-overlapping sets.
    HashSet.isSubset
        (HashSet.ofArray [| 5; 3; 11; 12; 14; 22; 42; 25; |])
        (HashSet.ofArray [| 5; 3; 11; 12; 14; 32; 57; 53; |])
    |> should be False

    // Disjoint sets.
    HashSet.isSubset
        (HashSet.ofArray [| 1..5 |])
        (HashSet.ofArray [| 6..10 |])
    |> should be False

[<Test>]
let isProperSubset () : unit =
    // The empty set is a proper subset of any set except itself.
    HashSet.isProperSubset
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        HashSet.empty
    |> should be True

    HashSet.isProperSubset
        HashSet.empty HashSet.empty
    |> should be False

    // A set is a subset of itself (this distinguishes isSubset from isProperSubset).
    HashSet.isProperSubset
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be False

    // Basic tests.
    HashSet.isProperSubset
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 12; 14 |])
    |> should be True

    HashSet.isProperSubset
        (HashSet.ofArray [| 5; 3; 11; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be False

    // Partially-overlapping sets.
    HashSet.isProperSubset
        (HashSet.ofArray [| 5; 3; 11; 12; 14; 22; 42; 25; |])
        (HashSet.ofArray [| 5; 3; 11; 12; 14; 32; 57; 53; |])
    |> should be False

    // Disjoint sets.
    HashSet.isProperSubset
        (HashSet.ofArray [| 1..5 |])
        (HashSet.ofArray [| 6..10 |])
    |> should be False

[<Test>]
let isSuperset () : unit =
    // The empty set is never a superset of any other set except itself.
    HashSet.isSuperset
        HashSet.empty
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be False

    HashSet.isSuperset
        HashSet.empty HashSet.empty
    |> should be True

    // A set is a superset of itself (this distinguishes isSuperset from isProperSuperset).
    HashSet.isSuperset
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be True

    // Basic tests.
    HashSet.isSuperset
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 12; 14 |])
    |> should be False

    HashSet.isSuperset
        (HashSet.ofArray [| 5; 3; 11; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be True

    // Partially-overlapping sets.
    HashSet.isSuperset
        (HashSet.ofArray [| 5; 3; 11; 12; 14; 22; 42; 25; |])
        (HashSet.ofArray [| 5; 3; 11; 12; 14; 32; 57; 53; |])
    |> should be False

    // Disjoint sets.
    HashSet.isSuperset
        (HashSet.ofArray [| 1..5 |])
        (HashSet.ofArray [| 6..10 |])
    |> should be False

[<Test>]
let isProperSuperset () : unit =
    // The empty set is never a proper superset of any set.
    HashSet.isProperSuperset
        HashSet.empty
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be False

    HashSet.isProperSuperset
        HashSet.empty HashSet.empty
    |> should be False

    // A set is a superset of itself (this distinguishes isSuperset from isProperSuperset).
    HashSet.isProperSuperset
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be False

    // Basic tests.
    HashSet.isProperSuperset
        (HashSet.ofArray [| 5; 3; 11; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> should be False

    HashSet.isProperSuperset
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 12; 14 |])
    |> should be True

    // Partially-overlapping sets.
    HashSet.isProperSuperset
        (HashSet.ofArray [| 5; 3; 11; 12; 14; 22; 42; 25; |])
        (HashSet.ofArray [| 5; 3; 11; 12; 14; 32; 57; 53; |])
    |> should be False

    // Disjoint sets.
    HashSet.isProperSuperset
        (HashSet.ofArray [| 1..5 |])
        (HashSet.ofArray [| 6..10 |])
    |> should be False
*)
[<Test>]
let ofSeq () : unit =
    Seq.empty
    |> HashSet.ofSeq
    |> assertEqual HashSet.empty
    
    seq {
        yield! seq { 2 .. 5 }
        yield 11
        yield 12
        yield 14
        yield 17 }
    |> HashSet.ofSeq
    |> assertEqual (
        HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])

[<Test>]
let ofList () : unit =
    List.empty
    |> HashSet.ofList
    |> assertEqual HashSet.empty

    [5; 3; 11; 2; 17; 4; 12; 14]
    |> HashSet.ofList
    |> assertEqual (
        HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])

[<Test>]
let ofArray () : unit =
    Array.empty
    |> HashSet.ofArray
    |> assertEqual HashSet.empty

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> assertEqual (
        HashSet.empty
        |> HashSet.add 2
        |> HashSet.add 3
        |> HashSet.add 4
        |> HashSet.add 5
        |> HashSet.add 11
        |> HashSet.add 12
        |> HashSet.add 14
        |> HashSet.add 17)

[<Test>]
let ofSet () : unit =
    Set.empty
    |> HashSet.ofSet
    |> assertEqual HashSet.empty

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> Set.ofArray
    |> HashSet.ofSet
    |> assertEqual (
        HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])

[<Test>]
let toSeq () : unit =
    HashSet.empty
    |> HashSet.toSeq
    |> Seq.isEmpty
    |> should be True

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.toSeq
    |> Seq.toArray
    |> assertEqual
        [|2; 3; 4; 5; 11; 12; 14; 17|]

[<Test>]
let toList () : unit =
    HashSet.empty
    |> HashSet.toList
    |> assertEqual List.empty<int>

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.toList
    |> Collection.assertEquiv
        [2; 3; 4; 5; 11; 12; 14; 17]

[<Test>]
let toArray () : unit =
    HashSet.empty
    |> HashSet.toArray
    |> assertEqual Array.empty<int>

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.toArray
    |> Collection.assertEquiv
        [|2; 3; 4; 5; 11; 12; 14; 17|]

[<Test>]
let toSet () : unit =
    HashSet.empty
    |> HashSet.toSet
    |> assertEqual Set.empty<int>

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.toSet
    |> assertEqual
        (Set.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])

[<Test>]
let iter () : unit =
    let elements = ResizeArray ()

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.iter (fun el ->
        elements.Add (el + 2))

    elements
    |> ResizeArray.toArray
    |> Collection.assertEquiv
        [|4; 5; 6; 7; 13; 14; 16; 19|]

[<Test>]
let iterBack () : unit =
    let elements = ResizeArray ()

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.iterBack (fun el ->
        elements.Add (el + 2))

    elements
    |> ResizeArray.toArray
    |> Collection.assertEquiv
        [|19; 16; 14; 13; 7; 6; 5; 4|]

[<Test>]
let fold () : unit =
    do
        let elements = ResizeArray ()

        (0, HashSet.empty)
        ||> HashSet.fold (fun counter el ->
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
            |> HashSet.ofArray

        (0, testSet)
        ||> HashSet.fold (fun counter el ->
            elements.Add (el + 2)
            counter + 3)
        |> assertEqual (HashSet.count testSet * 3)

        elements
        |> ResizeArray.toArray
        |> Collection.assertEquiv
            [| 7; 5; 13; 4; 19; 6; 14; 16 |]

[<Test>]
let foldBack () : unit =
    do
        let elements = ResizeArray ()

        (HashSet.empty, 0)
        ||> HashSet.foldBack (fun el counter ->
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
            |> HashSet.ofArray

        (testSet, 0)
        ||> HashSet.foldBack (fun el sum ->
            elements.Add (el + 2)
            sum + el + 3)
        |> assertEqual 92

        elements
        |> ResizeArray.toArray
        |> Collection.assertEquiv
            [| 7; 5; 13; 4; 19; 6; 14; 16 |]

[<Test>]
let choose () : unit =
    HashSet.empty
    |> HashSet.choose (fun el ->
        if el % 2 = 0 then
            Some (el + 1)
        else None)
    |> assertEqual HashSet.empty

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.choose (fun el ->
        if el % 2 = 0 then
            Some (el + 1)
        else None)
    |> assertEqual
        (HashSet.ofArray [|3; 5; 13; 15|])

[<Test>]
let filter () : unit =
    HashSet.empty
    |> HashSet.filter (fun el ->
        el % 2 <> 0)
    |> HashSet.isEmpty
    |> should be True

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.filter (fun el ->
        el % 2 <> 0)
    |> assertEqual
        (HashSet.ofArray [|5; 3; 11; 17|])

[<Test>]
let map () : unit =
    HashSet.empty
    |> HashSet.map (fun el ->
        el * 2)
    |> HashSet.isEmpty
    |> should be True

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.map (fun el ->
        el * 2)
    |> assertEqual (
        [| 5; 3; 11; 2; 17; 4; 12; 14 |]
        |> Set.ofArray
        |> ExtCore.Collections.Set.mapToArray (fun el ->
            el * 2)
        |> HashSet.ofArray)

[<Test>]
let partition () : unit =
    do
        let evens, odds =
            HashSet.empty
            |> HashSet.partition (fun el ->
                el % 2 = 0)

        evens
        |> HashSet.isEmpty
        |> should be True

        odds
        |> HashSet.isEmpty
        |> should be True

    do
        let evens, odds =
            [| 5; 3; 11; 2; 17; 4; 12; 14 |]
            |> HashSet.ofArray
            |> HashSet.partition (fun el ->
                el % 2 = 0)

        evens
        |> assertEqual
            (HashSet.ofArray [|2; 4; 12; 14|])

        odds
        |> assertEqual
            (HashSet.ofArray [|5; 3; 11; 17|])

[<Test>]
let exists () : unit =
    HashSet.empty
    |> HashSet.exists (fun el ->
        el % 7 = 0)
    |> should be False

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.exists (fun el ->
        el = 6)
    |> should be False

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.exists (fun el ->
        el % 7 = 0)
    |> should be True

[<Test>]
let forall () : unit =
    HashSet.empty
    |> HashSet.forall (fun el ->
        el % 7 = 0)
    |> should be True

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.forall (fun el ->
        el < 100)
    |> should be True

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.forall (fun el ->
        el % 2 = 0)
    |> should be False

[<Test>]
let tryPick () : unit =
    // Test against an empty set.
    HashSet.empty
    |> HashSet.tryPick (fun el ->
        if el % 7 = 0 then
            Some (el + 2)
        else None)
    |> assertEqual (None : int option)

    // Test for case where the set does not contain a matching element.
    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.tryPick (fun el ->
        if el > 30 then
            Some (el - 10)
        else None)
    |> assertEqual (None : int option)

    // Test for case where the set contains a single matching element.
    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.tryPick (fun el ->
        if el % 7 = 0 then
            Some (el - 2)
        else None)
    |> assertEqual (Some 12)

    // Test for case where the set contains multiple matching elements.
    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.tryPick (fun el ->
        if el % 3 = 2 then
            Some (el + 1)
        else None)
    |> assertEqual (Some 3)

[<Test>]
let pick () : unit =
    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.pick (fun el ->
        if el % 3 = 2 then
            Some (el + 1)
        else None)
    |> assertEqual 3

[<Test; ExpectedException(typeof<KeyNotFoundException>)>]
let ``pick raises exn on empty input`` () : unit =
    HashSet.empty
    |> HashSet.pick (fun el ->
        if el % 3 = 2 then
            Some (el + 1)
        else None)
    |> ignore


open FsCheck

(* TODO : Implement FsCheck tests. *)

