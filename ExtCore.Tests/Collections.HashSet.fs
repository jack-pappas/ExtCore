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
open ExtCore.Collections    // Necessary because of naming conflict with System.Collections.Generic.HashSet.


[<Test>]
let isEmpty () : unit =
    HashSet.empty
    |> HashSet.isEmpty
    |> assertTrue
    
    HashSet.singleton 5
    |> HashSet.isEmpty
    |> assertFalse

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
    |> assertTrue

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.contains 6
    |> assertFalse
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

[<Test>]
let union () : unit =
    HashSet.union
        (HashSet.ofArray [| 3; 11; 2; 4; 12 |])
        (HashSet.ofArray [| 5; 11; 17; 4; 14 |])
    |> HashSet.toArray
    |> Collection.assertEquiv
        [| 5; 3; 11; 2; 17; 4; 12; 14 |]

[<Test>]
let unionMany () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let intersect () : unit =
    HashSet.intersect
        (HashSet.ofArray [| 5; 11; 17; 4; 14 |])
        HashSet.empty
    |> HashSet.isEmpty
    |> assertTrue

    HashSet.intersect
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (HashSet.ofArray [| 5; 11; 17; 4; 14 |])
    |> HashSet.toArray
    |> Collection.assertEquiv
        [| 5; 11; 17; 4; 14 |]

[<Test>]
let intersectMany () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let difference () : unit =
    HashSet.difference
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        HashSet.empty
    |> HashSet.toArray
    |> Collection.assertEquiv
        [| 5; 3; 11; 2; 17; 4; 12; 14 |]

    HashSet.difference
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (HashSet.ofArray [| 5; 11; 17; 4; 14 |])
    |> HashSet.toArray
    |> Collection.assertEquiv
        [| 3; 2; 12 |]

[<Test>]
let isSubset () : unit =
    // The empty set is always a subset of any other set.
    HashSet.isSubset
        HashSet.empty
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> assertTrue

    HashSet.isSubset
        HashSet.empty HashSet.empty
    |> assertTrue

    // A set is a subset of itself (this distinguishes isSubset from isProperSubset).
    HashSet.isSubset
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> assertTrue

    // Basic tests.
    HashSet.isSubset
        (HashSet.ofArray [| 5; 3; 11; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> assertTrue

    HashSet.isSubset
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 12; 14 |])
    |> assertFalse

    // Partially-overlapping sets.
    HashSet.isSubset
        (HashSet.ofArray [| 5; 3; 11; 12; 14; 22; 42; 25; |])
        (HashSet.ofArray [| 5; 3; 11; 12; 14; 32; 57; 53; |])
    |> assertFalse

    // Disjoint sets.
    HashSet.isSubset
        (HashSet.ofArray [| 1..5 |])
        (HashSet.ofArray [| 6..10 |])
    |> assertFalse

[<Test>]
let isProperSubset () : unit =
    // The empty set is a proper subset of any set except itself.
    HashSet.isProperSubset
        HashSet.empty
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> assertTrue

    HashSet.isProperSubset
        HashSet.empty HashSet.empty
    |> assertFalse

    // A set is not a proper subset of itself (this distinguishes isSubset from isProperSubset).
    HashSet.isProperSubset
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> assertFalse

    // Basic tests.
    HashSet.isProperSubset
        (HashSet.ofArray [| 5; 3; 11; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> assertTrue

    HashSet.isProperSubset
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 12; 14 |])
    |> assertFalse

    // Partially-overlapping sets.
    HashSet.isProperSubset
        (HashSet.ofArray [| 5; 3; 11; 12; 14; 22; 42; 25; |])
        (HashSet.ofArray [| 5; 3; 11; 12; 14; 32; 57; 53; |])
    |> assertFalse

    // Disjoint sets.
    HashSet.isProperSubset
        (HashSet.ofArray [| 1..5 |])
        (HashSet.ofArray [| 6..10 |])
    |> assertFalse

[<Test>]
let isSuperset () : unit =
    // The empty set is never a superset of any other set except itself.
    HashSet.isSuperset
        HashSet.empty
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> assertFalse

    HashSet.isSuperset
        HashSet.empty HashSet.empty
    |> assertTrue

    // A set is a superset of itself (this distinguishes isSuperset from isProperSuperset).
    HashSet.isSuperset
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> assertTrue

    // Basic tests.
    HashSet.isSuperset
        (HashSet.ofArray [| 5; 3; 11; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> assertFalse

    HashSet.isSuperset
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 12; 14 |])
    |> assertTrue

    // Partially-overlapping sets.
    HashSet.isSuperset
        (HashSet.ofArray [| 5; 3; 11; 12; 14; 22; 42; 25; |])
        (HashSet.ofArray [| 5; 3; 11; 12; 14; 32; 57; 53; |])
    |> assertFalse

    // Disjoint sets.
    HashSet.isSuperset
        (HashSet.ofArray [| 1..5 |])
        (HashSet.ofArray [| 6..10 |])
    |> assertFalse

[<Test>]
let isProperSuperset () : unit =
    // The empty set is never a proper superset of any set.
    HashSet.isProperSuperset
        HashSet.empty
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> assertFalse

    HashSet.isProperSuperset
        HashSet.empty HashSet.empty
    |> assertFalse

    // A set is a superset of itself (this distinguishes isSuperset from isProperSuperset).
    HashSet.isProperSuperset
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> assertFalse

    // Basic tests.
    HashSet.isProperSuperset
        (HashSet.ofArray [| 5; 3; 11; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
    |> assertFalse

    HashSet.isProperSuperset
        (HashSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])
        (HashSet.ofArray [| 5; 3; 11; 12; 14 |])
    |> assertTrue

    // Partially-overlapping sets.
    HashSet.isProperSuperset
        (HashSet.ofArray [| 5; 3; 11; 12; 14; 22; 42; 25; |])
        (HashSet.ofArray [| 5; 3; 11; 12; 14; 32; 57; 53; |])
    |> assertFalse

    // Disjoint sets.
    HashSet.isProperSuperset
        (HashSet.ofArray [| 1..5 |])
        (HashSet.ofArray [| 6..10 |])
    |> assertFalse

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
    |> assertTrue

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
        |> assertTrue

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
        |> assertTrue

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
    |> assertTrue

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
    |> assertTrue

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
        |> assertTrue

        odds
        |> HashSet.isEmpty
        |> assertTrue

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
    |> assertFalse

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.exists (fun el ->
        el = 6)
    |> assertFalse

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.exists (fun el ->
        el % 7 = 0)
    |> assertTrue

[<Test>]
let forall () : unit =
    HashSet.empty
    |> HashSet.forall (fun el ->
        el % 7 = 0)
    |> assertTrue

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.forall (fun el ->
        el < 100)
    |> assertTrue

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> HashSet.ofArray
    |> HashSet.forall (fun el ->
        el % 2 = 0)
    |> assertFalse

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
    [<Ignore("The failure of this test may to be due to a bug in the compiler-generated seq implementation. \
              This test is ignored for now until that is fixed or the workaround (implementing a custom enumerator) is implemented.")>]
    let ``IEnumerable (Legit)`` () : unit =
        // Legit IE
        let ie = (new HashSet<char>(['a'; 'b'; 'c'])) :> IEnumerable
        //let alphabet = new HashSet<char>([| 'a' .. 'z' |])
        let enum = ie.GetEnumerator()
        
        let testStepping () : unit =
            checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, 'a')
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, 'b')
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, 'c')
            Assert.AreEqual(enum.MoveNext(), false)
            checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
    
        testStepping()
        enum.Reset()
        testStepping()

    [<Test>]
    [<Ignore("The failure of this test may to be due to a bug in the compiler-generated seq implementation. \
              This test is ignored for now until that is fixed or the workaround (implementing a custom enumerator) is implemented.")>]
    let ``IEnumerable (Empty)`` () : unit =
        // Empty IE
        let ie = (new HashSet<char>([])) :> IEnumerable  // Note no type args
        let enum = ie.GetEnumerator()
        
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        Assert.AreEqual(enum.MoveNext(), false)
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)

    [<Test>]
    [<Ignore("The failure of this test may to be due to a bug in the compiler-generated seq implementation. \
              This test is ignored for now until that is fixed or the workaround (implementing a custom enumerator) is implemented.")>]
    let ``IEnumerable<T> (Legit)`` () : unit =
        // Legit IE
        let ie =(new HashSet<char>(['a'; 'b'; 'c'])) :> IEnumerable<char>
        let enum = ie.GetEnumerator()
        
        let testStepping () : unit =
            checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, 'a')
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, 'b')
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, 'c')
            Assert.AreEqual(enum.MoveNext(), false)
            checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        
        testStepping()
        enum.Reset()
        testStepping()
    
    [<Test>]
    [<Ignore("The failure of this test may to be due to a bug in the compiler-generated seq implementation. \
              This test is ignored for now until that is fixed or the workaround (implementing a custom enumerator) is implemented.")>]
    let ``IEnumerable<T> (Empty)`` () : unit =
        // Empty IE
        let ie = (new HashSet<int>([])) :> IEnumerable<int>  
        let enum = ie.GetEnumerator()
        
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        Assert.AreEqual(enum.MoveNext(), false)
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        
    [<Test>]
    let ``ICollection (Legit)`` () : unit =
        // Legit IC        
        let ic = (new HashSet<int>([1;2;3;4])) :> ICollection<int>
        let st = new HashSet<int>([1;2;3;4])        
        
        Assert.IsTrue(ic.Contains(3)) 
        let newArr = Array.create 5 0
        ic.CopyTo(newArr,0) 
        Assert.IsTrue(ic.IsReadOnly)

    [<Test>]
    let ``ICollection (Empty)`` () : unit =
        // Empty IC
        let ic = (new HashSet<string>([])) :> ICollection<string>
        Assert.IsFalse(ic.Contains("A") )     
        let newArr = Array.create 5 "a"
        ic.CopyTo(newArr,0) 
    
    [<Test>]
    let ``IComparable (Legit)`` () : unit =
        // Legit IC
        let ic = (new HashSet<int>([1;2;3;4])) :> IComparable    
        Assert.AreEqual(ic.CompareTo(new HashSet<int>([1;2;3;4])),0) 
    
    [<Test>]
    let ``IComparable (Empty)`` () : unit =    
        // Empty IC
        let ic = (new HashSet<string>([])) :> IComparable   
        Assert.AreEqual(ic.CompareTo(HashSet.empty<string>),0)
        
        
    // Base class methods
    [<Test>]
    let ObjectGetHashCode () : unit =
        // Verify order added is independent
        let x = HashSet.ofList [1; 2; 3]
        let y = HashSet.ofList [3; 2; 1]
        Assert.AreEqual(x.GetHashCode(), y.GetHashCode())
    
    [<Test>]
    let ObjectToString () : unit =
        Assert.AreEqual("hashSet [1; 2; 3; ... ]", (new HashSet<int>([1;2;3;4])).ToString())
        Assert.AreEqual("hashSet []", (HashSet.empty).ToString())
        Assert.AreEqual("hashSet [1; 3]", (new HashSet<decimal>([1M;3M])).ToString())
        
    
    [<Test>]
    let ``ObjectEquals (Basic)`` () : unit =
        // All three are different references, but equality has been
        // provided by the F# compiler.
        let a = new HashSet<int>([1;2;3])
        let b = new HashSet<int>([1..3])
        let c = new HashSet<int>(seq{1..3})
        Assert.IsTrue( (a = b) )
        Assert.IsTrue( (b = c) )
        Assert.IsTrue( (c = a) )
        Assert.IsTrue( a.Equals(b) ); Assert.IsTrue( b.Equals(a) )
        Assert.IsTrue( b.Equals(c) ); Assert.IsTrue( c.Equals(b) )
        Assert.IsTrue( c.Equals(a) ); Assert.IsTrue( a.Equals(c) )

    [<Test>]
    let ``ObjectEquals (Equality between types)`` () : unit =
        // Equality between types
        let a = HashSet.empty<int>
        let b = HashSet.empty<string>
        Assert.IsFalse( b.Equals(a) )
        Assert.IsFalse( a.Equals(b) )
        
    [<Test>]
    let ``ObjectEquals (co- and contra-variance)`` () : unit =
        // Co/contra variance not supported
        let a = HashSet.empty<string>
        let b = HashSet.empty
        Assert.IsFalse(a.Equals(b))
        Assert.IsFalse(b.Equals(a))
        
    [<Test>]
    let ``ObjectEquals (Self-equality)`` () : unit =
        // Self equality
        let a = new HashSet<int>([1])
        Assert.IsTrue( (a = a) )
        Assert.IsTrue(a.Equals(a))
        
    [<Test>]
    let ``ObjectEquals (Null)`` () : unit =
        // Null
        let a = new HashSet<int>([1])
        Assert.IsFalse(a.Equals(null))  
        
        
    // Instance methods
    [<Test>]
    let Add () : unit =    
        let l = new HashSet<int>([1 .. 10])
        let ad = l.Add 88
        Assert.IsTrue(ad.Contains(88))
    
        let e : HashSet<string> = HashSet.empty<string>
        let ade = e.Add "A"
        Assert.IsTrue(ade.Contains("A"))
        
        let s = HashSet.singleton 168
        let ads = s.Add 100
        Assert.IsTrue(ads.Contains(100))
        
    [<Test>]
    let Contains () : unit =    
        let i = new HashSet<int>([1 .. 10])
        Assert.IsTrue(i.Contains(8))
    
        let e : HashSet<string> = HashSet.empty<string>
        Assert.IsFalse(e.Contains("A"))
        
        let s = HashSet.singleton 168
        Assert.IsTrue(s.Contains(168))
    
    [<Test>]
    let Count () : unit =    
        let l = new HashSet<int>([1 .. 10])
        Assert.AreEqual(l.Count, 10)
    
        let e : HashSet<string> = HashSet.empty<string>
        Assert.AreEqual(e.Count, 0)
        
        let s = HashSet.singleton 'a'
        Assert.AreEqual(s.Count, 1)        
        
    [<Test>]
    let IsEmpty () : unit =
        let i = new HashSet<int>([1 .. 10])
        Assert.IsFalse(i.IsEmpty)
    
        let e : HashSet<string> = HashSet.empty<string>
        Assert.IsTrue(e.IsEmpty)
        
        let s = HashSet.singleton 168
        Assert.IsFalse(s.IsEmpty)   
        
    [<Test>]
    let IsSubsetOf () : unit =
        let fir = new HashSet<int>([1 .. 20])
        let sec = new HashSet<int>([1 .. 10])
        Assert.IsTrue(sec.IsSubsetOf(fir))
        Assert.IsTrue(HashSet.isSubset sec fir)
    
        let e : HashSet<int> = HashSet.empty<int>
        Assert.IsTrue(e.IsSubsetOf(fir))
        Assert.IsTrue(HashSet.isSubset e fir)
        
        let s = HashSet.singleton 8
        Assert.IsTrue(s.IsSubsetOf(fir)) 
        Assert.IsTrue(HashSet.isSubset s fir)
        
        let s100 = set [0..100]
        let s101 = set [0..101]
        for i = 0 to 100 do 
            Assert.IsFalse( (set [-1..i]).IsSubsetOf s100)
            Assert.IsTrue( (set [0..i]).IsSubsetOf s100)
            Assert.IsTrue( (set [0..i]).IsProperSubsetOf s101)
           
        
    [<Test>]
    let IsSupersetOf () : unit =
        let fir = new HashSet<int>([1 .. 10])
        let sec = new HashSet<int>([1 .. 20])
        Assert.IsTrue(sec.IsSupersetOf(fir))
        Assert.IsTrue(HashSet.isSuperset sec fir)
    
        let e : HashSet<int> = HashSet.empty<int>
        Assert.IsFalse(e.IsSupersetOf(fir))
        Assert.IsFalse(HashSet.isSuperset e fir)
        
        let s = HashSet.singleton 168
        Assert.IsFalse(s.IsSupersetOf(fir))  
        Assert.IsFalse(HashSet.isSuperset s fir)

        let s100 = set [0..100]
        let s101 = set [0..101]
        for i = 0 to 100 do 
            Assert.IsFalse( s100.IsSupersetOf (set [-1..i]))
            Assert.IsTrue( s100.IsSupersetOf (set [0..i]))
            Assert.IsTrue( s101.IsSupersetOf (set [0..i]))
        
    [<Test>]
    let Remove () : unit =    
        let i = new HashSet<int>([1;2;3;4])
        Assert.AreEqual(i.Remove 3,(new HashSet<int>([1;2;4])))
    
        let e : HashSet<string> = HashSet.empty<string>
        Assert.AreEqual(e.Remove "A", e)
        
        let s = HashSet.singleton 168
        Assert.AreEqual(s.Remove 168, HashSet.empty<int>)
        
        
    // Static methods
    [<Test>]
    let Addition () : unit =
        let fir = new HashSet<int>([1;3;5])
        let sec = new HashSet<int>([2;4;6])
        Assert.AreEqual(fir + sec, new HashSet<int>([1;2;3;4;5;6]))
        Assert.AreEqual(HashSet.op_Addition(fir,sec), new HashSet<int>([1;2;3;4;5;6]))
    
        let e : HashSet<int> = HashSet.empty<int>
        Assert.AreEqual(e + e, e)
        Assert.AreEqual(HashSet.op_Addition(e,e),e)
        
        let s1 = HashSet.singleton 8
        let s2 = HashSet.singleton 6
        Assert.AreEqual(s1 + s2, new HashSet<int>([8;6]))
        Assert.AreEqual(HashSet.op_Addition(s1,s2), new HashSet<int>([8;6]))
        

    [<Test>]
    let Subtraction () : unit =
        let fir = new HashSet<int>([1..6])
        let sec = new HashSet<int>([2;4;6])
        Assert.AreEqual(fir - sec, new HashSet<int>([1;3;5]))
        Assert.AreEqual(HashSet.difference fir sec, new HashSet<int>([1;3;5]))
        Assert.AreEqual(HashSet.op_Subtraction(fir,sec), new HashSet<int>([1;3;5]))
    
        let e : HashSet<int> = HashSet.empty<int>
        Assert.AreEqual(e - e, e)
        Assert.AreEqual(HashSet.difference e e, e)
        Assert.AreEqual(HashSet.op_Subtraction(e,e),e)
        
        let s1 = HashSet.singleton 8
        let s2 = HashSet.singleton 6
        Assert.AreEqual(s1 - s2, new HashSet<int>([8]))
        Assert.AreEqual(HashSet.difference s1 s2, new HashSet<int>([8]))
        Assert.AreEqual(HashSet.op_Subtraction(s1,s2), new HashSet<int>([8]))


(*
[Test Strategy]
Make sure each method works on:
* Empty set
* Single-element set
* Sets with 4 more more elements
*)

module SetModule =
    [<Test>]
    let empty () : unit =
        let emptySet = HashSet.empty
        if HashSet.count emptySet <> 0 then Assert.Fail()    
        
        let c : HashSet<int>    = HashSet.empty
        let d : HashSet<string> = HashSet.empty
        ()

    [<Test>]
    let singleton () : unit =
        let intSingleton = HashSet.singleton 5
        Assert.IsTrue(intSingleton.Count = 1)
        Assert.IsTrue(intSingleton.Contains(5))
                
        let stringSingleton = HashSet.singleton (null)
        Assert.IsFalse(stringSingleton.Contains(""))
        
    [<Test>]
    let add () : unit =
        let empty = HashSet.empty
        let x     = HashSet.add 'x' empty
        let xy    = HashSet.add 'y' x
        let xyz   = HashSet.add 'z' xy
        let wxyz  = HashSet.add 'w' xyz
        
        Assert.IsTrue(HashSet.count xy   = 2)
        Assert.IsTrue(HashSet.count xyz  = 3)
        Assert.IsTrue(HashSet.count wxyz = 4)
        
    [<Test>]
    let contains () : unit =
        // Empty set searching for null = false
        if HashSet.contains null (HashSet.empty) <> false then Assert.Fail()

        // Single element set (of tuple) = true
        let digits = new HashSet<string * int>([("one", 1)])
        if HashSet.contains ("one", 1) digits <> true then Assert.Fail()

        let odds = new HashSet<int>([1 .. 2 .. 11])
        if HashSet.contains 6 odds <> false then Assert.Fail()
        ()
        
    [<Test>]
    let count () : unit = 
        let empty = HashSet.empty
        if HashSet.count empty <> 0 then Assert.Fail()
        
        let one = HashSet.add 1 empty
        if HashSet.count one <> 1 then Assert.Fail()
        
        let multi = new HashSet<char>([| 'a' .. 'z' |])
        if HashSet.count multi <> 26 then Assert.Fail()
        ()
        
    [<Test>]
    let diff () : unit = 
        // Given a large set and removing 0, 1, x elements...
        let alphabet = new HashSet<char>([| 'a' .. 'z' |])
        let emptyChar = HashSet.empty : HashSet<char>
        
        let removeEmpty = alphabet - emptyChar
        assertEqual alphabet removeEmpty
        
        let number = HashSet.singleton '1'
        let removeNumber = alphabet - number
        assertEqual alphabet removeNumber
        
        let vowels = new HashSet<char>([| 'a'; 'e'; 'i'; 'o'; 'u' |])
        let noVowels = alphabet - vowels
        assertEqual 21 noVowels.Count
        
        // Give a set of 0, 1, x elements remove some other set
        let odds  = new HashSet<int>([1 .. 2 .. 10])
        let evens = new HashSet<int>([2 .. 2 .. 10])
        
        let emptyNum = HashSet.empty : HashSet<int>
        let removeOddsFromEmpty = emptyNum - odds
        assertEqual emptyNum removeOddsFromEmpty
        
        let one = HashSet.singleton 1
        let removeOddsFrom1 = one - odds
        assertEqual emptyNum removeOddsFrom1
        
        let evensSansOdds = evens - odds
        assertEqual evens evensSansOdds

    [<Test>]
    let equal () : unit =
        let emptySet1 : HashSet<string> = HashSet.empty
        let emptySet2 : HashSet<string> = HashSet.empty
        if (emptySet1 = emptySet2) <> true then Assert.Fail()
        
        let a  = new HashSet<int>([1; 2; 3; 4; 5])
        let b = new HashSet<int>([1; 3; 5])
        
        if (a = b) <> false then Assert.Fail()
        
        let a = a |> HashSet.remove 2 |> HashSet.remove 4
        if (a = b) <> true then Assert.Fail()
        ()
        
    [<Test>]
    let ``compare (empty sets)`` () : unit =
        // Comparing empty sets
        let emptyString1 = HashSet.empty : HashSet<string>
        let emptyString2 = HashSet.empty : HashSet<string>
        
        if compare emptyString1 emptyString1 <> 0 then Assert.Fail()
        if compare emptyString1 emptyString2 <> 0 then Assert.Fail()

    [<Test>]
    let ``compare (single-element sets)`` () : unit =
        // Comparing single-element sets
        let one = HashSet.singleton 1
        let two = HashSet.singleton 2
        if compare one two <> -1 then Assert.Fail()
        if compare one one <> 0  then Assert.Fail()
        if compare two two <> 0  then Assert.Fail()
        if compare two one <> 1  then Assert.Fail()

    [<Test>]
    let ``compare (multi-element sets)`` () : unit =
        // Comparing multi-element sets
        let alphabet = new HashSet<char>(['a' .. 'z'])
        let vowels   = new HashSet<char>(['a'; 'e'; 'i'; 'o'; 'u'])
        
        let noVowelAlpa = alphabet - vowels
        if compare noVowelAlpa alphabet     <> 1  then Assert.Fail()
        if compare alphabet alphabet        <> 0  then Assert.Fail()
        if compare noVowelAlpa noVowelAlpa  <> 0  then Assert.Fail()
        if compare alphabet noVowelAlpa     <> -1 then Assert.Fail()
        ()

    [<Test>]
    let exists () : unit =
        
        let emptyInt = HashSet.empty : HashSet<int>
        if HashSet.exists (fun _ -> true) emptyInt <> false then Assert.Fail()
        
        let x = HashSet.singleton 'x'
        if HashSet.exists (fun c -> c = 'x') x  <> true  then Assert.Fail()
        if HashSet.exists (fun c -> c <> 'x') x <> false then Assert.Fail()
        
        let letNumPairs = new HashSet<string * int>([("one", 1); ("two", 2); ("three", 3)])
        if HashSet.exists (fun (text, num) -> text = "one" && num = 1) letNumPairs <> true then Assert.Fail()
        if HashSet.exists (fun (text, num) -> text = "four") letNumPairs           <> false then Assert.Fail()
        ()
        
    [<Test>]
    let filter () : unit =
        let emptyComplex = HashSet.empty : HashSet<int * (string * HashSet<decimal>) list * HashSet<int * string * (char * char * char)>>
        let fileredEmpty = HashSet.filter (fun _ -> false) emptyComplex 
        if (fileredEmpty = emptyComplex) <> true then Assert.Fail()
        
        let nullSet = HashSet.singleton null
        if nullSet.Count <> 1 then Assert.Fail()
        let filteredNull = HashSet.filter (fun x -> x <> null) nullSet
        if filteredNull.Count <> 0 then Assert.Fail()
        
        let digits = new HashSet<int>([1 .. 10])
        let evens  = new HashSet<int>([2 .. 2 .. 10])
        let filteredDigits = HashSet.filter(fun i -> i % 2 = 0) digits
        if (filteredDigits = evens) <> true then Assert.Fail()
        ()
        

    [<Test>]
    let map () : unit =
        let emptySet : HashSet<string> = HashSet.empty
        
        let result = HashSet.map (fun _ -> Assert.Fail(); "") emptySet
        if (emptySet = result) <> true then Assert.Fail()
        
        let alphabet = new HashSet<_>(['a' .. 'z'])
        let capped = HashSet.map (fun c -> Char.ToUpper(c)) alphabet
        
        if HashSet.exists (fun c -> c = Char.ToLower(c)) capped then Assert.Fail()
        ()

    [<Test>]
    let fold () : unit =
        
        let emptySet : HashSet<decimal> = HashSet.empty
        let result = HashSet.fold (fun _ _ -> Assert.Fail(); -1I) 0I emptySet
        assertEqual 0I result
        
        let input = new HashSet<_>([1; 2; 3; 4; 5])
        
        HashSet.fold (+) 0 input
        |> assertEqual 15
        
    [<Test>]
    let foldBack () : unit =
        
        let emptySet : HashSet<decimal> = HashSet.empty
        let result = HashSet.foldBack (fun _ _ -> Assert.Fail(); -1I) emptySet 0I
        assertEqual 0I result
        
        let input = new HashSet<_>([1; 2; 3; 4; 5])
        
        HashSet.foldBack (+) input 0
        |> assertEqual 15

    [<Test>]
    let forall () : unit =
        (HashSet.empty : HashSet<string>)
        |> HashSet.forall (fun _ -> Assert.Fail(); false)
        |> assertTrue
        
        let seta = new HashSet<_>( [1 .. 99] |> List.map (fun i -> i.ToString()) )
        let result = seta |> HashSet.forall (fun str -> str.Length < 3)
        Assert.IsTrue(result)

        let setb = new HashSet<_>( [50 .. 150] |> List.map (fun i -> i.ToString()) )
        let result = setb |> HashSet.forall (fun str -> str.Length < 3)
        Assert.IsFalse(result)
        ()

    [<Test>]
    let intersect () : unit =
        
        let emptySet1 : HashSet<int> = HashSet.empty
        let emptySet2 : HashSet<int> = HashSet.empty
        let four                = HashSet.singleton 4
       
        let emptyInterEmpty = HashSet.intersect emptySet1 emptySet2
        Assert.IsTrue( (emptyInterEmpty = emptySet1) )
        
        let xInterEmpty = HashSet.intersect four emptySet1
        Assert.IsFalse( (four = xInterEmpty) )
        
        let emptyInterX = HashSet.intersect emptySet1 four
        Assert.IsFalse( (four = emptyInterX) )
        ()
    
    [<Test>]
    let intersect2 () : unit =
        let a = new HashSet<int>([3; 4; 5; 6])
        let b = new HashSet<int>([5; 6; 7; 8])
        
        let intersection   = HashSet.intersect a b
        let expectedResult = new HashSet<int>([5; 6])
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
                    yield new HashSet<int>([i .. 7])
            ]
            
        let result : HashSet<_> = HashSet.intersectMany setsToIntersect
        Assert.IsTrue(result.Count = 2)
        
        let contains x s = s |> HashSet.exists (fun i -> i = x) 
        Assert.IsTrue(contains 6 result)
        Assert.IsTrue(contains 7 result)
                  
    [<Test>]
    let intersectMany2 () : unit =
        let all   = new HashSet<_>([1 .. 10])
        let odds  = new HashSet<_>([1 .. 2 .. 10])
        let evens = new HashSet<_>([2 .. 2 .. 10])
        
        let result = HashSet.intersectMany [odds; evens; all]
        Assert.IsTrue(HashSet.count result = 0)

    [<Test>]
    let intersectMany3 () : unit =
        let all   = new HashSet<_>([1 .. 10])
        let empty = HashSet.empty : HashSet<int>
        
        let result = HashSet.intersectMany [all; empty; all]
        Assert.IsTrue(HashSet.count result = 0)
        
        
    [<Test>]
    let intersectMany4 () : unit =
        checkThrowsArgumentException (fun () -> HashSet.intersectMany (Seq.empty : seq<HashSet<int>>) |> ignore)
        ()

    [<Test>]
    let union () : unit =
        let emptySet1 : HashSet<int> = HashSet.empty
        let emptySet2 : HashSet<int> = HashSet.empty
        let four                 = HashSet.singleton 4
       
        let emptyUnionEmpty = HashSet.union emptySet1 emptySet2
        Assert.IsTrue( (emptyUnionEmpty = emptySet1) )
        
        let xUnionEmpty = HashSet.union four emptySet1
        Assert.IsTrue( (four = xUnionEmpty) )
        
        let emptyUnionX = HashSet.union emptySet1 four
        Assert.IsTrue( (four = emptyUnionX) )
        ()
    
    [<Test>]
    let union2 () : unit =
        let a = new HashSet<int>([1; 2; 3; 4])
        let b = new HashSet<int>([5; 6; 7; 8])
        
        let union = HashSet.union a b
        let expectedResult = new HashSet<int>([1 .. 8])
        Assert.IsTrue( (union = expectedResult) )

    [<Test>]
    let union3 () : unit =
        let x : HashSet<_> = 
            HashSet.singleton 1
            |> HashSet.union (HashSet.singleton 1)
            |> HashSet.union (HashSet.singleton 1)
            |> HashSet.union (HashSet.singleton 1)
            
        Assert.IsTrue(x.Count = 1)
        
    [<Test>]
    let unionMany () : unit =
        let odds  = new HashSet<int>([1 .. 2 .. 10])
        let evens = new HashSet<int>([2 .. 2 .. 10])
        let empty = HashSet.empty : HashSet<int>
        let rest  = new HashSet<int>([11 .. 19])
        let zero  = HashSet.singleton 0
        
        let result : HashSet<_> = HashSet.unionMany [odds; evens; empty; rest; zero]
        Assert.IsTrue(result.Count = 20)

    [<Test>]
    let unionMany2 () : unit =
        let result : HashSet<_> = HashSet.unionMany (Seq.empty : seq<HashSet<string>>)
        Assert.IsTrue(result.Count = 0)
        
    [<Test>]
    let isEmpty () : unit =
        let zero  = HashSet.empty : HashSet<decimal>
        let zero2 = new HashSet<int>([])
        let one   = HashSet.singleton "foo"
        let n     = new HashSet<_>( [1 .. 10] )
        
        Assert.IsTrue(HashSet.isEmpty zero)
        Assert.IsTrue(HashSet.isEmpty zero2)
        
        Assert.IsFalse(HashSet.isEmpty one)
        Assert.IsFalse(HashSet.isEmpty n)
        
    [<Test>]
    let iter () : unit =

        // Empty set
        HashSet.empty |> HashSet.iter (fun _ -> Assert.Fail())

        // Full set
        let elements = [| for i = 0 to 9 do yield false |]
        
        let set = new HashSet<_>(['0' .. '9'])
        HashSet.iter (fun c ->
            let i = int c - int '0'
            elements.[i] <- true) set
        
        Assert.IsTrue (Array.forall ( (=) true ) elements)

    [<Test>]
    let partition () : unit =
        
        // Empty
        let resulta, resultb = HashSet.partition (fun (x : int) -> Assert.Fail(); false) HashSet.empty
        Assert.IsTrue(resulta.Count = 0 && resultb.Count = 0)

        // One
        let single = HashSet.singleton "foo"
        
        let resulta, resultb = HashSet.partition (fun (str : string) -> str.Length <> 3) single
        Assert.IsTrue(resulta.Count = 0 && resultb.Count = 1)
        
        let resulta, resultb = HashSet.partition (fun (str : string) -> str.Length = 3) single
        Assert.IsTrue(resulta.Count = 1 && resultb.Count = 0)

        // Multi
        let alphabet = HashSet.ofList ['a' .. 'z']
        let isVowel = function |'a' | 'e' | 'i' | 'o' | 'u' -> true
                               | _ -> false

        let resulta, resultb = HashSet.partition isVowel alphabet
        Assert.IsTrue(resulta.Count = 5 && resultb.Count = 21)

    [<Test>]
    let remove () : unit =
        
        let emptySet : HashSet<int> = HashSet.empty
        let result = HashSet.remove 42 emptySet
        Assert.IsTrue(result.Count = 0)
        
        // One
        let single = HashSet.singleton 100I
        let resulta = HashSet.remove 100I single
        let resultb = HashSet.remove   1I single
        
        Assert.IsTrue (resulta.Count = 0)
        Assert.IsTrue (resultb.Count = 1)
        
        // Multi
        let a = new HashSet<int>([1 .. 5])
        Assert.IsTrue(a.Count = 5)
        
        let b = HashSet.remove 3 a
        Assert.IsTrue(b.Count = 4)
        // Call again, double delete
        let c = HashSet.remove 3 b
        Assert.IsTrue(c.Count = 4)
        
        Assert.IsFalse(HashSet.exists ( (=) 3 ) c)

    [<Test>]
    let ofList () : unit =
        
        // Empty
        let emptySet = HashSet.ofList ([] : (string * int * HashSet<int>) list)
        Assert.IsTrue(HashSet.isEmpty emptySet)
        
        // Single
        let single = HashSet.ofList [1]
        Assert.IsTrue(single.Count = 1)
        Assert.IsTrue(HashSet.exists ( (=) 1 ) single)
        
        // Multi
        let multi = HashSet.ofList ["mon"; "tue"; "wed"; "thu"; "fri"]
        Assert.IsTrue(multi.Count = 5)
        let expected = new HashSet<_>(["mon"; "tue"; "wed"; "thu"; "fri"])
        Assert.IsTrue( (multi = expected) )

    [<Test>]
    let toList () : unit =

        // Empty
        let emptySet : HashSet<byte> = HashSet.empty
        Assert.IsTrue(HashSet.toList emptySet = [])
        
        // Single
        let single = HashSet.singleton "stuff"
        Assert.IsTrue(HashSet.toList single = ["stuff"])
        
        // Multi
        let multi = new HashSet<_>([5; 2; 3; 1; 4])
        HashSet.toList multi
        |> Collection.assertEquiv [1; 2; 3; 4; 5]

    [<Test>]
    let ofArray () : unit =
        
        // Empty
        let emptySet = HashSet.ofArray ([| |] : (string * int * HashSet<int>) [])
        Assert.IsTrue(HashSet.isEmpty emptySet)
        
        // Single
        let single = HashSet.ofArray [| 1 |]
        Assert.IsTrue(single.Count = 1)
        Assert.IsTrue(HashSet.exists ( (=) 1 ) single)
        
        // Multi
        let multi = HashSet.ofArray [| "mon"; "tue"; "wed"; "thu"; "fri" |]
        Assert.IsTrue(multi.Count = 5)
        let expected = new HashSet<_>(["mon"; "tue"; "wed"; "thu"; "fri"])
        Assert.IsTrue( (multi = expected) )

    [<Test>]
    let toArray () : unit =

        // Empty
        let emptySet : HashSet<byte> = HashSet.empty
        Assert.IsTrue(HashSet.toArray emptySet = [| |])
        
        // Single
        let single = HashSet.singleton "stuff"
        Assert.IsTrue(HashSet.toArray single = [| "stuff" |])
        
        // Multi
        let multi = new HashSet<_>([5; 2; 3; 1; 4])
        Assert.IsTrue(HashSet.toArray multi = [| 1; 2; 3; 4; 5 |])


    [<Test>]
    let ofSeq () : unit =
        
        // Empty
        let emptySet = HashSet.ofSeq ([| |] : (string * int * HashSet<int>) [])
        Assert.IsTrue(HashSet.isEmpty emptySet)
        
        // Single
        let single = HashSet.ofSeq [ 1 ]
        Assert.IsTrue(single.Count = 1)
        Assert.IsTrue(HashSet.exists ( (=) 1 ) single)
        
        // Multi
        let multi = HashSet.ofSeq [| "mon"; "tue"; "wed"; "thu"; "fri" |]
        Assert.IsTrue(multi.Count = 5)
        let expected = new HashSet<_>(["mon"; "tue"; "wed"; "thu"; "fri"])
        Assert.IsTrue( (multi = expected) )

    [<Test>]
    let toSeq () : unit =

        // Empty
        let emptySet : HashSet<byte> = HashSet.empty
        let emptySeq = HashSet.toSeq emptySet
        Assert.IsTrue (Seq.length emptySeq = 0)
        
        // Single
        let single = HashSet.singleton "stuff"
        let singleSeq = HashSet.toSeq single
        Assert.IsTrue(Seq.toList singleSeq = [ "stuff" ])
        
        // Multi
        let multi = new HashSet<_>([5; 2; 3; 1; 4])
        let multiSeq = HashSet.toSeq multi
        Assert.IsTrue(Seq.toList multiSeq = [ 1; 2; 3; 4; 5 ])


    [<Test>]
    let isProperSubset () : unit =
        let set1 = HashSet.ofList [10; 8; 100]
        let set2 = HashSet.ofList [100]
        Assert.IsTrue(HashSet.isProperSubset set2 set1)
        Assert.IsTrue(HashSet.isProperSubset HashSet.empty set2)
        Assert.IsFalse(HashSet.isProperSubset HashSet.empty HashSet.empty)
        Assert.IsFalse(HashSet.isProperSubset set1 set2)

    [<Test>]
    let isProperSuperset () : unit =
        let set1 = HashSet.ofList [10; 8; 100]
        let set2 = HashSet.ofList [100; 8]
        Assert.IsTrue(HashSet.isProperSuperset set1 set2)
        Assert.IsTrue(HashSet.isProperSuperset set2 HashSet.empty)
        Assert.IsFalse(HashSet.isProperSuperset HashSet.empty HashSet.empty)
        Assert.IsFalse(HashSet.isProperSuperset set1 set1)
        Assert.IsFalse(HashSet.isProperSuperset set2 set1)
        
    // ----- Not associated with a module function -----

    // TODO : If possible, modify this test to work with FsCheck instead.
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
            
            let set : HashSet<int> ref = ref HashSet.empty
            // Add permutation items to set in order
            Array.iter (fun i -> set := HashSet.add i !set) permutation
            // Check that the set equals the full list
            Collection.assertEquiv [0 .. i] <| HashSet.toList !set
            // Remove items in permutation order, ensuring set is delt with correctly
            Array.iteri
                (fun idx i -> set := HashSet.remove i !set
                              // Verify all elements have been correctly removed
                              let removedElements = Array.sub permutation 0 (idx + 1) |> HashSet.ofSeq
                              let inter : HashSet<_> = HashSet.intersect !set removedElements
                              Assert.IsTrue(inter.Count = 0))
                permutation
        ()



open FsCheck

(* TODO : Implement FsCheck tests. *)

