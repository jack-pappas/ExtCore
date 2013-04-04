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
(*

Some of the tests below were adapted from the unit tests for
the Haskell 'containers' package:
    https://github.com/haskell/containers/blob/master/tests/intmap-properties.hs

*)

/// Unit tests for the ExtCore.Collections.IntMap type and module.
module ExtCore.Collections.IntMap.Tests

open System.Collections.Generic
open NUnit.Framework
open FsUnit


[<TestCase>]
let isEmpty () : unit =
    IntMap.empty
    |> IntMap.isEmpty
    |> should be True

    IntMap.singleton 1 'a'
    |> IntMap.isEmpty
    |> should be False

[<TestCase>]
let count () : unit =
    IntMap.empty
    |> IntMap.count
    |> should equal 0

    IntMap.singleton 1 'a'
    |> IntMap.count
    |> should equal 1

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.count
    |> should equal 8

[<TestCase>]
let singleton () : unit =
    IntMap.singleton 1 'a'
    |> should equal (
        IntMap.add 1 'a' IntMap.empty)

    IntMap.singleton 1 'a'
    |> IntMap.count
    |> should equal 1

[<TestCase>]
let containsKey () : unit =
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.containsKey 5
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.containsKey 1
    |> should be False

[<TestCase>]
let tryFind () : unit =
    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.tryFind 5
    |> should equal (Some 'a')

    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.tryFind 7
    |> should equal None

[<TestCase>]
let find () : unit =
    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.find 5
    |> should equal 'a'

[<TestCase>]
let findOrDefault () : unit =
    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.findOrDefault 'z' 5
    |> should equal 'a'

    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.findOrDefault 'z' 7
    |> should equal 'z'

[<TestCase>]
let tryFindKey () : unit =
    (IntMap.empty : IntMap<string>)
    |> IntMap.tryFindKey (fun k v ->
        k % 2 = 0)
    |> should equal None

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.tryFindKey (fun k v ->
        (k + int v) % 11 = 0)
    |> should equal (Some 12)

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.tryFindKey (fun k v ->
        (k + int v) % 289 = 0)
    |> should equal None

[<TestCase>]
let add () : unit =
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.add 5 'x'
    |> should equal (IntMap.ofArray
       [| (5, 'x'); (3, 'b'); (11, 'f'); (2, 'd');
          (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |])

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.add 13 'x'
    |> should equal (IntMap.ofArray
       [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
          (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (13, 'x'); |])

    IntMap.empty
    |> IntMap.add 5 'x'
    |> should equal (
        IntMap.singleton 5 'x')

[<TestCase>]
let remove () : unit =
    (IntMap.empty : IntMap<string>)
    |> IntMap.remove 5
    |> IntMap.isEmpty
    |> should be True

    [(5, "a"); (3, "b")]
    |> IntMap.ofList
    |> IntMap.remove 5
    |> should equal (
        IntMap.singleton 3 "b")

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.remove 4
    |> should equal (IntMap.ofArray
       [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
          (17, 'a'); (12, 'b'); (14, 'c'); |])

[<TestCase>]
let union () : unit =
    IntMap.union
        (IntMap.ofArray [| (3, 'b'); (11, 'F'); (2, 'd'); (4, 'G'); (12, 'b'); |])
        (IntMap.ofArray [| (5, 'a'); (11, 'f'); (17, 'a'); (4, 'g'); (14, 'c'); |])
    |> should equal
        (IntMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])

[<TestCase>]
let intersect () : unit =
    IntMap.intersect
        (IntMap.ofArray [| (3, 'b'); (11, 'F'); (2, 'd'); (4, 'G'); (12, 'b'); |])
        IntMap.empty
    |> IntMap.isEmpty
    |> should be True

    IntMap.intersect
        (IntMap.ofArray [| (3, 'b'); (11, 'F'); (2, 'd'); (4, 'G'); (12, 'b'); |])
        (IntMap.ofArray [| (5, 'a'); (11, 'f'); (17, 'a'); (4, 'g'); (14, 'c'); |])
    |> should equal
        (IntMap.ofArray [| (11, 'F'); (4, 'G'); |])

[<TestCase>]
let difference () : unit =
    IntMap.difference
        (IntMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])
        IntMap.empty
    |> should equal
        (IntMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])

    IntMap.difference
        (IntMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])
        (IntMap.ofArray [| (3, 'b'); (11, 'f'); (2, 'd'); (4, 'g'); (12, 'b'); |])
    |> should equal
        (IntMap.ofArray [| (5, 'a'); (17, 'a'); (14, 'c'); |])

[<TestCase>]
let ofSeq () : unit =
    (Seq.empty : seq<int * string>)
    |> IntMap.ofSeq
    |> IntMap.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> Seq.ofArray
    |> IntMap.ofSeq
    |> should equal
        (IntMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])

[<TestCase>]
let ofList () : unit =
    IntMap.ofList []
    |> IntMap.isEmpty
    |> should be True

    [(5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G')]
    |> IntMap.ofList
    |> should equal
        (IntMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])

[<TestCase>]
let ofArray () : unit =
    Array.empty
    |> IntMap.ofArray
    |> IntMap.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> should equal
        (IntMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])

[<TestCase>]
let ofMap () : unit =
    Map.empty
    |> IntMap.ofMap
    |> IntMap.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |]
    |> Map.ofArray
    |> IntMap.ofMap
    |> should equal
        (IntMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])

[<TestCase>]
let toSeq () : unit =
    IntMap.empty
    |> IntMap.toSeq
    |> Seq.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.toSeq
    |> Seq.toArray
    |> should equal
        [| (2, 'd'); (3, 'b'); (4, 'G'); (5, 'a'); (11, 'F'); (12, 'b'); (14, 'c'); (17, 'a'); |]

[<TestCase>]
let toList () : unit =
    IntMap.empty
    |> IntMap.toList
    |> List.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.toList
    |> should equal
        [(2, 'd'); (3, 'b'); (4, 'G'); (5, 'a'); (11, 'F'); (12, 'b'); (14, 'c'); (17, 'a')]

[<TestCase>]
let toArray () : unit =
    IntMap.empty
    |> IntMap.toArray
    |> Array.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.toArray
    |> should equal
        [| (2, 'd'); (3, 'b'); (4, 'G'); (5, 'a'); (11, 'F'); (12, 'b'); (14, 'c'); (17, 'a'); |]

[<TestCase>]
let toMap () : unit =
    IntMap.empty
    |> IntMap.toMap
    |> Map.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.toMap
    |> should equal
        (Map.ofArray [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |])

[<TestCase>]
let tryPick () : unit =
    // Test case for empty input.
    IntMap.empty
    |> IntMap.tryPick (fun k v ->
        if (k + v) = 10 then
            Some 11
        else None)
    |> should equal None

    // Test case where no elements match the 'picker' function.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.tryPick (fun k v ->
        if System.Char.IsControl v then
            Some (int v + k)
        else None)
    |> should equal None

    // Test case where a single binding matches the 'picker' function.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.tryPick (fun k v ->
        if (k % 7 = 0) && v = 'c' then
            Some (k - 2)
        else None)
    |> should equal (Some 12)

    // Test case where multiple bindings match the 'picker' function.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.tryPick (fun k v ->
        if k % 3 = 2 then
            Some v
        else None)
    |> should equal (Some 'd')

[<TestCase>]
let pick () : unit =
    // Test case where a single binding matches the 'picker' function.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.pick (fun k v ->
        if (k % 7 = 0) && v = 'c' then
            Some (k - 2)
        else None)
    |> should equal 12

    // Test case where multiple bindings match the 'picker' function.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.pick (fun k v ->
        if k % 3 = 2 then
            Some v
        else None)
    |> should equal 'd'

[<TestCase; ExpectedException(typeof<KeyNotFoundException>)>]
let ``pick raises exn on empty input`` () : unit =
    IntMap.empty
    |> IntMap.pick (fun k v ->
        if (k + v) % 2 = 0 then
            Some (v + 1)
        else None)
    |> ignore

[<TestCase; ExpectedException(typeof<KeyNotFoundException>)>]
let ``pick raises exn when no match is found`` () : unit =
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.pick (fun k v ->
        if System.Char.IsControl v then
            Some (int v + k)
        else None)
    |> ignore

[<TestCase>]
let map () : unit =
    IntMap.empty
    |> IntMap.map (sprintf "%i:%c")
    |> IntMap.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.map (sprintf "%i:%c")
    |> should equal (IntMap.ofArray
        [| (5, "5:a"); (3, "3:b"); (11, "11:f"); (2, "2:d");
            (17, "17:a"); (4, "4:g"); (12, "12:b"); (14, "14:c"); |])

[<TestCase>]
let filter () : unit =
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.filter (fun k v ->
        (k % 2 = 0) && System.Char.IsLower v)
    |> should equal
        (IntMap.ofArray [| (2, 'd'); (12, 'b'); (14, 'c'); |])

[<TestCase>]
let choose () : unit =
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.choose (fun k v ->
        if k % 2 <> 0 then
            Some (System.Char.ToUpper v)
        else None)
    |> should equal
        (IntMap.ofArray [| (5, 'A'); (3, 'B'); (17, 'A'); (11, 'F'); |])

[<TestCase>]
let iter () : unit =
    do
        let elements = ResizeArray ()

        IntMap.empty
        |> IntMap.iter (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.isEmpty
        |> should be True

    do
        let elements = ResizeArray ()

        [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
        |> IntMap.ofArray
        |> IntMap.iter (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.toArray
        |> should equal
            [| 'D'; 'B'; 'G'; 'A'; 'F'; 'B'; 'C'; 'A'; |]

[<TestCase>]
let iterBack () : unit =
    do
        let elements = ResizeArray ()

        IntMap.empty
        |> IntMap.iter (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.isEmpty
        |> should be True

    do
        let elements = ResizeArray ()

        [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
        |> IntMap.ofArray
        |> IntMap.iterBack (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.toArray
        |> should equal
            [| 'A'; 'C'; 'B'; 'F'; 'A'; 'G'; 'B'; 'D'; |]

[<TestCase>]
let fold () : unit =
    do
        let elements = ResizeArray ()

        (0, IntMap.empty)
        ||> IntMap.fold (fun counter k v ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> should equal 0

        elements
        |> ResizeArray.isEmpty
        |> should be True

    do
        let elements = ResizeArray ()

        let testMap =
            [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
            |> IntMap.ofArray

        (0, testMap)
        ||> IntMap.fold (fun counter k v ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> should equal (IntMap.count testMap)

        elements
        |> ResizeArray.toArray
        |> should equal
            [| 102; 102; 77; 105; 85; 115; 119; 121; |]

[<TestCase>]
let foldBack () : unit =
    do
        let elements = ResizeArray ()

        (IntMap.empty, 0)
        ||> IntMap.foldBack (fun counter k v ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> should equal 0

        elements
        |> ResizeArray.isEmpty
        |> should be True

    do
        let elements = ResizeArray ()

        let testMap =
            [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
            |> IntMap.ofArray

        (testMap, 0)
        ||> IntMap.foldBack (fun k v counter ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> should equal (IntMap.count testMap)

        elements
        |> ResizeArray.toArray
        |> should equal
            [| 114; 114; 112; 84; 106; 80; 107; 109; |]

[<TestCase>]
let exists () : unit =
    IntMap.empty
    |> IntMap.exists (fun k v ->
        (k + int v) % 2 = 0)
    |> should be False

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.exists (fun k v ->
        (k + int v) > 200)
    |> should be False

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.exists (fun k v ->
        (k + int v) % 2 = 0)
    |> should be True

[<TestCase>]
let forall () : unit =
    IntMap.empty
    |> IntMap.forall (fun k v ->
        (k + int v) < 200)
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.forall (fun k v ->
        (k + int v) < 200)
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.forall (fun k v ->
        (k + int v) % 2 = 0)
    |> should be False

[<TestCase>]
let partition () : unit =
    do
        let evens, odds =
            IntMap.empty
            |> IntMap.partition (fun k v ->
                (k + int v) % 2 = 0)

        evens
        |> IntMap.isEmpty
        |> should be True

        odds
        |> IntMap.isEmpty
        |> should be True

    do
        let evens, odds =
            [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
            |> IntMap.ofArray
            |> IntMap.partition (fun k v ->
                (k + int v) % 2 = 0)

        evens
        |> should equal
            (IntMap.ofArray [| (5, 'a'); (2, 'd'); (17, 'a'); (12, 'b'); |])

        odds
        |> should equal
            (IntMap.ofArray [| (3, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |])


[<TestCase>]
let mapPartition () : unit =
    do
        let evens, odds =
            IntMap.empty
            |> IntMap.mapPartition (fun k v ->
                if (k + int v) % 2 = 0 then
                    Choice1Of2 (k + 10)
                else
                    Choice2Of2 (System.Char.ToUpper v))

        evens
        |> IntMap.isEmpty
        |> should be True

        odds
        |> IntMap.isEmpty
        |> should be True

    do
        let evens, odds =
            [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
            |> IntMap.ofArray
            |> IntMap.mapPartition (fun k v ->
                if (k + int v) % 2 = 0 then
                    Choice1Of2 (k + 10)
                else
                    Choice2Of2 (System.Char.ToUpper v))

        let evensExpected, oddsExpected =
            [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
            |> Array.mapPartition (fun (k, v) ->
                if (k + int v) % 2 = 0 then
                    Choice1Of2 (k, k + 10)
                else
                    Choice2Of2 (k, System.Char.ToUpper v))

        evens |> should equal (IntMap.ofArray evensExpected)

        odds |> should equal (IntMap.ofArray oddsExpected)



(* FsCheck Tests *)

open FsCheck

//
let arbIntMap<'T> : Arbitrary<IntMap<'T>> =
    gen {
    let! ks = Arb.generate<int list>
    let! xs = Arb.generate<'T list>

    return IntMap.ofList <| List.zip ks xs
    
    } |> Arb.fromGen

type UMap = IntMap<unit>
type IMap = IntMap<int>
type SMap = IntMap<string>
