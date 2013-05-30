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

/// Unit tests for the ExtCore.Caching.LruCache type and module.
module Tests.ExtCore.Caching.LruCache

open System.Collections.Generic
open ExtCore.Caching
open NUnit.Framework
open FsUnit


[<Test>]
let count () : unit =
    LruCache.empty
    |> LruCache.count
    |> assertEqual 0

    LruCache.create 1u
    |> LruCache.add "red" 123
    |> LruCache.count
    |> assertEqual 1

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> LruCache.ofArray 4u
    |> LruCache.count
    |> assertEqual 4

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> LruCache.ofArray 100u
    |> LruCache.count
    |> assertEqual 8

[<Test>]
let capacity () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let isEmpty () : unit =
    LruCache.empty
    |> LruCache.isEmpty
    |> assertTrue

    LruCache.empty
    |> LruCache.add "red" 123
    |> LruCache.isEmpty
    |> assertTrue

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> LruCache.ofArray 100u
    |> LruCache.isEmpty
    |> assertFalse

[<Test>]
let create () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let changeCapacity () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let containsKey () : unit =
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> LruCache.ofArray 100u
    |> LruCache.containsKey 5
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> LruCache.ofArray 100u
    |> LruCache.containsKey 1
    |> should be False

[<Test>]
let tryFind () : unit =
    [(5, 'a'); (3, 'b')]
    |> LruCache.ofList 100u
    |> LruCache.tryFind 5
    |> fst
    |> assertEqual (Some 'a')

    [(5, 'a'); (3, 'b')]
    |> LruCache.ofList 100u
    |> LruCache.tryFind 7
    |> fst
    |> assertEqual None

[<Test>]
let find () : unit =
    [(5, 'a'); (3, 'b')]
    |> LruCache.ofList 100u
    |> LruCache.find 5
    |> fst
    |> assertEqual 'a'

[<Test; ExpectedException(typeof<KeyNotFoundException>)>]
let ``find raises exn when key is not found`` () : unit =
    [(5, 'a'); (3, 'b')]
    |> LruCache.ofList 100u
    |> LruCache.find 9
    |> ignore

[<Test>]
let add () : unit =
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> LruCache.ofArray 100u
    |> LruCache.add 5 'x'
    |> LruCache.toArray
    |> Collection.assertEqual
       [| (3, 'b'); (11, 'f'); (2, 'd');
          (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (5, 'x'); |]

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> LruCache.ofArray 100u
    |> LruCache.add 13 'x'
    |> LruCache.toArray
    |> Collection.assertEqual
       [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
          (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (13, 'x'); |]

    LruCache.create 20u
    |> LruCache.add 5 'x'
    |> LruCache.toArray
    |> Collection.assertEqual
        [| (5, 'x'); |]

[<Test>]
let remove () : unit =
    (LruCache.empty : LruCache<int, string>)
    |> LruCache.remove 5
    |> LruCache.isEmpty
    |> should be True

    [(5, "a"); (3, "b")]
    |> LruCache.ofList 100u
    |> LruCache.remove 5
    |> LruCache.toArray
    |> Collection.assertEqual
        [| (3, "b"); |]

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> LruCache.ofArray 100u
    |> LruCache.remove 4
    |> LruCache.toArray
    |> Collection.assertEqual
       [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
          (17, 'a'); (12, 'b'); (14, 'c'); |]

[<Test>]
let ofSeq () : unit =
    (Seq.empty : seq<int * string>)
    |> LruCache.ofSeq 100u
    |> LruCache.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> Seq.ofArray
    |> LruCache.ofSeq 100u
    |> LruCache.toArray
    |> Collection.assertEqual
        [| (5, 'a'); (3, 'b'); (2, 'd'); (17, 'a'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]

[<Test>]
let ofList () : unit =
    LruCache.ofList 100u []
    |> LruCache.isEmpty
    |> should be True

    [(5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G')]
    |> LruCache.ofList 100u
    |> LruCache.toArray
    |> Collection.assertEqual
        [| (5, 'a'); (3, 'b'); (2, 'd'); (17, 'a'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]

[<Test>]
let ofArray () : unit =
    Array.empty
    |> LruCache.ofArray 100u
    |> LruCache.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> LruCache.ofArray 100u
    |> LruCache.toArray
    |> Collection.assertEqual
        [| (5, 'a'); (3, 'b'); (2, 'd'); (17, 'a'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]

[<Test>]
let ofMap () : unit =
    Map.empty
    |> LruCache.ofMap 100u
    |> LruCache.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |]
    |> Map.ofArray
    |> LruCache.ofMap 100u
    |> LruCache.toArray
    |> Collection.assertEqual
        [| (2, 'd'); (3, 'b'); (4, 'G'); (5, 'a'); (11, 'F'); (12, 'b'); (14, 'c'); (17, 'a'); |]

[<Test>]
let toSeq () : unit =
    LruCache.empty
    |> LruCache.toSeq
    |> Seq.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> LruCache.ofArray 100u
    |> LruCache.toSeq
    |> Seq.toArray
    |> Collection.assertEqual
        [| (5, 'a'); (3, 'b'); (2, 'd'); (17, 'a'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]

[<Test>]
let toList () : unit =
    LruCache.empty
    |> LruCache.toList
    |> List.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> LruCache.ofArray 100u
    |> LruCache.toList
    |> Collection.assertEqual
        [(5, 'a'); (3, 'b'); (2, 'd'); (17, 'a'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G')]

[<Test>]
let toArray () : unit =
    LruCache.empty
    |> LruCache.toArray
    |> Array.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> LruCache.ofArray 100u
    |> LruCache.toArray
    |> Collection.assertEqual
        [| (5, 'a'); (3, 'b'); (2, 'd'); (17, 'a'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]

[<Test>]
let toMap () : unit =
    LruCache.empty
    |> LruCache.toMap
    |> Map.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> LruCache.ofArray 100u
    |> LruCache.toMap
    |> assertEqual
        (Map.ofArray [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |])

[<Test>]
let tryPick () : unit =
    // Test case for empty input.
    LruCache.empty
    |> LruCache.tryPick (fun k v ->
        if (k + v) = 10 then
            Some 11
        else None)
    |> assertEqual None

    // Test case where no elements match the 'picker' function.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> LruCache.ofArray 100u
    |> LruCache.tryPick (fun k v ->
        if System.Char.IsControl v then
            Some (int v + k)
        else None)
    |> assertEqual None

    // Test case where a single binding matches the 'picker' function.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> LruCache.ofArray 100u
    |> LruCache.tryPick (fun k v ->
        if (k % 7 = 0) && v = 'c' then
            Some (k - 2)
        else None)
    |> assertEqual (Some 12)

    // Test case where multiple bindings match the 'picker' function.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> LruCache.ofArray 100u
    |> LruCache.tryPick (fun k v ->
        if k % 3 = 2 then
            Some v
        else None)
    |> assertEqual (Some 'a')

[<Test>]
let pick () : unit =
    // Test case where a single binding matches the 'picker' function.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> LruCache.ofArray 100u
    |> LruCache.pick (fun k v ->
        if (k % 7 = 0) && v = 'c' then
            Some (k - 2)
        else None)
    |> assertEqual 12

    // Test case where multiple bindings match the 'picker' function.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> LruCache.ofArray 100u
    |> LruCache.pick (fun k v ->
        if k % 3 = 2 then
            Some v
        else None)
    |> assertEqual 'a'

[<Test; ExpectedException(typeof<KeyNotFoundException>)>]
let ``pick raises exn on empty input`` () : unit =
    LruCache.empty
    |> LruCache.pick (fun k v ->
        if (k + v) % 2 = 0 then
            Some (v + 1)
        else None)
    |> ignore

[<Test; ExpectedException(typeof<KeyNotFoundException>)>]
let ``pick raises exn when no match is found`` () : unit =
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> LruCache.ofArray 100u
    |> LruCache.pick (fun k v ->
        if System.Char.IsControl v then
            Some (int v + k)
        else None)
    |> ignore

[<Test>]
let map () : unit =
    LruCache.empty
    |> LruCache.map (sprintf "%i:%c")
    |> LruCache.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> LruCache.ofArray 100u
    |> LruCache.map (sprintf "%i:%c")
    |> LruCache.toArray
    |> Collection.assertEqual
        [| (5, "5:a"); (3, "3:b"); (11, "11:f"); (2, "2:d");
            (17, "17:a"); (4, "4:g"); (12, "12:b"); (14, "14:c"); |]

[<Test>]
let filter () : unit =
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> LruCache.ofArray 100u
    |> LruCache.filter (fun k v ->
        (k % 2 = 0) && System.Char.IsLower v)
    |> LruCache.toArray
    |> Collection.assertEqual
        [| (2, 'd'); (12, 'b'); (14, 'c'); |]

[<Test>]
let choose () : unit =
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> LruCache.ofArray 100u
    |> LruCache.choose (fun k v ->
        if k % 2 <> 0 then
            Some (System.Char.ToUpper v)
        else None)
    |> LruCache.toArray
    |> Collection.assertEqual
        [| (5, 'A'); (3, 'B'); (17, 'A'); (11, 'F'); |]

[<Test>]
let iter () : unit =
    do
        let elements = ResizeArray ()

        LruCache.empty
        |> LruCache.iter (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.isEmpty
        |> should be True

    do
        let elements = ResizeArray ()

        [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
        |> LruCache.ofArray 100u
        |> LruCache.iter (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.toArray
        |> Collection.assertEqual
            [| 'A'; 'B'; 'D'; 'A'; 'B'; 'C'; 'F'; 'G'; |]

[<Test>]
let iterBack () : unit =
    do
        let elements = ResizeArray ()

        LruCache.empty
        |> LruCache.iter (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.isEmpty
        |> should be True

    do
        let elements = ResizeArray ()

        [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
        |> LruCache.ofArray 100u
        |> LruCache.iterBack (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.toArray
        |> Collection.assertEqual
            [| 'G'; 'F'; 'C'; 'B'; 'A'; 'D'; 'B'; 'A'; |]

[<Test>]
let fold () : unit =
    do
        let elements = ResizeArray ()

        (0, LruCache.empty)
        ||> LruCache.fold (fun counter k v ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> assertEqual 0

        elements
        |> ResizeArray.isEmpty
        |> should be True

    do
        let elements = ResizeArray ()

        let testMap =
            [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
            |> LruCache.ofArray 100u
            
        (0, testMap)
        ||> LruCache.fold (fun counter k v ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> assertEqual (LruCache.count testMap)

        elements
        |> ResizeArray.toArray
        |> Collection.assertEqual
            [| 102; 102; 104; 117; 114; 118; 87; 82; |]

[<Test>]
let foldBack () : unit =
    do
        let elements = ResizeArray ()

        (LruCache.empty, 0)
        ||> LruCache.foldBack (fun counter k v ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> assertEqual 0

        elements
        |> ResizeArray.isEmpty
        |> should be True

    do
        let elements = ResizeArray ()

        let testMap =
            [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
            |> LruCache.ofArray 100u

        (testMap, 0)
        ||> LruCache.foldBack (fun k v counter ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> assertEqual (LruCache.count testMap)

        elements
        |> ResizeArray.toArray
        |> Collection.assertEqual
            [| 75; 82; 115; 113; 118; 107; 107; 109; |]

[<Test>]
let exists () : unit =
    LruCache.empty
    |> LruCache.exists (fun k v ->
        (k + int v) % 2 = 0)
    |> should be False

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> LruCache.ofArray 100u
    |> LruCache.exists (fun k v ->
        (k + int v) > 200)
    |> should be False

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> LruCache.ofArray 100u
    |> LruCache.exists (fun k v ->
        (k + int v) % 2 = 0)
    |> should be True

[<Test>]
let forall () : unit =
    LruCache.empty
    |> LruCache.forall (fun k v ->
        (k + int v) < 200)
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> LruCache.ofArray 100u
    |> LruCache.forall (fun k v ->
        (k + int v) < 200)
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> LruCache.ofArray 100u
    |> LruCache.forall (fun k v ->
        (k + int v) % 2 = 0)
    |> should be False

[<Test>]
let partition () : unit =
    do
        let evens, odds =
            LruCache.empty
            |> LruCache.partition (fun k v ->
                (k + int v) % 2 = 0)

        evens
        |> LruCache.isEmpty
        |> should be True

        odds
        |> LruCache.isEmpty
        |> should be True

    do
        let evens, odds =
            [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
            |> LruCache.ofArray 100u
            |> LruCache.partition (fun k v ->
                (k + int v) % 2 = 0)

        evens
        |> LruCache.toArray
        |> Collection.assertEqual
            [| (5, 'a'); (2, 'd'); (17, 'a'); (12, 'b'); |]

        odds
        |> LruCache.toArray
        |> Collection.assertEqual
            [| (3, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]


[<Test>]
let mapPartition () : unit =
    do
        let evens, odds =
            LruCache.empty
            |> LruCache.mapPartition (fun k v ->
                if (k + int v) % 2 = 0 then
                    Choice1Of2 (k + 10)
                else
                    Choice2Of2 (System.Char.ToUpper v))

        evens
        |> LruCache.isEmpty
        |> should be True

        odds
        |> LruCache.isEmpty
        |> should be True

    do
        let evens, odds =
            [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
            |> LruCache.ofArray 100u
            |> LruCache.mapPartition (fun k v ->
                if (k + int v) % 2 = 0 then
                    Choice1Of2 (k + 10)
                else
                    Choice2Of2 (System.Char.ToUpper v))

        let evensExpected, oddsExpected =
            [| (5, 'a'); (3, 'b'); (2, 'd'); (17, 'a'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
            |> ExtCore.Collections.Array.mapPartition (fun (k, v) ->
                if (k + int v) % 2 = 0 then
                    Choice1Of2 (k, k + 10)
                else
                    Choice2Of2 (k, System.Char.ToUpper v))

        evens
        |> LruCache.toArray
        |> Collection.assertEqual evensExpected

        odds
        |> LruCache.toArray
        |> Collection.assertEqual oddsExpected


(* TODO : Import MapType and MapModule tests from F# distribution. *)


open FsCheck

(* TODO : Implement FsCheck tests. *)

