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
(*

Some of the tests below were adapted from the unit tests for
the Haskell 'containers' package:
    https://github.com/haskell/containers/blob/master/tests/intmap-properties.hs

*)

/// Unit tests for the ExtCore.Collections.IntMap type and module.
module Tests.ExtCore.Collections.IntMap

open System
open System.Collections
open System.Collections.Generic
open NUnit.Framework
open FsUnit


[<Test>]
let isEmpty () : unit =
    IntMap.empty
    |> IntMap.isEmpty
    |> should be True

    IntMap.singleton 1 'a'
    |> IntMap.isEmpty
    |> should be False

[<Test>]
let count () : unit =
    IntMap.empty
    |> IntMap.count
    |> assertEqual 0

    IntMap.singleton 1 'a'
    |> IntMap.count
    |> assertEqual 1

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.count
    |> assertEqual 8

[<Test>]
let singleton () : unit =
    IntMap.singleton 1 'a'
    |> assertEqual (
        IntMap.add 1 'a' IntMap.empty)

    IntMap.singleton 1 'a'
    |> IntMap.count
    |> assertEqual 1

[<Test>]
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

[<Test>]
let tryFind () : unit =
    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.tryFind 5
    |> assertEqual (Some 'a')

    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.tryFind 7
    |> assertEqual None

[<Test>]
let find () : unit =
    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.find 5
    |> assertEqual 'a'

[<Test; ExpectedException(typeof<KeyNotFoundException>)>]
let ``find raises exn when key is not found`` () : unit =
    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.find 9
    |> ignore

[<Test>]
let findOrDefault () : unit =
    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.findOrDefault 'z' 5
    |> assertEqual 'a'

    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.findOrDefault 'z' 7
    |> assertEqual 'z'

[<Test>]
let tryFindKey () : unit =
    (IntMap.empty : IntMap<string>)
    |> IntMap.tryFindKey (fun k v ->
        k % 2 = 0)
    |> assertEqual None

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.tryFindKey (fun k v ->
        (k + int v) % 11 = 0)
    |> assertEqual (Some 12)

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.tryFindKey (fun k v ->
        (k + int v) % 289 = 0)
    |> assertEqual None

[<Test>]
let add () : unit =
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.add 5 'x'
    |> assertEqual (IntMap.ofArray
       [| (5, 'x'); (3, 'b'); (11, 'f'); (2, 'd');
          (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |])

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.add 13 'x'
    |> assertEqual (IntMap.ofArray
       [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
          (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (13, 'x'); |])

    IntMap.empty
    |> IntMap.add 5 'x'
    |> assertEqual (
        IntMap.singleton 5 'x')

[<Test>]
let remove () : unit =
    (IntMap.empty : IntMap<string>)
    |> IntMap.remove 5
    |> IntMap.isEmpty
    |> should be True

    [(5, "a"); (3, "b")]
    |> IntMap.ofList
    |> IntMap.remove 5
    |> assertEqual (
        IntMap.singleton 3 "b")

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.remove 4
    |> assertEqual (IntMap.ofArray
       [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
          (17, 'a'); (12, 'b'); (14, 'c'); |])

[<Test>]
let union () : unit =
    IntMap.union
        (IntMap.ofArray [| (3, 'b'); (11, 'F'); (2, 'd'); (4, 'G'); (12, 'b'); |])
        (IntMap.ofArray [| (5, 'a'); (11, 'f'); (17, 'a'); (4, 'g'); (14, 'c'); |])
    |> assertEqual
        (IntMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])

[<Test>]
let intersect () : unit =
    IntMap.intersect
        (IntMap.ofArray [| (3, 'b'); (11, 'F'); (2, 'd'); (4, 'G'); (12, 'b'); |])
        IntMap.empty
    |> IntMap.isEmpty
    |> should be True

    IntMap.intersect
        (IntMap.ofArray [| (3, 'b'); (11, 'F'); (2, 'd'); (4, 'G'); (12, 'b'); |])
        (IntMap.ofArray [| (5, 'a'); (11, 'f'); (17, 'a'); (4, 'g'); (14, 'c'); |])
    |> assertEqual
        (IntMap.ofArray [| (11, 'F'); (4, 'G'); |])

[<Test>]
let difference () : unit =
    IntMap.difference
        (IntMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])
        IntMap.empty
    |> assertEqual
        (IntMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])

    IntMap.difference
        (IntMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])
        (IntMap.ofArray [| (3, 'b'); (11, 'f'); (2, 'd'); (4, 'g'); (12, 'b'); |])
    |> assertEqual
        (IntMap.ofArray [| (5, 'a'); (17, 'a'); (14, 'c'); |])

[<Test>]
let isSubmapOfBy () : unit =
    let map1 = IntMap.ofList [(1,1);(2,2)]
        
    map1.IsSubmapOfBy ((=),
        IntMap.ofList [(1,1)])
    |> should be True

    map1.IsSubmapOfBy ((<=),
        IntMap.ofList [(1,1)])
    |> should be True

    map1.IsSubmapOfBy ((=),
        IntMap.ofList [(1,1);(2,2)])
    |> should be True

    map1.IsSubmapOfBy ((=),
        IntMap.ofList [(1,2)])
    |> should be False

    map1.IsSubmapOfBy ((<),
        IntMap.ofList [(1,1)])
    |> should be False

    let map2 = IntMap.ofList [(1,1)]

    map2.IsSubmapOfBy ((=),
        IntMap.ofList [(1,1);(2,2)])
    |> should be False


[<Test>]
let ofSeq () : unit =
    (Seq.empty : seq<int * string>)
    |> IntMap.ofSeq
    |> IntMap.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> Seq.ofArray
    |> IntMap.ofSeq
    |> assertEqual
        (IntMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])

[<Test>]
let ofList () : unit =
    IntMap.ofList []
    |> IntMap.isEmpty
    |> should be True

    [(5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G')]
    |> IntMap.ofList
    |> assertEqual
        (IntMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])

[<Test>]
let ofArray () : unit =
    Array.empty
    |> IntMap.ofArray
    |> IntMap.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> assertEqual
        (IntMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])

[<Test>]
let ofMap () : unit =
    Map.empty
    |> IntMap.ofMap
    |> IntMap.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |]
    |> Map.ofArray
    |> IntMap.ofMap
    |> assertEqual
        (IntMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])

[<Test>]
let toSeq () : unit =
    IntMap.empty
    |> IntMap.toSeq
    |> Seq.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.toSeq
    |> Seq.toArray
    |> assertEqual
        [| (2, 'd'); (3, 'b'); (4, 'G'); (5, 'a'); (11, 'F'); (12, 'b'); (14, 'c'); (17, 'a'); |]

[<Test>]
let toList () : unit =
    IntMap.empty
    |> IntMap.toList
    |> List.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.toList
    |> assertEqual
        [(2, 'd'); (3, 'b'); (4, 'G'); (5, 'a'); (11, 'F'); (12, 'b'); (14, 'c'); (17, 'a')]

[<Test>]
let toArray () : unit =
    IntMap.empty
    |> IntMap.toArray
    |> Array.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.toArray
    |> assertEqual
        [| (2, 'd'); (3, 'b'); (4, 'G'); (5, 'a'); (11, 'F'); (12, 'b'); (14, 'c'); (17, 'a'); |]

[<Test>]
let toMap () : unit =
    IntMap.empty
    |> IntMap.toMap
    |> Map.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.toMap
    |> assertEqual
        (Map.ofArray [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |])

[<Test>]
let tryPick () : unit =
    // Test case for empty input.
    IntMap.empty
    |> IntMap.tryPick (fun k v ->
        if (k + v) = 10 then
            Some 11
        else None)
    |> assertEqual None

    // Test case where no elements match the 'picker' function.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.tryPick (fun k v ->
        if System.Char.IsControl v then
            Some (int v + k)
        else None)
    |> assertEqual None

    // Test case where a single binding matches the 'picker' function.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.tryPick (fun k v ->
        if (k % 7 = 0) && v = 'c' then
            Some (k - 2)
        else None)
    |> assertEqual (Some 12)

    // Test case where multiple bindings match the 'picker' function.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.tryPick (fun k v ->
        if k % 3 = 2 then
            Some v
        else None)
    |> assertEqual (Some 'd')

[<Test>]
let pick () : unit =
    // Test case where a single binding matches the 'picker' function.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.pick (fun k v ->
        if (k % 7 = 0) && v = 'c' then
            Some (k - 2)
        else None)
    |> assertEqual 12

    // Test case where multiple bindings match the 'picker' function.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.pick (fun k v ->
        if k % 3 = 2 then
            Some v
        else None)
    |> assertEqual 'd'

[<Test; ExpectedException(typeof<KeyNotFoundException>)>]
let ``pick raises exn on empty input`` () : unit =
    IntMap.empty
    |> IntMap.pick (fun k v ->
        if (k + v) % 2 = 0 then
            Some (v + 1)
        else None)
    |> ignore

[<Test; ExpectedException(typeof<KeyNotFoundException>)>]
let ``pick raises exn when no match is found`` () : unit =
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.pick (fun k v ->
        if System.Char.IsControl v then
            Some (int v + k)
        else None)
    |> ignore

[<Test>]
let map () : unit =
    IntMap.empty
    |> IntMap.map (sprintf "%i:%c")
    |> IntMap.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.map (sprintf "%i:%c")
    |> assertEqual (IntMap.ofArray
        [| (5, "5:a"); (3, "3:b"); (11, "11:f"); (2, "2:d");
            (17, "17:a"); (4, "4:g"); (12, "12:b"); (14, "14:c"); |])

[<Test>]
let filter () : unit =
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.filter (fun k v ->
        (k % 2 = 0) && System.Char.IsLower v)
    |> assertEqual
        (IntMap.ofArray [| (2, 'd'); (12, 'b'); (14, 'c'); |])

[<Test>]
let choose () : unit =
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntMap.ofArray
    |> IntMap.choose (fun k v ->
        if k % 2 <> 0 then
            Some (System.Char.ToUpper v)
        else None)
    |> assertEqual
        (IntMap.ofArray [| (5, 'A'); (3, 'B'); (17, 'A'); (11, 'F'); |])

[<Test>]
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
        |> assertEqual
            [| 'D'; 'B'; 'G'; 'A'; 'F'; 'B'; 'C'; 'A'; |]

[<Test>]
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
        |> assertEqual
            [| 'A'; 'C'; 'B'; 'F'; 'A'; 'G'; 'B'; 'D'; |]

[<Test>]
let fold () : unit =
    do
        let elements = ResizeArray ()

        (0, IntMap.empty)
        ||> IntMap.fold (fun counter k v ->
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
            |> IntMap.ofArray

        (0, testMap)
        ||> IntMap.fold (fun counter k v ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> assertEqual (IntMap.count testMap)

        elements
        |> ResizeArray.toArray
        |> assertEqual
            [| 102; 102; 77; 105; 85; 115; 119; 121; |]

[<Test>]
let foldBack () : unit =
    do
        let elements = ResizeArray ()

        (IntMap.empty, 0)
        ||> IntMap.foldBack (fun counter k v ->
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
            |> IntMap.ofArray

        (testMap, 0)
        ||> IntMap.foldBack (fun k v counter ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> assertEqual (IntMap.count testMap)

        elements
        |> ResizeArray.toArray
        |> assertEqual
            [| 114; 114; 112; 84; 106; 80; 107; 109; |]

[<Test>]
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

[<Test>]
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

[<Test>]
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
        |> assertEqual
            (IntMap.ofArray [| (5, 'a'); (2, 'd'); (17, 'a'); (12, 'b'); |])

        odds
        |> assertEqual
            (IntMap.ofArray [| (3, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |])


[<Test>]
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
            |> ExtCore.Collections.Array.mapPartition (fun (k, v) ->
                if (k + int v) % 2 = 0 then
                    Choice1Of2 (k, k + 10)
                else
                    Choice2Of2 (k, System.Char.ToUpper v))

        evens |> assertEqual (IntMap.ofArray evensExpected)

        odds |> assertEqual (IntMap.ofArray oddsExpected)


(* MapModule and MapType tests from the F# distribution. *)

module MapType =
    // Interfaces
    [<Test>]
    let IEnumerable() =        
        // Legit IE
        let ie = (IntMap.ofArray [|(1,1);(2,4);(3,9)|]) :> IEnumerable
        let enum = ie.GetEnumerator()
        
        let testStepping() =
            checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current :?> KeyValuePair<int,int>, KeyValuePair<int,int>(1,1))
            
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current :?> KeyValuePair<int,int>, KeyValuePair<int,int>(2,4))
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current :?> KeyValuePair<int,int>, KeyValuePair<int,int>(3,9))
            Assert.AreEqual(enum.MoveNext(), false)
            checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
    
        testStepping()
        enum.Reset()
        testStepping()
    
        // Empty IE
        let ie = [] |> IntMap.ofList :> IEnumerable  // Note no type args
        let enum = ie.GetEnumerator()
        
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        Assert.AreEqual(enum.MoveNext(), false)
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)  
    
    [<Test>]
    let IEnumerable_T() =        
        // Legit IE
        let ie = (IntMap.ofArray [|(1,1);(2,4);(3,9)|]) :> IEnumerable<KeyValuePair<_,_>>
        let enum = ie.GetEnumerator()
        
        let testStepping() =
            checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, new KeyValuePair<int,int>(1,1))
            
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, new KeyValuePair<int,int>(2,4))
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, new KeyValuePair<int,int>(3,9))
            Assert.AreEqual(enum.MoveNext(), false)
            checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
    
        testStepping()
        enum.Reset()
        testStepping()
    
        // Empty IE
        let ie = [] |> IntMap.ofList :> IEnumerable  // Note no type args
        let enum = ie.GetEnumerator()
        
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        Assert.AreEqual(enum.MoveNext(), false)
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)  
    
    
    [<Test>]
    let IDictionary() =        
        // Legit ID
        let id = (IntMap.ofArray [|(1,1);(2,4);(3,9)|]) :> IDictionary<_,_> 
        
        Assert.IsTrue(id.ContainsKey(1))   
        Assert.IsFalse(id.ContainsKey(5))  
        Assert.AreEqual(id.[1], 1)  
        Assert.AreEqual(id.[3], 9) 
        CollectionAssert.AreEqual(id.Keys,   [| 1; 2; 3|] :> ICollection<_>)
        CollectionAssert.AreEqual(id.Values, [| 1; 4; 9|] :> ICollection<_>)
        
        checkThrowsNotSupportedException(fun () -> id.[2] <-88)

        checkThrowsNotSupportedException(fun () -> id.Add(new KeyValuePair<int,int>(4,16)))
        Assert.IsTrue(id.TryGetValue(1, ref 1))
        Assert.IsFalse(id.TryGetValue(100, ref 1))
        checkThrowsNotSupportedException(fun () -> id.Remove(1) |> ignore)
        
        // Empty ID
        let id = IntMap.empty :> IDictionary<int, int>   // Note no type args  
        Assert.IsFalse(id.ContainsKey(5))
        checkThrowsKeyNotFoundException(fun () -> id.[1] |> ignore)  
        CollectionAssert.AreEqual(id.Keys,   [| |] :> ICollection<_>)
        CollectionAssert.AreEqual(id.Values, [| |] :> ICollection<_>) 
    
    [<Test>]
    let ICollection() =        
        // Legit IC
        let ic = (IntMap.ofArray [|(1,1);(2,4);(3,9)|]) :> ICollection<KeyValuePair<_,_>>
        
        Assert.AreEqual(ic.Count, 3)
        Assert.IsTrue(ic.Contains(new KeyValuePair<int,int>(3,9))) 
        let newArr = Array.create 5 (new KeyValuePair<int,int>(3,9))
        ic.CopyTo(newArr,0) 
        Assert.IsTrue(ic.IsReadOnly)
        
        
        // raise ReadOnlyCollection exception
        checkThrowsNotSupportedException(fun () -> ic.Add(new KeyValuePair<int,int>(3,9)) |> ignore)
        checkThrowsNotSupportedException(fun () -> ic.Clear() |> ignore)
        checkThrowsNotSupportedException(fun () -> ic.Remove(new KeyValuePair<int,int>(3,9)) |> ignore) 
        
            
        // Empty IC
        let ic = IntMap.empty :> ICollection<KeyValuePair<int, int>>   
        Assert.IsFalse(ic.Contains(new KeyValuePair<int,int>(3,9)))      
        let newArr = Array.create 5 (new KeyValuePair<int,int>(0,0))
        ic.CopyTo(newArr,0) 
    
    [<Test>]
    let IComparable() =        
        // Legit IC
        let ic = (IntMap.ofArray [|(1,1);(2,4);(3,9)|]) :> IComparable    
        Assert.AreEqual(ic.CompareTo([(1,1);(2,4);(3,9)]|> IntMap.ofList),0) 
        Assert.AreEqual(ic.CompareTo([(1,1);(3,9);(2,4)]|> IntMap.ofList),0) 
        Assert.AreEqual(ic.CompareTo([(1,1);(9,81);(2,4)]|> IntMap.ofList),-1) 
        Assert.AreEqual(ic.CompareTo([(1,1);(0,0);(2,4)]|> IntMap.ofList),1)      
        checkThrowsArgumentException(fun() -> ic.CompareTo([(1,1);(2,4);(3,9)]) |> ignore)
                  
        // Empty IC
        let ic = [] |> IntMap.ofList :> IComparable   
        Assert.AreEqual(ic.CompareTo([]|> IntMap.ofList),0)
    
    
    // Base class methods
    [<Test>]
    let ObjectGetHashCode() =
        // Works on empty maps
        let e = IntMap.ofList (List.empty<int * decimal>)
        let m = IntMap.ofList [ (1, -1.0M) ]
        Assert.AreNotEqual(e.GetHashCode(), m.GetHashCode())
        
        // Should be order independent
        let x = IntMap.ofList [(1, -1.0M); (2, -2.0M)]
        let y = IntMap.ofList [(2, -2.0M); (1, -1.0M)]
        Assert.AreEqual(x.GetHashCode(), y.GetHashCode())
    
    [<Test>]
    let ObjectToString() =
        Assert.AreEqual("intMap [(1, 1); (2, 4); (3, 9)]", (IntMap.ofArray [|(1,1);(2,4);(3,9)|]).ToString())
        Assert.AreEqual("intMap []", ([] |> IntMap.ofList).ToString())
    
    [<Test>]
    let ObjectEquals() =
        // All three are different references, but equality has been
        // provided by the F# compiler.
        let a = [(1,1);(2,4);(3,9)] |> IntMap.ofList
        let b = (1,1) :: [(2,4);(3,9)] |> IntMap.ofList
        Assert.IsTrue( (a = b) )

        Assert.IsTrue( a.Equals(b) ); Assert.IsTrue( b.Equals(a) )

        // Equality between types
        let a = ([] : (int*int) list)  |> IntMap.ofList
        let b = ([] : (int*string) list ) |> IntMap.ofList
        Assert.IsFalse( b.Equals(a) )
        Assert.IsFalse( a.Equals(b) )
        
        // Co/contra varience not supported
        let a = ([] : (int*string) list) |> IntMap.ofList
        let b = ([] : (int*System.IComparable) list)    |> IntMap.ofList
        Assert.IsFalse(a.Equals(b))
        Assert.IsFalse(b.Equals(a))
        
        // Self equality
        let a = [(1,1)] |> IntMap.ofList
        Assert.IsTrue( (a = a) )
        Assert.IsTrue(a.Equals(a))
        
        // Null
        Assert.IsFalse(a.Equals(null))

    // Instance methods
    [<Test>]
    let New() =    
        let newIntMap = new IntMap<int>([|(1,1);(2,4);(3,9)|])
        let b = newIntMap.Add(4,16)
        Assert.AreEqual(b.[4], 16)
        Assert.AreEqual(b.[2], 4)
    
        let e  = new  IntMap<string>([])
        let ae = e.Add(1,"Monday")
        Assert.AreEqual(ae.[1], "Monday")
        
    let Add() =
    
        let a = (IntMap.ofArray [|(1,1);(2,4);(3,9)|])
        let b = a.Add(4,16)
        Assert.AreEqual(b.[4], 16)
        Assert.AreEqual(b.[2], 4)
    
        let e  = IntMap.empty<string>
        let ae = e.Add(1,"Monday")
        Assert.AreEqual(ae.[1], "Monday")
    
    [<Test>]
    let ContainsKey() =
    
        let a = (IntMap.ofArray [|(1,1);(2,4);(3,9)|])        
        Assert.IsTrue(a.ContainsKey(3))
    
        let e  = IntMap.empty<string>
        Assert.IsFalse(e.ContainsKey(3)) 
    
    
    [<Test>]
    let Count() =
    
        let a = (IntMap.ofArray [|(1,1);(2,4);(3,9)|])
        Assert.AreEqual(a.Count, 3)
    
        let e  = IntMap.empty<string>
        Assert.AreEqual(e.Count, 0) 
    
    [<Test>]
    let IsEmpty() =
    
        let l = (IntMap.ofArray [|(1,1);(2,4);(3,9)|])
        Assert.IsFalse(l.IsEmpty)
    
        let e = IntMap.empty<int>
        Assert.IsTrue(e.IsEmpty)        
    
    [<Test>]
    let Item() =

        let mutable l = [(1,1)] |> IntMap.ofList
        Assert.AreEqual(l.[1], 1)
        l <- l.Add(100,8)
        Assert.AreEqual(l.[100], 8)
        
        for testidx = 0 to 20 do
            let l = IntMap.ofSeq (seq { for i in 0..testidx do yield (i,i*i)})
            for i = 0 to l.Count - 1 do
                Assert.AreEqual(i*i, l.[i])
                Assert.AreEqual(i*i, l.Item(i))
        
        // Invalid index
        let l = (IntMap.ofArray [|(1,1);(2,4);(3,9)|])
        checkThrowsKeyNotFoundException(fun () -> l.[ -1 ] |> ignore)
        checkThrowsKeyNotFoundException(fun () -> l.[1000] |> ignore)
    
    [<Test>]
    let Remove() =
    
        let l = (IntMap.ofArray [|(1,1);(2,4);(3,9)|])
        let rem = l.Remove(2)
        Assert.AreEqual(rem.Count, 2) 
        checkThrowsKeyNotFoundException(fun () -> rem.[ 2 ] |> ignore)
    
        let e  = IntMap.empty<string>
        let ae = e.Remove(2)
        Assert.AreEqual(ae.Count, 0)
        
    [<Test>]
    let TryFind() =
    
        let l = (IntMap.ofArray [|(1,1);(2,4);(3,9)|])
        let rem = l.TryFind(2)
        Assert.AreEqual(l.TryFind(2),Some 4)         
    
        let e  = IntMap.empty<string>
    
        Assert.AreEqual(e.TryFind(2), None)


module MapModule =
    [<Test>]
    let empty () : unit =
        let emptyMap = IntMap.empty        
        Assert.IsTrue(IntMap.isEmpty emptyMap)
        
        let a:IntMap<int>    = IntMap.empty<int>
        let c : IntMap<string> = IntMap.empty<string>  
              
        ()
        
    [<Test>]
    let add () : unit =
        // value keys
        let valueKeyMap = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = IntMap.add 1 "a" valueKeyMap        
        Assert.AreEqual(resultValueMap.[1], "a")
        
        // empty IntMap
        let eptMap = IntMap.empty
        let resultEpt = IntMap.add 1 "a" eptMap
        Assert.AreEqual(resultEpt.[1], "a")
        
        // One-element IntMap
        let oeleMap = IntMap.ofSeq [(1, "one")]
        let resultOele = IntMap.add  7  "seven" oeleMap
        Assert.AreEqual(resultOele.[7], "seven")     
        
        // extra test for add -- add some key which already exit in the IntMap
        let extMap = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultExt = IntMap.add 2 "dup" extMap        
        Assert.AreEqual(resultExt.[2], "dup")   
        
         
        ()

    [<Test>]
    let exists () : unit =
        // value keys
        let valueKeyMap = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = IntMap.exists (fun x y -> x > 3) valueKeyMap        
        Assert.IsTrue(resultValueMap)
        
        // One-element IntMap
        let oeleMap = IntMap.ofSeq [(1, "one")]
        let resultOele = oeleMap |> IntMap.exists  (fun x y -> (x + y.Length) % 4 = 0 ) 
        Assert.IsTrue(resultOele)
        
        // empty IntMap
        let eptMap = IntMap.empty
        let resultEpt = IntMap.exists (fun x y -> false) eptMap
        Assert.IsFalse(resultEpt)
       
        ()
        
    [<Test>]
    let filter () : unit =
        // value keys
        let valueKeyMap = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap =valueKeyMap |> IntMap.filter (fun x y -> x % 3 = 0)         
        Assert.AreEqual(resultValueMap,[3,"c"] |> IntMap.ofList)
        
        // One-element IntMap
        let oeleMap = IntMap.ofSeq [(1, "one")]
        let resultOele = oeleMap |> IntMap.filter  (fun x y -> x<3 )         
        Assert.AreEqual(resultOele,oeleMap)
        
        // empty IntMap
        let eptMap = IntMap.empty
        let resultEpt = IntMap.filter (fun x y -> true) eptMap        
        Assert.AreEqual(resultEpt,eptMap)
               
        ()       


    [<Test>]
    let find () : unit =
        // value keys
        let valueKeyMap = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = IntMap.find 5 valueKeyMap        
        Assert.AreEqual(resultValueMap,"e")
        
        // One-element IntMap
        let oeleMap = IntMap.ofSeq [(1, "one")]
        let resultOele = IntMap.find 1 oeleMap        
        Assert.AreEqual(resultOele,"one")
        
        // empty IntMap
        let eptMap = IntMap.empty
        checkThrowsKeyNotFoundException (fun () -> IntMap.find 1 eptMap |> ignore)
               
        ()  

    [<Test>]
    let findIndex () : unit =
        // value keys
        let valueKeyMap = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap =valueKeyMap |> IntMap.findKey (fun x y -> x % 3 = 0)         
        Assert.AreEqual(resultValueMap,3)
        
        // One-element IntMap
        let oeleMap = IntMap.ofSeq [(1, "one")]
        let resultOele = oeleMap |> IntMap.findKey  (fun x y -> x = 1 )         
        Assert.AreEqual(resultOele,1)
        
        // empty IntMap
        let eptMap = IntMap.empty
        checkThrowsKeyNotFoundException (fun () -> IntMap.findKey (fun x y -> true) eptMap |> ignore)
               
        ()          
     
    [<Test>]
    let tryPick () : unit =
        // value keys
        let valueKeyMap = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = valueKeyMap |> IntMap.tryPick (fun x y -> if x % 3 = 0 then Some (x) else None)         
        Assert.AreEqual(resultValueMap,Some 3)
        
        // One-element IntMap
        let oeleMap = IntMap.ofSeq [(1, "one")]
        let resultOele = oeleMap |> IntMap.tryPick  (fun x y -> if(x + y.Length) % 4 = 0 then Some y else None )         
        Assert.AreEqual(resultOele,Some "one")
        
        // empty IntMap
        let eptMap = IntMap.empty
        let resultEpt = IntMap.tryPick (fun x y -> Some x) eptMap        
        Assert.AreEqual(resultEpt,None)
               
        ()     

    [<Test>]
    let pick () : unit =
        // value keys
        let valueKeyMap = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValue = valueKeyMap |> IntMap.pick (fun x y -> if x % 3 = 0 then Some (y) else None)         
        Assert.AreEqual(resultValue, "c")
        
        // One-element IntMap
        let oeleMap = IntMap.ofSeq [(1, "one")]
        let resultOele = oeleMap |> IntMap.pick (fun x y -> if(x + y.Length) % 4 = 0 then Some y else None )         
        Assert.AreEqual(resultOele, "one")
        
        // empty IntMap
        let eptMap = IntMap.empty
        let resultEpt = 
            try 
                IntMap.pick (fun x y -> Some x) eptMap
            with :? System.Collections.Generic.KeyNotFoundException -> 0
        Assert.AreEqual(resultEpt, 0)
        
        ()

    [<Test>]
    let fold () : unit =
        // value keys
        let valueKeyMap = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = valueKeyMap |> IntMap.fold (fun x y z -> x + y + z.Length) 10         
        Assert.AreEqual(resultValueMap,28)
        
        // One-element IntMap
        let oeleMap = IntMap.ofSeq [(1, "one")]
        let resultOele =   oeleMap |> IntMap.fold  (fun x y z -> x + y.ToString() + z)  "got"      
        Assert.AreEqual(resultOele,"got1one")
        
        // empty IntMap
        let eptMap = IntMap.empty
        let resultEpt = eptMap |> IntMap.fold (fun x y z -> 1) 1         
        Assert.AreEqual(resultEpt,1)
               
        ()

    [<Test>]
    let foldBack () : unit =
        // value keys
        let valueKeyMap = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = IntMap.foldBack (fun x y z -> x.ToString() + y + z.ToString()) valueKeyMap "*"     
        Assert.AreEqual(resultValueMap,"2b3c4d5e*")
        
        // One-element IntMap
        let oeleMap = IntMap.ofSeq [(1, "one")]
        let resultOele = IntMap.foldBack  (fun x y z -> x.ToString() + y + z) oeleMap "right"         
        Assert.AreEqual(resultOele,"1oneright")
        
        // empty IntMap
        let eptMap = IntMap.empty
        let resultEpt = IntMap.foldBack (fun x y z -> 1) eptMap 1         
        Assert.AreEqual(resultEpt,1)
               
        ()
        
    [<Test>]
    let forall () : unit =
        // value keys
        let valueKeyMap = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = valueKeyMap |> IntMap.forall (fun x y -> x % 3 = 0)         
        Assert.IsFalse(resultValueMap)
        
        // One-element IntMap
        let oeleMap = IntMap.ofSeq [(1, "one")]
        let resultOele = oeleMap |> IntMap.forall  (fun x y -> x<3 )         
        Assert.IsTrue(resultOele)
        
        // empty IntMap
        let eptMap = IntMap.empty
        let resultEpt =eptMap |>  IntMap.forall (fun x y -> true)         
        Assert.IsTrue(resultEpt)
               
        ()       


    [<Test>]
    let isEmpty () : unit =
        // value keys
        let valueKeyMap = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = IntMap.isEmpty  valueKeyMap        
        Assert.IsFalse(resultValueMap)
        
        // One-element IntMap
        let oeleMap = IntMap.ofSeq [(1, "one")]
        let resultOele = IntMap.isEmpty   oeleMap        
        Assert.IsFalse(resultOele)
        
        // empty IntMap
        let eptMap = IntMap.empty
        let resultEpt = IntMap.isEmpty  eptMap        
        Assert.IsTrue(resultEpt)
               
        ()  

    [<Test>]
    let iter () : unit =
        // value keys
        let valueKeyMap = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = ref 0    
        let funInt (x:int) (y:string) =   
            resultValueMap := !resultValueMap + x + y.Length             
            () 
        IntMap.iter funInt valueKeyMap        
        Assert.AreEqual(!resultValueMap,18)
        
        // One-element IntMap
        let oeleMap = IntMap.ofSeq [(1, "one")]
        let resultOele = ref ""
        let funMix  (x:int) (y:string) =
            resultOele := !resultOele + x.ToString() + y
            ()
        IntMap.iter funMix oeleMap        
        Assert.AreEqual(!resultOele,"1one")
        
        // empty IntMap
        let eptMap = IntMap.empty
        let resultEpt = ref 0    
        let funEpt (x:int) (y:int) =   
            resultEpt := !resultEpt + x + y              
            () 
        IntMap.iter funEpt eptMap        
        Assert.AreEqual(!resultEpt,0)
               
        ()          
     
    [<Test>]
    let map () : unit =

        // value keys
        let valueKeyMap = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = valueKeyMap |> IntMap.map (fun x y -> x.ToString() + y )         
        Assert.AreEqual(resultValueMap,[(2,"2b"); (3,"3c"); (4,"4d"); (5,"5e")] |> IntMap.ofList)
        
        // One-element IntMap
        let oeleMap = IntMap.ofSeq [(1, "one")]
        let resultOele = oeleMap |> IntMap.map  (fun x y -> x.ToString() + y )         
        Assert.AreEqual(resultOele,[1,"1one"] |> IntMap.ofList)
        
        // empty IntMap
        let eptMap = IntMap.empty<int>
        let resultEpt = eptMap |> IntMap.map (fun x y -> x+y)         
        Assert.AreEqual(resultEpt,eptMap)
               
        ()     

    [<Test>]
    let contains () : unit =
        // value keys
        let valueKeyMap = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = IntMap.containsKey 2 valueKeyMap        
        Assert.IsTrue(resultValueMap)
        
        // One-element IntMap
        let oeleMap = IntMap.ofSeq [(1, "one")]
        let resultOele = IntMap.containsKey 1 oeleMap        
        Assert.IsTrue(resultOele)
        
        // empty IntMap
        let eptMap = IntMap.empty
        let resultEpt = IntMap.containsKey 3 eptMap        
        Assert.IsFalse(resultEpt)
               
        () 
        
    [<Test>]
    let ofArray_ofList_ofSeq () : unit =
        // value keys    
        let valueKeyMapOfArr = IntMap.ofArray [|(2,"b"); (3,"c"); (4,"d"); (5,"e")|]     
        let valueKeyMapOfList = IntMap.ofList [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let valueKeyMapOfSeq = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]

        CollectionAssert.AreEqual(valueKeyMapOfArr,valueKeyMapOfList)
        CollectionAssert.AreEqual(valueKeyMapOfList,valueKeyMapOfSeq)
        CollectionAssert.AreEqual(valueKeyMapOfArr,valueKeyMapOfSeq)
        
        // One-element IntMap
        let oeleMapOfArr = IntMap.ofArray [|(1,"one")|]
        let oeleMapOfList = IntMap.ofList [(1,"one") ]
        let oeleMapOfSeq = IntMap.ofSeq [(1,"one") ]

        CollectionAssert.AreEqual(oeleMapOfArr,oeleMapOfList)
        CollectionAssert.AreEqual(oeleMapOfList,oeleMapOfSeq)
        CollectionAssert.AreEqual(oeleMapOfArr,oeleMapOfSeq)
                
        ()

    [<Test>]
    let partition () : unit =
        // value keys
        let valueKeyMap = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = IntMap.partition (fun x y  -> x%2 = 0) valueKeyMap         
        let choosed = [(2,"b"); (4,"d")] |> IntMap.ofList
        let notChoosed = [(3,"c"); (5,"e")] |> IntMap.ofList
        Assert.AreEqual(resultValueMap,(choosed,notChoosed))
        
        // One-element IntMap
        let oeleMap = IntMap.ofSeq [(1, "one")]
        let resultOele = IntMap.partition  (fun x y  -> x<4) oeleMap     
        let choosed = [(1,"one")] |> IntMap.ofList
        let notChoosed = IntMap.empty<string>
        Assert.AreEqual(resultOele,(choosed,notChoosed))
        
        // empty IntMap
        let eptMap = IntMap.empty
        let resultEpt = IntMap.partition (fun x y  -> true) eptMap          
        Assert.AreEqual(resultEpt,(eptMap,eptMap))
               
        ()
    
        
    [<Test>]
    let remove () : unit =
        // value keys
        let valueKeyMap = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = IntMap.remove 5 valueKeyMap        
        Assert.AreEqual(resultValueMap,[(2,"b"); (3,"c"); (4,"d")] |> IntMap.ofList)
        
        // One-element IntMap
        let oeleMap = IntMap.ofSeq [(1, "one")]
        let resultOele = IntMap.remove  1 oeleMap             
        Assert.AreEqual(resultOele,IntMap.empty<string>)
        
        // Two-element IntMap
        let oeleMap = IntMap.ofSeq [(1, "one");(2,"Two")]
        let resultOele = IntMap.remove  1 oeleMap             
        let exOele = IntMap.ofSeq [(2, "Two")]
        Assert.AreEqual(resultOele, exOele)
        
        // Item which want to be removed not included in the map
        let valueKeyMap = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = IntMap.remove 5 valueKeyMap        
        Assert.AreEqual(resultValueMap,[(2,"b"); (3,"c"); (4,"d")] |> IntMap.ofList)
        
                               
        ()
        
    [<Test>]
    let toArray () : unit =
        // value keys    
        let valueKeyMapOfArr = IntMap.ofArray [|(1,1);(2,4);(3,9)|]     
        let resultValueMap = IntMap.toArray valueKeyMapOfArr        
        Assert.AreEqual(resultValueMap,[|(1,1);(2,4);(3,9)|])
        
        // One-element IntMap
        let oeleMapOfArr = IntMap.ofArray [|(1,"one")|]
        let resultOele = IntMap.toArray oeleMapOfArr        
        Assert.AreEqual(resultOele,[|(1,"one")|])
        
        // empty IntMap
        let eptMap = IntMap.ofArray [||]
        let resultEpt = IntMap.toArray eptMap            
        Assert.AreEqual(resultEpt,[||])

        () 

    [<Test>]
    let toList () : unit =
        // value keys    
        let valueKeyMapOfArr = IntMap.ofList [(1,1);(2,4);(3,9)]     
        let resultValueMap = IntMap.toList valueKeyMapOfArr        
        Assert.AreEqual(resultValueMap,[(1,1);(2,4);(3,9)])
        
        // One-element IntMap
        let oeleMapOfArr = IntMap.ofList [(1,"one")]
        let resultOele = IntMap.toList oeleMapOfArr        
        Assert.AreEqual(resultOele,[(1,"one")])
        
        // empty IntMap
        let eptMap = IntMap.empty<string>
        let resultEpt = IntMap.toList eptMap  
        let eptList :(int*string) list = []          
        Assert.AreEqual(resultEpt,eptList)

        ()     

    [<Test>]
    let toSeq () : unit =
        // value keys    
        let valueKeyMapOfArr = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]  
        let resultValueMap = IntMap.toSeq valueKeyMapOfArr
        let originInt = seq { for i in 1..3 do yield (i,i*i)}
        CollectionAssert.AreEqual (
            resultValueMap,
            [(2,"b"); (3,"c"); (4,"d"); (5,"e")] :> IEnumerable)

        
        // One-element IntMap
        let oeleMapOfArr = IntMap.ofSeq [(1,"one")]
        let resultOele = IntMap.toSeq oeleMapOfArr
        let originMix = seq { for x in [ "is" ;"str"; "this" ;"lists"] do yield (x.Length,x.ToUpper())}
        CollectionAssert.AreEqual (resultOele, [(1,"one")])

         
        ()           

    [<Test>]
    let tryFind () : unit =
        // value keys
        let valueKeyMap = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = IntMap.tryFind 5 valueKeyMap        
        Assert.AreEqual(resultValueMap,Some "e")
        
        // One-element IntMap
        let oeleMap = IntMap.ofSeq [(1, "one")]
        let resultOele = IntMap.tryFind 1 oeleMap        
        Assert.AreEqual(resultOele,Some "one")
        
        // empty IntMap
        let eptMap = IntMap.empty
        let resultEpt = IntMap.tryFind 1 eptMap        
        Assert.AreEqual(resultEpt,None)
               
        ()      

    [<Test>]
    let tryFindIndex () : unit =
        // value keys
        let valueKeyMap = IntMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = valueKeyMap |> IntMap.tryFindKey (fun x y -> x+y.Length >30)         
        Assert.AreEqual(resultValueMap,None)
        
        // One-element IntMap
        let oeleMap = IntMap.ofSeq [(1, "one")]
        let resultOele = oeleMap |> IntMap.tryFindKey (fun x y -> y.Contains("o"))        
        Assert.AreEqual(resultOele,Some 1)
        
        // empty IntMap
        let eptMap = IntMap.empty
        let resultEpt = IntMap.tryFindKey (fun x y -> x+y >30) eptMap        
        Assert.AreEqual(resultEpt,None)
               
        ()  


/// FsCheck-based tests for IntMap.
module FsCheck =
    open FsCheck

    /// FsCheck generators for IntMap.
    type IntMapGenerator =
        /// Generates an arbitrary IntMap instance.
        static member IntMap () : Arbitrary<IntMap<'T>> =
            gen {
                let! keys = Arb.generate
                let! values = Arb.generate
            
                // It seems FsCheck requires the use of sequences here --
                // using List.fold2 to build the IntMap causes FsCheck to crash.
                let kvpSeq = (Seq.ofList keys, Seq.ofList values) ||> Seq.zip
                return IntMap.ofSeq kvpSeq
            } |> Arb.fromGen

    /// Registers the FsCheck generators so they're already loaded
    /// when NUnit runs the tests in this fixture.
    [<TestFixtureSetUp>]
    let registerFsCheckGenerators =
        Arb.register<IntMapGenerator> () |> ignore


    (* Tests *)

    [<Test>]
    let ``prop addLookup``() =
        assertProp "addLookup" <| fun k v map ->
            IntMap.add k v map
            |> IntMap.containsKey k

