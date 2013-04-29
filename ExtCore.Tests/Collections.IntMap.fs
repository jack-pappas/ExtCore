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
module Tests.ExtCore.Collections.IntMap

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
