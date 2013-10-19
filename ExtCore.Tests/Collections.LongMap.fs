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

/// Unit tests for the ExtCore.Collections.LongMap type and module.
module Tests.ExtCore.Collections.LongMap

open System
open System.Collections
open System.Collections.Generic
open NUnit.Framework


[<Test>]
let isEmpty () : unit =
    LongMap.empty
    |> LongMap.isEmpty
    |> assertTrue

    LongMap.singleton 1L 'a'
    |> LongMap.isEmpty
    |> assertFalse

[<Test>]
let count () : unit =
    LongMap.empty
    |> LongMap.count
    |> assertEqual 0L

    LongMap.singleton 1L 'a'
    |> LongMap.count
    |> assertEqual 1L

    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd');
        (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); |]
    |> LongMap.ofArray
    |> LongMap.count
    |> assertEqual 8L

[<Test>]
let singleton () : unit =
    LongMap.singleton 1L 'a'
    |> assertEqual (
        LongMap.add 1L 'a' LongMap.empty)

    LongMap.singleton 1L 'a'
    |> LongMap.count
    |> assertEqual 1L

[<Test>]
let containsKey () : unit =
    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd');
        (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); |]
    |> LongMap.ofArray
    |> LongMap.containsKey 5L
    |> assertTrue

    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd');
        (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); |]
    |> LongMap.ofArray
    |> LongMap.containsKey 1L
    |> assertFalse

[<Test>]
let tryFind () : unit =
    [(5L, 'a'); (3L, 'b')]
    |> LongMap.ofList
    |> LongMap.tryFind 5L
    |> assertEqual (Some 'a')

    [(5L, 'a'); (3L, 'b')]
    |> LongMap.ofList
    |> LongMap.tryFind 7L
    |> assertEqual None

[<Test>]
let find () : unit =
    [(5L, 'a'); (3L, 'b')]
    |> LongMap.ofList
    |> LongMap.find 5L
    |> assertEqual 'a'

[<Test; ExpectedException(typeof<KeyNotFoundException>)>]
let ``find raises exn when key is not found`` () : unit =
    [(5L, 'a'); (3L, 'b')]
    |> LongMap.ofList
    |> LongMap.find 9L
    |> ignore

[<Test>]
let findOrDefault () : unit =
    [(5L, 'a'); (3L, 'b')]
    |> LongMap.ofList
    |> LongMap.findOrDefault 'z' 5L
    |> assertEqual 'a'

    [(5L, 'a'); (3L, 'b')]
    |> LongMap.ofList
    |> LongMap.findOrDefault 'z' 7L
    |> assertEqual 'z'

[<Test>]
let tryFindKey () : unit =
    (LongMap.empty : LongMap<string>)
    |> LongMap.tryFindKey (fun k v ->
        k % 2L = 0L)
    |> assertEqual None

    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd');
        (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); |]
    |> LongMap.ofArray
    |> LongMap.tryFindKey (fun k v ->
        (k + int64 v) % 11L = 0L)
    |> assertEqual (Some 12L)

    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd');
        (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); |]
    |> LongMap.ofArray
    |> LongMap.tryFindKey (fun k v ->
        (k + int64 v) % 289L = 0L)
    |> assertEqual None

[<Test>]
let add () : unit =
    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd');
        (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); |]
    |> LongMap.ofArray
    |> LongMap.add 5L 'x'
    |> assertEqual (LongMap.ofArray
       [| (5L, 'x'); (3L, 'b'); (11L, 'f'); (2L, 'd');
          (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); |])

    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd');
        (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); |]
    |> LongMap.ofArray
    |> LongMap.add 13L 'x'
    |> assertEqual (LongMap.ofArray
       [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd');
          (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (13L, 'x'); |])

    LongMap.empty
    |> LongMap.add 5L 'x'
    |> assertEqual (
        LongMap.singleton 5L 'x')

[<Test>]
let remove () : unit =
    (LongMap.empty : LongMap<string>)
    |> LongMap.remove 5L
    |> LongMap.isEmpty
    |> assertTrue

    [(5L, "a"); (3L, "b")]
    |> LongMap.ofList
    |> LongMap.remove 5L
    |> assertEqual (
        LongMap.singleton 3L "b")

    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd');
        (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); |]
    |> LongMap.ofArray
    |> LongMap.remove 4L
    |> assertEqual (LongMap.ofArray
       [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd');
          (17L, 'a'); (12L, 'b'); (14L, 'c'); |])

[<Test>]
let union () : unit =
    LongMap.union
        (LongMap.ofArray [| (3L, 'b'); (11L, 'F'); (2L, 'd'); (4L, 'G'); (12L, 'b'); |])
        (LongMap.ofArray [| (5L, 'a'); (11L, 'f'); (17L, 'a'); (4L, 'g'); (14L, 'c'); |])
    |> assertEqual
        (LongMap.ofArray [| (5L, 'a'); (3L, 'b'); (11L, 'F'); (2L, 'd'); (17L, 'a'); (4L, 'G'); (12L, 'b'); (14L, 'c'); |])

[<Test>]
let intersect () : unit =
    LongMap.intersect
        (LongMap.ofArray [| (3L, 'b'); (11L, 'F'); (2L, 'd'); (4L, 'G'); (12L, 'b'); |])
        LongMap.empty
    |> LongMap.isEmpty
    |> assertTrue

    LongMap.intersect
        (LongMap.ofArray [| (3L, 'b'); (11L, 'F'); (2L, 'd'); (4L, 'G'); (12L, 'b'); |])
        (LongMap.ofArray [| (5L, 'a'); (11L, 'f'); (17L, 'a'); (4L, 'g'); (14L, 'c'); |])
    |> assertEqual
        (LongMap.ofArray [| (11L, 'F'); (4L, 'G'); |])

[<Test>]
let difference () : unit =
    LongMap.difference
        (LongMap.ofArray [| (5L, 'a'); (3L, 'b'); (11L, 'F'); (2L, 'd'); (17L, 'a'); (4L, 'G'); (12L, 'b'); (14L, 'c'); |])
        LongMap.empty
    |> assertEqual
        (LongMap.ofArray [| (5L, 'a'); (3L, 'b'); (11L, 'F'); (2L, 'd'); (17L, 'a'); (4L, 'G'); (12L, 'b'); (14L, 'c'); |])

    LongMap.difference
        (LongMap.ofArray [| (5L, 'a'); (3L, 'b'); (11L, 'F'); (2L, 'd'); (17L, 'a'); (4L, 'G'); (12L, 'b'); (14L, 'c'); |])
        (LongMap.ofArray [| (3L, 'b'); (11L, 'f'); (2L, 'd'); (4L, 'g'); (12L, 'b'); |])
    |> assertEqual
        (LongMap.ofArray [| (5L, 'a'); (17L, 'a'); (14L, 'c'); |])

[<Test>]
let isSubmapOfBy () : unit =
    let map1 = LongMap.ofList [(1L,1);(2L,2)]
        
    map1.IsSubmapOfBy ((=),
        LongMap.ofList [(1L,1)])
    |> assertTrue

    map1.IsSubmapOfBy ((<=),
        LongMap.ofList [(1L,1)])
    |> assertTrue

    map1.IsSubmapOfBy ((=),
        LongMap.ofList [(1L,1);(2L,2)])
    |> assertTrue

    map1.IsSubmapOfBy ((=),
        LongMap.ofList [(1L,2)])
    |> assertFalse

    map1.IsSubmapOfBy ((<),
        LongMap.ofList [(1L,1)])
    |> assertFalse

    let map2 = LongMap.ofList [(1L,1)]

    map2.IsSubmapOfBy ((=),
        LongMap.ofList [(1L,1);(2L,2)])
    |> assertFalse


[<Test>]
let ofSeq () : unit =
    (Seq.empty : seq<int64 * string>)
    |> LongMap.ofSeq
    |> LongMap.isEmpty
    |> assertTrue

    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
    |> Seq.ofArray
    |> LongMap.ofSeq
    |> assertEqual
        (LongMap.ofArray [| (5L, 'a'); (3L, 'b'); (11L, 'F'); (2L, 'd'); (17L, 'a'); (4L, 'G'); (12L, 'b'); (14L, 'c'); |])

[<Test>]
let ofList () : unit =
    LongMap.ofList []
    |> LongMap.isEmpty
    |> assertTrue

    [(5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G')]
    |> LongMap.ofList
    |> assertEqual
        (LongMap.ofArray [| (5L, 'a'); (3L, 'b'); (11L, 'F'); (2L, 'd'); (17L, 'a'); (4L, 'G'); (12L, 'b'); (14L, 'c'); |])

[<Test>]
let ofArray () : unit =
    Array.empty
    |> LongMap.ofArray
    |> LongMap.isEmpty
    |> assertTrue

    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
    |> LongMap.ofArray
    |> assertEqual
        (LongMap.ofArray [| (5L, 'a'); (3L, 'b'); (11L, 'F'); (2L, 'd'); (17L, 'a'); (4L, 'G'); (12L, 'b'); (14L, 'c'); |])

[<Test>]
let ofMap () : unit =
    Map.empty
    |> LongMap.ofMap
    |> LongMap.isEmpty
    |> assertTrue

    [| (5L, 'a'); (3L, 'b'); (11L, 'F'); (2L, 'd'); (17L, 'a'); (4L, 'G'); (12L, 'b'); (14L, 'c'); |]
    |> Map.ofArray
    |> LongMap.ofMap
    |> assertEqual
        (LongMap.ofArray [| (5L, 'a'); (3L, 'b'); (11L, 'F'); (2L, 'd'); (17L, 'a'); (4L, 'G'); (12L, 'b'); (14L, 'c'); |])

[<Test>]
let toSeq () : unit =
    LongMap.empty
    |> LongMap.toSeq
    |> Seq.isEmpty
    |> assertTrue

    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
    |> LongMap.ofArray
    |> LongMap.toSeq
    |> Seq.toArray
    |> assertEqual
        [| (2L, 'd'); (3L, 'b'); (4L, 'G'); (5L, 'a'); (11L, 'F'); (12L, 'b'); (14L, 'c'); (17L, 'a'); |]

[<Test>]
let toList () : unit =
    LongMap.empty
    |> LongMap.toList
    |> List.isEmpty
    |> assertTrue

    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
    |> LongMap.ofArray
    |> LongMap.toList
    |> assertEqual
        [(2L, 'd'); (3L, 'b'); (4L, 'G'); (5L, 'a'); (11L, 'F'); (12L, 'b'); (14L, 'c'); (17L, 'a')]

[<Test>]
let toArray () : unit =
    LongMap.empty
    |> LongMap.toArray
    |> Array.isEmpty
    |> assertTrue

    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
    |> LongMap.ofArray
    |> LongMap.toArray
    |> assertEqual
        [| (2L, 'd'); (3L, 'b'); (4L, 'G'); (5L, 'a'); (11L, 'F'); (12L, 'b'); (14L, 'c'); (17L, 'a'); |]

[<Test>]
let toMap () : unit =
    LongMap.empty
    |> LongMap.toMap
    |> Map.isEmpty
    |> assertTrue

    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
    |> LongMap.ofArray
    |> LongMap.toMap
    |> assertEqual
        (Map.ofArray [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |])

[<Test>]
let tryPick () : unit =
    // Test case for empty input.
    LongMap.empty
    |> LongMap.tryPick (fun k v ->
        if (k + v) = 10L then
            Some 11
        else None)
    |> assertEqual None

    // Test case where no elements match the 'picker' function.
    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
    |> LongMap.ofArray
    |> LongMap.tryPick (fun k v ->
        if System.Char.IsControl v then
            Some (int64 v + k)
        else None)
    |> assertEqual None

    // Test case where a single binding matches the 'picker' function.
    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
    |> LongMap.ofArray
    |> LongMap.tryPick (fun k v ->
        if (k % 7L = 0L) && v = 'c' then
            Some (k - 2L)
        else None)
    |> assertEqual (Some 12L)

    // Test case where multiple bindings match the 'picker' function.
    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
    |> LongMap.ofArray
    |> LongMap.tryPick (fun k v ->
        if k % 3L = 2L then
            Some v
        else None)
    |> assertEqual (Some 'd')

[<Test>]
let pick () : unit =
    // Test case where a single binding matches the 'picker' function.
    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
    |> LongMap.ofArray
    |> LongMap.pick (fun k v ->
        if (k % 7L = 0L) && v = 'c' then
            Some (k - 2L)
        else None)
    |> assertEqual 12L

    // Test case where multiple bindings match the 'picker' function.
    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
    |> LongMap.ofArray
    |> LongMap.pick (fun k v ->
        if k % 3L = 2L then
            Some v
        else None)
    |> assertEqual 'd'

[<Test; ExpectedException(typeof<KeyNotFoundException>)>]
let ``pick raises exn on empty input`` () : unit =
    LongMap.empty
    |> LongMap.pick (fun k v ->
        if (k + v) % 2L = 0L then
            Some (v + 1L)
        else None)
    |> ignore

[<Test; ExpectedException(typeof<KeyNotFoundException>)>]
let ``pick raises exn when no match is found`` () : unit =
    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
    |> LongMap.ofArray
    |> LongMap.pick (fun k v ->
        if System.Char.IsControl v then
            Some (int64 v + k)
        else None)
    |> ignore

[<Test>]
let map () : unit =
    LongMap.empty
    |> LongMap.map (sprintf "%i:%c")
    |> LongMap.isEmpty
    |> assertTrue

    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); |]
    |> LongMap.ofArray
    |> LongMap.map (sprintf "%i:%c")
    |> assertEqual (LongMap.ofArray
        [| (5L, "5:a"); (3L, "3:b"); (11L, "11:f"); (2L, "2:d");
            (17L, "17:a"); (4L, "4:g"); (12L, "12:b"); (14L, "14:c"); |])

[<Test>]
let filter () : unit =
    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
    |> LongMap.ofArray
    |> LongMap.filter (fun k v ->
        (k % 2L = 0L) && System.Char.IsLower v)
    |> assertEqual
        (LongMap.ofArray [| (2L, 'd'); (12L, 'b'); (14L, 'c'); |])

[<Test>]
let choose () : unit =
    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
    |> LongMap.ofArray
    |> LongMap.choose (fun k v ->
        if k % 2L <> 0L then
            Some (System.Char.ToUpper v)
        else None)
    |> assertEqual
        (LongMap.ofArray [| (5L, 'A'); (3L, 'B'); (17L, 'A'); (11L, 'F'); |])

[<Test>]
let iter () : unit =
    do
        let elements = ResizeArray ()

        LongMap.empty
        |> LongMap.iter (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.isEmpty
        |> assertTrue

    do
        let elements = ResizeArray ()

        [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
        |> LongMap.ofArray
        |> LongMap.iter (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.toArray
        |> assertEqual
            [| 'D'; 'B'; 'G'; 'A'; 'F'; 'B'; 'C'; 'A'; |]

[<Test>]
let iterBack () : unit =
    do
        let elements = ResizeArray ()

        LongMap.empty
        |> LongMap.iter (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.isEmpty
        |> assertTrue

    do
        let elements = ResizeArray ()

        [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
        |> LongMap.ofArray
        |> LongMap.iterBack (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.toArray
        |> assertEqual
            [| 'A'; 'C'; 'B'; 'F'; 'A'; 'G'; 'B'; 'D'; |]

[<Test>]
let fold () : unit =
    do
        let elements = ResizeArray ()

        (0L, LongMap.empty)
        ||> LongMap.fold (fun counter k v ->
            elements.Add (counter + k + int64 v)
            counter + 1L)
        |> assertEqual 0L

        elements
        |> ResizeArray.isEmpty
        |> assertTrue

    do
        let elements = ResizeArray ()

        let testMap =
            [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
            |> LongMap.ofArray

        (0L, testMap)
        ||> LongMap.fold (fun counter k v ->
            elements.Add (counter + k + int64 v)
            counter + 1L)
        |> assertEqual (LongMap.count testMap)

        elements
        |> ResizeArray.toArray
        |> assertEqual
            [| 102L; 102L; 77L; 105L; 85L; 115L; 119L; 121L; |]

[<Test>]
let foldBack () : unit =
    do
        let elements = ResizeArray ()

        (LongMap.empty, 0L)
        ||> LongMap.foldBack (fun counter k v ->
            elements.Add (counter + k + int64 v)
            counter + 1L)
        |> assertEqual 0L

        elements
        |> ResizeArray.isEmpty
        |> assertTrue

    do
        let elements = ResizeArray ()

        let testMap =
            [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
            |> LongMap.ofArray

        (testMap, 0L)
        ||> LongMap.foldBack (fun k v counter ->
            elements.Add (counter + k + int64 v)
            counter + 1L)
        |> assertEqual (LongMap.count testMap)

        elements
        |> ResizeArray.toArray
        |> assertEqual
            [| 114L; 114L; 112L; 84L; 106L; 80L; 107L; 109L; |]

[<Test>]
let exists () : unit =
    LongMap.empty
    |> LongMap.exists (fun k v ->
        (k + int64 v) % 2L = 0L)
    |> assertFalse

    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
    |> LongMap.ofArray
    |> LongMap.exists (fun k v ->
        (k + int64 v) > 200L)
    |> assertFalse

    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
    |> LongMap.ofArray
    |> LongMap.exists (fun k v ->
        (k + int64 v) % 2L = 0L)
    |> assertTrue

[<Test>]
let forall () : unit =
    LongMap.empty
    |> LongMap.forall (fun k v ->
        (k + int64 v) < 200L)
    |> assertTrue

    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
    |> LongMap.ofArray
    |> LongMap.forall (fun k v ->
        (k + int64 v) < 200L)
    |> assertTrue

    [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
    |> LongMap.ofArray
    |> LongMap.forall (fun k v ->
        (k + int64 v) % 2L = 0L)
    |> assertFalse

[<Test>]
let partition () : unit =
    do
        let evens, odds =
            LongMap.empty
            |> LongMap.partition (fun k v ->
                (k + int64 v) % 2L = 0L)

        evens
        |> LongMap.isEmpty
        |> assertTrue

        odds
        |> LongMap.isEmpty
        |> assertTrue

    do
        let evens, odds =
            [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
            |> LongMap.ofArray
            |> LongMap.partition (fun k v ->
                (k + int64 v) % 2L = 0L)

        evens
        |> assertEqual
            (LongMap.ofArray [| (5L, 'a'); (2L, 'd'); (17L, 'a'); (12L, 'b'); |])

        odds
        |> assertEqual
            (LongMap.ofArray [| (3L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |])


[<Test>]
let mapPartition () : unit =
    do
        let evens, odds =
            LongMap.empty
            |> LongMap.mapPartition (fun k v ->
                if (k + int64 v) % 2L = 0L then
                    Choice1Of2 (k + 10L)
                else
                    Choice2Of2 (System.Char.ToUpper v))

        evens
        |> LongMap.isEmpty
        |> assertTrue

        odds
        |> LongMap.isEmpty
        |> assertTrue

    do
        let evens, odds =
            [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
            |> LongMap.ofArray
            |> LongMap.mapPartition (fun k v ->
                if (k + int64 v) % 2L = 0L then
                    Choice1Of2 (k + 10L)
                else
                    Choice2Of2 (System.Char.ToUpper v))

        let evensExpected, oddsExpected =
            [| (5L, 'a'); (3L, 'b'); (11L, 'f'); (2L, 'd'); (17L, 'a'); (4L, 'g'); (12L, 'b'); (14L, 'c'); (11L, 'F'); (4L, 'G'); |]
            |> ExtCore.Collections.Array.mapPartition (fun (k, v) ->
                if (k + int64 v) % 2L = 0L then
                    Choice1Of2 (k, k + 10L)
                else
                    Choice2Of2 (k, System.Char.ToUpper v))

        evens |> assertEqual (LongMap.ofArray evensExpected)

        odds |> assertEqual (LongMap.ofArray oddsExpected)


(* MapModule and MapType tests from the F# distribution. *)

module MapType =
    // Interfaces
    [<Test>]
    let IEnumerable() =        
        // Legit IE
        let ie = (LongMap.ofArray [|(1L,1L);(2L,4L);(3L,9L)|]) :> IEnumerable
        let enum = ie.GetEnumerator()
        
        let testStepping() =
            checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current :?> KeyValuePair<int64,int64>, KeyValuePair<int64,int64>(1L,1L))
            
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current :?> KeyValuePair<int64,int64>, KeyValuePair<int64,int64>(2L,4L))
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current :?> KeyValuePair<int64,int64>, KeyValuePair<int64,int64>(3L,9L))
            Assert.AreEqual(enum.MoveNext(), false)
            checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
    
        testStepping()
        enum.Reset()
        testStepping()
    
        // Empty IE
        let ie = [] |> LongMap.ofList :> IEnumerable  // Note no type args
        let enum = ie.GetEnumerator()
        
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        Assert.AreEqual(enum.MoveNext(), false)
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)  
    
    [<Test>]
    let IEnumerable_T() =        
        // Legit IE
        let ie = (LongMap.ofArray [|(1L,1L);(2L,4L);(3L,9L)|]) :> IEnumerable<KeyValuePair<_,_>>
        let enum = ie.GetEnumerator()
        
        let testStepping() =
            checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, new KeyValuePair<int64,int64>(1L,1L))
            
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, new KeyValuePair<int64,int64>(2L,4L))
            Assert.AreEqual(enum.MoveNext(), true)
            Assert.AreEqual(enum.Current, new KeyValuePair<int64,int64>(3L,9L))
            Assert.AreEqual(enum.MoveNext(), false)
            checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
    
        testStepping()
        enum.Reset()
        testStepping()
    
        // Empty IE
        let ie = [] |> LongMap.ofList :> IEnumerable  // Note no type args
        let enum = ie.GetEnumerator()
        
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        Assert.AreEqual(enum.MoveNext(), false)
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)  
    
    
    [<Test>]
    let IDictionary() =        
        // Legit ID
        let id = (LongMap.ofArray [|(1L,1L);(2L,4L);(3L,9L)|]) :> IDictionary<_,_> 
        
        Assert.IsTrue(id.ContainsKey(1L))   
        Assert.IsFalse(id.ContainsKey(5L))  
        Assert.AreEqual(id.[1L], 1L)  
        Assert.AreEqual(id.[3L], 9L) 
        CollectionAssert.AreEqual(id.Keys,   [| 1; 2; 3|] :> ICollection<_>)
        CollectionAssert.AreEqual(id.Values, [| 1; 4; 9|] :> ICollection<_>)
        
        checkThrowsNotSupportedException(fun () -> id.[2L] <-88L)

        checkThrowsNotSupportedException(fun () -> id.Add(new KeyValuePair<int64,int64>(4L,16L)))
        Assert.IsTrue(id.TryGetValue(1L, ref 1L))
        Assert.IsFalse(id.TryGetValue(100L, ref 1L))
        checkThrowsNotSupportedException(fun () -> id.Remove(1L) |> ignore)
        
        // Empty ID
        let id = LongMap.empty :> IDictionary<int64, int64>   // Note no type args  
        Assert.IsFalse(id.ContainsKey(5L))
        checkThrowsKeyNotFoundException(fun () -> id.[1L] |> ignore)  
        CollectionAssert.AreEqual(id.Keys,   [| |] :> ICollection<_>)
        CollectionAssert.AreEqual(id.Values, [| |] :> ICollection<_>) 
    
    [<Test>]
    let ICollection() =        
        // Legit IC
        let ic = (LongMap.ofArray [|(1L,1L);(2L,4L);(3L,9L)|]) :> ICollection<KeyValuePair<_,_>>
        
        Assert.AreEqual(ic.Count, 3)
        Assert.IsTrue(ic.Contains(new KeyValuePair<int64,int64>(3L,9L))) 
        let newArr = Array.create 5 (new KeyValuePair<int64,int64>(3L,9L))
        ic.CopyTo(newArr,0) 
        Assert.IsTrue(ic.IsReadOnly)
        
        
        // raise ReadOnlyCollection exception
        checkThrowsNotSupportedException(fun () -> ic.Add(new KeyValuePair<int64,int64>(3L,9L)) |> ignore)
        checkThrowsNotSupportedException(fun () -> ic.Clear() |> ignore)
        checkThrowsNotSupportedException(fun () -> ic.Remove(new KeyValuePair<int64,int64>(3L,9L)) |> ignore) 
        
            
        // Empty IC
        let ic = LongMap.empty :> ICollection<KeyValuePair<int64, int64>>   
        Assert.IsFalse(ic.Contains(new KeyValuePair<int64,int64>(3L,9L)))      
        let newArr = Array.create 5 (new KeyValuePair<int64,int64>(0L,0L))
        ic.CopyTo(newArr,0) 
    
    [<Test>]
    let IComparable() =        
        // Legit IC
        let ic = (LongMap.ofArray [|(1L,1L);(2L,4L);(3L,9L)|]) :> IComparable    
        Assert.AreEqual(ic.CompareTo([(1L,1L);(2L,4L);(3L,9L)]|> LongMap.ofList),0) 
        Assert.AreEqual(ic.CompareTo([(1L,1L);(3L,9L);(2L,4L)]|> LongMap.ofList),0) 
        Assert.AreEqual(ic.CompareTo([(1L,1L);(9L,81L);(2L,4L)]|> LongMap.ofList),-1) 
        Assert.AreEqual(ic.CompareTo([(1L,1L);(0L,0L);(2L,4L)]|> LongMap.ofList),1)      
        checkThrowsArgumentException(fun() -> ic.CompareTo([(1L,1L);(2L,4L);(3L,9L)]) |> ignore)
                  
        // Empty IC
        let ic = [] |> LongMap.ofList :> IComparable   
        Assert.AreEqual(ic.CompareTo([]|> LongMap.ofList),0)
    
    
    // Base class methods
    [<Test>]
    let ObjectGetHashCode() =
        // Works on empty maps
        let e = LongMap.ofList (List.empty<int64 * decimal>)
        let m = LongMap.ofList [ (1L, -1.0M) ]
        Assert.AreNotEqual(e.GetHashCode(), m.GetHashCode())
        
        // Should be order independent
        let x = LongMap.ofList [(1L, -1.0M); (2L, -2.0M)]
        let y = LongMap.ofList [(2L, -2.0M); (1L, -1.0M)]
        Assert.AreEqual(x.GetHashCode(), y.GetHashCode())
    
    [<Test>]
    let ObjectToString() =
        Assert.AreEqual("longMap [(1L, 1); (2L, 4); (3L, 9)]", (LongMap.ofArray [|(1L,1);(2L,4);(3L,9)|]).ToString())
        Assert.AreEqual("longMap []", ([] |> LongMap.ofList).ToString())
    
    [<Test>]
    let ObjectEquals() =
        // All three are different references, but equality has been
        // provided by the F# compiler.
        let a = [(1L,1);(2L,4);(3L,9)] |> LongMap.ofList
        let b = (1L,1) :: [(2L,4);(3L,9)] |> LongMap.ofList
        Assert.IsTrue( (a = b) )

        Assert.IsTrue( a.Equals(b) ); Assert.IsTrue( b.Equals(a) )

        // Equality between types
        let a = ([] : (int64*int64) list)  |> LongMap.ofList
        let b = ([] : (int64*string) list ) |> LongMap.ofList
        Assert.IsFalse( b.Equals(a) )
        Assert.IsFalse( a.Equals(b) )
        
        // Co/contra varience not supported
        let a = ([] : (int64*string) list) |> LongMap.ofList
        let b = ([] : (int64*System.IComparable) list)    |> LongMap.ofList
        Assert.IsFalse(a.Equals(b))
        Assert.IsFalse(b.Equals(a))
        
        // Self equality
        let a = [(1L,1)] |> LongMap.ofList
        Assert.IsTrue( (a = a) )
        Assert.IsTrue(a.Equals(a))
        
        // Null
        Assert.IsFalse(a.Equals(null))

    // Instance methods
    [<Test>]
    let New() =    
        let newLongMap = new LongMap<int>([|(1L,1);(2L,4);(3L,9)|])
        let b = newLongMap.Add(4L,16)
        Assert.AreEqual(b.[4L], 16)
        Assert.AreEqual(b.[2L], 4)
    
        let e  = new  LongMap<string>([])
        let ae = e.Add(1L,"Monday")
        Assert.AreEqual(ae.[1L], "Monday")
        
    let Add() =
    
        let a = (LongMap.ofArray [|(1L,1);(2L,4);(3L,9)|])
        let b = a.Add(4L,16)
        Assert.AreEqual(b.[4L], 16)
        Assert.AreEqual(b.[2L], 4)
    
        let e  = LongMap.empty<string>
        let ae = e.Add(1L,"Monday")
        Assert.AreEqual(ae.[1L], "Monday")
    
    [<Test>]
    let ContainsKey() =
    
        let a = (LongMap.ofArray [|(1L,1);(2L,4);(3L,9)|])        
        Assert.IsTrue(a.ContainsKey(3L))
    
        let e  = LongMap.empty<string>
        Assert.IsFalse(e.ContainsKey(3L)) 
    
    
    [<Test>]
    let Count() =
    
        let a = (LongMap.ofArray [|(1L,1);(2L,4);(3L,9)|])
        Assert.AreEqual(a.Count, 3L)
    
        let e  = LongMap.empty<string>
        Assert.AreEqual(e.Count, 0L) 
    
    [<Test>]
    let IsEmpty() =
    
        let l = (LongMap.ofArray [|(1L,1);(2L,4);(3L,9)|])
        Assert.IsFalse(l.IsEmpty)
    
        let e = LongMap.empty<int64>
        Assert.IsTrue(e.IsEmpty)        
    
    [<Test>]
    let Item() =

        let mutable l = [(1L,1)] |> LongMap.ofList
        Assert.AreEqual(l.[1L], 1)
        l <- l.Add(100L,8)
        Assert.AreEqual(l.[100L], 8)
        
        for testidx in 0L .. 20L do
            let l = LongMap.ofSeq (seq { for i in 0L..testidx do yield (i,i*i)})
            for i in 0L .. (l.Count - 1L) do
                Assert.AreEqual(i*i, l.[i])
                Assert.AreEqual(i*i, l.Item(i))
        
        // Invalid index
        let l = (LongMap.ofArray [|(1L,1);(2L,4);(3L,9)|])
        checkThrowsKeyNotFoundException(fun () -> l.[ -1L ] |> ignore)
        checkThrowsKeyNotFoundException(fun () -> l.[1000L] |> ignore)
    
    [<Test>]
    let Remove() =
    
        let l = (LongMap.ofArray [|(1L,1);(2L,4);(3L,9)|])
        let rem = l.Remove(2L)
        Assert.AreEqual(rem.Count, 2L) 
        checkThrowsKeyNotFoundException(fun () -> rem.[ 2L ] |> ignore)
    
        let e  = LongMap.empty<string>
        let ae = e.Remove(2L)
        Assert.AreEqual(ae.Count, 0L)
        
    [<Test>]
    let TryFind() =
    
        let l = (LongMap.ofArray [|(1L,1);(2L,4);(3L,9)|])
        let rem = l.TryFind(2L)
        Assert.AreEqual(l.TryFind(2L),Some 4)         
    
        let e  = LongMap.empty<string>
    
        Assert.AreEqual(e.TryFind(2L), None)


module MapModule =
    [<Test>]
    let empty () : unit =
        let emptyMap = LongMap.empty        
        Assert.IsTrue(LongMap.isEmpty emptyMap)
        
        let a:LongMap<int64>    = LongMap.empty<int64>
        let c : LongMap<string> = LongMap.empty<string>  
              
        ()
        
    [<Test>]
    let add () : unit =
        // value keys
        let valueKeyMap = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]
        let resultValueMap = LongMap.add 1L "a" valueKeyMap        
        Assert.AreEqual(resultValueMap.[1L], "a")
        
        // empty LongMap
        let eptMap = LongMap.empty
        let resultEpt = LongMap.add 1L "a" eptMap
        Assert.AreEqual(resultEpt.[1L], "a")
        
        // One-element LongMap
        let oeleMap = LongMap.ofSeq [(1L, "one")]
        let resultOele = LongMap.add  7L  "seven" oeleMap
        Assert.AreEqual(resultOele.[7L], "seven")     
        
        // extra test for add -- add some key which already exit in the LongMap
        let extMap = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]
        let resultExt = LongMap.add 2L "dup" extMap        
        Assert.AreEqual(resultExt.[2L], "dup")   
        
         
        ()

    [<Test>]
    let exists () : unit =
        // value keys
        let valueKeyMap = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]
        let resultValueMap = LongMap.exists (fun x y -> x > 3L) valueKeyMap        
        Assert.IsTrue(resultValueMap)
        
        // One-element LongMap
        let oeleMap = LongMap.ofSeq [(1L, "one")]
        let resultOele = oeleMap |> LongMap.exists  (fun x y -> (x + int64 y.Length) % 4L = 0L ) 
        Assert.IsTrue(resultOele)
        
        // empty LongMap
        let eptMap = LongMap.empty
        let resultEpt = LongMap.exists (fun x y -> false) eptMap
        Assert.IsFalse(resultEpt)
       
        ()
        
    [<Test>]
    let filter () : unit =
        // value keys
        let valueKeyMap = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]
        let resultValueMap =valueKeyMap |> LongMap.filter (fun x y -> x % 3L = 0L)         
        Assert.AreEqual(resultValueMap,[3L,"c"] |> LongMap.ofList)
        
        // One-element LongMap
        let oeleMap = LongMap.ofSeq [(1L, "one")]
        let resultOele = oeleMap |> LongMap.filter  (fun x y -> x<3L )         
        Assert.AreEqual(resultOele,oeleMap)
        
        // empty LongMap
        let eptMap = LongMap.empty
        let resultEpt = LongMap.filter (fun x y -> true) eptMap        
        Assert.AreEqual(resultEpt,eptMap)
               
        ()       


    [<Test>]
    let find () : unit =
        // value keys
        let valueKeyMap = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]
        let resultValueMap = LongMap.find 5L valueKeyMap        
        Assert.AreEqual(resultValueMap,"e")
        
        // One-element LongMap
        let oeleMap = LongMap.ofSeq [(1L, "one")]
        let resultOele = LongMap.find 1L oeleMap        
        Assert.AreEqual(resultOele,"one")
        
        // empty LongMap
        let eptMap = LongMap.empty
        checkThrowsKeyNotFoundException (fun () -> LongMap.find 1L eptMap |> ignore)
               
        ()  

    [<Test>]
    let findIndex () : unit =
        // value keys
        let valueKeyMap = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]
        let resultValueMap =valueKeyMap |> LongMap.findKey (fun x y -> x % 3L = 0L)         
        Assert.AreEqual(resultValueMap,3L)
        
        // One-element LongMap
        let oeleMap = LongMap.ofSeq [(1L, "one")]
        let resultOele = oeleMap |> LongMap.findKey  (fun x y -> x = 1L )         
        Assert.AreEqual(resultOele,1L)
        
        // empty LongMap
        let eptMap = LongMap.empty
        checkThrowsKeyNotFoundException (fun () -> LongMap.findKey (fun x y -> true) eptMap |> ignore)
               
        ()          
     
    [<Test>]
    let tryPick () : unit =
        // value keys
        let valueKeyMap = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]
        let resultValueMap = valueKeyMap |> LongMap.tryPick (fun x y -> if x % 3L = 0L then Some (x) else None)         
        Assert.AreEqual(resultValueMap,Some 3L)
        
        // One-element LongMap
        let oeleMap = LongMap.ofSeq [(1L, "one")]
        let resultOele = oeleMap |> LongMap.tryPick  (fun x y -> if(x + int64 y.Length) % 4L = 0L then Some y else None )         
        Assert.AreEqual(resultOele,Some "one")
        
        // empty LongMap
        let eptMap = LongMap.empty
        let resultEpt = LongMap.tryPick (fun x y -> Some x) eptMap        
        Assert.AreEqual(resultEpt,None)
               
        ()     

    [<Test>]
    let pick () : unit =
        // value keys
        let valueKeyMap = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]
        let resultValue = valueKeyMap |> LongMap.pick (fun x y -> if x % 3L = 0L then Some (y) else None)         
        Assert.AreEqual(resultValue, "c")
        
        // One-element LongMap
        let oeleMap = LongMap.ofSeq [(1L, "one")]
        let resultOele = oeleMap |> LongMap.pick (fun x y -> if(x + int64 y.Length) % 4L = 0L then Some y else None )         
        Assert.AreEqual(resultOele, "one")
        
        // empty LongMap
        let eptMap = LongMap.empty
        let resultEpt = 
            try 
                LongMap.pick (fun x y -> Some x) eptMap
            with :? System.Collections.Generic.KeyNotFoundException -> 0L
        Assert.AreEqual(resultEpt, 0L)
        
        ()

    [<Test>]
    let fold () : unit =
        // value keys
        let valueKeyMap = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]
        let resultValueMap = valueKeyMap |> LongMap.fold (fun x y z -> x + y + int64 z.Length) 10L         
        Assert.AreEqual(resultValueMap,28L)
        
        // One-element LongMap
        let oeleMap = LongMap.ofSeq [(1L, "one")]
        let resultOele =   oeleMap |> LongMap.fold  (fun x y z -> x + y.ToString() + z)  "got"      
        Assert.AreEqual(resultOele,"got1one")
        
        // empty LongMap
        let eptMap = LongMap.empty
        let resultEpt = eptMap |> LongMap.fold (fun x y z -> 1) 1         
        Assert.AreEqual(resultEpt,1)
               
        ()

    [<Test>]
    let foldBack () : unit =
        // value keys
        let valueKeyMap = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]
        let resultValueMap = LongMap.foldBack (fun x y z -> x.ToString() + y + z.ToString()) valueKeyMap "*"     
        Assert.AreEqual(resultValueMap,"2b3c4d5e*")
        
        // One-element LongMap
        let oeleMap = LongMap.ofSeq [(1L, "one")]
        let resultOele = LongMap.foldBack  (fun x y z -> x.ToString() + y + z) oeleMap "right"         
        Assert.AreEqual(resultOele,"1oneright")
        
        // empty LongMap
        let eptMap = LongMap.empty
        let resultEpt = LongMap.foldBack (fun x y z -> 1) eptMap 1         
        Assert.AreEqual(resultEpt,1)
               
        ()
        
    [<Test>]
    let forall () : unit =
        // value keys
        let valueKeyMap = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]
        let resultValueMap = valueKeyMap |> LongMap.forall (fun x y -> x % 3L = 0L)         
        Assert.IsFalse(resultValueMap)
        
        // One-element LongMap
        let oeleMap = LongMap.ofSeq [(1L, "one")]
        let resultOele = oeleMap |> LongMap.forall  (fun x y -> x<3L )         
        Assert.IsTrue(resultOele)
        
        // empty LongMap
        let eptMap = LongMap.empty
        let resultEpt =eptMap |>  LongMap.forall (fun x y -> true)         
        Assert.IsTrue(resultEpt)
               
        ()       


    [<Test>]
    let isEmpty () : unit =
        // value keys
        let valueKeyMap = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]
        let resultValueMap = LongMap.isEmpty  valueKeyMap        
        Assert.IsFalse(resultValueMap)
        
        // One-element LongMap
        let oeleMap = LongMap.ofSeq [(1L, "one")]
        let resultOele = LongMap.isEmpty   oeleMap        
        Assert.IsFalse(resultOele)
        
        // empty LongMap
        let eptMap = LongMap.empty
        let resultEpt = LongMap.isEmpty  eptMap        
        Assert.IsTrue(resultEpt)
               
        ()  

    [<Test>]
    let iter () : unit =
        // value keys
        let valueKeyMap = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]
        let resultValueMap = ref 0L    
        let funInt (x:int64) (y:string) =   
            resultValueMap := !resultValueMap + x + int64 y.Length             
            () 
        LongMap.iter funInt valueKeyMap        
        Assert.AreEqual(!resultValueMap,18L)
        
        // One-element LongMap
        let oeleMap = LongMap.ofSeq [(1L, "one")]
        let resultOele = ref ""
        let funMix  (x:int64) (y:string) =
            resultOele := !resultOele + x.ToString() + y
            ()
        LongMap.iter funMix oeleMap        
        Assert.AreEqual(!resultOele,"1one")
        
        // empty LongMap
        let eptMap = LongMap.empty
        let resultEpt = ref 0L    
        let funEpt (x:int64) (y:int64) =   
            resultEpt := !resultEpt + x + y              
            () 
        LongMap.iter funEpt eptMap        
        Assert.AreEqual(!resultEpt,0L)
               
        ()          
     
    [<Test>]
    let map () : unit =

        // value keys
        let valueKeyMap = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]
        let resultValueMap = valueKeyMap |> LongMap.map (fun x y -> x.ToString() + y )         
        Assert.AreEqual(resultValueMap,[(2L,"2b"); (3L,"3c"); (4L,"4d"); (5L,"5e")] |> LongMap.ofList)
        
        // One-element LongMap
        let oeleMap = LongMap.ofSeq [(1L, "one")]
        let resultOele = oeleMap |> LongMap.map  (fun x y -> x.ToString() + y )         
        Assert.AreEqual(resultOele,[1L,"1one"] |> LongMap.ofList)
        
        // empty LongMap
        let eptMap = LongMap.empty<int64>
        let resultEpt = eptMap |> LongMap.map (fun x y -> x+y)         
        Assert.AreEqual(resultEpt,eptMap)
               
        ()     

    [<Test>]
    let contains () : unit =
        // value keys
        let valueKeyMap = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]
        let resultValueMap = LongMap.containsKey 2L valueKeyMap        
        Assert.IsTrue(resultValueMap)
        
        // One-element LongMap
        let oeleMap = LongMap.ofSeq [(1L, "one")]
        let resultOele = LongMap.containsKey 1L oeleMap        
        Assert.IsTrue(resultOele)
        
        // empty LongMap
        let eptMap = LongMap.empty
        let resultEpt = LongMap.containsKey 3L eptMap        
        Assert.IsFalse(resultEpt)
               
        () 
        
    [<Test>]
    let ofArray_ofList_ofSeq () : unit =
        // value keys    
        let valueKeyMapOfArr = LongMap.ofArray [|(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")|]     
        let valueKeyMapOfList = LongMap.ofList [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]
        let valueKeyMapOfSeq = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]

        CollectionAssert.AreEqual(valueKeyMapOfArr,valueKeyMapOfList)
        CollectionAssert.AreEqual(valueKeyMapOfList,valueKeyMapOfSeq)
        CollectionAssert.AreEqual(valueKeyMapOfArr,valueKeyMapOfSeq)
        
        // One-element LongMap
        let oeleMapOfArr = LongMap.ofArray [|(1L,"one")|]
        let oeleMapOfList = LongMap.ofList [(1L,"one") ]
        let oeleMapOfSeq = LongMap.ofSeq [(1L,"one") ]

        CollectionAssert.AreEqual(oeleMapOfArr,oeleMapOfList)
        CollectionAssert.AreEqual(oeleMapOfList,oeleMapOfSeq)
        CollectionAssert.AreEqual(oeleMapOfArr,oeleMapOfSeq)
                
        ()

    [<Test>]
    let partition () : unit =
        // value keys
        let valueKeyMap = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]
        let resultValueMap = LongMap.partition (fun x y  -> x%2L = 0L) valueKeyMap         
        let choosed = [(2L,"b"); (4L,"d")] |> LongMap.ofList
        let notChoosed = [(3L,"c"); (5L,"e")] |> LongMap.ofList
        Assert.AreEqual(resultValueMap,(choosed,notChoosed))
        
        // One-element LongMap
        let oeleMap = LongMap.ofSeq [(1L, "one")]
        let resultOele = LongMap.partition  (fun x y  -> x<4L) oeleMap     
        let choosed = [(1L,"one")] |> LongMap.ofList
        let notChoosed = LongMap.empty<string>
        Assert.AreEqual(resultOele,(choosed,notChoosed))
        
        // empty LongMap
        let eptMap = LongMap.empty
        let resultEpt = LongMap.partition (fun x y  -> true) eptMap          
        Assert.AreEqual(resultEpt,(eptMap,eptMap))
               
        ()
    
        
    [<Test>]
    let remove () : unit =
        // value keys
        let valueKeyMap = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]
        let resultValueMap = LongMap.remove 5L valueKeyMap        
        Assert.AreEqual(resultValueMap,[(2L,"b"); (3L,"c"); (4L,"d")] |> LongMap.ofList)
        
        // One-element LongMap
        let oeleMap = LongMap.ofSeq [(1L, "one")]
        let resultOele = LongMap.remove  1L oeleMap             
        Assert.AreEqual(resultOele,LongMap.empty<string>)
        
        // Two-element LongMap
        let oeleMap = LongMap.ofSeq [(1L, "one");(2L,"Two")]
        let resultOele = LongMap.remove  1L oeleMap             
        let exOele = LongMap.ofSeq [(2L, "Two")]
        Assert.AreEqual(resultOele, exOele)
        
        // Item which want to be removed not included in the map
        let valueKeyMap = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]
        let resultValueMap = LongMap.remove 5L valueKeyMap        
        Assert.AreEqual(resultValueMap,[(2L,"b"); (3L,"c"); (4L,"d")] |> LongMap.ofList)
        
                               
        ()
        
    [<Test>]
    let toArray () : unit =
        // value keys    
        let valueKeyMapOfArr = LongMap.ofArray [|(1L,1);(2L,4);(3L,9)|]     
        let resultValueMap = LongMap.toArray valueKeyMapOfArr        
        Assert.AreEqual(resultValueMap,[|(1L,1);(2L,4);(3L,9)|])
        
        // One-element LongMap
        let oeleMapOfArr = LongMap.ofArray [|(1L,"one")|]
        let resultOele = LongMap.toArray oeleMapOfArr        
        Assert.AreEqual(resultOele,[|(1L,"one")|])
        
        // empty LongMap
        let eptMap = LongMap.ofArray [||]
        let resultEpt = LongMap.toArray eptMap            
        Assert.AreEqual(resultEpt,[||])

        () 

    [<Test>]
    let toList () : unit =
        // value keys    
        let valueKeyMapOfArr = LongMap.ofList [(1L,1);(2L,4);(3L,9)]     
        let resultValueMap = LongMap.toList valueKeyMapOfArr        
        Assert.AreEqual(resultValueMap,[(1L,1);(2L,4);(3L,9)])
        
        // One-element LongMap
        let oeleMapOfArr = LongMap.ofList [(1L,"one")]
        let resultOele = LongMap.toList oeleMapOfArr        
        Assert.AreEqual(resultOele,[(1L,"one")])
        
        // empty LongMap
        let eptMap = LongMap.empty<string>
        let resultEpt = LongMap.toList eptMap  
        let eptList :(int64*string) list = []          
        Assert.AreEqual(resultEpt,eptList)

        ()     

    [<Test>]
    let toSeq () : unit =
        // value keys    
        let valueKeyMapOfArr = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]  
        let resultValueMap = LongMap.toSeq valueKeyMapOfArr
        let originInt = seq { for i in 1..3 do yield (i,i*i)}
        CollectionAssert.AreEqual (
            resultValueMap,
            [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")] :> IEnumerable)

        
        // One-element LongMap
        let oeleMapOfArr = LongMap.ofSeq [(1L,"one")]
        let resultOele = LongMap.toSeq oeleMapOfArr
        let originMix = seq { for x in [ "is" ;"str"; "this" ;"lists"] do yield (x.Length,x.ToUpper())}
        CollectionAssert.AreEqual (resultOele, [(1L,"one")])

         
        ()           

    [<Test>]
    let tryFind () : unit =
        // value keys
        let valueKeyMap = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]
        let resultValueMap = LongMap.tryFind 5L valueKeyMap        
        Assert.AreEqual(resultValueMap,Some "e")
        
        // One-element LongMap
        let oeleMap = LongMap.ofSeq [(1L, "one")]
        let resultOele = LongMap.tryFind 1L oeleMap        
        Assert.AreEqual(resultOele,Some "one")
        
        // empty LongMap
        let eptMap = LongMap.empty
        let resultEpt = LongMap.tryFind 1L eptMap        
        Assert.AreEqual(resultEpt,None)
               
        ()      

    [<Test>]
    let tryFindIndex () : unit =
        // value keys
        let valueKeyMap = LongMap.ofSeq [(2L,"b"); (3L,"c"); (4L,"d"); (5L,"e")]
        let resultValueMap = valueKeyMap |> LongMap.tryFindKey (fun x y -> x+ int64 y.Length >30L)         
        Assert.AreEqual(resultValueMap,None)
        
        // One-element LongMap
        let oeleMap = LongMap.ofSeq [(1L, "one")]
        let resultOele = oeleMap |> LongMap.tryFindKey (fun x y -> y.Contains("o"))        
        Assert.AreEqual(resultOele,Some 1L)
        
        // empty LongMap
        let eptMap = LongMap.empty
        let resultEpt = LongMap.tryFindKey (fun x y -> x+y >30L) eptMap        
        Assert.AreEqual(resultEpt,None)
               
        ()  


/// FsCheck-based tests for LongMap.
module FsCheck =
    open FsCheck

    /// FsCheck generators for LongMap.
    type LongMapGenerator =
        /// Generates an arbitrary LongMap instance.
        static member LongMap () : Arbitrary<LongMap<'T>> =
            gen {
                let! keys = Arb.generate
                let! values = Arb.generate
            
                // It seems FsCheck requires the use of sequences here --
                // using List.fold2 to build the LongMap causes FsCheck to crash.
                let kvpSeq = (Seq.ofList keys, Seq.ofList values) ||> Seq.zip
                return LongMap.ofSeq kvpSeq
            } |> Arb.fromGen

    /// Registers the FsCheck generators so they're already loaded
    /// when NUnit runs the tests in this fixture.
    [<TestFixtureSetUp>]
    let registerFsCheckGenerators =
        Arb.register<LongMapGenerator> () |> ignore


    (* Tests *)

    [<Test>]
    let ``prop addLookup``() =
        assertProp "addLookup" <| fun k v map ->
            LongMap.add k v map
            |> LongMap.containsKey k

