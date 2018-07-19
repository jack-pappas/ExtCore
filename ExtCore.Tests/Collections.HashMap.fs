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

/// Unit tests for the ExtCore.Collections.HashMap type and module.
module Tests.ExtCore.Collections.HashMap

open System
open System.Collections
open System.Collections.Generic
open NUnit.Framework


/// A type which uses the same hash value for every instance.
/// This is used to test how HashMap handles collisions.
[<Struct; CustomEquality; NoComparison>]
type SameHash<'T when 'T : equality> =
    val Value : 'T

    override this.Equals (other : obj) =
        match other with
        | :? SameHash<'T> as other ->
            this.Value = other.Value
        | _ ->
            false

    override __.GetHashCode () = 1234

/// A type which wraps an instance of another type, and restricts
/// it's hash code values to some smaller range of values.
[<Struct; CustomEquality; NoComparison>]
type RestrictHash<'T when 'T : equality> =
    val Value : 'T

    override this.Equals (other : obj) =
        match other with
        | :? RestrictHash<'T> as other ->
            this.Value = other.Value
        | _ ->
            false

    override this.GetHashCode () =
        (hash this.Value) / 10


[<Test>]
let isEmpty () : unit =
    // Test case for an empty map.
    HashMap.empty
    |> HashMap.isEmpty
    |> assertTrue

    // Sample usage test cases.
    HashMap.singleton 1 'a'
    |> HashMap.isEmpty
    |> assertFalse

[<Test>]
let count () : unit =
    // Test case for an empty map.
    HashMap.empty
    |> HashMap.count
    |> assertEqual 0

    // Sample usage test cases.
    HashMap.singleton 1 'a'
    |> HashMap.count
    |> assertEqual 1

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> HashMap.ofArray
    |> HashMap.count
    |> assertEqual 8

[<Test>]
let singleton () : unit =
    // Sample usage test cases.
    HashMap.singleton 1 'a'
    |> assertEqual (
        HashMap.add 1 'a' HashMap.empty)

    HashMap.singleton 1 'a'
    |> HashMap.count
    |> assertEqual 1

[<Test>]
let containsKey () : unit =
    // Sample usage test cases.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> HashMap.ofArray
    |> HashMap.containsKey 5
    |> assertTrue

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> HashMap.ofArray
    |> HashMap.containsKey 1
    |> assertFalse

[<Test>]
let tryFind () : unit =
    // Sample usage test cases.
    [(5, 'a'); (3, 'b')]
    |> HashMap.ofList
    |> HashMap.tryFind 5
    |> assertEqual (Some 'a')

    [(5, 'a'); (3, 'b')]
    |> HashMap.ofList
    |> HashMap.tryFind 7
    |> assertEqual None

[<Test>]
let find () : unit =
    // Sample usage test cases.
    [(5, 'a'); (3, 'b')]
    |> HashMap.ofList
    |> HashMap.find 5
    |> assertEqual 'a'

[<Test>]
let ``find raises exn when key is not found`` () : unit =
    Assert.Throws<KeyNotFoundException>(fun () ->
        [(5, 'a'); (3, 'b')]
        |> HashMap.ofList
        |> HashMap.find 9
        |> ignore) |> ignore

[<Test>]
let findOrDefault () : unit =
    // Sample usage test cases.
    [(5, 'a'); (3, 'b')]
    |> HashMap.ofList
    |> HashMap.findOrDefault 'z' 5
    |> assertEqual 'a'

    [(5, 'a'); (3, 'b')]
    |> HashMap.ofList
    |> HashMap.findOrDefault 'z' 7
    |> assertEqual 'z'

[<Test>]
let tryFindKey () : unit =
    // Test case for an empty map.
    (HashMap.empty : HashMap<int, string>)
    |> HashMap.tryFindKey (fun k v ->
        k % 2 = 0)
    |> assertEqual None

    // Sample usage test cases.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> HashMap.ofArray
    |> HashMap.tryFindKey (fun k v ->
        (k + int v) % 11 = 0)
    |> assertEqual (Some 12)

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> HashMap.ofArray
    |> HashMap.tryFindKey (fun k v ->
        (k + int v) % 289 = 0)
    |> assertEqual None

[<Test>]
let add () : unit =
    // Test case for an empty map.
    HashMap.empty
    |> HashMap.add 5 'x'
    |> assertEqual (
        HashMap.singleton 5 'x')

    // Sample usage test cases.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> HashMap.ofArray
    |> HashMap.add 5 'x'
    |> assertEqual (HashMap.ofArray
       [| (5, 'x'); (3, 'b'); (11, 'f'); (2, 'd');
          (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |])

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> HashMap.ofArray
    |> HashMap.add 13 'x'
    |> assertEqual (HashMap.ofArray
       [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
          (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (13, 'x'); |])

[<Test>]
let remove () : unit =
    // Test case for an empty map.
    (HashMap.empty : HashMap<int, string>)
    |> HashMap.remove 5
    |> HashMap.isEmpty
    |> assertTrue

    // Sample usage test cases.
    [(5, "a"); (3, "b")]
    |> HashMap.ofList
    |> HashMap.remove 5
    |> assertEqual (
        HashMap.singleton 3 "b")

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> HashMap.ofArray
    |> HashMap.remove 4
    |> assertEqual (HashMap.ofArray
       [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
          (17, 'a'); (12, 'b'); (14, 'c'); |])

[<Test>]
let union () : unit =
    let initial1 =
        HashMap.empty
        |> HashMap.add ConsoleColor.Red "Red"
        |> HashMap.add ConsoleColor.Green "Green"
        |> HashMap.add ConsoleColor.Blue "Blue"
        |> HashMap.add ConsoleColor.Black "Black"
        |> HashMap.add ConsoleColor.Cyan "Cyan"

    let initial2 =
        HashMap.empty
        |> HashMap.add ConsoleColor.Black "Black"
        |> HashMap.add ConsoleColor.Cyan "Cyna"     // Note the misspelling!
        |> HashMap.add ConsoleColor.Gray "Gray"
        |> HashMap.add ConsoleColor.Yellow "Yellow"
        |> HashMap.add ConsoleColor.White "White"

    let expected =
        HashMap.empty
        |> HashMap.add ConsoleColor.Red "Red"
        |> HashMap.add ConsoleColor.Green "Green"
        |> HashMap.add ConsoleColor.Blue "Blue"
        |> HashMap.add ConsoleColor.Black "Black"
        |> HashMap.add ConsoleColor.Cyan "Cyan"
        |> HashMap.add ConsoleColor.Gray "Gray"
        |> HashMap.add ConsoleColor.Yellow "Yellow"
        |> HashMap.add ConsoleColor.White "White"

    HashMap.union initial1 initial2
    |> assertEqual expected
(*
[<Test>]
let union () : unit =
    HashMap.union
        (HashMap.ofArray [| (3, 'b'); (11, 'F'); (2, 'd'); (4, 'G'); (12, 'b'); |])
        (HashMap.ofArray [| (5, 'a'); (11, 'f'); (17, 'a'); (4, 'g'); (14, 'c'); |])
    |> assertEqual
        (HashMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])

[<Test>]
let intersect () : unit =
    HashMap.intersect
        (HashMap.ofArray [| (3, 'b'); (11, 'F'); (2, 'd'); (4, 'G'); (12, 'b'); |])
        HashMap.empty
    |> HashMap.isEmpty
    |> assertTrue

    HashMap.intersect
        (HashMap.ofArray [| (3, 'b'); (11, 'F'); (2, 'd'); (4, 'G'); (12, 'b'); |])
        (HashMap.ofArray [| (5, 'a'); (11, 'f'); (17, 'a'); (4, 'g'); (14, 'c'); |])
    |> assertEqual
        (HashMap.ofArray [| (11, 'F'); (4, 'G'); |])

[<Test>]
let difference () : unit =
    HashMap.difference
        (HashMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])
        HashMap.empty
    |> assertEqual
        (HashMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])

    HashMap.difference
        (HashMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])
        (HashMap.ofArray [| (3, 'b'); (11, 'f'); (2, 'd'); (4, 'g'); (12, 'b'); |])
    |> assertEqual
        (HashMap.ofArray [| (5, 'a'); (17, 'a'); (14, 'c'); |])

[<Test>]
let isSubmapOfBy () : unit =
    let map1 = HashMap.ofList [(1,1);(2,2)]

    map1.IsSubmapOfBy ((=),
        HashMap.ofList [(1,1)])
    |> assertTrue

    map1.IsSubmapOfBy ((<=),
        HashMap.ofList [(1,1)])
    |> assertTrue

    map1.IsSubmapOfBy ((=),
        HashMap.ofList [(1,1);(2,2)])
    |> assertTrue

    map1.IsSubmapOfBy ((=),
        HashMap.ofList [(1,2)])
    |> assertFalse

    map1.IsSubmapOfBy ((<),
        HashMap.ofList [(1,1)])
    |> assertFalse

    let map2 = HashMap.ofList [(1,1)]

    map2.IsSubmapOfBy ((=),
        HashMap.ofList [(1,1);(2,2)])
    |> assertFalse
*)

[<Test>]
let ofSeq () : unit =
    // Test case for an empty sequence.
    (Seq.empty : seq<int * string>)
    |> HashMap.ofSeq
    |> HashMap.isEmpty
    |> assertTrue

    // Sample usage test cases.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> Seq.ofArray
    |> HashMap.ofSeq
    |> Collection.assertEquiv
        (HashMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])

[<Test>]
let ofList () : unit =
    // Test case for an empty list.
    HashMap.ofList []
    |> HashMap.isEmpty
    |> assertTrue

    // Sample usage test cases.
    [(5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G')]
    |> HashMap.ofList
    |> Collection.assertEquiv
        (HashMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])

[<Test>]
let ofArray () : unit =
    // Test case for an empty array.
    Array.empty
    |> HashMap.ofArray
    |> HashMap.isEmpty
    |> assertTrue

    // Sample usage test cases.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> Collection.assertEquiv
        (HashMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])
(*
[<Test>]
let ofMap () : unit =
    // Test case for an empty map.
    Map.empty
    |> HashMap.ofMap
    |> HashMap.isEmpty
    |> assertTrue

    // Sample usage test cases.
    [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |]
    |> Map.ofArray
    |> HashMap.ofMap
    |> Collection.assertEquiv
        (HashMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])
*)
[<Test>]
let toSeq () : unit =
    // Test case for an empty map.
    HashMap.empty
    |> HashMap.toSeq
    |> Seq.isEmpty
    |> assertTrue

    // Sample usage test cases.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.toSeq
    |> Seq.toArray
    |> Collection.assertEquiv
        [| (2, 'd'); (3, 'b'); (4, 'G'); (5, 'a'); (11, 'F'); (12, 'b'); (14, 'c'); (17, 'a'); |]

[<Test>]
let toList () : unit =
    // Test case for an empty map.
    HashMap.empty
    |> HashMap.toList
    |> List.isEmpty
    |> assertTrue

    // Sample usage test cases.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.toList
    |> Collection.assertEquiv
        [(2, 'd'); (3, 'b'); (4, 'G'); (5, 'a'); (11, 'F'); (12, 'b'); (14, 'c'); (17, 'a')]

[<Test>]
let toArray () : unit =
    // Test case for an empty map.
    HashMap.empty
    |> HashMap.toArray
    |> Array.isEmpty
    |> assertTrue

    // Sample usage test cases.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.toArray
    |> Collection.assertEquiv
        [| (2, 'd'); (3, 'b'); (4, 'G'); (5, 'a'); (11, 'F'); (12, 'b'); (14, 'c'); (17, 'a'); |]
(*
[<Test>]
let toMap () : unit =
    // Test case for an empty map.
    HashMap.empty
    |> HashMap.toMap
    |> Map.isEmpty
    |> assertTrue

    // Sample usage test cases.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.toMap
    |> assertEqual
        (Map.ofArray [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |])
*)
[<Test>]
let tryPick () : unit =
    // Test case for empty input.
    HashMap.empty
    |> HashMap.tryPick (fun k v ->
        if (k + v) = 10 then
            Some 11
        else None)
    |> assertEqual None

    // Test case where no elements match the 'picker' function.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.tryPick (fun k v ->
        if System.Char.IsControl v then
            Some (int v + k)
        else None)
    |> assertEqual None

    // Test case where a single binding matches the 'picker' function.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.tryPick (fun k v ->
        if (k % 7 = 0) && v = 'c' then
            Some (k - 2)
        else None)
    |> assertEqual (Some 12)

    // Test case where multiple bindings match the 'picker' function.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.tryPick (fun k v ->
        if k % 3 = 2 then
            Some v
        else None)
    |> assertEqual (Some 'd')

[<Test>]
let pick () : unit =
    // Test case where a single binding matches the 'picker' function.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.pick (fun k v ->
        if (k % 7 = 0) && v = 'c' then
            Some (k - 2)
        else None)
    |> assertEqual 12

    // Test case where multiple bindings match the 'picker' function.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.pick (fun k v ->
        if k % 3 = 2 then
            Some v
        else None)
    |> assertEqual 'd'

[<Test>]
let ``pick raises exn on empty input`` () : unit =
    Assert.Throws<KeyNotFoundException>(fun () ->
        HashMap.empty
        |> HashMap.pick (fun k v ->
            if (k + v) % 2 = 0 then
                Some (v + 1)
            else None)
        |> ignore) |> ignore

[<Test>]
let ``pick raises exn when no match is found`` () : unit =
    Assert.Throws<KeyNotFoundException>(fun () ->
        [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
        |> HashMap.ofArray
        |> HashMap.pick (fun k v ->
            if System.Char.IsControl v then
                Some (int v + k)
            else None)
        |> ignore) |> ignore

[<Test>]
let map () : unit =
    // Test case for an empty map.
    HashMap.empty
    |> HashMap.map (sprintf "%i:%c")
    |> HashMap.isEmpty
    |> assertTrue

    // Sample usage test cases.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> HashMap.ofArray
    |> HashMap.map (sprintf "%i:%c")
    |> assertEqual (HashMap.ofArray
        [| (5, "5:a"); (3, "3:b"); (11, "11:f"); (2, "2:d");
            (17, "17:a"); (4, "4:g"); (12, "12:b"); (14, "14:c"); |])

[<Test>]
let filter () : unit =
    // Sample usage test cases.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.filter (fun k v ->
        (k % 2 = 0) && System.Char.IsLower v)
    |> assertEqual
        (HashMap.ofArray [| (2, 'd'); (12, 'b'); (14, 'c'); |])

[<Test>]
let choose () : unit =
    // Sample usage test cases.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.choose (fun k v ->
        if k % 2 <> 0 then
            Some (System.Char.ToUpper v)
        else None)
    |> assertEqual
        (HashMap.ofArray [| (5, 'A'); (3, 'B'); (17, 'A'); (11, 'F'); |])

[<Test>]
let iter () : unit =
    // Test case for an empty map.
    do
        let elements = ResizeArray ()

        HashMap.empty
        |> HashMap.iter (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.isEmpty
        |> assertTrue

    // Sample usage test cases.
    do
        let elements = ResizeArray ()

        [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
        |> HashMap.ofArray
        |> HashMap.iter (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.toArray
        |> assertEqual
            [| 'D'; 'B'; 'G'; 'A'; 'F'; 'B'; 'C'; 'A'; |]

[<Test>]
let iterBack () : unit =
    // Test case for an empty map.
    do
        let elements = ResizeArray ()

        HashMap.empty
        |> HashMap.iter (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.isEmpty
        |> assertTrue

    // Sample usage test cases.
    do
        let elements = ResizeArray ()

        [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
        |> HashMap.ofArray
        |> HashMap.iterBack (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.toArray
        |> Collection.assertEquiv
            [| 'A'; 'C'; 'B'; 'F'; 'A'; 'G'; 'B'; 'D'; |]

[<Test>]
let fold () : unit =
    // Test case for an empty map.
    do
        let elements = ResizeArray ()

        (0, HashMap.empty)
        ||> HashMap.fold (fun counter k v ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> assertEqual 0

        elements
        |> ResizeArray.isEmpty
        |> assertTrue

    // Sample usage test cases.
    do
        let elements = ResizeArray ()

        let testMap =
            [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
            |> HashMap.ofArray

        (0, testMap)
        ||> HashMap.fold (fun counter k v ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> assertEqual (HashMap.count testMap)

        elements
        |> ResizeArray.toArray
        |> Collection.assertEquiv
            [| 102; 102; 77; 105; 85; 115; 119; 121; |]

[<Test>]
let foldBack () : unit =
    // Test case for an empty map.
    do
        let elements = ResizeArray ()

        (HashMap.empty, 0)
        ||> HashMap.foldBack (fun counter k v ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> assertEqual 0

        elements
        |> ResizeArray.isEmpty
        |> assertTrue

    // Sample usage test cases.
    do
        let elements = ResizeArray ()

        let testMap =
            [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
            |> HashMap.ofArray

        (testMap, 0)
        ||> HashMap.foldBack (fun k v counter ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> assertEqual (HashMap.count testMap)

        elements
        |> ResizeArray.toArray
        |> Collection.assertEquiv
            [| 114; 114; 112; 84; 106; 80; 107; 109; |]

[<Test>]
let exists () : unit =
    // Test case for an empty map.
    HashMap.empty
    |> HashMap.exists (fun k v ->
        (k + int v) % 2 = 0)
    |> assertFalse

    // Sample usage test cases.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.exists (fun k v ->
        (k + int v) > 200)
    |> assertFalse

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.exists (fun k v ->
        (k + int v) % 2 = 0)
    |> assertTrue

[<Test>]
let forall () : unit =
    // Test case for an empty map.
    HashMap.empty
    |> HashMap.forall (fun k v ->
        (k + int v) < 200)
    |> assertTrue

    // Sample usage test cases.
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.forall (fun k v ->
        (k + int v) < 200)
    |> assertTrue

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.forall (fun k v ->
        (k + int v) % 2 = 0)
    |> assertFalse

[<Test>]
let partition () : unit =
    // Test case for an empty map.
    do
        let evens, odds =
            HashMap.empty
            |> HashMap.partition (fun k v ->
                (k + int v) % 2 = 0)

        evens
        |> HashMap.isEmpty
        |> assertTrue

        odds
        |> HashMap.isEmpty
        |> assertTrue

    // Sample usage test cases.
    do
        let evens, odds =
            [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
            |> HashMap.ofArray
            |> HashMap.partition (fun k v ->
                (k + int v) % 2 = 0)

        evens
        |> assertEqual
            (HashMap.ofArray [| (5, 'a'); (2, 'd'); (17, 'a'); (12, 'b'); |])

        odds
        |> assertEqual
            (HashMap.ofArray [| (3, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |])


[<Test>]
let mapPartition () : unit =
    // Test case for an empty map.
    do
        let evens, odds =
            HashMap.empty
            |> HashMap.mapPartition (fun k v ->
                if (k + int v) % 2 = 0 then
                    Choice1Of2 (k + 10)
                else
                    Choice2Of2 (System.Char.ToUpper v))

        evens
        |> HashMap.isEmpty
        |> assertTrue

        odds
        |> HashMap.isEmpty
        |> assertTrue

    // Sample usage test cases.
    do
        let evens, odds =
            [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
            |> HashMap.ofArray
            |> HashMap.mapPartition (fun k v ->
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

        evens |> assertEqual (HashMap.ofArray evensExpected)
        odds |> assertEqual (HashMap.ofArray oddsExpected)


(* MapModule and MapType tests from the F# source distribution (in FSharp.Core.Unittests). *)

module MapType =
    // Interfaces
    [<Test>]
    let IEnumerable () : unit =
        // Legit IE
        let ie = (HashMap.ofArray [|(1,1);(2,4);(3,9)|]) :> IEnumerable
        let enum = ie.GetEnumerator()

        let testStepping () : unit =
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
        let ie = [] |> HashMap.ofList :> IEnumerable  // Note no type args
        let enum = ie.GetEnumerator()

        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        Assert.AreEqual(enum.MoveNext(), false)
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)

    [<Test>]
    let IEnumerable_T () : unit =
        // Legit IE
        let ie = (HashMap.ofArray [|(1,1);(2,4);(3,9)|]) :> IEnumerable<KeyValuePair<_,_>>
        let enum = ie.GetEnumerator()

        let testStepping () : unit =
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
        let ie = [] |> HashMap.ofList :> IEnumerable  // Note no type args
        let enum = ie.GetEnumerator()

        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        Assert.AreEqual(enum.MoveNext(), false)
        checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)


    [<Test>]
    let IDictionary () : unit =
        // Legit ID
        let id = (HashMap.ofArray [|(1,1);(2,4);(3,9)|]) :> IDictionary<_,_>

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
        let id = HashMap.empty :> IDictionary<int, int>   // Note no type args
        Assert.IsFalse(id.ContainsKey(5))
        checkThrowsKeyNotFoundException(fun () -> id.[1] |> ignore)
        CollectionAssert.AreEqual(id.Keys,   [| |] :> ICollection<_>)
        CollectionAssert.AreEqual(id.Values, [| |] :> ICollection<_>)

    [<Test>]
    let ICollection () : unit =
        // Legit IC
        let ic = (HashMap.ofArray [|(1,1);(2,4);(3,9)|]) :> ICollection<KeyValuePair<_,_>>

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
        let ic = HashMap.empty :> ICollection<KeyValuePair<int, int>>
        Assert.IsFalse(ic.Contains(new KeyValuePair<int,int>(3,9)))
        let newArr = Array.create 5 (new KeyValuePair<int,int>(0,0))
        ic.CopyTo(newArr,0)

    [<Test>]
    let IComparable () : unit =
        // Legit IC
        let ic = (HashMap.ofArray [|(1,1);(2,4);(3,9)|]) :> IComparable
        Assert.AreEqual(ic.CompareTo([(1,1);(2,4);(3,9)]|> HashMap.ofList),0)
        Assert.AreEqual(ic.CompareTo([(1,1);(3,9);(2,4)]|> HashMap.ofList),0)
        Assert.AreEqual(ic.CompareTo([(1,1);(9,81);(2,4)]|> HashMap.ofList),-1)
        Assert.AreEqual(ic.CompareTo([(1,1);(0,0);(2,4)]|> HashMap.ofList),1)
        checkThrowsArgumentException(fun() -> ic.CompareTo([(1,1);(2,4);(3,9)]) |> ignore)

        // Empty IC
        let ic = [] |> HashMap.ofList :> IComparable
        Assert.AreEqual(ic.CompareTo([]|> HashMap.ofList),0)


    // Base class methods
    [<Test>]
    let ObjectGetHashCode () : unit =
        // Works on empty maps
        let e = HashMap.ofList (List.empty<int * decimal>)
        let m = HashMap.ofList [ (1, -1.0M) ]
        Assert.AreNotEqual(e.GetHashCode(), m.GetHashCode())

        // Should be order independent
        let x = HashMap.ofList [(1, -1.0M); (2, -2.0M)]
        let y = HashMap.ofList [(2, -2.0M); (1, -1.0M)]
        Assert.AreEqual(x.GetHashCode(), y.GetHashCode())

    [<Test>]
    let ObjectToString () : unit =
        Assert.AreEqual("hashMap [(1, 1); (2, 4); (3, 9)]", (HashMap.ofArray [|(1,1);(2,4);(3,9)|]).ToString())
        Assert.AreEqual("hashMap []", ([] |> HashMap.ofList).ToString())
        Assert.AreEqual("hashMap []",
                        (([] :(decimal*decimal)list) |> HashMap.ofList).ToString())

    [<Test>]
    let ObjectEquals () : unit =
        // All three are different references, but equality has been
        // provided by the F# compiler.
        let a = [(1,1);(2,4);(3,9)] |> HashMap.ofList
        let b = (1,1) :: [(2,4);(3,9)] |> HashMap.ofList
        Assert.IsTrue( (a = b) )

        Assert.IsTrue( a.Equals(b) ); Assert.IsTrue( b.Equals(a) )

        // Equality between types
        let a = ([] : (int*int) list)  |> HashMap.ofList
        let b = ([] : (string*string) list ) |> HashMap.ofList
        Assert.IsFalse( b.Equals(a) )
        Assert.IsFalse( a.Equals(b) )

        // Co/contra varience not supported
        let a = ([] : (string*string) list) |> HashMap.ofList
        let b = ([] : (System.IComparable*System.IComparable) list)    |> HashMap.ofList
        Assert.IsFalse(a.Equals(b))
        Assert.IsFalse(b.Equals(a))

        // Self equality
        let a = [(1,1)] |> HashMap.ofList
        Assert.IsTrue( (a = a) )
        Assert.IsTrue(a.Equals(a))

        // Null
        Assert.IsFalse(a.Equals(null))

    // Instance methods
    [<Test>]
    let New () : unit =
        let newHashMap = new HashMap<int,int>([|(1,1);(2,4);(3,9)|])
        let b = newHashMap.Add(4,16)
        Assert.AreEqual(b.[4], 16)
        Assert.AreEqual(b.[2], 4)

        let e  = new  HashMap<int,string>([])
        let ae = e.Add(1,"Monday")
        Assert.AreEqual(ae.[1], "Monday")

    let Add () : unit =

        let a = (HashMap.ofArray [|(1,1);(2,4);(3,9)|])
        let b = a.Add(4,16)
        Assert.AreEqual(b.[4], 16)
        Assert.AreEqual(b.[2], 4)

        let e  = HashMap.empty<int,string>
        let ae = e.Add(1,"Monday")
        Assert.AreEqual(ae.[1], "Monday")

    [<Test>]
    let ContainsKey () : unit =

        let a = (HashMap.ofArray [|(1,1);(2,4);(3,9)|])
        Assert.IsTrue(a.ContainsKey(3))

        let e  = HashMap.empty<int,string>
        Assert.IsFalse(e.ContainsKey(3))


    [<Test>]
    let Count () : unit =

        let a = (HashMap.ofArray [|(1,1);(2,4);(3,9)|])
        Assert.AreEqual(a.Count, 3)

        let e  = HashMap.empty<int,string>
        Assert.AreEqual(e.Count, 0)

    [<Test>]
    let IsEmpty () : unit =

        let l = (HashMap.ofArray [|(1,1);(2,4);(3,9)|])
        Assert.IsFalse(l.IsEmpty)

        let e = HashMap.empty<int,int>
        Assert.IsTrue(e.IsEmpty)

    [<Test>]
    let Item () : unit =

        let mutable l = [(1,1)] |> HashMap.ofList
        Assert.AreEqual(l.[1], 1)
        l <- l.Add(100,8)
        Assert.AreEqual(l.[100], 8)

        for testidx = 0 to 20 do
            let l = HashMap.ofSeq (seq { for i in 0..testidx do yield (i,i*i)})
            for i = 0 to l.Count - 1 do
                Assert.AreEqual(i*i, l.[i])
                Assert.AreEqual(i*i, l.Item(i))

        // Invalid index
        let l = (HashMap.ofArray [|(1,1);(2,4);(3,9)|])
        checkThrowsKeyNotFoundException(fun () -> l.[ -1 ] |> ignore)
        checkThrowsKeyNotFoundException(fun () -> l.[1000] |> ignore)

    [<Test>]
    let Remove () : unit =

        let l = (HashMap.ofArray [|(1,1);(2,4);(3,9)|])
        let rem = l.Remove(2)
        Assert.AreEqual(rem.Count, 2)
        checkThrowsKeyNotFoundException(fun () -> rem.[ 2 ] |> ignore)

        let e  = HashMap.empty<int,string>
        let ae = e.Remove(2)
        Assert.AreEqual(ae.Count, 0)

    [<Test>]
    let TryFind () : unit =

        let l = (HashMap.ofArray [|(1,1);(2,4);(3,9)|])
        let rem = l.TryFind(2)
        Assert.AreEqual(l.TryFind(2),Some 4)

        let e  = HashMap.empty<int,string>

        Assert.AreEqual(e.TryFind(2), None)


module MapModule =
    [<Test>]
    let empty () : unit =
        let emptyMap = HashMap.empty
        Assert.IsTrue(HashMap.isEmpty emptyMap)

        let a:HashMap<int,int>    = HashMap.empty<int,int>
        let b : HashMap<string,string> = HashMap.empty<string,string>
        let c : HashMap<int,string> = HashMap.empty<int,string>

        ()

    [<Test>]
    let add () : unit =
        // value keys
        let valueKeyMap = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = HashMap.add 1 "a" valueKeyMap
        Assert.AreEqual(resultValueMap.[1], "a")

        // reference keys
        let refMap = HashMap.ofSeq [for c in ["."; ".."; "..."; "...."] do yield (c, c.Length) ]
        let resultRefMap = HashMap.add  ""  0 refMap
        Assert.AreEqual(resultRefMap.[""] , 0)

        // empty HashMap
        let eptMap = HashMap.empty
        let resultEpt = HashMap.add 1 "a" eptMap
        Assert.AreEqual(resultEpt.[1], "a")

        // One-element HashMap
        let oeleMap = HashMap.ofSeq [(1, "one")]
        let resultOele = HashMap.add  7  "seven" oeleMap
        Assert.AreEqual(resultOele.[7], "seven")

        // extra test for add -- add some key which already exit in the HashMap
        let extMap = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultExt = HashMap.add 2 "dup" extMap
        Assert.AreEqual(resultExt.[2], "dup")


        ()

    [<Test>]
    let exists () : unit =
        // value keys
        let valueKeyMap = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = HashMap.exists (fun x y -> x > 3) valueKeyMap
        Assert.IsTrue(resultValueMap)


        // reference keys
        let refMap = HashMap.ofSeq [for c in ["."; ".."; "..."; "...."] do yield (c, c.Length) ]
        let resultRefMap = refMap |> HashMap.exists  (fun x y -> y>2 )
        Assert.IsTrue(resultRefMap)

        // One-element HashMap
        let oeleMap = HashMap.ofSeq [(1, "one")]
        let resultOele = oeleMap |> HashMap.exists  (fun x y -> (x + y.Length) % 4 = 0 )
        Assert.IsTrue(resultOele)

        // empty HashMap
        let eptMap = HashMap.empty
        let resultEpt = HashMap.exists (fun x y -> false) eptMap
        Assert.IsFalse(resultEpt)

        ()

    [<Test>]
    let filter () : unit =
        // value keys
        let valueKeyMap = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap =valueKeyMap |> HashMap.filter (fun x y -> x % 3 = 0)
        Assert.AreEqual(resultValueMap,[3,"c"] |> HashMap.ofList)

        // reference keys
        let refMap = HashMap.ofSeq [for c in ["."; ".."; "..."; "...."] do yield (c, c.Length) ]
        let resultRefMap = refMap |> HashMap.filter  (fun x y -> y  > 3 )
        Assert.AreEqual(resultRefMap,["....",4] |> HashMap.ofList)

        // One-element HashMap
        let oeleMap = HashMap.ofSeq [(1, "one")]
        let resultOele = oeleMap |> HashMap.filter  (fun x y -> x<3 )
        Assert.AreEqual(resultOele,oeleMap)

        // empty HashMap
        let eptMap = HashMap.empty
        let resultEpt = HashMap.filter (fun x y -> true) eptMap
        Assert.AreEqual(resultEpt,eptMap)

        ()


    [<Test>]
    let find () : unit =
        // value keys
        let valueKeyMap = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = HashMap.find 5 valueKeyMap
        Assert.AreEqual(resultValueMap,"e")

        // reference keys
        let refMap = HashMap.ofSeq [for c in ["."; ".."; "..."; "...."] do yield (c, c.Length) ]
        let resultRefMap = HashMap.find  ".." refMap
        Assert.AreEqual(resultRefMap,2)

        // One-element HashMap
        let oeleMap = HashMap.ofSeq [(1, "one")]
        let resultOele = HashMap.find 1 oeleMap
        Assert.AreEqual(resultOele,"one")

        // empty HashMap
        let eptMap = HashMap.empty
        checkThrowsKeyNotFoundException (fun () -> HashMap.find 1 eptMap |> ignore)

        ()

    [<Test>]
    let findIndex () : unit =
        // value keys
        let valueKeyMap = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap =valueKeyMap |> HashMap.findKey (fun x y -> x % 3 = 0)
        Assert.AreEqual(resultValueMap,3)

        // reference keys
        let refMap = HashMap.ofSeq [for c in ["."; ".."; "..."; "...."] do yield (c, c.Length) ]
        let resultRefMap = refMap |> HashMap.findKey  (fun x y -> y % 3 = 0 )
        Assert.AreEqual(resultRefMap,"...")

        // One-element HashMap
        let oeleMap = HashMap.ofSeq [(1, "one")]
        let resultOele = oeleMap |> HashMap.findKey  (fun x y -> x = 1 )
        Assert.AreEqual(resultOele,1)

        // empty HashMap
        let eptMap = HashMap.empty
        checkThrowsKeyNotFoundException (fun () -> HashMap.findKey (fun x y -> true) eptMap |> ignore)

        ()

    [<Test>]
    let tryPick () : unit =
        // value keys
        let valueKeyMap = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = valueKeyMap |> HashMap.tryPick (fun x y -> if x % 3 = 0 then Some (x) else None)
        Assert.AreEqual(resultValueMap,Some 3)

        // reference keys
        let refMap = HashMap.ofSeq [for c in ["."; ".."; "..."; "...."] do yield (c, c.Length) ]
        let resultRefMap = refMap |> HashMap.tryPick  (fun x y -> if (y % 3 = 0 ) then Some y else None )

        Assert.AreEqual(resultRefMap,Some 3)

        // One-element HashMap
        let oeleMap = HashMap.ofSeq [(1, "one")]
        let resultOele = oeleMap |> HashMap.tryPick  (fun x y -> if(x + y.Length) % 4 = 0 then Some y else None )
        Assert.AreEqual(resultOele,Some "one")

        // empty HashMap
        let eptMap = HashMap.empty
        let resultEpt = HashMap.tryPick (fun x y -> Some x) eptMap
        Assert.AreEqual(resultEpt,None)

        ()

    [<Test>]
    let pick () : unit =
        // value keys
        let valueKeyMap = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValue = valueKeyMap |> HashMap.pick (fun x y -> if x % 3 = 0 then Some (y) else None)
        Assert.AreEqual(resultValue, "c")

        // reference keys
        let refMap = HashMap.ofSeq [for c in ["."; ".."; "..."; "...."] do yield (c, c.Length) ]
        let result = refMap |> HashMap.pick (fun x y -> if (y % 3 = 0 ) then Some y else None )
        Assert.AreEqual(result, 3)

        // One-element HashMap
        let oeleMap = HashMap.ofSeq [(1, "one")]
        let resultOele = oeleMap |> HashMap.pick (fun x y -> if(x + y.Length) % 4 = 0 then Some y else None )
        Assert.AreEqual(resultOele, "one")

        // empty HashMap
        let eptMap = HashMap.empty
        let resultEpt =
            try
                HashMap.pick (fun x y -> Some x) eptMap
            with :? System.Collections.Generic.KeyNotFoundException -> Some 0
        Assert.AreEqual(resultEpt, Some 0)

        ()

    [<Test>]
    let fold () : unit =
        // value keys
        let valueKeyMap = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = valueKeyMap |> HashMap.fold (fun x y z -> x + y + z.Length) 10
        Assert.AreEqual(resultValueMap,28)

        // reference keys
        let refMap = HashMap.ofSeq [for c in ["."; ".."; "..."; "...."] do yield (c, c.Length) ]
        let resultRefMap = refMap |> HashMap.fold  (fun x y z -> (y,z) :: x)  [("initial",83)]
        CollectionAssert.AreEquivalent([("initial",83);("....",4);("...",3);("..",2);(".",1)], resultRefMap)
        Assert.AreEqual(("initial",83), resultRefMap |> Array.ofSeq |> Array.last)

        // One-element HashMap
        let oeleMap = HashMap.ofSeq [(1, "one")]
        let resultOele =   oeleMap |> HashMap.fold  (fun x y z -> x + y.ToString() + z)  "got"
        Assert.AreEqual("got1one", resultOele)

        // empty HashMap
        let eptMap = HashMap.empty
        let resultEpt = eptMap |> HashMap.fold (fun x y z -> 1) 1
        Assert.AreEqual(1,resultEpt)

        ()

    [<Test>]
    [<Ignore("Test is ignored because it relies on a certain traversal ordering, which doesn't make sense for a hash-based (unordered) map.")>]
    let foldBack () : unit =
        // value keys
        let valueKeyMap = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = HashMap.foldBack (fun x y z -> x.ToString() + y + z.ToString()) valueKeyMap "*"
        Assert.AreEqual("2b3c4d5e*", resultValueMap)

        // reference keys
        let refMap = HashMap.ofSeq [for c in ["."; ".."; "..."; "...."] do yield (c, c.Length) ]
        let resultRefMap = HashMap.foldBack  (fun x y z -> x + y.ToString() + z) refMap "right"
        Assert.AreEqual(".1..2...3....4right", resultRefMap)

        // One-element HashMap
        let oeleMap = HashMap.ofSeq [(1, "one")]
        let resultOele = HashMap.foldBack  (fun x y z -> x.ToString() + y + z) oeleMap "right"
        Assert.AreEqual("1oneright", resultOele)

        // empty HashMap
        let eptMap = HashMap.empty
        let resultEpt = HashMap.foldBack (fun x y z -> 1) eptMap 1
        Assert.AreEqual(1, resultEpt)

        ()

    [<Test>]
    let forall () : unit =
        // value keys
        let valueKeyMap = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = valueKeyMap |> HashMap.forall (fun x y -> x % 3 = 0)
        Assert.IsFalse(resultValueMap)

        // reference keys
        let refMap = HashMap.ofSeq [for c in ["."; ".."; "..."; "...."] do yield (c, c.Length) ]
        let resultRefMap = refMap |> HashMap.forall  (fun x y -> x.Length  > 4 )
        Assert.IsFalse(resultRefMap)

        // One-element HashMap
        let oeleMap = HashMap.ofSeq [(1, "one")]
        let resultOele = oeleMap |> HashMap.forall  (fun x y -> x<3 )
        Assert.IsTrue(resultOele)

        // empty HashMap
        let eptMap = HashMap.empty
        let resultEpt =eptMap |>  HashMap.forall (fun x y -> true)
        Assert.IsTrue(resultEpt)

        ()


    [<Test>]
    let isEmpty () : unit =
        // value keys
        let valueKeyMap = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = HashMap.isEmpty  valueKeyMap
        Assert.IsFalse(resultValueMap)

        // reference keys
        let refMap = HashMap.ofSeq [for c in ["."; ".."; "..."; "...."] do yield (c, c.Length) ]
        let resultRefMap = HashMap.isEmpty   refMap
        Assert.IsFalse(resultRefMap)

        // One-element HashMap
        let oeleMap = HashMap.ofSeq [(1, "one")]
        let resultOele = HashMap.isEmpty   oeleMap
        Assert.IsFalse(resultOele)

        // empty HashMap
        let eptMap = HashMap.empty
        let resultEpt = HashMap.isEmpty  eptMap
        Assert.IsTrue(resultEpt)

        ()

    [<Test>]
    let iter () : unit =
        // value keys
        let valueKeyMap = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = ref 0
        let funInt (x:int) (y:string) =
            resultValueMap := !resultValueMap + x + y.Length
            ()
        HashMap.iter funInt valueKeyMap
        Assert.AreEqual(!resultValueMap,18)

        // reference keys
        let refMap = HashMap.ofSeq [for c in ["."; ".."; "..."; "...."] do yield (c, c.Length) ]
        let resultRefMap = ResizeArray<string * int>()
        let funStr (x:string) (y:int) =
            resultRefMap.Add(x, y)
        HashMap.iter funStr refMap
        CollectionAssert.AreEquivalent([("....",4); ("...",3); ("..",2); (".",1)], resultRefMap)

        // One-element HashMap
        let oeleMap = HashMap.ofSeq [(1, "one")]
        let resultOele = ref ""
        let funMix  (x:int) (y:string) =
            resultOele := !resultOele + x.ToString() + y
            ()
        HashMap.iter funMix oeleMap
        Assert.AreEqual(!resultOele,"1one")

        // empty HashMap
        let eptMap = HashMap.empty
        let resultEpt = ref 0
        let funEpt (x:int) (y:int) =
            resultEpt := !resultEpt + x + y
            ()
        HashMap.iter funEpt eptMap
        Assert.AreEqual(!resultEpt,0)

        ()

    [<Test>]
    let map () : unit =

        // value keys
        let valueKeyMap = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = valueKeyMap |> HashMap.map (fun x y -> x.ToString() + y )
        Assert.AreEqual(resultValueMap,[(2,"2b"); (3,"3c"); (4,"4d"); (5,"5e")] |> HashMap.ofList)

        // reference keys
        let refMap = HashMap.ofSeq [for c in ["."; ".."; "..."; "...."] do yield (c, c.Length) ]
        let resultRefMap = refMap |> HashMap.map (fun x y -> x.Length + y )
        Assert.AreEqual(resultRefMap,[(".",2); ("..",4);( "...",6); ("....",8)] |> HashMap.ofList)

        // One-element HashMap
        let oeleMap = HashMap.ofSeq [(1, "one")]
        let resultOele = oeleMap |> HashMap.map  (fun x y -> x.ToString() + y )
        Assert.AreEqual(resultOele,[1,"1one"] |> HashMap.ofList)

        // empty HashMap
        let eptMap = HashMap.empty<int,int>
        let resultEpt = eptMap |> HashMap.map (fun x y -> x+y)
        Assert.AreEqual(resultEpt,eptMap)

        ()

    [<Test>]
    let contains () : unit =
        // value keys
        let valueKeyMap = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = HashMap.containsKey 2 valueKeyMap
        Assert.IsTrue(resultValueMap)

        // reference keys
        let refMap = HashMap.ofSeq [for c in ["."; ".."; "..."; "...."] do yield (c, c.Length) ]
        let resultRefMap = HashMap.containsKey ".."  refMap
        Assert.IsTrue(resultRefMap)

        // One-element HashMap
        let oeleMap = HashMap.ofSeq [(1, "one")]
        let resultOele = HashMap.containsKey 1 oeleMap
        Assert.IsTrue(resultOele)

        // empty HashMap
        let eptMap = HashMap.empty
        let resultEpt = HashMap.containsKey 3 eptMap
        Assert.IsFalse(resultEpt)

        ()

    [<Test>]
    let ofArray_ofList_ofSeq () : unit =
        // value keys
        let valueKeyMapOfArr = HashMap.ofArray [|(2,"b"); (3,"c"); (4,"d"); (5,"e")|]
        let valueKeyMapOfList = HashMap.ofList [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let valueKeyMapOfSeq = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]

        CollectionAssert.AreEquivalent (
            valueKeyMapOfArr, valueKeyMapOfList, "ofArray and ofList produced different results (with value keys).")
        CollectionAssert.AreEquivalent (
            valueKeyMapOfList,valueKeyMapOfSeq, "ofList and ofSeq produced different results (with value keys).")
        CollectionAssert.AreEquivalent (
            valueKeyMapOfArr,valueKeyMapOfSeq, "ofArray and ofSeq produced different results (with value keys).")

        // reference keys
        let refMapOfArr = HashMap.ofArray [|(".",1); ("..",2);( "...",3); ("....",4)|]
        let refMapOfList = HashMap.ofList [(".",1); ("..",2);( "...",3); ("....",4)]
        let refMapOfSeq = HashMap.ofSeq [for c in ["."; ".."; "..."; "...."] do yield (c, c.Length) ]

        CollectionAssert.AreEquivalent (refMapOfArr, refMapOfList, "ofArray and ofList produced different results (with reference keys).")
        CollectionAssert.AreEquivalent (refMapOfList, refMapOfSeq, "ofList and ofSeq produced different results (with reference keys).")
        CollectionAssert.AreEquivalent (refMapOfArr, refMapOfSeq, "ofArray and ofSeq produced different results (with reference keys).")

        // One-element HashMap
        let oeleMapOfArr = HashMap.ofArray [|(1,"one")|]
        let oeleMapOfList = HashMap.ofList [(1,"one") ]
        let oeleMapOfSeq = HashMap.ofSeq [(1,"one") ]

        CollectionAssert.AreEquivalent (oeleMapOfArr, oeleMapOfList, "ofArray and ofList produced different results (one-element maps).")
        CollectionAssert.AreEquivalent (oeleMapOfList, oeleMapOfSeq, "ofList and ofSeq produced different results (one-element maps).")
        CollectionAssert.AreEquivalent (oeleMapOfArr, oeleMapOfSeq, "ofArray and ofSeq produced different results (one-element maps).")

        ()

    [<Test>]
    let partition () : unit =
        // value keys
        let valueKeyMap = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = HashMap.partition (fun x y  -> x%2 = 0) valueKeyMap
        let choosed = [(2,"b"); (4,"d")] |> HashMap.ofList
        let notChoosed = [(3,"c"); (5,"e")] |> HashMap.ofList
        Assert.AreEqual(resultValueMap,(choosed,notChoosed))

        // reference keys
        let refMap = HashMap.ofSeq [for c in ["."; ".."; "..."; "...."] do yield (c, c.Length) ]
        let resultRefMap = refMap |>  HashMap.partition  (fun x y  -> x.Length >2 )
        let choosed = [( "...",3); ("....",4)] |> HashMap.ofList
        let notChoosed = [(".",1); ("..",2)] |> HashMap.ofList
        Assert.AreEqual(resultRefMap,(choosed,notChoosed))

        // One-element HashMap
        let oeleMap = HashMap.ofSeq [(1, "one")]
        let resultOele = HashMap.partition  (fun x y  -> x<4) oeleMap
        let choosed = [(1,"one")] |> HashMap.ofList
        let notChoosed = HashMap.empty<int,string>
        Assert.AreEqual(resultOele,(choosed,notChoosed))

        // empty HashMap
        let eptMap = HashMap.empty
        let resultEpt = HashMap.partition (fun x y  -> true) eptMap
        Assert.AreEqual(resultEpt,(eptMap,eptMap))

        ()


    [<Test>]
    let remove () : unit =
        // value keys
        let valueKeyMap = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = HashMap.remove 5 valueKeyMap
        Assert.AreEqual(resultValueMap,[(2,"b"); (3,"c"); (4,"d")] |> HashMap.ofList)

        // reference keys
        let refMap = HashMap.ofSeq [for c in ["."; ".."; "..."; "...."] do yield (c, c.Length) ]
        let resultRefMap = HashMap.remove  ".." refMap
        Assert.AreEqual(resultRefMap,[(".",1); ( "...",3); ("....",4)] |> HashMap.ofList)

        // One-element HashMap
        let oeleMap = HashMap.ofSeq [(1, "one")]
        let resultOele = HashMap.remove  1 oeleMap
        Assert.AreEqual(resultOele,HashMap.empty<int,string>)

        // Two-element HashMap
        let oeleMap = HashMap.ofSeq [(1, "one");(2,"Two")]
        let resultOele = HashMap.remove  1 oeleMap
        let exOele = HashMap.ofSeq [(2, "Two")]
        Assert.AreEqual(resultOele, exOele)

        // Item which want to be removed not included in the map
        let valueKeyMap = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = HashMap.remove 5 valueKeyMap
        Assert.AreEqual(resultValueMap,[(2,"b"); (3,"c"); (4,"d")] |> HashMap.ofList)


        ()

    [<Test>]
    let toArray () : unit =
        // value keys
        let valueKeyMapOfArr = HashMap.ofArray [|(1,1);(2,4);(3,9)|]
        let resultValueMap = HashMap.toArray valueKeyMapOfArr
        CollectionAssert.AreEquivalent(resultValueMap,[|(1,1);(2,4);(3,9)|])

        // reference keys
        let refMapOfArr = HashMap.ofArray [|(".",1); ("..",2);( "...",3); ("....",4)|]
        let resultRefMap = HashMap.toArray refMapOfArr
        CollectionAssert.AreEquivalent(resultRefMap,[|(".",1); ("..",2);( "...",3); ("....",4)|])

        // One-element HashMap
        let oeleMapOfArr = HashMap.ofArray [|(1,"one")|]
        let resultOele = HashMap.toArray oeleMapOfArr
        CollectionAssert.AreEquivalent(resultOele,[|(1,"one")|])

        // empty HashMap
        let eptMap = HashMap.ofArray [||]
        let resultEpt = HashMap.toArray eptMap
        CollectionAssert.AreEquivalent(resultEpt,[||])

        ()

    [<Test>]
    let toList () : unit =
        // value keys
        let valueKeyMapOfArr = HashMap.ofList [(1,1);(2,4);(3,9)]
        let resultValueMap = HashMap.toList valueKeyMapOfArr
        CollectionAssert.AreEquivalent(resultValueMap,[(1,1);(2,4);(3,9)])

        // reference keys
        let refMapOfArr = HashMap.ofList [(".",1); ("..",2);( "...",3); ("....",4)]
        let resultRefMap = HashMap.toList refMapOfArr
        CollectionAssert.AreEquivalent(resultRefMap,[(".",1); ("..",2);( "...",3); ("....",4)])

        // One-element HashMap
        let oeleMapOfArr = HashMap.ofList [(1,"one")]
        let resultOele = HashMap.toList oeleMapOfArr
        CollectionAssert.AreEquivalent(resultOele,[(1,"one")])

        // empty HashMap
        let eptMap = HashMap.empty<int,string>
        let resultEpt = HashMap.toList eptMap
        let eptList :(int*string) list = []
        CollectionAssert.AreEquivalent(resultEpt,eptList)

        ()

    [<Test>]
    let toSeq () : unit =
        // value keys
        let valueKeyMapOfArr = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = HashMap.toSeq valueKeyMapOfArr
        let originInt = seq { for i in 1..3 do yield (i,i*i)}
        CollectionAssert.AreEquivalent (
            resultValueMap,
            [(2,"b"); (3,"c"); (4,"d"); (5,"e")] :> IEnumerable)


        // reference keys
        let refMapOfArr = HashMap.ofSeq [(".",1); ("..",2);( "...",3); ("....",4)]
        let resultRefMap = HashMap.toSeq refMapOfArr
        let originStr = seq { for x in [  "is" ;"lists";"str"; "this"] do yield (x,x.ToUpper())}
        CollectionAssert.AreEquivalent (
            resultRefMap,
            [(".",1); ("..",2);( "...",3); ("....",4)] :> IEnumerable)


        // One-element HashMap
        let oeleMapOfArr = HashMap.ofSeq [(1,"one")]
        let resultOele = HashMap.toSeq oeleMapOfArr
        let originMix = seq { for x in [ "is" ;"str"; "this" ;"lists"] do yield (x.Length,x.ToUpper())}
        CollectionAssert.AreEquivalent (resultOele, [(1,"one")])


        ()

    [<Test>]
    let tryFind () : unit =
        // value keys
        let valueKeyMap = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = HashMap.tryFind 5 valueKeyMap
        Assert.AreEqual(resultValueMap,Some "e")

        // reference keys
        let refMap = HashMap.ofSeq [for c in ["."; ".."; "..."; "...."] do yield (c, c.Length) ]
        let resultRefMap = HashMap.tryFind  "..." refMap
        Assert.AreEqual(resultRefMap,Some 3)

        // One-element HashMap
        let oeleMap = HashMap.ofSeq [(1, "one")]
        let resultOele = HashMap.tryFind 1 oeleMap
        Assert.AreEqual(resultOele,Some "one")

        // empty HashMap
        let eptMap = HashMap.empty
        let resultEpt = HashMap.tryFind 1 eptMap
        Assert.AreEqual(resultEpt,None)

        ()

    [<Test>]
    let tryFindIndex () : unit =
        // value keys
        let valueKeyMap = HashMap.ofSeq [(2,"b"); (3,"c"); (4,"d"); (5,"e")]
        let resultValueMap = valueKeyMap |> HashMap.tryFindKey (fun x y -> x+y.Length >30)
        Assert.AreEqual(resultValueMap,None)

        // reference keys
        let refMap = HashMap.ofSeq [for c in ["."; ".."; "..."; "...."] do yield (c, c.Length) ]
        let resultRefMap = refMap |> HashMap.tryFindKey  (fun x y -> (x.Length+y)>6)
        Assert.AreEqual(resultRefMap,Some "....")

        // One-element HashMap
        let oeleMap = HashMap.ofSeq [(1, "one")]
        let resultOele = oeleMap |> HashMap.tryFindKey (fun x y -> y.Contains("o"))
        Assert.AreEqual(resultOele,Some 1)

        // empty HashMap
        let eptMap = HashMap.empty
        let resultEpt = HashMap.tryFindKey (fun x y -> x+y >30) eptMap
        Assert.AreEqual(resultEpt,None)

        ()


/// FsCheck-based tests for HashMap.
module FsCheck =
    open FsCheck

    /// FsCheck generators for HashMap.
    type HashMapGenerator =
        /// Generates an arbitrary HashMap instance.
        static member HashMap () : Arbitrary<HashMap<_,_>> =
            gen {
                let! keys = Arb.generate
                let! values = Arb.generate

                // It seems FsCheck requires the use of sequences here --
                // using List.fold2 to build the IntMap causes FsCheck to crash.
                let kvpSeq = (Seq.ofList keys, Seq.ofList values) ||> Seq.zip
                return HashMap.ofSeq kvpSeq
            } |> Arb.fromGen

    /// Registers the FsCheck generators so they're already loaded
    /// when NUnit runs the tests in this fixture.
    [<OneTimeSetUp>]
    let registerFsCheckGenerators =
        Arb.register<HashMapGenerator> () |> ignore


    (* Tests *)

    [<Test>]
    let ``prop addLookup``() =
        assertProp "addLookup" <| fun (k : int) v map ->
            HashMap.add k v map
            |> HashMap.containsKey k

