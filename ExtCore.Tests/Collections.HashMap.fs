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
open FsUnit
//open FsCheck


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
    HashMap.empty
    |> HashMap.isEmpty
    |> should be True

    HashMap.singleton 1 'a'
    |> HashMap.isEmpty
    |> should be False

[<Test>]
let count () : unit =
    HashMap.empty
    |> HashMap.count
    |> assertEqual 0

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
    HashMap.singleton 1 'a'
    |> assertEqual (
        HashMap.add 1 'a' HashMap.empty)

    HashMap.singleton 1 'a'
    |> HashMap.count
    |> assertEqual 1

[<Test>]
let containsKey () : unit =
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> HashMap.ofArray
    |> HashMap.containsKey 5
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> HashMap.ofArray
    |> HashMap.containsKey 1
    |> should be False

[<Test>]
let tryFind () : unit =
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
    [(5, 'a'); (3, 'b')]
    |> HashMap.ofList
    |> HashMap.find 5
    |> assertEqual 'a'

[<Test; ExpectedException(typeof<KeyNotFoundException>)>]
let ``find raises exn when key is not found`` () : unit =
    [(5, 'a'); (3, 'b')]
    |> HashMap.ofList
    |> HashMap.find 9
    |> ignore

[<Test>]
let findOrDefault () : unit =
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
    (HashMap.empty : HashMap<int, string>)
    |> HashMap.tryFindKey (fun k v ->
        k % 2 = 0)
    |> assertEqual None

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

    HashMap.empty
    |> HashMap.add 5 'x'
    |> assertEqual (
        HashMap.singleton 5 'x')

[<Test>]
let remove () : unit =
    (HashMap.empty : HashMap<int, string>)
    |> HashMap.remove 5
    |> HashMap.isEmpty
    |> should be True

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
    |> should be True

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
    |> should be True

    map1.IsSubmapOfBy ((<=),
        HashMap.ofList [(1,1)])
    |> should be True

    map1.IsSubmapOfBy ((=),
        HashMap.ofList [(1,1);(2,2)])
    |> should be True

    map1.IsSubmapOfBy ((=),
        HashMap.ofList [(1,2)])
    |> should be False

    map1.IsSubmapOfBy ((<),
        HashMap.ofList [(1,1)])
    |> should be False

    let map2 = HashMap.ofList [(1,1)]

    map2.IsSubmapOfBy ((=),
        HashMap.ofList [(1,1);(2,2)])
    |> should be False
*)

[<Test>]
let ofSeq () : unit =
    (Seq.empty : seq<int * string>)
    |> HashMap.ofSeq
    |> HashMap.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> Seq.ofArray
    |> HashMap.ofSeq
    |> assertEqual
        (HashMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])

[<Test>]
let ofList () : unit =
    HashMap.ofList []
    |> HashMap.isEmpty
    |> should be True

    [(5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G')]
    |> HashMap.ofList
    |> assertEqual
        (HashMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])

[<Test>]
let ofArray () : unit =
    Array.empty
    |> HashMap.ofArray
    |> HashMap.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> assertEqual
        (HashMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])
(*
[<Test>]
let ofMap () : unit =
    Map.empty
    |> HashMap.ofMap
    |> HashMap.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |]
    |> Map.ofArray
    |> HashMap.ofMap
    |> assertEqual
        (HashMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])
*)
[<Test>]
let toSeq () : unit =
    HashMap.empty
    |> HashMap.toSeq
    |> Seq.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.toSeq
    |> Seq.toArray
    |> assertEqual
        [| (2, 'd'); (3, 'b'); (4, 'G'); (5, 'a'); (11, 'F'); (12, 'b'); (14, 'c'); (17, 'a'); |]

[<Test>]
let toList () : unit =
    HashMap.empty
    |> HashMap.toList
    |> List.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.toList
    |> assertEqual
        [(2, 'd'); (3, 'b'); (4, 'G'); (5, 'a'); (11, 'F'); (12, 'b'); (14, 'c'); (17, 'a')]

[<Test>]
let toArray () : unit =
    HashMap.empty
    |> HashMap.toArray
    |> Array.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.toArray
    |> assertEqual
        [| (2, 'd'); (3, 'b'); (4, 'G'); (5, 'a'); (11, 'F'); (12, 'b'); (14, 'c'); (17, 'a'); |]
(*
[<Test>]
let toMap () : unit =
    HashMap.empty
    |> HashMap.toMap
    |> Map.isEmpty
    |> should be True

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

[<Test; ExpectedException(typeof<KeyNotFoundException>)>]
let ``pick raises exn on empty input`` () : unit =
    HashMap.empty
    |> HashMap.pick (fun k v ->
        if (k + v) % 2 = 0 then
            Some (v + 1)
        else None)
    |> ignore

[<Test; ExpectedException(typeof<KeyNotFoundException>)>]
let ``pick raises exn when no match is found`` () : unit =
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.pick (fun k v ->
        if System.Char.IsControl v then
            Some (int v + k)
        else None)
    |> ignore

[<Test>]
let map () : unit =
    HashMap.empty
    |> HashMap.map (sprintf "%i:%c")
    |> HashMap.isEmpty
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> HashMap.ofArray
    |> HashMap.map (sprintf "%i:%c")
    |> assertEqual (HashMap.ofArray
        [| (5, "5:a"); (3, "3:b"); (11, "11:f"); (2, "2:d");
            (17, "17:a"); (4, "4:g"); (12, "12:b"); (14, "14:c"); |])

[<Test>]
let filter () : unit =
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.filter (fun k v ->
        (k % 2 = 0) && System.Char.IsLower v)
    |> assertEqual
        (HashMap.ofArray [| (2, 'd'); (12, 'b'); (14, 'c'); |])

[<Test>]
let choose () : unit =
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
    do
        let elements = ResizeArray ()

        HashMap.empty
        |> HashMap.iter (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.isEmpty
        |> should be True

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
    do
        let elements = ResizeArray ()

        HashMap.empty
        |> HashMap.iter (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.isEmpty
        |> should be True

    do
        let elements = ResizeArray ()

        [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
        |> HashMap.ofArray
        |> HashMap.iterBack (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.toArray
        |> assertEqual
            [| 'A'; 'C'; 'B'; 'F'; 'A'; 'G'; 'B'; 'D'; |]

[<Test>]
let fold () : unit =
    do
        let elements = ResizeArray ()

        (0, HashMap.empty)
        ||> HashMap.fold (fun counter k v ->
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
            |> HashMap.ofArray

        (0, testMap)
        ||> HashMap.fold (fun counter k v ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> assertEqual (HashMap.count testMap)

        elements
        |> ResizeArray.toArray
        |> assertEqual
            [| 102; 102; 77; 105; 85; 115; 119; 121; |]

[<Test>]
let foldBack () : unit =
    do
        let elements = ResizeArray ()

        (HashMap.empty, 0)
        ||> HashMap.foldBack (fun counter k v ->
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
            |> HashMap.ofArray

        (testMap, 0)
        ||> HashMap.foldBack (fun k v counter ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> assertEqual (HashMap.count testMap)

        elements
        |> ResizeArray.toArray
        |> assertEqual
            [| 114; 114; 112; 84; 106; 80; 107; 109; |]

[<Test>]
let exists () : unit =
    HashMap.empty
    |> HashMap.exists (fun k v ->
        (k + int v) % 2 = 0)
    |> should be False

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.exists (fun k v ->
        (k + int v) > 200)
    |> should be False

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.exists (fun k v ->
        (k + int v) % 2 = 0)
    |> should be True

[<Test>]
let forall () : unit =
    HashMap.empty
    |> HashMap.forall (fun k v ->
        (k + int v) < 200)
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.forall (fun k v ->
        (k + int v) < 200)
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> HashMap.ofArray
    |> HashMap.forall (fun k v ->
        (k + int v) % 2 = 0)
    |> should be False

[<Test>]
let partition () : unit =
    do
        let evens, odds =
            HashMap.empty
            |> HashMap.partition (fun k v ->
                (k + int v) % 2 = 0)

        evens
        |> HashMap.isEmpty
        |> should be True

        odds
        |> HashMap.isEmpty
        |> should be True

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
        |> should be True

        odds
        |> HashMap.isEmpty
        |> should be True

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


(* MapModule and MapType tests from the F# distribution. *)

// Interfaces
[<Test>]
let IEnumerable() =        
    // Legit IE
    let ie = (HashMap.ofArray [|(1,1);(2,4);(3,9)|]) :> IEnumerable
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
    let ie = [] |> HashMap.ofList :> IEnumerable  // Note no type args
    let enum = ie.GetEnumerator()
        
    checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
    Assert.AreEqual(enum.MoveNext(), false)
    checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)  
    
[<Test>]
let IEnumerable_T() =        
    // Legit IE
    let ie = (HashMap.ofArray [|(1,1);(2,4);(3,9)|]) :> IEnumerable<KeyValuePair<_,_>>
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
    let ie = [] |> HashMap.ofList :> IEnumerable  // Note no type args
    let enum = ie.GetEnumerator()
        
    checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
    Assert.AreEqual(enum.MoveNext(), false)
    checkThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)  
    
    
[<Test>]
let IDictionary() =        
    // Legit ID
    let id = (HashMap.ofArray [|(1,1);(2,4);(3,9)|]) :> IDictionary<_,_> 
        
    Assert.IsTrue(id.ContainsKey(1))   
    Assert.IsFalse(id.ContainsKey(5))  
    Assert.AreEqual(id.[1], 1)  
    Assert.AreEqual(id.[3], 9) 
    Assert.AreEqual(id.Keys,   [| 1; 2; 3|] :> ICollection<_>)
    Assert.AreEqual(id.Values, [| 1; 4; 9|] :> ICollection<_>)
        
    checkThrowsNotSupportedException(fun () -> id.[2] <-88)

    checkThrowsNotSupportedException(fun () -> id.Add(new KeyValuePair<int,int>(4,16)))
    Assert.IsTrue(id.TryGetValue(1, ref 1))
    Assert.IsFalse(id.TryGetValue(100, ref 1))
    checkThrowsNotSupportedException(fun () -> id.Remove(1) |> ignore)
        
    // Empty ID
    let id = HashMap.empty :> IDictionary<int, int>   // Note no type args  
    Assert.IsFalse(id.ContainsKey(5))
    checkThrowsKeyNotFoundException(fun () -> id.[1] |> ignore)  
    Assert.AreEqual(id.Keys,   [| |] :> ICollection<_>)
    Assert.AreEqual(id.Values, [| |] :> ICollection<_>) 
    
[<Test>]
let ICollection() =        
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
let IComparable() =        
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
let ObjectGetHashCode() =
    // Works on empty maps
    let e = HashMap.ofList (List.empty<int * decimal>)
    let m = HashMap.ofList [ (1, -1.0M) ]
    Assert.AreNotEqual(e.GetHashCode(), m.GetHashCode())
        
    // Should be order independent
    let x = HashMap.ofList [(1, -1.0M); (2, -2.0M)]
    let y = HashMap.ofList [(2, -2.0M); (1, -1.0M)]
    Assert.AreEqual(x.GetHashCode(), y.GetHashCode())
    
[<Test>]
let ObjectToString() =
    Assert.AreEqual("map [(1, 1); (2, 4); (3, 9)]", (HashMap.ofArray [|(1,1);(2,4);(3,9)|]).ToString())
    Assert.AreEqual("map []", ([] |> HashMap.ofList).ToString())
    Assert.AreEqual("map []", 
                    (([] :(decimal*decimal)list) |> HashMap.ofList).ToString())
    
[<Test>]
let ObjectEquals() =
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
let New() =    
    let newHashMap = new HashMap<int,int>([|(1,1);(2,4);(3,9)|])
    let b = newHashMap.Add(4,16)
    Assert.AreEqual(b.[4], 16)
    Assert.AreEqual(b.[2], 4)
    
    let e  = new  HashMap<int,string>([])
    let ae = e.Add(1,"Monday")
    Assert.AreEqual(ae.[1], "Monday")
        
let Add() =
    
    let a = (HashMap.ofArray [|(1,1);(2,4);(3,9)|])
    let b = a.Add(4,16)
    Assert.AreEqual(b.[4], 16)
    Assert.AreEqual(b.[2], 4)
    
    let e  = HashMap.empty<int,string>
    let ae = e.Add(1,"Monday")
    Assert.AreEqual(ae.[1], "Monday")
    
[<Test>]
let ContainsKey() =
    
    let a = (HashMap.ofArray [|(1,1);(2,4);(3,9)|])        
    Assert.IsTrue(a.ContainsKey(3))
    
    let e  = HashMap.empty<int,string>
    Assert.IsFalse(e.ContainsKey(3)) 
    
    
[<Test>]
let Count() =
    
    let a = (HashMap.ofArray [|(1,1);(2,4);(3,9)|])
    Assert.AreEqual(a.Count, 3)
    
    let e  = HashMap.empty<int,string>
    Assert.AreEqual(e.Count, 0) 
    
[<Test>]
let IsEmpty() =
    
    let l = (HashMap.ofArray [|(1,1);(2,4);(3,9)|])
    Assert.IsFalse(l.IsEmpty)
    
    let e = HashMap.empty<int,int>
    Assert.IsTrue(e.IsEmpty)        
    
[<Test>]
let Item() =

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
let Remove() =
    
    let l = (HashMap.ofArray [|(1,1);(2,4);(3,9)|])
    let rem = l.Remove(2)
    Assert.AreEqual(rem.Count, 2) 
    checkThrowsKeyNotFoundException(fun () -> rem.[ 2 ] |> ignore)
    
    let e  = HashMap.empty<int,string>
    let ae = e.Remove(2)
    Assert.AreEqual(ae.Count, 0)
        
[<Test>]
let TryFind() =
    
    let l = (HashMap.ofArray [|(1,1);(2,4);(3,9)|])
    let rem = l.TryFind(2)
    Assert.AreEqual(l.TryFind(2),Some 4)         
    
    let e  = HashMap.empty<int,string>
    
    Assert.AreEqual(e.TryFind(2), None)    


(* TODO : Implement FsCheck tests. *)

