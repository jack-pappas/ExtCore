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

/// Unit tests for the ExtCore.Collections.IntBimap type and module.
module Tests.ExtCore.Collections.IntBimap

open System.Collections.Generic
open NUnit.Framework
//open FsCheck

// TODO : Implement tests for equality/comparison.

[<Test>]
let equality () : unit =
    IntBimap.empty = IntBimap.empty
    |> assertTrue

    IntBimap.singleton 11 "Hello"
    |> IntBimap.remove 11
    |> assertEqual (IntBimap.empty : IntBimap<string>)

    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> assertEqual (
        IntBimap.empty
        |> IntBimap.add 5 "foo"
        |> IntBimap.add 8 "bar"
        |> IntBimap.add 2 "baz"
        |> IntBimap.add 9 "cdr"
        |> IntBimap.add 6 "car")

[<Test>]
let singleton () : unit =
    IntBimap.singleton 11 "Hello"
    |> assertEqual (
        IntBimap.empty
        |> IntBimap.add 11 "Hello")

[<Test>]
let isEmpty () : unit =
    IntBimap.empty
    |> IntBimap.isEmpty
    |> assertTrue

    IntBimap.singleton 5 'f'
    |> IntBimap.isEmpty
    |> assertFalse

[<Test>]
let count () : unit =
    IntBimap.empty
    |> IntBimap.count
    |> assertEqual 0

    IntBimap.singleton 123 'F'
    |> IntBimap.count
    |> assertEqual 1

    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.count
    |> assertEqual 5

[<Test>]
let containsKey () : unit =
    IntBimap.empty
    |> IntBimap.containsKey 77
    |> assertFalse

    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.containsKey 3
    |> assertFalse

    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.containsKey 8
    |> assertTrue

[<Test>]
let containsValue () : unit =
    IntBimap.empty
    |> IntBimap.containsValue 5
    |> assertFalse

    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.containsValue "Hello"
    |> assertFalse

    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.containsValue "bar"
    |> assertTrue

    // Update one of the existing bindings with a new value then
    // check to make sure it was actually changed.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.add 8 "Hello"
    |> IntBimap.containsValue "bar"
    |> assertFalse

    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.add 8 "Hello"
    |> IntBimap.containsValue "Hello"
    |> assertTrue

[<Test>]
let paired () : unit =
    IntBimap.empty
    |> IntBimap.paired 5 "foo"
    |> assertFalse

    // Test case for when the map contains neither the key nor the value.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.paired 99 "Hello"
    |> assertFalse

    // Test case for when the map contains the key, but it is paired with a different value.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.paired 99 "car"
    |> assertFalse

    // Test case for when the map contains both the key and the value, but they are not paired.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.paired 2 "car"
    |> assertFalse

    // Test case for when the map contains both the key and the value and they are paired.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.paired 8 "bar"
    |> assertTrue

[<Test>]
let tryFind () : unit =
    IntBimap.empty
    |> IntBimap.tryFind 6
    |> assertEqual None

    // Test case for when the map does not contain the specified key.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.tryFind 3
    |> assertEqual None

    // Test case for when the map does contain the specified key.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.tryFind 5
    |> assertEqual (Some "foo")

    // Test case for when the map does contain the specified key,
    // but it has been updated with a new value.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.add 8 "Hello"
    |> IntBimap.tryFind 8
    |> assertEqual (Some "Hello")

[<Test>]
let tryFindValue () : unit =
    IntBimap.empty
    |> IntBimap.tryFindValue 5
    |> assertEqual None

    // Test case for when the map does not contain the specified value.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.tryFindValue "Hello"
    |> assertEqual None

    // Test case for when the map does contain the specified value.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.tryFindValue "foo"
    |> assertEqual (Some 5)

    // Test case for when the map did contain the specified value, but no longer
    // does because it has been overwritten/updated with a new value.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.add 8 "Hello"
    |> IntBimap.tryFindValue "bar"
    |> assertEqual None

    // Test case for when the map does contain the specified value,
    // and it overwrote/updated an existing value.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.add 8 "Hello"
    |> IntBimap.tryFindValue "Hello"
    |> assertEqual (Some 8)

[<Test>]
let find () : unit =
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.find 8
    |> assertEqual "bar"

[<Test>]
let ``find raises exn when key is not found`` () : unit =
    Assert.Throws<KeyNotFoundException>(fun () ->
        [(5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car")]
        |> IntBimap.ofList
        |> IntBimap.find 3
        |> ignore) |> ignore

[<Test>]
let findValue () : unit =
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.findValue "baz"
    |> assertEqual 2

[<Test>]
let ``findValue raises exn when value is not found`` () : unit =
    Assert.Throws<KeyNotFoundException>(fun () ->
        [(5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car")]
        |> IntBimap.ofList
        |> IntBimap.findValue "Hello"
        |> ignore) |> ignore

[<Test>]
let remove () : unit =
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.remove 6
    |> IntBimap.containsKey 6
    |> assertFalse

    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.remove 5
    |> IntBimap.remove 8
    |> IntBimap.count
    |> assertEqual 3

    // If the IntBimap does not contain a binding with the specified key,
    // the IntBimap should be returned without altering it.
    // TODO : Make the check below stronger -- it should check that the returned
    // object is the *same* IntBimap instance as the input.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.remove 3
    |> assertEqual
        (IntBimap.ofArray [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |])

[<Test>]
let removeValue () : unit =
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.removeValue "cdr"
    |> IntBimap.containsValue "cdr"
    |> assertFalse

    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.removeValue "foo"
    |> IntBimap.removeValue "bar"
    |> IntBimap.count
    |> assertEqual 3

    // If the IntBimap does not contain a binding with the specified value,
    // the IntBimap should be returned without altering it.
    // TODO : Make the check below stronger -- it should check that the returned
    // object is the *same* IntBimap instance as the input.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.removeValue "Hello"
    |> assertEqual
        (IntBimap.ofArray [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |])

[<Test>]
let add () : unit =
    IntBimap.empty
    |> IntBimap.add 5 "foo"
    |> IntBimap.isEmpty
    |> assertFalse

    // Test case for when neither the key nor the value being added exist in the IntBimap.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.add 11 "let"
    |> IntBimap.count
    |> assertEqual 6

    // Test case for when the key and value being added already exist in the IntBimap,
    // and they are paired.
    // TODO : Make this check stronger -- it should check that the returned object
    // is the *same* IntBimap instance as the input.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.add 8 "bar"
    |> assertEqual
        (IntBimap.ofArray [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |])

    // Test case for when the key and value already exist in the IntBimap, but they aren't paired.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.add 2 "bar"
    |> assertEqual
        (IntBimap.ofArray [|(5, "foo"); (2, "bar"); (9, "cdr"); (6, "car"); |])

    // Test case for when the key already exists in the IntBimap, but the value does not.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.add 8 "Hello"
    |> assertEqual
        (IntBimap.ofArray [| (5, "foo"); (8, "Hello"); (2, "baz"); (9, "cdr"); (6, "car"); |])

    // Test case for when the value already exists in the IntBimap, but the key does not.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.add 3 "foo"
    |> assertEqual
        (IntBimap.ofArray [| (3, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |])

[<Test>]
let tryAdd () : unit =
    // The empty map can always be added to.
    IntBimap.empty
    |> IntBimap.tryAdd 5 "foo"
    |> IntBimap.isEmpty
    |> assertFalse

    // Test case for when neither the key nor the value being added exist in the IntBimap.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.tryAdd 11 "let"
    |> IntBimap.count
    |> assertEqual 6

    // Test case for when the key and value being added already exist in the IntBimap,
    // and they are paired.
    // TODO : Make this check stronger -- it should check that the returned object
    // is the *same* IntBimap instance as the input.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.tryAdd 8 "bar"
    |> assertEqual
        (IntBimap.ofArray [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |])

    // Test case for when the key and value already exist in the IntBimap, but they aren't paired.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.tryAdd 2 "bar"
    |> assertEqual
        (IntBimap.ofArray [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |])

    // Test case for when the key already exists in the IntBimap, but the value does not.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.tryAdd 7 "bar"
    |> assertEqual
        (IntBimap.ofArray [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |])

    // Test case for when the value already exists in the IntBimap, but the key does not.
    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |]
    |> IntBimap.ofArray
    |> IntBimap.tryAdd 3 "foo"
    |> assertEqual
        (IntBimap.ofArray [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); |])

[<Test>]
let ofSeq () : unit =
    Seq.empty
    |> IntBimap.ofSeq
    |> IntBimap.isEmpty
    |> assertTrue

    [| (5, "foo") |]
    |> Seq.ofArray
    |> IntBimap.ofSeq
    |> assertEqual
        (IntBimap.singleton 5 "foo")

    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); (8, "Hello"); |]
    |> Seq.ofArray
    |> IntBimap.ofSeq
    |> assertEqual (
        IntBimap.empty
        |> IntBimap.add 5 "foo"
        |> IntBimap.add 8 "bar"
        |> IntBimap.add 2 "baz"
        |> IntBimap.add 9 "cdr"
        |> IntBimap.add 6 "car"
        |> IntBimap.add 8 "Hello")

[<Test>]
let ofList () : unit =
    List.empty
    |> IntBimap.ofList
    |> IntBimap.isEmpty
    |> assertTrue

    [(5, "foo")]
    |> IntBimap.ofList
    |> assertEqual
        (IntBimap.singleton 5 "foo")

    [(5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); (8, "Hello")]
    |> IntBimap.ofList
    |> assertEqual (
        IntBimap.empty
        |> IntBimap.add 5 "foo"
        |> IntBimap.add 8 "bar"
        |> IntBimap.add 2 "baz"
        |> IntBimap.add 9 "cdr"
        |> IntBimap.add 6 "car"
        |> IntBimap.add 8 "Hello")

[<Test>]
let ofArray () : unit =
    Array.empty
    |> IntBimap.ofArray
    |> IntBimap.isEmpty
    |> assertTrue

    [| (5, "foo") |]
    |> IntBimap.ofArray
    |> assertEqual
        (IntBimap.singleton 5 "foo")

    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); (8, "Hello"); |]
    |> IntBimap.ofArray
    |> assertEqual (
        IntBimap.empty
        |> IntBimap.add 5 "foo"
        |> IntBimap.add 8 "bar"
        |> IntBimap.add 2 "baz"
        |> IntBimap.add 9 "cdr"
        |> IntBimap.add 6 "car"
        |> IntBimap.add 8 "Hello")

//[<Test>]
//let ofMap () : unit =
//    Map.empty
//    |> IntBimap.ofMap
//    |> IntBimap.isEmpty
//    |> assertTrue
//
//    Map.singleton "foo" 5
//    |> IntBimap.ofMap
//    |> assertEqual
//        (IntBimap.singleton 5 "foo")
//
//    [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); (8, "Hello"); |]
//    |> Map.ofArray
//    |> IntBimap.ofMap
//    |> assertEqual
//        (IntBimap.ofArray [| (5, "foo"); (8, "bar"); (2, "baz"); (9, "cdr"); (6, "car"); (8, "Hello"); |])
//
//    // Test case for when the map contains multiple keys with the same value.
//    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); ("let", 9); |]
//    |> Map.ofArray
//    |> IntBimap.ofMap
//    |> assertEqual
//        (IntBimap.ofArray [| ("foo", 5); ("bar", 8); ("baz", 2); ("car", 6); ("let", 9); |])

[<Test>]
let toSeq () : unit =
    IntBimap.empty
    |> IntBimap.toSeq
    |> Seq.isEmpty
    |> assertTrue

    IntBimap.singleton 5 "foo"
    |> IntBimap.toSeq
    |> Seq.toArray
    |> assertEqual [| (5, "foo") |]

    IntBimap.empty
    |> IntBimap.add 5 "foo"
    |> IntBimap.add 8 "bar"
    |> IntBimap.add 2 "baz"
    |> IntBimap.add 9 "cdr"
    |> IntBimap.add 6 "car"
    |> IntBimap.add 8 "Hello"
    |> IntBimap.toSeq
    |> Seq.toArray
    |> assertEqual
        [| (2, "baz"); (5, "foo"); (6, "car"); (8, "Hello"); (9, "cdr"); |]

[<Test>]
let toList () : unit =
    IntBimap.empty
    |> IntBimap.toList
    |> List.isEmpty
    |> assertTrue

    IntBimap.singleton 5 "foo"
    |> IntBimap.toList
    |> assertEqual [(5, "foo")]

    IntBimap.empty
    |> IntBimap.add 5 "foo"
    |> IntBimap.add 8 "bar"
    |> IntBimap.add 2 "baz"
    |> IntBimap.add 9 "cdr"
    |> IntBimap.add 6 "car"
    |> IntBimap.add 8 "Hello"
    |> IntBimap.toList
    |> assertEqual
        [(2, "baz"); (5, "foo"); (6, "car"); (8, "Hello"); (9, "cdr")]

[<Test>]
let toArray () : unit =
    IntBimap.empty
    |> IntBimap.toArray
    |> Array.isEmpty
    |> assertTrue

    IntBimap.singleton 5 "foo"
    |> IntBimap.toArray
    |> assertEqual [| (5, "foo") |]

    IntBimap.empty
    |> IntBimap.add 5 "foo"
    |> IntBimap.add 8 "bar"
    |> IntBimap.add 2 "baz"
    |> IntBimap.add 9 "cdr"
    |> IntBimap.add 6 "car"
    |> IntBimap.add 8 "Hello"
    |> IntBimap.toArray
    |> assertEqual
        [| (2, "baz"); (5, "foo"); (6, "car"); (8, "Hello"); (9, "cdr"); |]

//[<Test>]
//let toMap () : unit =
//    IntBimap.empty
//    |> IntBimap.toMap
//    |> Map.isEmpty
//    |> assertTrue
//
//    IntBimap.singleton "foo" 5
//    |> IntBimap.toMap
//    |> assertEqual
//        (Map.singleton "foo" 5)
//
//    IntBimap.empty
//    |> IntBimap.add "foo" 5
//    |> IntBimap.add "bar" 8
//    |> IntBimap.add "baz" 2
//    |> IntBimap.add "cdr" 9
//    |> IntBimap.add "car" 6
//    |> IntBimap.add 8 "Hello"
//    |> IntBimap.toMap
//    |> assertEqual
//        (Map.ofArray [| ("bar", 8); ("baz", 2); ("car", 6); ("cdr", 9); ("foo", 5); ("bar", 7); |])

[<Test>]
let iter () : unit =
    do
        let elements = ResizeArray ()

        IntBimap.empty
        |> IntBimap.iter (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.isEmpty
        |> assertTrue

    do
        let elements = ResizeArray ()

        [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
        |> IntBimap.ofArray
        |> IntBimap.iter (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.toArray
        |> assertEqual
            [|'D'; 'G'; 'F'; 'B'; 'C'; 'A'|]

[<Test>]
let fold () : unit =
    do
        let elements = ResizeArray ()

        (0, IntBimap.empty)
        ||> IntBimap.fold (fun counter k v ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> assertEqual 0

        elements
        |> ResizeArray.isEmpty
        |> assertTrue

    do
        let elements = ResizeArray ()

        let testMap =
            [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
            |> IntBimap.ofArray

        (0, testMap)
        ||> IntBimap.fold (fun counter k v ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> assertEqual (IntBimap.count testMap)

        elements
        |> ResizeArray.toArray
        |> assertEqual
            [| 102; 76; 83; 113; 117; 119; |]

[<Test>]
let foldBack () : unit =
    do
        let elements = ResizeArray ()

        (IntBimap.empty, 0)
        ||> IntBimap.foldBack (fun counter k v ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> assertEqual 0

        elements
        |> ResizeArray.isEmpty
        |> assertTrue

    do
        let elements = ResizeArray ()

        let testMap =
            [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
            |> IntBimap.ofArray

        (testMap, 0)
        ||> IntBimap.foldBack (fun k v counter ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> assertEqual (IntBimap.count testMap)

        elements
        |> ResizeArray.toArray
        |> assertEqual
            [| 114; 114; 112; 84; 79; 107; |]

[<Test>]
let filter () : unit =
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> IntBimap.ofArray
    |> IntBimap.filter (fun k v ->
        (k % 2 = 0) && System.Char.IsLower v)
    |> assertEqual
        (IntBimap.ofArray [| (2, 'd'); (12, 'b'); (14, 'c'); |])

[<Test>]
let partition () : unit =
    do
        let evens, odds =
            IntBimap.empty
            |> IntBimap.partition (fun k v ->
                (k + int v) % 2 = 0)

        evens
        |> IntBimap.isEmpty
        |> assertTrue

        odds
        |> IntBimap.isEmpty
        |> assertTrue

    do
        let evens, odds =
            [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
            |> IntBimap.ofArray
            |> IntBimap.partition (fun k v ->
                (k + int v) % 2 = 0)

        evens
        |> assertEqual
            (IntBimap.ofArray [| (2, 'd'); (12, 'b'); (17, 'a'); |])

        odds
        |> assertEqual
            (IntBimap.ofArray [| (4, 'G'); (11, 'F'); (14, 'c'); |])
