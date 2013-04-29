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

/// Unit tests for the ExtCore.Collections.Bimap type and module.
module Tests.ExtCore.Collections.Bimap

open System.Collections.Generic
open NUnit.Framework
open FsUnit
//open FsCheck

// TODO : Implement tests for equality/comparison.

[<Test>]
let equality () : unit =
    Bimap.empty = Bimap.empty
    |> should be True

    Bimap.singleton "Hello" "World!"
    |> Bimap.remove "Hello"
    |> assertEqual (Bimap.empty : Bimap<string, string>)

    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> assertEqual (
        Bimap.empty
        |> Bimap.add "foo" 5
        |> Bimap.add "bar" 8
        |> Bimap.add "baz" 2
        |> Bimap.add "cdr" 9
        |> Bimap.add "car" 6)

[<Test>]
let singleton () : unit =
    Bimap.singleton "Hello" "World!"
    |> assertEqual (
        Bimap.empty
        |> Bimap.add "Hello" "World!")

[<Test>]
let isEmpty () : unit =
    Bimap.empty
    |> Bimap.isEmpty
    |> should be True

    Bimap.singleton 5 'f'
    |> Bimap.isEmpty
    |> should be False

[<Test>]
let count () : unit =
    Bimap.empty
    |> Bimap.count
    |> assertEqual 0

    Bimap.singleton 'F' 123
    |> Bimap.count
    |> assertEqual 1

    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.count
    |> assertEqual 5

[<Test>]
let containsKey () : unit =
    Bimap.empty
    |> Bimap.containsKey "foo"
    |> should be False

    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.containsKey "Hello"
    |> should be False

    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.containsKey "bar"
    |> should be True

[<Test>]
let containsValue () : unit =
    Bimap.empty
    |> Bimap.containsValue 5
    |> should be False

    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.containsValue 4
    |> should be False

    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.containsValue 8
    |> should be True

    // Update one of the existing bindings with a new value then
    // check to make sure it was actually changed.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.add "bar" 7
    |> Bimap.containsValue 8
    |> should be False

    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.add "bar" 7
    |> Bimap.containsValue 7
    |> should be True

[<Test>]
let paired () : unit =
    Bimap.empty
    |> Bimap.paired "foo" 5
    |> should be False

    // Test case for when the map contains neither the key nor the value.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.paired "Hello" 99
    |> should be False

    // Test case for when the map contains the key, but it is paired with a different value.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.paired "car" 99
    |> should be False

    // Test case for when the map contains both the key and the value, but they are not paired.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.paired "car" 2
    |> should be False

    // Test case for when the map contains both the key and the value and they are paired.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.paired "bar" 8
    |> should be True

[<Test>]
let tryFind () : unit =
    Bimap.empty
    |> Bimap.tryFind "foo"
    |> assertEqual None

    // Test case for when the map does not contain the specified key.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.tryFind "Hello"
    |> assertEqual None

    // Test case for when the map does contain the specified key.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.tryFind "foo"
    |> assertEqual (Some 5)

    // Test case for when the map does contain the specified key,
    // but it has been updated with a new value.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.add "bar" 7
    |> Bimap.tryFind "bar"
    |> assertEqual (Some 7)

[<Test>]
let tryFindValue () : unit =
    Bimap.empty
    |> Bimap.tryFindValue 5
    |> assertEqual None

    // Test case for when the map does not contain the specified value.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.tryFindValue 33
    |> assertEqual None

    // Test case for when the map does contain the specified value.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.tryFindValue 5
    |> assertEqual (Some "foo")

    // Test case for when the map did contain the specified value, but no longer
    // does because it has been overwritten/updated with a new value.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.add "bar" 7
    |> Bimap.tryFindValue 8
    |> assertEqual None

    // Test case for when the map does contain the specified value,
    // and it overwrote/updated an existing value.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.add "bar" 7
    |> Bimap.tryFindValue 7
    |> assertEqual (Some "bar")

[<Test>]
let find () : unit =
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.find "bar"
    |> assertEqual 8

[<Test; ExpectedException(typeof<KeyNotFoundException>)>]
let ``find raises exn when key is not found`` () : unit =
    [("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6)]
    |> Bimap.ofList
    |> Bimap.find "Hello"
    |> ignore

[<Test>]
let findValue () : unit =
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.findValue 2
    |> assertEqual "baz"

[<Test; ExpectedException(typeof<KeyNotFoundException>)>]
let ``findValue raises exn when value is not found`` () : unit =
    [("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6)]
    |> Bimap.ofList
    |> Bimap.findValue 33
    |> ignore

[<Test>]
let remove () : unit =
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.remove "cdr"
    |> Bimap.containsKey "cdr"
    |> should be False

    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.remove "foo"
    |> Bimap.remove "bar"
    |> Bimap.count
    |> assertEqual 3

    // If the Bimap does not contain a binding with the specified key,
    // the Bimap should be returned without altering it.
    // TODO : Make the check below stronger -- it should check that the returned
    // object is the *same* Bimap instance as the input.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.remove "Hello"
    |> assertEqual
        (Bimap.ofArray [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |])

[<Test>]
let removeValue () : unit =
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.removeValue 9
    |> Bimap.containsValue 9
    |> should be False

    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.removeValue 5
    |> Bimap.removeValue 8
    |> Bimap.count
    |> assertEqual 3

    // If the Bimap does not contain a binding with the specified value,
    // the Bimap should be returned without altering it.
    // TODO : Make the check below stronger -- it should check that the returned
    // object is the *same* Bimap instance as the input.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.removeValue 33
    |> assertEqual
        (Bimap.ofArray [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |])

[<Test>]
let add () : unit =
    Bimap.empty
    |> Bimap.add "foo" 5
    |> Bimap.isEmpty
    |> should be False

    // Test case for when neither the key nor the value being added exist in the Bimap.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.add "let" 11
    |> Bimap.count
    |> assertEqual 6

    // Test case for when the key and value being added already exist in the Bimap,
    // and they are paired.
    // TODO : Make this check stronger -- it should check that the returned object
    // is the *same* Bimap instance as the input.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.add "bar" 8
    |> assertEqual
        (Bimap.ofArray [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |])
    
    // Test case for when the key and value already exist in the Bimap, but they aren't paired.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.add "bar" 2
    |> assertEqual
        (Bimap.ofArray [| ("foo", 5); ("bar", 2); ("cdr", 9); ("car", 6); |])

    // Test case for when the key already exists in the Bimap, but the value does not.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.add "bar" 7
    |> assertEqual
        (Bimap.ofArray [| ("foo", 5); ("bar", 7); ("baz", 2); ("cdr", 9); ("car", 6); |])

    // Test case for when the value already exists in the Bimap, but the key does not.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.add "let" 5
    |> assertEqual
        (Bimap.ofArray [| ("let", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |])

[<Test>]
let tryAdd () : unit =
    // The empty map can always be added to.
    Bimap.empty
    |> Bimap.tryAdd "foo" 5
    |> Bimap.isEmpty
    |> should be False

    // Test case for when neither the key nor the value being added exist in the Bimap.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.tryAdd "let" 11
    |> Bimap.count
    |> assertEqual 6

    // Test case for when the key and value being added already exist in the Bimap,
    // and they are paired.
    // TODO : Make this check stronger -- it should check that the returned object
    // is the *same* Bimap instance as the input.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.tryAdd "bar" 8
    |> assertEqual
        (Bimap.ofArray [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |])
    
    // Test case for when the key and value already exist in the Bimap, but they aren't paired.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.tryAdd "bar" 2
    |> assertEqual
        (Bimap.ofArray [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |])

    // Test case for when the key already exists in the Bimap, but the value does not.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.tryAdd "bar" 7
    |> assertEqual
        (Bimap.ofArray [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |])

    // Test case for when the value already exists in the Bimap, but the key does not.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |]
    |> Bimap.ofArray
    |> Bimap.tryAdd "let" 5
    |> assertEqual
        (Bimap.ofArray [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); |])

[<Test>]
let ofSeq () : unit =
    Seq.empty
    |> Bimap.ofSeq
    |> Bimap.isEmpty
    |> should be True

    [| ("foo", 5) |]
    |> Seq.ofArray
    |> Bimap.ofSeq
    |> assertEqual
        (Bimap.singleton "foo" 5)

    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); ("bar", 7); |]
    |> Seq.ofArray
    |> Bimap.ofSeq
    |> assertEqual (
        Bimap.empty
        |> Bimap.add "foo" 5
        |> Bimap.add "bar" 8
        |> Bimap.add "baz" 2
        |> Bimap.add "cdr" 9
        |> Bimap.add "car" 6
        |> Bimap.add "bar" 7)

[<Test>]
let ofList () : unit =
    List.empty
    |> Bimap.ofList
    |> Bimap.isEmpty
    |> should be True

    [("foo", 5)]
    |> Bimap.ofList
    |> assertEqual
        (Bimap.singleton "foo" 5)

    [("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); ("bar", 7)]
    |> Bimap.ofList
    |> assertEqual (
        Bimap.empty
        |> Bimap.add "foo" 5
        |> Bimap.add "bar" 8
        |> Bimap.add "baz" 2
        |> Bimap.add "cdr" 9
        |> Bimap.add "car" 6
        |> Bimap.add "bar" 7)

[<Test>]
let ofArray () : unit =
    Array.empty
    |> Bimap.ofArray
    |> Bimap.isEmpty
    |> should be True

    [| ("foo", 5) |]
    |> Bimap.ofArray
    |> assertEqual
        (Bimap.singleton "foo" 5)

    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); ("bar", 7); |]
    |> Bimap.ofArray
    |> assertEqual (
        Bimap.empty
        |> Bimap.add "foo" 5
        |> Bimap.add "bar" 8
        |> Bimap.add "baz" 2
        |> Bimap.add "cdr" 9
        |> Bimap.add "car" 6
        |> Bimap.add "bar" 7)

[<Test>]
let ofMap () : unit =
    Map.empty
    |> Bimap.ofMap
    |> Bimap.isEmpty
    |> should be True

    Map.singleton "foo" 5
    |> Bimap.ofMap
    |> assertEqual
        (Bimap.singleton "foo" 5)

    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); ("bar", 7); |]
    |> Map.ofArray
    |> Bimap.ofMap
    |> assertEqual
        (Bimap.ofArray [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); ("bar", 7); |])

    // Test case for when the map contains multiple keys with the same value.
    [| ("foo", 5); ("bar", 8); ("baz", 2); ("cdr", 9); ("car", 6); ("let", 9); |]
    |> Map.ofArray
    |> Bimap.ofMap
    |> assertEqual
        (Bimap.ofArray [| ("foo", 5); ("bar", 8); ("baz", 2); ("car", 6); ("let", 9); |])

[<Test>]
let toSeq () : unit =
    Bimap.empty
    |> Bimap.toSeq
    |> Seq.isEmpty
    |> should be True

    Bimap.singleton "foo" 5
    |> Bimap.toSeq
    |> Seq.toArray
    |> assertEqual [| ("foo", 5) |]

    Bimap.empty
    |> Bimap.add "foo" 5
    |> Bimap.add "bar" 8
    |> Bimap.add "baz" 2
    |> Bimap.add "cdr" 9
    |> Bimap.add "car" 6
    |> Bimap.add "bar" 7
    |> Bimap.toSeq
    |> Seq.toArray
    |> assertEqual
        [| ("bar", 7); ("baz", 2); ("car", 6); ("cdr", 9); ("foo", 5); |]

[<Test>]
let toList () : unit =
    Bimap.empty
    |> Bimap.toList
    |> List.isEmpty
    |> should be True

    Bimap.singleton "foo" 5
    |> Bimap.toList
    |> assertEqual [("foo", 5)]

    Bimap.empty
    |> Bimap.add "foo" 5
    |> Bimap.add "bar" 8
    |> Bimap.add "baz" 2
    |> Bimap.add "cdr" 9
    |> Bimap.add "car" 6
    |> Bimap.add "bar" 7
    |> Bimap.toList
    |> assertEqual
        [("bar", 7); ("baz", 2); ("car", 6); ("cdr", 9); ("foo", 5)]

[<Test>]
let toArray () : unit =
    Bimap.empty
    |> Bimap.toArray
    |> Array.isEmpty
    |> should be True

    Bimap.singleton "foo" 5
    |> Bimap.toArray
    |> assertEqual [| ("foo", 5) |]

    Bimap.empty
    |> Bimap.add "foo" 5
    |> Bimap.add "bar" 8
    |> Bimap.add "baz" 2
    |> Bimap.add "cdr" 9
    |> Bimap.add "car" 6
    |> Bimap.add "bar" 7
    |> Bimap.toArray
    |> assertEqual
        [| ("bar", 7); ("baz", 2); ("car", 6); ("cdr", 9); ("foo", 5); |]

[<Test>]
let toMap () : unit =
    Bimap.empty
    |> Bimap.toMap
    |> Map.isEmpty
    |> should be True

    Bimap.singleton "foo" 5
    |> Bimap.toMap
    |> assertEqual
        (Map.singleton "foo" 5)

    Bimap.empty
    |> Bimap.add "foo" 5
    |> Bimap.add "bar" 8
    |> Bimap.add "baz" 2
    |> Bimap.add "cdr" 9
    |> Bimap.add "car" 6
    |> Bimap.add "bar" 7
    |> Bimap.toMap
    |> assertEqual
        (Map.ofArray [| ("bar", 8); ("baz", 2); ("car", 6); ("cdr", 9); ("foo", 5); ("bar", 7); |])

[<Test>]
let iter () : unit =
    do
        let elements = ResizeArray ()

        Bimap.empty
        |> Bimap.iter (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.isEmpty
        |> should be True

    do
        let elements = ResizeArray ()

        [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
        |> Bimap.ofArray
        |> Bimap.iter (fun _ v ->
            elements.Add (System.Char.ToUpper v))

        elements
        |> ResizeArray.toArray
        |> assertEqual
            [|'D'; 'G'; 'F'; 'B'; 'C'; 'A'|]

[<Test>]
let fold () : unit =
    do
        let elements = ResizeArray ()

        (0, Bimap.empty)
        ||> Bimap.fold (fun counter k v ->
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
            |> Bimap.ofArray

        (0, testMap)
        ||> Bimap.fold (fun counter k v ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> assertEqual (Bimap.count testMap)

        elements
        |> ResizeArray.toArray
        |> assertEqual
            [| 102; 76; 83; 113; 117; 119; |]

[<Test>]
let foldBack () : unit =
    do
        let elements = ResizeArray ()

        (Bimap.empty, 0)
        ||> Bimap.foldBack (fun counter k v ->
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
            |> Bimap.ofArray

        (testMap, 0)
        ||> Bimap.foldBack (fun k v counter ->
            elements.Add (counter + k + int v)
            counter + 1)
        |> assertEqual (Bimap.count testMap)

        elements
        |> ResizeArray.toArray
        |> assertEqual
            [| 114; 114; 112; 84; 79; 107; |]

[<Test>]
let filter () : unit =
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
    |> Bimap.ofArray
    |> Bimap.filter (fun k v ->
        (k % 2 = 0) && System.Char.IsLower v)
    |> assertEqual
        (Bimap.ofArray [| (2, 'd'); (12, 'b'); (14, 'c'); |])

[<Test>]
let partition () : unit =
    do
        let evens, odds =
            Bimap.empty
            |> Bimap.partition (fun k v ->
                (k + int v) % 2 = 0)

        evens
        |> Bimap.isEmpty
        |> should be True

        odds
        |> Bimap.isEmpty
        |> should be True

    do
        let evens, odds =
            [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd'); (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (11, 'F'); (4, 'G'); |]
            |> Bimap.ofArray
            |> Bimap.partition (fun k v ->
                (k + int v) % 2 = 0)

        evens
        |> assertEqual
            (Bimap.ofArray [| (2, 'd'); (12, 'b'); (17, 'a'); |])

        odds
        |> assertEqual
            (Bimap.ofArray [| (4, 'G'); (11, 'F'); (14, 'c'); |])


