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

/// Unit tests for the ExtCore.Collections.ArrayView module.
module Tests.ExtCore.Collections.ArrayView

open NUnit.Framework
open FsUnit
//open FsCheck


[<Test>]
let isEmpty () : unit =
    (2, 0)
    ||> ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |]
    |> ArrayView.isEmpty
    |> should be True

    (2, 3)
    ||> ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |]
    |> ArrayView.isEmpty
    |> should be False

[<Test>]
let ofArray () : unit =
    Array.empty
    |> ArrayView.ofArray
    |> ArrayView.isEmpty
    |> should be True

    [| 5; 3; 11; 2; 17; 4; 12; 14; |]
    |> ArrayView.ofArray
    |> ArrayView.count
    |> assertEqual 8

[<Test>]
let create () : unit =
    // Create an empty ArrayView of an empty array.
    ArrayView.create Array.empty 0 0
    |> ArrayView.isEmpty
    |> should be True

    // Create an empty ArrayView of a non-empty array.
    (2, 0)
    ||> ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |]
    |> ArrayView.isEmpty
    |> should be True

    // Create a non-empty ArrayView.
    (2, 3)
    ||> ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |]
    |> ArrayView.toArray
    |> assertEqual
        [| 11; 2; 17; |]

[<Test>]
let get () : unit =
    do
        let view = ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4

        ArrayView.get view 1
        |> assertEqual 17

        ArrayView.get view 3
        |> assertEqual 12

[<Test>]
let set () : unit =
    do
        let arr = [| 5; 3; 11; 2; 17; 4; 12; 14; |]
        let view = ArrayView.create arr 3 4

        // Set the array element value via the ArrayView.
        ArrayView.set view 1 9

        // Confirm the new element value via the ArrayView.
        ArrayView.get view 1
        |> assertEqual 9

        // Confirm the new element value by directly checking the underlying array.
        arr.[4] |> assertEqual 9

[<Test>]
let first () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.first
    |> assertEqual 2

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.first
    |> assertEqual 11

    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 47; |] 4 3
    |> ArrayView.first
    |> assertEqual 7

[<Test>]
let lastIndex () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.lastIndex
    |> assertEqual 6

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.lastIndex
    |> assertEqual 3

    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 47; |] 4 3
    |> ArrayView.lastIndex
    |> assertEqual 6

[<Test>]
let last () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.last
    |> assertEqual 12

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.last
    |> assertEqual 47

    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 47; |] 4 3
    |> ArrayView.last
    |> assertEqual 18

[<Test>]
let clear () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let toList () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.toList
    |> assertEqual
        [2; 17; 4; 12]

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.toList
    |> assertEqual
        [11; 23; 47]

    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 47; |] 4 3
    |> ArrayView.toList
    |> assertEqual
        [7; 11; 18]

[<Test>]
let toArray () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.toArray
    |> assertEqual
        [| 2; 17; 4; 12; |]

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.toArray
    |> assertEqual
        [| 11; 23; 47; |]

    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 47; |] 4 3
    |> ArrayView.toArray
    |> assertEqual
        [| 7; 11; 18; |]

[<Test>]
let mapToArray () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.mapToArray (fun x -> 3 * x)
    |> assertEqual
        [| 6; 51; 12; 36; |]

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.mapToArray (fun x -> 3 * x)
    |> assertEqual
        [| 33; 69; 141; |]

    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 47; |] 4 3
    |> ArrayView.mapToArray (fun x -> 3 * x)
    |> assertEqual
        [| 21; 33; 54; |]

[<Test>]
let tryPick () : unit =
    // Test case for an empty ArrayView.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 0
    |> ArrayView.tryPick (fun x ->
        if x % 3 = 0 && x % 4 = 0 then
            Some (sprintf "Match: %i" x)
        else None)
    |> assertEqual None

    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.tryPick (fun x ->
        if x % 3 = 0 && x % 4 = 0 then
            Some (sprintf "Match: %i" x)
        else None)
    |> assertEqual (Some "Match: 12")

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.tryPick (fun x ->
        if x % 3 = 0 && x % 4 = 0 then
            Some (sprintf "Match: %i" x)
        else None)
    |> assertEqual None

    // Test case for multiple matching values.
    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5
    |> ArrayView.tryPick (fun x ->
        if x % 2 = 0 && x % 3 = 0 then
            Some (sprintf "Match: %i" x)
        else None)
    |> assertEqual (Some "Match: 18")

[<Test>]
let pick () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.pick (fun x ->
        if x % 3 = 0 && x % 4 = 0 then
            Some (sprintf "Match: %i" x)
        else None)
    |> assertEqual "Match: 12"

    // Test case for multiple matching values.
    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5
    |> ArrayView.pick (fun x ->
        if x % 2 = 0 && x % 3 = 0 then
            Some (sprintf "Match: %i" x)
        else None)
    |> assertEqual "Match: 18"

[<Test>]
let tryFind () : unit =
    // Test case for an empty ArrayView.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 0
    |> ArrayView.tryFind (fun x ->
        x % 3 = 0 && x % 4 = 0)
    |> assertEqual None

    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.tryFind (fun x ->
        x % 3 = 0 && x % 4 = 0)
    |> assertEqual (Some 12)

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.tryFind (fun x ->
        x % 3 = 0 && x % 4 = 0)
    |> assertEqual None

    // Test case for multiple matching values.
    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5
    |> ArrayView.tryFind (fun x ->
        x % 2 = 0 && x % 3 = 0)
    |> assertEqual (Some 18)

[<Test>]
let find () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.find (fun x ->
        x % 3 = 0 && x % 4 = 0)
    |> assertEqual 12

    // Test case for multiple matching values.
    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5
    |> ArrayView.find (fun x ->
        x % 2 = 0 && x % 3 = 0)
    |> assertEqual 18

[<Test>]
let tryFindIndex () : unit =
    // Test case for an empty ArrayView.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 0
    |> ArrayView.tryFindIndex (fun x ->
        x % 3 = 0 && x % 4 = 0)
    |> assertEqual None

    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.tryFindIndex (fun x ->
        x % 3 = 0 && x % 4 = 0)
    |> assertEqual (Some 6)

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.tryFindIndex (fun x ->
        x % 3 = 0 && x % 4 = 0)
    |> assertEqual None

    // Test case for multiple matching values.
    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5
    |> ArrayView.tryFindIndex (fun x ->
        x % 2 = 0 && x % 3 = 0)
    |> assertEqual (Some 6)

[<Test>]
let findIndex () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.findIndex (fun x ->
        x % 3 = 0 && x % 4 = 0)
    |> assertEqual 6

    // Test case for multiple matching values.
    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5
    |> ArrayView.findIndex (fun x ->
        x % 2 = 0 && x % 3 = 0)
    |> assertEqual 6

[<Test>]
let exists () : unit =
    // Test case for an empty ArrayView.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 0
    |> ArrayView.exists (fun x ->
        x % 3 = 0 && x % 4 = 0)
    |> should be False

    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.exists (fun x ->
        x % 3 = 0 && x % 4 = 0)
    |> should be True

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.exists (fun x ->
        x % 3 = 0 && x % 4 = 0)
    |> should be False

    // Test case for multiple matching values.
    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5
    |> ArrayView.exists (fun x ->
        x % 2 = 0 && x % 3 = 0)
    |> should be True

[<Test>]
let forall () : unit =
    // Test case for an empty ArrayView.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 0
    |> ArrayView.forall (fun x ->
        x % 3 = 0 && x % 4 = 0)
    |> should be True

    // Test case for multiple matching values.
    ArrayView.create [| 5; 3; 11; 2; 16; 4; 12; 14; |] 3 4
    |> ArrayView.forall (fun x ->
        x % 2 = 0)
    |> should be True

    // Sample usage test cases.
    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.forall (fun x ->
        x % 3 = 0 && x % 4 = 0)
    |> should be False
    
    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5
    |> ArrayView.forall (fun x ->
        x % 2 = 0 && x % 3 = 0)
    |> should be False

[<Test>]
let iter () : unit =
    do
        // Test case for an empty ArrayView.
        let elements = ResizeArray ()

        ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 0
        |> ArrayView.iter (fun x ->
            elements.Add (x * 3))

        ResizeArray.isEmpty elements
        |> should be True

    do
        // Sample usage test case.
        let elements = ResizeArray ()

        ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
        |> ArrayView.iter (fun x ->
            elements.Add (x * 3))

        ResizeArray.toArray elements
        |> assertEqual
            [| 6; 51; 12; 36; |]

    do
        // Sample usage test case.
        let elements = ResizeArray ()

        ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
        |> ArrayView.iter (fun x ->
            elements.Add (x * 3))

        ResizeArray.toArray elements
        |> assertEqual
            [| 33; 69; 141; |]

    do
        // Sample usage test case.
        let elements = ResizeArray ()

        ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 47; |] 4 3
        |> ArrayView.iter (fun x ->
            elements.Add (x * 3))

        ResizeArray.toArray elements
        |> assertEqual
            [| 21; 33; 54; |]

[<Test>]
let fold () : unit =
    // Test case for an empty ArrayView.
    (String.empty, ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 0)
    ||> ArrayView.fold (fun state x ->
        state + x.ToString())
    |> assertEqual String.empty

    // Sample usage test cases.
    (String.empty, ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4)
    ||> ArrayView.fold (fun state x ->
        state + x.ToString())
    |> assertEqual "217412"

    (String.empty, ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3)
    ||> ArrayView.fold (fun state x ->
        state + x.ToString())
    |> assertEqual "112347"

    (String.empty, ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5)
    ||> ArrayView.fold (fun state x ->
        state + x.ToString())
    |> assertEqual "711182948"

[<Test>]
let foldBack () : unit =
    // Test case for an empty ArrayView.
    (ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 0, String.empty)
    ||> ArrayView.foldBack (fun x state ->
        state + x.ToString())
    |> assertEqual String.empty

    // Sample usage test cases.
    (ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4, String.empty)
    ||> ArrayView.foldBack (fun x state ->
        state + x.ToString())
    |> assertEqual "124172"

    (ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3, String.empty)
    ||> ArrayView.foldBack (fun x state ->
        state + x.ToString())
    |> assertEqual "472311"

    (ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5, String.empty)
    ||> ArrayView.foldBack (fun x state ->
        state + x.ToString())
    |> assertEqual "482918117"

[<Test>]
let reduce () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.reduce (fun x y ->
        (x - y) * y)
    |> assertEqual -12576

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.reduce (fun x y ->
        (x - y) * y)
    |> assertEqual -15181

    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5
    |> ArrayView.reduce (fun x y ->
        (x - y) * y)
    |> assertEqual -1596144

[<Test>]
let reduceBack () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.reduceBack (fun x y ->
        (y - x) * x)
    |> assertEqual 506

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.reduceBack (fun x y ->
        (y - x) * x)
    |> assertEqual 5951

    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5
    |> ArrayView.reduceBack (fun x y ->
        (y - x) * x)
    |> assertEqual 737842

[<Test>]
let min () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.min
    |> assertEqual 2

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.min
    |> assertEqual 11
    
    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 0 5
    |> ArrayView.min
    |> assertEqual 1

[<Test>]
let max () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.max
    |> assertEqual 17

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.max
    |> assertEqual 47
    
    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 0 5
    |> ArrayView.max
    |> assertEqual 7

[<Test>]
let sum () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.sum
    |> assertEqual 35

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.sum
    |> assertEqual 81
    
    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 0 5
    |> ArrayView.sum
    |> assertEqual 17

[<Test>]
let mapInPlace () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let mapiInPlace () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let chooseInPlace () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let chooseiInPlace () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let average () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let averageBy () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let maxBy () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let minBy () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let sumBy () : unit =
    Assert.Ignore "Test not yet implemented."
