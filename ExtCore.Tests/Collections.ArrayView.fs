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


[<TestCase>]
let isEmpty () : unit =
    (2, 0)
    ||> ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |]
    |> ArrayView.isEmpty
    |> should be True

    (2, 3)
    ||> ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |]
    |> ArrayView.isEmpty
    |> should be False

[<TestCase>]
let ofArray () : unit =
    Array.empty
    |> ArrayView.ofArray
    |> ArrayView.isEmpty
    |> should be True

    [| 5; 3; 11; 2; 17; 4; 12; 14; |]
    |> ArrayView.ofArray
    |> ArrayView.count
    |> should equal 8

[<TestCase>]
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
    |> should equal
        [| 11; 2; 17; |]

[<TestCase>]
let get () : unit =
    do
        let view = ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4

        ArrayView.get view 1
        |> should equal 17

        ArrayView.get view 3
        |> should equal 12

[<TestCase>]
let set () : unit =
    do
        let arr = [| 5; 3; 11; 2; 17; 4; 12; 14; |]
        let view = ArrayView.create arr 3 4

        // Set the array element value via the ArrayView.
        ArrayView.set view 1 9

        // Confirm the new element value via the ArrayView.
        ArrayView.get view 1
        |> should equal 9

        // Confirm the new element value by directly checking the underlying array.
        arr.[4] |> should equal 9

[<TestCase>]
let first () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.first
    |> should equal 2

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.first
    |> should equal 11

    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 47; |] 4 3
    |> ArrayView.first
    |> should equal 7

[<TestCase>]
let lastIndex () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.lastIndex
    |> should equal 6

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.lastIndex
    |> should equal 3

    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 47; |] 4 3
    |> ArrayView.lastIndex
    |> should equal 6

[<TestCase>]
let last () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.last
    |> should equal 12

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.last
    |> should equal 47

    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 47; |] 4 3
    |> ArrayView.last
    |> should equal 18

[<TestCase>]
let clear () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let toList () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.toList
    |> should equal
        [2; 17; 4; 12]

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.toList
    |> should equal
        [11; 23; 47]

    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 47; |] 4 3
    |> ArrayView.toList
    |> should equal
        [7; 11; 18]

[<TestCase>]
let toArray () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.toArray
    |> should equal
        [| 2; 17; 4; 12; |]

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.toArray
    |> should equal
        [| 11; 23; 47; |]

    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 47; |] 4 3
    |> ArrayView.toArray
    |> should equal
        [| 7; 11; 18; |]

[<TestCase>]
let mapToArray () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.mapToArray (fun x -> 3 * x)
    |> should equal
        [| 6; 51; 12; 36; |]

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.mapToArray (fun x -> 3 * x)
    |> should equal
        [| 33; 69; 141; |]

    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 47; |] 4 3
    |> ArrayView.mapToArray (fun x -> 3 * x)
    |> should equal
        [| 21; 33; 54; |]

[<TestCase>]
let tryPick () : unit =
    // Test case for an empty ArrayView.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 0
    |> ArrayView.tryPick (fun x ->
        if x % 3 = 0 && x % 4 = 0 then
            Some (sprintf "Match: %i" x)
        else None)
    |> should equal None

    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.tryPick (fun x ->
        if x % 3 = 0 && x % 4 = 0 then
            Some (sprintf "Match: %i" x)
        else None)
    |> should equal (Some "Match: 12")

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.tryPick (fun x ->
        if x % 3 = 0 && x % 4 = 0 then
            Some (sprintf "Match: %i" x)
        else None)
    |> should equal None

    // Test case for multiple matching values.
    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5
    |> ArrayView.tryPick (fun x ->
        if x % 2 = 0 && x % 3 = 0 then
            Some (sprintf "Match: %i" x)
        else None)
    |> should equal (Some "Match: 18")

[<TestCase>]
let pick () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.pick (fun x ->
        if x % 3 = 0 && x % 4 = 0 then
            Some (sprintf "Match: %i" x)
        else None)
    |> should equal "Match: 12"

    // Test case for multiple matching values.
    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5
    |> ArrayView.pick (fun x ->
        if x % 2 = 0 && x % 3 = 0 then
            Some (sprintf "Match: %i" x)
        else None)
    |> should equal "Match: 18"

[<TestCase>]
let tryFind () : unit =
    // Test case for an empty ArrayView.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 0
    |> ArrayView.tryFind (fun x ->
        x % 3 = 0 && x % 4 = 0)
    |> should equal None

    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.tryFind (fun x ->
        x % 3 = 0 && x % 4 = 0)
    |> should equal (Some 12)

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.tryFind (fun x ->
        x % 3 = 0 && x % 4 = 0)
    |> should equal None

    // Test case for multiple matching values.
    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5
    |> ArrayView.tryFind (fun x ->
        x % 2 = 0 && x % 3 = 0)
    |> should equal (Some 18)

[<TestCase>]
let find () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.find (fun x ->
        x % 3 = 0 && x % 4 = 0)
    |> should equal 12

    // Test case for multiple matching values.
    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5
    |> ArrayView.find (fun x ->
        x % 2 = 0 && x % 3 = 0)
    |> should equal 18

[<TestCase>]
let tryFindIndex () : unit =
    // Test case for an empty ArrayView.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 0
    |> ArrayView.tryFindIndex (fun x ->
        x % 3 = 0 && x % 4 = 0)
    |> should equal None

    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.tryFindIndex (fun x ->
        x % 3 = 0 && x % 4 = 0)
    |> should equal (Some 6)

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.tryFindIndex (fun x ->
        x % 3 = 0 && x % 4 = 0)
    |> should equal None

    // Test case for multiple matching values.
    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5
    |> ArrayView.tryFindIndex (fun x ->
        x % 2 = 0 && x % 3 = 0)
    |> should equal (Some 6)

[<TestCase>]
let findIndex () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.findIndex (fun x ->
        x % 3 = 0 && x % 4 = 0)
    |> should equal 6

    // Test case for multiple matching values.
    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5
    |> ArrayView.findIndex (fun x ->
        x % 2 = 0 && x % 3 = 0)
    |> should equal 6

[<TestCase>]
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

[<TestCase>]
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

[<TestCase>]
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
        |> should equal
            [| 6; 51; 12; 36; |]

    do
        // Sample usage test case.
        let elements = ResizeArray ()

        ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
        |> ArrayView.iter (fun x ->
            elements.Add (x * 3))

        ResizeArray.toArray elements
        |> should equal
            [| 33; 69; 141; |]

    do
        // Sample usage test case.
        let elements = ResizeArray ()

        ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 47; |] 4 3
        |> ArrayView.iter (fun x ->
            elements.Add (x * 3))

        ResizeArray.toArray elements
        |> should equal
            [| 21; 33; 54; |]

[<TestCase>]
let fold () : unit =
    // Test case for an empty ArrayView.
    (String.empty, ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 0)
    ||> ArrayView.fold (fun state x ->
        state + x.ToString())
    |> should equal String.empty

    // Sample usage test cases.
    (String.empty, ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4)
    ||> ArrayView.fold (fun state x ->
        state + x.ToString())
    |> should equal "217412"

    (String.empty, ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3)
    ||> ArrayView.fold (fun state x ->
        state + x.ToString())
    |> should equal "112347"

    (String.empty, ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5)
    ||> ArrayView.fold (fun state x ->
        state + x.ToString())
    |> should equal "711182948"

[<TestCase>]
let foldBack () : unit =
    // Test case for an empty ArrayView.
    (ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 0, String.empty)
    ||> ArrayView.foldBack (fun x state ->
        state + x.ToString())
    |> should equal String.empty

    // Sample usage test cases.
    (ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4, String.empty)
    ||> ArrayView.foldBack (fun x state ->
        state + x.ToString())
    |> should equal "124172"

    (ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3, String.empty)
    ||> ArrayView.foldBack (fun x state ->
        state + x.ToString())
    |> should equal "472311"

    (ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5, String.empty)
    ||> ArrayView.foldBack (fun x state ->
        state + x.ToString())
    |> should equal "482918117"

[<TestCase>]
let reduce () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.reduce (fun x y ->
        (x - y) * y)
    |> should equal -12576

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.reduce (fun x y ->
        (x - y) * y)
    |> should equal -15181

    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5
    |> ArrayView.reduce (fun x y ->
        (x - y) * y)
    |> should equal -1596144

[<TestCase>]
let reduceBack () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.reduceBack (fun x y ->
        (y - x) * x)
    |> should equal 506

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.reduceBack (fun x y ->
        (y - x) * x)
    |> should equal 5951

    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 4 5
    |> ArrayView.reduceBack (fun x y ->
        (y - x) * x)
    |> should equal 737842

[<TestCase>]
let min () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.min
    |> should equal 2

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.min
    |> should equal 11
    
    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 0 5
    |> ArrayView.min
    |> should equal 1

[<TestCase>]
let max () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.max
    |> should equal 17

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.max
    |> should equal 47
    
    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 0 5
    |> ArrayView.max
    |> should equal 7

[<TestCase>]
let sum () : unit =
    // Sample usage test cases.
    ArrayView.create [| 5; 3; 11; 2; 17; 4; 12; 14; |] 3 4
    |> ArrayView.sum
    |> should equal 35

    ArrayView.create [| 6; 11; 23; 47; 106; 235; |] 1 3
    |> ArrayView.sum
    |> should equal 81
    
    ArrayView.create [| 2; 1; 3; 4; 7; 11; 18; 29; 48; |] 0 5
    |> ArrayView.sum
    |> should equal 17

[<TestCase>]
let mapInPlace () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let mapiInPlace () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let chooseInPlace () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let chooseiInPlace () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let average () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let averageBy () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let maxBy () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let minBy () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let sumBy () : unit =
    Assert.Inconclusive "Test not yet implemented."
