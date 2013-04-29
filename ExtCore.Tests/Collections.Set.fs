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

/// Unit tests for the ExtCore.Collections.Set module.
module Tests.ExtCore.Collections.Set

open System
open NUnit.Framework
open FsUnit
//open FsCheck


[<Test>]
let foldi () : unit =
    let colors =
        Set.ofArray [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]

    ([], colors)
    ||> Set.foldi (fun lst idx el ->
        (idx, el) :: lst)
    |> assertEqual [
        5, "Yellow";
        4, "Violet";
        3, "Red";
        2, "Orange";
        1, "Green";
        0, "Blue"; ]

[<Test>]
let mapToArray () : unit =
    let colors =
        Set.ofArray [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]

    colors
    |> Set.mapToArray String.length
    |> assertEqual [|
        4; 5; 6; 3; 6; 6 |]

[<Test>]
let init () : unit =
    let expected =
        Set.ofArray [| 'a' .. 'z' |]

    Set.init 26 <| fun i ->
        char (int 'a' + i)
    |> assertEqual expected

[<Test>]
let tryExtractMin () : unit =
    do
        let initialSet = Set.empty
        let minElement, remaining =
            Set.tryExtractMin initialSet

        minElement
        |> assertEqual None

        remaining
        |> assertEqual initialSet  // TODO : Make this even stricter by checking for reference (physical) equality.

    do
        let initialSet = Set.ofArray [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]
        let minElement, remaining =
            Set.tryExtractMin initialSet

        minElement
        |> assertEqual (Some "Blue")

        remaining
        |> assertEqual (Set.ofArray [| "Red"; "Orange"; "Yellow"; "Green"; "Violet" |])

[<Test>]
let tryExtractMax () : unit =
    do
        let initialSet = Set.empty
        let maxElement, remaining =
            Set.tryExtractMax initialSet

        maxElement
        |> assertEqual None

        remaining
        |> assertEqual initialSet  // TODO : Make this even stricter by checking for reference (physical) equality.

    do
        let maxElement, remaining =
            [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]
            |> Set.ofArray
            |> Set.tryExtractMax

        maxElement
        |> assertEqual (Some "Yellow")

        remaining
        |> assertEqual (Set.ofArray [| "Red"; "Orange"; "Green"; "Blue"; "Violet" |])
    
[<Test>]
let extractMin () : unit =
    let minElement, remaining =
        [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]
        |> Set.ofArray
        |> Set.extractMin

    minElement
    |> assertEqual "Blue"

    remaining
    |> assertEqual (Set.ofArray [| "Red"; "Orange"; "Yellow"; "Green"; "Violet" |])

[<Test>]
let extractMax () : unit =
    let maxElement, remaining =
        [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]
        |> Set.ofArray
        |> Set.extractMax

    maxElement
    |> assertEqual "Yellow"

    remaining
    |> assertEqual (Set.ofArray [| "Red"; "Orange"; "Green"; "Blue"; "Violet" |])

[<Test>]
let reduce () : unit =
    [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]
    |> Set.ofArray
    |> Set.reduce (+)
    |> assertEqual "BlueGreenOrangeRedVioletYellow"

[<Test>]
let reduceBack () : unit =
    [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]
    |> Set.ofArray
    |> Set.reduceBack (+)
    |> assertEqual "BlueGreenOrangeRedVioletYellow"

[<Test>]
let choose () : unit =
    [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]
    |> Set.ofArray
    |> Set.choose (fun colorName ->
        let len = String.length colorName
        if len < 6 then Some len else None)
    |> assertEqual
        <| set [| 3; 4; 5 |]

[<Test>]
let tryPick () : unit =
    [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]
    |> Set.ofArray
    |> Set.tryPick (fun colorName ->
        if colorName.StartsWith "T" then Some (String.length colorName) else None)
    |> assertEqual None

    [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]
    |> Set.ofArray
    |> Set.tryPick (fun colorName ->
        if colorName.StartsWith "G" then Some (String.length colorName) else None)
    |> assertEqual (Some 5)

[<Test>]
let pick () : unit =
    [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]
    |> Set.ofArray
    |> Set.pick (fun colorName ->
        if colorName.StartsWith "G" then Some (String.length colorName) else None)
    |> assertEqual 5

[<Test>]
let tryFind () : unit =
    [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]
    |> Set.ofArray
    |> Set.tryFind (fun colorName ->
        colorName.StartsWith "T")
    |> assertEqual None

    [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]
    |> Set.ofArray
    |> Set.tryFind (fun colorName ->
        colorName.StartsWith "G")
    |> assertEqual (Some "Green")

[<Test>]
let find () : unit =
    [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]
    |> Set.ofArray
    |> Set.find (fun colorName ->
        colorName.StartsWith "G")
    |> assertEqual "Green"

[<Test>]
let mapPartition () : unit =
    let left, right =
        [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]
        |> Set.ofArray
        |> Set.mapPartition (fun colorName ->
            let len = String.length colorName
            if len % 2 = 0 then
                Choice1Of2 len
            else
                Choice2Of2 <| colorName.ToLower ())

    left
    |> assertEqual (Set.ofArray [| 4; 6; 6; 6 |])

    right
    |> assertEqual (Set.ofArray [| "red"; "green" |])

[<Test>]
let symmetricDifference () : unit =
    Set.symmetricDifference Set.empty Set.empty
    |> assertEqual Set.empty

    Set.symmetricDifference (Set.ofArray [| 0..2..20 |]) (Set.ofArray [| 0..3..20 |])
    |> assertEqual (Set.ofArray [| 2; 3; 4; 8; 9; 10; 14; 15; 16; 20 |])

[<Test>]
let cartesian () : unit =
    Set.cartesian Set.empty Set.empty
    |> assertEqual Set.empty

    Set.cartesian
        (Set.ofArray [| "Red"; "Green"; "Blue" |])
        (Set.ofArray [| 20; 30 |])
    |> assertEqual (Set.ofArray
        [|
        "Blue", 20;
        "Blue", 30;
        "Green", 20;
        "Green", 30;
        "Red", 20;
        "Red", 30; |])

[<Test>]
let collect () : unit =
    [| 0; 1; 1; 2; 3; 5; 8 |]
    |> Set.ofArray
    |> Set.collect (fun el ->
        Set.ofArray [|
            el; el + 1; el * 2 |])
    |> assertEqual (Set.ofArray
        [| 0; 1; 2; 3; 4; 5; 6; 8; 9; 10; 16 |])

[<Test>]
let condense () : unit =
    [| 7; 11; 13; 17; 19 |]
    |> Set.ofArray
    |> Set.condense (fun el ->
        Set.ofArray [| 0 .. el |])
    |> assertEqual (Set.ofArray [| 0; 1; 2; 3; 4; 5; 6; 7 |])

[<Test>]
let disjoint () : unit =
    Set.disjoint Set.empty Set.empty
    |> should be True

    Set.disjoint (Set.ofArray [| 3; 6; 9; 12; 15 |]) (Set.ofArray [| 7; 14; 21; 28; 35 |])
    |> should be True

    Set.disjoint
        (Set.ofArray [| 0..20 |] |> Set.filter (fun el -> el % 2 = 0))
        (Set.ofArray [| 0..20 |] |> Set.filter (fun el -> el % 3 = 0))
    |> should be False

[<Test>]
let countWith () : unit =
    // Test case for an empty set.
    Set.empty
    |> Set.countWith (fun x ->
        x % 7 = 0)
    |> assertEqual 0

    // Sample usage test cases.
    [| 0; 1; 2; 3; 4; 5; 6; 8; 9; 10; 16 |]
    |> Set.ofArray
    |> Set.countWith (fun x ->
        x < 0)
    |> assertEqual 0

    [| 0; 1; 2; 3; 4; 5; 6; 8; 9; 10; 16 |]
    |> Set.ofArray
    |> Set.countWith (fun x ->
        x % 7 = 0)
    |> assertEqual 1

    [| 0; 1; 2; 3; 4; 5; 6; 8; 9; 10; 16 |]
    |> Set.ofArray
    |> Set.countWith (fun x ->
        x % 3 = 0)
    |> assertEqual 4


module Cartesian =
    [<Test>]
    let fold () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let foldBack () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let iter () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let map () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let choose () : unit =
        Assert.Ignore "Test not yet implemented."

