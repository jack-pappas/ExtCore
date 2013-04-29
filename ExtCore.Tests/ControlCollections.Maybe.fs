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

/// Unit tests for the ExtCore.Control.Collections.Maybe module.
module Tests.ExtCore.Control.Collections.Maybe

open ExtCore.Control
open ExtCore.Control.Collections
open NUnit.Framework
open FsUnit
//open FsCheck


/// Tests for the ExtCore.Control.Collections.Maybe.Array module.
module Array =
    [<Test>]
    let init () : unit =
        // Test case for an empty array.
        Maybe.Array.init 0 <| fun _ -> None
        |> should equal (Some Array.empty)

        // Sample usage test cases.
        Maybe.Array.init 5 <| fun x ->
            Some (x * 7)
        |> should equal
            <| Some [| 0; 7; 14; 21; 28; |]

        Maybe.Array.init 5 <| fun x ->
            if x > 0 && x % 5 = 0 then None
            else Some (x * 2)
        |> should equal
            <| Some [| 0; 2; 4; 6; 8; |]

        Maybe.Array.init 6 <| fun x ->
            if x > 0 && x % 5 = 0 then None
            else Some (x * 2)
        |> should equal None

    [<Test>]
    let iter () : unit =
        // Test case for an empty array.
        Array.empty
        |> Maybe.Array.iter (fun _ -> None)
        |> should equal (Some ())

        // Sample usage test cases.
        do
            let iterationCount = ref 0

            [| 0..4 |]
            |> Maybe.Array.iter (fun x ->
                incr iterationCount
                if x > 0 && x % 5 = 0 then None
                else Some ())
            |> Option.isSome
            |> should be True

            !iterationCount |> should equal 5

        do
            let iterationCount = ref 0

            [| 0..5 |]
            |> Maybe.Array.iter (fun x ->
                incr iterationCount
                if x > 0 && x % 5 = 0 then None
                else Some ())
            |> Option.isNone
            |> should be True

            !iterationCount |> should equal 6

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            [| 0..5 |]
            |> Maybe.Array.iter (fun x ->
                incr iterationCount
                if x > 0 && x % 2 = 0 then None
                else Some ())
            |> Option.isNone
            |> should be True

            !iterationCount |> should equal 3

    [<Test>]
    let iteri () : unit =
        // Test case for an empty array.
        Array.empty
        |> Maybe.Array.iteri (fun _ _ -> None)
        |> should equal (Some ())

        // Sample usage test cases.
        do
            let iterationCount = ref 0

            [| 0..4 |]
            |> Maybe.Array.iteri (fun idx x ->
                incr iterationCount
                let y = x * idx
                if y > 0 && y % 5 = 0 then None
                else Some ())
            |> Option.isSome
            |> should be True

            !iterationCount |> should equal 5

        do
            let iterationCount = ref 0

            [| 0..5 |]
            |> Maybe.Array.iteri (fun idx x ->
                incr iterationCount
                let y = x * idx
                if y > 0 && y % 5 = 0 then None
                else Some ())
            |> Option.isNone
            |> should be True

            !iterationCount |> should equal 6

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            [| 0..4 |]
            |> Maybe.Array.iteri (fun idx x ->
                incr iterationCount
                let y = x * idx
                if y > 0 && y % 2 = 0 then None
                else Some ())
            |> Option.isNone
            |> should be True

            !iterationCount |> should equal 3

    [<Test>]
    let map () : unit =
        // Test case for an empty array.
        Array.empty
        |> Maybe.Array.map (fun _ -> None)
        |> should equal (Some Array.empty)

        // Sample usage test cases.
        [| 0..4 |]
        |> Maybe.Array.map (fun x ->
            if x > 0 && x % 5 = 0 then None
            else Some (x * 3))
        |> should equal (Some [| 0; 3; 6; 9; 12; |])

        [| 0..5 |]
        |> Maybe.Array.map (fun x ->
            if x > 0 && x % 5 = 0 then None
            else Some (x * 3))
        |> should equal None

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            [| 0..4 |]
            |> Maybe.Array.map (fun x ->
                incr iterationCount
                if x > 0 && x % 2 = 0 then None
                else Some (x * 3))
            |> should equal None

            !iterationCount |> should equal 3

    [<Test>]
    let mapi () : unit =
        // Test case for an empty array.
        Array.empty
        |> Maybe.Array.mapi (fun _ _ -> None)
        |> should equal (Some Array.empty)

        // Sample usage test cases.
        [| 0..4 |]
        |> Maybe.Array.mapi (fun idx x ->
            let y = x * idx
            if y > 0 && y % 5 = 0 then None
            else Some (y * 3))
        |> should equal (Some [| 0; 3; 12; 27; 48; |])

        [| 0..5 |]
        |> Maybe.Array.mapi (fun idx x ->
            let y = x * idx
            if y > 0 && y % 5 = 0 then None
            else Some (y * 3))
        |> should equal None

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            [| 0..4 |]
            |> Maybe.Array.mapi (fun idx x ->
                incr iterationCount
                let y = x * idx
                if y > 0 && y % 2 = 0 then None
                else Some (y * 3))
            |> should equal None

            !iterationCount |> should equal 3

    [<Test>]
    let map2 () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let fold () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let foldi () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let reduce () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Control.Collections.Maybe.List module.
module List =
    [<Test>]
    let iter2 () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let map2 () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let mapi2 () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let fold () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Control.Collections.Maybe.Seq module.
module Seq =
    [<Test>]
    let iter () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Control.Collections.Maybe.Set module.
module Set =
    [<Test>]
    let fold () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let mapToArray () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Control.Collections.Maybe.ArrayView module.
module ArrayView =
    [<Test>]
    let fold () : unit =
        Assert.Ignore "Test not yet implemented."

