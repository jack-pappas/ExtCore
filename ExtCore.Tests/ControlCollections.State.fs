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

/// Unit tests for the ExtCore.Control.Collections.State module.
module Tests.ExtCore.Control.Collections.State

open ExtCore.Control
open ExtCore.Control.Collections
open NUnit.Framework


/// Helper functions for implementing tests.
[<AutoOpen>]
module private StateTestHelpers =
    /// Increments the counter (representing some global state) and
    /// combines it with a state value passed via the state workflow.
    let modifyState (counter : int ref) state : unit * int =
        let state' =
            let c = !counter
            if c % 2 = 0 then state + c else state * c
        incr counter
        (), state'

    /// Combines the length of a string with the state value.
    let combineLength (str : string) (state : int) : unit * int =
        let state' =
            if state % 2 = 0 then str.Length + state
            else str.Length * state
        (), state'


/// Tests for the ExtCore.Control.Collections.State.Array module.
module Array =
    [<Test>]
    let iter () : unit =
        // Test case for an empty array.
        do
            let iterationCount = ref 0
            let testFunc =
                Array.empty
                |> State.Array.iter (fun _ ->
                    state {
                    do! modifyState iterationCount
                    })
            let (), finalState = State.run testFunc 7

            finalState |> assertEqual 7
            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let testFunc =
                [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet"; |]
                |> State.Array.iter combineLength
            let (), finalState = State.run testFunc 0

            finalState |> assertEqual 122

    [<Test>]
    let iteri () : unit =
        // Test case for an empty array.
        do
            let iterationCount = ref 0
            let testFunc =
                Array.empty
                |> State.Array.iteri (fun _ _ ->
                    state {
                    do! modifyState iterationCount
                    })
            let (), finalState = State.run testFunc 7

            finalState |> assertEqual 7
            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let initials = ref ""
            let testFunc =
                [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet"; |]
                |> State.Array.iteri (fun idx colorName ->
                    state {
                    initials := !initials + (String.replicate (idx + 1) <| colorName.[0].ToString())
                    do! combineLength colorName
                    })
            let (), finalState = State.run testFunc 0

            !initials |> assertEqual "ROOYYYGGGGBBBBBVVVVVV"
            finalState |> assertEqual 122

    [<Test>]
    let map () : unit =
        // Test case for an empty array.
        do
            let iterationCount = ref 0
            let testFunc =
                Array.empty
                |> State.Array.map (fun _ ->
                    state {
                    do! modifyState iterationCount
                    return ()
                    })
            let results, finalState = State.run testFunc 7

            Array.isEmpty results |> assertTrue
            finalState |> assertEqual 7
            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let testFunc =
                [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet"; |]
                |> State.Array.map (fun colorName ->
                    state {
                    do! combineLength colorName
                    return colorName.ToLowerInvariant ()
                    })
            let results, finalState = State.run testFunc 0

            results
            |> assertEqual
                [| "red"; "orange"; "yellow"; "green"; "blue"; "violet"; |]
            finalState |> assertEqual 122

    [<Test>]
    let mapi () : unit =
        // Test case for an empty array.
        do
            let iterationCount = ref 0
            let testFunc =
                Array.empty
                |> State.Array.mapi (fun _ _ ->
                    state {
                    do! modifyState iterationCount
                    return ()
                    })
            let results, finalState = State.run testFunc 7

            Array.isEmpty results |> assertTrue
            finalState |> assertEqual 7
            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let testFunc =
                [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet"; |]
                |> State.Array.mapi (fun idx colorName ->
                    state {
                    do! combineLength colorName

                    if idx % 2 = 0 then
                        return colorName.ToLowerInvariant ()
                    else
                        return colorName.ToUpperInvariant ()
                    })
            let results, finalState = State.run testFunc 0

            results
            |> assertEqual
                [| "red"; "ORANGE"; "yellow"; "GREEN"; "blue"; "VIOLET"; |]
            finalState |> assertEqual 122

    [<Test>]
    let mapBack () : unit =
        // Test case for an empty array.
        do
            let iterationCount = ref 0
            let testFunc =
                Array.empty
                |> State.Array.mapBack (fun _ ->
                    state {
                    do! modifyState iterationCount
                    return ()
                    })
            let results, finalState = State.run testFunc 7

            Array.isEmpty results |> assertTrue
            finalState |> assertEqual 7
            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let testFunc =
                [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet"; |]
                |> State.Array.mapBack (fun colorName ->
                    state {
                    do! combineLength colorName
                    return colorName.ToLowerInvariant ()
                    })
            let results, finalState = State.run testFunc 0

            results
            |> assertEqual
                [| "red"; "orange"; "yellow"; "green"; "blue"; "violet"; |]
            finalState |> assertEqual 99

    [<Test>]
    let mapiBack () : unit =
        // Test case for an empty array.
        do
            let iterationCount = ref 0
            let testFunc =
                Array.empty
                |> State.Array.mapiBack (fun _ _ ->
                    state {
                    do! modifyState iterationCount
                    return ()
                    })
            let results, finalState = State.run testFunc 7

            Array.isEmpty results |> assertTrue
            finalState |> assertEqual 7
            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let testFunc =
                [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet"; |]
                |> State.Array.mapiBack (fun idx colorName ->
                    state {
                    do! combineLength colorName

                    if idx % 2 = 0 then
                        return colorName.ToLowerInvariant ()
                    else
                        return colorName.ToUpperInvariant ()
                    })
            let results, finalState = State.run testFunc 0

            results
            |> assertEqual
                [| "red"; "ORANGE"; "yellow"; "GREEN"; "blue"; "VIOLET"; |]
            finalState |> assertEqual 99

    [<Test>]
    let map2 () : unit =
        // Test case for an empty array.
        do
            let iterationCount = ref 0
            let testFunc =
                (Array.empty, Array.empty)
                ||> State.Array.map2 (fun _ _ ->
                    state {
                    do! modifyState iterationCount
                    return ()
                    })
            let results, finalState = State.run testFunc 7

            Array.isEmpty results |> assertTrue
            finalState |> assertEqual 7
            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let testFunc =
                ([| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet"; |],
                 [| 1; 1; 2; 3; 5; 8; |])
                ||> State.Array.map2 (fun colorName fibo ->
                    state {
                    do! combineLength colorName
                    return sprintf "%i:%s" fibo <| colorName.ToLowerInvariant ()
                    })
            let results, finalState = State.run testFunc 0

            results
            |> assertEqual
                [| "1:red"; "1:orange"; "2:yellow"; "3:green"; "5:blue"; "8:violet"; |]
            finalState |> assertEqual 122

    [<Test>]
    let ``map2 raises exn when arrays have different lengths`` () : unit =
        let testFunc =
            ([| 1..3 |], [| 2..10 |])
            ||> State.Array.map2 (fun _ _ ->
                state {
                return ()
                })
        Assert.Throws<System.ArgumentException>(fun () ->
            State.run testFunc 15
            |> ignore) |> ignore

    [<Test>]
    let mapi2 () : unit =
        // Test case for an empty array.
        do
            let iterationCount = ref 0
            let testFunc =
                (Array.empty, Array.empty)
                ||> State.Array.mapi2 (fun _ _ _ ->
                    state {
                    do! modifyState iterationCount
                    return ()
                    })
            let results, finalState = State.run testFunc 7

            Array.isEmpty results |> assertTrue
            finalState |> assertEqual 7
            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let testFunc =
                ([| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet"; |],
                 [| 1; 1; 2; 3; 5; 8; |])
                ||> State.Array.mapi2 (fun idx colorName fibo ->
                    state {
                    do! combineLength colorName
                    if idx % 2 = 0 then
                        return sprintf "%i:%s" fibo <| colorName.ToLowerInvariant ()
                    else
                        return sprintf "%i:%s" fibo <| colorName.ToUpperInvariant ()
                    })
            let results, finalState = State.run testFunc 0

            results
            |> assertEqual
                [| "1:red"; "1:ORANGE"; "2:yellow"; "3:GREEN"; "5:blue"; "8:VIOLET"; |]
            finalState |> assertEqual 122

    [<Test>]
    let ``mapi2 raises exn when arrays have different lengths`` () : unit =
        let testFunc =
            ([| 1..3 |], [| 2..10 |])
            ||> State.Array.mapi2 (fun _ _ _ ->
                state {
                return ()
                })
        Assert.Throws<System.ArgumentException>(fun () ->
            State.run testFunc 15
            |> ignore) |> ignore

    [<Test>]
    let fold () : unit =
        // Test case for an empty array.
        do
            let iterationCount = ref 0
            let testFunc =
                ("", (Array.empty : string[]))
                ||> State.Array.fold (fun initials colorName ->
                    state {
                    do! modifyState iterationCount
                    return initials + colorName.[0].ToString()
                    })
            let foldResult, finalState = State.run testFunc 7

            String.isEmpty foldResult |> assertTrue
            finalState |> assertEqual 7
            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let testFunc =
                ("", [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet"; |])
                ||> State.Array.fold (fun initials colorName ->
                    state {
                    do! combineLength colorName
                    return initials + colorName.[0].ToString()
                    })
            let foldResult, finalState = State.run testFunc 0

            foldResult |> assertEqual "ROYGBV"
            finalState |> assertEqual 122

    [<Test>]
    let foldi () : unit =
        // Test case for an empty array.
        do
            let iterationCount = ref 0
            let testFunc =
                ("", (Array.empty : string[]))
                ||> State.Array.foldi (fun idx initials colorName ->
                    state {
                    do! modifyState iterationCount
                    return initials + (String.replicate (idx + 1) <| colorName.[0].ToString())
                    })
            let foldResult, finalState = State.run testFunc 7

            String.isEmpty foldResult |> assertTrue
            finalState |> assertEqual 7
            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let testFunc =
                ("", [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet"; |])
                ||> State.Array.foldi (fun idx initials colorName ->
                    state {
                    do! combineLength colorName
                    return initials + (String.replicate (idx + 1) <| colorName.[0].ToString())
                    })
            let foldResult, finalState = State.run testFunc 0

            foldResult |> assertEqual "ROOYYYGGGGBBBBBVVVVVV"
            finalState |> assertEqual 122

    [<Test>]
    let reduce () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let reduceBack () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Control.Collections.State.List module.
module List =
    [<Test>]
    let init () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let iter () : unit =
        // Test case for an empty list.
        do
            let iterationCount = ref 0
            let testFunc =
                List.empty
                |> State.List.iter (fun _ ->
                    state {
                    do! modifyState iterationCount
                    })
            let (), finalState = State.run testFunc 7

            finalState |> assertEqual 7
            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let testFunc =
                ["Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet"]
                |> State.List.iter combineLength
            let (), finalState = State.run testFunc 0

            finalState |> assertEqual 122

    [<Test>]
    let iteri () : unit =
        // Test case for an empty list.
        do
            let iterationCount = ref 0
            let testFunc =
                List.empty
                |> State.List.iteri (fun _ _ ->
                    state {
                    do! modifyState iterationCount
                    })
            let (), finalState = State.run testFunc 7

            finalState |> assertEqual 7
            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let initials = ref ""
            let testFunc =
                ["Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet"]
                |> State.List.iteri (fun idx colorName ->
                    state {
                    initials := !initials + (String.replicate (idx + 1) <| colorName.[0].ToString())
                    do! combineLength colorName
                    })
            let (), finalState = State.run testFunc 0

            !initials |> assertEqual "ROOYYYGGGGBBBBBVVVVVV"
            finalState |> assertEqual 122

    [<Test>]
    let iterBack () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let map () : unit =
        // Test case for an empty list.
        do
            let iterationCount = ref 0
            let testFunc =
                List.empty
                |> State.List.map (fun _ ->
                    state {
                    do! modifyState iterationCount
                    return ()
                    })
            let results, finalState = State.run testFunc 7

            List.isEmpty results |> assertTrue
            finalState |> assertEqual 7
            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let testFunc =
                ["Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet"]
                |> State.List.map (fun colorName ->
                    state {
                    do! combineLength colorName
                    return colorName.ToLowerInvariant ()
                    })
            let results, finalState = State.run testFunc 0

            results
            |> assertEqual
                ["red"; "orange"; "yellow"; "green"; "blue"; "violet"]
            finalState |> assertEqual 122

    [<Test>]
    let mapi () : unit =
        // Test case for an empty list.
        do
            let iterationCount = ref 0
            let testFunc =
                List.empty
                |> State.List.mapi (fun _ _ ->
                    state {
                    do! modifyState iterationCount
                    return ()
                    })
            let results, finalState = State.run testFunc 7

            List.isEmpty results |> assertTrue
            finalState |> assertEqual 7
            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let testFunc =
                ["Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet"]
                |> State.List.mapi (fun idx colorName ->
                    state {
                    do! combineLength colorName

                    if idx % 2 = 0 then
                        return colorName.ToLowerInvariant ()
                    else
                        return colorName.ToUpperInvariant ()
                    })
            let results, finalState = State.run testFunc 0

            results
            |> assertEqual
                ["red"; "ORANGE"; "yellow"; "GREEN"; "blue"; "VIOLET"]
            finalState |> assertEqual 122

    [<Test>]
    let fold () : unit =
        // Test case for an empty list.
        do
            let iterationCount = ref 0
            let testFunc =
                ("", (List.empty : string list))
                ||> State.List.fold (fun initials colorName ->
                    state {
                    do! modifyState iterationCount
                    return initials + colorName.[0].ToString()
                    })
            let foldResult, finalState = State.run testFunc 7

            String.isEmpty foldResult |> assertTrue
            finalState |> assertEqual 7
            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let testFunc =
                ("", ["Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet"])
                ||> State.List.fold (fun initials colorName ->
                    state {
                    do! combineLength colorName
                    return initials + colorName.[0].ToString()
                    })
            let foldResult, finalState = State.run testFunc 0

            foldResult |> assertEqual "ROYGBV"
            finalState |> assertEqual 122

    [<Test>]
    let foldBack () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Control.Collections.State.TaggedArray module.
module TaggedArray =
    [<Measure>] type Foo

    [<Test>]
    let mapi () : unit =
        // Test case for an empty array.
        do
            let iterationCount = ref 0
            let testFunc =
                Array.empty
                |> State.TaggedArray.mapi (fun _ _ ->
                    state {
                    do! modifyState iterationCount
                    return ()
                    })
            let results, finalState = State.run testFunc 7

            Array.isEmpty results |> assertTrue
            finalState |> assertEqual 7
            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let testFunc =
                [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet"; |]
                |> State.TaggedArray.mapi (fun (idx : int<Foo>) colorName ->
                    state {
                    do! combineLength colorName

                    if idx % 2<Foo> = 0<Foo> then
                        return colorName.ToLowerInvariant ()
                    else
                        return colorName.ToUpperInvariant ()
                    })
            let results, finalState = State.run testFunc 0

            results
            |> assertEqual
                [| "red"; "ORANGE"; "yellow"; "GREEN"; "blue"; "VIOLET"; |]
            finalState |> assertEqual 122

    [<Test>]
    let mapiBack () : unit =
        // Test case for an empty array.
        do
            let iterationCount = ref 0
            let testFunc =
                Array.empty
                |> State.TaggedArray.mapiBack (fun _ _ ->
                    state {
                    do! modifyState iterationCount
                    return ()
                    })
            let results, finalState = State.run testFunc 7

            Array.isEmpty results |> assertTrue
            finalState |> assertEqual 7
            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let testFunc =
                [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet"; |]
                |> State.TaggedArray.mapiBack (fun (idx : int<Foo>) colorName ->
                    state {
                    do! combineLength colorName

                    if idx % 2<Foo> = 0<Foo> then
                        return colorName.ToLowerInvariant ()
                    else
                        return colorName.ToUpperInvariant ()
                    })
            let results, finalState = State.run testFunc 0

            results
            |> assertEqual
                [| "red"; "ORANGE"; "yellow"; "GREEN"; "blue"; "VIOLET"; |]
            finalState |> assertEqual 99

    [<Test>]
    let foldi () : unit =
        // Test case for an empty array.
        do
            let iterationCount = ref 0
            let testFunc =
                ("", (Array.empty : string[]))
                ||> State.TaggedArray.foldi (fun (idx : int<Foo>) initials colorName ->
                    state {
                    do! modifyState iterationCount
                    return initials + (String.replicate ((int idx) + 1) <| colorName.[0].ToString())
                    })
            let foldResult, finalState = State.run testFunc 7

            String.isEmpty foldResult |> assertTrue
            finalState |> assertEqual 7
            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let testFunc =
                ("", [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet"; |])
                ||> State.TaggedArray.foldi (fun (idx : int<Foo>) initials colorName ->
                    state {
                    do! combineLength colorName
                    return initials + (String.replicate ((int idx) + 1) <| colorName.[0].ToString())
                    })
            let foldResult, finalState = State.run testFunc 0

            foldResult |> assertEqual "ROOYYYGGGGBBBBBVVVVVV"
            finalState |> assertEqual 122


/// Tests for the ExtCore.Control.Collections.State.ArrayView module.
module ArrayView =
    [<Test>]
    let iter () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let iteri () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let map () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let mapi () : unit =
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

    [<Test>]
    let reduceBack () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Control.Collections.State.Set module.
module Set =
    [<Test>]
    let iter () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let iterBack () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let map () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let mapBack () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let fold () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let foldBack () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Control.Collections.State.IntSet module.
module IntSet =
    [<Test>]
    let iter () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let iterBack () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let map () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let mapBack () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let fold () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let foldBack () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Control.Collections.State.HashSet module.
module HashSet =
    [<Test>]
    let iter () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let iterBack () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let map () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let mapBack () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let fold () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let foldBack () : unit =
        Assert.Ignore "Test not yet implemented."

