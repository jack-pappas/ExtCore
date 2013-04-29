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

/// Unit tests for the ExtCore.Control.Collections.Reader module.
module Tests.ExtCore.Control.Collections.Reader

open ExtCore.Control
open ExtCore.Control.Collections
open NUnit.Framework
open FsUnit


/// Helper functions for implementing tests.
[<AutoOpen>]
module private ReaderTestHelpers =
    /// A subset of the F# language keywords.
    let keywords = Set.ofArray [| "if"; "then"; "elif"; "else"; "do"; "while"; "let"; "for"; |]


/// Tests for the ExtCore.Control.Collections.Reader.Array module.
module Array =
    [<Test>]
    let iter () : unit =
        // Test case for an empty array.
        do
            let keywordCount = ref 0

            let testFunc =
                Array.empty
                |> Reader.Array.iter (fun str ->
                    reader {
                    let! isKeyword = Set.contains str
                    if isKeyword then
                        incr keywordCount
                    })
            Reader.run testFunc keywords

            !keywordCount |> should equal 0

        // Sample usage test cases.
        do
            let keywordCount = ref 0

            let testFunc =
                [| "if"; "foo"; "<>"; "123"; "then"; |]
                |> Reader.Array.iter (fun str ->
                    reader {
                    let! isKeyword = Set.contains str
                    if isKeyword then
                        incr keywordCount
                    })
            Reader.run testFunc keywords

            !keywordCount |> should equal 2

    [<Test>]
    let iteri () : unit =
        // Test case for an empty array.
        do
            let keywordWeight = ref 0

            let testFunc =
                Array.empty
                |> Reader.Array.iteri (fun idx str ->
                    reader {
                    let! isKeyword = Set.contains str
                    if isKeyword then
                        keywordWeight := !keywordWeight + (idx + 1)
                    })
            Reader.run testFunc keywords

            !keywordWeight |> should equal 0

        // Sample usage test cases.
        do
            let keywordWeight = ref 0

            let testFunc =
                [| "if"; "foo"; "<>"; "123"; "then"; |]
                |> Reader.Array.iteri (fun idx str ->
                    reader {
                    let! isKeyword = Set.contains str
                    if isKeyword then
                        keywordWeight := !keywordWeight + (idx + 1)
                    })
            Reader.run testFunc keywords

            !keywordWeight |> should equal 6

    [<Test>]
    let map () : unit =
        // Test case for an empty array.
        do
            let testFunc =
                Array.empty
                |> Reader.Array.map (fun str ->
                    reader {
                    return! Set.contains str
                    })

            keywords
            |> Reader.run testFunc
            |> Array.isEmpty
            |> should be True

        // Sample usage test cases.
        do
            let testFunc =
                [| "if"; "foo"; "<>"; "123"; "then"; |]
                |> Reader.Array.map (fun str ->
                    reader {
                    return! Set.contains str
                    })
            keywords
            |> Reader.run testFunc
            |> should equal
                [| true; false; false; false; true; |]

    [<Test>]
    let mapi () : unit =
        // Test case for an empty array.
        do
            let testFunc =
                Array.empty
                |> Reader.Array.mapi (fun idx str ->
                    reader {
                    let! isKeyword = Set.contains str
                    return
                        if isKeyword then Some (sprintf "%i:%s" idx str) else None
                    })
            keywords
            |> Reader.run testFunc
            |> Array.isEmpty
            |> should be True

        // Sample usage test cases.
        do
            let testFunc =
                [| "if"; "foo"; "<>"; "123"; "then"; |]
                |> Reader.Array.mapi (fun idx str ->
                    reader {
                    let! isKeyword = Set.contains str
                    return
                        if isKeyword then Some (sprintf "%i:%s" idx str) else None
                    })
            keywords
            |> Reader.run testFunc
            |> should equal
                [| Some "0:if"; None; None; None; Some "4:then"; |]

