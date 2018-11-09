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

namespace Tests.ExtCore.Control

open System
open System.Runtime.CompilerServices
open ExtCore.Control
open NUnit.Framework

/// <summary>
/// Tests for <see cref="ExtCore.Control.MaybeBuilder"/>.
/// </summary>
module MaybeBuilder =
    /// <summary>Tests for <see cref="ExtCore.Control.MaybeBuilder.For"/>.</summary>
    module ``MaybeBuilder_For`` =
        [<Test>]
        let ``simple test`` () : unit =
            let count = ref 0
            let data = [| 1..3 |]
            let result = maybe {
                for i in data do
                    incr count
                return true
            }

            // Check the loop iteration count is correct.
            assertEqual 3 !count

            // Check the result of the computation.
            assertEqual (Some true) result

    /// <summary>Tests for <see cref="ExtCore.Control.MaybeBuilder.While"/>.</summary>
    module ``MaybeBuilder_While`` =
        [<Test>]
        let ``simple test`` () : unit =
            let count = ref 0
            let result = maybe {
                while !count < 3 do
                    incr count
                return true
            }

            // Check the loop iteration count is correct.
            assertEqual 3 !count

            // Check the result of the computation.
            assertEqual (Some true) result


/// Tests for the ExtCore.Control.State module.
module State =
    [<Test>]
    let bindChoice () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let ``bindChoice raises exn when given Choice2Of2`` () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let evaluate () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let execute () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let getState () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let map () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let readonly () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let run () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let setState () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Control.Reader module.
module Reader =
    /// A subset of the F# language keywords.
    let private keywords =
        Set.ofArray [| "if"; "then"; "elif"; "else"; "do"; "while"; "let"; "for"; |]

    [<Test>]
    let run () : unit =
        // Sample usage test cases.
        do
            let testFunc =
                reader {
                return! Set.contains "else"
                }

            keywords
            |> Reader.run testFunc
            |> assertTrue

        do
            let testFunc =
                reader {
                let! kwd = Set.minElement
                return String.length kwd
                }

            keywords
            |> Reader.run testFunc
            |> assertEqual 2

    [<Test>]
    let read () : unit =
        // Sample usage test cases.
        do
            let testFunc =
                reader {
                let! kwds = Reader.read
                return Set.maxElement kwds
                }

            keywords
            |> Reader.run testFunc
            |> assertEqual "while"


/// Tests for the ExtCore.Control.ReaderState module.
module ReaderState =
    [<Test>]
    let liftReader () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let liftState () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let evaluate () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let execute () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let getState () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let run () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let setState () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Control.ProtectedState module.
module ProtectedState =
    [<Test>]
    let discardState () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let failwith () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let getState () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let liftChoice () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let liftReader () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let liftReaderChoice () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let liftState () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let setError () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let setState () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Control.StatefulChoice module.
module StatefulChoice =
    [<Test>]
    let getState () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let liftChoice () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let liftState () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let map () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let setState () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Control.Async module.
module Async =
    [<Test>]
    let map () : unit =
        // Sample usage test cases.
        async {
            return "Hello World!" }
        |> Async.map String.toLower
        |> Async.RunSynchronously
        |> assertEqual "hello world!"



