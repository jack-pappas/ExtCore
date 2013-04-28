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

open ExtCore.Control
open NUnit.Framework
open FsUnit
//open FsCheck


/// Tests for the ExtCore.Control.State module.
module State =
    [<TestCase>]
    let bindChoice () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let evaluate () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let execute () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let getState () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let map () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let readonly () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let run () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let setState () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Control.Reader module.
module Reader =
    [<TestCase>]
    let run () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let read () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Control.ReaderState module.
module ReaderState =
    [<TestCase>]
    let evaluate () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let execute () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let getState () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let run () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let setState () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Control.Choice module.
module Choice =
    [<TestCase>]
    let bindOrFail () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let bindOrRaise () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase; ExpectedException(typeof<exn>)>]
    let ``bindOrRaise raises exn for Choice2Of2`` () : unit =
        Choice2Of2 (exn "An error occurred within the computation.")
        |> Choice.bindOrRaise
        |> ignore

    [<TestCase>]
    let failwith () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let setError () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Control.ProtectedState module.
module ProtectedState =
    [<TestCase>]
    let discardState () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let failwith () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let getState () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let liftChoice () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let liftReader () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let liftReaderChoice () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let liftState () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let setError () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let setState () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Control.StatefulChoice module.
module StatefulChoice =
    [<TestCase>]
    let getState () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let liftChoice () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let liftState () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let map () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let setState () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Control.Async module.
module Async =
    [<TestCase>]
    let map () : unit =
        // Sample usage test cases.
        async {
            return "Hello World!" }
        |> Async.map String.toLower
        |> Async.RunSynchronously
        |> should equal "hello world!"



