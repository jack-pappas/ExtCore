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
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State =
    [<TestCase>]
    let run () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let evaluate () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let execute () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let getState () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let setState () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let bindChoice () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let readonly () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let map () : unit =
        Assert.Inconclusive "Test not yet implemented."


/// Tests for the ExtCore.Control.Reader module.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Reader =
    [<TestCase>]
    let run () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let read () : unit =
        Assert.Inconclusive "Test not yet implemented."


/// Tests for the ExtCore.Control.ReaderState module.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ReaderState =
    [<TestCase>]
    let run () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let evaluate () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let execute () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let getState () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let setState () : unit =
        Assert.Inconclusive "Test not yet implemented."


/// Tests for the ExtCore.Control.Choice module.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Choice =
    [<TestCase>]
    let bindOrRaise () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let bindOrFail () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let setError () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let failwith () : unit =
        Assert.Inconclusive "Test not yet implemented."


/// Tests for the ExtCore.Control.ProtectedState module.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ProtectedState =
    [<TestCase>]
    let liftState () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let liftChoice () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let liftReader () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let liftReaderChoice () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let setState () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let getState () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let setError () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let failwith () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let discardState () : unit =
        Assert.Inconclusive "Test not yet implemented."


/// Tests for the ExtCore.Control.StatefulChoice module.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StatefulChoice =
    [<TestCase>]
    let liftState () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let liftChoice () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let setState () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let getState () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let map () : unit =
        Assert.Inconclusive "Test not yet implemented."


/// Tests for the ExtCore.Control.Async module.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Async =
    [<TestCase>]
    let map () : unit =
        Assert.Inconclusive "Test not yet implemented."


