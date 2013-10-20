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

namespace Tests.ExtCore

open NUnit.Framework
//open FsCheck


(* TODO : Implement tests for the various modules in ExtCore *)

// Operators
// Printf

/// Tests for the ExtCore.Enum module.
module Enum =
    [<Test>]
    let hasFlag () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let values () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let isDefined () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Lazy module.
module Lazy =
    [<Test>]
    let force () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let value () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let create () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let init () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let tryGetValue () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let map () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let map2 () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let map3 () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Option module.
module Option =
    [<Test>]
    let ofNull () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let toNull () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let ofNullable () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let toNullable () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let ofChoice () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let toChoice () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let toChoiceWith () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let conditional () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let condition () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let coalesce () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let fill () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let fillWith () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let tryFillWith () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let attempt () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let toOutAndBool () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let filter () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let bind2 () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Choice module.
module Choice =
    [<Test>]
    let isResult () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let isError () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let get () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let getChoice () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let failwith () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let failwithf () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let ofOption () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let ofOptionWith () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let toOption () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let map () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let mapError () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let bind () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let bind2 () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let exists () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let forall () : unit =
        Assert.Ignore "Test not yet implemented."

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
    let bindOrRaise () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test; ExpectedException(typeof<exn>)>]
    let ``bindOrRaise raises exn for Choice2Of2`` () : unit =
        Choice2Of2 (exn "An error occurred within the computation.")
        |> Choice.bindOrRaise
        |> ignore

    [<Test>]
    let bindOrFail () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let attempt () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let compose () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let composeBack () : unit =
        Assert.Ignore "Test not yet implemented."

