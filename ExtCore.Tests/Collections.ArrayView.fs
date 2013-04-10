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
module ExtCore.Collections.ArrayView.Tests

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
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let get () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let set () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let first () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let lastIndex () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let last () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let toArray () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let mapToArray () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let tryPick () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let pick () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let tryFind () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let find () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let tryFindIndex () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let findIndex () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let iter () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let exists () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let forall () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let fold () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let foldBack () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let reduce () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let reduceBack () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let toList () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let min () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let max () : unit =
    Assert.Inconclusive "Test not yet implemented."

[<TestCase>]
let sum () : unit =
    Assert.Inconclusive "Test not yet implemented."
