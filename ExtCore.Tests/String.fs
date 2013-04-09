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

namespace ExtCore.String.Tests

open NUnit.Framework
open FsUnit
//open FsCheck


/// Tests for the ExtCore.Substring module.
module Substring =
    // TODO : Implement equality/comparison tests for substring.

    [<TestCase>]
    let get () : unit =
        do
            let substr = Substring ("Hello World!", 3, 5)
            
            Substring.get substr 1
            |> should equal 'o'

            Substring.get substr 3
            |> should equal 'W'

    [<TestCase>]
    let isEmpty () : unit =
        Substring ("Hello World!", 4, 0)
        |> Substring.isEmpty
        |> should be True

        Substring ("Hello World!", 3, 4)
        |> Substring.isEmpty
        |> should be False

    [<TestCase>]
    let ofString () : unit =
        do
            let substr = Substring.ofString ""

            substr
            |> Substring.string
            |> should equal ""

            substr
            |> Substring.offset
            |> should equal 0

            substr
            |> Substring.length
            |> should equal 0

        do
            let substr = Substring.ofString "Hello World!"

            substr
            |> Substring.string
            |> should equal "Hello World!"

            substr
            |> Substring.offset
            |> should equal 0

            substr
            |> Substring.length
            |> should equal 12

    [<TestCase>]
    let toString () : unit =
        // Test for empty substring.
        Substring ("Hello World!", 3, 0)
        |> Substring.toString
        |> should equal String.empty

        Substring ("Hello World!", 3, 6)
        |> Substring.toString
        |> should equal "lo Wor"

    [<TestCase>]
    let toArray () : unit =
        // Test for empty substring.
        Substring ("Hello World!", 3, 0)
        |> Substring.toArray
        |> should equal Array.empty

        Substring ("Hello World!", 3, 6)
        |> Substring.toArray
        |> should equal [| 'l'; 'o'; ' '; 'W'; 'o'; 'r'; |]

    [<TestCase>]
    let sub () : unit =
        do
            let str = "The quick brown fox jumps over the lazy dog."

            let substr = Substring (str, 4, 15)  // "quick brown fox"
            
            Substring.sub substr 6 5
            |> Substring.toString
            |> should equal "brown"

    [<TestCase>]
    let concat () : unit =
        do
            let str1 = "The quick brown fox jumps over the lazy dog."
            let str2 = "Hello World!"

            seq {
            yield Substring (str1, 0, 35)
            yield Substring (str2, 6, 6)
            yield Substring (str1, 3, 1)
            yield Substring (str2, 0, 6)
            yield Substring (str1, 35, 9) }
            |> Substring.concat
            |> should equal "The quick brown fox jumps over the World! Hello lazy dog."

    [<TestCase>]
    let iter () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let iteri () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let iterBack () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let fold () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let foldi () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let foldBack () : unit =
        Assert.Inconclusive "Test not yet implemented."


/// Tests for the ExtCore.String module.
module String =
    [<TestCase>]
    let sub () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let tryFindIndexOf () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let findIndexOf () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let tryFindIndex () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let findIndex () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let fold () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let foldBack () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let iter () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let iteri () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let map () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let mapi () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let choose () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let choosei () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let trimStart () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let trimEnd () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let trimStartWith () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let trimEndWith () : unit =
        Assert.Inconclusive "Test not yet implemented."

    [<TestCase>]
    let trimWith () : unit =
        Assert.Inconclusive "Test not yet implemented."

    module Split =
        [<TestCase>]
        let iter () : unit =
            Assert.Inconclusive "Test not yet implemented."

        [<TestCase>]
        let iteri () : unit =
            Assert.Inconclusive "Test not yet implemented."

        [<TestCase>]
        let fold () : unit =
            Assert.Inconclusive "Test not yet implemented."

        [<TestCase>]
        let foldi () : unit =
            Assert.Inconclusive "Test not yet implemented."

