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


/// Tests for the ExtCore.Operators module.
module Operators =
    [<Test>]
    let swap () : unit =
        swap (3, 5)
        |> assertEqual (5, 3)

    [<Test>]
    let flip () : unit =
        do
            let join (x : string) (y : string) = x + " " + y
                
            (flip join) "World!" "Hello"
            |> assertEqual "Hello World!"

        (flip Operators.(/)) 2 10
        |> assertEqual 5

    [<Test>]
    let isNull () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase(false, false, ExpectedResult = true)>]
    [<TestCase(false, true, ExpectedResult = true)>]
    [<TestCase(true, false, ExpectedResult = true)>]
    [<TestCase(true, true, ExpectedResult = false)>]
    let nand (x : bool, y : bool) : bool =
        nand x y

    [<TestCase(false, false, ExpectedResult = true)>]
    [<TestCase(false, true, ExpectedResult = false)>]
    [<TestCase(true, false, ExpectedResult = false)>]
    [<TestCase(true, true, ExpectedResult = false)>]
    let nor (x : bool, y : bool) : bool =
        nor x y

    [<TestCase(false, false, ExpectedResult = false)>]
    [<TestCase(false, true, ExpectedResult = true)>]
    [<TestCase(true, false, ExpectedResult = true)>]
    [<TestCase(true, true, ExpectedResult = false)>]
    let xor (x : bool, y : bool) : bool =
        xor x y

    [<TestCase(false, false, ExpectedResult = true)>]
    [<TestCase(false, true, ExpectedResult = false)>]
    [<TestCase(true, false, ExpectedResult = false)>]
    [<TestCase(true, true, ExpectedResult = true)>]
    let xnor (x : bool, y : bool) : bool =
        xnor x y

    [<Test>]
    let tap () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let notlazy () : unit =
        let value = notlazy 10
        assertTrue value.IsValueCreated
        
        let value = notlazy "Hello World!"
        assertTrue value.IsValueCreated

    [<Test>]
    let orf () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let andf () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let xorf () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let tapAssert () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let tryLock () : unit =
        Assert.Ignore "Test not yet implemented."

    /// Helper function which sums the values of characters in a string.
    let private sumCharacterValues (str : string) =
        let mutable sum = 0L
        for i = 0 to str.Length - 1 do
            sum <- sum + int64 str.[i]
        sum

    [<Test>]
    let minBy () : unit =
        minBy sumCharacterValues "aaa" "aab"
        |> assertEqual "aaa"

        minBy (fun x -> x * 2) 10 30
        |> assertEqual 10

    [<Test>]
    let maxBy () : unit =
        maxBy sumCharacterValues "aaa" "aab"
        |> assertEqual "aab"

        maxBy (fun x -> x * 2) 10 30
        |> assertEqual 30

    [<Test>]
    let minWith () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let maxWith () : unit =
        Assert.Ignore "Test not yet implemented."

#if PROTO_COMPILER
    [<Test>]
    let typehandleof () : unit =
        Assert.Ignore "Test not yet implemented."
#endif

    [<Test>]
    let raiseNew () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let notImpl () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let notSupported () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let argOutOfRange () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let keyNotFound () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let checkNonNull () : unit =
        Assert.Ignore "Test not yet implemented."

#if PROTO_COMPILER
    [<Test>]
    let checkFinite () : unit =
        Assert.Ignore "Test not yet implemented."
#endif

    [<Test>]
    let ``(|Success|Error|)`` () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let ``(|Less|Equal|Greater|)`` () : unit =
        Assert.Ignore "Test not yet implemented."


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
    let getError () : unit =
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


/// Tests for the ExtCore.Printf module.
module Printf =
    [<Test>]
    let bprintfn () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let dprintf () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let dprintfn () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let tprintf () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let tprintfn () : unit =
        Assert.Ignore "Test not yet implemented."
