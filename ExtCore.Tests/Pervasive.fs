(*

Copyright 2013-2014 Jack Pappas

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
    let ``isNull on null reference`` () : unit =
        isNull null
        |> assertTrue

    [<Test>]
    let ``isNull on non-null reference`` () : unit =
        isNull "Hello World!"
        |> assertFalse

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
        [| 1; 2; 3; 4; 5 |]
        |> Array.map (fun x -> x * 2)
        |> tap (fun arr ->
            // Check that the input array has the expected values.
            arr
            |> Collection.assertEqual [| 2; 4; 6; 8; 10 |]

            // Now, to test ordering of side effects, change one of the array values.
            arr.[0] <- 123)
        |> Collection.assertEqual [| 123; 4; 6; 8; 10 |]

    [<Test>]
    let ``notlazy returns a Lazy containing the input value or instance`` () : unit =
        // Test with a value type.
        do
            let lz = notlazy 10
            lz.Value |> assertEqual 10

        // Test with a reference type.
        do
            let arr = [| 1; 2; 3; 4; 5 |]
            let lz = notlazy arr
            lz.Value
            |> assertSame arr

    [<Test>]
    let ``notlazy always returns an instance with IsValueCreated=true`` () : unit =
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
        ("aaaa", "bbb")
        ||> minWith String.length
        |> assertEqual 3

        ("abcd", "efgh")
        ||> minWith Seq.max
        |> assertEqual 'd'

    [<Test>]
    let maxWith () : unit =
        ("aaaa", "bbb")
        ||> maxWith String.length
        |> assertEqual 4

        ("abcd", "efgh")
        ||> maxWith Seq.max
        |> assertEqual 'h'

#if PROTO_COMPILER
    [<Test>]
    let typehandleof () : unit =
        Assert.Ignore "Test not yet implemented."
#endif
#if !NETSTANDARD1_6
    [<Test>]
    let raiseNew () : unit =
        Assert.Throws<System.NotFiniteNumberException>(fun () ->
            raiseNew<System.NotFiniteNumberException, _> ()) |> ignore
#endif
    /// Checks for a non-empty exeption message.
    let private NonEmptyExceptionMessage (ex : exn) : unit =
        Assert.IsNotNull ex.Message
        Assert.IsNotEmpty ex.Message

//    [<Test; ExpectedException(typeof<System.NotImplementedException>,
//        Handler = "NonEmptyExceptionMessage")>]
//    let ``notImpl with empty message`` () : unit =
//        notImpl ""
//
//    [<Test; ExpectedException(typeof<System.NotImplementedException>,
//        ExpectedMessage = "This functionality not yet implemented. It's on the TODO list.")>]
//    let ``notImpl with nonempty message`` () : unit =
//        notImpl "This functionality not yet implemented. It's on the TODO list."
//
//    [<Test; ExpectedException(typeof<System.NotSupportedException>,
//        Handler = "NonEmptyExceptionMessage")>]
//    let ``notSupported with empty message`` () : unit =
//        notSupported ""
//
//    [<Test; ExpectedException(typeof<System.NotSupportedException>,
//        ExpectedMessage = "This functionality is not supported, because I didn't want to support it.")>]
//    let ``notSupported with nonempty message`` () : unit =
//        notSupported "This functionality is not supported, because I didn't want to support it."

    [<Test>]
    let ``argOutOfRange with empty paramname, empty message`` () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let ``argOutOfRange with empty paramname, nonempty message`` () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let ``argOutOfRange with nonempty paramname, empty message`` () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let ``argOutOfRange with nonempty paramname, nonempty message`` () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>] //        Handler = "NonEmptyExceptionMessage")>]
    let ``keyNotFound with empty message`` () : unit =
        Assert.Throws<System.Collections.Generic.KeyNotFoundException>(fun () -> keyNotFound "") |> ignore

    [<Test>] //        ExpectedMessage = "THAT KEY DOES NOT EXIST")>]
    let ``keyNotFound with nonempty message`` () : unit =
        Assert.Throws<System.Collections.Generic.KeyNotFoundException>(fun () -> keyNotFound "THAT KEY DOES NOT EXIST") |> ignore

    [<Test>]
    let checkNonNull () : unit =
        Assert.Ignore "Test not yet implemented."

#if PROTO_COMPILER
    [<Test>]
    let ``ckfinite with normal value`` () : unit =
        1.234
        |> ckfinite
        |> assertEqual 1.234

    [<Test; ExpectedException(typeof<System.NotFiniteNumberException>)>]
    let ``ckfinite with infinity`` () : unit =
        System.Double.PositiveInfinity
        |> ckfinite
        |> ignore

    [<Test; ExpectedException(typeof<System.NotFiniteNumberException>)>]
    let ``ckfinite with NaN`` () : unit =
        System.Double.NaN
        |> ckfinite
        |> ignore

    [<Test>]
    let ``ckfinitef with normal value`` () : unit =
        1.234f
        |> ckfinitef
        |> assertEqual 1.234f

    [<Test; ExpectedException(typeof<System.NotFiniteNumberException>)>]
    let ``ckfinitef with infinity`` () : unit =
        System.Single.PositiveInfinity
        |> ckfinitef
        |> ignore

    [<Test; ExpectedException(typeof<System.NotFiniteNumberException>)>]
    let ``ckfinitef with NaN`` () : unit =
        System.Single.NaN
        |> ckfinitef
        |> ignore
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
        let foo =
            lazy
                String.init 3 (fun x -> string <| char x + 'a')
        foo
        |> Lazy.force
        |> assertEqual "abc"

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
    let ``tryGetValue for unforced value`` () : unit =
        let foo =
            lazy
                String.init 3 (fun x -> string <| char x + 'a')
        foo
        |> Lazy.tryGetValue
        |> assertEqual None

    [<Test>]
    let ``tryGetValue for forced value`` () : unit =
        let foo =
            lazy
                String.init 3 (fun x -> string <| char x + 'a')

        // Force evaluation of the value before calling Lazy.tryGetValue.
        foo |> Lazy.force |> ignore

        foo
        |> Lazy.tryGetValue
        |> assertEqual (Some "abc")

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
    let ``isResult on Choice1Of2`` () : unit =
        Choice1Of2 "Hello World!"
        |> Choice.isResult
        |> assertTrue

    [<Test>]
    let ``isResult on Choice2Of2`` () : unit =
        Choice2Of2 123456
        |> Choice.isResult
        |> assertFalse

    [<Test>]
    let ``isError on Choice1Of2`` () : unit =
        Choice1Of2 "Hello World!"
        |> Choice.isError
        |> assertFalse

    [<Test>]
    let ``isError on Choice2Of2`` () : unit =
        Choice2Of2 123456
        |> Choice.isError
        |> assertTrue

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
    let ``bindOrRaise on Choice1Of2`` () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>] //        ExpectedMessage = "An error occurred within the computation.")>]
    let ``bindOrRaise on Choice2Of2`` () : unit =
        Assert.Throws<exn>(fun () ->
            Choice2Of2 (exn "An error occurred within the computation.")
            |> Choice.bindOrRaise
            |> ignore) |> ignore

    [<Test>]
    let ``bindOrFail on Choice1Of2`` () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>] // ExpectedMessage = "An error occurred within the computation.")>]
    let ``bindOrFail on Choice2Of2`` () : unit =
        Assert.Throws<exn>(fun () ->
            Choice2Of2 "An error occurred within the computation."
            |> Choice.bindOrFail
            |> ignore) |> ignore

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
