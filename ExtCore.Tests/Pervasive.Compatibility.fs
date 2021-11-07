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

namespace Tests.ExtCore.Compatibility

open NUnit.Framework
open ExtCore.Compatibility

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
