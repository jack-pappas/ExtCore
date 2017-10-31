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

namespace Tests.ExtCore.Control.Compatibility

open System
open System.Runtime.CompilerServices
open ExtCore.Control
open ExtCore.Control.Compatibility
open NUnit.Framework

/// Test fixture which tests that the .Using() member of AsyncChoiceBuilder
/// disposes the supplied resource at the correct point in the program's execution.
[<TestFixture; Sealed>]
type AsyncChoiceBuilderDisposeFixture() =
    let disposed = StrongBox (false)

    let createDisposable (disposed : StrongBox<bool>) =
        { new IDisposable with
            member __.Dispose () =
                printfn "disposing!"
                disposed.Value <- true }

    let createAsyncChoiceDisposable() =
        async { return Choice1Of2(createDisposable disposed) }

    let waitAsyncChoice() =
        asyncChoice {
            printfn "waiting"
        }

    let shouldNotBeDisposed() =
        printfn "Should not be disposed. Checking..."
        Assert.IsFalse(disposed.Value)

    let createAsyncDisposable() = async { return (createDisposable disposed) }

    let waitAsync() =
        async {
            printfn "waiting"
        }

    [<SetUp>]
    member __.Setup () : unit =
        disposed.Value <- false

    [<TearDown>]
    member __.Teardown () : unit =
        printfn "Should be disposed. Checking..."
        Assert.IsTrue(disposed.Value)

    // asyncChoice wrong behavior
    [<Test>]
    member __.UsingAsyncChoice() : unit =
        asyncChoice {
            use! d = createAsyncChoiceDisposable()
            shouldNotBeDisposed()
            let! a = waitAsyncChoice()
            shouldNotBeDisposed()
        }
        |> Async.RunSynchronously
        |> Choice.get
        |> ignore

    // async - expected behavior
    [<Test>]
    member __.UsingAsync() : unit =
        async {
            use! d = createAsyncDisposable()
            shouldNotBeDisposed()
            do! waitAsync()
            shouldNotBeDisposed()
        }
        |> Async.RunSynchronously
        |> ignore


/// <summary>
/// Tests for <see cref="ExtCore.Control.ChoiceBuilder"/>.
/// </summary>
module ChoiceBuilder =
    /// <summary>Tests for <see cref="ExtCore.Control.ChoiceBuilder.For"/>.</summary>
    module ``ChoiceBuilder_For`` =
        [<Test>]
        let ``simple test`` () : unit =
            let count = ref 0
            let data = [| 1..3 |]
            let result = choice {
                for i in data do
                    incr count
                return true
            }

            // Check the loop iteration count is correct.
            assertEqual 3 !count

            // Check the result of the computation.
            assertEqual (Choice1Of2 true) result

    /// <summary>Tests for <see cref="ExtCore.Control.ChoiceBuilder.While"/>.</summary>
    module ``ChoiceBuilder_While`` =
        [<Test>]
        let ``simple test`` () : unit =
            let count = ref 0
            let data = [| 1..3 |]
            let result = choice {
                while !count < 3 do
                    incr count
                return true
            }

            // Check the loop iteration count is correct.
            assertEqual 3 !count

            // Check the result of the computation.
            assertEqual (Choice1Of2 true) result

