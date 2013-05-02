(*

Copyright 2005-2009 Microsoft Corporation
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

(* Attribution :    The NUnitRunner class was adapted from some test code in the FSharpx project. *)

/// Helper functions for implementing unit tests.
[<AutoOpen>]
module TestHelpers

open System
open System.Collections.Generic
open NUnit.Framework
open FsUnit
open FsCheck


(* Fluent test helpers for use with NUnit and FsUnit. *)

/// Asserts that two values are equal.
let inline assertEqual<'T when 'T : equality> (expected : 'T) (actual : 'T) =
    Assert.AreEqual (expected, actual)

/// Asserts that two objects are identical.
let inline assertSame<'T when 'T : not struct> (expected : 'T) (actual : 'T) =
    Assert.AreSame (expected, actual)

/// Tests that the specified condition is true.
/// If not, calls Assert.Fail with a formatted string.
let inline assertf (condition : bool) format : 'T =
    Printf.ksprintf (fun str -> if not condition then Assert.Fail str) format


(* Fluent test helpers for use with NUnit and FsCheck. *)

/// An FsCheck runner which reports FsCheck test results to NUnit.
type private NUnitRunner () =
    interface IRunner with
        member x.OnStartFixture _ = ()
        member x.OnArguments (_,_,_) = ()
        member x.OnShrink (_,_) = ()
        member x.OnFinished (name, result) =
            match result with
            | TestResult.True data ->
                // TODO : Log the result data.
                Runner.onFinishedToString name result
                |> stdout.WriteLine

            | TestResult.Exhausted data ->
                // TODO : Log the result data.
                Runner.onFinishedToString name result
                |> Assert.Inconclusive

            | TestResult.False (_,_,_,_,_) ->
                // TODO : Log more information about the test failure.
                Runner.onFinishedToString name result
                |> Assert.Fail

/// An FsCheck configuration which logs test results to NUnit.
let private nUnitConfig = {
    Config.Default with
        Runner = NUnitRunner (); }

/// Tests that the specified property is correct.
let assertProp testName (property : 'Testable) =
    Check.One (testName, nUnitConfig, property)


(*  Test helpers from the F# PowerPack.
    TODO : Get rid of most of these -- they can be replaced with FsUnit and built-in NUnit functions. *)

let numActiveEnumerators = ref 0

let countEnumeratorsAndCheckedDisposedAtMostOnceAtEnd (seq : seq<'T>) =
    let enumerator () =
        numActiveEnumerators := !numActiveEnumerators + 1
        let disposed = ref false
        let endReached = ref false
        let ie = seq.GetEnumerator ()
        { new System.Collections.Generic.IEnumerator<'T> with
            member __.Current =
                Assert.IsFalse (!endReached, "MiniTest 'rvlrve0'")
                Assert.IsFalse (!disposed, "MiniTest 'rvlrve1'")
                ie.Current
            member __.Dispose () =
                Assert.IsTrue (!endReached, "MiniTest 'rvlrve2'")
                Assert.IsFalse (!disposed, "MiniTest 'rvlrve4'")
                numActiveEnumerators := !numActiveEnumerators - 1
                disposed := true
                ie.Dispose ()
        interface System.Collections.IEnumerator with
            member __.MoveNext () =
                Assert.IsFalse (!endReached, "MiniTest 'rvlrve0'")
                Assert.IsFalse (!disposed, "MiniTest 'rvlrve3'")
                endReached := not <| ie.MoveNext ()
                not !endReached
            member __.Current =
                Assert.IsFalse (!endReached, "MiniTest 'qrvlrve0'")
                Assert.IsFalse (!disposed, "MiniTest 'qrvlrve1'")
                box ie.Current
            member __.Reset () =
                ie.Reset()
                }

    { new seq<'T> with 
            member __.GetEnumerator () = enumerator () 
        interface System.Collections.IEnumerable with 
            member __.GetEnumerator () = enumerator () :> _ }

let countEnumeratorsAndCheckedDisposedAtMostOnce (seq : seq<'T>) =
    let enumerator () =
        let disposed = ref false
        let endReached = ref false
        let ie = seq.GetEnumerator ()
        numActiveEnumerators := !numActiveEnumerators + 1
        { new System.Collections.Generic.IEnumerator<'T> with
            member x.Current =
                Assert.IsFalse (!endReached, "MiniTest 'qrvlrve0'")
                Assert.IsFalse (!disposed, "MiniTest 'qrvlrve1'")
                ie.Current
            member x.Dispose () =
                Assert.IsFalse (!disposed, "MiniTest 'qrvlrve4'")
                numActiveEnumerators := !numActiveEnumerators - 1
                disposed := true
                ie.Dispose ()
        interface System.Collections.IEnumerator with
            member x.MoveNext () =
                Assert.IsFalse (!endReached, "MiniTest 'qrvlrve0'")
                Assert.IsFalse (!disposed, "MiniTest 'qrvlrve3'")
                endReached := not <| ie.MoveNext ()
                not !endReached
            member x.Current =
                Assert.IsFalse (!endReached, "MiniTest 'qrvlrve0'")
                Assert.IsFalse (!disposed, "MiniTest 'qrvlrve1'")
                box ie.Current
            member __.Reset() =
                ie.Reset()
                }

    { new seq<'T> with
            member __.GetEnumerator () = enumerator () 
        interface System.Collections.IEnumerable with 
            member __.GetEnumerator () = enumerator () :> _ }
        
/// Check that the lambda throws an exception of the given type.
/// Otherwise calls Assert.Fail().
let private checkThrowsExn<'Exn when 'Exn :> exn> (f : unit -> unit) : unit =
    try
        do f ()

        // Did not throw -- return an error message explaining this.
        let msg = sprintf "The function did not throw an exception. (Expected: %s)" typeof<'Exn>.FullName
        Assert.Fail msg
    with
    | :? 'Exn ->
        // The expected exception type was raised, so there's nothing to do.
        Assert.Pass ()
    | ex ->
        // The expected exception type was not raised.
        let msg =
            sprintf "The function raised an exception of type '%s'. (Expected: %s)"
                (ex.GetType().FullName) typeof<'Exn>.FullName
        Assert.Fail msg

// Illegitimate exceptions. Once we've scrubbed the library, we should add an
// attribute to flag these exception's usage as a bug.
[<Obsolete>]
let checkThrowsNullRefException = checkThrowsExn<NullReferenceException>
[<Obsolete>]
let checkThrowsIndexOutRangException = checkThrowsExn<IndexOutOfRangeException>

// Legit exceptions
let checkThrowsNotSupportedException = checkThrowsExn<NotSupportedException>
let checkThrowsArgumentException = checkThrowsExn<ArgumentException>
let checkThrowsArgumentNullException = checkThrowsExn<ArgumentNullException>
let checkThrowsKeyNotFoundException = checkThrowsExn<KeyNotFoundException>
let checkThrowsDivideByZeroException = checkThrowsExn<DivideByZeroException>
let checkThrowsInvalidOperationExn = checkThrowsExn<InvalidOperationException>

