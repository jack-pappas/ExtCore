﻿(*

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
open FsCheck


(* Fluent test helpers for use with NUnit and FsUnit. *)

/// Tests that the specified condition is true.
/// If not, calls Assert.Fail with a formatted string.
let inline assertf (condition : bool) format : 'T =
    Printf.ksprintf (fun str -> if not condition then Assert.Fail str) format

/// Asserts that two values are equal.
let inline assertEqual<'T when 'T : equality> (expected : 'T) (actual : 'T) =
    Assert.AreEqual (expected, actual)

/// Asserts that two values are NOT equal.
let inline assertNotEqual<'T when 'T : equality> (expected : 'T) (actual : 'T) =
    Assert.AreNotEqual (expected, actual)

/// Asserts that two objects are identical.
let inline assertSame<'T when 'T : not struct> (expected : 'T) (actual : 'T) =
    Assert.AreSame (expected, actual)

/// Asserts that two objects are NOT identical.
let inline assertNotSame<'T when 'T : not struct> (expected : 'T) (actual : 'T) =
    Assert.AreNotSame (expected, actual)

/// Asserts that a condition is true.
let inline assertTrue (condition: bool) =
    Assert.IsTrue (condition)

/// Asserts that a condition is false.
let inline assertFalse (condition: bool) =
    Assert.IsFalse (condition)

/// Asserts that the given function raises an exception of a specified exception type
/// or a type which inherits from the specified exception type.
[<RequiresExplicitTypeArguments>]
let inline assertRaises<'T when 'T :> exn> assertion =
    Assert.Catch<'T> (TestDelegate (assertion))
    |> ignore

/// Asserts that the given function raises an exception of exactly the specified exception type.
[<RequiresExplicitTypeArguments>]
let inline assertRaisesExact<'T when 'T :> exn> assertion =
    Assert.Throws<'T> (TestDelegate (assertion))
    |> ignore

/// Assertion functions for collections.
[<RequireQualifiedAccess>]
module Collection =
    open System.Collections

    /// Asserts that two collections are exactly equal.
    /// The collections must have the same count, and contain the exact same objects in the same order.
    let inline assertEqual<'T, 'U when 'T :> seq<'U>> (expected : 'T) (actual : 'T) =
        CollectionAssert.AreEqual (expected, actual)

    /// Asserts that two collections are not exactly equal.
    let inline assertNotEqual<'T, 'U when 'T :> seq<'U>> (expected : 'T) (actual : 'T) =
        CollectionAssert.AreNotEqual (expected, actual)

    /// Asserts that two collections are exactly equal.
    /// The collections must have the same count, and contain the exact same objects but the match may be in any order.
    let inline assertEquiv<'T, 'U when 'T :> seq<'U>> (expected : 'T) (actual : 'T) =
        CollectionAssert.AreEquivalent (expected, actual)

    /// Asserts that two collections are not exactly equal.
    let inline assertNotEquiv<'T, 'U when 'T :> seq<'U>> (expected : 'T) (actual : 'T) =
        CollectionAssert.AreNotEquivalent (expected, actual)

    /// Asserts that a collection is empty.
    let inline assertEmpty<'T, 'U when 'T :> seq<'U>> (sequence : 'T) =
        CollectionAssert.IsEmpty sequence

    /// Asserts that a collection is not empty.
    let inline assertNotEmpty<'T, 'U when 'T :> seq<'U>> (sequence : 'T) =
        CollectionAssert.IsNotEmpty sequence

    /// Asserts that a collection is ordered.
    let inline assertOrdered<'T, 'U when 'T :> seq<'U>> (sequence : 'T) =
        CollectionAssert.IsOrdered sequence


(* Fluent test helpers for use with NUnit and FsCheck. *)

/// An FsCheck runner which reports FsCheck test results to NUnit.
type private NUnitRunner () =
    interface IRunner with
        member x.OnStartFixture _ = ()
        member x.OnArguments (_,_,_) = ()
        member x.OnShrink (_,_) = ()
        member x.OnFinished (name, result) =
            match result with
            | TestResult.True (data, _) ->
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
    // Config.Verbose
    Config.Quick with
        MaxTest = 5000;
        Runner = NUnitRunner (); }

/// Tests that the specified property is correct.
let assertProp testName (property : 'Testable) =
    Check.One (testName, nUnitConfig, property)


(*  Test helpers from the F# PowerPack.
    TODO : Get rid of most of these -- they can be replaced with FsUnit and built-in NUnit functions. *)

let numActiveEnumerators = ref 0

let countEnumeratorsAndCheckedDisposedAtMostOnceAtEnd (seq : seq<'T>) =
    let enumerator () =
        incr numActiveEnumerators
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
                decr numActiveEnumerators
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
        incr numActiveEnumerators
        { new System.Collections.Generic.IEnumerator<'T> with
            member x.Current =
                Assert.IsFalse (!endReached, "MiniTest 'qrvlrve0'")
                Assert.IsFalse (!disposed, "MiniTest 'qrvlrve1'")
                ie.Current
            member x.Dispose () =
                Assert.IsFalse (!disposed, "MiniTest 'qrvlrve4'")
                decr numActiveEnumerators
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
    | :? AssertionException ->
        // If an assertion exception is thrown, don't handle it -- the exception should
        // propagate back up through the call stack to the next handler.
        // Without this, calls to Assert.Fail() within the 'try' block would be captured by
        // the catch-all case below.
        reraise ()

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
let checkThrowsIndexOutRangeException = checkThrowsExn<IndexOutOfRangeException>

// Legit exceptions
let checkThrowsNotSupportedException action = checkThrowsExn<NotSupportedException> action
let checkThrowsArgumentException action = checkThrowsExn<ArgumentException> action
let checkThrowsArgumentNullException action = checkThrowsExn<ArgumentNullException> action
let checkThrowsKeyNotFoundException action = checkThrowsExn<KeyNotFoundException> action
let checkThrowsDivideByZeroException action = checkThrowsExn<DivideByZeroException> action
let checkThrowsInvalidOperationExn action = checkThrowsExn<InvalidOperationException> action

