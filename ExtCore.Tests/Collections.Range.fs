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

/// Tests for the ExtCore.Collections.Range module.
module Tests.ExtCore.Collections.Range

open NUnit.Framework


[<Test>]
let iter () : unit =
    do
        // Test case for an inverted range (this should
        // be treated just like an empty range).
        let elements = ResizeArray ()

        (5, 2)
        ||> Range.iter (fun x ->
            String.replicate x "A"
            |> elements.Add)

        ResizeArray.isEmpty elements
        |> assertTrue

    do
        // Test case for a single-element range.
        let elements = ResizeArray ()

        (4, 4)
        ||> Range.iter (fun x ->
            String.replicate x "A"
            |> elements.Add)

        ResizeArray.toArray elements
        |> Collection.assertEqual [| "AAAA"; |]

    do
        // Sample usage test case.
        let elements = ResizeArray ()

        (2, 7)
        ||> Range.iter (fun x ->
            String.replicate x "A"
            |> elements.Add)

        ResizeArray.toArray elements
        |> Collection.assertEqual
            [| "AA"; "AAA"; "AAAA"; "AAAAA"; "AAAAAA"; "AAAAAAA"; |]

[<Test>]
let fold () : unit =
    /// The set of prime numbers less than 40.
    let primes =
        Set.ofArray [| 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; |]

    do
        // Test case for an inverted range (this should
        // be treated just like an empty range).
        (5, 2, 1L)
        |||> Range.fold (fun checksum x ->
            if Set.contains x primes then
                checksum * int64 x
            else
                checksum + int64 x)
        |> assertEqual 1L

    do
        // Test cases for a single-element range.
        (6, 6, 1L)
        |||> Range.fold (fun checksum x ->
            if Set.contains x primes then
                checksum * int64 x
            else
                checksum + int64 x)
        |> assertEqual 7L

        (5, 5, 1L)
        |||> Range.fold (fun checksum x ->
            if Set.contains x primes then
                checksum * int64 x
            else
                checksum + int64 x)
        |> assertEqual 5L

    do
        // Sample usage test cases.
        (2, 7, 1L)
        |||> Range.fold (fun checksum x ->
            if Set.contains x primes then
                checksum * int64 x
            else
                checksum + int64 x)
        |> assertEqual 392L

        (20, 30, 1L)
        |||> Range.fold (fun checksum x ->
            if Set.contains x primes then
                checksum * int64 x
            else
                checksum + int64 x)
        |> assertEqual 46488L

[<Test>]
let foldBack () : unit =
    /// The set of prime numbers less than 40.
    let primes =
        Set.ofArray [| 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; |]

    do
        // Test case for an inverted range (this should
        // be treated just like an empty range).
        (5, 2, 1L)
        |||> Range.foldBack (fun x checksum ->
            if Set.contains x primes then
                checksum * int64 x
            else
                checksum + int64 x)
        |> assertEqual 1L

    do
        // Test cases for a single-element range.
        (6, 6, 1L)
        |||> Range.foldBack (fun x checksum ->
            if Set.contains x primes then
                checksum * int64 x
            else
                checksum + int64 x)
        |> assertEqual 7L

        (5, 5, 1L)
        |||> Range.foldBack (fun x checksum ->
            if Set.contains x primes then
                checksum * int64 x
            else
                checksum + int64 x)
        |> assertEqual 5L

    do
        // Sample usage test cases.
        (2, 7, 1L)
        |||> Range.foldBack (fun x checksum ->
            if Set.contains x primes then
                checksum * int64 x
            else
                checksum + int64 x)
        |> assertEqual 414L

        (20, 30, 1L)
        |||> Range.foldBack (fun x checksum ->
            if Set.contains x primes then
                checksum * int64 x
            else
                checksum + int64 x)
        |> assertEqual 23730L

[<Test>]
let exists () : unit =
    /// The set of prime numbers less than 40.
    let primes =
        Set.ofArray [| 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; |]

    do
        // Test case for an inverted range (this should
        // be treated just like an empty range).
        (5, 2)
        ||> Range.exists (fun x ->
            Set.contains x primes)
        |> assertFalse

    do
        // Test cases for a single-element range.
        (4, 4)
        ||> Range.exists (fun x ->
            Set.contains x primes)
        |> assertFalse

        (5, 5)
        ||> Range.exists (fun x ->
            Set.contains x primes)
        |> assertTrue

    do
        // Sample usage test case.
        (2, 7)
        ||> Range.exists (fun x ->
            Set.contains x primes)
        |> assertTrue

[<Test>]
let forall () : unit =
    /// The set of prime numbers less than 40.
    let primes =
        Set.ofArray [| 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; |]

    do
        // Test case for an inverted range (this should
        // be treated just like an empty range).
        (5, 2)
        ||> Range.forall (fun x ->
            not <| Set.contains x primes)
        |> assertTrue

    do
        // Test cases for a single-element range.
        (4, 4)
        ||> Range.forall (fun x ->
            not <| Set.contains x primes)
        |> assertTrue

        (5, 5)
        ||> Range.forall (fun x ->
            not <| Set.contains x primes)
        |> assertFalse

    do
        // Sample usage test cases.
        (14, 18)
        ||> Range.forall (fun x ->
            not <| Set.contains x primes)
        |> assertFalse

        (24, 28)
        ||> Range.forall (fun x ->
            not <| Set.contains x primes)
        |> assertTrue
