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

/// Unit tests for the ExtCore.Collections.Array module.
module Tests.ExtCore.Collections.Array

open System
open NUnit.Framework
//open FsCheck


[<Test>]
let exactlyOne () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let projectValues () : unit =
    Array.empty
    |> Array.projectValues ignore
    |> Collection.assertEqual Array.empty

    [|  ConsoleColor.Magenta;
        ConsoleColor.DarkGreen;
        ConsoleColor.Cyan;
        ConsoleColor.Black; |]
    |> Array.projectValues (sprintf "%O")
    |> Collection.assertEqual [|
        ConsoleColor.Magenta, "Magenta";
        ConsoleColor.DarkGreen, "DarkGreen";
        ConsoleColor.Cyan, "Cyan";
        ConsoleColor.Black, "Black"; |]

[<Test>]
let projectKeys () : unit =
    Array.empty
    |> Array.projectKeys ignore
    |> Collection.assertEqual Array.empty

    [|"Magenta"; "DarkGreen"; "Cyan"; "Black"|]
    |> Array.projectKeys (fun colorName ->
        Enum.Parse (typeof<ConsoleColor>, colorName)
        :?> System.ConsoleColor)
    |> Collection.assertEqual [|
        ConsoleColor.Magenta, "Magenta";
        ConsoleColor.DarkGreen, "DarkGreen";
        ConsoleColor.Cyan, "Cyan";
        ConsoleColor.Black, "Black"; |]

[<Test>]
let clear () : unit =
    // Test case for an empty array.
    // Just checks to make sure an exn isn't raised.
    do
        let arr = Array.empty
        Array.clear arr

    // Sample usage test cases.
    do
        let arr = [| 2 |]
        Array.clear arr
        arr |> Collection.assertEqual (Array.zeroCreate arr.Length)

    do
        let arr = [| -2..7 |]
        Array.clear arr
        arr |> Collection.assertEqual (Array.zeroCreate arr.Length)

[<Test>]
let ``contains (value type)`` () : unit =
    // Test the function with an array of value types.
    let ``0 to 10`` = [| 0 .. 10 |]

    ``0 to 10``
    |> Array.contains 5
    |> assertTrue

    ``0 to 10``
    |> Array.contains 15
    |> assertFalse

[<Test>]
let ``contains (reference type with IEquatable<T>)`` () : unit =
    // Test the function with an array of reference types,
    // where the element type implements IEquatable<'T>.
    let colors = [| "Black"; "Blue"; "Cyan"; "DarkBlue"; "DarkGray"; "DarkGreen";
                    "DarkMagenta"; "DarkRed"; "DarkYellow"; "Gray"; "Green"; |]

    colors
    |> Array.contains "DarkBlue"
    |> assertTrue

    colors
    |> Array.contains "Aquamarine"
    |> assertFalse

[<Test>]
let ``contains (reference type)`` () : unit =
    // Test the function with an array of reference types,
    // where the element type does NOT implement IEquatable<'T>.
    let ex = exn ()

    [| exn (); exn (); exn (); ex; exn (); |]
    |> Array.contains ex
    |> assertTrue

    [| exn (); exn (); exn (); exn (); exn (); |]
    |> Array.contains ex
    |> assertFalse

[<Test>]
let expandRight () : unit =
    (Array.empty : int[])
    |> Array.expandRight 10
    |> Collection.assertEqual <| Array.zeroCreate<int> 10

    [| 0; 1; 1; 2; 3; 5; 8; 13 |]
    |> Array.expandRight 4
    |> Collection.assertEqual [|
        0; 1; 1; 2; 3; 5; 8; 13; 0; 0; 0; 0 |]

[<Test>]
let expandLeft () : unit =
    (Array.empty : int[])
    |> Array.expandLeft 10
    |> Collection.assertEqual <| Array.zeroCreate<int> 10

    [| 0; 1; 1; 2; 3; 5; 8; 13 |]
    |> Array.expandLeft 4
    |> Collection.assertEqual [|
        0; 0; 0; 0; 0; 1; 1; 2; 3; 5; 8; 13 |]

[<Test>]
let tryFindBack () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let findBack () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let tryFindIndexBack () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let findIndexBack () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let tryPickBack () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let pickBack () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let foldi () : unit =
    ("", [| 'a' .. 'f' |])
    ||> Array.foldi (fun str idx c ->
        str + (String (Array.create idx c)))
    |> assertEqual "bccdddeeeefffff"

[<Test>]
let foldiBack () : unit =
    ([| 'a' .. 'f' |], "")
    ||> Array.foldiBack (fun idx c str ->
        str + (String (Array.create idx c)))
    |> assertEqual "fffffeeeedddccb"

[<Test>]
let split () : unit =
    /// The set of prime numbers less than 40.
    let primes =
        Set.ofArray [| 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; |]

    let chunks =
        [| 0 .. 40 |]
        |> Array.split (fun x ->
            Set.contains x primes)

    chunks
    |> Collection.assertEqual [|
        [| 0; 1 |];
        [| 2 |];
        [| 3; 4 |];
        [| 5; 6 |];
        [| 7 .. 10 |];
        [| 11; 12 |];
        [| 13 .. 16 |];
        [| 17; 18 |];
        [| 19 .. 22 |];
        [| 23 .. 28 |];
        [| 29; 30 |];
        [| 31 .. 36 |];
        [| 37 .. 40 |]; |]

[<Test>]
let segment () : unit =
    /// The set of prime numbers less than 40.
    let primes =
        Set.ofArray [| 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; |]

    let segments =
        [| 0 .. 40 |]
        |> Array.segment (fun x ->
            Set.contains x primes)

    segments
    |> Array.map (fun view ->
        view.Count)
    |> Collection.assertEqual [|
        2; 1; 2; 2; 4; 2; 4; 2; 4; 6; 2; 6; 4; |]

[<Test>]
let segment2 () : unit =
    let vowels = Set.ofArray [| 'a'; 'e'; 'i'; 'o'; 'u' |]

    let intSegments, charSegments =
        ([| 1..3..78 |], [| 'a'..'z' |])
        ||> Array.segment2 (fun x c ->
            x % 7 = 0 || Set.contains c vowels)

    // First, check that each pair of segments has the same number of elements.
    (intSegments, charSegments)
    ||> Array.forall2 (fun intSeg charSeg ->
        intSeg.Count = charSeg.Count)
    |> assertTrue

    // Now check that the array was segmented correctly.
    intSegments
    |> Array.map (fun view ->
        view.Count)
    |> Collection.assertEqual [| 2; 2; 4; 1; 5; 2; 4; 3; 3 |]

[<Test>]
let mapPartition () : unit =
    let left, right =
        [| 0 .. 10 |]
        |> Array.mapPartition (fun x ->
            if x % 2 = 0 then
                Choice1Of2 <| x.ToString ()
            else
                Choice2Of2 <| x * x * x)

    left
    |> Collection.assertEqual [| "0"; "2"; "4"; "6"; "8"; "10" |]

    right
    |> Collection.assertEqual [| 1; 27; 125; 343; 729 |]

[<Test>]
let mapiPartition () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let mapPartition3 () : unit =
    let left, middle, right =
        [| 0 .. 15 |]
        |> Array.mapPartition3 (fun x ->
            match x % 3 with
            | 0 ->
                Choice1Of3 <| (1 <<< x)
            | 1 ->
                Choice2Of3 <| x.ToString ()
            | _ ->
                Choice3Of3 <| x * x)

    left
    |> Collection.assertEqual [| 1; 8; 64; 512; 4096; 32768 |]

    middle
    |> Collection.assertEqual [| "1"; "4"; "7"; "10"; "13" |]

    right
    |> Collection.assertEqual [| 4; 25; 64; 121; 196 |]

[<Test>]
let mapReduce () : unit =
    let expected =
        Map.ofArray [| 'i', 4; 'm', 1; 'p', 2; 's', 4 |]

    "mississippi".ToCharArray ()
    |> Array.mapReduce
        { new IMapReduction<char, Map<char, int>> with
            member __.Map c =
                Map.singleton c 1
            member __.Reduce left right =
                (left, right)
                ||> Map.fold (fun charCounts c count ->
                    match Map.tryFind c charCounts with
                    | None ->
                        Map.add c count charCounts
                    | Some existingCount ->
                        Map.add c (existingCount + count) charCounts) }
    |> Collection.assertEqual expected

[<Test>]
let findIndices () : unit =
    let primeArray = [| 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; |]

    /// The set of prime numbers less than 40.
    let primes = Set.ofArray primeArray

    [| 0 .. 40 |]
    |> Array.findIndices (fun x ->
        Set.contains x primes)
    |> Collection.assertEqual primeArray

    Array.empty
    |> Array.findIndices (fun x ->
        Set.contains x primes)
    |> Collection.assertEqual (Array.empty : int[])

[<Test>]
let choosei () : unit =
    let colors =
        [| "Black"; "Blue"; "Cyan"; "DarkBlue"; "DarkGray";
           "DarkGreen"; "DarkMagenta"; "DarkRed"; "DarkYellow"; "Gray"; "Green" |]

    colors
    |> Array.choosei (fun idx colorName ->
        if String.length colorName <= idx then
            Some <| colorName.ToLower ()
        else None)
    |> Collection.assertEqual [|
        "darkred"; "gray"; "green" |]

[<Test>]
let choose2 () : unit =
    let colors =
        [| "Black"; "Blue"; "Cyan"; "DarkBlue"; "DarkGray";
           "DarkGreen"; "DarkMagenta"; "DarkRed"; "DarkYellow"; "Gray"; "Green" |]

    ([|10..-1..0|], colors)
    ||> Array.choose2 (fun x colorName ->
        if (x + String.length colorName) % 2 = 0 then
            Some <| colorName.ToLower ()
        else None)
    |> Collection.assertEqual
       [| "cyan"; "darkgray"; "darkgreen"; "darkred"; "darkyellow" |]

[<Test>]
let mapInPlace () : unit =
    // Test case for an empty array.
    do
        let arr = Array.empty
        arr
        |> Array.mapInPlace (fun el ->
            el * 2)
        arr |> Collection.assertEqual Array.empty

    // Sample usage test cases.
    do
        let arr = [| 0..4 |]
        arr
        |> Array.mapInPlace (fun el ->
            el * 2)
        arr |> Collection.assertEqual [| 0; 2; 4; 6; 8; |]

    do
        let colors =
            [| "Black"; "Blue"; "Cyan"; "DarkBlue"; "DarkGray";
               "DarkGreen"; "DarkMagenta"; "DarkRed"; "DarkYellow"; "Gray"; "Green"; |]
        colors
        |> Array.mapInPlace (fun colorName ->
            colorName.ToLowerInvariant ())
        colors |> Collection.assertEqual [|
            "black"; "blue"; "cyan"; "darkblue"; "darkgray";
            "darkgreen"; "darkmagenta"; "darkred"; "darkyellow"; "gray"; "green"; |]

[<Test>]
let mapiInPlace () : unit =
    // Test case for an empty array.
    do
        let arr = Array.empty
        arr
        |> Array.mapiInPlace (fun idx el ->
            el * (2 + idx))
        arr |> Collection.assertEqual Array.empty

    // Sample usage test cases.
    do
        let arr = [| 0..4 |]
        arr
        |> Array.mapiInPlace (fun idx el ->
            el * (2 + idx))
        arr |> Collection.assertEqual [| 0; 3; 8; 15; 24; |]

    do
        let colors =
            [| "Black"; "Blue"; "Cyan"; "DarkBlue"; "DarkGray";
               "DarkGreen"; "DarkMagenta"; "DarkRed"; "DarkYellow"; "Gray"; "Green"; |]
        colors
        |> Array.mapiInPlace (fun idx colorName ->
            if idx % 2 = 0 then
                colorName.ToLowerInvariant ()
            else
                colorName.ToUpperInvariant ())
        colors |> Collection.assertEqual [|
            "black"; "BLUE"; "cyan"; "DARKBLUE"; "darkgray";
            "DARKGREEN"; "darkmagenta"; "DARKRED"; "darkyellow"; "GRAY"; "green"; |]

[<Test>]
let chooseInPlace () : unit =
    // Test case for an empty array.
    do
        let arr = Array.empty
        arr
        |> Array.chooseInPlace (fun el ->
            if el > 0 && el % 2 = 0 then Some (el * 2)
            else None)
        arr |> Collection.assertEqual Array.empty

    // Sample usage test cases.
    do
        let arr = [| 0..4 |]
        arr
        |> Array.chooseInPlace (fun el ->
            if el > 0 && el % 2 = 0 then Some (el * 2)
            else None)
        arr |> Collection.assertEqual [| 0; 1; 4; 3; 8; |]

    do
        let colors =
            [| "Black"; "Blue"; "Cyan"; "DarkBlue"; "DarkGray";
               "DarkGreen"; "DarkMagenta"; "DarkRed"; "DarkYellow"; "Gray"; "Green"; |]
        colors
        |> Array.chooseInPlace (fun colorName ->
            if String.length colorName <= 5 then
                Some <| colorName.ToLowerInvariant ()
            else None)
        colors |> Collection.assertEqual [|
            "black"; "blue"; "cyan"; "DarkBlue"; "DarkGray";
            "DarkGreen"; "DarkMagenta"; "DarkRed"; "DarkYellow"; "gray"; "green"; |]

[<Test>]
let chooseiInPlace () : unit =
    // Test case for an empty array.
    do
        let arr = Array.empty
        arr
        |> Array.chooseiInPlace (fun idx el ->
            if idx % 2 = 0 then None
            else Some (el * (2 + idx)))
        arr |> assertEqual Array.empty

    // Sample usage test cases.
    do
        let arr = [| 0..4 |]
        arr
        |> Array.chooseiInPlace (fun idx el ->
            if idx % 2 = 0 then None
            else Some (el * (2 + idx)))
        arr |> Collection.assertEqual [| 0; 3; 2; 15; 4; |]

    do
        let colors =
            [| "Black"; "Blue"; "Cyan"; "DarkBlue"; "DarkGray";
               "DarkGreen"; "DarkMagenta"; "DarkRed"; "DarkYellow"; "Gray"; "Green"; |]
        colors
        |> Array.chooseiInPlace (fun idx colorName ->
            if idx % 2 = 0 then
                Some <| colorName.ToLowerInvariant ()
            else None)
        colors |> Collection.assertEqual [|
            "black"; "Blue"; "cyan"; "DarkBlue"; "darkgray";
            "DarkGreen"; "darkmagenta"; "DarkRed"; "darkyellow"; "Gray"; "green"; |]

[<Test>]
let countWith () : unit =
    // Test case for an empty array.
    Array.empty
    |> Array.countWith (fun x ->
        x % 7 = 0)
    |> assertEqual 0

    // Sample usage test cases.
    [| 0; 1; 2; 3; 4; 5; 6; 8; 9; 10; 16 |]
    |> Array.countWith (fun x ->
        x < 0)
    |> assertEqual 0

    [| 0; 1; 2; 3; 4; 5; 6; 8; 9; 10; 16 |]
    |> Array.countWith (fun x ->
        x % 7 = 0)
    |> assertEqual 1

    [| 0; 1; 2; 3; 4; 5; 6; 8; 9; 10; 16 |]
    |> Array.countWith (fun x ->
        x % 3 = 0)
    |> assertEqual 4

[<Test>]
let foldPairwise () : unit =
    // Count the number of occurrences where adjacent characters are the same.
    (0, "mississippi".ToCharArray ())
    ||> Array.foldPairwise (fun count x y ->
        if x = y then count + 1 else count)
    |> assertEqual 3

    (0, [| 0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144; |])
    ||> Array.foldPairwise (fun diffSum x y ->
        diffSum + (y - x))
    |> assertEqual 144

[<Test>]
let foldBackPairwise () : unit =
    // Count the number of occurrences where adjacent characters are the same.
    ("mississippi".ToCharArray (), 0)
    ||> Array.foldBackPairwise (fun x y count ->
        if x = y then count + 1 else count)
    |> assertEqual 3

[<Test>]
let unfold () : unit =
    5
    |> Array.unfold (function
        | 0 -> None
        | n ->
            Some (n.ToString(), n - 1))
    |> Collection.assertEqual (Array.ofList ["5"; "4"; "3"; "2"; "1"])

[<Test>]
let unfoldBack () : unit =
    5
    |> Array.unfoldBack (function
        | 0 -> None
        | n ->
            Some (n.ToString(), n - 1))
    |> Collection.assertEqual (Array.ofList ["1"; "2"; "3"; "4"; "5"])

[<Test>]
let init2 () : unit =
    Assert.Ignore "Test not yet implemented."


#if FX_NO_TPL_PARALLEL
#else
module Parallel =
    (* TODO : When implementing these tests, make sure to test both that the returned result is correct
              and also the behavior of the functions when an exception is thrown by the function being
              passed in (e.g., the generator function for init2). Exceptions thrown during the operation
              should be propagated to callers of these functions instead of returning partially-valid results. *)

    [<Test>]
    let init2 () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let mapInPlace () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let mapiInPlace () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let countWith () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let choosei () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let ``choosei repro #1`` () : unit =
        let n = 43
        let m = 27
        let arr = [|36; 26; 10; -21; -31; -22; -38; 20; 10; 19; 3; -28; -38; -29; 44; 34; 3; -13;
                      -4; -14; 44; 28; 37; 27; 11; -20; -11; -21; -37; 21; 11; 20; 4; -27; -37; -28;
                      -44; 35; 4; -12; -3|]

        // Chooser function.
        let chooser =
            fun i x ->
                if n = 0 then
                    if x > m then Some x else None
                else
                    if x / n < m then Some x else None


        let chooseResults =
            arr |> Array.Parallel.choose (chooser -1)

        let chooseiResults =
            arr |> Array.Parallel.choosei chooser

        Collection.assertEqual chooseResults chooseiResults

    [<Test>]
    let ``combineWorkerResults striped`` () =
        let arr = [|36; 26; 10; -21; -31; -22; -38; 20; 10; 19; 3; -28; -38; -29; 44; 34; 3; -13;
                      -4; -14; 44; 28; 37; 27; 11; -20; -11; -21; -37; 21; 11; 20; 4; -27; -37; -28;
                      -44; 35; 4; -12; -3|]

        let workerResults =
            let workerCount = 11
            let results = Array.init workerCount <| fun _ -> ResizeArray ()

            for i = 0 to arr.Length - 1 do
                System.Collections.Generic.KeyValuePair (i, arr.[i])
                |> results.[i % workerCount].Add

            ResizeArray results
                
        // Combine the results and check them.
        workerResults
        |> Array.Parallel.combineWorkerResults
        |> Collection.assertEqual arr


    /// FsCheck-based tests for Array.Parallel functions.
    module FsCheck =
        open FsCheck

        (* Tests *)

        [<Test>]
        let ``choose and choosei equivalent``() =
            assertProp "choose and choosei equivalent" <| fun (n : int) (m : int) (arr : int[]) ->
                // Given some input array of integers, choose some of the values according to n and m.
                // Do this with both Array.Parallel.choose and Array.Parallel.choosei and check that they produce the same results.
                
                // Chooser function.
                let chooser =
                    fun i x ->
                        if n = 0 then
                            if x > m then Some x else None
                        else
                            if x / n < m then Some x else None


                let chooseResults =
                    arr |> Array.Parallel.choose (chooser -1)

                let chooseiResults =
                    arr |> Array.Parallel.choosei chooser

                chooseResults = chooseiResults

#endif
