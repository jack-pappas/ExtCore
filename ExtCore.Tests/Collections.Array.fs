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
open FsUnit
//open FsCheck


[<Test>]
let projectValues () : unit =
    Array.empty
    |> Array.projectValues ignore
    |> assertEqual Array.empty

    [|  ConsoleColor.Magenta;
        ConsoleColor.DarkGreen;
        ConsoleColor.Cyan;
        ConsoleColor.Black; |]
    |> Array.projectValues (sprintf "%O")
    |> assertEqual [|
        ConsoleColor.Magenta, "Magenta";
        ConsoleColor.DarkGreen, "DarkGreen";
        ConsoleColor.Cyan, "Cyan";
        ConsoleColor.Black, "Black"; |]

[<Test>]
let projectKeys () : unit =
    Array.empty
    |> Array.projectKeys ignore
    |> assertEqual Array.empty

    [|"Magenta"; "DarkGreen"; "Cyan"; "Black"|]
    |> Array.projectKeys (fun colorName ->
        Enum.Parse (typeof<ConsoleColor>, colorName)
        :?> System.ConsoleColor)
    |> assertEqual [|
        ConsoleColor.Magenta, "Magenta";
        ConsoleColor.DarkGreen, "DarkGreen";
        ConsoleColor.Cyan, "Cyan";
        ConsoleColor.Black, "Black"; |]

[<Test>]
let clear () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let ``contains (value type)`` () : unit =
    // Test the function with an array of value types.
    let ``0 to 10`` = [| 0 .. 10 |]

    ``0 to 10``
    |> Array.contains 5
    |> should be True

    ``0 to 10``
    |> Array.contains 15
    |> should be False

[<Test>]
let ``contains (reference type with IEquatable<T>)`` () : unit =
    // Test the function with an array of reference types,
    // where the element type implements IEquatable<'T>.
    let colors = [| "Black"; "Blue"; "Cyan"; "DarkBlue"; "DarkGray"; "DarkGreen";
                    "DarkMagenta"; "DarkRed"; "DarkYellow"; "Gray"; "Green"; |]

    colors
    |> Array.contains "DarkBlue"
    |> should be True

    colors
    |> Array.contains "Aquamarine"
    |> should be False

[<Test>]
let ``contains (reference type)`` () : unit =
    // Test the function with an array of reference types,
    // where the element type does NOT implement IEquatable<'T>.
    let ex = exn ()

    [| exn (); exn (); exn (); ex; exn (); |]
    |> Array.contains ex
    |> should be True

    [| exn (); exn (); exn (); exn (); exn (); |]
    |> Array.contains ex
    |> should be False

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
    |> assertEqual [|
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
    |> assertEqual [|
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
    |> should be True

    // Now check that the array was segmented correctly.
    intSegments
    |> Array.map (fun view ->
        view.Count)
    |> assertEqual [| 2; 2; 4; 1; 5; 2; 4; 3; 3 |]
        
[<Test>]
let expandRight () : unit =
    (Array.empty : int[])
    |> Array.expandRight 10
    |> assertEqual <| Array.zeroCreate<int> 10

    [| 0; 1; 1; 2; 3; 5; 8; 13 |]
    |> Array.expandRight 4
    |> assertEqual [|
        0; 1; 1; 2; 3; 5; 8; 13; 0; 0; 0; 0 |]

[<Test>]
let expandLeft () : unit =
    (Array.empty : int[])
    |> Array.expandLeft 10
    |> assertEqual <| Array.zeroCreate<int> 10

    [| 0; 1; 1; 2; 3; 5; 8; 13 |]
    |> Array.expandLeft 4
    |> assertEqual [|
        0; 0; 0; 0; 0; 1; 1; 2; 3; 5; 8; 13 |]

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
    |> assertEqual [| "0"; "2"; "4"; "6"; "8"; "10" |]

    right
    |> assertEqual [| 1; 27; 125; 343; 729 |]

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
    |> assertEqual [| 1; 8; 64; 512; 4096; 32768 |]

    middle
    |> assertEqual [| "1"; "4"; "7"; "10"; "13" |]

    right
    |> assertEqual [| 4; 25; 64; 121; 196 |]

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
    |> assertEqual expected

[<Test>]
let findIndices () : unit =
    let primeArray = [| 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; |]

    /// The set of prime numbers less than 40.
    let primes = Set.ofArray primeArray

    [| 0 .. 40 |]
    |> Array.findIndices (fun x ->
        Set.contains x primes)
    |> assertEqual primeArray

    Array.empty
    |> Array.findIndices (fun x ->
        Set.contains x primes)
    |> assertEqual (Array.empty : int[])

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
    |> assertEqual [|
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
    |> assertEqual
       [| "cyan"; "darkgray"; "darkgreen"; "darkred"; "darkyellow" |]

[<Test>]
let mapInPlace () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let mapiInPlace () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let chooseInPlace () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let chooseiInPlace () : unit =
    Assert.Ignore "Test not yet implemented."

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
