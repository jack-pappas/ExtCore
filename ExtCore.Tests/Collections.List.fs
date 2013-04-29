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

/// Unit tests for the ExtCore.Collections.List module.
module Tests.ExtCore.Collections.List

open System
open NUnit.Framework
open FsUnit
//open FsCheck


[<Test>]
let indexed () : unit =
    ["Red"; "Green"; "Blue"; "Yellow"]
    |> List.indexed
    |> assertEqual
       [0, "Red";
        1, "Green";
        2, "Blue";
        3, "Yellow"]

[<Test>]
let revIntoArray () : unit =
    List.empty
    |> List.revIntoArray
    |> assertEqual Array.empty

    ["Red"; "Green"; "Blue"; "Yellow"]
    |> List.revIntoArray
    |> assertEqual
       [| "Yellow"; "Blue"; "Green"; "Red" |]

[<Test>]
let revMapIntoArray () : unit =
    List.empty
    |> List.revMapIntoArray ignore
    |> assertEqual Array.empty

    ["Red"; "Green"; "Blue"; "Yellow"]
    |> List.revMapIntoArray String.length
    |> assertEqual [| 6; 4; 5; 3 |]

[<Test>]
let projectValues () : unit =
    List.empty
    |> List.projectValues ignore
    |> assertEqual List.empty

    [ ConsoleColor.Magenta;
      ConsoleColor.DarkGreen;
      ConsoleColor.Cyan;
      ConsoleColor.Black; ]
    |> List.projectValues (sprintf "%O")
    |> assertEqual
       [ConsoleColor.Magenta, "Magenta";
        ConsoleColor.DarkGreen, "DarkGreen";
        ConsoleColor.Cyan, "Cyan";
        ConsoleColor.Black, "Black"; ]

[<Test>]
let projectKeys () : unit =
    List.empty
    |> List.projectKeys ignore
    |> assertEqual List.empty

    ["Magenta"; "DarkGreen"; "Cyan"; "Black"]
    |> List.projectKeys (fun colorName ->
        Enum.Parse (typeof<ConsoleColor>, colorName)
        :?> System.ConsoleColor)
    |> assertEqual
       [ConsoleColor.Magenta, "Magenta";
        ConsoleColor.DarkGreen, "DarkGreen";
        ConsoleColor.Cyan, "Cyan";
        ConsoleColor.Black, "Black"; ]

[<Test>]
let take () : unit =
    let taken, remaining =
        [0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144]
        |> List.take 5

    taken
    |> assertEqual [0; 1; 1; 2; 3]

    remaining
    |> assertEqual [5; 8; 13; 21; 34; 55; 89; 144]

[<Test>]
let takeArray () : unit =
    let taken, remaining =
        [0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144]
        |> List.takeArray 5

    taken
    |> assertEqual [| 0; 1; 1; 2; 3 |]

    remaining
    |> assertEqual [5; 8; 13; 21; 34; 55; 89; 144]

[<Test>]
let foldPairwise () : unit =
    // Count the number of occurrences where adjacent characters are the same.
    (0, List.ofArray <| "mississippi".ToCharArray ())
    ||> List.foldPairwise (fun count x y ->
        if x = y then count + 1 else count)
    |> assertEqual 3

    (0, [0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144])
    ||> List.foldPairwise (fun diffSum x y ->
        diffSum + (y - x))
    |> assertEqual 144

[<Test>]
let foldBackPairwise () : unit =
    // Count the number of occurrences where adjacent characters are the same.
    (List.ofArray <| "mississippi".ToCharArray (), 0)
    ||> List.foldBackPairwise (fun x y count ->
        if x = y then count + 1 else count)
    |> assertEqual 3

[<Test>]
let mapPartition () : unit =
    let left, right =
        [0 .. 10]
        |> List.mapPartition (fun x ->
            if x % 2 = 0 then
                Choice1Of2 <| x.ToString ()
            else
                Choice2Of2 <| x * x * x)

    left
    |> assertEqual ["0"; "2"; "4"; "6"; "8"; "10"]

    right
    |> assertEqual [1; 27; 125; 343; 729]

[<Test>]
let mapPartition3 () : unit =
    let left, middle, right =
        [0 .. 15]
        |> List.mapPartition3 (fun x ->
            match x % 3 with
            | 0 ->
                Choice1Of3 <| (1 <<< x)
            | 1 ->
                Choice2Of3 <| x.ToString ()
            | _ ->
                Choice3Of3 <| x * x)

    left
    |> assertEqual [1; 8; 64; 512; 4096; 32768]

    middle
    |> assertEqual ["1"; "4"; "7"; "10"; "13"]

    right
    |> assertEqual [4; 25; 64; 121; 196]

[<Test>]
let choose2 () : unit =
    let colors =
       ["Black"; "Blue"; "Cyan"; "DarkBlue"; "DarkGray";
        "DarkGreen"; "DarkMagenta"; "DarkRed"; "DarkYellow"; "Gray"; "Green"]

    ([10..-1..0], colors)
    ||> List.choose2 (fun x colorName ->
        if (x + String.length colorName) % 2 = 0 then
            Some <| colorName.ToLower ()
        else None)
    |> assertEqual
       ["cyan"; "darkgray"; "darkgreen"; "darkred"; "darkyellow"]


[<Test>]
let unfold () : unit =
    5
    |> List.unfold (function
        | 0 -> None
        | n ->
            Some (n.ToString(), n - 1))
    |> assertEqual ["5"; "4"; "3"; "2"; "1"]

[<Test>]
let unzipWith () : unit =
    let left, right =
        ["Black"; "Blue"; "Cyan"; "DarkBlue"; "DarkGray";]
        |> List.unzipWith (fun colorName ->
            String.length colorName,
            Enum.Parse (typeof<ConsoleColor>, colorName) :?> ConsoleColor)

    left
    |> assertEqual [5; 4; 4; 8; 8]

    right
    |> assertEqual [
        ConsoleColor.Black;
        ConsoleColor.Blue;
        ConsoleColor.Cyan;
        ConsoleColor.DarkBlue;
        ConsoleColor.DarkGray; ]

[<Test>]
let countWith () : unit =
    // Test case for an empty list.
    List.empty
    |> List.countWith (fun x ->
        x % 7 = 0)
    |> assertEqual 0

    // Sample usage test cases.
    [0; 1; 2; 3; 4; 5; 6; 8; 9; 10; 16]
    |> List.countWith (fun x ->
        x < 0)
    |> assertEqual 0

    [0; 1; 2; 3; 4; 5; 6; 8; 9; 10; 16]
    |> List.countWith (fun x ->
        x % 7 = 0)
    |> assertEqual 1

    [0; 1; 2; 3; 4; 5; 6; 8; 9; 10; 16]
    |> List.countWith (fun x ->
        x % 3 = 0)
    |> assertEqual 4
