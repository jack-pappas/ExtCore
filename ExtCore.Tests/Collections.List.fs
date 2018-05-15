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


[<Test>]
let cons () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let optcons () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let tryHead () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let singleton () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let ofOption () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let ofString () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let toString () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let contains () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let last () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let dropLast () : unit =
    Assert.Ignore "Test not yet implemented."

[<Test>]
let indexed () : unit =
    ["Red"; "Green"; "Blue"; "Yellow"]
    |> List.indexed
    |> Collection.assertEqual
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
    |> Collection.assertEqual
       [| "Yellow"; "Blue"; "Green"; "Red" |]

[<Test>]
let revMapIntoArray () : unit =
    List.empty
    |> List.revMapIntoArray ignore
    |> assertEqual Array.empty

    ["Red"; "Green"; "Blue"; "Yellow"]
    |> List.revMapIntoArray String.length
    |> Collection.assertEqual [| 6; 4; 5; 3 |]

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
    |> Collection.assertEqual
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
    |> Collection.assertEqual
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
    |> Collection.assertEqual [0; 1; 1; 2; 3]

    remaining
    |> Collection.assertEqual [5; 8; 13; 21; 34; 55; 89; 144]

[<Test>]
let takeArray () : unit =
    let taken, remaining =
        [0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144]
        |> List.takeArray 5

    taken
    |> Collection.assertEqual [| 0; 1; 1; 2; 3 |]

    remaining
    |> Collection.assertEqual [5; 8; 13; 21; 34; 55; 89; 144]

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
    |> Collection.assertEqual ["0"; "2"; "4"; "6"; "8"; "10"]

    right
    |> Collection.assertEqual [1; 27; 125; 343; 729]

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
    |> Collection.assertEqual [1; 8; 64; 512; 4096; 32768]

    middle
    |> Collection.assertEqual ["1"; "4"; "7"; "10"; "13"]

    right
    |> Collection.assertEqual [4; 25; 64; 121; 196]

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
    |> Collection.assertEqual
       ["cyan"; "darkgray"; "darkgreen"; "darkred"; "darkyellow"]


[<Test>]
let unfold () : unit =
    5
    |> List.unfold (function
        | 0 -> None
        | n ->
            Some (n.ToString(), n - 1))
    |> Collection.assertEqual ["5"; "4"; "3"; "2"; "1"]

[<Test>]
let unfoldBack () : unit =
    5
    |> List.unfoldBack (function
        | 0 -> None
        | n ->
            Some (n.ToString(), n - 1))
    |> Collection.assertEqual ["1"; "2"; "3"; "4"; "5"]

[<Test>]
let unzipWith () : unit =
    let left, right =
        ["Black"; "Blue"; "Cyan"; "DarkBlue"; "DarkGray";]
        |> List.unzipWith (fun colorName ->
            String.length colorName,
            Enum.Parse (typeof<ConsoleColor>, colorName) :?> ConsoleColor)

    left
    |> Collection.assertEqual [5; 4; 4; 8; 8]

    right
    |> Collection.assertEqual [
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
    |> assertEqual 0L

    // Sample usage test cases.
    [0; 1; 2; 3; 4; 5; 6; 8; 9; 10; 16]
    |> List.countWith (fun x ->
        x < 0)
    |> assertEqual 0L

    [0; 1; 2; 3; 4; 5; 6; 8; 9; 10; 16]
    |> List.countWith (fun x ->
        x % 7 = 0)
    |> assertEqual 1L

    [0; 1; 2; 3; 4; 5; 6; 8; 9; 10; 16]
    |> List.countWith (fun x ->
        x % 3 = 0)
    |> assertEqual 4L

[<Test>]
let intersperse () : unit =
    // Test case for an empty list.
    List.empty
    |> List.intersperse 9999
    |> List.isEmpty
    |> assertTrue

    // Test case for a single-element list.
    [10]
    |> List.intersperse 5
    |> Collection.assertEqual [10]

    // Sample usage test cases.
    ["Mock"; "ing"; "bird"]
    |> List.intersperse "yeah"
    |> Collection.assertEqual ["Mock"; "yeah"; "ing"; "yeah"; "bird"]

    [1; 2; 3; 4]
    |> List.intersperse 5
    |> Collection.assertEqual [1; 5; 2; 5; 3; 5; 4]

[<Test>]
let weave () : unit =
    // Test case for both lists empty.
    List.weave [] []
    |> List.isEmpty
    |> assertTrue

    // Test case for the first list being empty.
    // The second list should be returned immediately (without making a copy).
    do
        let lst = [3; 1; 0; 2]
        List.weave [] lst
        |> assertSame lst

    // Test case for the second list being empty.
    // The first list should be returned immediately (without making a copy).
    do
        let lst = [3; 1; 0; 2]
        List.weave lst []
        |> assertSame lst

    // Sample usage test cases.
    List.weave [1] [0; 2; 4]
    |> Collection.assertEqual [1; 0; 2; 4]

    List.weave [0; 2; 4] [1]
    |> Collection.assertEqual [0; 1; 2; 4]

    List.weave [2; 4; 8; 16] [1; 2; 3; 4]
    |> Collection.assertEqual [2; 1; 4; 2; 8; 3; 16; 4]

[<Test>]
let exactlyOne () : unit =
    // Test case for an empty string.
    assertRaises<System.ArgumentException> <| fun () ->
        List.exactlyOne []
        |> ignore

    // Test case for a single-element list.
    ["Hello World!"]
    |> List.exactlyOne
    |> assertEqual "Hello World!"

    // Test case for a multi-element list.
    assertRaises<System.ArgumentException> <| fun () ->
        List.exactlyOne [0; 1; 2; 3; 4; 5; 6; 8; 9; 10; 16]
        |> ignore

[<Test>]
let distinct () : unit =
    // Test case for an empty string.
    List.empty
    |> List.distinct
    |> List.isEmpty
    |> assertTrue

    // Sample usage test cases.
    [0; 1; 2; 3; 4]
    |> List.distinct
    |> Collection.assertEqual [0; 1; 2; 3; 4]

    [0; 1; 1; 2; 3; 2; 4; 5; 2]
    |> List.distinct
    |> Collection.assertEqual [0; 1; 2; 3; 4; 5]

    [0; 2; 4; 1; 2; 0; 1; 3]
    |> List.distinct
    |> Collection.assertEqual [0; 2; 4; 1; 3]

    // Repro for issue #8
    ["A"; "N"; "N"; "A"]
    |> List.distinct
    |> Collection.assertEqual ["A"; "N"]

[<Test>]
let difference () : unit =
    // Test case that mirrors Haskell's for (\\)
    List.difference ("Hello World!" |> Seq.toList) ("ell W" |> Seq.toList)
    |> Collection.assertEqual ("Hoorld!" |> Seq.toList)

    // Empty list1
    List.difference [] [1; 2; 3; 4; 5; 6; 7; 8; 9; 0]
    |> Collection.assertEqual []

    // Empty list2
    List.difference [1; 2; 3; 4; 5; 6; 7; 8; 9; 0] []
    |> Collection.assertEqual [1; 2; 3; 4; 5; 6; 7; 8; 9; 0]

    // Both empty
    List.difference [] []
    |> Collection.assertEqual []

    // Test for removing the same list from another (invariant from Haskell docs).
    // ((xs ++ ys) \\ xs) == ys
    List.difference ([6; 7; 8; 9; 0] @ [1; 2; 3; 4; 5; 6; 7; 8; 9; 0]) [6; 7; 8; 9; 0]
    |> Collection.assertEqual [1; 2; 3; 4; 5; 6; 7; 8; 9; 0]



module FsCheck =
    open FsCheck

    [<Test>]
    let ``List difference random testing``() =
        assertProp "List difference random testing" <| fun (m:int list) (n:int list) ->
            if isNull m || isNull n then
                assertRaises<System.ArgumentNullException>
                    (fun () -> List.difference m n |> ignore)
            else ()

            let removeFirst p xs =
                match List.tryFindIndex p xs with
                | Some i -> (List.take i xs |> fst ) @ (List.skip (i+1) xs)
                | None -> xs

            let o = List.fold (fun state el -> removeFirst ((=) el) state) m n

            List.difference m n = o

(* TODO : Implement randomized tests with FsCheck. *)
