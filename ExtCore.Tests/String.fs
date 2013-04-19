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

namespace Tests.ExtCore

open NUnit.Framework
open FsUnit
//open FsCheck


/// Tests for the ExtCore.Substring module.
module Substring =
    // TODO : Implement equality/comparison tests for substring.

    [<TestCase>]
    let get () : unit =
        do
            let substr = Substring ("Hello World!", 3, 5)
            
            Substring.get substr 1
            |> should equal 'o'

            Substring.get substr 3
            |> should equal 'W'

    [<TestCase>]
    let isEmpty () : unit =
        Substring ("Hello World!", 4, 0)
        |> Substring.isEmpty
        |> should be True

        Substring ("Hello World!", 3, 4)
        |> Substring.isEmpty
        |> should be False

    [<TestCase>]
    let ofString () : unit =
        do
            let substr = Substring.ofString ""

            substr
            |> Substring.string
            |> should equal ""

            substr
            |> Substring.offset
            |> should equal 0

            substr
            |> Substring.length
            |> should equal 0

        do
            let substr = Substring.ofString "Hello World!"

            substr
            |> Substring.string
            |> should equal "Hello World!"

            substr
            |> Substring.offset
            |> should equal 0

            substr
            |> Substring.length
            |> should equal 12

    [<TestCase>]
    let toString () : unit =
        // Test for empty substring.
        Substring ("Hello World!", 3, 0)
        |> Substring.toString
        |> should equal String.empty

        Substring ("Hello World!", 3, 6)
        |> Substring.toString
        |> should equal "lo Wor"

    [<TestCase>]
    let toArray () : unit =
        // Test for empty substring.
        Substring ("Hello World!", 3, 0)
        |> Substring.toArray
        |> should equal Array.empty

        Substring ("Hello World!", 3, 6)
        |> Substring.toArray
        |> should equal [| 'l'; 'o'; ' '; 'W'; 'o'; 'r'; |]

    [<TestCase>]
    let sub () : unit =
        do
            let str = "The quick brown fox jumps over the lazy dog."

            let substr = Substring (str, 4, 15)  // "quick brown fox"
            
            Substring.sub substr 6 5
            |> Substring.toString
            |> should equal "brown"

    [<TestCase>]
    let concat () : unit =
        do
            let str1 = "The quick brown fox jumps over the lazy dog."
            let str2 = "Hello World!"

            seq {
            yield Substring (str1, 0, 35)
            yield Substring (str2, 6, 6)
            yield Substring (str1, 3, 1)
            yield Substring (str2, 0, 6)
            yield Substring (str1, 35, 9) }
            |> Substring.concat
            |> should equal "The quick brown fox jumps over the World! Hello lazy dog."

    [<TestCase>]
    let iter () : unit =
        // Test case for empty substring.
        do
            let elements = ResizeArray ()

            Substring ("The quick brown fox jumps over the lazy dog.", 4, 0)
            |> Substring.iter (System.Char.ToUpper >> elements.Add)

            elements.ToArray ()
            |> Array.isEmpty
            |> should be True

        // Test case for "normal" usage of this function.
        do
            let elements = ResizeArray ()

            Substring ("The quick brown fox jumps over the lazy dog.", 4, 15)
            |> Substring.iter (System.Char.ToUpper >> elements.Add)

            elements.ToArray ()
            |> should equal
                [| 'Q'; 'U'; 'I'; 'C'; 'K'; ' '; 'B'; 'R'; 'O'; 'W'; 'N'; ' '; 'F'; 'O'; 'X'; |]

    [<TestCase>]
    let iteri () : unit =
        // Test case for empty substring.
        do
            let elements = ResizeArray ()

            Substring ("The quick brown fox jumps over the lazy dog.", 4, 0)
            |> Substring.iteri (fun idx c ->
                elements.Add (
                    if idx % 2 = 0 then System.Char.ToUpper c else c))

            elements.ToArray ()
            |> Array.isEmpty
            |> should be True

        // Test case for "normal" usage of this function.
        do
            let elements = ResizeArray ()

            Substring ("The quick brown fox jumps over the lazy dog.", 4, 15)
            |> Substring.iteri (fun idx c ->
                elements.Add (
                    if idx % 2 = 0 then System.Char.ToUpper c else c))

            elements.ToArray ()
            |> should equal
                [| 'Q'; 'u'; 'I'; 'c'; 'K'; ' '; 'B'; 'r'; 'O'; 'w'; 'N'; ' '; 'F'; 'o'; 'X'; |]

    [<TestCase>]
    let iterBack () : unit =
        // Test case for empty substring.
        do
            let elements = ResizeArray ()

            Substring ("The quick brown fox jumps over the lazy dog.", 4, 0)
            |> Substring.iterBack (System.Char.ToUpper >> elements.Add)

            elements.ToArray ()
            |> Array.isEmpty
            |> should be True

        // Test case for "normal" usage of this function.
        do
            let elements = ResizeArray ()

            Substring ("The quick brown fox jumps over the lazy dog.", 4, 15)
            |> Substring.iterBack (System.Char.ToUpper >> elements.Add)

            elements.ToArray ()
            |> should equal
                [| 'X'; 'O'; 'F'; ' '; 'N'; 'W'; 'O'; 'R'; 'B'; ' '; 'K'; 'C'; 'I'; 'U'; 'Q'; |]

    [<TestCase>]
    let fold () : unit =
        // Test case for empty substring.
        do
            ((0L, 0), Substring ("The quick brown fox jumps over the lazy dog.", 4, 0))
            ||> Substring.fold (fun (checksum, index) c ->
                (checksum + int64 index) * int64 c,
                index + 1)
            |> fst  // Discard the index
            |> should equal 0L

        // Test case for "normal" usage of this function.
        do
            ((0L, 0), Substring ("The quick brown fox jumps over the lazy dog.", 4, 15))
            ||> Substring.fold (fun (checksum, index) c ->
                (checksum + int64 index) * int64 c,
                index + 1)
            |> fst  // Discard the index
            |> should equal 8117010307721961272L

    [<TestCase>]
    let foldi () : unit =
        // Test case for empty substring.
        do
            (0L, Substring ("The quick brown fox jumps over the lazy dog.", 4, 0))
            ||> Substring.foldi (fun checksum index c ->
                (checksum + int64 index) * int64 c)
            |> should equal 0L

        // Test case for "normal" usage of this function.
        do
            (0L, Substring ("The quick brown fox jumps over the lazy dog.", 4, 15))
            ||> Substring.foldi (fun checksum index c ->
                (checksum + int64 index) * int64 c)
            |> should equal 8117010307721961272L

    [<TestCase>]
    let foldBack () : unit =
        // Test case for empty substring.
        do
            (Substring ("The quick brown fox jumps over the lazy dog.", 4, 0), (0L, 0))
            ||> Substring.foldBack (fun c (checksum, index) ->
                (checksum + int64 index) * int64 c,
                index + 1)
            |> fst  // Discard the index
            |> should equal 0L

        // Test case for "normal" usage of this function.
        do
            (Substring ("The quick brown fox jumps over the lazy dog.", 4, 15), (0L, 0))
            ||> Substring.foldBack (fun c (checksum, index) ->
                (checksum + int64 index) * int64 c,
                index + 1)
            |> fst  // Discard the index
            |> should equal -8792059055315210054L


/// Tests for the ExtCore.String module.
module String =
    [<TestCase>]
    let isEmpty () : unit =
        // Test case for empty string.
        String.isEmpty String.empty
        |> should be True

        // Test case for any other string.
        "The quick brown fox jumps over the lazy dog."
        |> String.isEmpty
        |> should be False

    [<TestCase>]
    let ofOption () : unit =
        // Test case for None.
        String.ofOption None
        |> String.isEmpty
        |> should be True

        // Test case for Some containing an empty string.
        Some String.empty
        |> String.ofOption
        |> String.isEmpty
        |> should be True

        // Test case for Some containing a non-empty string.
        Some "Hello World!"
        |> String.ofOption
        |> should equal "Hello World!"

    [<TestCase>]
    let ofArray () : unit =
        // Test case for empty array.
        Array.empty
        |> String.ofArray
        |> String.isEmpty
        |> should be True

        // Test case for a non-empty array.
        [| 'H'; 'e'; 'l'; 'l'; 'o'; ' '; 'W'; 'o'; 'r'; 'l'; 'd'; '!'; |]
        |> String.ofArray
        |> should equal "Hello World!"

    [<TestCase>]
    let toOption () : unit =
        // Test case for null.
        String.toOption null
        |> Option.isNone
        |> should be True

        // Test case for empty string.
        String.toOption String.empty
        |> Option.isNone
        |> should be True

        // Test case for a non-empty string.
        "Hello World!"
        |> String.toOption
        |> should equal (Some "Hello World!")

    [<TestCase>]
    let toArray () : unit =
        // Test case for an empty string.
        String.empty
        |> String.toArray
        |> Array.isEmpty
        |> should be True

        // Test case for a non-empty string.
        "Hello World!"
        |> String.toArray
        |> should equal
            [| 'H'; 'e'; 'l'; 'l'; 'o'; ' '; 'W'; 'o'; 'r'; 'l'; 'd'; '!'; |]

    [<TestCase>]
    let split () : unit =
        // Test case for an empty string.
        String.empty
        |> String.split [| ','; |]
        |> Array.isEmpty
        |> should be True

        // Test case for a non-empty string which does not contain the specified characters.
        "Hello World!"
        |> String.split [| ','; |]
        |> should equal [| "Hello World!"; |]

        // Test case for a string which does contain the specified characters.
        "Id,Title,First,MI,Last,DOB"
        |> String.split [| ','; |]
        |> should equal
            [| "Id"; "Title"; "First"; "MI"; "Last"; "DOB"; |]

        // Test case for a string which does contain the specified characters,
        // and where there are adjacent occurrences of the characters.
        "3262,,John,Q,Doe,1970-Jan-01"
        |> String.split [| ','; |]
        |> should equal
            [| "3262"; ""; "John"; "Q"; "Doe"; "1970-Jan-01"; |]

    [<TestCase>]
    let concatArray () : unit =
        // Test case for empty array.
        Array.empty
        |> String.concatArray
        |> String.isEmpty
        |> should be True

        // Test case for non-empty array.
        [| "The"; "quick"; "brown"; "fox"; "jumps"; "over"; "the"; "lazy"; "dog."; |]
        |> String.concatArray
        |> should equal
            "Thequickbrownfoxjumpsoverthelazydog."

    [<TestCase>]
    let ofLines () : unit =
        // Test case for empty array.
        Array.empty
        |> String.ofLines
        |> String.isEmpty
        |> should be True

        // Sample usage test case.
        [| "Lorem"; "ipsum"; "dolor"; "sit"; "amet"; |]
        |> String.ofLines
        |> should equal @"Lorem
ipsum
dolor
sit
amet"

    [<TestCase>]
    let toLines () : unit =
        // Test case for empty string.
        String.empty
        |> String.toLines
        |> Array.isEmpty
        |> should be True

        // Test case for a single-line string.
        "The quick brown fox jumps over the lazy dog."
        |> String.toLines
        |> should equal
            [| "The quick brown fox jumps over the lazy dog."; |]

        // Test case for a multi-line string.
        @"Lorem
ipsum
dolor
sit
amet"
        |> String.toLines
        |> should equal
            [| "Lorem"; "ipsum"; "dolor"; "sit"; "amet"; |]

    [<TestCase>]
    let sub () : unit =
        // Test case for empty substring of an empty string.
        String.sub String.empty 0 0
        |> ExtCore.Substring.isEmpty
        |> should be True

        // Test case for empty substring of a non-empty string.
        String.sub "The quick brown fox jumps over the lazy dog." 4 0
        |> ExtCore.Substring.toString
        |> String.isEmpty
        |> should be True

        // Test case for a non-empty substring of a non-empty string.
        String.sub "The quick brown fox jumps over the lazy dog." 4 15
        |> ExtCore.Substring.toString
        |> should equal "quick brown fox"

    [<TestCase>]
    let tryFindIndexOf () : unit =
        // Test case for empty string.
        String.empty
        |> String.tryFindIndexOf 'j'
        |> should equal None

        // Test case for a string which does not contain the specified character.
        "Hello World!"
        |> String.tryFindIndexOf 'j'
        |> should equal None

        // Test case for a string which contains exactly one (1) instance of the specified character.
        "The quick brown fox jumps over the lazy dog."
        |> String.tryFindIndexOf 'j'
        |> should equal (Some 20)

        // Test case for a string which contains multiple instances of the specified character.
        "Hello World!"
        |> String.tryFindIndexOf 'l'
        |> should equal (Some 2)

    [<TestCase>]
    let findIndexOf () : unit =
        // Test case for a string which contains exactly one (1) instance of the specified character.
        "The quick brown fox jumps over the lazy dog."
        |> String.findIndexOf 'j'
        |> should equal 20

        // Test case for a string which contains multiple instances of the specified character.
        "Hello World!"
        |> String.findIndexOf 'l'
        |> should equal 2

    [<TestCase; ExpectedException(typeof<System.Collections.Generic.KeyNotFoundException>)>]
    let ``findIndexOf raises exn when the string does not contain the character`` () : unit =
        // Test case for a string which does not contain the specified character.
        "Hello World!"
        |> String.findIndexOf 'j'
        |> ignore

    [<TestCase>]
    let tryFindIndex () : unit =
        // Test case for empty string.
        String.empty
        |> String.tryFindIndex (fun c ->
            // Is this a vowel (in English)?
            let c = System.Char.ToLowerInvariant c
            c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u')
        |> should equal None

        // Test case for a non-empty string which does not contain a matching character.
        "Glyndyfrdwy"
        |> String.tryFindIndex (fun c ->
            // Is this a vowel (in English)?
            let c = System.Char.ToLowerInvariant c
            c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u')
        |> should equal None

        // Test case for a string which contains exactly one (1) matching character.
        "Rhythmist"
        |> String.tryFindIndex (fun c ->
            // Is this a vowel (in English)?
            let c = System.Char.ToLowerInvariant c
            c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u')
        |> should equal (Some 6)

        // Test case for a string which contains multiple matching characters.
        "Hello World!"
        |> String.tryFindIndex (fun c ->
            // Is this a vowel (in English)?
            let c = System.Char.ToLowerInvariant c
            c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u')
        |> should equal (Some 1)

    [<TestCase>]
    let findIndex () : unit =
        // Test case for a string which contains exactly one (1) matching character.
        "Rhythmist"
        |> String.findIndex (fun c ->
            // Is this a vowel (in English)?
            let c = System.Char.ToLowerInvariant c
            c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u')
        |> should equal 6

        // Test case for a string which contains multiple matching characters.
        "Hello World!"
        |> String.findIndex (fun c ->
            // Is this a vowel (in English)?
            let c = System.Char.ToLowerInvariant c
            c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u')
        |> should equal 1

    [<TestCase; ExpectedException(typeof<System.Collections.Generic.KeyNotFoundException>)>]
    let ``findIndex raises exn when the string does not contain a matching character`` () : unit =
        // Test case for a non-empty string which does not contain a matching character.
        "Glyndyfrdwy"
        |> String.findIndex (fun c ->
            // Is this a vowel (in English)?
            let c = System.Char.ToLowerInvariant c
            c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u')
        |> ignore

    [<TestCase>]
    let fold () : unit =
        // Test case for empty string.
        ((0L, 0), String.empty)
        ||> String.fold (fun (checksum, index) c ->
            (checksum + int64 index) * int64 c,
            index + 1)
        |> fst  // Discard the index
        |> should equal 0L

        // Test case for "normal" usage of this function.
        ((0L, 0), "The quick brown fox jumps over the lazy dog.")
        ||> String.fold (fun (checksum, index) c ->
            (checksum + int64 index) * int64 c,
            index + 1)
        |> fst  // Discard the index
        |> should equal 1464868296444951516L

    [<TestCase>]
    let foldBack () : unit =
        // Test case for empty string.
        (String.empty, (0L, 0))
        ||> String.foldBack (fun c (checksum, index) ->
            (checksum + int64 index) * int64 c,
            index + 1)
        |> fst  // Discard the index
        |> should equal 0L

        // Test case for "normal" usage of this function.
        ("The quick brown fox jumps over the lazy dog.", (0L, 0))
        ||> String.foldBack (fun c (checksum, index) ->
            (checksum + int64 index) * int64 c,
            index + 1)
        |> fst  // Discard the index
        |> should equal 6529802340200151804L

    [<TestCase>]
    let iter () : unit =
        // Test case for empty substring.
        do
            let elements = ResizeArray ()

            String.empty
            |> String.iter (System.Char.ToUpper >> elements.Add)

            elements.ToArray ()
            |> Array.isEmpty
            |> should be True

        // Test case for "normal" usage of this function.
        do
            let elements = ResizeArray ()

            "The quick brown fox jumps over the lazy dog."
            |> String.iter (System.Char.ToUpper >> elements.Add)

            elements.ToArray ()
            |> should equal
                [| 'T'; 'H'; 'E'; ' '; 'Q'; 'U'; 'I'; 'C'; 'K'; ' '; 'B'; 'R'; 'O'; 'W'; 'N';
                   ' '; 'F'; 'O'; 'X'; ' '; 'J'; 'U'; 'M'; 'P'; 'S'; ' '; 'O'; 'V'; 'E'; 'R';
                   ' '; 'T'; 'H'; 'E'; ' '; 'L'; 'A'; 'Z'; 'Y'; ' '; 'D'; 'O'; 'G'; '.'; |]

    [<TestCase>]
    let iteri () : unit =
        // Test case for empty substring.
        do
            let elements = ResizeArray ()

            String.empty
            |> String.iteri (fun idx c ->
                elements.Add (
                    if idx % 2 = 0 then System.Char.ToUpper c else c))

            elements.ToArray ()
            |> Array.isEmpty
            |> should be True

        // Test case for "normal" usage of this function.
        do
            let elements = ResizeArray ()

            "The quick brown fox jumps over the lazy dog."
            |> String.iteri (fun idx c ->
                elements.Add (
                    if idx % 2 = 0 then System.Char.ToUpper c else c))

            elements.ToArray ()
            |> should equal
                [| 'T'; 'h'; 'E'; ' '; 'Q'; 'u'; 'I'; 'c'; 'K'; ' '; 'B'; 'r'; 'O'; 'w'; 'N';
                   ' '; 'F'; 'o'; 'X'; ' '; 'J'; 'u'; 'M'; 'p'; 'S'; ' '; 'O'; 'v'; 'E'; 'r';
                   ' '; 't'; 'H'; 'e'; ' '; 'l'; 'A'; 'z'; 'Y'; ' '; 'D'; 'o'; 'G'; '.'; |]

    [<TestCase>]
    let map () : unit =
        // Test case for empty string.
        String.empty
        |> String.map System.Char.ToUpper
        |> should equal String.empty

        // Test case for "normal" usage of this function.
        "The quick brown fox jumps over the lazy dog."
        |> String.map System.Char.ToUpper
        |> should equal "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG."

    [<TestCase>]
    let mapi () : unit =
        // Test case for empty string.
        String.empty
        |> String.mapi (fun idx c ->
            if idx % 2 = 0 then System.Char.ToUpper c else c)
        |> should equal String.empty

        // Test case for "normal" usage of this function.
        "The quick brown fox jumps over the lazy dog."
        |> String.mapi (fun idx c ->
            if idx % 2 = 0 then System.Char.ToUpper c else c)
        |> should equal "ThE QuIcK BrOwN FoX JuMpS OvEr tHe lAzY DoG."

    [<TestCase>]
    let choose () : unit =
        // Test case for empty string.
        String.empty
        |> String.choose (fun c ->
            if int c % 2 = 0 then
                Some (System.Char.ToUpper c)
            else None)
        |> should equal String.empty

        // Test case for "normal" usage of this function.
        "The quick brown fox jumps over the lazy dog."
        |> String.choose (fun c ->
            if int c % 2 = 0 then
                Some (System.Char.ToUpper c)
            else None)
        |> should equal "TH  BRN FX JP VR TH LZ D."

    [<TestCase>]
    let choosei () : unit =
        // Test case for empty string.
        String.empty
        |> String.choosei (fun idx c ->
            if (idx + int c) % 2 = 0 then
                Some (System.Char.ToUpper c)
            else None)
        |> should equal String.empty

        // Test case for "normal" usage of this function.
        "The quick brown fox jumps over the lazy dog."
        |> String.choosei (fun idx c ->
            if (idx + int c) % 2 = 0 then
                Some (System.Char.ToUpper c)
            else None)
        |> should equal "TUCBWNFOXJU HE DO"

    [<TestCase>]
    let foldPairwise () : unit =
        // Count the number of occurrences where adjacent characters are the same.
        (0, "mississippi")
        ||> String.foldPairwise (fun count x y ->
            if x = y then count + 1 else count)
        |> should equal 3

    [<TestCase>]
    let trimChars () : unit =
        // Test case for empty string.
        String.empty
        |> String.trim [| ' '; '.'; ','; |]
        |> String.isEmpty
        |> should be True

        // Test case for a string which DOES NOT start or end with any of the specified characters.
        "Hello World!"
        |> String.trim [| ' '; '.'; ','; |]
        |> should equal "Hello World!"

        // Test case for a string which DOES start AND end with one or more of the specified characters.
        "    ,, ,Hello World!"
        |> String.trim [| ' '; '.'; ','; |]
        |> should equal "Hello World!"

    [<TestCase>]
    let trimStart () : unit =
        // Test case for empty string.
        String.empty
        |> String.trimStart [| ' '; '.'; ','; |]
        |> should equal String.empty

        // Test case for a string which DOES NOT start with any of the specified characters.
        "The quick brown fox jumps over the lazy dog."
        |> String.trimStart [| ' '; '.'; ','; |]
        |> should equal "The quick brown fox jumps over the lazy dog."

        // Test case for a string which DOES start with one or more of the specified characters.
        "    ,, ,Hello World!"
        |> String.trimStart [| ' '; '.'; ','; |]
        |> should equal "Hello World!"

    [<TestCase>]
    let trimEnd () : unit =
        // Test case for empty string.
        String.empty
        |> String.trimEnd [| ' '; '.'; ','; |]
        |> should equal String.empty

        // Test case for when the string DOES NOT end with any of the specified characters.
        "The quick brown fox jumps over the lazy dog."
        |> String.trimEnd [| '!'; '?'; '*'; |]
        |> should equal "The quick brown fox jumps over the lazy dog."

        // Test case for when the string DOES end with one or more of the specified characters.
        "Hello World!1!?!!"
        |> String.trimEnd [| '!'; '?'; '*'; |]
        |> should equal "Hello World!1"

    [<TestCase>]
    let trimStartWith () : unit =
        // Test case for empty string.
        String.empty
        |> String.trimStartWith (not << System.Char.IsWhiteSpace)
        |> String.isEmpty
        |> should be True

        // Test case for non-empty string with leading characters which
        // don't match the predicate (i.e., they'll be filtered out of the string).
        "    ,, ,Hello World!"
        |> String.trimStartWith (not << System.Char.IsWhiteSpace)
        |> should equal ",, ,Hello World!"

    [<TestCase>]
    let trimEndWith () : unit =
        // Test case for empty string.
        String.empty
        |> String.trimEndWith (not << System.Char.IsPunctuation)
        |> String.isEmpty
        |> should be True

        // Test case for non-empty string with trailing characters which
        // don't match the predicate (i.e., they'll be filtered out of the string).
        "Hello World!1!?!!"
        |> String.trimEndWith (not << System.Char.IsPunctuation)
        |> should equal "Hello World!1"

    [<TestCase>]
    let trimWith () : unit =
        // Test case for empty string.
        String.empty
        |> String.trimWith System.Char.IsLetterOrDigit
        |> String.isEmpty
        |> should be True

        // Test case for non-empty string with leading AND trailing characters which
        // don't match the predicate (i.e., they'll be filtered out of the string).
        "    ,, ,Hello World!1!?!!"
        |> String.trimWith System.Char.IsLetterOrDigit
        |> should equal "Hello World!1"

    module Split =
        open System

        [<TestCase>]
        let iter () : unit =
            do
                // Test case for the empty string.
                let elements = ResizeArray ()

                String.empty
                |> String.Split.iter
                    ([| ','; |], StringSplitOptions.None)
                    (Substring.length >> elements.Add)

                ResizeArray.isEmpty elements
                |> should be True

            do
                // Test cases for a string which does contain the specified characters.
                let elements = ResizeArray ()

                "Id,Title,First,MI,Last,DOB"
                |> String.Split.iter ([| ','; |], StringSplitOptions.None)
                    (Substring.length >> elements.Add)

                ResizeArray.toArray elements
                |> should equal
                    [| 2; 5; 5; 2; 4; 3; |]

                // Re-test using the option to remove empty strings.
                let elements = ResizeArray ()
                
                "Id,Title,First,MI,Last,DOB"
                |> String.Split.iter ([| ','; |], StringSplitOptions.RemoveEmptyEntries)
                    (Substring.length >> elements.Add)

                ResizeArray.toArray elements
                |> should equal
                    [| 2; 5; 5; 2; 4; 3; |]

            do
                // Test case for a string which does contain the specified characters,
                // and where there are adjacent occurrences of the characters.
                let elements = ResizeArray ()

                "3262,,John,Q,Doe,1970-Jan-01"
                |> String.Split.iter ([| ','; |], StringSplitOptions.None)
                    (Substring.length >> elements.Add)

                ResizeArray.toArray elements
                |> should equal
                    [| 4; 0; 4; 1; 3; 11; |]

                // Re-test using the option to remove empty strings.
                let elements = ResizeArray ()
                
                "3262,,John,Q,Doe,1970-Jan-01"
                |> String.Split.iter ([| ','; |], StringSplitOptions.RemoveEmptyEntries)
                    (Substring.length >> elements.Add)

                ResizeArray.toArray elements
                |> should equal
                    [| 4; 4; 1; 3; 11; |]

        [<TestCase>]
        let iteri () : unit =
            do
                // Test case for the empty string.
                let elements = ResizeArray ()

                String.empty
                |> String.Split.iteri ([| ','; |], StringSplitOptions.None)
                    (fun index substr ->
                        index + Substring.length substr
                        |> elements.Add)

                ResizeArray.isEmpty elements
                |> should be True

            do
                // Test cases for a string which does contain the specified characters.
                let elements = ResizeArray ()

                "Id,Title,First,MI,Last,DOB"
                |> String.Split.iteri ([| ','; |], StringSplitOptions.None)
                    (fun index substr ->
                        index + Substring.length substr
                        |> elements.Add)

                ResizeArray.toArray elements
                |> should equal
                    [| 2; 6; 7; 5; 8; 8; |]

                // Re-test using the option to remove empty strings.
                let elements = ResizeArray ()
                
                "Id,Title,First,MI,Last,DOB"
                |> String.Split.iteri ([| ','; |], StringSplitOptions.RemoveEmptyEntries)
                    (fun index substr ->
                        index + Substring.length substr
                        |> elements.Add)

                ResizeArray.toArray elements
                |> should equal
                    [| 2; 6; 7; 5; 8; 8; |]

            do
                // Test case for a string which does contain the specified characters,
                // and where there are adjacent occurrences of the characters.
                let elements = ResizeArray ()

                "3262,,John,Q,Doe,1970-Jan-01"
                |> String.Split.iteri ([| ','; |], StringSplitOptions.None)
                    (fun index substr ->
                        index + Substring.length substr
                        |> elements.Add)

                ResizeArray.toArray elements
                |> should equal
                    [| 4; 1; 6; 4; 7; 16; |]

                // Re-test using the option to remove empty strings.
                let elements = ResizeArray ()
                
                "3262,,John,Q,Doe,1970-Jan-01"
                |> String.Split.iteri ([| ','; |], StringSplitOptions.RemoveEmptyEntries)
                    (fun index substr ->
                        index + Substring.length substr
                        |> elements.Add)

                ResizeArray.toArray elements
                |> should equal
                    [| 4; 6; 4; 7; 16; |]

        [<TestCase>]
        let fold () : unit =
            do
                // Test case for the empty string.
                (String.empty, String.empty)
                ||> String.Split.fold
                    ([| ','; |], StringSplitOptions.None)
                    (fun state substr ->
                        state + ExtCore.Substring.toString substr)
                |> String.isEmpty
                |> should be True

            do
                // Test cases for a string which does contain the specified characters.
                (String.empty, "Id,Title,First,MI,Last,DOB")
                ||> String.Split.fold ([| ','; |], StringSplitOptions.None)
                    (fun state substr ->
                        state + ExtCore.Substring.toString substr)
                |> should equal "IdTitleFirstMILastDOB"

                // Re-test using the option to remove empty strings.
                (String.empty, "Id,Title,First,MI,Last,DOB")
                ||> String.Split.fold ([| ','; |], StringSplitOptions.RemoveEmptyEntries)
                    (fun state substr ->
                        state + ExtCore.Substring.toString substr)
                |> should equal "IdTitleFirstMILastDOB"

            do
                // Test case for a string which does contain the specified characters,
                // and where there are adjacent occurrences of the characters.
                (String.empty, "3262,,John,Q,Doe,1970-Jan-01")
                ||> String.Split.fold ([| ','; |], StringSplitOptions.None)
                    (fun state substr ->
                        state + (
                            if ExtCore.Substring.isEmpty substr then "(NULL)"
                            else ExtCore.Substring.toString substr))
                |> should equal "3262(NULL)JohnQDoe1970-Jan-01"

                // Re-test using the option to remove empty strings.
                (String.empty, "3262,,John,Q,Doe,1970-Jan-01")
                ||> String.Split.fold ([| ','; |], StringSplitOptions.RemoveEmptyEntries)
                    (fun state substr ->
                        state + (
                            if ExtCore.Substring.isEmpty substr then "(NULL)"
                            else ExtCore.Substring.toString substr))
                |> should equal "3262JohnQDoe1970-Jan-01"

        [<TestCase>]
        let foldi () : unit =
            do
                // Test case for the empty string.
                (String.empty, String.empty)
                ||> String.Split.foldi
                    ([| ','; |], StringSplitOptions.None)
                    (fun state index substr ->
                        state + ExtCore.Substring.toString substr)
                |> String.isEmpty
                |> should be True

            do
                // Test cases for a string which does contain the specified characters.
                (String.empty, "Id,Title,First,MI,Last,DOB")
                ||> String.Split.foldi ([| ','; |], StringSplitOptions.None)
                    (fun state index substr ->
                        state + (
                            index.ToString() + ExtCore.Substring.toString substr))
                |> should equal "0Id1Title2First3MI4Last5DOB"

                // Re-test using the option to remove empty strings.
                (String.empty, "Id,Title,First,MI,Last,DOB")
                ||> String.Split.foldi ([| ','; |], StringSplitOptions.RemoveEmptyEntries)
                    (fun state index substr ->
                        state + (
                            index.ToString() + ExtCore.Substring.toString substr))
                |> should equal "0Id1Title2First3MI4Last5DOB"

            do
                // Test case for a string which does contain the specified characters,
                // and where there are adjacent occurrences of the characters.
                (String.empty, "3262,,John,Q,Doe,1970-Jan-01")
                ||> String.Split.foldi ([| ','; |], StringSplitOptions.None)
                    (fun state index substr ->
                        state + (
                            index.ToString() + (
                                if ExtCore.Substring.isEmpty substr then "(NULL)"
                                else ExtCore.Substring.toString substr)))
                |> should equal "032621(NULL)2John3Q4Doe51970-Jan-01"

                // Re-test using the option to remove empty strings.
                (String.empty, "3262,,John,Q,Doe,1970-Jan-01")
                ||> String.Split.foldi ([| ','; |], StringSplitOptions.RemoveEmptyEntries)
                    (fun state index substr ->
                        state + (
                            index.ToString() + (
                                if ExtCore.Substring.isEmpty substr then "(NULL)"
                                else ExtCore.Substring.toString substr)))
                |> should equal "032622John3Q4Doe51970-Jan-01"

