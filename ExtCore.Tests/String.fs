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

    [<Test>]
    let get () : unit =
        do
            let substr = substring ("Hello World!", 3, 5)
            
            Substring.get substr 1
            |> assertEqual 'o'

            Substring.get substr 3
            |> assertEqual 'W'

    [<Test>]
    let isEmpty () : unit =
        substring ("Hello World!", 4, 0)
        |> Substring.isEmpty
        |> should be True

        substring ("Hello World!", 3, 4)
        |> Substring.isEmpty
        |> should be False

    [<Test>]
    let ofString () : unit =
        do
            let substr = Substring.ofString ""

            substr
            |> Substring.string
            |> assertEqual ""

            substr
            |> Substring.offset
            |> assertEqual 0

            substr
            |> Substring.length
            |> assertEqual 0

        do
            let substr = Substring.ofString "Hello World!"

            substr
            |> Substring.string
            |> assertEqual "Hello World!"

            substr
            |> Substring.offset
            |> assertEqual 0

            substr
            |> Substring.length
            |> assertEqual 12

    [<Test>]
    let toString () : unit =
        // Test for empty substring.
        substring ("Hello World!", 3, 0)
        |> Substring.toString
        |> assertEqual String.empty

        substring ("Hello World!", 3, 6)
        |> Substring.toString
        |> assertEqual "lo Wor"

    [<Test>]
    let toArray () : unit =
        // Test for empty substring.
        substring ("Hello World!", 3, 0)
        |> Substring.toArray
        |> assertEqual Array.empty

        substring ("Hello World!", 3, 6)
        |> Substring.toArray
        |> assertEqual [| 'l'; 'o'; ' '; 'W'; 'o'; 'r'; |]

    [<Test>]
    let sub () : unit =
        do
            let str = "The quick brown fox jumps over the lazy dog."

            let substr = substring (str, 4, 15)  // "quick brown fox"
            
            Substring.sub substr 6 5
            |> Substring.toString
            |> assertEqual "brown"

    [<Test>]
    let concat () : unit =
        do
            let str1 = "The quick brown fox jumps over the lazy dog."
            let str2 = "Hello World!"

            seq {
            yield substring (str1, 0, 35)
            yield substring (str2, 6, 6)
            yield substring (str1, 3, 1)
            yield substring (str2, 0, 6)
            yield substring (str1, 35, 9) }
            |> Substring.concat
            |> assertEqual "The quick brown fox jumps over the World! Hello lazy dog."

    [<Test>]
    let iter () : unit =
        // Test case for empty substring.
        do
            let elements = ResizeArray ()

            substring ("The quick brown fox jumps over the lazy dog.", 4, 0)
            |> Substring.iter (System.Char.ToUpper >> elements.Add)

            elements.ToArray ()
            |> Array.isEmpty
            |> should be True

        // Test case for "normal" usage of this function.
        do
            let elements = ResizeArray ()

            substring ("The quick brown fox jumps over the lazy dog.", 4, 15)
            |> Substring.iter (System.Char.ToUpper >> elements.Add)

            elements.ToArray ()
            |> assertEqual
                [| 'Q'; 'U'; 'I'; 'C'; 'K'; ' '; 'B'; 'R'; 'O'; 'W'; 'N'; ' '; 'F'; 'O'; 'X'; |]

    [<Test>]
    let iteri () : unit =
        // Test case for empty substring.
        do
            let elements = ResizeArray ()

            substring ("The quick brown fox jumps over the lazy dog.", 4, 0)
            |> Substring.iteri (fun idx c ->
                elements.Add (
                    if idx % 2 = 0 then System.Char.ToUpper c else c))

            elements.ToArray ()
            |> Array.isEmpty
            |> should be True

        // Test case for "normal" usage of this function.
        do
            let elements = ResizeArray ()

            substring ("The quick brown fox jumps over the lazy dog.", 4, 15)
            |> Substring.iteri (fun idx c ->
                elements.Add (
                    if idx % 2 = 0 then System.Char.ToUpper c else c))

            elements.ToArray ()
            |> assertEqual
                [| 'Q'; 'u'; 'I'; 'c'; 'K'; ' '; 'B'; 'r'; 'O'; 'w'; 'N'; ' '; 'F'; 'o'; 'X'; |]

    [<Test>]
    let iterBack () : unit =
        // Test case for empty substring.
        do
            let elements = ResizeArray ()

            substring ("The quick brown fox jumps over the lazy dog.", 4, 0)
            |> Substring.iterBack (System.Char.ToUpper >> elements.Add)

            elements.ToArray ()
            |> Array.isEmpty
            |> should be True

        // Test case for "normal" usage of this function.
        do
            let elements = ResizeArray ()

            substring ("The quick brown fox jumps over the lazy dog.", 4, 15)
            |> Substring.iterBack (System.Char.ToUpper >> elements.Add)

            elements.ToArray ()
            |> assertEqual
                [| 'X'; 'O'; 'F'; ' '; 'N'; 'W'; 'O'; 'R'; 'B'; ' '; 'K'; 'C'; 'I'; 'U'; 'Q'; |]

    [<Test>]
    let fold () : unit =
        // Test case for empty substring.
        do
            ((0L, 0), substring ("The quick brown fox jumps over the lazy dog.", 4, 0))
            ||> Substring.fold (fun (checksum, index) c ->
                (checksum + int64 index) * int64 c,
                index + 1)
            |> fst  // Discard the index
            |> assertEqual 0L

        // Test case for "normal" usage of this function.
        do
            ((0L, 0), substring ("The quick brown fox jumps over the lazy dog.", 4, 15))
            ||> Substring.fold (fun (checksum, index) c ->
                (checksum + int64 index) * int64 c,
                index + 1)
            |> fst  // Discard the index
            |> assertEqual 8117010307721961272L

    [<Test>]
    let foldi () : unit =
        // Test case for empty substring.
        do
            (0L, substring ("The quick brown fox jumps over the lazy dog.", 4, 0))
            ||> Substring.foldi (fun checksum index c ->
                (checksum + int64 index) * int64 c)
            |> assertEqual 0L

        // Test case for "normal" usage of this function.
        do
            (0L, substring ("The quick brown fox jumps over the lazy dog.", 4, 15))
            ||> Substring.foldi (fun checksum index c ->
                (checksum + int64 index) * int64 c)
            |> assertEqual 8117010307721961272L

    [<Test>]
    let foldBack () : unit =
        // Test case for empty substring.
        do
            (substring ("The quick brown fox jumps over the lazy dog.", 4, 0), (0L, 0))
            ||> Substring.foldBack (fun c (checksum, index) ->
                (checksum + int64 index) * int64 c,
                index + 1)
            |> fst  // Discard the index
            |> assertEqual 0L

        // Test case for "normal" usage of this function.
        do
            (substring ("The quick brown fox jumps over the lazy dog.", 4, 15), (0L, 0))
            ||> Substring.foldBack (fun c (checksum, index) ->
                (checksum + int64 index) * int64 c,
                index + 1)
            |> fst  // Discard the index
            |> assertEqual -8792059055315210054L


/// Tests for the ExtCore.String module.
module String =
    [<Test>]
    let isEmpty () : unit =
        // Test case for empty string.
        String.isEmpty String.empty
        |> should be True

        // Test case for any other string.
        "The quick brown fox jumps over the lazy dog."
        |> String.isEmpty
        |> should be False

    [<Test>]
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
        |> assertEqual "Hello World!"

    [<Test>]
    let ofArray () : unit =
        // Test case for empty array.
        Array.empty
        |> String.ofArray
        |> String.isEmpty
        |> should be True

        // Test case for a non-empty array.
        [| 'H'; 'e'; 'l'; 'l'; 'o'; ' '; 'W'; 'o'; 'r'; 'l'; 'd'; '!'; |]
        |> String.ofArray
        |> assertEqual "Hello World!"

    [<Test>]
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
        |> assertEqual (Some "Hello World!")

    [<Test>]
    let toArray () : unit =
        // Test case for an empty string.
        String.empty
        |> String.toArray
        |> Array.isEmpty
        |> should be True

        // Test case for a non-empty string.
        "Hello World!"
        |> String.toArray
        |> assertEqual
            [| 'H'; 'e'; 'l'; 'l'; 'o'; ' '; 'W'; 'o'; 'r'; 'l'; 'd'; '!'; |]

    [<Test>]
    let split () : unit =
        // Test case for an empty string.
        String.empty
        |> String.split [| ','; |]
        |> Array.isEmpty
        |> should be True

        // Test case for a non-empty string which does not contain the specified characters.
        "Hello World!"
        |> String.split [| ','; |]
        |> assertEqual [| "Hello World!"; |]

        // Test case for a string which does contain the specified characters.
        "Id,Title,First,MI,Last,DOB"
        |> String.split [| ','; |]
        |> assertEqual
            [| "Id"; "Title"; "First"; "MI"; "Last"; "DOB"; |]

        // Test case for a string which does contain the specified characters,
        // and where there are adjacent occurrences of the characters.
        "3262,,John,Q,Doe,1970-Jan-01"
        |> String.split [| ','; |]
        |> assertEqual
            [| "3262"; ""; "John"; "Q"; "Doe"; "1970-Jan-01"; |]

    [<Test>]
    let concatArray () : unit =
        // Test case for empty array.
        Array.empty
        |> String.concatArray
        |> String.isEmpty
        |> should be True

        // Test case for non-empty array.
        [| "The"; "quick"; "brown"; "fox"; "jumps"; "over"; "the"; "lazy"; "dog."; |]
        |> String.concatArray
        |> assertEqual
            "Thequickbrownfoxjumpsoverthelazydog."

    [<Test>]
    let ofLines () : unit =
        // Test case for empty array.
        Array.empty
        |> String.ofLines
        |> String.isEmpty
        |> should be True

        // Sample usage test case.
        [| "Lorem"; "ipsum"; "dolor"; "sit"; "amet"; |]
        |> String.ofLines
        |> assertEqual @"Lorem
ipsum
dolor
sit
amet"

    [<Test>]
    let toLines () : unit =
        // Test case for empty string.
        String.empty
        |> String.toLines
        |> Array.isEmpty
        |> should be True

        // Test case for a single-line string.
        "The quick brown fox jumps over the lazy dog."
        |> String.toLines
        |> assertEqual
            [| "The quick brown fox jumps over the lazy dog."; |]

        // Test case for a multi-line string.
        @"Lorem
ipsum
dolor
sit
amet"
        |> String.toLines
        |> assertEqual
            [| "Lorem"; "ipsum"; "dolor"; "sit"; "amet"; |]

    [<Test>]
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
        |> assertEqual "quick brown fox"

    [<Test>]
    let tryFindIndexOf () : unit =
        // Test case for empty string.
        String.empty
        |> String.tryFindIndexOf 'j'
        |> assertEqual None

        // Test case for a string which does not contain the specified character.
        "Hello World!"
        |> String.tryFindIndexOf 'j'
        |> assertEqual None

        // Test case for a string which contains exactly one (1) instance of the specified character.
        "The quick brown fox jumps over the lazy dog."
        |> String.tryFindIndexOf 'j'
        |> assertEqual (Some 20)

        // Test case for a string which contains multiple instances of the specified character.
        "Hello World!"
        |> String.tryFindIndexOf 'l'
        |> assertEqual (Some 2)

    [<Test>]
    let findIndexOf () : unit =
        // Test case for a string which contains exactly one (1) instance of the specified character.
        "The quick brown fox jumps over the lazy dog."
        |> String.findIndexOf 'j'
        |> assertEqual 20

        // Test case for a string which contains multiple instances of the specified character.
        "Hello World!"
        |> String.findIndexOf 'l'
        |> assertEqual 2

    [<Test; ExpectedException(typeof<System.Collections.Generic.KeyNotFoundException>)>]
    let ``findIndexOf raises exn when the string does not contain the character`` () : unit =
        // Test case for a string which does not contain the specified character.
        "Hello World!"
        |> String.findIndexOf 'j'
        |> ignore

    [<Test>]
    let tryFindIndex () : unit =
        // Test case for empty string.
        String.empty
        |> String.tryFindIndex (fun c ->
            // Is this a vowel (in English)?
            let c = System.Char.ToLowerInvariant c
            c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u')
        |> assertEqual None

        // Test case for a non-empty string which does not contain a matching character.
        "Glyndyfrdwy"
        |> String.tryFindIndex (fun c ->
            // Is this a vowel (in English)?
            let c = System.Char.ToLowerInvariant c
            c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u')
        |> assertEqual None

        // Test case for a string which contains exactly one (1) matching character.
        "Rhythmist"
        |> String.tryFindIndex (fun c ->
            // Is this a vowel (in English)?
            let c = System.Char.ToLowerInvariant c
            c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u')
        |> assertEqual (Some 6)

        // Test case for a string which contains multiple matching characters.
        "Hello World!"
        |> String.tryFindIndex (fun c ->
            // Is this a vowel (in English)?
            let c = System.Char.ToLowerInvariant c
            c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u')
        |> assertEqual (Some 1)

    [<Test>]
    let findIndex () : unit =
        // Test case for a string which contains exactly one (1) matching character.
        "Rhythmist"
        |> String.findIndex (fun c ->
            // Is this a vowel (in English)?
            let c = System.Char.ToLowerInvariant c
            c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u')
        |> assertEqual 6

        // Test case for a string which contains multiple matching characters.
        "Hello World!"
        |> String.findIndex (fun c ->
            // Is this a vowel (in English)?
            let c = System.Char.ToLowerInvariant c
            c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u')
        |> assertEqual 1

    [<Test; ExpectedException(typeof<System.Collections.Generic.KeyNotFoundException>)>]
    let ``findIndex raises exn when the string does not contain a matching character`` () : unit =
        // Test case for a non-empty string which does not contain a matching character.
        "Glyndyfrdwy"
        |> String.findIndex (fun c ->
            // Is this a vowel (in English)?
            let c = System.Char.ToLowerInvariant c
            c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u')
        |> ignore

    [<Test>]
    let fold () : unit =
        // Test case for empty string.
        ((0L, 0), String.empty)
        ||> String.fold (fun (checksum, index) c ->
            (checksum + int64 index) * int64 c,
            index + 1)
        |> fst  // Discard the index
        |> assertEqual 0L

        // Test case for "normal" usage of this function.
        ((0L, 0), "The quick brown fox jumps over the lazy dog.")
        ||> String.fold (fun (checksum, index) c ->
            (checksum + int64 index) * int64 c,
            index + 1)
        |> fst  // Discard the index
        |> assertEqual 1464868296444951516L

    [<Test>]
    let foldBack () : unit =
        // Test case for empty string.
        (String.empty, (0L, 0))
        ||> String.foldBack (fun c (checksum, index) ->
            (checksum + int64 index) * int64 c,
            index + 1)
        |> fst  // Discard the index
        |> assertEqual 0L

        // Test case for "normal" usage of this function.
        ("The quick brown fox jumps over the lazy dog.", (0L, 0))
        ||> String.foldBack (fun c (checksum, index) ->
            (checksum + int64 index) * int64 c,
            index + 1)
        |> fst  // Discard the index
        |> assertEqual 6529802340200151804L

    [<Test>]
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
            |> assertEqual
                [| 'T'; 'H'; 'E'; ' '; 'Q'; 'U'; 'I'; 'C'; 'K'; ' '; 'B'; 'R'; 'O'; 'W'; 'N';
                   ' '; 'F'; 'O'; 'X'; ' '; 'J'; 'U'; 'M'; 'P'; 'S'; ' '; 'O'; 'V'; 'E'; 'R';
                   ' '; 'T'; 'H'; 'E'; ' '; 'L'; 'A'; 'Z'; 'Y'; ' '; 'D'; 'O'; 'G'; '.'; |]

    [<Test>]
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
            |> assertEqual
                [| 'T'; 'h'; 'E'; ' '; 'Q'; 'u'; 'I'; 'c'; 'K'; ' '; 'B'; 'r'; 'O'; 'w'; 'N';
                   ' '; 'F'; 'o'; 'X'; ' '; 'J'; 'u'; 'M'; 'p'; 'S'; ' '; 'O'; 'v'; 'E'; 'r';
                   ' '; 't'; 'H'; 'e'; ' '; 'l'; 'A'; 'z'; 'Y'; ' '; 'D'; 'o'; 'G'; '.'; |]

    [<Test>]
    let iter2 () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let iteri2 () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let map () : unit =
        // Test case for empty string.
        String.empty
        |> String.map System.Char.ToUpper
        |> assertEqual String.empty

        // Test case for "normal" usage of this function.
        "The quick brown fox jumps over the lazy dog."
        |> String.map System.Char.ToUpper
        |> assertEqual "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG."

    [<Test>]
    let mapi () : unit =
        // Test case for empty string.
        String.empty
        |> String.mapi (fun idx c ->
            if idx % 2 = 0 then System.Char.ToUpper c else c)
        |> assertEqual String.empty

        // Test case for "normal" usage of this function.
        "The quick brown fox jumps over the lazy dog."
        |> String.mapi (fun idx c ->
            if idx % 2 = 0 then System.Char.ToUpper c else c)
        |> assertEqual "ThE QuIcK BrOwN FoX JuMpS OvEr tHe lAzY DoG."

    [<Test>]
    let choose () : unit =
        // Test case for empty string.
        String.empty
        |> String.choose (fun c ->
            if int c % 2 = 0 then
                Some (System.Char.ToUpper c)
            else None)
        |> assertEqual String.empty

        // Test case for "normal" usage of this function.
        "The quick brown fox jumps over the lazy dog."
        |> String.choose (fun c ->
            if int c % 2 = 0 then
                Some (System.Char.ToUpper c)
            else None)
        |> assertEqual "TH  BRN FX JP VR TH LZ D."

    [<Test>]
    let choosei () : unit =
        // Test case for empty string.
        String.empty
        |> String.choosei (fun idx c ->
            if (idx + int c) % 2 = 0 then
                Some (System.Char.ToUpper c)
            else None)
        |> assertEqual String.empty

        // Test case for "normal" usage of this function.
        "The quick brown fox jumps over the lazy dog."
        |> String.choosei (fun idx c ->
            if (idx + int c) % 2 = 0 then
                Some (System.Char.ToUpper c)
            else None)
        |> assertEqual "TUCBWNFOXJU HE DO"

    [<Test>]
    let foldPairwise () : unit =
        // Count the number of occurrences where adjacent characters are the same.
        (0, "mississippi")
        ||> String.foldPairwise (fun count x y ->
            if x = y then count + 1 else count)
        |> assertEqual 3

    [<Test>]
    let trimChars () : unit =
        // Test case for empty string.
        String.empty
        |> String.trim [| ' '; '.'; ','; |]
        |> String.isEmpty
        |> should be True

        // Test case for a string which DOES NOT start or end with any of the specified characters.
        "Hello World!"
        |> String.trim [| ' '; '.'; ','; |]
        |> assertEqual "Hello World!"

        // Test case for a string which DOES start AND end with one or more of the specified characters.
        "    ,, ,Hello World!"
        |> String.trim [| ' '; '.'; ','; |]
        |> assertEqual "Hello World!"

    [<Test>]
    let trimStart () : unit =
        // Test case for empty string.
        String.empty
        |> String.trimStart [| ' '; '.'; ','; |]
        |> assertEqual String.empty

        // Test case for a string which DOES NOT start with any of the specified characters.
        "The quick brown fox jumps over the lazy dog."
        |> String.trimStart [| ' '; '.'; ','; |]
        |> assertEqual "The quick brown fox jumps over the lazy dog."

        // Test case for a string which DOES start with one or more of the specified characters.
        "    ,, ,Hello World!"
        |> String.trimStart [| ' '; '.'; ','; |]
        |> assertEqual "Hello World!"

    [<Test>]
    let trimEnd () : unit =
        // Test case for empty string.
        String.empty
        |> String.trimEnd [| ' '; '.'; ','; |]
        |> assertEqual String.empty

        // Test case for when the string DOES NOT end with any of the specified characters.
        "The quick brown fox jumps over the lazy dog."
        |> String.trimEnd [| '!'; '?'; '*'; |]
        |> assertEqual "The quick brown fox jumps over the lazy dog."

        // Test case for when the string DOES end with one or more of the specified characters.
        "Hello World!1!?!!"
        |> String.trimEnd [| '!'; '?'; '*'; |]
        |> assertEqual "Hello World!1"

    [<Test>]
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
        |> assertEqual ",, ,Hello World!"

    [<Test>]
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
        |> assertEqual "Hello World!1"

    [<Test>]
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
        |> assertEqual "Hello World!1"

    [<Test>]
    let fold2 () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let foldBack2 () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let map2 () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let mapi2 () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let tryFind () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let find () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let tryPick () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let pick () : unit =
        Assert.Ignore "Test not yet implemented."


    module Split =
        open System

        [<Test>]
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
                |> assertEqual
                    [| 2; 5; 5; 2; 4; 3; |]

                // Re-test using the option to remove empty strings.
                let elements = ResizeArray ()
                
                "Id,Title,First,MI,Last,DOB"
                |> String.Split.iter ([| ','; |], StringSplitOptions.RemoveEmptyEntries)
                    (Substring.length >> elements.Add)

                ResizeArray.toArray elements
                |> assertEqual
                    [| 2; 5; 5; 2; 4; 3; |]

            do
                // Test case for a string which does contain the specified characters,
                // and where there are adjacent occurrences of the characters.
                let elements = ResizeArray ()

                "3262,,John,Q,Doe,1970-Jan-01"
                |> String.Split.iter ([| ','; |], StringSplitOptions.None)
                    (Substring.length >> elements.Add)

                ResizeArray.toArray elements
                |> assertEqual
                    [| 4; 0; 4; 1; 3; 11; |]

                // Re-test using the option to remove empty strings.
                let elements = ResizeArray ()
                
                "3262,,John,Q,Doe,1970-Jan-01"
                |> String.Split.iter ([| ','; |], StringSplitOptions.RemoveEmptyEntries)
                    (Substring.length >> elements.Add)

                ResizeArray.toArray elements
                |> assertEqual
                    [| 4; 4; 1; 3; 11; |]

        [<Test>]
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
                |> assertEqual
                    [| 2; 6; 7; 5; 8; 8; |]

                // Re-test using the option to remove empty strings.
                let elements = ResizeArray ()
                
                "Id,Title,First,MI,Last,DOB"
                |> String.Split.iteri ([| ','; |], StringSplitOptions.RemoveEmptyEntries)
                    (fun index substr ->
                        index + Substring.length substr
                        |> elements.Add)

                ResizeArray.toArray elements
                |> assertEqual
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
                |> assertEqual
                    [| 4; 1; 6; 4; 7; 16; |]

                // Re-test using the option to remove empty strings.
                let elements = ResizeArray ()
                
                "3262,,John,Q,Doe,1970-Jan-01"
                |> String.Split.iteri ([| ','; |], StringSplitOptions.RemoveEmptyEntries)
                    (fun index substr ->
                        index + Substring.length substr
                        |> elements.Add)

                ResizeArray.toArray elements
                |> assertEqual
                    [| 4; 6; 4; 7; 16; |]

        [<Test>]
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
                |> assertEqual "IdTitleFirstMILastDOB"

                // Re-test using the option to remove empty strings.
                (String.empty, "Id,Title,First,MI,Last,DOB")
                ||> String.Split.fold ([| ','; |], StringSplitOptions.RemoveEmptyEntries)
                    (fun state substr ->
                        state + ExtCore.Substring.toString substr)
                |> assertEqual "IdTitleFirstMILastDOB"

            do
                // Test case for a string which does contain the specified characters,
                // and where there are adjacent occurrences of the characters.
                (String.empty, "3262,,John,Q,Doe,1970-Jan-01")
                ||> String.Split.fold ([| ','; |], StringSplitOptions.None)
                    (fun state substr ->
                        state + (
                            if ExtCore.Substring.isEmpty substr then "(NULL)"
                            else ExtCore.Substring.toString substr))
                |> assertEqual "3262(NULL)JohnQDoe1970-Jan-01"

                // Re-test using the option to remove empty strings.
                (String.empty, "3262,,John,Q,Doe,1970-Jan-01")
                ||> String.Split.fold ([| ','; |], StringSplitOptions.RemoveEmptyEntries)
                    (fun state substr ->
                        state + (
                            if ExtCore.Substring.isEmpty substr then "(NULL)"
                            else ExtCore.Substring.toString substr))
                |> assertEqual "3262JohnQDoe1970-Jan-01"

        [<Test>]
        let foldi () : unit =
            do
                // Test case for the empty string.
                (String.empty, String.empty)
                ||> String.Split.foldi
                    ([| ','; |], StringSplitOptions.None)
                    (fun state index substr ->
                        state + (
                            index.ToString() + ExtCore.Substring.toString substr))
                |> String.isEmpty
                |> should be True

            do
                // Test cases for a string which does contain the specified characters.
                (String.empty, "Id,Title,First,MI,Last,DOB")
                ||> String.Split.foldi ([| ','; |], StringSplitOptions.None)
                    (fun state index substr ->
                        state + (
                            index.ToString() + ExtCore.Substring.toString substr))
                |> assertEqual "0Id1Title2First3MI4Last5DOB"

                // Re-test using the option to remove empty strings.
                (String.empty, "Id,Title,First,MI,Last,DOB")
                ||> String.Split.foldi ([| ','; |], StringSplitOptions.RemoveEmptyEntries)
                    (fun state index substr ->
                        state + (
                            index.ToString() + ExtCore.Substring.toString substr))
                |> assertEqual "0Id1Title2First3MI4Last5DOB"

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
                |> assertEqual "032621(NULL)2John3Q4Doe51970-Jan-01"

                // Re-test using the option to remove empty strings.
                (String.empty, "3262,,John,Q,Doe,1970-Jan-01")
                ||> String.Split.foldi ([| ','; |], StringSplitOptions.RemoveEmptyEntries)
                    (fun state index substr ->
                        state + (
                            index.ToString() + (
                                if ExtCore.Substring.isEmpty substr then "(NULL)"
                                else ExtCore.Substring.toString substr)))
                |> assertEqual "032622John3Q4Doe51970-Jan-01"

