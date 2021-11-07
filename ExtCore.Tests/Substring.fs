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


/// Tests for the properties/methods of the substring type.
module SubstringType =
    let [<Literal>] alphabet = "abcdefghijklmnopqrstuvwxyz"
    let [<Literal>] abcd = "abcababcdabcaba"

    [<Test>]
    let IsEmpty () =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let Item () =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    (* Contains empty string. *)
    [<TestCase("", "", ExpectedResult = true)>]
    [<TestCase(alphabet, "", ExpectedResult = true)>]

    (* Test single-character strings near edges of substring. *)
    [<TestCase(alphabet, "a", ExpectedResult = true)>]
    [<TestCase(alphabet, "b", ExpectedResult = true)>]
    [<TestCase(alphabet, "z", ExpectedResult = true)>]
    [<TestCase(alphabet, "Z", ExpectedResult = false)>]

    (* Test strings which occur zero or one time in a string. *)
    [<TestCase(alphabet, "abcd", ExpectedResult = true)>]
    [<TestCase(alphabet, "abce", ExpectedResult = false)>]
    [<TestCase(alphabet, "bcde", ExpectedResult = true)>]
    [<TestCase(alphabet, "bcdf", ExpectedResult = false)>]
    [<TestCase(alphabet, "wxyz", ExpectedResult = true)>]
    [<TestCase(alphabet, "txyz", ExpectedResult = false)>]
    [<TestCase(alphabet, "vwxy", ExpectedResult = true)>]
    [<TestCase(alphabet, "twxy", ExpectedResult = false)>]

    (* Test strings which occur zero or multiple times in a string. *)
    [<TestCase(abcd, "abc", ExpectedResult = true)>]
    [<TestCase(abcd, "abe", ExpectedResult = false)>]
    [<TestCase(abcd, "aba", ExpectedResult = true)>]
    [<TestCase(abcd, "bcd", ExpectedResult = true)>]
    [<TestCase(abcd, "bbb", ExpectedResult = false)>]

    (* Test substrings longer than the input string. *)
    [<TestCase("abcdef", "mnopqrs", ExpectedResult = false)>]
    [<TestCase("abcdef", "abcdefg", ExpectedResult = false)>]
    [<TestCase("abcdef", "bcdefgh", ExpectedResult = false)>]

    (* Test substrings with the same length as the input string. *)
    [<TestCase("abcdef", "abcdef", ExpectedResult = true)>]
    [<TestCase("abcdef", "abcabc", ExpectedResult = false)>]
    [<TestCase("abcdef", "mnopqr", ExpectedResult = false)>]
    let Contains_String (baseString : string, value : string) : bool =
        substring(baseString).Contains value

    [<Test>]
    (* Contains empty string. *)
//    [<TestCase("", "", ExpectedResult = true)>]
//    [<TestCase(alphabet, "", ExpectedResult = true)>]

    (* Test single-character strings near edges of substring. *)
//    [<TestCase(alphabet, "a", ExpectedResult = true)>]
//    [<TestCase(alphabet, "b", ExpectedResult = true)>]
//    [<TestCase(alphabet, "z", ExpectedResult = true)>]
//    [<TestCase(alphabet, "Z", ExpectedResult = false)>]

    (* Test strings which occur zero or one time in a string. *)
    [<TestCase(alphabet, 0, 26, alphabet, 0, 4, ExpectedResult = true)>]
    [<TestCase(alphabet, 0, 26, "abce", 0, 4, ExpectedResult = false)>]
    [<TestCase(alphabet, 0, 26, alphabet, 1, 20, ExpectedResult = true)>]
    [<TestCase(alphabet, 0, 26, "bcdf", 0, 4, ExpectedResult = false)>]
    [<TestCase(alphabet, 0, 26, alphabet, 22, 4, ExpectedResult = true)>]
    [<TestCase(alphabet, 0, 26, "txyz", 0, 4, ExpectedResult = false)>]
    [<TestCase(alphabet, 0, 26, alphabet, 20, 5, ExpectedResult = true)>]
    [<TestCase(alphabet, 0, 26, "twxy", 0, 4, ExpectedResult = false)>]
    [<TestCase(alphabet, 0, 26, "twxy", 1, 3, ExpectedResult = true)>]

    (* Test strings which occur zero or multiple times in a string. *)
//    [<TestCase(abcd, "abc", ExpectedResult = true)>]
//    [<TestCase(abcd, "abe", ExpectedResult = false)>]
//    [<TestCase(abcd, "aba", ExpectedResult = true)>]
//    [<TestCase(abcd, "bcd", ExpectedResult = true)>]
//    [<TestCase(abcd, "bbb", ExpectedResult = false)>]

    (* Test substrings longer than the input string. *)
//    [<TestCase("abcdef", "mnopqrs", ExpectedResult = false)>]
//    [<TestCase("abcdef", "abcdefg", ExpectedResult = false)>]
//    [<TestCase("abcdef", "bcdefgh", ExpectedResult = false)>]

    (* Test substrings with the same length as the input string. *)
//    [<TestCase("abcdef", "abcdef", ExpectedResult = true)>]
//    [<TestCase("abcdef", "abcabc", ExpectedResult = false)>]
//    [<TestCase("abcdef", "mnopqr", ExpectedResult = false)>]
    let Contains_Substring (baseString, baseOffset, baseLength, valueString, valueOffset, valueLength) : bool =
        substring(baseString, baseOffset, baseLength).Contains(substring(valueString, valueOffset, valueLength))

    [<Test>]
    let EndsWith_Substring () =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let EndsWith_String () =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let GetSlice () =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let IndexOf () =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let LastIndexOf () =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let StartsWith_Substring () =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let StartsWith_String () =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let ToCharArray () =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let ToString () =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let ``IEquatable<substring>.Equals`` () =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let ``IComparable.CompareTo`` () =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let ``IComparable<substring>.CompareTo`` () =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let ``IEnumerable.GetEnumerator`` () =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let ``IEnumerable<char>.GetEnumerator`` () =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Substring module.
module SubstringModule =
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
        |> assertTrue

        substring ("Hello World!", 3, 4)
        |> Substring.isEmpty
        |> assertFalse

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
        |> Collection.assertEqual Array.empty

        substring ("Hello World!", 3, 6)
        |> Substring.toArray
        |> Collection.assertEqual [| 'l'; 'o'; ' '; 'W'; 'o'; 'r'; |]

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
    let tryFindIndexOf () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let findIndexOf () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let tryFindIndex () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let findIndex () : unit =
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

    [<Test>]
    let iter () : unit =
        // Test case for empty substring.
        do
            let elements = ResizeArray ()

            substring ("The quick brown fox jumps over the lazy dog.", 4, 0)
            |> Substring.iter (System.Char.ToUpper >> elements.Add)

            elements.ToArray ()
            |> Array.isEmpty
            |> assertTrue

        // Test case for "normal" usage of this function.
        do
            let elements = ResizeArray ()

            substring ("The quick brown fox jumps over the lazy dog.", 4, 15)
            |> Substring.iter (System.Char.ToUpper >> elements.Add)

            elements.ToArray ()
            |> Collection.assertEqual
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
            |> assertTrue

        // Test case for "normal" usage of this function.
        do
            let elements = ResizeArray ()

            substring ("The quick brown fox jumps over the lazy dog.", 4, 15)
            |> Substring.iteri (fun idx c ->
                elements.Add (
                    if idx % 2 = 0 then System.Char.ToUpper c else c))

            elements.ToArray ()
            |> Collection.assertEqual
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
            |> assertTrue

        // Test case for "normal" usage of this function.
        do
            let elements = ResizeArray ()

            substring ("The quick brown fox jumps over the lazy dog.", 4, 15)
            |> Substring.iterBack (System.Char.ToUpper >> elements.Add)

            elements.ToArray ()
            |> Collection.assertEqual
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

    [<Test>]
    let trimStartWith () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let trimEndWith () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let trimWith () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let trimStart () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let trimEnd () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let trim () : unit =
        Assert.Ignore "Test not yet implemented."


    /// Unit tests for the Substring.Split module.
    module Split =
        [<Test>]
        let iter () : unit =
            Assert.Ignore "Test not yet implemented."

        [<Test>]
        let iteri () : unit =
            Assert.Ignore "Test not yet implemented."

        [<Test>]
        let fold () : unit =
            Assert.Ignore "Test not yet implemented."

        [<Test>]
        let foldi () : unit =
            Assert.Ignore "Test not yet implemented."


/// Unit tests for the extension methods in the SubstringExtensions module.
module SubstringExtensions =
    /// Unit tests for the substring extension methods for System.String.
    module String =
        [<Test>]
        let GetSlice () : unit =
            Assert.Ignore "Test not yet implemented."

    /// Unit tests for the substring extension methods for System.Text.StringBuilder.
    module StringBuilder =
        [<Test>]
        let Append () : unit =
            Assert.Ignore "Test not yet implemented."

        [<Test>]
        let AppendLine () : unit =
            Assert.Ignore "Test not yet implemented."

    /// Unit tests for the substring extension methods for System.Text.RegularExpressions.Regex.
    module Regex =
        [<Test>]
        let Match () : unit =
            Assert.Ignore "Test not yet implemented."


(* TODO :   Implement some randomized tests with FsCheck. *)

