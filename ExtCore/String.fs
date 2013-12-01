(*

Copyright 2010-2012 TidePowerd Ltd.
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

namespace ExtCore

open System
//open System.Diagnostics.Contracts


/// Additional functional operators on strings.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module String =
    open OptimizedClosures

    /// The empty string literal.
    [<CompiledName("Empty")>]
    let [<Literal>] empty = ""

    /// <summary>Gets a character from a string.</summary>
    /// <param name="str"></param>
    /// <param name="index"></param>
    /// <returns></returns>
    [<CompiledName("Get")>]
    let inline get (str : string) (index : int) =
        str.[index]

    /// <summary>Indicates whether the specified string is empty.</summary>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (str : string) =
        str.Length < 1

    /// <summary>Indicates whether the specified string is null or empty.</summary>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("IsNullOrEmpty")>]
    let inline isNullOrEmpty (str : string) =
        System.String.IsNullOrEmpty str

    /// <summary>
    /// Creates a string from an F# option value. If the option value is <c>None</c>, returns an empty string;
    /// returns <c>s</c> when the option value is <c>Some(s)</c>.
    /// </summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("OfOption")>]
    let inline ofOption value =
        match value with
        | None -> empty
        | Some str -> str

    /// <summary>Builds a string from the given character array.</summary>
    /// <param name="chars"></param>
    /// <returns></returns>
    [<CompiledName("OfArray")>]
    let inline ofArray (chars : char[]) =
        System.String (chars)

    /// <summary>
    /// Creates an F# option value from the specified string.
    /// If the string '<paramref name="s"/>' is null or empty, returns <c>None</c>; otherwise, returns <c>Some(s)</c>.
    /// </summary>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("ToOption")>]
    let inline toOption str =
        if System.String.IsNullOrEmpty str then None
        else Some str

    /// <summary>Builds a character array from the given string.</summary>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("ToArray")>]
    let inline toArray (str : string) =
        str.ToCharArray ()

    /// <summary>Returns a copy of the given string, converted to lowercase using the casing rules of the invariant culture.</summary>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("ToLowerInvariant")>]
    let inline toLower (str : string) : string =
        str.ToLowerInvariant ()

    /// <summary>Returns a copy of the given string, converted to lowercase using the casing rules of the invariant culture.</summary>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("ToUpperInvariant")>]
    let inline toUpper (str : string) : string =
        str.ToUpperInvariant ()

    /// <summary>Returns a new string created by instantiating the substring.</summary>
    /// <param name="substr"></param>
    /// <returns></returns>
    [<CompiledName("OfSubstring")>]
    let inline ofSubstring (substr : substring) : string =
        Substring.toString substr

    /// <summary>Returns a new substring spanning the specified string.</summary>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("ToSubstring")>]
    let inline toSubstring (str : string) : substring =
        Substring.ofString str

    /// <summary>Determines whether the beginning of a string matches the specified string.</summary>
    /// <param name="value"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("StartsWith")>]
    let inline startsWith (value : string) (str : string) : bool =
        str.StartsWith value

    /// <summary>Determines whether the end of a string matches the specified string.</summary>
    /// <param name="value"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("EndsWith")>]
    let inline endsWith (value : string) (str : string) : bool =
        str.EndsWith value

    /// <summary>
    /// Returns a new string in which all occurrences of a specified string within the given string are replaced with
    /// another specified string.
    /// </summary>
    /// <param name="oldValue"></param>
    /// <param name="newValue"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("Replace")>]
    let inline replace (oldValue : string) (newValue : string) (str : string) : string =
        str.Replace (oldValue, newValue)

    /// <summary>
    /// Returns a string array that contains the substrings in a string that are delimited by elements of the given Unicode character array.
    /// The returned array will be empty if and only if the input string is empty.
    /// </summary>
    /// <param name="chars"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("Split")>]
    let split (chars : char[]) (str : string) =
        if isEmpty str then Array.empty
        else str.Split chars

    /// <summary>
    /// Returns a string array that contains the substrings in a string that are delimited by elements of a specified string array.
    /// The returned array will be empty if and only if the input string is empty.
    /// </summary>
    /// <param name="strings"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("Splits")>]
    let splits (strings : string[]) (str : string) =
        if isEmpty str then Array.empty
        else str.Split (strings, System.StringSplitOptions.None)

    /// <summary>Returns a new string created by concatenating the strings in the specified string array.</summary>
    /// <param name="arr"></param>
    /// <returns></returns>
    /// <remarks>This method is empirically known to be the fastest way to concatenate a small number (&lt;10) of short strings.</remarks>
    [<CompiledName("ConcatArray")>]
    let inline concatArray (arr : string[]) =
        System.String.Join (empty, arr)

    /// <summary>Creates a new string by joining the specified strings using an environment-specific newline character sequence.</summary>
    /// <param name="lines"></param>
    /// <return></return>
    [<CompiledName("OfLines")>]
    let inline ofLines (lines : string[]) =
        System.String.Join (System.Environment.NewLine, lines)

    /// <summary>Splits a string into individual lines.</summary>
    /// <param name="str"></param>
    /// <return></return>
    [<CompiledName("ToLines")>]
    let inline toLines (str : string) =
        str.Split ([| "\r\n"; "\r"; "\n" |], System.StringSplitOptions.None)

    /// <summary>Gets a substring of a string.</summary>
    /// <param name="str"></param>
    /// <param name="offset"></param>
    /// <param name="count"></param>
    /// <return></return>
    [<CompiledName("Sub")>]
    let inline sub (str : string) offset count : substring =
        substring (str, offset, count)

    /// <summary>Returns the index of the first occurrence of a specified character within a string.</summary>
    /// <param name="c"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("TryFindIndexOf")>]
    let tryFindIndexOf (c : char) (str : string) =
        // Preconditions
        checkNonNull "str" str

        Substring.tryFindIndexOf c (substring (str))

    /// <summary>Returns the index of the first occurrence of a specified character within a string.</summary>
    /// <param name="c"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("FindIndexOf")>]
    let findIndexOf (c : char) (str : string) =
        // Preconditions
        checkNonNull "str" str

        Substring.findIndexOf c (substring (str))

    /// <summary>Returns the index of the first character in the string which satisfies the given predicate.</summary>
    /// <param name="predicate"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("TryFindIndex")>]
    let tryFindIndex (predicate : char -> bool) (str : string) : int option =
        // Preconditions
        checkNonNull "str" str

        Substring.tryFindIndex predicate (substring (str))

    /// <summary>Returns the index of the first character in the string which satisfies the given predicate.</summary>
    /// <param name="predicate"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("FindIndex")>]
    let findIndex (predicate : char -> bool) (str : string) : int =
        // Preconditions
        checkNonNull "str" str

        Substring.findIndex predicate (substring (str))

    /// <summary>Returns the first character in the string which satisfies the given predicate.</summary>
    /// <param name="predicate"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("TryFind")>]
    let tryFind (predicate : char -> bool) (str : string) : char option =
        // Preconditions
        checkNonNull "str" str

        Substring.tryFind predicate (substring (str))

    /// <summary>Returns the first character in the string which satisfies the given predicate.</summary>
    /// <param name="predicate"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("Find")>]
    let find (predicate : char -> bool) (str : string) : char =
        // Preconditions
        checkNonNull "str" str

        Substring.find predicate (substring (str))

    /// <summary></summary>
    /// <param name="picker"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("TryPick")>]
    let tryPick (picker : char -> 'T option) (str : string) : 'T option =
        // Preconditions
        checkNonNull "str" str

        Substring.tryPick picker (substring (str))

    /// <summary></summary>
    /// <param name="picker"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("Pick")>]
    let pick (picker : char -> 'T option) (str : string) : 'T =
        // Preconditions
        checkNonNull "str" str

        Substring.pick picker (substring (str))

    /// <summary>
    /// Applies a function to each character of the string, threading an accumulator argument through the computation.
    /// If the input function is <c>f</c> and the characters are <c>c0...cN</c> then computes <c>f (...(f s c0)...) cN</c>.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> char -> 'State) (state : 'State) (str : string) : 'State =
        // Preconditions
        checkNonNull "str" str

        Substring.fold folder state (substring (str))

    /// <summary>
    /// Applies a function to each character of the string, threading an accumulator argument through the computation.
    /// If the input function is <c>f</c> and the characters are <c>c0...cN</c> then computes <c>f c0 (...(f cN s))</c>.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="str"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    [<CompiledName("FoldBack")>]
    let foldBack (folder : char -> 'State -> 'State) (str : string) (state : 'State) : 'State =
        // Preconditions
        checkNonNull "str" str

        Substring.foldBack folder (substring (str)) state

    /// <summary>
    /// Applies the given function to pairs of characters drawn from matching indices in two strings, threading an accumulator argument
    /// through the computation. The two strings must have the same length, otherwise an <see cref="ArgumentException"/> is raised. If the
    /// input function is <c>f</c> and the characters are <c>c0...cN</c> and <c>d0...dN</c> then computes <c>f (...(f s c0 d0)...) cN d0</c>.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="str1"></param>
    /// <param name="str2"></param>
    /// <returns></returns>
    [<CompiledName("Fold2")>]
    let fold2 (folder : 'State -> char -> char -> 'State) (state : 'State) (str1 : string) (str2 : string) : 'State =
        // Preconditions
        checkNonNull "str1" str1
        checkNonNull "str2" str2

        /// The length of the input string.
        let len = String.length str1
        if len <> String.length str2 then
            invalidArg "str" "The strings have different lengths."

        // OPTIMIZATION : If the strings are empty return immediately.
        if len = 0 then state
        else
            let folder = FSharpFunc<_,_,_,_>.Adapt folder

            // Fold over the strings.
            let mutable state = state
            for i = 0 to len - 1 do
                state <- folder.Invoke (state, str1.[i], str2.[i])
            state

    /// <summary>
    /// Applies the given function to pairs of characters drawn from matching indices in two strings, threading an accumulator argument
    /// through the computation. The two strings must have the same length, otherwise an <see cref="ArgumentException"/> is raised.
    /// If the input function is <c>f</c> and the characters are <c>c0...cN</c> and <c>d0...dN</c> then computes <c>f c0 d0 (...(f cN dN s))</c>.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="str1"></param>
    /// <param name="str2"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    [<CompiledName("FoldBack2")>]
    let foldBack2 (folder : char -> char -> 'State -> 'State) (str1 : string) (str2 : string) (state : 'State) : 'State =
        // Preconditions
        checkNonNull "str1" str1
        checkNonNull "str2" str2

        /// The length of the input string.
        let len = String.length str1
        if len <> String.length str2 then
            invalidArg "str" "The strings have different lengths."

        // OPTIMIZATION : If the strings are empty return immediately.
        if len = 0 then state
        else
            let folder = FSharpFunc<_,_,_,_>.Adapt folder

            // Fold backwards over the strings.
            let mutable state = state
            for i = len - 1 downto 0 do
                state <- folder.Invoke (str1.[i], str2.[i], state)
            state

    /// <summary>Applies the given function to each character of the string.</summary>
    /// <param name="action"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("Iterate")>]
    let iter (action : char -> unit) (str : string) : unit =
        // Preconditions
        checkNonNull "str" str
        
        /// The length of the input string.
        let len = String.length str

        // Iterate over the string, applying the action to each character.
        for i = 0 to len - 1 do
            action str.[i]

    /// <summary>
    /// Applies the given function to each character of the string.
    /// The integer passed to the function indicates the index of the character.
    /// </summary>
    /// <param name="action"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("IterateIndexed")>]
    let iteri (action : int -> char -> unit) (str : string) : unit =
        // Preconditions
        checkNonNull "str" str
        
        let action = FSharpFunc<_,_,_>.Adapt action

        /// The length of the input string.
        let len = String.length str

        // Iterate over the string, applying the action to each character.
        for i = 0 to len - 1 do
            action.Invoke (i, str.[i])

    /// <summary>
    /// Applies the given function to pairs of characters drawn from matching indices in two strings.
    /// The two strings must have the same length, otherwise an <see cref="ArgumentException"/> is raised.
    /// </summary>
    /// <param name="action"></param>
    /// <param name="str1"></param>
    /// <param name="str2"></param>
    /// <returns></returns>
    [<CompiledName("Iterate2")>]
    let iter2 (action : char -> char -> unit) (str1 : string) (str2 : string) : unit =
        // Preconditions
        checkNonNull "str1" str1
        checkNonNull "str2" str2
        
        let len = String.length str1
        if len <> String.length str2 then
            invalidArg "str2" "The strings have different lengths."

        let action = FSharpFunc<_,_,_>.Adapt action

        for i = 0 to len - 1 do
            action.Invoke (str1.[i], str2.[i])

    /// <summary>
    /// Applies the given function to pairs of characters drawn from matching indices in two strings, also passing the
    /// index of the characters. The two strings must have the same length, otherwise an <see cref="ArgumentException"/> is raised.
    /// </summary>
    /// <param name="action"></param>
    /// <param name="str1"></param>
    /// <param name="str2"></param>
    /// <returns></returns>
    [<CompiledName("IterateIndexed2")>]
    let iteri2 (action : int -> char -> char -> unit) (str1 : string) (str2 : string) : unit =
        // Preconditions
        checkNonNull "str1" str1
        checkNonNull "str2" str2
        
        let len = String.length str1
        if len <> String.length str2 then
            invalidArg "str2" "The strings have different lengths."

        let action = FSharpFunc<_,_,_,_>.Adapt action

        for i = 0 to len - 1 do
            action.Invoke (i, str1.[i], str2.[i])

    /// <summary>
    /// Builds a new string whose characters are the results of applying the given function to each character of the string.
    /// </summary>
    /// <param name="mapping"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("Map")>]
    let map (mapping : char -> char) (str : string) : string =
        // Preconditions
        checkNonNull "str" str
        
        /// The length of the input string.
        let len = String.length str

        // OPTIMIZATION : If the input string is empty return immediately.
        if len = 0 then
            empty
        else
            /// The mapped characters.
            let mappedChars = Array.zeroCreate len

            // Iterate over the string, applying the mapping to each character
            // and storing the result into the array of mapped characters.
            for i = 0 to len - 1 do
                mappedChars.[i] <- mapping str.[i]

            // Create a new string from the mapped characters.
            ofArray mappedChars

    /// <summary>
    /// Builds a new string whose characters are the results of applying the given function to each character of the string.
    /// The integer index passed to the function indicates the index of the character being transformed.
    /// </summary>
    /// <param name="mapping"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> char -> char) (str : string) : string =
        // Preconditions
        checkNonNull "str" str

        /// The length of the input string.
        let len = String.length str

        // OPTIMIZATION : If the input string is empty return immediately.
        if len = 0 then
            empty
        else
            let mapping = FSharpFunc<_,_,_>.Adapt mapping

            /// The mapped characters.
            let mappedChars = Array.zeroCreate len

            // Iterate over the string, applying the mapping to each character
            // and storing the result into the array of mapped characters.
            for i = 0 to len - 1 do
                mappedChars.[i] <- mapping.Invoke (i, str.[i])

            // Create a new string from the mapped characters.
            ofArray mappedChars

    /// <summary>
    /// Builds a new string whose characters are the results of applying the given function to the corresponding characters of the
    /// two strings pairwise. The two inputs strings must have the same length, otherwise <see cref="ArgumentException"/> is raised.
    /// </summary>
    /// <param name="mapping"></param>
    /// <param name="str1"></param>
    /// <param name="str2"></param>
    /// <returns></returns>
    [<CompiledName("Map2")>]
    let map2 (mapping : char -> char -> char) (str1 : string) (str2 : string) : string =
        // Preconditions
        checkNonNull "str1" str1
        checkNonNull "str2" str2

        /// The length of the input string.
        let len = String.length str1
        if len <> String.length str2 then
            invalidArg "str" "The strings have different lengths."

        // OPTIMIZATION : If the strings are empty return immediately.
        if len = 0 then
            empty
        else
            let mapping = FSharpFunc<_,_,_>.Adapt mapping

            /// The mapped characters.
            let mappedChars = Array.zeroCreate len

            // Iterate over the string, applying the mapping to each character pair
            // and storing the result into the array of mapped characters.
            for i = 0 to len - 1 do
                mappedChars.[i] <- mapping.Invoke (str1.[i], str2.[i])

            // Create a new string from the mapped characters.
            ofArray mappedChars

    /// <summary>
    /// Builds a new string whose characters are the results of applying the given function to the corresponding characters
    /// of the two strings pairwise, also passing the index of the characters.
    /// The two inputs strings must have the same length, otherwise <see cref="ArgumentException"/> is raised.
    /// </summary>
    /// <param name="mapping"></param>
    /// <param name="str1"></param>
    /// <param name="str2"></param>
    /// <returns></returns>
    [<CompiledName("MapIndexed2")>]
    let mapi2 (mapping : int -> char -> char -> char) (str1 : string) (str2 : string) : string =
       // Preconditions
        checkNonNull "str1" str1
        checkNonNull "str2" str2
        
        /// The length of the input string.
        let len = String.length str1
        if len <> String.length str2 then
            invalidArg "str" "The strings have different lengths."

        // OPTIMIZATION : If the strings are empty return immediately.
        if len = 0 then
            empty
        else
            let mapping = FSharpFunc<_,_,_,_>.Adapt mapping

            /// The mapped characters.
            let mappedChars = Array.zeroCreate len

            // Iterate over the string, applying the mapping to each character pair
            // and storing the result into the array of mapped characters.
            for i = 0 to len - 1 do
                mappedChars.[i] <- mapping.Invoke (i, str1.[i], str2.[i])

            // Create a new string from the mapped characters.
            ofArray mappedChars

    /// <summary>
    /// Applies the given function to each character in the string.
    /// Returns the string comprised of the results <c>x</c> where the function returns <c>Some(x)</c>.
    /// </summary>
    /// <param name="chooser"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("Choose")>]
    let choose (chooser : char -> char option) (str : string) : string =
        // Preconditions
        checkNonNull "str" str
        
        /// The length of the input string.
        let len = String.length str

        // OPTIMIZATION : If the input string is empty return immediately.
        if len = 0 then
            empty
        else
            /// The chosen characters.
            let chosenChars = Array.zeroCreate len

            /// The number of chosen characters.
            let mutable chosenCount = 0

            // Loop over the characters in the string, applying the chooser function and
            // copying the result (if any) into the array of chosen characters.
            for i = 0 to len - 1 do
                match chooser str.[i] with
                | None -> ()
                | Some chosen ->
                    chosenChars.[chosenCount] <- chosen
                    chosenCount <- chosenCount + 1

            // Create a new string from the chosen characters.
            ofArray chosenChars.[..chosenCount-1]

    /// <summary>
    /// Applies the given function to each character in the string. Returns the string comprised of the results <c>x</c> where the
    /// function returns <c>Some(x)</c>. The integer index passed to the function indicates the index of the character being transformed.
    /// </summary>
    /// <param name="chooser"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("ChooseIndexed")>]
    let choosei (chooser : int -> char -> char option) (str : string) : string =
        // Preconditions
        checkNonNull "str" str
        
        /// The length of the input string.
        let len = String.length str

        // OPTIMIZATION : If the input string is empty return immediately.
        if len = 0 then
            empty
        else
            let chooser = FSharpFunc<_,_,_>.Adapt chooser

            /// The chosen characters.
            let chosenChars = Array.zeroCreate len

            /// The number of chosen characters.
            let mutable chosenCount = 0

            // Loop over the characters in the string, applying the chooser function and
            // copying the result (if any) into the array of chosen characters.
            for i = 0 to len - 1 do
                match chooser.Invoke (i, str.[i]) with
                | None -> ()
                | Some chosen ->
                    chosenChars.[chosenCount] <- chosen
                    chosenCount <- chosenCount + 1

            // Create a new string from the chosen characters.
            ofArray chosenChars.[..chosenCount-1]

    /// <summary>
    /// Applies a function to each character in the string and the character which
    /// follows it, threading an accumulator argument through the computation.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("FoldPairwise")>]
    let foldPairwise (folder : 'State -> char -> char -> 'State) state (str : string) =
        // Preconditions
        checkNonNull "str" str

        // OPTIMIZATION : If the string is empty or contains just one element,
        // immediately return the input state.
        let len = String.length str
        if len < 2 then state
        else           
            let folder = FSharpFunc<_,_,_,_>.Adapt folder
            let mutable state = state

            for i = 0 to len - 2 do
                state <- folder.Invoke (state, str.[i], str.[i + 1])

            // Return the final state value
            state

    /// <summary>Removes all leading and trailing occurrences of the specified characters from a string.</summary>
    /// <param name="chars"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("Trim")>]
    let inline trim (chars : char[]) (str : string) =
        str.Trim chars

    /// <summary>Removes all leading occurrences of the specified set of characters from a string.</summary>
    /// <param name="trimChars"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("TrimStart")>]
    let inline trimStart trimChars (str : string) =
        str.TrimStart trimChars

    /// <summary>Removes all trailing occurrences of the specified set of characters from a string.</summary>
    /// <param name="trimChars"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("TrimEnd")>]
    let inline trimEnd trimChars (str : string) =
        str.TrimEnd trimChars

    /// <summary>Removes all leading occurrences of characters satisfying the given predicate from a string.</summary>
    /// <param name="predicate"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("TrimStartWith")>]
    let trimStartWith (predicate : char -> bool) (str : string) =
        // Preconditions
        checkNonNull "str" str

        Substring.trimStartWith predicate (substring (str))
        |> Substring.toString

    /// <summary>Removes all trailing occurrences of characters satisfying the given predicate from a string.</summary>
    /// <param name="predicate"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("TrimEndWith")>]
    let trimEndWith (predicate : char -> bool) (str : string) =
        // Preconditions
        checkNonNull "str" str

        Substring.trimEndWith predicate (substring (str))
        |> Substring.toString

    /// <summary>Removes all leading and trailing occurrences of characters satisfying the given predicate from a string.</summary>
    /// <param name="predicate"></param>
    /// <param name="str"></param>
    /// <returns></returns>
    [<CompiledName("TrimWith")>]
    let trimWith (predicate : char -> bool) (str : string) =
        // Preconditions
        checkNonNull "str" str

        Substring.trimWith predicate (substring (str))
        |> Substring.toString

    
    /// <summary>
    /// String-splitting functions.
    /// These functions are analagous calling the <see cref="String.Split"/> method with <c>StringSplitOptions.None</c>,
    /// but are faster because they avoid creating the intermediate array of strings.
    /// </summary>
    [<RequireQualifiedAccess>]
    module Split =
        open System

        /// <summary>
        /// Applies the given function to each of the substrings in the input string that are
        /// delimited by elements of a specified Unicode character array.
        /// </summary>
        /// <param name="separator"></param>
        /// <param name="action"></param>
        /// <param name="str"></param>
        /// <returns></returns>
        [<CompiledName("Iterate")>]
        let iter (separator : char[]) (action : substring -> unit) (str : string) : unit =
            // Preconditions
            checkNonNull "str" str

            // OPTIMIZATION : If the input string is empty, call the action once then return.
            if isEmpty str then
                action (substring (str, 0, 0))
            else
                Substring.Split.iter separator action (substring (str))

        /// <summary>
        /// Applies the given function to each of the substrings in the input string that are delimited by elements of a specified
        /// Unicode character array. The integer index applied to the function is the index of the substring within the virtual array
        /// of substrings in the input string. For example, if the newline character (\n) is used as the separator, the index of each
        /// substring would be the line number.
        /// </summary>
        /// <param name="separator"></param>
        /// <param name="action"></param>
        /// <param name="str"></param>
        /// <returns></returns>
        [<CompiledName("IterateIndexed")>]
        let iteri (separator : char[]) (action : int -> substring -> unit) (str : string) : unit =
            // Preconditions
            checkNonNull "str" str

            // OPTIMIZATION : If the input string is empty, call the action once then return.
            if isEmpty str then
                action 0 (substring (str, 0, 0))
            else
                Substring.Split.iteri separator action (substring (str))

        /// <summary>
        /// Applies the given function to each of the substrings in the input string that are delimited by elements
        /// of a specified Unicode character array, threading an accumulator argument through the computation.
        /// </summary>
        /// <param name="separator"></param>
        /// <param name="folder"></param>
        /// <param name="state"></param>
        /// <param name="str"></param>
        /// <returns></returns>
        [<CompiledName("Fold")>]
        let fold (separator : char[]) (folder : 'State -> substring -> 'State) (state : 'State) (str : string) : 'State =
            // Preconditions
            checkNonNull "str" str

            // OPTIMIZATION : If the input string is empty, call the folder once then return.
            if isEmpty str then
                folder state (substring (str, 0, 0))
            else
                Substring.Split.fold separator folder state (substring (str))

        /// <summary>
        /// Applies the given function to each of the substrings in the input string that are delimited by elements of a specified Unicode
        /// character array, threading an accumulator argument through the computation. The integer index applied to the function is the
        /// index of the substring within the virtual array of substrings in the input string. For example, if the newline character (\n)
        /// is used as the separator, the index of each substring would be the line number.
        /// </summary>
        /// <param name="separator"></param>
        /// <param name="folder"></param>
        /// <param name="state"></param>
        /// <param name="str"></param>
        /// <returns></returns>
        [<CompiledName("FoldIndexed")>]
        let foldi (separator : char[]) (folder : 'State -> int -> substring -> 'State) (state : 'State) (str : string) : 'State =
            // Preconditions
            checkNonNull "str" str

            // OPTIMIZATION : If the input string is empty, call the folder once then return.
            if isEmpty str then
                folder state 0 (substring (str, 0, 0))
            else
                Substring.Split.foldi separator folder state (substring (str))

        (*
        /// <summary></summary>
        /// <param name="separator"></param>
        /// <param name="predicate"></param>
        /// <param name="str"></param>
        /// <returns></returns>
        [<CompiledName("Filter")>]
        let filter (separator : char[]) (predicate : substring -> bool) (str : string) : substring[] =
            // Preconditions
            checkNonNull "str" str

            // OPTIMIZATION : If the input string is empty, return immediately.
            if isEmpty str then Array.empty
            else
                notImpl "String.Split.filter"

        /// <summary></summary>
        /// <param name="separator"></param>
        /// <param name="chooser"></param>
        /// <param name="str"></param>
        /// <returns></returns>
        [<CompiledName("Choose")>]
        let choose (separator : char[]) (chooser : substring -> string option) (str : string) : string[] =
            // Preconditions
            checkNonNull "str" str

            // OPTIMIZATION : If the input string is empty, return immediately.
            if isEmpty str then Array.empty
            else
                notImpl "String.Split.choose"
        *)
