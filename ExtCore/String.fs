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

//open System.Diagnostics.Contracts


/// A substring.
[<Struct>]
type Substring =
    //
    val String : string
    //
    val Offset : int
    //
    val Length : int
    
    /// <summary>Create a new Substring value.</summary>
    /// <param name="string"></param>
    /// <param name="offset"></param>
    /// <param name="length"></param>
    new (str : string, offset : int, length : int) =
        // Preconditions
        checkNonNull "str" str
        if offset < 0 then
            argOutOfRange "offset" "The offset must be greater than or equal to zero."
        
        /// The length of the underlying string.
        let strLen = String.length str

        // More preconditions
        if offset > strLen then
            argOutOfRange "offset" "The offset must be less than the length of the string."
        elif length < 0 then
            argOutOfRange "length" "The substring length must be greater than or equal to zero."
        elif offset + length > strLen then
            argOutOfRange "length" "The specified length is greater than the number of characters \
                                    in the string from the given offset."

        { String = str;
          Offset = offset;
          Length = length; }

    /// Is this an empty substring?
    member this.IsEmpty
        with get () =
            this.Length = 0

    //
    member this.Item
        with get index =
            // Preconditions
            if index < 0 || index >= this.Length then
                // TODO : Provide a better error message here.
                raise <| System.IndexOutOfRangeException ()
            
            // Return the specified character from the underlying string.
            this.String.[this.Offset + index]

    /// <inherit />
    override this.ToString () =
        // OPTIMIZATION : Immediately return if this is an empty substring;
        // or, if the substring covers the entire string, just return the string.
        if this.Length = 0 then
            System.String.Empty
        elif this.Offset = 0 && this.Length = this.String.Length then
            this.String
        else
            this.String.Substring (this.Offset, this.Length)

    /// Copies the characters in this substring into a Unicode character array.
    member this.ToArray () : char[] =
        this.String.ToCharArray (this.Offset, this.Length)

    /// Implements F# slicing syntax for substrings.
    member this.GetSlice (startIndex, finishIndex) : substring =
        let startIndex = defaultArg startIndex 0
        let finishIndex = defaultArg finishIndex this.Length

        Substring (
            this.String,
            this.Offset + startIndex,
            finishIndex - startIndex + 1)

/// Represents a segment of a string.
and substring = Substring

/// Functional operators related to Substrings.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Substring =
    open OptimizedClosures

    /// Returns the string underlying the given substring.
    [<CompiledName("String")>]
    let inline string (substring : substring) : string =
        substring.String

    /// The starting offset of the substring within it's underlying string.
    [<CompiledName("Offset")>]
    let inline offset (substring : substring) : int =
        substring.Offset

    /// Returns the length of the substring.
    [<CompiledName("Length")>]
    let inline length (substring : substring) : int =
        substring.Length

    /// Gets a character from the substring.
    [<CompiledName("Get")>]
    let inline get (substring : substring) index : char =
        substring.[index]

    /// Is the substring empty?
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (substring : substring) : bool =
        substring.IsEmpty

    /// Returns a substring which covers the entire length of the given string.
    [<CompiledName("OfString")>]
    let inline ofString (str : string) : substring =
        Substring (str, 0, str.Length)

    /// Instantiates the substring as a string.
    [<CompiledName("ToString")>]
    let inline toString (substring : substring) : string =
        substring.ToString ()

    /// Returns the characters in the given substring as a Unicode character array. 
    [<CompiledName("ToArray")>]
    let inline toArray (substring : substring) : char[] =
        substring.ToArray ()

    /// Gets a substring of a substring.
    [<CompiledName("Sub")>]
    let sub (substring : substring) offset count : substring =
        // Preconditions
        if offset < 0 then
            argOutOfRange "offset" "The offset cannot be negative."
        elif count < 0 then
            argOutOfRange "count" "The length of the substring cannot be negative."
        elif offset >= substring.Length then
            argOutOfRange "offset" "The offset must be less than the length of the input substring."
        elif (offset + count) >= substring.Length then
            argOutOfRange "count" "There are fewer than 'count' elements in the \
                                   input substring when starting at the given offset."

        // Create a new substring based on the input substring.
        Substring (substring.String, substring.Offset + offset, count)

    /// Builds a new string by concatenating the given sequence of substrings.
    [<CompiledName("Concat")>]
    let concat (substrings : seq<substring>) : string =
        // Preconditions
        checkNonNull "substrings" substrings

        // If the sequence is empty, return immediately.
        if Seq.isEmpty substrings then
            System.String.Empty
        else
            let sb = System.Text.StringBuilder ()
            substrings |> Seq.iter (sb.Append >> ignore)
            sb.ToString ()

    /// Applies the given function to each character in the substring,
    /// in order from lowest to highest indices.
    [<CompiledName("Iter")>]
    let iter action (substring : substring) : unit =
        let len = substring.Length
        for i = 0 to len - 1 do
            action substring.[i]

    /// Applies the given function to each character in the substring,
    /// in order from lowest to highest indices.
    /// The integer index applied to the function is the character's index within the substring.
    [<CompiledName("IterIndexed")>]
    let iteri action (substring : substring) : unit =
        // OPTIMIZATION : Immediately return if the substring is empty.
        let len = substring.Length
        if len > 0 then
            let action = FSharpFunc<_,_,_>.Adapt action

            for i = 0 to len - 1 do
                action.Invoke (i, substring.[i])

    /// Applies the given function to each character in the substring,
    /// in order from highest to lowest indices.
    [<CompiledName("IterBack")>]
    let iterBack action (substring : substring) : unit =
        let len = substring.Length
        for i = len - 1 downto 0 do
            action substring.[i]

    /// Applies a function to each character of the substring, threading an accumulator
    /// argument through the computation.
    /// If the input function is f and the characters are c0...cN then computes f (...(f s c0)...) cN.
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> char -> 'State) state (substring : substring) : 'State =
        // OPTIMIZATION : Immediately return if the substring is empty.
        let len = substring.Length
        if len = 0 then state
        else
            let folder = FSharpFunc<_,_,_>.Adapt folder
            
            let mutable state = state
            for i = 0 to len - 1 do
                state <- folder.Invoke (state, substring.[i])
            state

    /// Applies a function to each character of the substring, threading an accumulator
    /// argument through the computation.
    /// The integer index applied to the function is the character's index within the substring.
    /// If the input function is f and the characters are c0...cN then computes f (...(f s c0)...) cN.
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : 'State -> int -> char -> 'State) state (substring : substring) : 'State =
        // OPTIMIZATION : Immediately return if the substring is empty.
        let len = substring.Length
        if len = 0 then state
        else
            let folder = FSharpFunc<_,_,_,_>.Adapt folder
            
            let mutable state = state
            for i = 0 to len - 1 do
                state <- folder.Invoke (state, i, substring.[i])
            state

    /// Applies a function to each character of the string, threading an accumulator
    /// argument through the computation.
    /// If the input function is f and the characters are c0...cN then computes f c0 (...(f cN s)).
    [<CompiledName("FoldBack")>]
    let foldBack (folder : char -> 'State -> 'State) (substring : substring) state : 'State =
        // OPTIMIZATION : Immediately return if the substring is empty.
        let len = substring.Length
        if len = 0 then state
        else
            let folder = FSharpFunc<_,_,_>.Adapt folder
            
            let mutable state = state
            for i = len - 1 downto 0 do
                state <- folder.Invoke (substring.[i], state)
            state


/// Additional functional operators on strings.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module String =
    open OptimizedClosures

    /// The empty string literal.
    [<CompiledName("Empty")>]
    let [<Literal>] empty = ""

    /// Gets a character from a string.
    [<CompiledName("Get")>]
    let inline get (str : string) (index : int) =
        str.[index]

    /// Indicates whether the specified string is empty.
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (str : string) =
        str.Length < 1

    /// Creates a string from an F# option value.
    /// If the option value is <c>None</c>, returns an empty string;
    /// returns <c>s</c> when the option value is <c>Some s</c>
    [<CompiledName("OfOption")>]
    let inline ofOption value =
        match value with
        | None -> empty
        | Some str -> str

    /// Builds a string from the given character array.
    [<CompiledName("OfArray")>]
    let inline ofArray (chars : char[]) =
        System.String (chars)

    /// Creates an F# option value from the specified string.
    /// If the string 's' is null or empty, returns None; otherwise, returns Some s.
    [<CompiledName("ToOption")>]
    let inline toOption str =
        if System.String.IsNullOrEmpty str then None
        else Some str

    /// Builds a character array from the given string.
    [<CompiledName("ToArray")>]
    let inline toArray (str : string) =
        str.ToCharArray ()

    /// Returns a string array that contains the substrings in a string that are delimited
    /// by elements of the given Unicode character array. The returned array will be empty
    /// if and only if the input string is empty.
    [<CompiledName("Split")>]
    let split (chars : char[]) (str : string) =
        if isEmpty str then Array.empty
        else str.Split chars

    /// <summary>Returns a new string created by concatenating the strings in the specified string array.</summary>
    /// <remarks>
    /// For a smaller number (&lt;10) of fairly short strings, this method is empirically known
    /// as the fastest way to concatenate them.
    /// </remarks>
    [<CompiledName("ConcatArray")>]
    let inline concatArray (arr : string[]) =
        System.String.Join (empty, arr)

    /// Creates a new string by joining the specified strings using
    /// an environment-specific newline character sequence.
    [<CompiledName("OfLines")>]
    let inline ofLines (lines : string[]) =
        System.String.Join (System.Environment.NewLine, lines)

    /// Splits a string into individual lines.
    [<CompiledName("ToLines")>]
    let inline toLines (str : string) =
        str.Split ([| '\r'; '\n' |], System.StringSplitOptions.RemoveEmptyEntries)

    /// Gets a substring of a string.
    [<CompiledName("Sub")>]
    let inline sub (str : string) offset count : substring =
        Substring (str, offset, count)

    /// Returns the index of the first occurrence of a specified character within a string.
    [<CompiledName("TryFindIndexOf")>]
    let inline tryFindIndexOf (c : char) (str : string) =
        // Preconditions
        checkNonNull "str" str

        match str.IndexOf c with
        | -1 -> None
        | idx -> Some idx

    /// Returns the index of the first occurrence of a specified character within a string.
    [<CompiledName("FindIndexOf")>]
    let inline findIndexOf (c : char) (str : string) =
        // Preconditions
        checkNonNull "str" str

        match str.IndexOf c with
        | -1 ->
            // TODO : Return a better error message.
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()
        | idx -> idx

    /// Returns the index of the first character in the string which satisfies the given predicate.
    [<CompiledName("TryFindIndex")>]
    let tryFindIndex (predicate : char -> bool) (str : string) =
        // Preconditions
        checkNonNull "str" str

        let len = String.length str
        
        let mutable index = 0
        let mutable foundMatch = false

        while index < len && not foundMatch do
            foundMatch <- predicate str.[index]
            index <- index + 1

        // Return the index of the matching character, if any.
        if foundMatch then
            // Subtract one from the index since it was incremented after finding
            // the match but before the loop terminated.
            Some (index - 1)
        else None

    /// Returns the index of the first character in the string which satisfies the given predicate.
    [<CompiledName("FindIndex")>]
    let findIndex (predicate : char -> bool) (str : string) =
        // Preconditions
        checkNonNull "str" str

        // Use tryFindIndex to find the match; raise an exception if one is not found.
        match tryFindIndex predicate str with
        | Some index ->
            index
        | None ->
            // TODO : Return a better error message.
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    /// Applies a function to each character of the string, threading an accumulator
    /// argument through the computation.
    /// If the input function is f and the characters are c0...cN then computes f (...(f s c0)...) cN.
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> char -> 'State) (state : 'State) (str : string) : 'State =
        // Preconditions
        checkNonNull "str" str

        let folder = FSharpFunc<_,_,_>.Adapt folder

        /// The length of the input string.
        let len = String.length str

        // Fold over the string.
        let mutable state = state
        for i = 0 to len - 1 do
            state <- folder.Invoke (state, str.[i])
        state

    /// Applies a function to each character of the string, threading an accumulator
    /// argument through the computation.
    /// If the input function is f and the characters are c0...cN then computes f c0 (...(f cN s)).
    [<CompiledName("FoldBack")>]
    let foldBack (folder : char -> 'State -> 'State) (str : string) (state : 'State) : 'State =
        // Preconditions
        checkNonNull "str" str

        let folder = FSharpFunc<_,_,_>.Adapt folder

        /// The length of the input string.
        let len = String.length str

        // Fold backwards over the string.
        let mutable state = state
        for i = len - 1 downto 0 do
            state <- folder.Invoke (str.[i], state)
        state

    /// Applies the given function to each character of the string.
    [<CompiledName("Iter")>]
    let iter (action : char -> unit) (str : string) : unit =
        // Preconditions
        checkNonNull "str" str
        
        /// The length of the input string.
        let len = String.length str

        // Iterate over the string, applying the action to each character.
        for i = 0 to len - 1 do
            action str.[i]

    /// Applies the given function to each character of the string.
    /// The integer passed to the function indicates the index of the character.
    [<CompiledName("IterIndexed")>]
    let iteri (action : int -> char -> unit) (str : string) : unit =
        // Preconditions
        checkNonNull "str" str
        
        let action = FSharpFunc<_,_,_>.Adapt action

        /// The length of the input string.
        let len = String.length str

        // Iterate over the string, applying the action to each character.
        for i = 0 to len - 1 do
            action.Invoke (i, str.[i])

    /// Builds a new string whose characters are the results of applying the given
    /// function to each character of the string.
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

    /// Builds a new string whose characters are the results of applying the given
    /// function to each character of the string.
    /// The integer index passed to the function indicates the index of the character being transformed.
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

    /// Applies the given function to each character in the string.
    /// Returns the string comprised of the results 'x' where the function returns <c>Some(x)</c>.
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

    /// Applies the given function to each character in the string.
    /// Returns the string comprised of the results 'x' where the function returns <c>Some(x)</c>.
    /// The integer index passed to the function indicates the index of the character being transformed.
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

    /// Removes all leading and trailing occurrences of
    /// the specified characters from a string.
    [<CompiledName("Trim")>]
    let inline trim (chars : char[]) (str : string) =
        str.Trim chars

    /// Removes all leading occurrences of the specified set of characters from a string.
    [<CompiledName("TrimStart")>]
    let inline trimStart trimChars (str : string) =
        str.TrimStart trimChars

    /// Removes all trailing occurrences of the specified set of characters from a string.
    [<CompiledName("TrimEnd")>]
    let inline trimEnd trimChars (str : string) =
        str.TrimEnd trimChars

    /// Removes all leading occurrences of characters satisfying the given predicate from a string.
    [<CompiledName("TrimStartWith")>]
    let trimStartWith (predicate : char -> bool) (str : string) =
        // Preconditions
        checkNonNull "str" str
        
        /// The length of the string.
        let len = String.length str

        // OPTIMIZATION : If the string is empty, return immediately.
        if len = 0 then empty
        else
            let mutable index = 0
            let mutable foundMatch = false

            // Loop until we find a character which _does_ match the predicate.
            while index < len && not foundMatch do
                foundMatch <- predicate str.[index]
                index <- index + 1

            // If none of the characters matched the predicate, don't bother
            // calling .Substring(), just return an empty string.
            if foundMatch then
                // If the predicate was immediately matched, there's nothing
                // to trim so return the initial string.
                if index = 1 then str
                else
                    str.Substring (index - 1)
            else
                empty

    /// Removes all trailing occurrences of characters satisfying the given predicate from a string.
    [<CompiledName("TrimEndWith")>]
    let trimEndWith (predicate : char -> bool) (str : string) =
        // Preconditions
        checkNonNull "str" str

        /// The length of the string.
        let len = String.length str

        // OPTIMIZATION : If the string is empty, return immediately.
        if len = 0 then empty
        else
            let mutable index = len - 1
            let mutable foundMatch = false

            // Loop until we find a character which _does_ match the predicate.
            while index >= 0 && not foundMatch do
                foundMatch <- predicate str.[index]
                index <- index - 1

            // If none of the characters matched the predicate, don't bother
            // calling .Substring(), just return an empty string.
            if foundMatch then
                // If the predicate was immediately matched, there's nothing
                // to trim so return the initial string.
                if index = len - 2 then str
                else
                    str.Substring (0, index + 2)
            else
                empty

    /// Removes all leading and trailing occurrences of characters satisfying the given predicate from a string.
    [<CompiledName("TrimWith")>]
    let trimWith (predicate : char -> bool) (str : string) =
        // Preconditions
        checkNonNull "str" str

        /// The length of the string.
        let len = String.length str

        // OPTIMIZATION : If the string is empty, return immediately.
        match len with
        | 0 -> empty
        | 1 ->
            if predicate str.[0] then str else empty
        | len ->
            let mutable index = 0
            let mutable foundLeftMatch = false

            // Loop until we find a character which _does_ match the predicate.
            while index < len && not foundLeftMatch do
                foundLeftMatch <- predicate str.[index]
                index <- index + 1

            // If none of the characters matched the predicate, don't bother
            // calling .Substring(), just return an empty string.
            if foundLeftMatch then
                /// The starting index of the trimmed string.
                let trimmedStartIndex = index - 1

                // Loop backwards to trim the right side of the string.
                let mutable index = len - 1
                let mutable foundRightMatch = false

                while index > trimmedStartIndex && not foundRightMatch do
                    foundRightMatch <- predicate str.[index]
                    index <- index - 1

                // Return the trimmed string.
                if foundRightMatch then
                    /// The index of the last character in the trimmed string.
                    let trimmedEndIndex = index + 1

                    /// The length of the trimmed string.
                    let trimmedLength =
                        trimmedEndIndex - trimmedStartIndex + 1

                    // If nothing was trimmed, just return the original string.
                    if trimmedLength = len then str
                    else
                        str.Substring (trimmedStartIndex, trimmedLength)
                else
                    str.[trimmedStartIndex].ToString ()
            else
                empty

    
    /// String-splitting functions.
    /// These work like String.Split but are faster because they avoid creating
    /// the intermediate array of strings.
    [<RequireQualifiedAccess>]
    module Split =
        open System

        // OPTIMIZE : The functions below could be modified to include an optimized case
        // for when the separator array contains just a single character.

        /// Determines if the StringSplitOptions.RemoveEmptyEntries flag is set in the given value.
        let inline private skipEmptyStrings (options : System.StringSplitOptions) =
            Enum.hasFlag StringSplitOptions.RemoveEmptyEntries options

        /// Applies the given function to each of the substrings in the input string that are
        /// delimited by elements of a specified Unicode character array.
        [<CompiledName("Iter")>]
        let iter (separator : char[], options : StringSplitOptions)
                (action : substring -> unit) (str : string) : unit =
            // Preconditions
            checkNonNull "str" str

            // OPTIMIZATION : If the input string is empty, return immediately.
            if not <| isEmpty str then
                /// The length of the input string.
                let len = String.length str

                // The string-splitting functionality must be implemented specially for
                // the case where the separator array is null or empty.
                match separator with
                | null
                | [| |] ->
                    // The offset and length of the current substring.
                    let mutable offset = 0
                    let mutable length = 0

                    // OPTIMIZATION : The check for the RemoveEmptyEntries option is moved
                    // out of the loop to avoid checking it repeatedly.
                    if skipEmptyStrings options then
                        for i = 0 to len - 1 do
                            // Is the current character a separator?
                            if Char.IsWhiteSpace str.[i] then
                                // Apply the function to the current substring unless it's empty.
                                if length > 0 then
                                    action <| Substring (str, offset, length)

                                // Update the offset to just past the end of this substring
                                // and reset the length to zero to begin a new substring.
                                offset <- i + 1
                                length <- 0

                            else
                                // "Add" this character to the current substring by
                                // incrementing the substring length.
                                length <- length + 1
                    else
                        for i = 0 to len - 1 do
                            // Is the current character a separator?
                            if Char.IsWhiteSpace str.[i] then
                                // Apply the function to the current substring.
                                action <| Substring (str, offset, length)

                                // Update the offset to just past the end of this substring
                                // and reset the length to zero to begin a new substring.
                                offset <- i + 1
                                length <- 0

                            else
                                // "Add" this character to the current substring by
                                // incrementing the substring length.
                                length <- length + 1

                    // If the length is nonzero, then the last substring is still "in-progress"
                    // so apply the function to it.
                    if length > 0 || not <| skipEmptyStrings options then
                        action <| Substring (str, offset, length)

                | separator ->
                    /// A sorted copy of the separator array. Used with Array.BinarySearch
                    /// to quickly determine if a given character is a separator.
                    let sortedSeparators = Array.sort separator

                    // The offset and length of the current substring.
                    let mutable offset = 0
                    let mutable length = 0

                    // OPTIMIZATION : The check for the RemoveEmptyEntries option is moved
                    // out of the loop to avoid checking it repeatedly.
                    if skipEmptyStrings options then
                        for i = 0 to len - 1 do
                            // Is the current character a separator?
                            if Array.BinarySearch (sortedSeparators, str.[i]) >= 0 then
                                // Apply the function to the current substring unless it's empty.
                                if length > 0 then
                                    action <| Substring (str, offset, length)

                                // Update the offset to just past the end of this substring
                                // and reset the length to zero to begin a new substring.
                                offset <- i + 1
                                length <- 0

                            else
                                // "Add" this character to the current substring by
                                // incrementing the substring length.
                                length <- length + 1
                    else
                        for i = 0 to len - 1 do
                            // Is the current character a separator?
                            if Array.BinarySearch (sortedSeparators, str.[i]) >= 0 then
                                // Apply the function to the current substring.
                                action <| Substring (str, offset, length)

                                // Update the offset to just past the end of this substring
                                // and reset the length to zero to begin a new substring.
                                offset <- i + 1
                                length <- 0

                            else
                                // "Add" this character to the current substring by
                                // incrementing the substring length.
                                length <- length + 1

                    // If the length is nonzero, then the last substring is still "in-progress"
                    // so apply the function to it.
                    if length > 0 || not <| skipEmptyStrings options then
                        action <| Substring (str, offset, length)

        /// Applies the given function to each of the substrings in the input string that are
        /// delimited by elements of a specified Unicode character array. The integer index
        /// applied to the function is the index of the substring within the virtual array of
        /// substrings in the input string. For example, if the newline character (\n) is used
        /// as the separator, the index of each substring would be the line number.
        [<CompiledName("IterIndexed")>]
        let iteri (separator : char[], options : System.StringSplitOptions)
                (action : int -> substring -> unit) (str : string) : unit =
            // Preconditions
            checkNonNull "str" str

            // OPTIMIZATION : If the input string is empty, return immediately.
            if not <| isEmpty str then
                /// The length of the input string.
                let len = String.length str

                let action = FSharpFunc<_,_,_>.Adapt action

                // The string-splitting functionality must be implemented specially for
                // the case where the separator array is null or empty.
                match separator with
                | null
                | [| |] ->
                    // The offset and length of the current substring.
                    let mutable offset = 0
                    let mutable length = 0

                    /// The current substring index (amongst the delimited substrings in the input string).
                    let mutable substringIndex = 0

                    // OPTIMIZATION : The check for the RemoveEmptyEntries option is moved
                    // out of the loop to avoid checking it repeatedly.
                    if skipEmptyStrings options then
                        for i = 0 to len - 1 do
                            // Is the current character a separator?
                            if Char.IsWhiteSpace str.[i] then
                                // Apply the function to the current substring unless it's empty.
                                if length > 0 then
                                    action.Invoke (substringIndex, Substring (str, offset, length))

                                // Update the offset to just past the end of this substring
                                // and reset the length to zero to begin a new substring.
                                offset <- i + 1
                                length <- 0

                                // Increment the substring index.
                                substringIndex <- substringIndex + 1

                            else
                                // "Add" this character to the current substring by
                                // incrementing the substring length.
                                length <- length + 1
                    else
                        for i = 0 to len - 1 do
                            // Is the current character a separator?
                            if Char.IsWhiteSpace str.[i] then
                                // Apply the function to the current substring.
                                action.Invoke (substringIndex, Substring (str, offset, length))

                                // Update the offset to just past the end of this substring
                                // and reset the length to zero to begin a new substring.
                                offset <- i + 1
                                length <- 0

                                // Increment the substring index.
                                substringIndex <- substringIndex + 1

                            else
                                // "Add" this character to the current substring by
                                // incrementing the substring length.
                                length <- length + 1

                    // If the length is nonzero, then the last substring is still "in-progress"
                    // so apply the function to it.
                    if length > 0 || not <| skipEmptyStrings options then
                        action.Invoke (substringIndex, Substring (str, offset, length))

                | separator ->
                    /// A sorted copy of the separator array. Used with Array.BinarySearch
                    /// to quickly determine if a given character is a separator.
                    let sortedSeparators = Array.sort separator

                    // The offset and length of the current substring.
                    let mutable offset = 0
                    let mutable length = 0

                    /// The current substring index (amongst the delimited substrings in the input string).
                    let mutable substringIndex = 0

                    // OPTIMIZATION : The check for the RemoveEmptyEntries option is moved
                    // out of the loop to avoid checking it repeatedly.
                    if skipEmptyStrings options then
                        for i = 0 to len - 1 do
                            // Is the current character a separator?
                            if Array.BinarySearch (sortedSeparators, str.[i]) >= 0 then
                                // Apply the function to the current substring unless it's empty.
                                if length > 0 then
                                    action.Invoke (substringIndex, Substring (str, offset, length))

                                // Update the offset to just past the end of this substring
                                // and reset the length to zero to begin a new substring.
                                offset <- i + 1
                                length <- 0

                            else
                                // "Add" this character to the current substring by
                                // incrementing the substring length.
                                length <- length + 1
                    else
                        for i = 0 to len - 1 do
                            // Is the current character a separator?
                            if Array.BinarySearch (sortedSeparators, str.[i]) >= 0 then
                                // Apply the function to the current substring.
                                action.Invoke (substringIndex, Substring (str, offset, length))

                                // Update the offset to just past the end of this substring
                                // and reset the length to zero to begin a new substring.
                                offset <- i + 1
                                length <- 0

                            else
                                // "Add" this character to the current substring by
                                // incrementing the substring length.
                                length <- length + 1

                    // If the length is nonzero, then the last substring is still "in-progress"
                    // so apply the function to it.
                    if length > 0 || not <| skipEmptyStrings options then
                        action.Invoke (substringIndex, Substring (str, offset, length))

        //
        [<CompiledName("Fold")>]
        let fold (separator : char[], options : System.StringSplitOptions)
                (folder : 'State -> substring -> 'State) (state : 'State) (str : string) : 'State =
            // Preconditions
            checkNonNull "str" str

            // OPTIMIZATION : If the input string is empty, return immediately.
            if isEmpty str then state
            else
                /// The length of the input string.
                let len = String.length str

                let folder = FSharpFunc<_,_,_>.Adapt folder

                // The string-splitting functionality must be implemented specially for
                // the case where the separator array is null or empty.
                match separator with
                | null
                | [| |] ->
                    // The offset and length of the current substring.
                    let mutable offset = 0
                    let mutable length = 0

                    /// The current state value.
                    let mutable state = state

                    // OPTIMIZATION : The check for the RemoveEmptyEntries option is moved
                    // out of the loop to avoid checking it repeatedly.
                    if skipEmptyStrings options then
                        for i = 0 to len - 1 do
                            // Is the current character a separator?
                            if Char.IsWhiteSpace str.[i] then
                                // Apply the function to the current substring unless it's empty.
                                if length > 0 then
                                    state <- folder.Invoke (state, Substring (str, offset, length))

                                // Update the offset to just past the end of this substring
                                // and reset the length to zero to begin a new substring.
                                offset <- i + 1
                                length <- 0

                            else
                                // "Add" this character to the current substring by
                                // incrementing the substring length.
                                length <- length + 1
                    else
                        for i = 0 to len - 1 do
                            // Is the current character a separator?
                            if Char.IsWhiteSpace str.[i] then
                                // Apply the function to the current substring.
                                state <- folder.Invoke (state, Substring (str, offset, length))

                                // Update the offset to just past the end of this substring
                                // and reset the length to zero to begin a new substring.
                                offset <- i + 1
                                length <- 0

                            else
                                // "Add" this character to the current substring by
                                // incrementing the substring length.
                                length <- length + 1

                    // If the length is nonzero, then the last substring is still "in-progress"
                    // so apply the function to it.
                    if length > 0 || not <| skipEmptyStrings options then
                        state <- folder.Invoke (state, Substring (str, offset, length))
                    state

                | separator ->
                    /// A sorted copy of the separator array. Used with Array.BinarySearch
                    /// to quickly determine if a given character is a separator.
                    let sortedSeparators = Array.sort separator

                    // The offset and length of the current substring.
                    let mutable offset = 0
                    let mutable length = 0

                    /// The current state value.
                    let mutable state = state

                    // OPTIMIZATION : The check for the RemoveEmptyEntries option is moved
                    // out of the loop to avoid checking it repeatedly.
                    if skipEmptyStrings options then
                        for i = 0 to len - 1 do
                            // Is the current character a separator?
                            if Array.BinarySearch (sortedSeparators, str.[i]) >= 0 then
                                // Apply the function to the current substring unless it's empty.
                                if length > 0 then
                                    state <- folder.Invoke (state, Substring (str, offset, length))

                                // Update the offset to just past the end of this substring
                                // and reset the length to zero to begin a new substring.
                                offset <- i + 1
                                length <- 0

                            else
                                // "Add" this character to the current substring by
                                // incrementing the substring length.
                                length <- length + 1
                    else
                        for i = 0 to len - 1 do
                            // Is the current character a separator?
                            if Array.BinarySearch (sortedSeparators, str.[i]) >= 0 then
                                // Apply the function to the current substring.
                                state <- folder.Invoke (state, Substring (str, offset, length))

                                // Update the offset to just past the end of this substring
                                // and reset the length to zero to begin a new substring.
                                offset <- i + 1
                                length <- 0

                            else
                                // "Add" this character to the current substring by
                                // incrementing the substring length.
                                length <- length + 1

                    // If the length is nonzero, then the last substring is still "in-progress"
                    // so apply the function to it.
                    if length > 0 || not <| skipEmptyStrings options then
                        state <- folder.Invoke (state, Substring (str, offset, length))
                    state

        //
        [<CompiledName("FoldIndexed")>]
        let foldi (separator : char[], options : System.StringSplitOptions)
                (folder : 'State -> int -> substring -> 'State) (state : 'State) (str : string) : 'State =
            // Preconditions
            checkNonNull "str" str

            // OPTIMIZATION : If the input string is empty, return immediately.
            if isEmpty str then state
            else
                /// The length of the input string.
                let len = String.length str

                let folder = FSharpFunc<_,_,_,_>.Adapt folder

                // The string-splitting functionality must be implemented specially for
                // the case where the separator array is null or empty.
                match separator with
                | null
                | [| |] ->
                    // The offset and length of the current substring.
                    let mutable offset = 0
                    let mutable length = 0

                    /// The current substring index (amongst the delimited substrings in the input string).
                    let mutable substringIndex = 0

                    /// The current state value.
                    let mutable state = state

                    // OPTIMIZATION : The check for the RemoveEmptyEntries option is moved
                    // out of the loop to avoid checking it repeatedly.
                    if skipEmptyStrings options then
                        for i = 0 to len - 1 do
                            // Is the current character a separator?
                            if Char.IsWhiteSpace str.[i] then
                                // Apply the function to the current substring unless it's empty.
                                if length > 0 then
                                    state <- folder.Invoke (state, substringIndex, Substring (str, offset, length))

                                // Update the offset to just past the end of this substring
                                // and reset the length to zero to begin a new substring.
                                offset <- i + 1
                                length <- 0

                                // Increment the substring index.
                                substringIndex <- substringIndex + 1

                            else
                                // "Add" this character to the current substring by
                                // incrementing the substring length.
                                length <- length + 1
                    else
                        for i = 0 to len - 1 do
                            // Is the current character a separator?
                            if Char.IsWhiteSpace str.[i] then
                                // Apply the function to the current substring.
                                state <- folder.Invoke (state, substringIndex, Substring (str, offset, length))

                                // Update the offset to just past the end of this substring
                                // and reset the length to zero to begin a new substring.
                                offset <- i + 1
                                length <- 0

                                // Increment the substring index.
                                substringIndex <- substringIndex + 1

                            else
                                // "Add" this character to the current substring by
                                // incrementing the substring length.
                                length <- length + 1

                    // If the length is nonzero, then the last substring is still "in-progress"
                    // so apply the function to it.
                    if length > 0 || not <| skipEmptyStrings options then
                        state <- folder.Invoke (state, substringIndex, Substring (str, offset, length))
                    state

                | separator ->
                    /// A sorted copy of the separator array. Used with Array.BinarySearch
                    /// to quickly determine if a given character is a separator.
                    let sortedSeparators = Array.sort separator

                    // The offset and length of the current substring.
                    let mutable offset = 0
                    let mutable length = 0

                    /// The current substring index (amongst the delimited substrings in the input string).
                    let mutable substringIndex = 0

                    /// The current state value.
                    let mutable state = state

                    // OPTIMIZATION : The check for the RemoveEmptyEntries option is moved
                    // out of the loop to avoid checking it repeatedly.
                    if skipEmptyStrings options then
                        for i = 0 to len - 1 do
                            // Is the current character a separator?
                            if Array.BinarySearch (sortedSeparators, str.[i]) >= 0 then
                                // Apply the function to the current substring unless it's empty.
                                if length > 0 then
                                    state <- folder.Invoke (state, substringIndex, Substring (str, offset, length))

                                // Update the offset to just past the end of this substring
                                // and reset the length to zero to begin a new substring.
                                offset <- i + 1
                                length <- 0

                            else
                                // "Add" this character to the current substring by
                                // incrementing the substring length.
                                length <- length + 1
                    else
                        for i = 0 to len - 1 do
                            // Is the current character a separator?
                            if Array.BinarySearch (sortedSeparators, str.[i]) >= 0 then
                                // Apply the function to the current substring.
                                state <- folder.Invoke (state, substringIndex, Substring (str, offset, length))

                                // Update the offset to just past the end of this substring
                                // and reset the length to zero to begin a new substring.
                                offset <- i + 1
                                length <- 0

                            else
                                // "Add" this character to the current substring by
                                // incrementing the substring length.
                                length <- length + 1

                    // If the length is nonzero, then the last substring is still "in-progress"
                    // so apply the function to it.
                    if length > 0 || not <| skipEmptyStrings options then
                        state <- folder.Invoke (state, substringIndex, Substring (str, offset, length))
                    state


//
module StringOperators =
    type System.String with
        member this.GetSlice (startIndex, finishIndex) : substring =
            let startIndex = defaultArg startIndex 0 
            let finishIndex = defaultArg finishIndex this.Length
            Substring (this, startIndex, finishIndex - startIndex + 1)

    type System.Text.StringBuilder with
        /// Appends a copy of the specified substring to this instance.
        member this.Append (value : substring) : System.Text.StringBuilder =
            // OPTIMIZATION : If the substring is empty, return immediately.
            if value.IsEmpty then this
            else
                this.Append (value.String, value.Offset, value.Length)
            
        /// Appends a copy of the specified substring to this instance, appending a newline.
        member this.AppendLine (value : substring) : System.Text.StringBuilder =
            let this = this.Append value
            this.AppendLine ()
