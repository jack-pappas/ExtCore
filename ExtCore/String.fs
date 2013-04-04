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
        if offset >= strLen then
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
        // OPTIMIZATION : Immediately return if this is an empty substring.
        if this.Length = 0 then
            System.String.Empty
        else
            this.String.Substring (this.Offset, this.Length)

    /// Copies the characters in this substring into a Unicode character array.
    member this.ToArray () : char[] =
        this.String.ToCharArray (this.Offset, this.Length)

//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Substring =
    open OptimizedClosures

    //
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (substring : Substring) : bool =
        substring.IsEmpty

    //
    [<CompiledName("ToString")>]
    let inline toString (substring : Substring) : string =
        substring.ToString ()

    //
    [<CompiledName("ToArray")>]
    let inline toArray (substring : Substring) : char[] =
        substring.ToArray ()

    //
    [<CompiledName("Iter")>]
    let iter action (substring : Substring) : unit =
        let len = substring.Length
        for i = 0 to len - 1 do
            action substring.[i]

    //
    [<CompiledName("IterIndexed")>]
    let iteri action (substring : Substring) : unit =
        // OPTIMIZATION : Immediately return if the substring is empty.
        let len = substring.Length
        if len > 0 then
            let action = FSharpFunc<_,_,_>.Adapt action

            for i = 0 to len - 1 do
                action.Invoke (i, substring.[i])

    //
    [<CompiledName("IterBack")>]
    let iterBack action (substring : Substring) : unit =
        let len = substring.Length
        for i = len - 1 downto 0 do
            action substring.[i]

    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> char -> 'State) state (substring : Substring) : 'State =
        // OPTIMIZATION : Immediately return if the substring is empty.
        let len = substring.Length
        if len = 0 then state
        else
            let folder = FSharpFunc<_,_,_>.Adapt folder
            
            let mutable state = state
            for i = 0 to len - 1 do
                state <- folder.Invoke (state, substring.[i])
            state

    //
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : 'State -> int -> char -> 'State) state (substring : Substring) : 'State =
        // OPTIMIZATION : Immediately return if the substring is empty.
        let len = substring.Length
        if len = 0 then state
        else
            let folder = FSharpFunc<_,_,_,_>.Adapt folder
            
            let mutable state = state
            for i = 0 to len - 1 do
                state <- folder.Invoke (state, i, substring.[i])
            state

    //
    [<CompiledName("FoldBack")>]
    let foldBack (folder : char -> 'State -> 'State) (substring : Substring) state : 'State =
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

    /// Indicates whether the specified string is null or empty.
    [<CompiledName("IsNullOrEmpty")>]
    let inline isNullOrEmpty str =
        System.String.IsNullOrEmpty str

    /// Indicates whether the specified string is empty.
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (str : string) =
        str.Length < 1

    /// <summary>Returns a new string created by concatenating the strings in the specified string array.</summary>
    /// <remarks>For a smaller number (&lt;10) of fairly short strings,
    /// this method is the empirically fastest-known way to concatenate
    /// strings without the overhead of a System.Text.StringBuilder.</remarks>
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

    /// Creates a string from an F# option value.
    /// If the option value is <c>None</c>, returns an empty string;
    /// returns <c>s</c> when the option value is <c>Some s</c>
    [<CompiledName("OfOption")>]
    let inline ofOption value =
        match value with
        | None -> empty
        | Some str -> str

    /// Creates an F# option value from the specified string.
    /// If the string 's' is null or empty, returns None; otherwise, returns Some s.
    [<CompiledName("ToOption")>]
    let inline toOption str =
        if System.String.IsNullOrEmpty str then None
        else Some str

    /// Creates a new string by removing all leading and
    /// trailing white-space characters from a string.
    [<CompiledName("Trim")>]
    let inline trim (str : string) =
        str.Trim ()

    /// Removes all leading and trailing occurrences of
    /// the specified characters from a string.
    [<CompiledName("TrimChars")>]
    let inline trimChars (chars : char[]) (str : string) =
        str.Trim chars

    /// Builds a character array from the given string.
    [<CompiledName("ToArray")>]
    let inline toArray (str : string) =
        str.ToCharArray ()

    /// Builds a string from the given character array.
    [<CompiledName("OfArray")>]
    let inline ofArray (chars : char[]) =
        System.String (chars)

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
            Some index
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
            ofArray chosenChars.[..chosenCount]

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
            ofArray chosenChars.[..chosenCount]

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

    (*
    /// String-splitting functions.
    /// These work like String.Split but without creating the intermediate array.
    [<RequireQualifiedAccess>]
    module Split =
        // OPTIMIZE : The functions below could be modified to include an optimized case
        // for when the separator array contains just a single character.

        //
        let iter (separator : char[]) (action : string -> unit) (str : string) : unit =
            // Preconditions
            checkNonNull "separator" separator
            checkNonNull "str" str
            // TODO : What if separator is empty?

            notImpl "String.Split.iter"

        //
        let iteri (separator : char[]) (action : int -> string -> unit) (str : string) : unit =
            // Preconditions
            checkNonNull "separator" separator
            checkNonNull "str" str
            // TODO : What if separator is empty?

            // NOTE : The int passed to the action is not the overall index of the character in the string,
            // but the index into the "virtual" array of separated strings.
            // E.g., if the separator was [| '\n' |] the index value would be the line number.

            notImpl "String.Split.iteri"

        //
        let fold (separator : char[]) (folder : 'State -> string -> 'State) (state : 'State) (str : string) : 'State =
            // Preconditions
            checkNonNull "separator" separator
            checkNonNull "str" str
            // TODO : What if separator is empty?

            notImpl "String.Split.fold"

        //
        let foldi (separator : char[]) (folder : int -> 'State -> string -> 'State) (state : 'State) (str : string) : 'State =
            // Preconditions
            checkNonNull "separator" separator
            checkNonNull "str" str
            // TODO : What if separator is empty?

            notImpl "String.Split.foldi"
    *)
    (*
    - String.Split.filter
    - String.Split.choose
      - These functions should work like .Split(...) |> Array.iter (or Array.fold, etc.),
        except that they won't actually need to traverse the entire string first and split
        it into an array of substrings. Instead, they'll improve performance by allowing us
        to execute a function on each of the substrings in a single forward pass.
      - We might be able to further improve performance by not actually applying the given
        function to the substrings themselves, but instead to some struct representing the
        initial position and length of the substring; this may be enough information for some
        functions (e.g., to filter out strings less than 10 characters), but then we'd easily
        be able to create the substring from that information if necessary.
            - Perhaps create some type that has a field containing a Lazy<string> which could
              create the string value if the user wants it.
      - This could be implemented by using the regular vector type / functions from fsharplex.
    *)

