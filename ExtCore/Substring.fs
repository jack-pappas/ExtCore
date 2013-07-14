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


/// Represents a segment of a string.
[<Struct; CompiledName("Substring")>]
[<CustomEquality; CustomComparison>]
type substring =
    /// The underlying string for this substring.
    val String : string
    //
    val Offset : int
    //
    val Length : int

    /// <summary>Create a new substring value.</summary>
    /// <param name="string"></param>
    new (str : string) =
        // Preconditions
        checkNonNull "str" str

        { String = str;
          Offset = 0;
          Length = str.Length; }
    
    /// <summary>Create a new substring value.</summary>
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

        substring (
            this.String,
            this.Offset + startIndex,
            finishIndex - startIndex + 1)

    /// Structural comparison on substrings.
    static member private Compare (x : substring, y : substring) =
        // OPTIMIZATION : If the substrings have the same (identical) underlying string
        // and offset, the comparison value will depend only on the length of the substrings.
        if x.String === y.String && x.Offset = y.Offset then
            compare x.Length y.Length
        else
            (* Structural comparison on substrings -- this uses the same comparison
               technique as the structural comparison on strings in FSharp.Core. *)
#if INVARIANT_CULTURE_STRING_COMPARISON
            // NOTE: we don't have to null check here because System.String.Compare
            // gives reliable results on null values.
            System.String.Compare (
                x.String, x.Offset,
                y.String, y.Offset,
                min x.Length y.Length,
                false,
                CultureInfo.InvariantCulture)
#else
            // NOTE: we don't have to null check here because System.String.CompareOrdinal
            // gives reliable results on null values.
            System.String.CompareOrdinal (
                x.String, x.Offset,
                y.String, y.Offset,
                min x.Length y.Length)
#endif

    /// <inherit />
    override this.GetHashCode () =
        if isNull this.String then 0
        else
            // OPTIMIZE : This needs to be re-implemented ASAP so it directly computes the
            // hash value of the substring (i.e., without creating the substring).
            this.ToString().GetHashCode ()

    /// <inherit />
    override this.Equals other =
        match other with
        | :? substring as other ->
            substring.Compare (this, other) = 0
        | _ ->
            invalidArg "other" "The value is not a substring."

    interface IEquatable<substring> with
        /// <inherit />
        member this.Equals other =
            substring.Compare (this, other) = 0

    interface IComparable with
        /// <inherit />
        member this.CompareTo other =
            match other with
            | :? substring as other ->
                substring.Compare (this, other)
            | _ ->
                invalidArg "other" "The value is not a substring."

    interface IComparable<substring> with
        /// <inherit />
        member this.CompareTo other =
            substring.Compare (this, other)


/// Functional operators related to substrings.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Substring =
    open OptimizedClosures

    /// Returns the string underlying the given substring.
    [<CompiledName("String")>]
    let inline string (substr : substring) : string =
        substr.String

    /// The starting offset of the substring within it's underlying string.
    [<CompiledName("Offset")>]
    let inline offset (substr : substring) : int =
        substr.Offset

    /// Returns the length of the substring.
    [<CompiledName("Length")>]
    let inline length (substr : substring) : int =
        substr.Length

    /// Gets a character from the substring.
    [<CompiledName("Get")>]
    let inline get (substr : substring) index : char =
        substr.[index]

    /// Is the substring empty?
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (substr : substring) : bool =
        substr.IsEmpty

    /// Returns a substring which covers the entire length of the given string.
    [<CompiledName("OfString")>]
    let inline ofString (str : string) : substring =
        substring (str, 0, str.Length)

    /// Instantiates the substring as a string.
    [<CompiledName("ToString")>]
    let inline toString (substr : substring) : string =
        substr.ToString ()

    /// Returns the characters in the given substring as a Unicode character array. 
    [<CompiledName("ToArray")>]
    let inline toArray (substr : substring) : char[] =
        substr.ToArray ()

    /// Extracts the first (left-most) character from a substring, returning a Some value
    /// containing the character and the remaining substring. Returns None if the given
    /// substring is empty.
    [<CompiledName("Read")>]
    let read (substr : substring) : (char * substring) option =
        if substr.Length = 0 then None
        else
            // "Extract" the first (left-most) character from the substring.
            Some (substr.[0], substr.[1..])

    /// Gets a substring of a substring.
    [<CompiledName("Sub")>]
    let sub (substr : substring) offset count : substring =
        // Preconditions
        if offset < 0 then
            argOutOfRange "offset" "The offset cannot be negative."
        elif count < 0 then
            argOutOfRange "count" "The length of the substring cannot be negative."
        elif offset >= substr.Length then
            argOutOfRange "offset" "The offset must be less than the length of the input substring."
        elif (offset + count) >= substr.Length then
            argOutOfRange "count" "There are fewer than 'count' elements in the \
                                   input substring when starting at the given offset."

        // Create a new substring based on the input substring.
        substring (substr.String, substr.Offset + offset, count)

    /// Builds a new string by concatenating the given sequence of substrings.
    [<CompiledName("Concat")>]
    let concat (substrs : seq<substring>) : string =
        // Preconditions
        checkNonNull "substrs" substrs

        // If the sequence is empty, return immediately.
        if Seq.isEmpty substrs then
            System.String.Empty
        else
            let sb = System.Text.StringBuilder ()
            substrs |> Seq.iter (sb.Append >> ignore)
            sb.ToString ()

    /// Returns the index of the first occurrence of a specified character within a substring.
    [<CompiledName("TryFindIndexOf")>]
    let tryFindIndexOf (c : char) (substr : substring) =
        // Preconditions
        // (None)

        // OPTIMIZATION : Return immediately if the substring is empty.
        match substr.Length with
        | 0 -> None
        | len ->
            match substr.String.IndexOf (c, substr.Offset, len) with
            | -1 -> None
            | idx -> Some idx

    /// Returns the index of the first occurrence of a specified character within a substring.
    [<CompiledName("FindIndexOf")>]
    let findIndexOf (c : char) (substr : substring) =
        // Preconditions
        // (None)

        // OPTIMIZATION : Return immediately if the substring is empty.
        match substr.Length with
        | 0 ->
            // TODO : Return a better error message.
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()
        | len ->
            match substr.String.IndexOf (c, substr.Offset, len) with
            | -1 ->
                // TODO : Return a better error message.
                //keyNotFound ""
                raise <| System.Collections.Generic.KeyNotFoundException ()
            | idx -> idx

    /// Returns the index of the first character in the substring which satisfies the given predicate.
    [<CompiledName("TryFindIndex")>]
    let tryFindIndex (predicate : char -> bool) (substr : substring) : int option =
        // Preconditions
        // (None)

        let len = substr.Length

        let mutable index = 0
        let mutable foundMatch = false

        while index < len && not foundMatch do
            foundMatch <- predicate substr.[index]
            index <- index + 1

        // Return the index of the matching character, if any.
        if foundMatch then
            // Subtract one from the index since it was incremented after finding
            // the match but before the loop terminated.
            Some (index - 1)
        else None

    /// Returns the index of the first character in the substring which satisfies the given predicate.
    [<CompiledName("FindIndex")>]
    let findIndex (predicate : char -> bool) (substr : substring) : int =
        // Preconditions
        // (None)

        // Use tryFindIndex to find the match; raise an exception if one is not found.
        match tryFindIndex predicate substr with
        | Some index ->
            index
        | None ->
            // TODO : Return a better error message.
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    /// Returns the first character in the substring which satisfies the given predicate.
    [<CompiledName("TryFind")>]
    let tryFind (predicate : char -> bool) (substr : substring) : char option =
        // Preconditions
        // (None)

        let len = substr.Length
        
        let mutable index = 0
        let mutable foundMatch = false

        while index < len && not foundMatch do
            foundMatch <- predicate substr.[index]
            index <- index + 1

        // Return the matching character, if any.
        if foundMatch then
            // Subtract one from the index since it was incremented after finding
            // the match but before the loop terminated.
            Some substr.[index - 1]
        else None

    /// Returns the first character in the substring which satisfies the given predicate.
    [<CompiledName("Find")>]
    let find (predicate : char -> bool) (substr : substring) : char =
        // Preconditions
        // (None)

        // Use tryFind to find the match; raise an exception if one is not found.
        match tryFind predicate substr with
        | Some ch ->
            ch
        | None ->
            // TODO : Return a better error message.
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    //
    [<CompiledName("TryPick")>]
    let tryPick (picker : char -> 'T option) (substr : substring) : 'T option =
        // Preconditions
        // (None)

        let len = substr.Length
        
        let mutable picked = None
        let mutable index = 0

        while index < len && Option.isNone picked do
            picked <- picker substr.[index]
            index <- index + 1

        // Return the picked value, if any.
        picked

    //
    [<CompiledName("Pick")>]
    let pick (picker : char -> 'T option) (substr : substring) : 'T =
        // Preconditions
        // (None)

        // Use tryPick to find the match; raise an exception if one is not found.
        match tryPick picker substr with
        | Some result ->
            result
        | None ->
            // TODO : Return a better error message.
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    /// Applies the given function to each character in the substring,
    /// in order from lowest to highest indices.
    [<CompiledName("Iterate")>]
    let iter action (substr : substring) : unit =
        let len = substr.Length
        for i = 0 to len - 1 do
            action substr.[i]

    /// Applies the given function to each character in the substring,
    /// in order from lowest to highest indices.
    /// The integer index applied to the function is the character's index within the substring.
    [<CompiledName("IterateIndexed")>]
    let iteri action (substr : substring) : unit =
        // OPTIMIZATION : Immediately return if the substring is empty.
        let len = substr.Length
        if len > 0 then
            let action = FSharpFunc<_,_,_>.Adapt action

            for i = 0 to len - 1 do
                action.Invoke (i, substr.[i])

    /// Applies the given function to each character in the substring,
    /// in order from highest to lowest indices.
    [<CompiledName("IterateBack")>]
    let iterBack action (substr : substring) : unit =
        let len = substr.Length
        for i = len - 1 downto 0 do
            action substr.[i]

    /// Applies the given function to each character in the substring,
    /// in order from highest to lowest indices.
    /// The integer index applied to the function is the character's index within the substring.
    [<CompiledName("IterateIndexedBack")>]
    let iteriBack action (substr : substring) : unit =
        // OPTIMIZATION : Immediately return if the substring is empty.
        let len = substr.Length
        if len > 0 then
            let action = FSharpFunc<_,_,_>.Adapt action

            for i = len - 1 downto 0 do
                action.Invoke (i, substr.[i])

    /// Applies a function to each character of the substring, threading an accumulator
    /// argument through the computation.
    /// If the input function is f and the characters are c0...cN then computes f (...(f s c0)...) cN.
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> char -> 'State) state (substr : substring) : 'State =
        // OPTIMIZATION : Immediately return if the substring is empty.
        let len = substr.Length
        if len = 0 then state
        else
            let folder = FSharpFunc<_,_,_>.Adapt folder
            
            let mutable state = state
            for i = 0 to len - 1 do
                state <- folder.Invoke (state, substr.[i])
            state

    /// Applies a function to each character of the substring, threading an accumulator
    /// argument through the computation.
    /// The integer index applied to the function is the character's index within the substring.
    /// If the input function is f and the characters are c0...cN then computes f (...(f s c0)...) cN.
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : 'State -> int -> char -> 'State) state (substr : substring) : 'State =
        // OPTIMIZATION : Immediately return if the substring is empty.
        let len = substr.Length
        if len = 0 then state
        else
            let folder = FSharpFunc<_,_,_,_>.Adapt folder
            
            let mutable state = state
            for i = 0 to len - 1 do
                state <- folder.Invoke (state, i, substr.[i])
            state

    /// Applies a function to each character of the string, threading an accumulator
    /// argument through the computation.
    /// If the input function is f and the characters are c0...cN then computes f c0 (...(f cN s)).
    [<CompiledName("FoldBack")>]
    let foldBack (folder : char -> 'State -> 'State) (substr : substring) state : 'State =
        // OPTIMIZATION : Immediately return if the substring is empty.
        let len = substr.Length
        if len = 0 then state
        else
            let folder = FSharpFunc<_,_,_>.Adapt folder
            
            let mutable state = state
            for i = len - 1 downto 0 do
                state <- folder.Invoke (substr.[i], state)
            state


/// Extension methods for System.String and System.Text.StringBuilder
/// which provide integration with the substring type.
module SubstringExtensions =
    type System.String with
        /// Returns a new substring created from this string and the given
        /// starting and ending indices.
        member this.GetSlice (startIndex, finishIndex) : substring =
            let startIndex = defaultArg startIndex 0 
            let finishIndex = defaultArg finishIndex this.Length
            substring (this, startIndex, finishIndex - startIndex + 1)

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

    type System.Text.RegularExpressions.Regex with
        /// Searches the input substring for the first occurrence of a regular expression. 
        member this.Match (value : substring) =
            this.Match (value.String, value.Offset, value.Length)
