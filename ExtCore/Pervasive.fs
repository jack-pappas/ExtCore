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


/// Additional operators for F# data types.
[<AutoOpen>]
module AdditionalOperators =
    /// Type abbreviation for System.NotImplementedException.
    type notImplExn = System.NotImplementedException

    /// Type abbreviation for System.ArraySegment.
    type ArraySegment<'T> = System.ArraySegment<'T>

    (* Type extensions for System.ArraySegment<'T> *)
    type System.ArraySegment<'T> with
        member this.Item
            with get index =
                assert (index >= 0)
                assert (index < this.Count)
                this.Array.[this.Offset + index]
                
            and set index value =
                assert (index >= 0)
                assert (index < this.Count)
                this.Array.[this.Offset + index] <- value


    /// The opticons ("optional cons") operator.
    let inline (%?) (x : 'T option) lst =
        match x with Some a -> a :: lst | None -> lst

    /// Swaps the values of a tuple so their order is reversed.
    [<CompiledName("Swap")>]
    let inline swap (x : 'T, y : 'U) =
        y, x

    /// Swaps the order of the arguments to a function.
    [<CompiledName("Flip")>]
    let inline flip f (x : 'T) (y : 'U) : 'V =
        f y x

    /// Raises a System.NotImplementedException.
    [<CompiledName("RaiseNotImplementedException")>]
    let inline notImpl msg =
        raise <| System.NotImplementedException msg

    /// Raises a System.NotSupportedException.
    [<CompiledName("RaiseNotSupportedException")>]
    let inline notSupported msg =
        raise <| System.NotSupportedException msg

    /// Raises a System.ArgumentOutOfRangeException.
    [<CompiledName("RaiseArgumentOutOfRangeException")>]
    let inline argOutOfRange (paramName : string) (message : string) =
        raise <| System.ArgumentOutOfRangeException (paramName, message)

    /// Compares two objects for reference equality.
    [<CompiledName("RefEquals")>]
    let inline refEquals< ^T, ^U when ^T : not struct and ^U : not struct> (x : ^T) (y : ^U) =
        System.Object.ReferenceEquals (x, y)

    /// Determines if a reference is a null reference.
    [<CompiledName("IsNull")>]
    let inline isNull< ^T when ^T : not struct> (x : ^T) =
        System.Object.ReferenceEquals (null, x)

    /// Determines if a reference is a null reference, and if it is, throws an ArgumentNullException.
    [<CompiledName("CheckNonNull")>]
    let inline checkNonNull< ^T when ^T : not struct> argName (value : ^T) =
        if isNull value then
            nullArg argName

    /// Raises a System.Collections.Generic.KeyNotFoundException.
    [<CompiledName("RaiseKeyNotFoundException")>]
    let inline keyNotFound (msg : string) =
        raise <| System.Collections.Generic.KeyNotFoundException msg

    /// Not-AND (NAND) of two boolean values.
    /// Returns false when both values are 'true'; otherwise, returns true.
    [<CompiledName("Nand")>]
    let inline nand (p : bool) (q : bool) =
        not (p && q)

    /// Not-OR (NOR) of two boolean values.
    /// Returns true when both values are 'false'; otherwise, returns false.
    [<CompiledName("Nor")>]
    let inline nor (p : bool) (q : bool) =
        not (p && q)

    /// Exclusive-or (XOR) of two boolean values.
    [<CompiledName("Xor")>]
    let inline xor (p : bool) (q : bool) =
        // OPTIMIZE : Use inline IL to emit a 'xor' instead of 'ceq, ldc.i4.0, ceq'
        p <> q

    /// If-and-only-if (XNOR) of two boolean values.
    /// Also known as the logical biconditional.
    [<CompiledName("Xnor")>]
    let inline xnor (p : bool) (q : bool) =
        // OPTIMIZE : Use inline IL to emit 'xor, not' instead of 'ceq, ldc.i4.0, ceq, not'
        not (p <> q)    

    /// Attempt to execute the function as a mutual-exclusion region using
    /// the input value as a lock. If the lock cannot be entered within a specified
    /// period of time, the attempt is abandoned and the function returns None.
    [<CompiledName("TryLock")>]
    let inline tryLock (timeout : System.TimeSpan) (lockObject : 'Lock) (action : unit -> 'T) : 'T option =
        if System.Threading.Monitor.TryEnter (lockObject, timeout) then
            try Some <| action ()
            finally
                System.Threading.Monitor.Exit lockObject
        else None

    /// Intercepts a value within a pipeline. The value is applied to a given
    /// function, then returned so it can continue through the pipeline.
    /// This function is primarily useful for debugging pipelines.
    [<CompiledName("Tap")>]
    let tap (action : 'T -> unit) (value : 'T) =
        action value
        value

(*
#if PROTO_COMPILER
    /// Returns the RuntimeTypeHandle of the specified type.
    [<NoDynamicInvocation>]
    let inline typehandleof<'T> =
        let tok = (# "ldtoken !0" type('T) : System.RuntimeTypeHandle #)
        tok
#endif
*)

/// Functional operators on enumerations.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Enum =
    /// Returns an array of the values defined by an enumeration type.
    [<CompiledName("GetValues")>]
    let values<'Enum, 'T when 'Enum : enum<'T>> () =
        typeof<'Enum>
        |> System.Enum.GetValues
        :?> 'Enum[]

//    //
//    let inline values2 () =
//        values< ^Enum, _> ()

    /// Alias for System.Enum.IsDefined.
    [<CompiledName("IsDefinedImpl")>]
    let inline private isDefinedImpl<'Enum, 'T when 'Enum : enum<'T>> (value : 'Enum) =
        System.Enum.IsDefined (typeof<'Enum>, value)

    /// Indicates whether a constant with the specified value exists in the given enumeration type.
    [<CompiledName("IsDefined")>]
    let inline isDefined value =
        isDefinedImpl< ^Enum, _> value


/// Functional operators on lazily-initialized values.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Lazy =
    /// Forces initialization of a lazily-initialized value (if it has not already
    /// been initialized) then returns the value.
    [<CompiledName("Force")>]
    let inline force (lazyValue : Lazy<'T>) =
        lazyValue.Force ()

    /// Retrieves the value from a lazily-initialized value.
    [<CompiledName("Value")>]
    let inline value (lazyValue : Lazy<'T>) =
        lazyValue.Value

    /// Creates a lazily-initialized value. When the lazy initialization occurs,
    /// the specified function is used to create the value.
    [<CompiledName("Create")>]
    let inline create creator : Lazy<'T> =
        System.Lazy.Create creator

    /// Returns the value of a lazily-initialized value as <c>Some value</c> if it has already
    /// been initialized; otherwise, returns <c>None</c>.
    [<CompiledName("TryGetValue")>]
    let tryGetValue (lazyValue : Lazy<'T>) =
        if lazyValue.IsValueCreated then
            Some lazyValue.Value
        else None


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
    [<CompiledName("Iterate")>]
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


/// Additional functional operators on options.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Option =
    open System
    open System.Runtime.InteropServices

    /// Creates an F# option from an instance of a reference type.
    /// If the reference is null, returns None; otherwise, (Some value).
    [<CompiledName("OfNull")>]
    let inline ofNull (value : 'T) =
        if isNull value then None else Some value

    /// Creates an instance of a type with the 'null' constraint from an F# option value for that type.
    /// If the option value is None, returns 'null'. Otherwise, returns the reference contained in the Some.
    [<CompiledName("ToNull")>]
    let inline toNull (value : 'T option) =
        match value with Some x -> x | None -> null

    /// Creates an F# option from a nullable value.
    [<CompiledName("OfNullable")>]
    let inline ofNullable (arg : Nullable<'T>) =
        if arg.HasValue then Some arg.Value else None

    /// Creates a nullable value from an F# option.
    [<CompiledName("ToNullable")>]
    let inline toNullable (value : 'T option) =
        match value with
        | Some x -> Nullable<_> x
        | None -> Nullable<_> ()

    /// Creates an F# option from a value 'x'.
    /// When the specified condition is true, returns Some x; otherwise, None.
    [<CompiledName("Conditional")>]
    let inline conditional cond value =
        if cond then Some value else None

    //
    [<CompiledName("Condition")>]
    let condition (predicate : 'T -> bool) value =
        if predicate value then Some value else None

    /// Chains two option values together.
    /// If the first value is Some, it is returned; otherwise, the second value is returned.
    /// Similar to the (??) operator in C#.
    [<CompiledName("Coalesce")>]
    let inline coalesce (x : 'T option) (y : 'T option) =
        match x with
        | (Some _) -> x
        | None -> y

    /// <summary>Gets the value of the option if Some, otherwise returns the specified default value.</summary>
    /// <remarks>Identical to the built-in 'defaultArg' operator, but with the arguments swapped.</remarks>
    [<CompiledName("Fill")>]
    let inline fill defaultValue (value : 'T option) =
        defaultArg value defaultValue

    /// <summary>Uses the specified function, if necessary, to create a default value for an option.</summary>
    /// <remarks>Similar to the 'defaultArg' operator -- but 'defaultArg' requires the
    /// default value to already be created, while this method allows for lazy creation.</remarks>
    [<CompiledName("FillWith")>]
    let inline fillWith generator (value : 'T option) =
        match value with
        | Some x -> x
        | None -> generator ()

    //
    [<CompiledName("ToOutAndBool")>]
    let inline toOutAndBool value ([<Out>] outValue : byref<'T>) =
        match value with
        | Some x ->
            outValue <- x
            true
        | None ->
            false

    //
    [<CompiledName("Filter")>]
    let filter (predicate : 'T -> bool) value =
        match value with
        | None -> None
        | Some x ->
            if predicate x then Some x else None

    //
    [<CompiledName("Bind2")>]
    let bind2 (binder : 'T1 -> 'T2 -> 'U option) value1 value2 =
        match value1, value2 with
        | Some x, Some y ->
            binder x y
        | _ ->
            None


/// Additional functional operators on Choice<_,_> values.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Choice =
    /// <summary>
    /// When the choice value is <c>Choice1Of2(x)</c>, returns <c>Choice1Of2 (f x)</c>.
    /// Otherwise, when the choice value is <c>Choice2Of2(x)</c>, returns <c>Choice2Of2(x)</c>. 
    /// </summary>
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'U) (value : Choice<'T, 'Error>) =
        match value with
        | Choice1Of2 result ->
            Choice1Of2 (mapping result)
        | Choice2Of2 error ->
            Choice2Of2 error

    //
    [<CompiledName("MapError")>]
    let mapError (mapping : 'Err1 -> 'Err2) (value : Choice<'T, 'Err1>) =
        match value with
        | Choice1Of2 result ->
            Choice1Of2 result
        | Choice2Of2 error ->
            Choice2Of2 (mapping error)

    //
    [<CompiledName("Bind")>]
    let bind (binding : 'T -> Choice<'U, 'Error>) value =
        match value with
        | Choice1Of2 result ->
            binding result
        | Choice2Of2 error ->
            Choice2Of2 error


/// Extensible printf-style formatting for numbers and other datatypes.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Printf =
    open System.Diagnostics
    open Printf

    /// Print to a System.Text.StringBuilder, adding a newline.
    [<CompiledName("PrintFormatLineToStringBuilder")>]
    let inline bprintfn (buf : System.Text.StringBuilder) fmt : 'T =
        kbprintf (fun _ -> buf.AppendLine () |> ignore) buf fmt

    /// Print formatted string to Debug listeners.
    [<CompiledName("PrintFormatToDebugListeners")>]
    let inline dprintf fmt : 'T =
        ksprintf Debug.Write fmt

    /// Print formatted string to Debug listeners, adding a newline.
    [<CompiledName("PrintFormatLineToDebugListeners")>]
    let inline dprintfn fmt : 'T =
        ksprintf Debug.WriteLine fmt

    /// Print formatted string to Trace listeners.
    [<CompiledName("PrintFormatToTraceListeners")>]
    let inline tprintf fmt : 'T =
        ksprintf Trace.Write fmt

    /// Print formatted string to Trace listeners, adding a newline.
    [<CompiledName("PrintFormatLineToTraceListeners")>]
    let inline tprintfn fmt : 'T =
        ksprintf Trace.WriteLine fmt


/// Helper functions for working with tagged integers.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Tag =
    open LanguagePrimitives

    /// Creates a tagged integer from an integer value.
    [<CompiledName("OfInt")>]
    let inline ofInt<[<Measure>] 'Tag> (value : int) : int<'Tag> =
        Int32WithMeasure value

    /// Removes the tag from a tagged integer, returning just the integer value.
    [<CompiledName("ToInt")>]
    let inline toInt<[<Measure>] 'Tag> (tag : int<'Tag>) : int =
        int tag

