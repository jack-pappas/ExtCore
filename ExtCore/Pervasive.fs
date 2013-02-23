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
        //
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
    let [<NoDynamicInvocation>] inline (%?) (x : 'T option) lst =
        match x with Some a -> a :: lst | None -> lst

    /// Swaps the values of a tuple so their order is reversed.
    let [<NoDynamicInvocation>] inline swap (x : 'T, y : 'U) =
        y, x

    /// Swaps the order of the arguments to a function.
    let [<NoDynamicInvocation>] inline flip f (x : 'T) (y : 'U) : 'V =
        f y x

    /// Raises a System.NotImplementedException.
    let [<NoDynamicInvocation>] inline notImpl msg =
        raise <| System.NotImplementedException msg

    /// Raises a System.NotSupportedException.
    let [<NoDynamicInvocation>] inline notSupported msg =
        raise <| System.NotSupportedException msg

    /// Raises a System.ArgumentOutOfRangeException.
    let [<NoDynamicInvocation>] inline outOfRangeArg (paramName : string) (message : string) =
        raise <| System.ArgumentOutOfRangeException (paramName, message)

    /// Compares two objects for reference equality.
    let [<NoDynamicInvocation>] inline refEquals< ^T, ^U when ^T : not struct and ^U : not struct> (x : ^T) (y : ^U) =
        System.Object.ReferenceEquals (x, y)

    /// Determines if a reference is a null reference.
    let [<NoDynamicInvocation>] inline isNull< ^T when ^T : not struct> (x : ^T) =
        System.Object.ReferenceEquals (null, x)

    /// Determines if a reference is a null reference, and if it is, throws an ArgumentNullException.
    let [<NoDynamicInvocation>] inline checkNonNull< ^T when ^T : not struct> argName (value : ^T) =
        if isNull value then
            nullArg argName

    /// Not-AND (NAND) of two boolean values.
    /// Returns false when both values are 'true'; otherwise, returns true.
    let [<NoDynamicInvocation>] inline nand (p : bool) (q : bool) =
        not (p && q)

    /// Not-OR (NOR) of two boolean values.
    /// Returns true when both values are 'false'; otherwise, returns false.
    let [<NoDynamicInvocation>] inline nor (p : bool) (q : bool) =
        not (p && q)

    /// Exclusive-or (XOR) of two boolean values.
    let [<NoDynamicInvocation>] inline xor (p : bool) (q : bool) =
        // OPTIMIZE : Use inline IL to emit a 'xor' instead of 'ceq, ldc.i4.0, ceq'
        p <> q

    /// If-and-only-if (XNOR) of two boolean values.
    /// Also known as the logical biconditional.
    let [<NoDynamicInvocation>] inline xnor (p : bool) (q : bool) =
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


/// Functional operators on enumerations.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Enum =
    //
    [<CompiledName("GetValues")>]
    let values<'Enum, 'T when 'Enum : enum<'T>> () =
        typeof<'Enum>
        |> System.Enum.GetValues
        :?> 'Enum[]


/// Functional operators on lazily-initialized values.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Lazy =
    //
    let [<NoDynamicInvocation>] inline force (lazyValue : Lazy<'T>) =
        lazyValue.Force ()

    //
    let [<NoDynamicInvocation>] inline value (lazyValue : Lazy<'T>) =
        lazyValue.Value

    //
    [<CompiledName("TryGetValue")>]
    let tryGetValue (lazyValue : Lazy<'T>) =
        if lazyValue.IsValueCreated then
            Some lazyValue.Value
        else None


/// Additional functional operators on strings.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module String =
    /// The empty string literal.
    let [<Literal>] empty = ""

    /// Indicates whether the specified string is null or empty.
    let [<NoDynamicInvocation>] inline isNullOrEmpty str =
        System.String.IsNullOrEmpty str

    /// Indicates whether the specified string is empty.
    let [<NoDynamicInvocation>] inline isEmpty (str : string) =
        str.Length < 1

    /// <summary>Returns a new string created by concatenating the strings in the specified string array.</summary>
    /// <remarks>For a smaller number (&lt;10) of fairly short strings,
    /// this method is the empirically fastest-known way to concatenate
    /// strings without the overhead of a System.Text.StringBuilder.</remarks>
    let [<NoDynamicInvocation>] inline concatArray (arr : string[]) =
        System.String.Join (empty, arr)

    /// Creates a new string by joining the specified strings using
    /// an environment-specific newline character sequence.
    let [<NoDynamicInvocation>] inline ofLines (lines : string[]) =
        System.String.Join (System.Environment.NewLine, lines)

    /// Splits a string into individual lines.
    let [<NoDynamicInvocation>] inline toLines (str : string) =
        str.Split ([| '\r'; '\n' |], System.StringSplitOptions.RemoveEmptyEntries)

    /// Creates an F# option value from the specified string.
    /// If the string 's' is null or empty, returns None; otherwise, returns Some s.
    let [<NoDynamicInvocation>] inline toOption str =
        if System.String.IsNullOrEmpty str then None else Some str

    /// Creates a new string by removing all leading and
    /// trailing white-space characters from a string.
    let [<NoDynamicInvocation>] inline trim (str : string) =
        str.Trim ()

    /// Removes all leading and trailing occurrences of
    /// the specified characters from a string.
    let [<NoDynamicInvocation>] inline trimChars (chars : char[]) (str : string) =
        str.Trim chars

    //
    let [<NoDynamicInvocation>] inline toArray (str : string) =
        str.ToCharArray ()

    //
    let [<NoDynamicInvocation>] inline ofArray (chars : char[]) =
        System.String (chars)

    //
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

    //
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

    //
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

    - String.Split.iter
    - String.Split.iteri
    - String.Split.fold
    - String.Split.filter
      - These functions should work like .Split(...) |> Array.iter (or Array.fold, etc.),
        except that they won't actually need to traverse the entire string first and split
        it into an array of substrings. Instead, they'll improve performance by allowing us
        to execute a function on each of the substrings in a single forward pass.
      - We might be able to further improve performance by not actually applying the given
        function to the substrings themselves, but instead to some struct representing the
        initial position and length of the substring; this may be enough information for some
        functions (e.g., to filter out strings less than 10 characters), but then we'd easily
        be able to create the substring from that information if necessary.
      - This could be implemented by using the regular vector type / functions from fsharplex.

    *)


/// Additional functional operators on options.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Option =
    open System
    open System.Runtime.InteropServices

    /// Creates an F# option from an instance of a reference type.
    /// If the reference is null, returns None; otherwise, (Some value).
    let [<NoDynamicInvocation>] inline ofNull (value : 'T) =
        if isNull value then None else Some value

    /// Creates an instance of a type with the 'null' constraint from an F# option value for that type.
    /// If the option value is None, returns 'null'. Otherwise, returns the reference contained in the Some.
    let [<NoDynamicInvocation>] inline toNull (value : 'T option) =
        match value with Some x -> x | None -> null

    /// Creates an F# option from a nullable value.
    let [<NoDynamicInvocation>] inline ofNullable (arg : Nullable<'T>) =
        if arg.HasValue then Some arg.Value else None

    /// Creates a nullable value from an F# option.
    let [<NoDynamicInvocation>] inline toNullable (value : 'T option) =
        match value with
        | Some x -> Nullable<_> x
        | None -> Nullable<_> ()

    /// Creates an F# option from a value 'x'.
    /// When the specified condition is true, returns Some x; otherwise, None.
    let [<NoDynamicInvocation>] inline ofCondition cond value =
        if cond then Some value else None

    /// Chains two option values together.
    /// If the first value is Some, it is returned; otherwise, the second value is returned.
    /// Similar to the (??) operator in C#.
    let [<NoDynamicInvocation>] inline coalesce (x : 'T option) (y : 'T option) =
        match x with
        | (Some _) -> x
        | None -> y

    /// <summary>Gets the value of the option if Some, otherwise returns the specified default value.</summary>
    /// <remarks>Identical to the built-in 'defaultArg' operator, but with the arguments swapped.</remarks>
    let [<NoDynamicInvocation>] inline fill defaultValue (value : 'T option) =
        defaultArg value defaultValue

    /// <summary>Uses the specified function, if necessary, to create a default value for an option.</summary>
    /// <remarks>Similar to the 'defaultArg' operator -- but 'defaultArg' requires the
    /// default value to already be created, while this method allows for lazy creation.</remarks>
    let [<NoDynamicInvocation>] inline fillWith generator (value : 'T option) =
        match value with
        | Some x -> x
        | None -> generator ()

    //
    let [<NoDynamicInvocation>] inline toOutAndBool value ([<Out>] outValue : byref<'T>) =
        match value with
        | Some x ->
            outValue <- x
            true
        | None ->
            false

    //
    let ofPredicate (predicate : 'T -> bool) value =
        if predicate value then Some value else None

    //
    let filter (predicate : 'T -> bool) value =
        match value with
        | None -> None
        | Some x ->
            if predicate x then Some x else None

    //
    let bind2 (binder : 'T1 -> 'T2 -> 'U option) value1 value2 =
        match value1, value2 with
        | Some x, Some y ->
            binder x y
        | _ ->
            None


(*
/// Additional functional operators on Choice<_,_> values.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Choice =
    //
    let map (mapping : 'T -> 'U) = function
        | Choice1Of2 result -> Choice1Of2 (mapping result)
        | Choice2Of2 error -> Choice2Of2 error

    //
    let bind (binding : 'T -> Choice<'U, 'Error>) = function
        | Choice1Of2 result -> binding result
        | Choice2Of2 error -> Choice2Of2 error
*)

