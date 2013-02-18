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
    let [<NoDynamicInvocation>] inline swap (x : 'T1, y : 'T2) =
        y, x

    /// Swaps the order of the arguments to a function.
    let [<NoDynamicInvocation>] inline flip f (x : 'T) (y : 'U) =
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

    /// Exclusive-or (XOR) of two boolean values.
    let [<NoDynamicInvocation>] inline xor (p : bool) (q : bool) =
        // OPTIMIZE : Use inline IL to emit a 'xor' instead of 'ceq, ldc.i4.0, ceq'
        p <> q

    /// If-and-only-if (XNOR) of two boolean values.
    /// Also known as the logical biconditional.
    let [<NoDynamicInvocation>] inline iff (p : bool) (q : bool) =
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
    let [<NoDynamicInvocation>] inline concatFast (arr : string[]) =
        System.String.Join (empty, arr)

    /// Creates a new string by joining the specified strings using
    /// an environment-specific newline character sequence.
    let [<NoDynamicInvocation>] inline ofLines (lines : string[]) =
        System.String.Join (System.Environment.NewLine, lines)

    /// Splits a string into individual lines.
    let [<NoDynamicInvocation>] inline toLines (str : string) =
        str.Split ([| '\r'; '\n' |], System.StringSplitOptions.RemoveEmptyEntries)

    /// Creates a new string by removing all leading and trailing
    /// white-space characters from the specified string.
    let [<NoDynamicInvocation>] inline trim (str : string) =
        str.Trim ()

    /// Creates an F# option value from the specified string.
    /// If the string 's' is null or empty, returns None; otherwise, returns Some s.
    let [<NoDynamicInvocation>] inline toOption str =
        if System.String.IsNullOrEmpty str then None else Some str


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


//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module KeyValuePair =
    open System.Collections.Generic

    //
    let [<NoDynamicInvocation>] inline key (kvp : KeyValuePair<'Key, 'T>) =
        kvp.Key

    //
    let [<NoDynamicInvocation>] inline value (kvp : KeyValuePair<'Key, 'T>) =
        kvp.Value

