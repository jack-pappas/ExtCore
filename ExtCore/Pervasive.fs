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
open System.Collections
open System.Collections.Generic

(* Type abbreviations *)

/// Represents an object whose underlying type is a value type
/// that can also be assigned null like a reference type.
type nullable<'T when 'T : struct and 'T : (new : unit -> 'T) and 'T :> System.ValueType> = System.Nullable<'T>

/// Represents a generic collection of key/value pairs.
type dict<'Key, 'Value> = System.Collections.Generic.IDictionary<'Key, 'Value>

/// <summary>
/// Array views are similar to array slices, but instead of creating a copy of the
/// 'sliced' elements they simply provide convienient access to some section of the
/// underlying array.
/// </summary>
/// <remarks>
/// Type abbreviation for System.ArraySegment&lt;T&gt;
/// </remarks>
type ArrayView<'T> = System.ArraySegment<'T>


/// Basic F# Operators. This module is automatically opened in all F# code.
[<AutoOpen>]
module Operators =
    (* Type extensions *)

    type System.ArraySegment<'T> with
        member this.Item
            with get index =
                if index < 0 || index >= this.Count then
                    raise <| System.IndexOutOfRangeException ()
                else
                    this.Array.[this.Offset + index]
                
            and set index value =
                if index < 0 || index >= this.Count then
                    raise <| System.IndexOutOfRangeException ()
                else
                    this.Array.[this.Offset + index] <- value

    type System.Collections.Generic.List<'T> with
        /// Implements F# slicing syntax for ResizeArray<'T>.
        member this.GetSlice (startIndex, finishIndex) : ResizeArray<'T> =
            let startIndex = defaultArg startIndex 0
            let finishIndex = defaultArg finishIndex this.Count

            this.GetRange (
                startIndex,
                finishIndex - startIndex + 1)

    (* Operators *)

    /// Reference/Physical-equality operator.
    let inline (===) (x : 'T) y =
        LanguagePrimitives.PhysicalEquality x y

    /// Negated reference/physical-equality operator.
    let inline (!==) (x : 'T) y =
        not (LanguagePrimitives.PhysicalEquality x y)

    /// The opticons ("optional cons") operator.
    let inline (%?) (x : 'T option) list =
        match x with
        | None -> list
        | Some x ->
            x :: list


    (* Simple functions *)

    /// Swaps the values of a tuple so their order is reversed.
    [<CompiledName("Swap")>]
    let inline swap (x : 'T, y : 'U) =
        y, x

    /// Swaps the order of the arguments to a function.
    [<CompiledName("Flip")>]
    let inline flip f (x : 'T) (y : 'U) : 'V =
        f y x

    /// Compares two objects for reference equality.
    [<CompiledName("RefEquals")>]
    let inline refEquals< ^T, ^U when ^T : not struct and ^U : not struct> (x : ^T) (y : ^U) =
        System.Object.ReferenceEquals (x, y)

    /// Determines if a reference is a null reference.
    [<CompiledName("IsNull")>]
    let inline isNull< ^T when ^T : not struct> (x : ^T) =
        System.Object.ReferenceEquals (null, x)

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

    /// Intercepts a value within a pipeline. The value is applied to a given
    /// function, then returned so it can continue through the pipeline.
    /// This function is primarily useful for debugging pipelines.
    [<CompiledName("Tap")>]
    let tap (action : 'T -> unit) (value : 'T) : 'T =
        action value
        value

    /// Creates a 'lazy' value whose value is immediately available; that is,
    /// it does not need to execute a thunk to compute it's value.
    [<CompiledName("NotLazy")>]
    let inline notlazy (value : 'T) =
        Lazy.CreateFromValue value

    /// Combines two predicates using a short-circuiting OR operator.
    [<CompiledName("Orf")>]
    let inline orf f g (x : 'T) =
        f x || g x

    /// Combines two predicates using a short-circuiting AND operator.
    [<CompiledName("Andf")>]
    let inline andf f g (x : 'T) =
        f x && g x

    /// Combines two predicates using the XOR (exclusive-or) operator.
    [<CompiledName("Xorf")>]
    let inline xorf f g (x : 'T) =
        xor (f x) (g x)


    (* General functions *)

    /// <summary>
    /// Applies the specified value to a function which can possibly return an error message.
    /// If the function returns an error message, it is used to invoke <c>Debug.Fail()</c>;
    /// otherwise, the value is returned unchanged. This function is designed for implementing
    /// debugging assertions within a computation 'pipeline'.
    /// </summary>
    [<CompiledName("TapAssert")>]
    let tapAssert (asserter : 'T -> string option) (value : 'T) : 'T =
        match asserter value with
        | None ->
            value
        | Some errorMsg ->
            System.Diagnostics.Debug.Fail errorMsg
            value   // Necessary for type-inference purposes.

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

    /// Applies a mapping function to two (2) values, returning the input value
    /// whose mapped value was the smaller (lesser) of the mapped values.
    [<CompiledName("MinBy")>]
    let inline minBy (mapping : 'T -> 'Key) (x : 'T) (y : 'T) =
        if mapping x <= mapping y then x else y

    /// Applies a mapping function to two (2) values, returning the input value
    /// whose mapped value was the larger (greater) of the mapped values.
    [<CompiledName("MaxBy")>]
    let inline maxBy (mapping : 'T -> 'Key) (x : 'T) (y : 'T) =
        if mapping x < mapping y then y else x

    /// Applies a mapping function to two (2) values, returning the
    /// smaller (lesser) of the resulting values.
    [<CompiledName("MinWith")>]
    let inline minWith (mapping : 'T -> 'U) (x : 'T) (y : 'T) =
        let mapped_x = mapping x
        let mapped_y = mapping y

        if mapped_x <= mapped_y then mapped_x else mapped_y

    /// Applies a mapping function to two (2) values, returning the
    /// larger (greater) of the resulting values.
    [<CompiledName("MaxWith")>]
    let inline maxWith (mapping : 'T -> 'U) (x : 'T) (y : 'T) =
        let mapped_x = mapping x
        let mapped_y = mapping y

        if mapped_x < mapped_y then mapped_y else mapped_x

    (*
    #if PROTO_COMPILER
    /// Returns the RuntimeTypeHandle of the specified type.
    [<NoDynamicInvocation>]
    let inline typehandleof<'T> : System.RuntimeTypeHandle =
        let tok = (# "ldtoken !0" type('T) : System.RuntimeTypeHandle #)
        tok
    #endif
    *)

    (* Exception-related functions *)

    /// Raises a new exception of the specified type.
    [<CompiledName("RaiseNew")>]
    let inline raiseNew<'T when 'T :> exn and 'T : (new : unit -> 'T)> () : 'T =
        raise <| new 'T()

    /// Raises a System.NotImplementedException.
    [<CompiledName("RaiseNotImplementedException")>]
    let inline notImpl msg : 'T =
        raise <| System.NotImplementedException msg

    /// Raises a System.NotSupportedException.
    [<CompiledName("RaiseNotSupportedException")>]
    let inline notSupported msg : 'T =
        raise <| System.NotSupportedException msg

    /// Raises a System.ArgumentOutOfRangeException.
    [<CompiledName("RaiseArgumentOutOfRangeException")>]
    let inline argOutOfRange (paramName : string) (message : string) : 'T =
        raise <| System.ArgumentOutOfRangeException (paramName, message)    

    /// Determines if a reference is a null reference, and if it is, throws an ArgumentNullException.
    [<CompiledName("CheckNonNull")>]
    let inline checkNonNull< ^T when ^T : not struct> argName (value : ^T) =
        if isNull value then
            nullArg argName

    /// Raises a System.Collections.Generic.KeyNotFoundException.
    [<CompiledName("RaiseKeyNotFoundException")>]
    let inline keyNotFound (msg : string) : 'T =
        raise <| System.Collections.Generic.KeyNotFoundException msg

    (* Active Patterns *)

    /// Classifies a Choice`2 value as a successful result or an error.
    [<CompiledName("SuccessOrErrorPattern")>]
    let inline (|Success|Error|) (result : Choice<'T, 'Error>) =
        match result with
        | Choice1Of2 res ->
            Success res
        | Choice2Of2 err ->
            Error err

    /// Classifies the result of a comparison.
    [<CompiledName("ComparisonPattern")>]
    let inline (|Less|Equal|Greater|) (comparisonResult : int) =
        match comparisonResult with
        | -1 -> Less
        | 0 -> Equal
        | 1 -> Greater
        | invalid ->
            let msg = sprintf "Invalid comparison value. Comparison operations must return -1, 0, or 1. (Value = %i)" invalid
            invalidArg "comparisonResult" msg


/// Functional operators on enumerations.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Enum =
    /// Determines whether one or more bit fields are set in the specified enum value.
    [<CompiledName("HasFlag")>]
    let inline hasFlag<'Enum when 'Enum : struct and 'Enum : unmanaged and 'Enum :> System.Enum>
            (flag : 'Enum) (value : 'Enum) : bool =
        value.HasFlag flag

    /// Returns an array of the values defined by an enumeration type.
    [<CompiledName("Values")>]
    let values<'Enum when 'Enum : struct and 'Enum : unmanaged and 'Enum :> System.Enum> =
        System.Enum.GetValues typeof<'Enum> :?> 'Enum[]

    /// Indicates whether a constant with the specified value exists in the given enumeration type.
    [<CompiledName("IsDefined")>]
    let inline isDefined<'Enum when 'Enum : struct and 'Enum : unmanaged and 'Enum :> System.Enum>
            (value : 'Enum) : bool =
        System.Enum.IsDefined (typeof<'Enum>, value)


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

    /// <summary>
    /// Creates a lazily-initialized value which is immediately initialized to the
    /// given value. In other words, the Lazy&lt;'T&gt; value returned by this function
    /// will not need to execute a thunk when forced -- it can just return the value
    /// it was initialized with.
    /// </summary>
    [<CompiledName("Init")>]
    let inline init value : Lazy<'T> =
        System.Lazy.CreateFromValue value

    /// Returns the value of a lazily-initialized value as <c>Some value</c> if it has already
    /// been initialized; otherwise, returns <c>None</c>.
    [<CompiledName("TryGetValue")>]
    let tryGetValue (lazyValue : Lazy<'T>) =
        if lazyValue.IsValueCreated then
            Some lazyValue.Value
        else None

    /// Transforms a lazily-initialized value by it to the given mapping function.
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'U) (lazyValue : Lazy<'T>) : Lazy<'U> =
        // If the value has already been created, perform the mapping
        // 'eagerly' for better performance.
        if lazyValue.IsValueCreated then
            mapping lazyValue.Value
            |> System.Lazy.CreateFromValue
        else
            lazy (mapping <| lazyValue.Force ())


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

    /// Applies a predicate function to the given value, returning <c>Some(value)</c>
    /// when the predicate returns 'true' and <c>None</c> otherwise.
    [<CompiledName("Condition")>]
    let condition (predicate : 'T -> bool) value =
        if predicate value then Some value else None

    /// <summary>
    /// Chains two option values together.
    /// If the first value is Some, it is returned; otherwise, the second value is returned.
    /// </summary>
    /// <remarks>
    /// Similar to the (??) operator in C#.
    /// </remarks>
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

    /// Uses the specified function, if necessary, to attempt to create a default value for an option.
    [<CompiledName("TryFillWith")>]
    let inline tryFillWith generator (value : 'T option) =
        match value with
        | Some _ -> value
        | None -> generator ()

    //
    [<CompiledName("Attempt")>]
    let attempt generator : 'T option =
        try Some <| generator ()
        with _ -> None

    //
    [<CompiledName("ToOutAndBool")>]
    let inline toOutAndBool (value, [<Out>] outValue : byref<'T>) : bool =
        match value with
        | Some x ->
            outValue <- x
            true
        | None ->
            false

    /// Filters a option value by applying the given predicate function to the value it
    /// contains (if any).
    [<CompiledName("Filter")>]
    let filter (predicate : 'T -> bool) value =
        match value with
        | None -> None
        | Some x ->
            if predicate x then Some x else None

    /// Applies the specified function to two (2) option values when both values are Some.
    /// Otherwise, returns None.
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
    //
    [<CompiledName("IsResult")>]
    let inline isResult (choice : Choice<'T, 'Error>) : bool =
        match choice with
        | Choice1Of2 _ -> true
        | Choice2Of2 _ -> false

    //
    [<CompiledName("IsError")>]
    let inline isError (choice : Choice<'T, 'Error>) : bool =
        match choice with
        | Choice1Of2 _ -> false
        | Choice2Of2 _ -> true

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

    /// Applies the specified mapping function to a choice value representing an error value
    /// (Choice2Of2). If the choice value represents a result value (Choice1Of2), the result value
    /// is passed through without modification.
    [<CompiledName("MapError")>]
    let mapError (mapping : 'Err1 -> 'Err2) (value : Choice<'T, 'Err1>) =
        match value with
        | Choice1Of2 result ->
            Choice1Of2 result
        | Choice2Of2 error ->
            Choice2Of2 (mapping error)

    /// Applies the specified binding function to a choice value representing a result value
    /// (Choice1Of2). If the choice value represents an error value (Choice2Of2), the error value
    /// is passed through without modification.
    [<CompiledName("Bind")>]
    let bind (binding : 'T -> Choice<'U, 'Error>) value =
        match value with
        | Choice1Of2 result ->
            binding result
        | Choice2Of2 error ->
            Choice2Of2 error

    //
    [<CompiledName("Exists")>]
    let exists (predicate : 'T -> bool) (value : Choice<'T, 'Error>) : bool =
        match value with
        | Choice1Of2 result ->
            predicate result
        | Choice2Of2 _ ->
            false

    //
    [<CompiledName("Forall")>]
    let forall (predicate : 'T -> bool) (value : Choice<'T, 'Error>) : bool =
        match value with
        | Choice1Of2 result ->
            predicate result
        | Choice2Of2 _ ->
            true

    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> 'State) (state : 'State) (value : Choice<'T, 'Error>) : 'State =
        match value with
        | Choice1Of2 result ->
            folder state result
        | Choice2Of2 _ ->
            state

    //
    [<CompiledName("FoldBack")>]
    let foldBack (folder : 'T -> 'State -> 'State) (value : Choice<'T, 'Error>) (state : 'State) : 'State =
        match value with
        | Choice1Of2 result ->
            folder result state
        | Choice2Of2 _ ->
            state

    //
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> unit) (value : Choice<'T, 'Error>) : unit =
        match value with
        | Choice2Of2 _ -> ()
        | Choice1Of2 result ->
            action result


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

