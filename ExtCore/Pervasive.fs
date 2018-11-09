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


/// <summary>
/// Represents an object whose underlying type is a value type that can also be assigned null like a reference type.
/// </summary>
/// <typeparam name="T"></typeparam>
type nullable<'T when 'T : struct and 'T : (new : unit -> 'T) and 'T :> System.ValueType> = System.Nullable<'T>

/// <summary>Represents a generic collection of key/value pairs.</summary>
/// <typeparam name="Key"></typeparam>
/// <typeparam name="Value"></typeparam>
type dict<'Key, 'Value> = System.Collections.Generic.IDictionary<'Key, 'Value>

/// <summary>
/// Array views are similar to array slices, but instead of creating a copy of the
/// 'sliced' elements they simply provide convienient access to some section of the
/// underlying array.
/// </summary>
/// <typeparam name="T"></typeparam>
/// <remarks>
/// Type abbreviation for System.ArraySegment&lt;T&gt;
/// </remarks>
type ArrayView<'T> = System.ArraySegment<'T>

/// <summary>
/// The type of strings, annotated with a unit of measure. The unit of measure is erased in compiled code and when values
/// of this type are analyzed using reflection. The type is representationally equivalent to <c>System.String</c>.
/// </summary>
[<MeasureAnnotatedAbbreviation>]
type string<[<Measure>] 'Measure> = string

/// <summary>
/// The type of boolean values, annotated with a unit of measure. The unit of measure is erased in compiled code and when values
/// of this type are analyzed using reflection. The type is representationally equivalent to <c>System.Boolean</c>.
/// </summary>
[<MeasureAnnotatedAbbreviation>]
type bool<[<Measure>] 'Measure> = bool

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
        /// <summary>Implements F# slicing syntax for ResizeArray<'T>.</summary>
        /// <param name="startIndex"></param>
        /// <param name="finishIndex"></param>
        /// <returns></returns>
        member this.GetSlice (startIndex, finishIndex) : ResizeArray<'T> =
            let startIndex = defaultArg startIndex 0
            let finishIndex = defaultArg finishIndex this.Count

            this.GetRange (
                startIndex,
                finishIndex - startIndex + 1)

    (* Operators *)

    /// <summary>Reference (physical) equality.</summary>
    /// <param name="x">The first parameter.</param>
    /// <param name="y">The second parameter.</param>
    /// <returns></returns>
    //[<Obsolete("This operator will be removed in a future release. Use the (===) operator instead.")>]
    let inline (==) (x : 'T) y =
        LanguagePrimitives.PhysicalEquality x y

    /// <summary>Reference (physical) equality.</summary>
    /// <param name="x">The first parameter.</param>
    /// <param name="y">The second parameter.</param>
    /// <returns></returns>
    [<Obsolete("This operator will be removed in a future release. Use the (==) operator instead.")>]
    let inline (===) (x : 'T) y =
        LanguagePrimitives.PhysicalEquality x y

    /// Negated reference/physical-equality operator.
    [<Obsolete("This operator will be removed in a future release. Use the (===) operator and the 'not' function instead.")>]
    let inline (!==) (x : 'T) y =
        not (LanguagePrimitives.PhysicalEquality x y)

    /// <summary>The opticons ("optional cons") operator.</summary>
    /// <param name="x"></param>
    /// <param name="list"></param>
    /// <returns></returns>
    let inline (%?) (x : 'T option) list =
        match x with
        | None -> list
        | Some x ->
            x :: list


    (* Simple functions *)

    /// <summary>Swaps the values of a tuple so their order is reversed.</summary>
    /// <param name="x"></param>
    /// <param name="y"></param>
    /// <returns></returns>
    [<CompiledName("Swap")>]
    let inline swap (x : 'T, y : 'U) =
        y, x

    /// <summary>Swaps the order of the arguments to a function.</summary>
    /// <param name="x"></param>
    /// <param name="y"></param>
    /// <returns></returns>
    [<CompiledName("Flip")>]
    let inline flip f (x : 'T) (y : 'U) : 'V =
        f y x

    /// <summary>Compares two objects for reference equality.</summary>
    /// <param name="x"></param>
    /// <param name="y"></param>
    /// <returns></returns>
    [<CompiledName("RefEquals")>]
    [<Obsolete("This function is redundant and will be removed in a future release. Use the (==) operator instead.")>]
    let inline refEquals<'T, 'U when 'T : not struct and 'U : not struct> (x : 'T) (y : 'U) =
        System.Object.ReferenceEquals (x, y)

    /// <summary>Determines if a reference is a null reference.</summary>
    /// <param name="arg"></param>
    /// <returns></returns>
    [<CompiledName("IsNull")>]
    let inline isNull<'T when 'T : not struct> (arg : 'T) =
        // OPTIMIZE :   Implement with inline IL (ldnull, ldarg.0, ceq). We can't use LanguagePrimitives.PhysicalEquality because it
        //              requires the 'null' constraint which we don't want to require for this function.
        System.Object.ReferenceEquals (null, arg)

    /// <summary>Not-AND (NAND) of two boolean values.</summary>
    /// <param name="p"></param>
    /// <param name="q"></param>
    /// <returns><c>false</c> when both values are <c>true</c>; otherwise, returns <c>true</c>.</returns>
    [<CompiledName("Nand")>]
    let inline nand (p : bool) (q : bool) =
        not (p && q)

    /// <summary>Not-OR (NOR) of two boolean values.</summary>
    /// <param name="p"></param>
    /// <param name="q"></param>
    /// <returns><c>true</c> when both values are <c>false</c>; otherwise, returns <c>false</c>.</returns>
    [<CompiledName("Nor")>]
    let inline nor (p : bool) (q : bool) =
        not (p || q)

    /// <summary>Exclusive-or (XOR) of two boolean values.</summary>
    /// <param name="p"></param>
    /// <param name="q"></param>
    /// <returns></returns>
    [<CompiledName("Xor")>]
    let inline xor (p : bool) (q : bool) =
        // OPTIMIZE : Use inline IL to emit a 'xor' instead of 'ceq, ldc.i4.0, ceq'
        p <> q

    /// <summary>If-and-only-if (XNOR) of two boolean values.</summary>
    /// <param name="p"></param>
    /// <param name="q"></param>
    /// <returns></returns>
    /// <remarks>Also known as the logical biconditional.</remarks>
    [<CompiledName("Xnor")>]
    let inline xnor (p : bool) (q : bool) =
        // OPTIMIZE : Use inline IL to emit 'xor, not' instead of 'ceq, ldc.i4.0, ceq, not'
        not (p <> q)

    /// <summary>
    /// Intercepts a value within a pipeline. The value is applied to a given function, then returned so it can
    /// continue through the pipeline. This function is primarily useful for debugging pipelines.
    /// </summary>
    /// <param name="arg1"></param>
    /// <param name="arg2"></param>
    /// <param name="arg3"></param>
    /// <param name="arg4"></param>
    /// <returns></returns>
    [<CompiledName("Tap")>]
    let tap (action : 'T -> unit) (value : 'T) : 'T =
        action value
        value

    /// <summary>
    /// Creates a 'lazy' value whose value is immediately available;
    /// that is, it does not need to execute a thunk to compute it's value.
    /// </summary>
    [<CompiledName("NotLazy")>]
    let inline notlazy (value : 'T) =
        let result = Lazy<'T>.CreateFromValue value
        result.Force () |> ignore
        result

    /// <summary>Combines two predicates using a short-circuiting OR operator.</summary>
    [<CompiledName("Orf")>]
    let inline orf f g (x : 'T) =
        f x || g x

    /// <summary>Combines two predicates using a short-circuiting AND operator.</summary>
    [<CompiledName("Andf")>]
    let inline andf f g (x : 'T) =
        f x && g x

    /// <summary>Combines two predicates using the XOR (exclusive-or) operator.</summary>
    [<CompiledName("Xorf")>]
    let inline xorf f g (x : 'T) =
        xor (f x) (g x)


    (* General functions *)

    /// <summary>
    /// Applies the specified value to a function which can possibly return an error message.
    /// If the function returns an error message, it is used to invoke <see cref="Debug.Fail"/>;
    /// otherwise, the value is returned unchanged.
    /// This function is designed for implementing debugging assertions within a computation 'pipeline'.
    /// </summary>
    [<CompiledName("TapAssert")>]
    let tapAssert (asserter : 'T -> string option) (value : 'T) : 'T =
        match asserter value with
        | None -> ()
        | Some errorMsg ->
            #if FX_SIMPLE_DIAGNOSTICS
            System.Diagnostics.Debug.Assert (false, errorMsg)
            #else
            System.Diagnostics.Debug.Fail errorMsg
            #endif

        // Return the value without modifying it.
        // This should only be returned if the asserter function returned None.
        value

    /// <summary>
    /// Attempt to execute the function as a mutual-exclusion region using the input value as a lock. If the lock cannot be entered
    /// within a specified period of time, the attempt is abandoned and the function returns <c>None</c>.
    /// </summary>
    [<CompiledName("TryLock")>]
    let inline tryLock (timeout : System.TimeSpan) (lockObject : 'Lock) (action : unit -> 'T) : 'T option =
        if System.Threading.Monitor.TryEnter (lockObject, timeout) then
            try Some <| action ()
            finally
                System.Threading.Monitor.Exit lockObject
        else None

    /// <summary>
    /// Applies a mapping function to two (2) values, returning the input value
    /// whose mapped value was the smaller (lesser) of the mapped values.
    /// </summary>
    [<CompiledName("MinBy")>]
    let inline minBy (mapping : 'T -> 'Key) (x : 'T) (y : 'T) =
        if mapping x <= mapping y then x else y

    /// <summary>
    /// Applies a mapping function to two (2) values, returning the input value
    /// whose mapped value was the larger (greater) of the mapped values.
    /// </summary>
    [<CompiledName("MaxBy")>]
    let inline maxBy (mapping : 'T -> 'Key) (x : 'T) (y : 'T) =
        if mapping x < mapping y then y else x

    /// <summary>Applies a mapping function to two (2) values, returning the smaller (lesser) of the resulting values.</summary>
    [<CompiledName("MinWith")>]
    let inline minWith (mapping : 'T -> 'U) (x : 'T) (y : 'T) =
        let mapped_x = mapping x
        let mapped_y = mapping y

        if mapped_x <= mapped_y then mapped_x else mapped_y

    /// <summary>Applies a mapping function to two (2) values, returning the larger (greater) of the resulting values.</summary>
    [<CompiledName("MaxWith")>]
    let inline maxWith (mapping : 'T -> 'U) (x : 'T) (y : 'T) =
        let mapped_x = mapping x
        let mapped_y = mapping y

        if mapped_x < mapped_y then mapped_y else mapped_x

    #if PROTO_COMPILER
(*
    /// Returns the RuntimeTypeHandle of the specified type.
    [<RequiresExplicitTypeArguments>]
    [<CompiledName("TypeHandleOf")>]
    let inline typehandleof<'T> = (# "ldtoken !0" type('T) : System.RuntimeTypeHandle #)
*)
    #endif

    (* Exception-related functions *)

    /// <summary>Raises a new exception of the specified type.</summary>
    /// <typeparam name="E">The type of exception to raise.</typeparam>
    /// <typeparam name="T"></typeparam>
    [<RequiresExplicitTypeArguments>]
    [<CompiledName("RaiseNew")>]
    let inline raiseNew<'E, 'T when 'E :> exn and 'E : (new : unit -> 'E)> () : 'T =
        raise <| new 'E()

    /// <summary>Raises a <see cref="System.NotImplementedException"/>.</summary>
    /// <param name="message">The exception message.</param>
    [<CompiledName("RaiseNotImplementedException")>]
    let inline notImpl message : 'T =
        if System.String.IsNullOrEmpty message then
            raise <| System.NotImplementedException ()
        else
            raise <| System.NotImplementedException message

    /// <summary>Raises a <see cref="System.NotSupportedException"/>.</summary>
    /// <param name="message">The exception message.</param>
    [<CompiledName("RaiseNotSupportedException")>]
    let inline notSupported message : 'T =
        if System.String.IsNullOrEmpty message then
            raise <| System.NotSupportedException ()
        else
            raise <| System.NotSupportedException message

    /// <summary>Raises an <see cref="System.ArgumentOutOfRangeException"/>.</summary>
    /// <param name="paramName">The name of the parameter that causes this exception.</param>
    /// <param name="message">The exception message.</param>
    [<CompiledName("RaiseArgumentOutOfRangeException")>]
    let argOutOfRange (paramName : string) (message : string) : 'T =
        match System.String.IsNullOrEmpty paramName, System.String.IsNullOrEmpty message with
        | false, false ->
            raise <| System.ArgumentOutOfRangeException (paramName, message)
        | false, true ->
            raise <| System.ArgumentOutOfRangeException (paramName)
        | true, true ->
            raise <| System.ArgumentOutOfRangeException ()
        | true, false ->
            raise <| System.ArgumentOutOfRangeException ("(Unspecified parameter)", message)

    /// <summary>Raises a <see cref="System.Collections.Generic.KeyNotFoundException"/>.</summary>
    /// <param name="message">The exception message.</param>
    [<CompiledName("RaiseKeyNotFoundException")>]
    let keyNotFound (message : string) : 'T =
        if System.String.IsNullOrEmpty message then
            raise <| System.Collections.Generic.KeyNotFoundException ()
        else
            raise <| System.Collections.Generic.KeyNotFoundException message

    /// <summary>
    /// Determines if a reference is a null reference, and if it is, throws an <see cref="System.ArgumentNullException"/>.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    /// <param name="paramName">The name of the parameter that causes this exception.</param>
    /// <param name="arg">The reference to check.</param>
    [<CompiledName("CheckNonNull")>]
    let inline checkNonNull<'T when 'T : not struct> paramName (arg : 'T) =
        if isNull arg then
            if System.String.IsNullOrWhiteSpace paramName then
                raise <| System.ArgumentNullException ()
            else
                raise <| System.ArgumentNullException paramName

    #if PROTO_COMPILER
    /// <summary>Checks whether a double-precision floating-point value is a
    /// finite real number and raises an exception if it is not.</summary>
    /// <param name="value">The value to check.</param>
    /// <returns>The unmodified input value, if it is a finite real number.</summary>
    /// <exception cref="T:System.ArithmeticException"><paramref name="value"/> is a <c>NaN</c> or an infinity</exception>
    [<CompiledName("CheckFiniteDouble")>]
    let inline ckfinite (x : float) = (# "ckfinite" x : float #)

    /// <summary>Checks whether a single-precision floating-point value is a
    /// finite real number and raises an exception if it is not.</summary>
    /// <param name="value">The value to check.</param>
    /// <returns>The unmodified input value, if it is a finite real number.</summary>
    /// <exception cref="T:System.ArithmeticException"><paramref name="value"/> is a <c>NaN</c> or an infinity</exception>
    [<CompiledName("CheckFiniteSingle")>]
    let inline ckfinitef (x : float32) = (# "ckfinite" x : float32 #)
    #endif

    (* Active Patterns *)

    /// <summary>Classifies the result of a comparison.</summary>
    /// <param name="comparisonResult"></param>
    /// <returns></returns>
    // TODO : For grammatical consistency, rename the patterns to (|LessThan|Equal|GreaterThan|).
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
    /// <summary>Determines whether one or more bit fields are set in the specified enum value.</summary>
    /// <typeparam name="Enum"></typeparam>
    /// <param name="flag"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("HasFlag")>]
    let inline hasFlag<'Enum when 'Enum : struct and 'Enum : unmanaged and 'Enum :> System.Enum>
            (flag : 'Enum) (value : 'Enum) : bool =
        value.HasFlag flag

    /// <summary>Contains an array of the values defined by an enumeration type.</summary>
    /// <typeparam name="Enum"></typeparam>
    /// <returns></returns>
    // TODO : Modify this to wrap the array in a vector so the array can't accidentally be mutated.
    [<CompiledName("Values")>]
    let values<'Enum when 'Enum : struct and 'Enum : unmanaged and 'Enum :> System.Enum> =
        System.Enum.GetValues typeof<'Enum> :?> 'Enum[]

    /// <summary>Indicates whether a constant with the specified value exists in the given enumeration type.</summary>
    /// <typeparam name="Enum"></typeparam>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("IsDefined")>]
    let inline isDefined<'Enum when 'Enum : struct and 'Enum : unmanaged and 'Enum :> System.Enum>
            (value : 'Enum) : bool =
        System.Enum.IsDefined (typeof<'Enum>, value)


/// Functional operators on lazily-initialized values.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Lazy =
    open System.Threading
    open System.Threading.Tasks

    /// <summary>
    /// Forces initialization of a lazily-initialized value (if it has not already been initialized) then returns the value.
    /// </summary>
    /// <param name="lazyValue"></param>
    /// <returns></returns>
    [<CompiledName("Force")>]
    let inline force (lazyValue : Lazy<'T>) =
        lazyValue.Force ()

    /// <summary>Retrieves the value from a lazily-initialized value.</summary>
    /// <param name="lazyValue"></param>
    /// <returns></returns>
    [<CompiledName("Value")>]
    let inline value (lazyValue : Lazy<'T>) =
        lazyValue.Value

    /// <summary>
    /// Creates a lazily-initialized value. When the lazy initialization occurs, the specified function is used to create the value.
    /// </summary>
    /// <param name="creator"></param>
    /// <returns></returns>
    [<CompiledName("Create")>]
    let inline create creator : Lazy<'T> =
        Lazy<'T>.Create creator

    /// <summary>
    /// Creates a lazily-initialized value which is immediately initialized to the given value. In other words, the
    /// <see cref="Lazy`1"/> value returned by this function will not need to execute a thunk when forced -- it can
    /// just return the value it was initialized with.
    /// </summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Init")>]
    let inline init value : Lazy<'T> =
        Lazy<'T>.CreateFromValue value

    /// <summary>
    /// Returns the value of a lazily-initialized value as <c>Some(value)</c> if it has already
    /// been initialized; otherwise, returns <c>None</c>.
    /// </summary>
    /// <param name="lazyValue"></param>
    /// <returns></returns>
    [<CompiledName("TryGetValue")>]
    let tryGetValue (lazyValue : Lazy<'T>) =
        // Preconditions
        checkNonNull "lazyValue" lazyValue

        if lazyValue.IsValueCreated then
            Some lazyValue.Value
        else None

    /// <summary>Transforms a lazily-initialized value by applying it to the given mapping function.</summary>
    /// <param name="mapping">A mapping function applied to the lazily-initialized value to produce a new value.</param>
    /// <param name="lazyValue">A lazily-initialized value.</param>
    /// <returns>
    /// The lazily-evaluated result of applying <paramref name="mapping"/> to <paramref name="lazyValue"/>.
    /// </returns>
    /// <c>mapImmediately</c> differs from <c>map</c> in that, if the lazily-initialized value
    /// has already been evaluated, the mapping function is immediately applied to the value.
    /// This distinction matters primarily when the mapping function has side effects (such as raising an exception),
    /// since they will occur at different times depending on whether the value has been initialized.
    /// It is recommended to use <c>map</c> for such cases to ensure side effects are handled uniformly.
    /// </remarks>
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'U) (lazyValue : Lazy<'T>) : Lazy<'U> =
        // Preconditions
        checkNonNull "lazyValue" lazyValue

        lazy (mapping lazyValue.Value)

    /// <summary>
    /// Transforms a lazily-initialized value by applying it to the given mapping function.
    /// </summary>
    /// <param name="mapping">A mapping function applied to the lazily-initialized value to produce a new value.</param>
    /// <param name="lazyValue">A lazily-initialized value.</param>
    /// <returns>
    /// The lazily-evaluated result of applying <paramref name="mapping"/> to <paramref name="lazyValue"/>.
    /// </returns>
    /// <remarks>
    /// <c>mapImmediately</c> differs from <c>map</c> in that, if the lazily-initialized value
    /// has already been evaluated, the mapping function is immediately applied to the value.
    /// This distinction matters primarily when the mapping function has side effects (such as raising an exception),
    /// since they will occur at different times depending on whether the value has been initialized.
    /// It is recommended to use <c>map</c> for such cases to ensure side effects are handled uniformly.
    /// </remarks>
    [<CompiledName("MapImmediately")>]
    let mapImmediately (mapping : 'T -> 'U) (lazyValue : Lazy<'T>) : Lazy<'U> =
        // Preconditions
        checkNonNull "lazyValue" lazyValue

        // Apply 'mapping' eagerly if the value has already been initialized.
        if lazyValue.IsValueCreated then
            lazyValue.Value
            |> mapping
            |> Lazy<'T>.CreateFromValue
        else
            lazy (mapping lazyValue.Value)

    /// <summary>Transforms two (2) lazily-initialized values by applying them to the given mapping function.</summary>
    /// <param name="mapping"></param>
    /// <param name="lazyValue1"></param>
    /// <param name="lazyValue2"></param>
    /// <returns></returns>
    [<CompiledName("Map2")>]
    let map2 (mapping : 'T1 -> 'T2 -> 'U) (lazyValue1 : Lazy<'T1>) (lazyValue2 : Lazy<'T2>) : Lazy<'U> =
        // Preconditions
        checkNonNull "lazyValue1" lazyValue1
        checkNonNull "lazyValue2" lazyValue2

        lazy (mapping lazyValue1.Value lazyValue2.Value)

    /// <summary>Transforms two (2) lazily-initialized values by applying them to the given mapping function.</summary>
    /// <param name="mapping"></param>
    /// <param name="lazyValue1"></param>
    /// <param name="lazyValue2"></param>
    /// <returns></returns>
    [<CompiledName("MapImmediately2")>]
    let mapImmediately2 (mapping : 'T1 -> 'T2 -> 'U) (lazyValue1 : Lazy<'T1>) (lazyValue2 : Lazy<'T2>) : Lazy<'U> =
        // Preconditions
        checkNonNull "lazyValue1" lazyValue1
        checkNonNull "lazyValue2" lazyValue2

        // If both values have already been created, perform the mapping
        // 'eagerly' for better performance (e.g., by avoiding the thunk).
        if lazyValue1.IsValueCreated && lazyValue2.IsValueCreated then
            mapping lazyValue1.Value lazyValue2.Value
            |> Lazy<'T>.CreateFromValue
        else
            lazy (mapping lazyValue1.Value lazyValue2.Value)

    /// <summary>Transforms three (3) lazily-initialized values by applying them to the given mapping function.</summary>
    /// <param name="mapping"></param>
    /// <param name="lazyValue1"></param>
    /// <param name="lazyValue2"></param>
    /// <param name="lazyValue3"></param>
    /// <returns></returns>
    [<CompiledName("Map3")>]
    let map3 (mapping : 'T1 -> 'T2 -> 'T3 -> 'U) (lazyValue1 : Lazy<'T1>) (lazyValue2 : Lazy<'T2>) (lazyValue3 : Lazy<'T3>)
        : Lazy<'U> =
        // Preconditions
        checkNonNull "lazyValue1" lazyValue1
        checkNonNull "lazyValue2" lazyValue2
        checkNonNull "lazyValue3" lazyValue3

        lazy (mapping lazyValue1.Value lazyValue2.Value lazyValue3.Value)

    /// <summary>Transforms three (3) lazily-initialized values by applying them to the given mapping function.</summary>
    /// <param name="mapping"></param>
    /// <param name="lazyValue1"></param>
    /// <param name="lazyValue2"></param>
    /// <param name="lazyValue3"></param>
    /// <returns></returns>
    [<CompiledName("MapImmediately3")>]
    let mapImmediately3 (mapping : 'T1 -> 'T2 -> 'T3 -> 'U) (lazyValue1 : Lazy<'T1>) (lazyValue2 : Lazy<'T2>) (lazyValue3 : Lazy<'T3>)
        : Lazy<'U> =
        // Preconditions
        checkNonNull "lazyValue1" lazyValue1
        checkNonNull "lazyValue2" lazyValue2
        checkNonNull "lazyValue3" lazyValue3

        // If all values have already been created, perform the mapping
        // 'eagerly' for better performance (e.g., by avoiding the thunk).
        if lazyValue1.IsValueCreated && lazyValue2.IsValueCreated && lazyValue3.IsValueCreated then
            mapping lazyValue1.Value lazyValue2.Value lazyValue3.Value
            |> Lazy<'T>.CreateFromValue
        else
            lazy (mapping lazyValue1.Value lazyValue2.Value lazyValue3.Value)

    /// <summary></summary>
    /// <param name="binding"></param>
    /// <param name="lazyValue"></param>
    /// <returns></returns>
    [<CompiledName("Bind")>]
    let bind (binding : 'T -> Lazy<'U>) (lazyValue : Lazy<'T>) : Lazy<'U> =
        // Preconditions
        checkNonNull "lazyValue" lazyValue

        (* NOTE :   At first glance, it seems like we could check to see if the input lazy value has already been evaluated,
                    and if so, optimize by immediately applying the it's value to the binding function. However, this is *NOT*
                    a valid optimization, because it doesn't preserve the expected semantics of this function -- if the binding
                    function has any side effects, they'd occur immediately instead of when the 'lazy' returned by this function
                    was forced. Worse yet, this optimization would make the semantics of this function inconsistent since side
                    effects could potentially occur immediately OR lazily. *)
        Lazy<'T>.Create <| fun () ->
            let result = binding <| lazyValue.Force ()
            result.Force ()

#if FX_NO_THREADPOOL
#else
    /// Callback delegate which forces evaluation of a Lazy<'T>.
    /// Meant to be used with ThreadPool.QueueUserWorkItem.
    let private forceCallback<'T> =
        System.Threading.WaitCallback (fun arg ->
            let lazyValue = arg :?> Lazy<'T>

            // Swallow any exception raised when initializing the value; it'll be cached
            // within the Lazy<_> and re-raised when the value is accessed later.
            try
                // Force the value, ignoring the result
                lazyValue.Force () |> ignore
            with _ -> ())
#endif

    /// <summary>Forces evaluation of a lazily-initalized value in the background, using the .NET ThreadPool.</summary>
    /// <param name="lazyValue"></param>
    /// <returns></returns>
    [<CompiledName("ForceBackground")>]
    let forceBackground (lazyValue : Lazy<'T>) : unit =
        // Preconditions
        checkNonNull "lazyValue" lazyValue

        // Evaluate the lazily-initialized value on a .NET ThreadPool thread.
#if FX_NO_THREADPOOL
        // If the ThreadPool API can't be used directly, use the Task API instead.
        Task.Run (fun () ->
            // Swallow any exception raised when initializing the value; it'll be cached
            // within the Lazy<_> and re-raised when the value is accessed later.
            try
                // Force the value, ignoring the result
                lazyValue.Force () |> ignore
            with _ -> ())
        |> ignore
#else
        // If the callback couldn't be enqueued, raise an exception.
        if not <| ThreadPool.QueueUserWorkItem (forceCallback<'T>, lazyValue) then
            failwith "The lazily-evaluated value could not be forced in the background, \
                      because the evaluation callback could not be enqueued in the .NET TheadPool."
#endif

    /// <summary>
    /// Invokes the specified generator function to create a value in the background using
    /// the .NET ThreadPool, and immediately returns a lazily-initialized value.
    /// </summary>
    /// <param name="creator"></param>
    /// <returns></returns>
    /// <remarks>
    /// When consuming code forces evaluation of this value, it will already be available if the
    /// generator function has finished executing in the background; otherwise, the calling thread
    /// is blocked until the generator finishes executing and the value is available.
    /// </remarks>
    [<CompiledName("Future")>]
    let future (creator : unit -> 'T) : Lazy<'T> =
        // Create a lazy value which uses the specified generator function.
        let lazyValue = Lazy<'T>.Create creator

        // Force initialization of the value in the background.
        forceBackground lazyValue

        // Return the lazy value.
        lazyValue

#if FX_NO_THREADPOOL
#else
    /// Callback delegate which forces evaluation of a Lazy<'T>,
    /// then sets a ManualResetEvent to signal the initialization has completed.
    /// Meant to be used with ThreadPool.QueueUserWorkItem.
    let private tryForceCallback<'T> =
        System.Threading.WaitCallback (fun arg ->
            let lazyValue, initCompleted = arg :?> (Lazy<'T> * ManualResetEvent)
            // Re-bind 'initCompleted' with a 'use' binding so it's disposed of when we're finished here.
            use initCompleted = initCompleted

            // Swallow any exception raised when initializing the value; it'll be cached
            // within the Lazy<_> and re-raised when the value is accessed later.
            try
                // Force the value, ignoring the result
                lazyValue.Force () |> ignore
            with _ -> ()

            // Set the ManualResetEvent to signal that initialization of the value is complete.
            // The return value is ignored here because there's not much we can do if the .Set()
            // operation fails; raising an exception on a ThreadPool thread is generally not a great idea.
            // TODO : We could pass a 'ref' cell into this callback, and use that to pass the .Set() result back.
            initCompleted.Set () |> ignore)
#endif

(* TODO : The 'tryForce' function could be modified to use the Task API instead of directly
          using the ThreadPool API, which would allow it to be included in portable profile builds.
          It's excluded for now to make it easier to get the portable builds working. *)
#if FX_NO_THREADPOOL
#else
    /// <summary>
    /// Forces evaluation of a lazily-initialized value, if necessary.
    /// If the evaluation is completed within the specified timeout period, returns <c>Some x</c>
    /// where <c>x</c> is the initialized value; otherwise, returns None.
    /// </summary>
    /// <param name="timeout"></param>
    /// <param name="lazyValue"></param>
    /// <returns></returns>
    /// <remarks>
    /// If the function returns <c>None</c> because evaluation did not complete in the specified
    /// timeout period, the evaluation function will continue to run in the background on the
    /// .NET ThreadPool until it does complete.
    /// </remarks>
    [<CompiledName("TryForce")>]
    let tryForce (timeout : System.TimeSpan) (lazyValue : Lazy<'T>) : 'T option =
        // Preconditions
        checkNonNull "lazyValue" lazyValue
        if timeout < TimeSpan.Zero then
            argOutOfRange "timeout" "The timeout duration cannot be negative."

        // If the value is already initialized, it can be returned immediately.
        if lazyValue.IsValueCreated then
            Some lazyValue.Value

        // Zero timeouts need to be handled specially.
        elif timeout = TimeSpan.Zero then
            // Return the value if it's available; otherwise, return None.
            tryGetValue lazyValue

        else
            // Force evaluation of the lazy value on a .NET ThreadPool thread.
            // The current (calling) thread is blocked until evaluation is complete or
            // the timeout duration elapses, whichever comes first.

            /// The EventWaitHandle which signals that initialization of the value is complete.
            // NOTE :   'let' is used here instead of 'use' because the WaitHandle is disposed by
            //          the ThreadPool callback; otherwise, if this function times out, an exn
            //          will be raised when the callback tries to call the .Set() method.
            let initCompleted = new ManualResetEvent (false)

            // Evaluate the lazily-initialized value on a .NET ThreadPool thread.
            if not <| ThreadPool.QueueUserWorkItem (tryForceCallback<'T>, (lazyValue, initCompleted)) then
                // Dispose the ManualResetEvent we created here, because it won't
                // be disposed by the callback like it normally is.
                initCompleted.Dispose ()

                // If the callback couldn't be enqueued in the ThreadPool, return None instead of raising an exn.
                // TODO : Determine if this is the best strategy, or if it would be better to raise an exn instead.
                //failwith "The callback to evaluate the lazily-initialized value could not be enqueued in the .NET ThreadPool."
                None
            else
                // Wait for the initialization to complete or the timeout period to elapse.
                if initCompleted.WaitOne timeout then
                    // Get the initialized value and return it.
                    Some lazyValue.Value
                else None
#endif

/// Additional functional operators on options.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Option =
    open System
    open System.Runtime.InteropServices

    /// <summary>
    /// Creates an F# option from an instance of a reference type.
    /// If the reference is <c>null</c>, returns <c>None</c>; otherwise, returns <c>Some(value)</c>.
    /// </summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("OfNull")>]
    let inline ofNull (value : 'T) =
        if isNull value then None else Some value

    /// <summary>
    /// Creates an instance of a type with the <c>null</c> constraint from an F# option value for that type.
    /// If the option value is <c>None</c>, returns <c>null</c>. Otherwise, returns the reference contained in the <c>Some</c>.
    /// </summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("ToNull")>]
    let inline toNull (value : 'T option) =
        match value with Some x -> x | None -> null

    /// <summary>Creates an F# option from a nullable value.</summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("OfNullable")>]
    let inline ofNullable (value : Nullable<'T>) =
        if value.HasValue then Some value.Value else None

    /// <summary>Creates a nullable value from an F# option.</summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("ToNullable")>]
    let inline toNullable (value : 'T option) =
        match value with
        | Some x -> Nullable<_> x
        | None -> Nullable<_> ()

    //
    [<CompiledName("OfString")>]
    let ofString (str : string) : string option =
        if String.IsNullOrEmpty str then None
        else Some str

    /// <summary>
    /// Creates an F# option from a value <c>x</c>.
    /// When the specified condition is <c>true</c>, returns <c>Some(x)</c>; otherwise, <c>None</c>.
    /// </summary>
    /// <param name="condition"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Conditional")>]
    let inline conditional condition value =
        if condition then Some value else None

    /// <summary>
    /// Applies a predicate function to the given value, returning <c>Some(value)</c>
    /// when the predicate returns <c>true</c> and <c>None</c> otherwise.
    /// </summary>
    /// <param name="predicate"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Condition")>]
    let condition (predicate : 'T -> bool) value =
        if predicate value then Some value else None

    /// <summary>
    /// Chains two option values together.
    /// If the first value is <c>Some</c>, it is returned; otherwise, the second value is returned.
    /// </summary>
    /// <param name="x"></param>
    /// <param name="y"></param>
    /// <returns></returns>
    /// <remarks>
    /// Similar to the coalesce (??) operator in C#.
    /// </remarks>
    [<CompiledName("Coalesce")>]
    let inline coalesce (x : 'T option) (y : 'T option) =
        match x with
        | (Some _) -> x
        | None -> y

    /// <summary>Gets the value of the option if the option is <c>Some</c>, otherwise returns the specified default value.</summary>
    /// <param name="defaultValue"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    /// <remarks>Identical to the built-in <see cref="defaultArg"/> operator, except with the arguments swapped.</remarks>
    [<CompiledName("Fill")>]
    let inline fill defaultValue (value : 'T option) =
        defaultArg value defaultValue

    /// <summary>Uses the specified function, if necessary, to create a default value for an option.</summary>
    /// <param name="generator"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    /// <remarks>
    /// This function is similar to the built-in <see cref="defaultArg"/> operator and <see cref="Option.fill"/>;
    /// however, those functions require the default value to be created before they are called, while this function
    /// allows the default value to be created only if it is needed.
    /// </remarks>
    [<CompiledName("FillWith")>]
    let inline fillWith generator (value : 'T option) =
        match value with
        | Some x -> x
        | None -> generator ()

    /// <summary>Uses the specified function, if necessary, to attempt to create a default value for an option.</summary>
    /// <param name="generator"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("TryFillWith")>]
    let inline tryFillWith generator (value : 'T option) =
        match value with
        | Some _ -> value
        | None -> generator ()

    /// <summary>
    /// Invokes the specified generator function to create a value. If the function returns a value <c>res</c>,
    /// this function returns <c>Some(res)</c>. If the function raises an exception, it is caught and ignored,
    /// and <c>None</c> is returned.
    /// </summary>
    /// <param name="generator"></param>
    /// <returns></returns>
    [<CompiledName("Attempt")>]
    let attempt generator : 'T option =
        try Some <| generator ()
        with _ -> None

    /// <summary></summary>
    /// <param name="value"></param>
    /// <param name="outValue"></param>
    /// <returns></returns>
    // TODO : Deprecate this function, and move it into a C#-compatibility project.
    [<CompiledName("ToOutAndBool")>]
    let toOutAndBool (value, [<Out>] outValue : byref<'T>) : bool =
        match value with
        | Some x ->
            outValue <- x
            true
        | None ->
            false

    /// <summary>Filters a option value by applying the given predicate function to the value it contains (if any).</summary>
    /// <param name="predicate"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Filter")>]
    let filter (predicate : 'T -> bool) value =
        match value with
        | None -> None
        | Some x ->
            if predicate x then Some x else None

    /// <summary>
    /// Applies the specified function to two (2) option values when both values are <c>Some</c>. Otherwise, returns <c>None</c>.
    /// </summary>
    /// <param name="binder"></param>
    /// <param name="value1"></param>
    /// <param name="value2"></param>
    /// <returns></returns>
    [<CompiledName("Bind2")>]
    let bind2 (binder : 'T1 -> 'T2 -> 'U option) value1 value2 =
        match value1, value2 with
        | Some x, Some y ->
            binder x y
        | _ ->
            None


/// Extensible printf-style formatting for numbers and other datatypes.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Printf =
    open System.Diagnostics
    open Printf

    /// <summary>Print to a System.Text.StringBuilder, adding a newline.</summary>
    /// <param name="builder"></param>
    /// <param name="format"></param>
    /// <returns></returns>
    [<CompiledName("PrintFormatLineToStringBuilder")>]
    let inline bprintfn (builder : System.Text.StringBuilder) format : 'T =
        kbprintf (fun _ -> builder.AppendLine () |> ignore) builder format

    /// <summary>Print formatted string to Debug listeners, adding a newline.</summary>
    /// <param name="format"></param>
    /// <returns></returns>
    [<CompiledName("PrintFormatLineToDebugListeners")>]
    let inline dprintfn format : 'T =
        ksprintf Debug.WriteLine format

#if FX_SIMPLE_DIAGNOSTICS || NETSTANDARD1_6
#else
    /// <summary>Print formatted string to Debug listeners.</summary>
    /// <param name="format"></param>
    /// <returns></returns>
    [<CompiledName("PrintFormatToDebugListeners")>]
    let inline dprintf format : 'T =
        ksprintf Debug.Write format

    /// <summary>Print formatted string to Trace listeners.</summary>
    /// <param name="format"></param>
    /// <returns></returns>
    [<CompiledName("PrintFormatToTraceListeners")>]
    let inline tprintf format : 'T =
        ksprintf Trace.Write format

    /// <summary>Print formatted string to Trace listeners, adding a newline.</summary>
    /// <param name="format"></param>
    /// <returns></returns>
    [<CompiledName("PrintFormatLineToTraceListeners")>]
    let inline tprintfn format : 'T =
        ksprintf Trace.WriteLine format

#endif
