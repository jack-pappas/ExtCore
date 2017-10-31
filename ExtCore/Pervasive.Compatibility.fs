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
namespace ExtCore.Compatibility

open System
open ExtCore

/// <summary>A value whose computation has been 'protected' by capturing any raised exception.</summary>
/// <typeparam name="T"></typeparam>
type Protected<'T> = Choice<'T, exn>

/// Basic F# Operators. This module is automatically opened in all F# code.
[<AutoOpen>]
module Operators =
    (* Active Patterns *)
    /// <summary>Classifies a Choice`2 value as a successful result or an error.</summary>
    /// <param name="result"></param>
    /// <returns></returns>
    [<CompiledName("SuccessOrErrorPattern")>]
    let inline (|Success|Error|) (result : Choice<'T, 'Error>) =
        match result with
        | Choice1Of2 res ->
            Success res
        | Choice2Of2 err ->
            Error err
/// Additional functional operators on options.
[<RequireQualifiedAccess>]
module Option =

    /// <summary></summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("OfChoice")>]
    let ofChoice (value : Choice<'T, 'Error>) : 'T option =
        match value with
        | Choice1Of2 result ->
            Some result
        | Choice2Of2 _ ->
            None

    /// <summary></summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("ToChoice")>]
    let toChoice (value : 'T option) : Choice<'T, unit> =
        match value with
        | Some result ->
            Choice1Of2 result
        | None ->
            Choice2Of2 ()

    /// <summary></summary>
    /// <param name="errorValue"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("ToChoiceWith")>]
    let toChoiceWith (errorValue : 'Error) (value : 'T option) : Choice<'T, 'Error> =
        match value with
        | Some result ->
            Choice1Of2 result
        | None ->
            Choice2Of2 errorValue


/// <summary>Additional functional operators on Choice<_,_> values.</summary>
[<RequireQualifiedAccess>]
module Choice =
    /// <summary>Does the Choice value represent a result value?</summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("IsResult")>]
    let inline isResult (value : Choice<'T, 'Error>) : bool =
        // Preconditions
        checkNonNull "value" value

        match value with
        | Choice1Of2 _ -> true
        | Choice2Of2 _ -> false

    /// <summary>Does the Choice value represent an error value?</summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("IsError")>]
    let inline isError (value : Choice<'T, 'Error>) : bool =
        // Preconditions
        checkNonNull "value" value

        match value with
        | Choice1Of2 _ -> false
        | Choice2Of2 _ -> true

    /// <summary>Gets the result value associated with the Choice.</summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Get")>]
    let get (value : Choice<'T, 'Error>) =
        // Preconditions
        checkNonNull "value" value

        match value with
        | Choice1Of2 result ->
            result
        | Choice2Of2 _ ->
            invalidArg "value" "Cannot get the result because the Choice`2 instance is an error value."

    /// <summary>Gets the error value associated with the Choice.</summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("GetError")>]
    let getError (value : Choice<'T, 'Error>) =
        // Preconditions
        checkNonNull "value" value

        match value with
        | Choice1Of2 _ ->
            invalidArg "value" "Cannot get the error because the Choice`2 instance is a result value."
        | Choice2Of2 error ->
            error

    /// <summary>Creates a Choice from a result value.</summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Result")>]
    let inline result value : Choice<'T, 'Error> =
        Choice1Of2 value

    /// <summary>Creates a Choice from an error value.</summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Error")>]
    let inline error value : Choice<'T, 'Error> =
        Choice2Of2 value

    /// <summary>
    /// Creates a Choice representing an error value. The error value in the Choice is the specified error message.
    /// </summary>
    /// <param name="message">The error message.</param>
    /// <returns></returns>
    [<CompiledName("FailWith")>]
    let inline failwith message : Choice<'T, string> =
        Choice2Of2 message

    /// <summary>
    /// Creates a Choice representing an error value. The error value in the Choice is the specified formatted error message.
    /// </summary>
    /// <param name="format"></param>
    /// <returns></returns>
    [<CompiledName("PrintFormatToStringThenFail")>]
    let inline failwithf (format : Printf.StringFormat<'T, Choice<'U, string>>) =
        Printf.ksprintf failwith format

    /// <summary></summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("OfOption")>]
    let ofOption (value : 'T option) : Choice<'T, unit> =
        match value with
        | Some result ->
            Choice1Of2 result
        | None ->
            Choice2Of2 ()

    /// <summary></summary>
    /// <param name="errorValue"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    // TODO :   Rename this to 'ofOptionDefault' or 'ofOptionWithDefault'.
    //          The "With" suffix should be reserved for higher-order functions.
    [<CompiledName("OfOptionWith")>]
    let ofOptionWith (errorValue : 'Error) (value : 'T option) : Choice<'T, 'Error> =
        match value with
        | Some result ->
            Choice1Of2 result
        | None ->
            Choice2Of2 errorValue

    /// <summary></summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("ToOption")>]
    let toOption (value : Choice<'T, 'Error>) : 'T option =
        // Preconditions
        checkNonNull "value" value

        match value with
        | Choice1Of2 result ->
            Some result
        | Choice2Of2 _ ->
            None

    /// <summary>
    /// When the choice value is <c>Choice1Of2(x)</c>, returns <c>Choice1Of2 (f x)</c>.
    /// Otherwise, when the choice value is <c>Choice2Of2(x)</c>, returns <c>Choice2Of2(x)</c>.
    /// </summary>
    /// <param name="mapping"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'U) (value : Choice<'T, 'Error>) =
        // Preconditions
        checkNonNull "value" value

        match value with
        | Choice1Of2 result ->
            Choice1Of2 (mapping result)
        | Choice2Of2 error ->
            Choice2Of2 error

    /// <summary>
    /// Applies the specified mapping function to a choice value representing an error value (Choice2Of2). If the choice
    /// value represents a result value (Choice1Of2), the result value is passed through without modification.
    /// </summary>
    /// <param name="mapping"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("MapError")>]
    let mapError (mapping : 'Error1 -> 'Error2) (value : Choice<'T, 'Error1>) =
        // Preconditions
        checkNonNull "value" value

        match value with
        | Choice1Of2 result ->
            Choice1Of2 result
        | Choice2Of2 error ->
            Choice2Of2 (mapping error)

    /// <summary>
    /// Applies the specified binding function to a choice value representing a result value (Choice1Of2). If the choice
    /// value represents an error value (Choice2Of2), the error value is passed through without modification.
    /// </summary>
    /// <param name="binding"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Bind")>]
    let bind (binding : 'T -> Choice<'U, 'Error>) value =
        // Preconditions
        checkNonNull "value" value

        match value with
        | Choice1Of2 result ->
            binding result
        | Choice2Of2 error ->
            Choice2Of2 error

    /// <summary>
    /// Applies the specified binding function to a choice value representing a pair of result values (Choice1Of2).
    /// If the first component of the pair represents an error value, the error is passed through without modification;
    /// otherwise, if the second component of the pair represents an error value, the error is passed through without
    /// modification; otherwise, both components represent result values, which are applied to the specified binding function.
    /// </summary>
    /// <param name="binding"></param>
    /// <param name="value1"></param>
    /// <param name="value2"></param>
    /// <returns></returns>
    [<CompiledName("Bind2")>]
    let bind2 (binding : 'T -> 'U -> Choice<'V, 'Error>) value1 value2 =
        // Preconditions
        checkNonNull "value1" value1
        checkNonNull "value2" value2

        match value1, value2 with
        | Choice1Of2 result1, Choice1Of2 result2 ->
            binding result1 result2
        | Choice1Of2 _, Choice2Of2 error
        | Choice2Of2 error, _ ->
            Choice2Of2 error

    /// <summary></summary>
    /// <param name="predicate"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Exists")>]
    let exists (predicate : 'T -> bool) (value : Choice<'T, 'Error>) : bool =
        // Preconditions
        checkNonNull "value" value

        match value with
        | Choice1Of2 result ->
            predicate result
        | Choice2Of2 _ ->
            false

    /// <summary></summary>
    /// <param name="predicate"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Forall")>]
    let forall (predicate : 'T -> bool) (value : Choice<'T, 'Error>) : bool =
        // Preconditions
        checkNonNull "value" value

        match value with
        | Choice1Of2 result ->
            predicate result
        | Choice2Of2 _ ->
            true

    /// <summary></summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> 'State) (state : 'State) (value : Choice<'T, 'Error>) : 'State =
        // Preconditions
        checkNonNull "value" value

        match value with
        | Choice1Of2 result ->
            folder state result
        | Choice2Of2 _ ->
            state

    /// <summary></summary>
    /// <param name="folder"></param>
    /// <param name="value"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    [<CompiledName("FoldBack")>]
    let foldBack (folder : 'T -> 'State -> 'State) (value : Choice<'T, 'Error>) (state : 'State) : 'State =
        // Preconditions
        checkNonNull "value" value

        match value with
        | Choice1Of2 result ->
            folder result state
        | Choice2Of2 _ ->
            state

    /// <summary></summary>
    /// <param name="action"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> unit) (value : Choice<'T, 'Error>) : unit =
        // Preconditions
        checkNonNull "value" value

        match value with
        | Choice2Of2 _ -> ()
        | Choice1Of2 result ->
            action result

    /// <summary></summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("BindOrRaise")>]
    let inline bindOrRaise (value : Choice<'T, #exn>) : 'T =
        // Preconditions
        checkNonNull "value" value

        match value with
        | Choice1Of2 result ->
            result
        | Choice2Of2 ex ->
            raise ex

    /// <summary></summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("BindOrFail")>]
    let inline bindOrFail (value : Choice<'T, string>) : 'T =
        // Preconditions
        checkNonNull "value" value

        match value with
        | Choice1Of2 result ->
            result
        | Choice2Of2 msg ->
            raise <| exn msg

    /// <summary></summary>
    /// <param name="generator"></param>
    /// <returns></returns>
    [<CompiledName("Attempt")>]
    let attempt generator : Choice<'T, _> =
        try Choice1Of2 <| generator ()
        with ex -> Choice2Of2 ex

    /// <summary>
    /// Composes two functions designed for use with the 'choice' workflow.
    /// This function is analagous to the F# (&gt;&gt;) operator.
    /// </summary>
    /// <param name="f"></param>
    /// <param name="g"></param>
    /// <returns></returns>
    [<CompiledName("Compose")>]
    let compose (f : 'T -> Choice<'U, 'Error>) (g : 'U -> Choice<'V, 'Error>) =
        f >> (bind g)

    /// <summary>
    /// Composes two functions designed for use with the 'choice' workflow.
    /// This function is analagous to the F# (&lt;&lt;) operator.
    /// </summary>
    /// <param name="f"></param>
    /// <param name="g"></param>
    /// <returns></returns>
    [<CompiledName("ComposeBack")>]
    let composeBack (f : 'U -> Choice<'V, 'Error>) (g : 'T -> Choice<'U, 'Error>) =
        g >> (bind f)
