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

/// <summary>Additional functional operators on Result<_,_> values.</summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Result =
    /// <summary>Does the Result value represent a result value?</summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("IsResult")>]
    let inline isResult (value : Result<'T, 'Error>) : bool =

        match value with
        | Ok _ -> true
        | Error _ -> false

    /// <summary>Does the Result value represent an error value?</summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("IsError")>]
    let inline isError (value : Result<'T, 'Error>) : bool =

        match value with
        | Ok _ -> false
        | Error _ -> true

    /// <summary>Gets the result value associated with the Result.</summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Get")>]
    let get (value : Result<'T, 'Error>) =
        match value with
        | Ok result ->
            result
        | Error _ ->
            invalidArg "value" "Cannot get the result because the Result`2 instance is an error value."

    /// <summary>Gets the error value associated with the Result.</summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("GetError")>]
    let getError (value : Result<'T, 'Error>) =
        match value with
        | Ok _ ->
            invalidArg "value" "Cannot get the error because the Result`2 instance is a result value."
        | Error error ->
            error

    /// <summary>Creates a Result from a result value.</summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Result")>]
    let inline result value : Result<'T, 'Error> =
        Ok value

    /// <summary>Creates a Result from an error value.</summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Error")>]
    let inline error value : Result<'T, 'Error> =
        Error value

    /// <summary>
    /// Creates a Result representing an error value. The error value in the Result is the specified error message.
    /// </summary>
    /// <param name="message">The error message.</param>
    /// <returns></returns>
    [<CompiledName("FailWith")>]
    let inline failwith message : Result<'T, string> =
        Error message

    /// <summary>
    /// Creates a Result representing an error value. The error value in the Result is the specified formatted error message.
    /// </summary>
    /// <param name="format"></param>
    /// <returns></returns>
    [<CompiledName("PrintFormatToStringThenFail")>]
    let inline failwithf (format : Printf.StringFormat<'T, Result<'U, string>>) =
        Printf.ksprintf failwith format

    /// <summary></summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("OfOption")>]
    let ofOption (value : 'T option) : Result<'T, unit> =
        match value with
        | Some result ->
            Ok result
        | None ->
            Error ()

    /// <summary></summary>
    /// <param name="errorValue"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("OfOptionDefault")>]
    let ofOptionDefault (errorValue : 'Error) (value : 'T option) : Result<'T, 'Error> =
        match value with
        | Some result ->
            Ok result
        | None ->
            Error errorValue

    /// <summary></summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("ToOption")>]
    let toOption (value : Result<'T, 'Error>) : 'T option =
        match value with
        | Ok result ->
            Some result
        | Error _ ->
            None

    /// <summary>
    /// Applies the specified binding function to a choice value representing a pair of result values (Ok).
    /// If the first component of the pair represents an error value, the error is passed through without modification;
    /// otherwise, if the second component of the pair represents an error value, the error is passed through without
    /// modification; otherwise, both components represent result values, which are applied to the specified binding function.
    /// </summary>
    /// <param name="binding"></param>
    /// <param name="value1"></param>
    /// <param name="value2"></param>
    /// <returns></returns>
    [<CompiledName("Bind2")>]
    let bind2 (binding : 'T -> 'U -> Result<'V, 'Error>) value1 value2 =

        match value1, value2 with
        | Ok result1, Ok result2 ->
            binding result1 result2
        | Ok _, Error error
        | Error error, _ ->
            Error error

    /// <summary></summary>
    /// <param name="predicate"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Exists")>]
    let exists (predicate : 'T -> bool) (value : Result<'T, 'Error>) : bool =
        match value with
        | Ok result ->
            predicate result
        | Error _ ->
            false

    /// <summary>Returns true if predicate holds true for all Ok values, ignores any errors</summary>
    /// <param name="predicate"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Forall")>]
    let forall (predicate : 'T -> bool) (value : Result<'T, 'Error>) : bool =
        match value with
        | Ok result ->
            predicate result
        | Error _ ->
            true // why?

    /// <summary></summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> 'State) (state : 'State) (value : Result<'T, 'Error>) : 'State =
        match value with
        | Ok result ->
            folder state result
        | Error _ ->
            state

    /// <summary></summary>
    /// <param name="folder"></param>
    /// <param name="value"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    [<CompiledName("FoldBack")>]
    let foldBack (folder : 'T -> 'State -> 'State) (value : Result<'T, 'Error>) (state : 'State) : 'State =
        match value with
        | Ok result ->
            folder result state
        | Error _ ->
            state

    /// <summary></summary>
    /// <param name="action"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> unit) (value : Result<'T, 'Error>) : unit =
        match value with
        | Error _ -> ()
        | Ok result ->
            action result

    /// <summary></summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("BindOrRaise")>]
    let inline bindOrRaise (value : Result<'T, #exn>) : 'T =
        match value with
        | Ok result ->
            result
        | Error ex ->
            raise ex

    /// <summary></summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("BindOrFail")>]
    let inline bindOrFail (value : Result<'T, string>) : 'T =
        match value with
        | Ok result ->
            result
        | Error msg ->
            raise <| exn msg

    /// <summary></summary>
    /// <param name="generator"></param>
    /// <returns></returns>
    [<CompiledName("Attempt")>]
    let attempt generator : Result<'T, _> =
        try Ok <| generator ()
        with ex -> Error ex

    /// <summary>
    /// Composes two functions designed for use with the 'choice' workflow.
    /// This function is analagous to the F# (&gt;&gt;) operator.
    /// </summary>
    /// <param name="f"></param>
    /// <param name="g"></param>
    /// <returns></returns>
    [<CompiledName("Compose")>]
    let compose (f : 'T -> Result<'U, 'Error>) (g : 'U -> Result<'V, 'Error>) =
        f >> (Result.bind g)

    /// <summary>
    /// Composes two functions designed for use with the 'choice' workflow.
    /// This function is analagous to the F# (&lt;&lt;) operator.
    /// </summary>
    /// <param name="f"></param>
    /// <param name="g"></param>
    /// <returns></returns>
    [<CompiledName("ComposeBack")>]
    let composeBack (f : 'U -> Result<'V, 'Error>) (g : 'T -> Result<'U, 'Error>) =
        g >> (Result.bind f)


