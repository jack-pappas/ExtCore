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
