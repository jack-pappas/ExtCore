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

namespace ExtCore.Collections

open LanguagePrimitives
open OptimizedClosures
open ExtCore


/// <summary></summary>
/// <typeparam name="State"></typeparam>
/// <typeparam name="Key"></typeparam>
/// <typeparam name="T"></typeparam>
type IMapFolder<'State, 'Key, 'T> =
    /// <summary></summary>
    /// <param name="key"></param>
    /// <returns></returns>
    abstract Map : key:'Key -> 'T

    /// <summary></summary>
    /// <param name="state"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    abstract Fold : state:'State -> value:'T -> 'State

//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MapFolder =
    /// <summary></summary>
    /// <param name="mapping"></param>
    /// <param name="folder"></param>
    /// <returns></returns>
    [<CompiledName("FromFunctions")>]
    let inline fromFunctions (mapping : 'Key -> 'T) (folder : 'State -> 'T -> 'State) =
        { new IMapFolder<'State, 'Key, 'T> with
            member __.Map (key : 'Key) : 'T =
                mapping key
            member __.Fold (state : 'State) (value : 'T) : 'State =
                folder state value }

    /// <summary></summary>
    /// <param name="mapping"></param>
    /// <param name="folder"></param>
    /// <returns></returns>
    [<CompiledName("FromFunctions")>]
    let inline fromFunctionsTupled (mapping : 'Key -> 'T) (folder : 'State * 'T -> 'State) =
        { new IMapFolder<'State, 'Key, 'T> with
            member __.Map (key : 'Key) : 'T =
                mapping key
            member __.Fold (state : 'State) (value : 'T) : 'State =
                folder (state, value) }


/// <summary></summary>
/// <typeparam name="Key"></typeparam>
/// <typeparam name="T"></typeparam>
type IMapReduction<'Key, 'T> =
    /// <summary></summary>
    /// <param name="key"></param>
    /// <returns></returns>
    abstract Map : key:'Key -> 'T

    /// <summary></summary>
    /// <param name="value1"></param>
    /// <param name="value2"></param>
    /// <returns></returns>
    abstract Reduce : value1:'T -> value2:'T -> 'T

//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MapReduction =
    /// <summary></summary>
    /// <param name="mapping"></param>
    /// <param name="reduction"></param>
    /// <returns></returns>
    [<CompiledName("FromFunctions")>]
    let inline fromFunctions (mapping : 'Key -> 'T) (reduction : 'T -> 'T -> 'T) =
        { new IMapReduction<'Key, 'T> with
            member __.Map (key : 'Key) : 'T =
                mapping key
            member __.Reduce (value1 : 'T) (value2 : 'T) : 'T =
                reduction value1 value2 }

    /// <summary></summary>
    /// <param name="mapping"></param>
    /// <param name="reduction"></param>
    /// <returns></returns>
    [<CompiledName("FromFunctions")>]
    let inline fromFunctionsTupled (mapping : 'Key -> 'T) (reduction : 'T * 'T -> 'T) =
        { new IMapReduction<'Key, 'T> with
            member __.Map (key : 'Key) : 'T =
                mapping key
            member __.Reduce (value1 : 'T) (value2 : 'T) : 'T =
                reduction (value1, value2) }


/// Functional operators over a range of values.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Range =
    open LanguagePrimitives

    (* NOTE :   In the functions below, 'start' and 'finish' are *inclusive*, just like the F# 'for' loop. *)

    /// <summary></summary>
    /// <param name="action"></param>
    /// <param name="start"></param>
    /// <param name="finish"></param>
    /// <remarks></remarks>
    [<CompiledName("Iterate")>]
    let inline iter (action : ^T -> unit) start finish : unit =
        let mutable index = start
        while index <= finish do
            action index
            index <- index + GenericOne

    /// <summary></summary>
    /// <param name="folder"></param>
    /// <param name="start"></param>
    /// <param name="finish"></param>
    /// <param name="state"></param>
    /// <remarks></remarks>
    [<CompiledName("Fold")>]
    let inline fold (folder : ^State -> ^T -> ^State) start finish state : ^State =
        let mutable state = state
        let mutable index = start
        while index <= finish do
            state <- folder state index
            index <- index + GenericOne
        state

    /// <summary></summary>
    /// <param name="folder"></param>
    /// <param name="start"></param>
    /// <param name="finish"></param>
    /// <param name="state"></param>
    /// <remarks></remarks>
    [<CompiledName("FoldBack")>]
    let inline foldBack (folder : ^T -> ^State -> ^State) start finish state : ^State =
        let mutable state = state
        let mutable index = finish
        while index >= start do
            state <- folder index state
            index <- index - GenericOne
        state

    /// <summary></summary>
    /// <param name="predicate"></param>
    /// <param name="start"></param>
    /// <param name="finish"></param>
    /// <remarks></remarks>
    [<CompiledName("Exists")>]
    let inline exists (predicate : ^T -> bool) start finish : bool =
        let mutable foundMatch = false
        let mutable index = start
        while index <= finish && not foundMatch do
            foundMatch <- predicate index
            index <- index + GenericOne
        foundMatch

    /// <summary></summary>
    /// <param name="predicate"></param>
    /// <param name="start"></param>
    /// <param name="finish"></param>
    /// <remarks></remarks>
    [<CompiledName("Forall")>]
    let inline forall (predicate : ^T -> bool) start finish : bool =
        let mutable allMatch = true
        let mutable index = start
        while index <= finish && allMatch do
            allMatch <- predicate index
            index <- index + GenericOne
        allMatch

    // TODO
    // mapReduce


/// <summary>
/// Functional programming operators related to the <see cref="System.Collections.Generic.KeyValuePair`2"/> type.
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module KeyValuePair =
    open System.Collections.Generic

    /// <summary>Gets the key from the key/value pair.
    [<CompiledName("Key")>]
    let inline key (kvp : KeyValuePair<'Key, 'T>) =
        kvp.Key

    /// <summary>Gets the value in the key/value pair.
    [<CompiledName("Value")>]
    let inline value (kvp : KeyValuePair<'Key, 'T>) =
        kvp.Value

    /// <summary>
    /// Transforms the value in a key/value pair by applying the specified function to it. The key passed to the function
    /// indicates the key of the value being transformed. This function is analogous to <see cref="Map.map"/>.
    /// </summary>
    [<CompiledName("Map")>]
    let map (mapping : 'Key -> 'T -> 'U) (kvp : KeyValuePair<'Key, 'T>) : KeyValuePair<'Key, 'U> =
        KeyValuePair (kvp.Key, mapping kvp.Key kvp.Value)

