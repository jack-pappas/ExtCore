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

//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExtCore.Control.Collections.ProtectedState.Compatibility
    
open Microsoft.FSharp.Control
open OptimizedClosures
open ExtCore
open ExtCore.Collections


/// The standard F# Array module, lifted into the ProtectedState monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array =
    /// A specialization of Array.iter which threads an accumulator through the computation
    /// and which also short-circuits the computation if the mapping function returns an
    /// error when any element is applied to it.
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> 'State -> Choice<unit * 'State, 'Error>)
            (array : 'T[]) (state : 'State) : Choice<unit * 'State, 'Error> =
        // Preconditions
        checkNonNull "array" array

        let action = FSharpFunc<_,_,_>.Adapt action
        let len = array.Length

        let mutable index = 0
        let mutable state = state
        let mutable error = None

        while index < len && Option.isNone error do
            match action.Invoke (array.[index], state) with
            | Choice2Of2 err ->
                error <- Some err
            | Choice1Of2 ((), state') ->
                state <- state'
                index <- index + 1
            
        // If the error was set, return it.
        // Otherwise return the result and updated state.
        match error with
        | Some error ->
            Choice2Of2 error
        | None ->
            Choice1Of2 ((), state)

    /// A specialization of Array.iteri which threads an accumulator through the computation
    /// and which also short-circuits the computation if the mapping function returns an
    /// error when any element is applied to it.
    [<CompiledName("IterateIndexed")>]
    let iteri (action : int -> 'T -> 'State -> Choice<unit * 'State, 'Error>)
            (array : 'T[]) (state : 'State) : Choice<unit * 'State, 'Error> =
        // Preconditions
        checkNonNull "array" array

        let action = FSharpFunc<_,_,_,_>.Adapt action
        let len = array.Length

        let mutable index = 0
        let mutable state = state
        let mutable error = None

        while index < len && Option.isNone error do
            match action.Invoke (index, array.[index], state) with
            | Choice2Of2 err ->
                error <- Some err
            | Choice1Of2 ((), state') ->
                state <- state'
                index <- index + 1
            
        // If the error was set, return it.
        // Otherwise return the result and updated state.
        match error with
        | Some error ->
            Choice2Of2 error
        | None ->
            Choice1Of2 ((), state)

    /// A specialization of Array.map which threads an accumulator through the computation
    /// and which also short-circuits the computation if the mapping function returns an
    /// error when any element is applied to it.
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'State -> Choice<'U * 'State, 'Error>)
            (array : 'T[]) (state : 'State) : Choice<'U[] * 'State, 'Error> =
        // Preconditions
        checkNonNull "array" array

        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        let len = array.Length
        let results = Array.zeroCreate len

        let mutable index = 0
        let mutable state = state
        let mutable error = None

        while index < len && Option.isNone error do
            match mapping.Invoke (array.[index], state) with
            | Choice2Of2 err ->
                error <- Some err
            | Choice1Of2 (result, state') ->
                results.[index] <- result
                state <- state'
                index <- index + 1
            
        // If the error was set, return it.
        // Otherwise return the result and updated state.
        match error with
        | Some error ->
            Choice2Of2 error
        | None ->
            Choice1Of2 (results, state)

    /// A specialization of Array.mapi which threads an accumulator through the computation
    /// and which also short-circuits the computation if the mapping function returns an
    /// error when any element is applied to it.
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> 'State -> Choice<'U * 'State, 'Error>)
            (array : 'T[]) (state : 'State) : Choice<'U[] * 'State, 'Error> =
        // Preconditions
        checkNonNull "array" array

        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
        let len = array.Length
        let results = Array.zeroCreate len

        let mutable index = 0
        let mutable state = state
        let mutable error = None

        while index < len && Option.isNone error do
            match mapping.Invoke (index, array.[index], state) with
            | Choice2Of2 err ->
                error <- Some err
            | Choice1Of2 (result, state') ->
                results.[index] <- result
                state <- state'
                index <- index + 1
            
        // If the error was set, return it.
        // Otherwise return the result and updated state.
        match error with
        | Some error ->
            Choice2Of2 error
        | None ->
            Choice1Of2 (results, state)


/// The standard F# List module, lifted into the ProtectedState monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module List =
    /// A specialization of List.map which threads an accumulator through the computation
    /// and which also short-circuits the computation if the mapping function returns an
    /// error when any element is applied to it.
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'State -> Choice<'U * 'State, 'Error>)
            (list : 'T list) (state : 'State) : Choice<'U list * 'State, 'Error> =
        // Preconditions
        checkNonNull "list" list

        let mapping = FSharpFunc<_,_,_>.Adapt mapping
            
        let rec mapRec (results, state, lst) =
            match lst with
            | [] ->
                let results = List.rev results
                Choice1Of2 (results, state)
            | hd :: tl ->
                // Apply the function to the head of the list.
                // If the result is an error, return it;
                // otherwise, continue processing recursively.
                match mapping.Invoke (hd, state) with
                | Choice2Of2 error ->
                    Choice2Of2 error
                | Choice1Of2 (result, state) ->
                    mapRec (result :: results, state, tl)

        // Call the recursive implementation function.
        mapRec ([], state, list)


/// The ExtCore.Collections.ArrayView module, lifted into the ProtectedState monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ArrayView =
    /// A specialization of ArrayView.iter which threads an accumulator through the
    /// computation and which also short-circuits the computation if the mapping function
    /// returns an error when any element is applied to it.
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> 'State -> Choice<unit * 'State, 'Error>)
            (view : ArrayView<'T>) (state : 'State) : Choice<unit * 'State, 'Error> =
        let action = FSharpFunc<_,_,_>.Adapt action

        let array = view.Array
        let endExclusive = view.Offset + view.Count

        let mutable index = view.Offset
        let mutable state = state
        let mutable error = None

        while index < endExclusive && Option.isNone error do
            match action.Invoke (array.[index], state) with
            | Choice2Of2 err ->
                error <- Some err
            | Choice1Of2 ((), state') ->
                state <- state'
                index <- index + 1
            
        // If the error was set, return it.
        match error with
        | Some error ->
            Choice2Of2 error
        | None ->
            Choice1Of2 ((), state)

    /// A specialization of ArrayView.iteri which threads an accumulator through the
    /// computation and which also short-circuits the computation if the mapping function
    /// returns an error when any element is applied to it.
    [<CompiledName("IterateIndexed")>]
    let iteri (action : int -> 'T -> 'State -> Choice<unit * 'State, 'Error>)
            (view : ArrayView<'T>) (state : 'State) : Choice<unit * 'State, 'Error> =
        let action = FSharpFunc<_,_,_,_>.Adapt action

        let array = view.Array
        let endExclusive = view.Offset + view.Count

        let mutable index = view.Offset
        let mutable state = state
        let mutable error = None

        while index < endExclusive && Option.isNone error do
            match action.Invoke (index, array.[index], state) with
            | Choice2Of2 err ->
                error <- Some err
            | Choice1Of2 ((), state') ->
                state <- state'
                index <- index + 1
            
        // If the error was set, return it.
        match error with
        | Some error ->
            Choice2Of2 error
        | None ->
            Choice1Of2 ((), state)

    /// A specialization of ArrayView.map which threads an accumulator through the
    /// computation and which also short-circuits the computation if the mapping function
    /// returns an error when any element is applied to it.
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'State -> Choice<'U * 'State, 'Error>)
            (view : ArrayView<'T>) (state : 'State) : Choice<'U[] * 'State, 'Error> =
        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        let array = view.Array
        let endExclusive = view.Offset + view.Count
        /// Holds the mapped results.
        let results = Array.zeroCreate view.Count

        let mutable index = view.Offset
        let mutable state = state
        let mutable error = None

        while index < endExclusive && Option.isNone error do
            match mapping.Invoke (array.[index], state) with
            | Choice2Of2 err ->
                error <- Some err
            | Choice1Of2 (result, state') ->
                results.[index] <- result
                state <- state'
                index <- index + 1
            
        // If the error was set, return it.
        // Otherwise return the result and updated state.
        match error with
        | Some error ->
            Choice2Of2 error
        | None ->
            Choice1Of2 (results, state)

    /// A specialization of ArrayView.mapi which threads an accumulator through the
    /// computation and which also short-circuits the computation if the mapping function
    /// returns an error when any element is applied to it.
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> 'State -> Choice<'U * 'State, 'Error>)
            (view : ArrayView<'T>) (state : 'State) : Choice<'U[] * 'State, 'Error> =
        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping

        let array = view.Array
        let endExclusive = view.Offset + view.Count
        /// Holds the mapped results.
        let results = Array.zeroCreate view.Count

        let mutable index = view.Offset
        let mutable state = state
        let mutable error = None

        while index < endExclusive && Option.isNone error do
            match mapping.Invoke (index, array.[index], state) with
            | Choice2Of2 err ->
                error <- Some err
            | Choice1Of2 (result, state') ->
                results.[index] <- result
                state <- state'
                index <- index + 1
            
        // If the error was set, return it.
        // Otherwise return the result and updated state.
        match error with
        | Some error ->
            Choice2Of2 error
        | None ->
            Choice1Of2 (results, state)

