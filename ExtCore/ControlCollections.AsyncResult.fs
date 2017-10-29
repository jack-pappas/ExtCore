﻿(*

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
module ExtCore.Control.Collections.AsyncResult

open OptimizedClosures
open ExtCore
open ExtCore.Collections
open ExtCore.Control


/// The standard F# Array module, adapted for use within 'asyncResult' workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array =
    open System.Collections

    /// AsyncChoice implementation of Array.fold.
    let rec private foldImpl (folder : FSharpFunc<_,_,_>, array : 'T[], state : 'State, currentIndex) : Async<Result<'State, 'Error>> =
        asyncResult {
        if currentIndex >= array.Length then
            // We've reached the end of the array so return the final state value.
            return state
        else
            // Invoke the folder with the current array element and state value.
            let! state = folder.Invoke (state, array.[currentIndex])

            // Continue folding over the remaining array elements.
            return! foldImpl (folder, array, state, currentIndex + 1)
        }

    /// AsyncChoice implementation of Array.fold.
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> Async<Result<'State, 'Error>>) (state : 'State) (array : 'T[]) : Async<Result<'State, 'Error>> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_>.Adapt folder
        foldImpl (folder, array, state, 0)

    /// AsyncChoice implementation of Array.foldBack.
    let rec private foldBackImpl (folder : FSharpFunc<_,_,_>, array : 'T[], state : 'State, currentIndex) : Async<Result<'State, 'Error>> =
        asyncResult {
        if currentIndex < 0 then
            // We've reached the beginning of the array so return the final state value.
            return state
        else
            // Invoke the folder with the current array element and state value.
            let! state = folder.Invoke (array.[currentIndex], state)

            // Continue folding over the remaining array elements.
            return! foldBackImpl (folder, array, state, currentIndex - 1)
        }

    /// AsyncChoice implementation of Array.foldBack.
    [<CompiledName("FoldBack")>]
    let foldBack (folder : 'T -> 'State -> Async<Result<'State, 'Error>>) (array : 'T[]) (state : 'State) : Async<Result<'State, 'Error>> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_>.Adapt folder
        foldBackImpl (folder, array, state, array.Length - 1)

    /// AsyncChoice implementation of Array.foldi.
    let rec private foldiImpl (folder : FSharpFunc<_,_,_,_>, array : 'T[], state : 'State, currentIndex) =
        asyncResult {
        if currentIndex >= array.Length then
            // We've reached the end of the array so return the final state value.
            return state
        else
            // Invoke the folder with the current array element and state value.
            let! state = folder.Invoke (state, currentIndex, array.[currentIndex])

            // Continue folding over the remaining array elements.
            return! foldiImpl (folder, array, state, currentIndex + 1)
        }

    /// AsyncChoice implementation of Array.foldi.
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : 'State -> int -> 'T -> Async<Result<'State, 'Error>>) (state : 'State) (array : 'T[]) : Async<Result<'State, 'Error>> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        foldiImpl (folder, array, state, 0)

    /// AsyncChoice implementation of Array.foldiBack.
    let rec private foldiBackImpl (folder : FSharpFunc<_,_,_,_>, array : 'T[], state : 'State, currentIndex) =
        asyncResult {
        if currentIndex < 0 then
            // We've reached the beginning of the array so return the final state value.
            return state
        else
            // Invoke the folder with the current array element and state value.
            let! state = folder.Invoke (currentIndex, array.[currentIndex], state)

            // Continue folding over the remaining array elements.
            return! foldiBackImpl (folder, array, state, currentIndex - 1)
        }

    /// AsyncChoice implementation of Array.foldiBack.
    [<CompiledName("FoldIndexedBack")>]
    let foldiBack (folder : int -> 'T -> 'State -> Async<Result<'State, 'Error>>) (array : 'T[]) (state : 'State) : Async<Result<'State, 'Error>> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        foldiBackImpl (folder, array, state, array.Length - 1)

    /// AsyncChoice implementation of Array.init.
    [<CompiledName("Init")>]
    let init (count : int) (initializer : int -> Async<Result<'T, 'Error>>) : Async<Result<'T[], 'Error>> =
        // Preconditions
        if count < 0 then
            invalidArg "count" "The count cannot be negative."

        let result = Array.zeroCreate count

        asyncResult {
        // Apply the mapping function to each array element.
        for i in 0 .. count - 1 do
            let! mappedValue = initializer i
            result.[i] <- mappedValue

        // Return the completed results.
        return result
        }

    /// AsyncChoice implementation of Array.iter.
    let rec private iterImpl (action, array : 'T[], currentIndex) : Async<Result<unit, 'Error>> =
        asyncResult {
        if currentIndex < array.Length then
            // Apply the current array element to the action function.
            do! action array.[currentIndex]

            // Continue iterating over the remaining array elements.
            return! iterImpl (action, array, currentIndex + 1)
        }

    /// AsyncChoice implementation of Array.iter.
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> Async<Result<unit, 'Error>>) (array : 'T[]) : Async<Result<unit, 'Error>> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        iterImpl (action, array, 0)

    /// AsyncChoice implementation of Array.iteri.
    let rec private iteriImpl (action : FSharpFunc<_,_,_>, array : 'T[], currentIndex) =
        asyncResult {
        if currentIndex < array.Length then
            // Invoke the action with the current index and array element.
            do! action.Invoke (currentIndex, array.[currentIndex])

            // Continue iterating over the remaining array elements.
            return! iteriImpl (action, array, currentIndex + 1)
        }

    /// AsyncChoice implementation of Array.iteri.
    [<CompiledName("IterateIndexed")>]
    let iteri (action : int -> 'T -> Async<Result<unit, 'Error>>) (array : 'T[]) : Async<Result<unit, 'Error>> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        let action = FSharpFunc<_,_,_>.Adapt action
        iteriImpl (action, array, 0)

    /// AsyncChoice implementation of Array.map.
    [<CompiledName("Map")>]
    let map (mapping : 'T -> Async<Result<'U, 'Error>>) (array : 'T[]) : Async<Result<'U[], 'Error>> =
        // Preconditions
        checkNonNull "array" array

        let len = Array.length array
        let result = Array.zeroCreate len

        asyncResult {
        // Apply the mapping function to each array element.
        for i in 0 .. len - 1 do
            let! mappedValue = mapping array.[i]
            result.[i] <- mappedValue

        // Return the completed results.
        return result
        }

    /// AsyncChoice implementation of Array.mapi.
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> Async<Result<'U, 'Error>>) (array : 'T[]) : Async<Result<'U[], 'Error>> =
        // Preconditions
        checkNonNull "array" array

        let len = Array.length array
        let result = Array.zeroCreate len

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        asyncResult {
        // Apply the mapping function to each array element.
        for i in 0 .. len - 1 do
            let! mappedValue = mapping.Invoke (i, array.[i])
            result.[i] <- mappedValue

        // Return the completed results.
        return result
        }

    /// AsyncChoice implementation of Array.map2.
    [<CompiledName("Map2")>]
    let map2 (mapping : 'T1 -> 'T2 -> Async<Result<'U, 'Error>>) (array1 : 'T1[]) (array2 : 'T2[]) : Async<Result<'U[], 'Error>> =
        // Preconditions
        checkNonNull "array1" array1
        checkNonNull "array2" array2

        let len = Array.length array1
        if Array.length array2 <> len then
            invalidArg "array2" "The arrays have different lengths."

        let result = Array.zeroCreate len
        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        asyncResult {
        // Apply the mapping function to each array element.
        for i in 0 .. len - 1 do
            let! mappedValue = mapping.Invoke (array1.[i], array2.[i])
            result.[i] <- mappedValue

        // Return the completed results.
        return result
        }

    /// AsyncChoice implementation of Array.mapi2.
    [<CompiledName("MapIndexed2")>]
    let mapi2 (mapping : int -> 'T1 -> 'T2 -> Async<Result<'U, 'Error>>) (array1 : 'T1[]) (array2 : 'T2[]) : Async<Result<'U[], 'Error>> =
        // Preconditions
        checkNonNull "array1" array1
        checkNonNull "array2" array2

        let len = Array.length array1
        if Array.length array2 <> len then
            invalidArg "array2" "The arrays have different lengths."

        let result = Array.zeroCreate len
        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping

        asyncResult {
        // Apply the mapping function to each array element.
        for i in 0 .. len - 1 do
            let! mappedValue = mapping.Invoke (i, array1.[i], array2.[i])
            result.[i] <- mappedValue

        // Return the completed results.
        return result
        }

    /// AsyncChoice implementation of Array.reduce.
    [<CompiledName("Reduce")>]
    let reduce (reduction : 'T -> 'T -> Async<Result<'T, 'Error>>) (array : 'T[]) : Async<Result<'T, 'Error>> =
        // Preconditions
        checkNonNull "array" array
        if Array.isEmpty array then
            invalidArg "array" "The array is empty."

        // Call the recursive implementation for Array.fold.
        // Skip the first array element and use it as the initial state of the fold.
        let reduction = FSharpFunc<_,_,_>.Adapt reduction
        foldImpl (reduction, array, array.[0], 1)

    /// AsyncChoice implementation of Array.reduceBack.
    [<CompiledName("ReduceBack")>]
    let reduceBack (reduction : 'T -> 'T -> Async<Result<'T, 'Error>>) (array : 'T[]) : Async<Result<'T, 'Error>> =
        // Preconditions
        checkNonNull "array" array
        if Array.isEmpty array then
            invalidArg "array" "The array is empty."

        // Call the recursive implementation for Array.foldBack.
        // Skip the last array element and use it as the initial state of the fold.
        let reduction = FSharpFunc<_,_,_>.Adapt reduction
        let len = Array.length array
        foldBackImpl (reduction, array, array.[len - 1], len - 2)

    /// AsyncChoice implementation of Array.exists.
    let rec private existsImpl (predicate, array : 'T[], currentIndex) : Async<Result<bool, 'Error>> =
        asyncResult {
        if currentIndex >= array.Length then
            // No matching element was found.
            return false
        else
            // Apply the predicate to the current array element.
            let! elementIsMatch = predicate array.[currentIndex]

            // If the element matched the predicate, short-circuit (return immediately);
            // otherwise, continue processing the remaining array elements.
            if elementIsMatch then
                return true
            else
                return! existsImpl (predicate, array, currentIndex + 1)
        }

    /// AsyncChoice implementation of Array.exists.
    [<CompiledName("Exists")>]
    let exists (predicate : 'T -> Async<Result<bool, 'Error>>) (array : 'T[]) : Async<Result<bool, 'Error>> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        existsImpl (predicate, array, 0)

    /// AsyncChoice implementation of Array.forall.
    let rec private forallImpl (predicate, array : 'T[], currentIndex) : Async<Result<bool, 'Error>> =
        asyncResult {
        if currentIndex >= array.Length then
            // All elements matched the predicate.
            return true
        else
            // Apply the predicate to the current array element.
            let! elementIsMatch = predicate array.[currentIndex]

            // If the element matched the predicate, continue processing the
            // remaining array elements; otherwise return immediately.
            if elementIsMatch then
                return! forallImpl (predicate, array, currentIndex + 1)
            else
                return false
        }

    /// AsyncChoice implementation of Array.forall.
    [<CompiledName("Forall")>]
    let forall (predicate : 'T -> Async<Result<bool, 'Error>>) (array : 'T[]) : Async<Result<bool, 'Error>> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        forallImpl (predicate, array, 0)


/// The standard F# List module, adapted for use within 'asyncResult' workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module List =
    (* NOTE :   Many of the functions below are implemented with a simple public "wrapper"
                function which calls a private recursive implementation. This reduces memory
                compared to a naive implementation using something like List.fold -- these
                recursive implementations avoid creating a large Async instance up-front
                (which would consume approximately the same amount of memory as the list itself). *)

    /// AsyncChoice implementation of List.map.
    let rec private mapImpl (mapping, mapped : 'U list, pending : 'T list) : Async<Result<'U list, 'Error>> =
        asyncResult {
        match pending with
        | [] ->
            // Reverse the list of mapped values before returning it.
            return List.rev mapped

        | el :: pending ->
            // Apply the current list element to the mapping function.
            let! mappedEl = mapping el

            // Cons the result to the list of mapped values, then continue
            // mapping the rest of the pending list elements.
            return! mapImpl (mapping, mappedEl :: mapped, pending)
        }

    /// AsyncChoice implementation of List.map.
    [<CompiledName("Map")>]
    let map (mapping : 'T -> Async<Result<'U, 'Error>>) (list : 'T list) : Async<Result<'U list, 'Error>> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        mapImpl (mapping, [], list)

    /// AsyncChoice implementation of List.mapi.
    let rec private mapiImpl (mapping : FSharpFunc<_,_,_>, mapped : 'U list, pending : 'T list, currentIndex) : Async<Result<'U list, 'Error>> =
        asyncResult {
        match pending with
        | [] ->
            // Reverse the list of mapped values before returning it.
            return List.rev mapped

        | el :: pending ->
            // Apply the current list element to the mapping function.
            let! mappedEl = mapping.Invoke (currentIndex, el)

            // Cons the result to the list of mapped values, then continue
            // mapping the rest of the pending list elements.
            return! mapiImpl (mapping, mappedEl :: mapped, pending, currentIndex + 1)
        }

    /// AsyncChoice implementation of List.mapi.
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> Async<Result<'U, 'Error>>) (list : 'T list) : Async<Result<'U list, 'Error>> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        mapiImpl (mapping, [], list, 0)

    /// AsyncChoice implementation of List.fold.
    let rec private foldImpl (folder : FSharpFunc<_,_,_>, pending : 'T list, state : 'State) : Async<Result<'State, 'Error>> =
        asyncResult {
        match pending with
        | [] ->
            // Return the final state value.
            return state

        | el :: pending ->
            // Apply the folder to the current list element and state value.
            let! state = folder.Invoke (state, el)

            // Continue folding over the rest of the list.
            return! foldImpl (folder, pending, state)
        }

    /// AsyncChoice implementation of List.fold.
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> Async<Result<'State, 'Error>>) (state : 'State) (list : 'T list) : Async<Result<'State, 'Error>> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_>.Adapt folder
        foldImpl (folder, list, state)

    /// AsyncChoice implementation of List.foldBack.
    let rec private foldBackImpl (folder : FSharpFunc<_,_,_>, pending : 'T list, state : 'State) : Async<Result<'State, 'Error>> =
        asyncResult {
        match pending with
        | [] ->
            // Return the final state value.
            return state

        | el :: pending ->
            // Apply the folder to the rest of the list before processing the
            // current element (because we're folding backwards).
            let! state = foldBackImpl (folder, pending, state)

            // Apply the folder to the current list element and state value.
            return! folder.Invoke (el, state)
        }

    /// AsyncChoice implementation of List.foldBack.
    [<CompiledName("FoldBack")>]
    let foldBack (folder : 'T -> 'State -> Async<Result<'State, 'Error>>) (list : 'T list) (state : 'State) : Async<Result<'State, 'Error>> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_>.Adapt folder
        foldBackImpl (folder, list, state)

    /// AsyncChoice implementation of List.collect.
    // OPTIMIZE : It may be possible to reduce memory usage by processing the "outer" list
    // backwards (like List.foldBack), in which case we could append each of the resulting
    // lists to an accumulator and we wouldn't need to reverse the result at the end.
    let rec private collectImpl (mapping, collected : 'U list, pending : 'T list) : Async<Result<_, 'Error>> =
        asyncResult {
        match pending with
        | [] ->
            // Return the collected results.
            return collected

        | el :: pending ->
            // Apply the current element to the mapping function.
            let! result = mapping el

            // Append the result (a list) to the list of collected values and
            // continue processing the remaining list elements.
            return! collectImpl (mapping, collected @ result, pending)

        }

    /// AsyncChoice implementation of List.collect.
    [<CompiledName("Collect")>]
    let collect (mapping : 'T -> Async<Result<'U list, 'Error>>) (list : 'T list) : Async<Result<'U list, 'Error>> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        collectImpl (mapping, [], list)

    /// AsyncChoice implementation of List.exists.
    let rec private existsImpl (predicate, pending : 'T list) : Async<Result<bool, 'Error>> =
        asyncResult {
        match pending with
        | [] ->
            // None of the list elements matched the predicate.
            return false

        | el :: pending ->
            // Apply the current list element to the predicate.
            let! result = predicate el

            // If the element matched, short-circuit (return immediately);
            // otherwise, continue processing the rest of the list.
            if result then
                return true
            else
                return! existsImpl (predicate, pending)
        }

    /// AsyncChoice implementation of List.exists.
    [<CompiledName("Exists")>]
    let exists (predicate : 'T -> Async<Result<bool, 'Error>>) (list : 'T list) : Async<Result<bool, 'Error>> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        existsImpl (predicate, list)

    /// AsyncChoice implementation of List.forall.
    let rec private forallImpl (predicate, pending : 'T list) : Async<Result<bool, 'Error>> =
        asyncResult {
        match pending with
        | [] ->
            // All of the list elements matched the predicate.
            return true

        | el :: pending ->
            // Apply the current list element to the predicate.
            let! result = predicate el

            // If the element didn't match, short-circuit (return immediately);
            // otherwise, continue processing the rest of the list.
            if result then
                return! forallImpl (predicate, pending)
            else
                return false
        }

    /// AsyncChoice implementation of List.forall.
    [<CompiledName("Forall")>]
    let forall (predicate : 'T -> Async<Result<bool, 'Error>>) (list : 'T list) : Async<Result<bool, 'Error>> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        forallImpl (predicate, list)

    /// AsyncChoice implementation of List.filter.
    let rec private filterImpl (predicate, filtered : 'T list, pending : 'T list) : Async<Result<'T list, 'Error>> =
        asyncResult {
        match pending with
        | [] ->
            // Reverse the list of filtered values before returning it.
            return List.rev filtered

        | el :: pending ->
            // Apply the current list element to the predicate.
            let! result = predicate el

            // If the current element matched the predicate, cons it onto the list of
            // filtered elements and continue processing the rest of the list.
            let filtered = if result then el :: filtered else filtered
            return! filterImpl (predicate, filtered, pending)
        }

    /// AsyncChoice implementation of List.filter.
    [<CompiledName("Filter")>]
    let filter (predicate : 'T -> Async<Result<bool, 'Error>>) (list : 'T list) : Async<Result<'T list, 'Error>> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        filterImpl (predicate, [], list)

    /// AsyncChoice implementation of List.init.
    let rec private initImpl (initializer, initialized : 'T list, count, index) =
        asyncResult {
        if index >= count then
            // Reverse the initialized list and return it.
            return List.rev initialized
        else
            // Initialize a value with the current index.
            let! newEl = initializer index

            // Cons the new element onto the list of initialized values
            // and continue processing.
            return! initImpl (initializer, newEl :: initialized, count, index + 1)
        }

    /// AsyncChoice implementation of List.init.
    [<CompiledName("Initialize")>]
    let init (count : int) (initializer : int -> Async<Result<'T, 'Error>>) : Async<Result<'T list, 'Error>> =
        // Preconditions
        if count < 0 then
            invalidArg "count" "The number of elements to initialize cannot be negative."

        // Call the recursive implementation.
        initImpl (initializer, [], count, 0)

    /// AsyncChoice implementation of Async.iter.
    let rec private iterImpl (action, pending : 'T list) : Async<Result<unit, 'Error>> =
        asyncResult {
        match pending with
        | [] ->
            return ()
        | el :: pending ->
            // Apply the action to the current element.
            do! action el

            // Continue processing the rest of the list.
            return! iterImpl (action, pending)
        }

    /// AsyncChoice implementation of List.iter.
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> Async<Result<unit, 'Error>>) (list : 'T list) : Async<Result<unit, 'Error>> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        iterImpl (action, list)

    /// AsyncChoice implementation of Async.iteri.
    let rec private iteriImpl (action : FSharpFunc<_,_,_>, pending : 'T list, index) : Async<Result<unit, 'Error>> =
        asyncResult {
        match pending with
        | [] ->
            return ()
        | el :: pending ->
            // Apply the action to the current element.
            do! action.Invoke (index, el)

            // Continue processing the rest of the list.
            return! iteriImpl (action, pending, index + 1)
        }

    /// AsyncChoice implementation of List.iteri.
    [<CompiledName("IterateIndexed")>]
    let iteri (action : int -> 'T -> Async<Result<unit, 'Error>>) (list : 'T list) : Async<Result<unit, 'Error>> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        let action = FSharpFunc<_,_,_>.Adapt action
        iteriImpl (action, list, 0)


/// The standard F# Seq module, adapted for use within 'asyncResult' workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Seq =
(*
    //
    [<CompiledName("Map")>]
    let map (mapping : 'T -> Async<'U option>) (sequence : seq<'T>) : Async<seq<'U> option> =
        // Preconditions
        checkNonNull "sequence" sequence

        asyncResult {
        // Apply the mapping function to each element.
        for el in sequence do
            let! mappedValue = mapping array.[i]
            result.[i] <- mappedValue

        // Return the completed results.
        return result
        }

    //
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> Async<'U option>) (sequence : seq<'T>) : Async<seq<'U> option> =
        // Preconditions
        checkNonNull "sequence" sequence

        let len = Array.length array
        let result = Array.zeroCreate len

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        asyncResult {
        // Apply the mapping function to each element.
        for i in 0 .. len - 1 do
            let! mappedValue = mapping.Invoke (i, array.[i])
            result.[i] <- mappedValue

        // Return the completed results.
        return result
        }

    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> Async<'State option>) (state : 'State) (sequence : seq<'T>) : Async<'State option> =
        // Preconditions
        checkNonNull "sequence" sequence

        let folder = FSharpFunc<_,_,_>.Adapt folder

        (async.Return state, array)
        ||> Array.fold (fun stateAsync el ->
            asyncResult {
            // Get the state.
            let! state = stateAsync

            // Invoke the folder and return the result.
            return! folder.Invoke (state, el)
            })

    //
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : int -> 'State -> 'T -> Async<'State option>) (state : 'State) (sequence : seq<'T>) : Async<'State option> =
        // Preconditions
        checkNonNull "sequence" sequence

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        (async.Return state, array)
        ||> Array.foldi (fun index stateAsync el ->
            asyncResult {
            // Get the state.
            let! state = stateAsync

            // Invoke the folder and return the result.
            return! folder.Invoke (index, state, el)
            })
*)
    //
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> Async<Result<unit, 'Error>>) (sequence : seq<'T>) : Async<Result<unit, 'Error>> =
        // Preconditions
        checkNonNull "sequence" sequence

        asyncResult {
        for el in sequence do
            do! action el
        }

    //
    [<CompiledName("IterateIndexed")>]
    let iteri (action : int -> 'T -> Async<Result<unit, 'Error>>) (sequence : seq<'T>) : Async<Result<unit, 'Error>> =
        // Preconditions
        checkNonNull "sequence" sequence

        let action = FSharpFunc<_,_,_>.Adapt action
        let indexedSequence =
            Seq.mapi (fun i x -> i, x) sequence

        asyncResult {
        for idx, el in indexedSequence do
            do! action.Invoke (idx, el)
        }