(*

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
module ExtCore.Control.Collections.AsyncState

open OptimizedClosures
open ExtCore
open ExtCore.Collections
open ExtCore.Control


/// The standard F# Array module, adapted for use within 'asyncState' workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array =
    open System.Collections

    /// AsyncState implementation of Array.init.
    let rec private initImpl (initializer : FSharpFunc<_,_,_>, array : 'T[], state : 'State, currentIndex) : Async<'T[] * 'State> =
        async {
        if currentIndex >= array.Length then
            // Return the initialized array and the final state value.
            return array, state
        else
            // Initialize the element for the current array index.
            let! element, state = initializer.Invoke (currentIndex, state)

            // Store the element into the array, then initialize the remaining elements.
            array.[currentIndex] <- element
            return! initImpl (initializer, array, state, currentIndex + 1)
        }

    /// AsyncState implementation of Array.init.
    [<CompiledName("Init")>]
    let init (count : int) (initializer : int -> 'State -> Async<'T * 'State>) (state : 'State) : Async<'T[] * 'State> =
        // Preconditions
        if count < 0 then
            invalidArg "count" "The count cannot be negative."

        // If the count is zero, we can return immediately.
        if count = 0 then
            async.Return (Array.empty, state)
        else
            // Call the recursive implementation.
            let initializer = FSharpFunc<_,_,_>.Adapt initializer
            let result = Array.zeroCreate count
            initImpl (initializer, result, state, 0)

    /// AsyncState implementation of Array.iter.
    let rec private iterImpl (action : FSharpFunc<_,_,_>, array : 'T[], state : 'State, currentIndex) : Async<unit * 'State> =
        async {
        if currentIndex >= array.Length then
            // Return the final state value.
            return (), state
        else
            // Invoke the action with the current array element.
            let! (), state = action.Invoke (array.[currentIndex], state)

            // Iterate over the remaining elements.
            return! iterImpl (action, array, state, currentIndex + 1)
        }

    /// AsyncState implementation of Array.iter.
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> 'State -> Async<unit * 'State>) (array : 'T[]) (state : 'State) : Async<unit * 'State> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        let action = FSharpFunc<_,_,_>.Adapt action
        iterImpl (action, array, state, 0)

    /// AsyncState implementation of Array.iteri.
    let rec private iteriImpl (action : FSharpFunc<_,_,_,_>, array : 'T[], state : 'State, currentIndex) =
        async {
        if currentIndex >= array.Length then
            // Return the final state value.
            return (), state
        else
            // Invoke the action with the current array element.
            let! (), state = action.Invoke (currentIndex, array.[currentIndex], state)

            // Iterate over the remaining elements.
            return! iteriImpl (action, array, state, currentIndex + 1)
        }

    /// AsyncState implementation of Array.iteri.
    [<CompiledName("IterateIndexed")>]
    let iteri (action : int -> 'T -> 'State -> Async<unit * 'State>) (array : 'T[]) (state : 'State) : Async<unit * 'State> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        let action = FSharpFunc<_,_,_,_>.Adapt action
        iteriImpl (action, array, state, 0)

    /// AsyncState implementation of Array.map.
    let rec private mapImpl (mapping : FSharpFunc<_,_,_>, results : 'U[], array : 'T[], state : 'State, currentIndex) : Async<'U[] * 'State> =
        async {
        if currentIndex >= array.Length then
            // Return the mapped array and the final state value.
            return results, state
        else
            // Map the current array element, then store the result into the results array.
            let! result, state = mapping.Invoke (array.[currentIndex], state)
            results.[currentIndex] <- result

            // Map the remaining elements.
            return! mapImpl (mapping, results, array, state, currentIndex + 1)
        }

    /// AsyncState implementation of Array.map.
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'State -> Async<'U * 'State>) (array : 'T[]) (state : 'State) : Async<'U[] * 'State> =
        // Preconditions
        checkNonNull "array" array

        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        let result = Array.zeroCreate <| Array.length array

        // Call the recursive implementation.
        mapImpl (mapping, result, array, state, 0)

    /// AsyncState implementation of Array.mapi.
    let rec private mapiImpl (mapping : FSharpFunc<_,_,_,_>, results : 'U[], array : 'T[], state : 'State, currentIndex) : Async<'U[] * 'State> =
        async {
        if currentIndex >= array.Length then
            // Return the mapped array and the final state value.
            return results, state
        else
            // Map the current array element, then store the result into the results array.
            let! result, state = mapping.Invoke (currentIndex, array.[currentIndex], state)
            results.[currentIndex] <- result

            // Map the remaining elements.
            return! mapiImpl (mapping, results, array, state, currentIndex + 1)
        }

    /// AsyncState implementation of Array.mapi.
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> 'State -> Async<'U * 'State>) (array : 'T[]) (state : 'State) : Async<'U[] * 'State> =
        // Preconditions
        checkNonNull "array" array

        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
        let result = Array.zeroCreate <| Array.length array

        // Call the recursive implementation.
        mapiImpl (mapping, result, array, state, 0)

    /// AsyncState implementation of Array.map2.
    let rec private map2Impl (mapping : FSharpFunc<_,_,_,_>, results : 'U[], array1 : 'T1[], array2 : 'T2[], state : 'State, currentIndex) : Async<'U[] * 'State> =
        async {
        if currentIndex >= array1.Length then
            // Return the mapped array and the final state value.
            return results, state
        else
            // Map the current array elements, then store the result into the results array.
            let! result, state = mapping.Invoke (array1.[currentIndex], array2.[currentIndex], state)
            results.[currentIndex] <- result

            // Map the remaining elements.
            return! map2Impl (mapping, results, array1, array2, state, currentIndex + 1)
        }

    /// AsyncState implementation of Array.map2.
    [<CompiledName("Map2")>]
    let map2 (mapping : 'T1 -> 'T2 -> 'State -> Async<'U * 'State>) (array1 : 'T1[]) (array2 : 'T2[]) (state : 'State) : Async<'U[] * 'State> =
        // Preconditions
        checkNonNull "array1" array1
        checkNonNull "array2" array2

        let len = Array.length array1
        if Array.length array2 <> len then
            invalidArg "array2" "The arrays have different lengths."

        let result = Array.zeroCreate len
        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping

        // Call the recursive implementation.
        map2Impl (mapping, result, array1, array2, state, 0)

    /// AsyncState implementation of Array.mapi2.
    let rec private mapi2Impl (mapping : FSharpFunc<_,_,_,_,_>, results : 'U[], array1 : 'T1[], array2 : 'T2[], state : 'State, currentIndex) : Async<'U[] * 'State> =
        async {
        if currentIndex >= array1.Length then
            // Return the mapped array and the final state value.
            return results, state
        else
            // Map the current array elements, then store the result into the results array.
            let! result, state = mapping.Invoke (currentIndex, array1.[currentIndex], array2.[currentIndex], state)
            results.[currentIndex] <- result

            // Map the remaining elements.
            return! mapi2Impl (mapping, results, array1, array2, state, currentIndex + 1)
        }

    /// AsyncState implementation of Array.mapi2.
    [<CompiledName("MapIndexed2")>]
    let mapi2 (mapping : int -> 'T1 -> 'T2 -> 'State -> Async<'U * 'State>) (array1 : 'T1[]) (array2 : 'T2[]) (state : 'State) : Async<'U[] * 'State> =
        // Preconditions
        checkNonNull "array1" array1
        checkNonNull "array2" array2

        let len = Array.length array1
        if Array.length array2 <> len then
            invalidArg "array2" "The arrays have different lengths."

        let result = Array.zeroCreate len
        let mapping = FSharpFunc<_,_,_,_,_>.Adapt mapping

        // Call the recursive implementation.
        mapi2Impl (mapping, result, array1, array2, state, 0)
(*
    /// AsyncState implementation of Array.mapPartition.
    [<CompiledName("MapPartition")>]
    let mapPartition (partitioner : 'T -> 'State -> Async<Choice<'U, 'V> * 'State>) (array : 'T[]) (state : 'State) : Async<('U[] * 'V[]) * 'State> =
        // Preconditions
        checkNonNull "array" array

        let len = Array.length array
        let resultList1 = ResizeArray ()
        let resultList2 = ResizeArray ()

        // Call the recursive implementation.
        let partitioner = FSharpFunc<_,_,_>.Adapt partitioner
        mapPartitionImpl (partitioner, results1, results2, array, state, 0)
*)
(*
    /// AsyncState implementation of Array.tryFind.
    let rec private tryFindImpl (predicate, array : 'T[], currentIndex) =
        async {
        if currentIndex >= array.Length then
            // No matching element was found.
            return None
        else
            // Apply the predicate to the current array element.
            let currentElement = array.[currentIndex]
            let! elementIsMatch = predicate currentElement

            // If the predicate matched, return the element;
            // otherwise, continue processing the remaining array elements.
            if elementIsMatch then
                return Some currentElement
            else
                return! tryFindImpl (predicate, array, currentIndex + 1)
        }

    /// AsyncState implementation of Array.tryFind.
    [<CompiledName("TryFind")>]
    let tryFind (predicate : 'T -> Async<bool>) (array : 'T[]) : Async<'T option> =
        // Preconditions
        checkNonNull "array" array
        
        // Call the recursive implementation.
        tryFindImpl (predicate, array, 0)

    /// AsyncState implementation of Array.find.
    [<CompiledName("Find")>]
    let find (predicate : 'T -> Async<bool>) (array : 'T[]) : Async<'T> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation for tryFind.
        async {
        let! result = tryFindImpl (predicate, array, 0)

        match result with
        | Some result ->
            return result

        | None ->
            // No matching element was found, so raise an exception.
            // NOTE : The 'return' keyword is needed here for type-checking reasons.
            return keyNotFound "The array does not contain any elements which match the given predicate."
        }

    /// AsyncState implementation of Array.tryFindIndex.
    let rec private tryFindIndexImpl (predicate, array : 'T[], currentIndex) =
        async {
        if currentIndex >= array.Length then
            // No matching element was found.
            return None
        else
            // Apply the predicate to the current array element.
            let currentElement = array.[currentIndex]
            let! elementIsMatch = predicate currentElement

            // If the predicate matched, return the element;
            // otherwise, continue processing the remaining array elements.
            if elementIsMatch then
                return Some currentIndex
            else
                return! tryFindIndexImpl (predicate, array, currentIndex + 1)
        }

    /// AsyncState implementation of Array.tryFindIndex.
    [<CompiledName("TryFindIndex")>]
    let tryFindIndex (predicate : 'T -> Async<bool>) (array : 'T[]) : Async<int option> =
        // Preconditions
        checkNonNull "array" array
        
        // Call the recursive implementation.
        tryFindIndexImpl (predicate, array, 0)

    /// AsyncState implementation of Array.findIndex.
    [<CompiledName("FindIndex")>]
    let findIndex (predicate : 'T -> Async<bool>) (array : 'T[]) : Async<int> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation for tryFindIndex.
        async {
        let! result = tryFindIndexImpl (predicate, array, 0)

        match result with
        | Some result ->
            return result

        | None ->
            // No matching element was found, so raise an exception.
            // NOTE : The 'return' keyword is needed here for type-checking reasons.
            return keyNotFound "The array does not contain any elements which match the given predicate."
        }

    /// AsyncState implementation of Array.exists.
    let rec private existsImpl (predicate, array : 'T[], currentIndex) =
        async {
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

    /// AsyncState implementation of Array.exists.
    [<CompiledName("Exists")>]
    let exists (predicate : 'T -> Async<bool>) (array : 'T[]) : Async<bool> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        existsImpl (predicate, array, 0)

    /// AsyncState implementation of Array.forall.
    let rec private forallImpl (predicate, array : 'T[], currentIndex) =
        async {
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

    /// AsyncState implementation of Array.forall.
    [<CompiledName("Forall")>]
    let forall (predicate : 'T -> Async<bool>) (array : 'T[]) : Async<bool> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        forallImpl (predicate, array, 0)

    /// AsyncState implementation of Array.tryPick.
    let rec private tryPickImpl (picker, array : 'T[], currentIndex) : Async<'U option> =
        async {
        if currentIndex >= array.Length then
            // No element was picked.
            return None
        else
            // Apply the picker to the current array element.
            let! pickResult = picker array.[currentIndex]

            // If a value was picked, return it; otherwise continue processing
            // the remaining array elements.
            if Option.isSome pickResult then
                return pickResult
            else
                return! tryPickImpl (picker, array, currentIndex + 1)
        }

    /// AsyncState implementation of Array.tryPick.
    [<CompiledName("TryPick")>]
    let tryPick (picker : 'T -> Async<'U option>) (array : 'T[]) : Async<'U option> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        tryPickImpl (picker, array, 0)

    /// AsyncState implementation of Array.pick.
    [<CompiledName("Pick")>]
    let pick (picker : 'T -> Async<'U option>) (array : 'T[]) : Async<'U> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation for tryPick.
        async {
        let! result = tryPickImpl (picker, array, 0)

        match result with
        | Some result ->
            return result

        | None ->
            // No matching element was found, so raise an exception.
            // NOTE : The 'return' keyword is needed here for type-checking reasons.
            return keyNotFound "The array does not contain any elements which match the given picker."
        }
*)
(*
    /// AsyncState implementation of Array.fold.
    let rec private foldImpl (folder : FSharpFunc<_,_,_>, array : 'T[], state : 'State, currentIndex) =
        async {
        if currentIndex >= array.Length then
            // We've reached the end of the array so return the final state value.
            return state
        else
            // Invoke the folder with the current array element and state value.
            let! state = folder.Invoke (state, array.[currentIndex])

            // Continue folding over the remaining array elements.
            return! foldImpl (folder, array, state, currentIndex + 1)
        }

    /// AsyncState implementation of Array.fold.
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> Async<'State>) (state : 'State) (array : 'T[]) : Async<'State> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_>.Adapt folder
        foldImpl (folder, array, state, 0)

    /// AsyncState implementation of Array.foldBack.
    let rec private foldBackImpl (folder : FSharpFunc<_,_,_>, array : 'T[], state : 'State, currentIndex) =
        async {
        if currentIndex < 0 then
            // We've reached the beginning of the array so return the final state value.
            return state
        else
            // Invoke the folder with the current array element and state value.
            let! state = folder.Invoke (array.[currentIndex], state)

            // Continue folding over the remaining array elements.
            return! foldBackImpl (folder, array, state, currentIndex - 1)
        }

    /// AsyncState implementation of Array.foldBack.
    [<CompiledName("FoldBack")>]
    let foldBack (folder : 'T -> 'State -> Async<'State>) (array : 'T[]) (state : 'State) : Async<'State> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_>.Adapt folder
        foldBackImpl (folder, array, state, array.Length - 1)

    /// AsyncState implementation of Array.foldi.
    let rec private foldiImpl (folder : FSharpFunc<_,_,_,_>, array : 'T[], state : 'State, currentIndex) =
        async {
        if currentIndex >= array.Length then
            // We've reached the end of the array so return the final state value.
            return state
        else
            // Invoke the folder with the current array element and state value.
            let! state = folder.Invoke (state, currentIndex, array.[currentIndex])

            // Continue folding over the remaining array elements.
            return! foldiImpl (folder, array, state, currentIndex + 1)
        }

    /// AsyncState implementation of Array.foldi.
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : 'State -> int -> 'T -> Async<'State>) (state : 'State) (array : 'T[]) : Async<'State> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        foldiImpl (folder, array, state, 0)

    /// AsyncState implementation of Array.foldiBack.
    let rec private foldiBackImpl (folder : FSharpFunc<_,_,_,_>, array : 'T[], state : 'State, currentIndex) =
        async {
        if currentIndex < 0 then
            // We've reached the beginning of the array so return the final state value.
            return state
        else
            // Invoke the folder with the current array element and state value.
            let! state = folder.Invoke (currentIndex, array.[currentIndex], state)

            // Continue folding over the remaining array elements.
            return! foldiBackImpl (folder, array, state, currentIndex - 1)
        }

    /// AsyncState implementation of Array.foldiBack.
    [<CompiledName("FoldIndexedBack")>]
    let foldiBack (folder : int -> 'T -> 'State -> Async<'State>) (array : 'T[]) (state : 'State) : Async<'State> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        foldiBackImpl (folder, array, state, array.Length - 1)

    /// AsyncState implementation of Array.reduce.
    [<CompiledName("Reduce")>]
    let reduce (reduction : 'T -> 'T -> Async<'T>) (array : 'T[]) : Async<'T> =
        // Preconditions
        checkNonNull "array" array
        if Array.isEmpty array then
            invalidArg "array" "The array is empty."

        // Call the recursive implementation for Array.fold.
        // Skip the first array element and use it as the initial state of the fold.
        let reduction = FSharpFunc<_,_,_>.Adapt reduction
        foldImpl (reduction, array, array.[0], 1)

    /// AsyncState implementation of Array.reduceBack.
    [<CompiledName("ReduceBack")>]
    let reduceBack (reduction : 'T -> 'T -> Async<'T>) (array : 'T[]) : Async<'T> =
        // Preconditions
        checkNonNull "array" array
        if Array.isEmpty array then
            invalidArg "array" "The array is empty."

        // Call the recursive implementation for Array.foldBack.
        // Skip the last array element and use it as the initial state of the fold.
        let reduction = FSharpFunc<_,_,_>.Adapt reduction
        let len = Array.length array
        foldBackImpl (reduction, array, array.[len - 1], len - 2)
*)

/// The standard F# List module, adapted for use within 'asyncState' workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module List =
    (* NOTE :   Many of the functions below are implemented with a simple public "wrapper"
                function which calls a private recursive implementation. This reduces memory
                compared to a naive implementation using something like List.fold -- these
                recursive implementations avoid creating a large Async instance up-front
                (which would consume approximately the same amount of memory as the list itself). *)

    /// AsyncState implementation of List.map.
    let rec private mapImpl (mapping : FSharpFunc<_,_,_>, mapped : 'U list, pending : 'T list, state : 'State) =
        async {
        match pending with
        | [] ->
            // Reverse the list of mapped values before returning it.
            return List.rev mapped, state

        | el :: pending ->
            // Apply the current list element to the mapping function.
            let! mappedEl, state = mapping.Invoke (el, state)

            // Cons the result to the list of mapped values, then continue
            // mapping the rest of the pending list elements.
            return! mapImpl (mapping, mappedEl :: mapped, pending, state)
        }

    /// AsyncState implementation of List.map.
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'State -> Async<'U * 'State>) (list : 'T list) (state : 'State) : Async<'U list * 'State> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        mapImpl (mapping, [], list, state)

    /// AsyncState implementation of List.mapi.
    let rec private mapiImpl (mapping : FSharpFunc<_,_,_,_>, mapped : 'U list, pending : 'T list, state : 'State, currentIndex) =
        async {
        match pending with
        | [] ->
            // Reverse the list of mapped values before returning it.
            return List.rev mapped, state

        | el :: pending ->
            // Apply the current list element to the mapping function.
            let! mappedEl, state = mapping.Invoke (currentIndex, el, state)

            // Cons the result to the list of mapped values, then continue
            // mapping the rest of the pending list elements.
            return! mapiImpl (mapping, mappedEl :: mapped, pending, state, currentIndex + 1)
        }

    /// AsyncState implementation of List.mapi.
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> 'State -> Async<'U * 'State>) (list : 'T list) (state : 'State) : Async<'U list * 'State> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
        mapiImpl (mapping, [], list, state, 0)
(*
    /// AsyncState implementation of List.fold.
    let rec private foldImpl (folder : FSharpFunc<_,_,_>, pending : 'T list, state : 'State) =
        async {
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

    /// AsyncState implementation of List.fold.
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> Async<'State>) (state : 'State) (list : 'T list) : Async<'State> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_>.Adapt folder
        foldImpl (folder, list, state)

    /// AsyncState implementation of List.foldBack.
    let rec private foldBackImpl (folder : FSharpFunc<_,_,_>, pending : 'T list, state : 'State) =
        async {
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

    /// AsyncState implementation of List.foldBack.
    [<CompiledName("FoldBack")>]
    let foldBack (folder : 'T -> 'State -> Async<'State>) (list : 'T list) (state : 'State) : Async<'State> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_>.Adapt folder
        foldBackImpl (folder, list, state)
*)
    /// AsyncState implementation of List.choose.
    let rec private chooseImpl (chooser : FSharpFunc<_,_,_>, chosen : 'U list, pending : 'T list, state : 'State) =
        async {
        match pending with
        | [] ->
            // Reverse the list of chosen values before returning it.
            return List.rev chosen, state

        | el :: pending ->
            // Apply the current list element to the chooser function.
            let! result, state = chooser.Invoke (el, state)
            
            match result with
            | None ->
                // Continue processing the remaining elements.
                return! chooseImpl (chooser, chosen, pending, state)

            | Some result ->
                // Cons the result to the list of chosen values, then continue
                // mapping the rest of the pending list elements.
                return! chooseImpl (chooser, result :: chosen, pending, state)
        }

    /// AsyncState implementation of List.choose.
    [<CompiledName("Choose")>]
    let choose (chooser : 'T -> 'State -> Async<'U option * 'State>) (list : 'T list) (state : 'State) : Async<'U list * 'State> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        let chooser = FSharpFunc<_,_,_>.Adapt chooser
        chooseImpl (chooser, [], list, state)
(*
    /// AsyncState implementation of List.collect.
    // OPTIMIZE : It may be possible to reduce memory usage by processing the "outer" list
    // backwards (like List.foldBack), in which case we could append each of the resulting
    // lists to an accumulator and we wouldn't need to reverse the result at the end.
    let rec private collectImpl (mapping, collected : 'U list, pending : 'T list) =
        async {
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

    /// AsyncState implementation of List.collect.
    [<CompiledName("Collect")>]
    let collect (mapping : 'T -> Async<'U list>) (list : 'T list) : Async<'U list> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        collectImpl (mapping, [], list)

    /// AsyncState implementation of List.exists.
    let rec private existsImpl (predicate, pending : 'T list) =
        async {
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

    /// AsyncState implementation of List.exists.
    [<CompiledName("Exists")>]
    let exists (predicate : 'T -> Async<bool>) (list : 'T list) : Async<bool> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        existsImpl (predicate, list)

    /// AsyncState implementation of List.forall.
    let rec private forallImpl (predicate, pending : 'T list) =
        async {
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

    /// AsyncState implementation of List.forall.
    [<CompiledName("Forall")>]
    let forall (predicate : 'T -> Async<bool>) (list : 'T list) : Async<bool> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        forallImpl (predicate, list)

    /// AsyncState implementation of List.filter.
    let rec private filterImpl (predicate, filtered : 'T list, pending : 'T list) =
        async {
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

    /// AsyncState implementation of List.filter.
    [<CompiledName("Filter")>]
    let filter (predicate : 'T -> Async<bool>) (list : 'T list) : Async<'T list> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        filterImpl (predicate, [], list)

    /// AsyncState implementation of List.tryFind.
    let rec private tryFindImpl (predicate, pending : 'T list) =
        async {
        match pending with
        | [] ->
            // None of the list elements matched the predicate.
            return None

        | el :: pending ->
            // Apply the current list element to the predicate.
            let! result = predicate el

            // If the element matched, short-circuit (return immediately);
            // otherwise, continue processing the rest of the list.
            if result then
                return Some el
            else
                return! tryFindImpl (predicate, pending)
        }

    /// AsyncState implementation of List.tryFind.
    [<CompiledName("TryFind")>]
    let tryFind (predicate : 'T -> Async<bool>) (list : 'T list) : Async<'T option> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        tryFindImpl (predicate, list)

    /// AsyncState implementation of List.find.
    [<CompiledName("Find")>]
    let find (predicate : 'T -> Async<bool>) (list : 'T list) : Async<'T> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation for tryFind.
        async {
        let! result = tryFindImpl (predicate, list)

        match result with
        | Some result ->
            return result

        | None ->
            // No matching element was found, so raise an exception.
            // NOTE : The 'return' keyword is needed here for type-checking reasons.
            return keyNotFound "The list does not contain any elements which match the given predicate."
        }

    /// AsyncState implementation of List.tryFindIndex.
    let rec private tryFindIndexImpl (predicate, pending : 'T list, currentIndex) =
        async {
        match pending with
        | [] ->
            // None of the list elements matched the predicate.
            return None

        | el :: pending ->
            // Apply the current list element to the predicate.
            let! result = predicate el

            // If the element matched, short-circuit (return immediately);
            // otherwise, continue processing the rest of the list.
            if result then
                return Some currentIndex
            else
                return! tryFindIndexImpl (predicate, pending, currentIndex + 1)
        }

    /// AsyncState implementation of List.tryFindIndex.
    [<CompiledName("TryFindIndex")>]
    let tryFindIndex (predicate : 'T -> Async<bool>) (list : 'T list) : Async<int option> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        tryFindIndexImpl (predicate, list, 0)

    /// AsyncState implementation of List.findIndex.
    [<CompiledName("FindIndex")>]
    let findIndex (predicate : 'T -> Async<bool>) (list : 'T list) : Async<int> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation for tryFindIndex.
        async {
        let! result = tryFindIndexImpl (predicate, list, 0)

        match result with
        | Some result ->
            return result

        | None ->
            // No matching element was found, so raise an exception.
            // NOTE : The 'return' keyword is needed here for type-checking reasons.
            return keyNotFound "The list does not contain any elements which match the given predicate."
        }
*)
    /// AsyncState implementation of List.init.
    let rec private initImpl (initializer : FSharpFunc<_,_,_>, initialized : 'T list, state : 'State, count, index) =
        async {
        if index >= count then
            // Reverse the initialized list and return it.
            return List.rev initialized, state
        else
            // Initialize a value with the current index.
            let! newEl, state = initializer.Invoke (index, state)

            // Cons the new element onto the list of initialized values
            // and continue processing.
            return! initImpl (initializer, newEl :: initialized, state, count, index + 1)
        }

    /// AsyncState implementation of List.init.
    [<CompiledName("Initialize")>]
    let init (count : int) (initializer : int -> 'State -> Async<'T * 'State>) (state : 'State) : Async<'T list * 'State> =
        // Preconditions
        if count < 0 then
            invalidArg "count" "The number of elements to initialize cannot be negative."

        // Call the recursive implementation.
        let initializer = FSharpFunc<_,_,_>.Adapt initializer
        initImpl (initializer, [], state, count, 0)

    /// AsyncState implementation of Async.iter.
    let rec private iterImpl (action : FSharpFunc<_,_,_>, pending : 'T list, state : 'State) =
        async {
        match pending with
        | [] ->
            return (), state
        | el :: pending ->
            // Apply the action to the current element.
            let! (), state = action.Invoke (el, state)

            // Continue processing the rest of the list.
            return! iterImpl (action, pending, state)
        }

    /// AsyncState implementation of List.iter.
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> 'State -> Async<unit * 'State>) (list : 'T list) (state : 'State) : Async<unit * 'State> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        let action = FSharpFunc<_,_,_>.Adapt action
        iterImpl (action, list, state)

    /// AsyncState implementation of Async.iteri.
    let rec private iteriImpl (action : FSharpFunc<_,_,_,_>, pending : 'T list, state : 'State, currentIndex) =
        async {
        match pending with
        | [] ->
            return (), state
        | el :: pending ->
            // Apply the action to the current element.
            let! (), state = action.Invoke (currentIndex, el, state)

            // Continue processing the rest of the list.
            return! iteriImpl (action, pending, state, currentIndex + 1)
        }

    /// AsyncState implementation of List.iteri.
    [<CompiledName("IterateIndexed")>]
    let iteri (action : int -> 'T -> 'State -> Async<unit * 'State>) (list : 'T list) (state : 'State) : Async<unit * 'State> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        let action = FSharpFunc<_,_,_,_>.Adapt action
        iteriImpl (action, list, state, 0)

(*
/// The standard F# Seq module, adapted for use within 'asyncState' workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Seq =
    //
    [<CompiledName("Map")>]
    let map (mapping : 'T -> Async<'U>) (sequence : seq<'T>) : Async<seq<'U>> =
        // Preconditions
        checkNonNull "sequence" sequence

        async {
        // Apply the mapping function to each element.
        for el in sequence do
            let! mappedValue = mapping array.[i]
            result.[i] <- mappedValue

        // Return the completed results.
        return result
        }

    //
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> Async<'U>) (sequence : 'T[]) : Async<seq<'U>> =
        // Preconditions
        checkNonNull "sequence" sequence

        let len = Array.length array
        let result = Array.zeroCreate len

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        async {
        // Apply the mapping function to each element.
        for i in 0 .. len - 1 do
            let! mappedValue = mapping.Invoke (i, array.[i])
            result.[i] <- mappedValue

        // Return the completed results.
        return result
        }

    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> Async<'State>) (state : 'State) (sequence : 'T[]) : Async<'State> =
        // Preconditions
        checkNonNull "sequence" sequence

        let folder = FSharpFunc<_,_,_>.Adapt folder

        (async.Return state, array)
        ||> Array.fold (fun stateAsync el ->
            async {
            // Get the state.
            let! state = stateAsync

            // Invoke the folder and return the result.
            return! folder.Invoke (state, el)
            })

    //
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : int -> 'State -> 'T -> Async<'State>) (state : 'State) (sequence : 'T[]) : Async<'State> =
        // Preconditions
        checkNonNull "sequence" sequence

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        (async.Return state, array)
        ||> Array.foldi (fun index stateAsync el ->
            async {
            // Get the state.
            let! state = stateAsync

            // Invoke the folder and return the result.
            return! folder.Invoke (index, state, el)
            })

    //
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> Async<unit>) (sequence : 'T[]) : Async<unit> =
        // Preconditions
        checkNonNull "sequence" sequence

        (async.Zero (), array)
        ||> Array.fold (fun iterPrevious el ->
            async {
            // Execute the workflow for the preceeding elements.
            do! iterPrevious

            // Asynchronously invoke the action for this element.
            do! action el
            })

    //
    [<CompiledName("IterateIndexed")>]
    let iteri (action : int -> 'T -> Async<unit>) (sequence : 'T[]) : Async<unit> =
        // Preconditions
        checkNonNull "sequence" sequence

        let action = FSharpFunc<_,_,_>.Adapt action

        (async.Zero (), array)
        ||> Array.foldi (fun index iterPrevious el ->
            async {
            // Execute the workflow for the preceeding elements.
            do! iterPrevious

            // Asynchronously invoke the action for this element.
            do! action.Invoke (index, el)
            })
*)
