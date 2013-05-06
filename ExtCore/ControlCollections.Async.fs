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
module ExtCore.Control.Collections.Async

open OptimizedClosures
open ExtCore
open ExtCore.Collections
open ExtCore.Control


/// The standard F# Array module, adapted for use within 'async' workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array =
    open System.Collections

    /// Returns a new array created by transforming each element of an
    /// array into an Async value which returns the element when executed.
    [<CompiledName("Lift")>]
    let lift (array : 'T[]) : Async<'T>[] =
        // Preconditions
        checkNonNull "array" array

        Array.map async.Return array

    /// Async implementation of Array.fold.
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

    /// Async implementation of Array.fold.
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> Async<'State>) (state : 'State) (array : 'T[]) : Async<'State> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_>.Adapt folder
        foldImpl (folder, array, state, 0)

    /// Async implementation of Array.foldBack.
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

    /// Async implementation of Array.foldBack.
    [<CompiledName("FoldBack")>]
    let foldBack (folder : 'T -> 'State -> Async<'State>) (array : 'T[]) (state : 'State) : Async<'State> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_>.Adapt folder
        foldBackImpl (folder, array, state, array.Length - 1)

    /// Async implementation of Array.foldi.
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

    /// Async implementation of Array.foldi.
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : 'State -> int -> 'T -> Async<'State>) (state : 'State) (array : 'T[]) : Async<'State> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        foldiImpl (folder, array, state, 0)

    /// Async implementation of Array.foldiBack.
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

    /// Async implementation of Array.foldiBack.
    [<CompiledName("FoldIndexedBack")>]
    let foldiBack (folder : int -> 'T -> 'State -> Async<'State>) (array : 'T[]) (state : 'State) : Async<'State> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        foldiBackImpl (folder, array, state, array.Length - 1)

    /// Async implementation of Array.init.
    [<CompiledName("Init")>]
    let init (count : int) (initializer : int -> Async<'T>) : Async<'T[]> =
        // Preconditions
        if count < 0 then
            invalidArg "count" "The count cannot be negative."

        let result = Array.zeroCreate count

        async {
        // Apply the mapping function to each array element.
        for i in 0 .. count - 1 do
            let! mappedValue = initializer i
            result.[i] <- mappedValue

        // Return the completed results.
        return result
        }

    /// Async implementation of Array.iter.
    let rec private iterImpl (action, array : 'T[], currentIndex) =
        async {
        if currentIndex < array.Length then
            // Apply the current array element to the action function.
            do! action array.[currentIndex]

            // Continue iterating over the remaining array elements.
            return! iterImpl (action, array, currentIndex + 1)
        }

    /// Async implementation of Array.iter.
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> Async<unit>) (array : 'T[]) : Async<unit> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        iterImpl (action, array, 0)

    /// Async implementation of Array.iteri.
    let rec private iteriImpl (action : FSharpFunc<_,_,_>, array : 'T[], currentIndex) =
        async {
        if currentIndex < array.Length then
            // Invoke the action with the current index and array element.
            do! action.Invoke (currentIndex, array.[currentIndex])

            // Continue iterating over the remaining array elements.
            return! iteriImpl (action, array, currentIndex + 1)
        }

    /// Async implementation of Array.iteri.
    [<CompiledName("IterateIndexed")>]
    let iteri (action : int -> 'T -> Async<unit>) (array : 'T[]) : Async<unit> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        let action = FSharpFunc<_,_,_>.Adapt action
        iteriImpl (action, array, 0)

    /// Async implementation of Array.map.
    [<CompiledName("Map")>]
    let map (mapping : 'T -> Async<'U>) (array : 'T[]) : Async<'U[]> =
        // Preconditions
        checkNonNull "array" array

        let len = Array.length array
        let result = Array.zeroCreate len

        async {
        // Apply the mapping function to each array element.
        for i in 0 .. len - 1 do
            let! mappedValue = mapping array.[i]
            result.[i] <- mappedValue

        // Return the completed results.
        return result
        }

    /// Async implementation of Array.mapi.
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> Async<'U>) (array : 'T[]) : Async<'U[]> =
        // Preconditions
        checkNonNull "array" array

        let len = Array.length array
        let result = Array.zeroCreate len

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        async {
        // Apply the mapping function to each array element.
        for i in 0 .. len - 1 do
            let! mappedValue = mapping.Invoke (i, array.[i])
            result.[i] <- mappedValue

        // Return the completed results.
        return result
        }

    /// Async implementation of Array.map2.
    [<CompiledName("Map2")>]
    let map2 (mapping : 'T1 -> 'T2 -> Async<'U>) (array1 : 'T1[]) (array2 : 'T2[]) : Async<'U[]> =
        // Preconditions
        checkNonNull "array1" array1
        checkNonNull "array2" array2

        let len = Array.length array1
        if Array.length array2 <> len then
            invalidArg "array2" "The arrays have different lengths."

        let result = Array.zeroCreate len
        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        async {
        // Apply the mapping function to each array element.
        for i in 0 .. len - 1 do
            let! mappedValue = mapping.Invoke (array1.[i], array2.[i])
            result.[i] <- mappedValue

        // Return the completed results.
        return result
        }

    /// Async implementation of Array.mapi2.
    [<CompiledName("MapIndexed2")>]
    let mapi2 (mapping : int -> 'T1 -> 'T2 -> Async<'U>) (array1 : 'T1[]) (array2 : 'T2[]) : Async<'U[]> =
        // Preconditions
        checkNonNull "array1" array1
        checkNonNull "array2" array2

        let len = Array.length array1
        if Array.length array2 <> len then
            invalidArg "array2" "The arrays have different lengths."

        let result = Array.zeroCreate len
        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping

        async {
        // Apply the mapping function to each array element.
        for i in 0 .. len - 1 do
            let! mappedValue = mapping.Invoke (i, array1.[i], array2.[i])
            result.[i] <- mappedValue

        // Return the completed results.
        return result
        }

    /// Async implementation of Array.mapPartition.
    [<CompiledName("MapPartition")>]
    let mapPartition (partitioner : 'T -> Async<Choice<'U, 'V>>) (array : 'T[]) : Async<'U[] * 'V[]> =
        // Preconditions
        checkNonNull "array" array

        let len = Array.length array
        let resultList1 = ResizeArray ()
        let resultList2 = ResizeArray ()

        async {
        // Apply the map-partition function to each array element.
        for i in 0 .. len - 1 do
            let! mappedValue = partitioner array.[i]
                
            // Store the mapped and partitioned value.
            match mappedValue with
            | Choice1Of2 value ->
                lock (resultList1 :> ICollection).SyncRoot <| fun _ ->
                    resultList1.Add value
            | Choice2Of2 value ->
                lock (resultList2 :> ICollection).SyncRoot <| fun _ ->
                    resultList2.Add value

        // Return the completed results.
        // TODO : Make sure the previous operations are guaranteed to be completed.
        // If not, we need to insert a blocking mechanism here to ensure we're done before calling .ToArray().
        let result1 = ResizeArray.toArray resultList1
        let result2 = ResizeArray.toArray resultList2
        return result1, result2
        }

    /// Async implementation of Array.reduce.
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

    /// Async implementation of Array.reduceBack.
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

    /// Async implementation of Array.tryFind.
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

    /// Async implementation of Array.tryFind.
    [<CompiledName("TryFind")>]
    let tryFind (predicate : 'T -> Async<bool>) (array : 'T[]) : Async<'T option> =
        // Preconditions
        checkNonNull "array" array
        
        // Call the recursive implementation.
        tryFindImpl (predicate, array, 0)

    /// Async implementation of Array.find.
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

    /// Async implementation of Array.tryFindIndex.
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

    /// Async implementation of Array.tryFindIndex.
    [<CompiledName("TryFindIndex")>]
    let tryFindIndex (predicate : 'T -> Async<bool>) (array : 'T[]) : Async<int option> =
        // Preconditions
        checkNonNull "array" array
        
        // Call the recursive implementation.
        tryFindIndexImpl (predicate, array, 0)

    /// Async implementation of Array.findIndex.
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

    /// Async implementation of Array.exists.
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

    /// Async implementation of Array.exists.
    [<CompiledName("Exists")>]
    let exists (predicate : 'T -> Async<bool>) (array : 'T[]) : Async<bool> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        existsImpl (predicate, array, 0)

    /// Async implementation of Array.forall.
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

    /// Async implementation of Array.forall.
    [<CompiledName("Forall")>]
    let forall (predicate : 'T -> Async<bool>) (array : 'T[]) : Async<bool> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        forallImpl (predicate, array, 0)

    /// Async implementation of Array.tryPick.
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

    /// Async implementation of Array.tryPick.
    [<CompiledName("TryPick")>]
    let tryPick (picker : 'T -> Async<'U option>) (array : 'T[]) : Async<'U option> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        tryPickImpl (picker, array, 0)

    /// Async implementation of Array.pick.
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


/// The standard F# List module, adapted for use within 'async' workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module List =
    (* NOTE :   Many of the functions below are implemented with a simple public "wrapper"
                function which calls a private recursive implementation. This reduces memory
                compared to a naive implementation using something like List.fold -- these
                recursive implementations avoid creating a large Async instance up-front
                (which would consume approximately the same amount of memory as the list itself). *)

    /// Returns a new list created by transforming each element of an
    /// list into an Async value which returns the element when executed.
    [<CompiledName("Lift")>]
    let lift (list : 'T list) : Async<'T> list =
        // Preconditions
        checkNonNull "list" list

        List.map async.Return list

    /// Async implementation of List.map.
    let rec private mapImpl (mapping, mapped : 'U list, pending : 'T list) =
        async {
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

    /// Async implementation of List.map.
    [<CompiledName("Map")>]
    let map (mapping : 'T -> Async<'U>) (list : 'T list) : Async<'U list> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        mapImpl (mapping, [], list)

    /// Async implementation of List.map2.
    let rec private map2Impl (mapping : FSharpFunc<_,_,_>, mapped : 'U list, list1 : 'T1 list, list2 : 'T2 list) =
        async {
        match list1, list2 with
        | [], [] ->
            // Reverse the list of mapped values before returning it.
            return List.rev mapped

        | el1 :: list1, el2 :: list2 ->
            // Apply the current list elements to the mapping function.
            let! mappedEl = mapping.Invoke (el1, el2)

            // Cons the result to the list of mapped values, then continue
            // mapping the rest of the pending list elements.
            return! map2Impl (mapping, mappedEl :: mapped, list1, list2)

        | _, _ ->
            // Raise an exception because the lists have different lengths.
            // NOTE : The return keyword is needed here for type-inference reasons.
            return invalidArg "list2" "The lists have different lengths."
        }

    /// <summary>Async implementation of List.map2.</summary>
    /// <exception cref="System.ArgumentException">Thrown when the input lists differ in length.</exception>
    [<CompiledName("Map2")>]
    let map2 (mapping : 'T1 -> 'T2 -> Async<'U>) (list1 : 'T1 list) (list2 : 'T2 list) : Async<'U list> =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        // Call the recursive implementation.
        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        map2Impl (mapping, [], list1, list2)

    /// Async implementation of List.map3.
    let rec private map3Impl (mapping : FSharpFunc<_,_,_,_>, mapped : 'U list, list1 : 'T1 list, list2 : 'T2 list, list3 : 'T3 list) =
        async {
        match list1, list2, list3 with
        | [], [], [] ->
            // Reverse the list of mapped values before returning it.
            return List.rev mapped

        | el1 :: list1, el2 :: list2, el3 :: list3 ->
            // Apply the current list elements to the mapping function.
            let! mappedEl = mapping.Invoke (el1, el2, el3)

            // Cons the result to the list of mapped values, then continue
            // mapping the rest of the pending list elements.
            return! map3Impl (mapping, mappedEl :: mapped, list1, list2, list3)

        | _, _, _ ->
            // Raise an exception because the lists have different lengths.
            // NOTE : The return keyword is needed here for type-inference reasons.
            return invalidArg "list3" "The lists have different lengths."
        }

    /// <summary>Async implementation of List.map3.</summary>
    /// <exception cref="System.ArgumentException">Thrown when the input lists differ in length.</exception>
    [<CompiledName("Map3")>]
    let map3 (mapping : 'T1 -> 'T2 -> 'T3 -> Async<'U>) (list1 : 'T1 list) (list2 : 'T2 list) (list3 : 'T3 list) : Async<'U list> =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2
        checkNonNull "list3" list3

        // Call the recursive implementation.
        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
        map3Impl (mapping, [], list1, list2, list3)

    /// Async implementation of List.mapi.
    let rec private mapiImpl (mapping : FSharpFunc<_,_,_>, mapped : 'U list, pending : 'T list, currentIndex) =
        async {
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

    /// Async implementation of List.mapi.
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> Async<'U>) (list : 'T list) : Async<'U list> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        mapiImpl (mapping, [], list, 0)

    /// Async implementation of List.mapi2.
    let rec private mapi2Impl (mapping : FSharpFunc<_,_,_,_>, mapped : 'U list, list1 : 'T1 list, list2 : 'T2 list, currentIndex) =
        async {
        match list1, list2 with
        | [], [] ->
            // Reverse the list of mapped values before returning it.
            return List.rev mapped

        | el1 :: list1, el2 :: list2 ->
            // Apply the current list elements to the mapping function.
            let! mappedEl = mapping.Invoke (currentIndex, el1, el2)

            // Cons the result to the list of mapped values, then continue
            // mapping the rest of the pending list elements.
            return! mapi2Impl (mapping, mappedEl :: mapped, list1, list2, currentIndex + 1)

        | _, _ ->
            // Raise an exception because the lists have different lengths.
            // NOTE : The return keyword is needed here for type-inference reasons.
            return invalidArg "list2" "The lists have different lengths."
        }

    /// <summary>Async implementation of List.mapi2.</summary>
    /// <exception cref="System.ArgumentException">Thrown when the input lists differ in length.</exception>
    [<CompiledName("MapIndexed2")>]
    let mapi2 (mapping : int -> 'T1 -> 'T2 -> Async<'U>) (list1 : 'T1 list) (list2 : 'T2 list) : Async<'U list> =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        // Call the recursive implementation.
        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
        mapi2Impl (mapping, [], list1, list2, 0)

    /// Async implementation of List.fold.
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

    /// Async implementation of List.fold.
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> Async<'State>) (state : 'State) (list : 'T list) : Async<'State> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_>.Adapt folder
        foldImpl (folder, list, state)

    /// Async implementation of List.fold2.
    let rec private fold2Impl (folder : FSharpFunc<_,_,_,_>, list1 : 'T1 list, list2 : 'T2 list, state : 'State) =
        async {
        match list1, list2 with
        | [], [] ->
            // Return the final state value.
            return state

        | el1 :: list1, el2 :: list2 ->
            // Apply the folder to the current list element and state value.
            let! state = folder.Invoke (state, el1, el2)

            // Continue folding over the rest of the list.
            return! fold2Impl (folder, list1, list2, state)

        | _, _ ->
            // Raise an exception because the lists have different lengths.
            // NOTE : The return keyword is needed here for type-inference reasons.
            return invalidArg "list2" "The lists have different lengths."
        }

    /// <summary>Async implementation of List.fold2.</summary>
    /// <exception cref="System.ArgumentException">Thrown when the input lists differ in length.</exception>
    [<CompiledName("Fold2")>]
    let fold2 (folder : 'State -> 'T1 -> 'T2 -> Async<'State>) (state : 'State) (list1 : 'T1 list) (list2 : 'T2 list) : Async<'State> =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        fold2Impl (folder, list1, list2, state)

    /// Async implementation of List.foldBack.
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

    /// Async implementation of List.foldBack.
    [<CompiledName("FoldBack")>]
    let foldBack (folder : 'T -> 'State -> Async<'State>) (list : 'T list) (state : 'State) : Async<'State> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_>.Adapt folder
        foldBackImpl (folder, list, state)

    /// Async implementation of List.foldBack2.
    let rec private foldBack2Impl (folder : FSharpFunc<_,_,_,_>, list1 : 'T1 list, list2 : 'T2 list, state : 'State) =
        async {
        match list1, list2 with
        | [], [] ->
            // Return the final state value.
            return state

        | el1 :: list1, el2 :: list2 ->
            // Apply the folder to the rest of the list before processing the
            // current element (because we're folding backwards).
            let! state = foldBack2Impl (folder, list1, list2, state)

            // Apply the folder to the current list element and state value.
            return! folder.Invoke (el1, el2, state)

        | _, _ ->
            // Raise an exception because the lists have different lengths.
            // NOTE : The return keyword is needed here for type-inference reasons.
            return invalidArg "list2" "The lists have different lengths."
        }

    /// <summary>Async implementation of List.foldBack2.</summary>
    /// <exception cref="System.ArgumentException">Thrown when the input lists differ in length.</exception>
    [<CompiledName("FoldBack2")>]
    let foldBack2 (folder : 'T1 -> 'T2 -> 'State -> Async<'State>) (list1 : 'T1 list) (list2 : 'T2 list) (state : 'State) : Async<'State> =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        foldBack2Impl (folder, list1, list2, state)

    /// Async implementation of List.choose.
    let rec private chooseImpl (chooser, chosen : 'U list, pending : 'T list) =
        async {
        match pending with
        | [] ->
            // Reverse the list of chosen values before returning it.
            return List.rev chosen

        | el :: pending ->
            // Apply the current list element to the chooser function.
            let! result = chooser el
            
            match result with
            | None ->
                // Continue processing the remaining elements.
                return! chooseImpl (chooser, chosen, pending)

            | Some result ->
                // Cons the result to the list of chosen values, then continue
                // mapping the rest of the pending list elements.
                return! chooseImpl (chooser, result :: chosen, pending)
        }

    /// Async implementation of List.choose.
    [<CompiledName("Choose")>]
    let choose (chooser : 'T -> Async<'U option>) (list : 'T list) : Async<'U list> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        chooseImpl (chooser, [], list)

    /// Async implementation of List.choose2.
    let rec private choose2Impl (chooser : FSharpFunc<_,_,_>, chosen : 'U list, list1 : 'T1 list, list2 : 'T2 list) =
        async {
        match list1, list2 with
        | [], [] ->
            // Reverse the list of chosen values before returning it.
            return List.rev chosen

        | el1 :: list1, el2 :: list2 ->
            // Apply the current list elements to the chooser function.
            let! result = chooser.Invoke (el1, el2)
            
            match result with
            | None ->
                // Continue processing the remaining elements.
                return! choose2Impl (chooser, chosen, list1, list2)

            | Some result ->
                // Cons the result to the list of chosen values, then continue
                // mapping the rest of the pending list elements.
                return! choose2Impl (chooser, result :: chosen, list1, list2)

        | _, _ ->
            // Raise an exception because the lists have different lengths.
            // NOTE : The return keyword is needed here for type-inference reasons.
            return invalidArg "list2" "The lists have different lengths."
        }

    /// <summary>Async implementation of List.choose2.</summary>
    /// <exception cref="System.ArgumentException">Thrown when the input lists differ in length.</exception>
    [<CompiledName("Choose2")>]
    let choose2 (chooser : 'T1 -> 'T2 -> Async<'U option>) (list1 : 'T1 list) (list2 : 'T2 list) : Async<'U list> =
        // Preconditions
        checkNonNull "list1" list1

        // Call the recursive implementation.
        let chooser = FSharpFunc<_,_,_>.Adapt chooser
        choose2Impl (chooser, [], list1, list2)

    /// Async implementation of List.collect.
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

    /// Async implementation of List.collect.
    [<CompiledName("Collect")>]
    let collect (mapping : 'T -> Async<'U list>) (list : 'T list) : Async<'U list> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        collectImpl (mapping, [], list)

    /// Async implementation of List.exists.
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

    /// Async implementation of List.exists.
    [<CompiledName("Exists")>]
    let exists (predicate : 'T -> Async<bool>) (list : 'T list) : Async<bool> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        existsImpl (predicate, list)

    /// Async implementation of List.exists2.
    let rec private exists2Impl (predicate : FSharpFunc<_,_,_>, list1 : 'T1 list, list2 : 'T2 list) =
        async {
        match list1, list2 with
        | [], [] ->
            // None of the list elements matched the predicate.
            return false

        | el1 :: list1, el2 :: list2 ->
            // Apply the current list element to the predicate.
            let! result = predicate.Invoke (el1, el2)

            // If the element matched, short-circuit (return immediately);
            // otherwise, continue processing the rest of the list.
            if result then
                return true
            else
                return! exists2Impl (predicate, list1, list2)

        | _, _ ->
            // Raise an exception because the lists have different lengths.
            // NOTE : The return keyword is needed here for type-inference reasons.
            return invalidArg "list2" "The lists have different lengths."
        }

    /// <summary>Async implementation of List.exists2.</summary>
    /// <exception cref="System.ArgumentException">Thrown when the input lists differ in length.</exception>
    [<CompiledName("Exists2")>]
    let exists2 (predicate : 'T1 -> 'T2 -> Async<bool>) (list1 : 'T1 list) (list2 : 'T2 list) : Async<bool> =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        // Call the recursive implementation.
        let predicate = FSharpFunc<_,_,_>.Adapt predicate
        exists2Impl (predicate, list1, list2)

    /// Async implementation of List.forall.
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

    /// Async implementation of List.forall.
    [<CompiledName("Forall")>]
    let forall (predicate : 'T -> Async<bool>) (list : 'T list) : Async<bool> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        forallImpl (predicate, list)

    /// Async implementation of List.forall2.
    let rec private forall2Impl (predicate : FSharpFunc<_,_,_>, list1 : 'T1 list, list2 : 'T2 list) =
        async {
        match list1, list2 with
        | [], [] ->
            // All of the list elements matched the predicate.
            return true

        | el1 :: list1, el2 :: list2 ->
            // Apply the current list element to the predicate.
            let! result = predicate.Invoke (el1, el2)

            // If the element didn't match, short-circuit (return immediately);
            // otherwise, continue processing the rest of the list.
            if result then
                return! forall2Impl (predicate, list1, list2)
            else
                return false

        | _, _ ->
            // Raise an exception because the lists have different lengths.
            // NOTE : The return keyword is needed here for type-inference reasons.
            return invalidArg "list2" "The lists have different lengths."
        }

    /// <summary>Async implementation of List.forall2.</summary>
    /// <exception cref="System.ArgumentException">Thrown when the input lists differ in length.</exception>
    [<CompiledName("Forall2")>]
    let forall2 (predicate : 'T1 -> 'T2 -> Async<bool>) (list1 : 'T1 list) (list2 : 'T2 list) : Async<bool> =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        // Call the recursive implementation.
        let predicate = FSharpFunc<_,_,_>.Adapt predicate
        forall2Impl (predicate, list1, list2)

    /// Async implementation of List.filter.
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

    /// Async implementation of List.filter.
    [<CompiledName("Filter")>]
    let filter (predicate : 'T -> Async<bool>) (list : 'T list) : Async<'T list> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        filterImpl (predicate, [], list)

    /// Async implementation of List.tryFind.
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

    /// Async implementation of List.tryFind.
    [<CompiledName("TryFind")>]
    let tryFind (predicate : 'T -> Async<bool>) (list : 'T list) : Async<'T option> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        tryFindImpl (predicate, list)

    /// Async implementation of List.find.
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

    /// Async implementation of List.tryFindIndex.
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

    /// Async implementation of List.tryFindIndex.
    [<CompiledName("TryFindIndex")>]
    let tryFindIndex (predicate : 'T -> Async<bool>) (list : 'T list) : Async<int option> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        tryFindIndexImpl (predicate, list, 0)

    /// Async implementation of List.findIndex.
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

    /// Async implementation of List.init.
    let rec private initImpl (initializer, initialized : 'T list, count, index) =
        async {
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

    /// Async implementation of List.init.
    [<CompiledName("Initialize")>]
    let init (count : int) (initializer : int -> Async<'T>) : Async<'T list> =
        // Preconditions
        if count < 0 then
            invalidArg "count" "The number of elements to initialize cannot be negative."

        // Call the recursive implementation.
        initImpl (initializer, [], count, 0)

    /// Async implementation of Async.iter.
    let rec private iterImpl (action, pending : 'T list) =
        async {
        match pending with
        | [] ->
            return ()
        | el :: pending ->
            // Apply the action to the current element.
            do! action el

            // Continue processing the rest of the list.
            return! iterImpl (action, pending)
        }

    /// Async implementation of List.iter.
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> Async<unit>) (list : 'T list) : Async<unit> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        iterImpl (action, list)

    /// Async implementation of Async.iter2.
    let rec private iter2Impl (action : FSharpFunc<_,_,_>, list1 : 'T1 list, list2 : 'T2 list) =
        async {
        match list1, list2 with
        | [], [] ->
            return ()
        | el1 :: list1, el2 :: list2 ->
            // Apply the action to the current element.
            do! action.Invoke (el1, el2)

            // Continue processing the rest of the list.
            return! iter2Impl (action, list1, list2)
        | _, _ ->
            // Raise an exception because the lists have different lengths.
            // NOTE : The return keyword is needed here for type-inference reasons.
            return invalidArg "list2" "The lists have different lengths."
        }

    /// <summary>Async implementation of List.iter2.</summary>
    /// <exception cref="System.ArgumentException">Thrown when the input lists differ in length.</exception>
    [<CompiledName("Iterate2")>]
    let iter2 (action : 'T1 -> 'T2 -> Async<unit>) (list1 : 'T1 list) (list2 : 'T2 list) : Async<unit> =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        // Call the recursive implementation.
        let action = FSharpFunc<_,_,_>.Adapt action
        iter2Impl (action, list1, list2)

    /// Async implementation of Async.iteri.
    let rec private iteriImpl (action : FSharpFunc<_,_,_>, pending : 'T list, index) =
        async {
        match pending with
        | [] ->
            return ()
        | el :: pending ->
            // Apply the action to the current element.
            do! action.Invoke (index, el)

            // Continue processing the rest of the list.
            return! iteriImpl (action, pending, index + 1)
        }

    /// Async implementation of List.iteri.
    [<CompiledName("IterateIndexed")>]
    let iteri (action : int -> 'T -> Async<unit>) (list : 'T list) : Async<unit> =
        // Preconditions
        checkNonNull "list" list

        // Call the recursive implementation.
        let action = FSharpFunc<_,_,_>.Adapt action
        iteriImpl (action, list, 0)

    /// Async implementation of Async.iteri2.
    let rec private iteri2Impl (action : FSharpFunc<_,_,_,_>, list1 : 'T1 list, list2 : 'T2 list, index) =
        async {
        match list1, list2 with
        | [], [] ->
            return ()
        | el1 :: list1, el2 :: list2 ->
            // Apply the action to the current element.
            do! action.Invoke (index, el1, el2)

            // Continue processing the rest of the list.
            return! iteri2Impl (action, list1, list2, index + 1)
        | _, _ ->
            // Raise an exception because the lists have different lengths.
            // NOTE : The return keyword is needed here for type-inference reasons.
            return invalidArg "list2" "The lists have different lengths."
        }

    /// <summary>Async implementation of List.iteri2.</summary>
    /// <exception cref="System.ArgumentException">Thrown when the input lists differ in length.</exception>
    [<CompiledName("IterateIndexed2")>]
    let iteri2 (action : int -> 'T1 -> 'T2 -> Async<unit>) (list1 : 'T1 list) (list2 : 'T2 list) : Async<unit> =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        // Call the recursive implementation.
        let action = FSharpFunc<_,_,_,_>.Adapt action
        iteri2Impl (action, list1, list2, 0)


/// The standard F# Seq module, adapted for use within 'async' workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Seq =
    /// Returns a new sequence created by transforming each element of an
    /// sequence into an Async value which returns the element when executed.
    [<CompiledName("Lift")>]
    let lift (source : seq<'T>) : seq<Async<'T>> =
        // Preconditions
        checkNonNull "source" source

        Seq.map async.Return source

(*
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

    //
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Parallel =
        /// Segments a sequence of asynchronously-computed values into batches; the values
        /// within each batch are computed in parallel while the batches are computed
        /// sequentially. Computing the values in batches makes it possible to take advantage
        /// of parallelism when processing a large collection while also avoiding the need
        /// to wait for the entire dataset to be processed before returning.
        [<CompiledName("Batch")>]
        let batch size (sequence : seq<Async<'T>>) : seq<'T> =
            // Preconditions
            checkNonNull "sequence" sequence
            if size < 1 then
                invalidArg "size" "The batch size cannot be less than one (1)."

            // OPTIMIZATION : If the batch size is one, there's no need to bother with Async.Parallel.
            if size = 1 then
                Seq.map Async.RunSynchronously sequence
            else
                sequence
                |> Seq.windowed size
                |> Seq.collect (Async.Parallel >> Async.RunSynchronously)

