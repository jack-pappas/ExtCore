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


/// The standard F# Array module, lifted into the Async monad.
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
        
    (* OPTIMIZE :   Some of the functions below build an Async workflow by folding forwards (left-to-right)
                    over the array. Depending on how async is implemented internally though, it might
                    be more efficient to fold backwards so that as each element is processed it can tail-call
                    the next element. *)

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

    /// Async implementation of Array.fold.
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> Async<'State>) (state : 'State) (array : 'T[]) : Async<'State> =
        // Preconditions
        checkNonNull "array" array

        let folder = FSharpFunc<_,_,_>.Adapt folder

        (async.Return state, array)
        ||> Array.fold (fun stateAsync el ->
            async {
            // Get the state.
            let! state = stateAsync

            // Invoke the folder and return the result.
            return! folder.Invoke (state, el)
            })

    /// Async implementation of Array.foldi.
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : 'State -> int -> 'T -> Async<'State>) (state : 'State) (array : 'T[]) : Async<'State> =
        // Preconditions
        checkNonNull "array" array

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        (async.Return state, array)
        ||> Array.foldi (fun stateAsync index el ->
            async {
            // Get the state.
            let! state = stateAsync

            // Invoke the folder and return the result.
            return! folder.Invoke (state, index, el)
            })

    /// Async implementation of Array.iter.
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> Async<unit>) (array : 'T[]) : Async<unit> =
        // Preconditions
        checkNonNull "array" array

        (async.Zero (), array)
        ||> Array.fold (fun iterPrevious el ->
            async {
            // Execute the workflow for the preceeding elements.
            do! iterPrevious

            // Asynchronously invoke the action for this element.
            do! action el
            })

    /// Async implementation of Array.iteri.
    [<CompiledName("IterateIndexed")>]
    let iteri (action : int -> 'T -> Async<unit>) (array : 'T[]) : Async<unit> =
        // Preconditions
        checkNonNull "array" array

        let action = FSharpFunc<_,_,_>.Adapt action

        (async.Zero (), array)
        ||> Array.foldi (fun iterPrevious index el ->
            async {
            // Execute the workflow for the preceeding elements.
            do! iterPrevious

            // Asynchronously invoke the action for this element.
            do! action.Invoke (index, el)
            })

    (*
    /// Async implementation of Array.reduce.
    [<CompiledName("Reduce")>]
    let reduce (reduction : 'T -> 'T -> Async<'T>) (array : 'T[]) : Async<'T> =
        // Preconditions
        checkNonNull "array" array
        if Array.isEmpty array then
            invalidArg "array" "The array is empty."

        raise <| System.NotImplementedException "ExtCore.Control.Collections.Async.Array.reduce"

    /// Async implementation of Array.tryFind.
    [<CompiledName("TryFind")>]
    let tryFind (predicate : 'T -> Async<bool>) (array : 'T[]) : Async<'T option> =
        // Preconditions
        checkNonNull "array" array
        if Array.isEmpty array then
            invalidArg "array" "The array is empty."

        let len = Array.length array
        let result = ref None
        let mutable index = 0

        async {
        // Loop until we find a matching element, or we run out of elements to try.

        raise <| System.NotImplementedException "ExtCore.Control.Collections.Async.Array.tryFind"
        return None
        }

    /// Async implementation of Array.find.
    [<CompiledName("Find")>]
    let find (predicate : 'T -> Async<bool>) (array : 'T[]) : Async<'T> =
        // Preconditions
        checkNonNull "array" array
        if Array.isEmpty array then
            invalidArg "array" "The array is empty."

        raise <| System.NotImplementedException "ExtCore.Control.Collections.Async.Array.find"
    *)

    // TODO : mapReduce

/// Functions for manipulating lists within 'async' workflows.
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

    /// Async implementation of List.collect.
    // OPTIMIZE : It may be possible to reduce memory usage by processing the "outer" list
    // backwards (like List.foldBack), in which case we could append each of the resulting
    // lists to an accumulator and we wouldn't need to reverse the result at the end.
    let rec private collectImpl (mapping, collected : 'U list, pending : 'T list) =
        async {
        match pending with
        | [] ->
            // Return the results before returning.
            return List.rev collected

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


/// Functions for manipulating sequences within 'async' workflows.
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
                sequence
                |> Seq.map Async.RunSynchronously
            else
                sequence
                |> Seq.windowed size
                |> Seq.collect (Async.Parallel >> Async.RunSynchronously)

