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
        
    (* OPTIMIZE :   Some of the functions below build an Async workflow by folding forwards (left-to-right)
                    over the array. Depending on how async is implemented internally though, it might
                    be more efficient to fold backwards so that as each element is processed it can tail-call
                    the next element. *)

    //
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

    //
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

    //
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

    //
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

    //
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

    //
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

    //
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

    //
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

    //
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
    //
    [<CompiledName("Reduce")>]
    let reduce (reduction : 'T -> 'T -> Async<'T>) (array : 'T[]) : Async<'T> =
        // Preconditions
        checkNonNull "array" array
        if Array.isEmpty array then
            invalidArg "array" "The array is empty."

        raise <| System.NotImplementedException "ExtCore.Control.Collections.Async.Array.reduce"

    //
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

    //
    [<CompiledName("Find")>]
    let find (predicate : 'T -> Async<bool>) (array : 'T[]) : Async<'T> =
        // Preconditions
        checkNonNull "array" array
        if Array.isEmpty array then
            invalidArg "array" "The array is empty."

        raise <| System.NotImplementedException "ExtCore.Control.Collections.Async.Array.find"
    *)

    // TODO : mapReduce


/// Functions for manipulating sequences within 'async' workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Seq =
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

