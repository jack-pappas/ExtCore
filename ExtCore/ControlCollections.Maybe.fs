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
module ExtCore.Control.Collections.Maybe
    
open Microsoft.FSharp.Control
open OptimizedClosures
open ExtCore
open ExtCore.Collections


/// The standard F# Array module, lifted into the Maybe monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array =
    //
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'U option) (array : 'T[]) : 'U[] option =
        // Preconditions
        checkNonNull "array" array

        let len = array.Length
        let results = Array.zeroCreate len

        let mutable index = 0
        let mutable foundError = false

        while index < len && not foundError do
            match mapping array.[index] with
            | None ->
                foundError <- true
            | Some result ->
                results.[index] <- result
                index <- index + 1                
            
        // If an error was encountered, return None.
        if foundError then None
        else Some results

    //
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> 'U option) (array : 'T[]) : 'U[] option =
        // Preconditions
        checkNonNull "array" array

        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        let len = array.Length
        let results = Array.zeroCreate len

        let mutable index = 0
        let mutable foundError = false

        while index < len && not foundError do
            match mapping.Invoke (index, array.[index]) with
            | None ->
                foundError <- true
            | Some result ->
                results.[index] <- result
                index <- index + 1                
            
        // If an error was encountered, return None.
        if foundError then None
        else Some results

    //
    [<CompiledName("Map2")>]
    let map2 (mapping : 'T1 -> 'T2 -> 'U option) (array1 : 'T1[]) (array2 : 'T2[]) : 'U[] option =
        // Preconditions
        checkNonNull "array1" array1
        checkNonNull "array2" array2

        let len = array1.Length
        if array2.Length <> len then
            invalidArg "array2" "The arrays have differing lengths."

        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        let results = Array.zeroCreate len

        let mutable index = 0
        let mutable foundError = false

        while index < len && not foundError do
            match mapping.Invoke (array1.[index], array2.[index]) with
            | None ->
                foundError <- true
            | Some result ->
                results.[index] <- result
                index <- index + 1                
            
        // If an error was encountered, return None.
        if foundError then None
        else Some results

    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> 'State option) (state : 'State) (array : 'T[]) : 'State option =
        // Preconditions
        checkNonNull "array" array

        let folder = FSharpFunc<_,_,_>.Adapt folder
        let len = array.Length
        let mutable state = state

        let mutable index = 0
        let mutable foundError = false

        while index < len && not foundError do
            match folder.Invoke (state, array.[index]) with
            | None ->
                foundError <- true
            | Some newState ->
                state <- newState
                index <- index + 1
            
        // If an error was encountered, return None.
        if foundError then None
        else Some state

    //
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : int -> 'State -> 'T -> 'State option) (state : 'State) (array : 'T[]) : 'State option =
        // Preconditions
        checkNonNull "array" array

        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        let len = array.Length
        let mutable state = state

        let mutable index = 0
        let mutable foundError = false

        while index < len && not foundError do
            match folder.Invoke (index, state, array.[index]) with
            | None ->
                foundError <- true
            | Some newState ->
                state <- newState
                index <- index + 1
            
        // If an error was encountered, return None.
        if foundError then None
        else Some state

    //
    [<CompiledName("Init")>]
    let init (count : int) (initializer : int -> 'T option) : 'T[] option =
        // Preconditions
        if count < 0 then invalidArg "count" "The count cannot be negative."

        let results = Array.zeroCreate count
        let mutable currentIndex = 0
        let mutable foundError = false

        while currentIndex < count && not foundError do
            match initializer currentIndex with
            | None ->
                foundError <- true
            | Some value ->
                results.[currentIndex] <- value
                currentIndex <- currentIndex + 1

        // If an error was encountered, return None.
        if foundError then None
        else Some results

    //
    [<CompiledName("Iter")>]
    let iter (action : 'T -> unit option) (array : 'T[]) : unit option =
        // Preconditions
        checkNonNull "array" array

        let len = array.Length
        let mutable index = 0
        let mutable foundError = false

        while index < len && not foundError do
            match action array.[index] with
            | None ->
                foundError <- true
            | Some () ->
                index <- index + 1
            
        // If an error was encountered, return None.
        if foundError then None
        else Some ()

    //
    [<CompiledName("IterIndexed")>]
    let iteri (action : int -> 'T -> unit option) (array : 'T[]) : unit option =
        // Preconditions
        checkNonNull "array" array

        let action = FSharpFunc<_,_,_>.Adapt action
        let len = array.Length

        let mutable index = 0
        let mutable foundError = false

        while index < len && not foundError do
            match action.Invoke (index, array.[index]) with
            | None ->
                foundError <- true
            | Some () ->
                index <- index + 1
            
        // If an error was encountered, return None.
        if foundError then None
        else Some ()

    //
    [<CompiledName("Reduce")>]
    let reduce (reduction : 'T -> 'T -> 'T option) (array : 'T[]) : 'T option =
        // Preconditions
        checkNonNull "array" array
        if Array.isEmpty array then
            invalidArg "array" "The array is empty."

        let reduction = FSharpFunc<_,_,_>.Adapt reduction
        let len = array.Length

        let mutable state = array.[0]   // The first (0-th) element is the initial state.
        let mutable index = 1   // Start at the *second* element (index = 1)
        let mutable foundError = false

        while index < len && not foundError do
            match reduction.Invoke (state, array.[index]) with
            | None ->
                foundError <- true
            | Some newState ->
                state <- newState
                index <- index + 1
            
        // If an error was encountered, return None.
        if foundError then None
        else Some state


/// The standard F# List module, lifted into the Maybe monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module List =
    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> 'State option) (state : 'State) (lst : 'T list) : 'State option =
        // Preconditions
        checkNonNull "lst" lst

        let folder = FSharpFunc<_,_,_>.Adapt folder
            
        let rec foldRec (state, lst) =
            match lst with
            | [] ->
                Some state
            | hd :: tl ->
                // Apply the function to the head of the list.
                // If the result is an error, return it;
                // otherwise, continue processing recursively.
                match folder.Invoke (state, hd) with
                | None ->
                    None
                | Some state ->
                    foldRec (state, tl)

        // Call the recursive implementation function.
        foldRec (state, lst)
        
    //
    [<CompiledName("Map2")>]
    let map2 (mapping : 'T1 -> 'T2 -> 'U option) (list1 : 'T1 list) (list2 : 'T2 list) : 'U list option =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2
        if List.length list1 <> List.length list2 then
            invalidArg "list2" "The lists have different lengths."

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        let rec mapRec (acc, list1, list2) =
            match list1, list2 with
            | [], [] ->
                Some <| List.rev acc
                
            | hd1 :: tl1, hd2 :: tl2 ->
                // Apply the function to the heads of the lists.
                // If the result is an error, return it;
                // otherwise continue processing recursively.
                match mapping.Invoke (hd1, hd2) with
                | None ->
                    None
                | Some result ->
                    mapRec (result :: acc, tl1, tl2)

            | _, _ ->
                failwith "The lists have differing lengths -- they may have been modified in some invalid way."
                        
        // Call the recursive implementation function. 
        mapRec (List.empty, list1, list2)

    //
    [<CompiledName("MapIndexed2")>]
    let mapi2 (mapping : int -> 'T1 -> 'T2 -> 'U option) (list1 : 'T1 list) (list2 : 'T2 list) : 'U list option =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2
        if List.length list1 <> List.length list2 then
            invalidArg "list2" "The lists have different lengths."

        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping

        let rec mapRec (acc, index, list1, list2) =
            match list1, list2 with
            | [], [] ->
                Some <| List.rev acc
                
            | hd1 :: tl1, hd2 :: tl2 ->
                // Apply the function to the heads of the lists.
                // If the result is an error, return it;
                // otherwise continue processing recursively.
                match mapping.Invoke (index, hd1, hd2) with
                | None ->
                    None
                | Some result ->
                    mapRec (result :: acc, index + 1, tl1, tl2)

            | _, _ ->
                failwith "The lists have differing lengths -- they may have been modified in some invalid way."
                        
        // Call the recursive implementation function.
        mapRec (List.empty, 0, list1, list2)

    //
    [<CompiledName("Iter2")>]
    let iter2 (action : 'T1 -> 'T2 -> unit option)
            (list1 : 'T1 list) (list2 : 'T2 list) : unit option =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2
        if List.length list1 <> List.length list2 then
            invalidArg "list2" "The lists have different lengths."

        let action = FSharpFunc<_,_,_>.Adapt action

        let rec iterRec (list1, list2) =
            match list1, list2 with
            | [], [] ->
                Some ()
                
            | hd1 :: tl1, hd2 :: tl2 ->
                // Apply the function to the heads of the lists.
                // If the result is an error, return it;
                // otherwise continue processing recursively.
                match action.Invoke (hd1, hd2) with
                | None ->
                    None
                | Some () ->
                    iterRec (tl1, tl2)

            | _, _ ->
                failwith "The lists have differing lengths -- they may have been modified in some invalid way."
                        
        // Call the recursive implementation function. 
        iterRec (list1, list2)


/// The standard F# Seq module, lifted into the Maybe monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Seq =
    //
    [<CompiledName("Iter")>]
    let iter (action : 'T -> unit option) (sequence : seq<'T>) : unit option =
        // Preconditions
        checkNonNull "seq" seq

        let mutable foundError = false

        let enumerator = sequence.GetEnumerator ()
        while enumerator.MoveNext () && not foundError do
            match action enumerator.Current with
            | None ->
                foundError <- true
            | Some () ->
                ()

        // If an error was encountered, return None.
        if foundError then None
        else Some ()


/// The standard F# Set module, lifted into the Maybe monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Set =
    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> 'State option) (state : 'State) (set : Set<'T>) : 'State option =
        // Preconditions
        checkNonNull "set" set

        let folder = FSharpFunc<_,_,_>.Adapt folder

        (* TODO : Is there a better (more performant) way to implement this than using 'IEnumerable'? *)
        use setEnumerator =
            let setAsEnumerable = set :> System.Collections.Generic.IEnumerable<'T>
            setAsEnumerable.GetEnumerator ()

        let mutable state = state
        let mutable foundError = false

        while setEnumerator.MoveNext () && not foundError do
            match folder.Invoke (state, setEnumerator.Current) with
            | None ->
                foundError <- true
            | Some newState ->
                state <- newState

        // If an error was encountered, return None.
        if foundError then None
        else Some state

    //
    [<CompiledName("MapToArray")>]
    let mapToArray (mapping : 'T -> 'U option) (set : Set<'T>) : 'U[] option =
        // Preconditions
        checkNonNull "set" set

        let results = Array.zeroCreate <| Set.count set            

        (* TODO : Is there a better (more performant) way to implement this than using 'IEnumerable'? *)
        use setEnumerator =
            let setAsEnumerable = set :> System.Collections.Generic.IEnumerable<'T>
            setAsEnumerable.GetEnumerator ()

        let mutable index = 0
        let mutable foundError = false

        while setEnumerator.MoveNext () && not foundError do
            match mapping setEnumerator.Current with
            | None ->
                foundError <- true
            | Some result ->
                results.[index] <- result
                index <- index + 1 

        // If an error was encountered, return None.
        if foundError then None
        else Some results


/// The ArrayView module, lifted into the Maybe monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ArrayView =
    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> 'State option)
            (state : 'State) (view : ArrayView<'T>) : 'State option =
        let folder = FSharpFunc<_,_,_>.Adapt folder

        let array = view.Array
        let endExclusive = view.Offset + view.Count

        let mutable index = view.Offset
        let mutable state = state
        let mutable foundError = false

        while index < endExclusive && not foundError do
            match folder.Invoke (state, array.[index]) with
            | None ->
                foundError <- true
            | Some state' ->
                state <- state'
                index <- index + 1
            
        // If an error was encountered, return None.
        if foundError then None
        else Some state

