﻿(*

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
module ExtCore.Control.Collections.Result
    
open Microsoft.FSharp.Control
open OptimizedClosures
open ExtCore
open ExtCore.Collections


/// The standard F# Array module, lifted into the Choice monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array =
    //
    [<CompiledName("Map")>]
    let map (mapping : 'T -> Result<'U, 'Error>) (array : 'T[]) =
        // Preconditions
        checkNonNull "array" array

        let len = array.Length
        let results = Array.zeroCreate len

        let mutable index = 0
        let mutable error = None

        while index < len && Option.isNone error do
            match mapping array.[index] with
            | Error err ->
                error <- Some err
            | Ok result ->
                results.[index] <- result
                index <- index + 1
            
        // If the error was set, return it; otherwise, return the array of results.
        match error with
        | Some error ->
            Error error
        | None ->
            Ok results

    //
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> Result<'U, 'Error>) (array : 'T[]) =
        // Preconditions
        checkNonNull "array" array

        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        let len = array.Length
        let results = Array.zeroCreate len

        let mutable index = 0
        let mutable error = None

        while index < len && Option.isNone error do
            match mapping.Invoke (index, array.[index]) with
            | Error err ->
                error <- Some err
            | Ok result ->
                results.[index] <- result
                index <- index + 1
            
        // If the error was set, return it; otherwise, return the array of results.
        match error with
        | Some error ->
            Error error
        | None ->
            Ok results

    //
    [<CompiledName("Map2")>]
    let map2 (mapping : 'T1 -> 'T2 -> Result<'U, 'Error>) (array1 : 'T1[]) (array2 : 'T2[]) =
        // Preconditions
        checkNonNull "array1" array1
        checkNonNull "array2" array2

        let len = array1.Length
        if array2.Length <> len then
            invalidArg "array2" "The arrays have differing lengths."

        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        let results = Array.zeroCreate len

        let mutable index = 0
        let mutable error = None

        while index < len && Option.isNone error do
            match mapping.Invoke (array1.[index], array2.[index]) with
            | Error err ->
                error <- Some err
            | Ok result ->
                results.[index] <- result
                index <- index + 1                
            
        // If the error was set, return it; otherwise, return the array of results.
        match error with
        | Some error ->
            Error error
        | None ->
            Ok results

    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> Result<'State, 'Error>) (state : 'State) (array : 'T[]) =
        // Preconditions
        checkNonNull "array" array

        let folder = FSharpFunc<_,_,_>.Adapt folder
        let len = array.Length
        let mutable state = state

        let mutable index = 0
        let mutable error = None

        while index < len && Option.isNone error do
            match folder.Invoke (state, array.[index]) with
            | Error err ->
                error <- Some err
            | Ok newState ->
                state <- newState
                index <- index + 1
            
        // If the error was set, return it; otherwise, return the final state.
        match error with
        | Some error ->
            Error error
        | None ->
            Ok state

    //
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : int -> 'State -> 'T -> Result<'State, 'Error>) (state : 'State) (array : 'T[]) =
        // Preconditions
        checkNonNull "array" array

        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        let len = array.Length
        let mutable state = state

        let mutable index = 0
        let mutable error = None

        while index < len && Option.isNone error do
            match folder.Invoke (index, state, array.[index]) with
            | Error err ->
                error <- Some err
            | Ok newState ->
                state <- newState
                index <- index + 1
            
        // If the error was set, return it; otherwise, return the final state.
        match error with
        | Some error ->
            Error error
        | None ->
            Ok state

    //
    [<CompiledName("Init")>]
    let init (count : int) (initializer : int -> Result<'T, 'Error>) =
        // Preconditions
        if count < 0 then invalidArg "count" "The count cannot be negative."

        let results = Array.zeroCreate count
        let mutable currentIndex = 0
        let mutable error = None

        while currentIndex < count && Option.isNone error do
            match initializer currentIndex with
            | Error err ->
                error <- Some err

            | Ok value ->
                results.[currentIndex] <- value
                currentIndex <- currentIndex + 1

        // If the error is set, return it; otherwise return the initialized array.
        match error with
        | None ->
            Ok results
        | Some error ->
            Error error

    //
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> Result<unit, 'Error>) (array : 'T[]) =
        // Preconditions
        checkNonNull "array" array

        let len = array.Length
        let mutable index = 0
        let mutable error = None

        while index < len && Option.isNone error do
            match action array.[index] with
            | Error err ->
                error <- Some err
            | Ok () ->
                index <- index + 1
            
        // If the error was set, return it.
        match error with
        | Some error ->
            Error error
        | None ->
            Ok ()

    //
    [<CompiledName("IterateIndexed")>]
    let iteri (action : int -> 'T -> Result<unit, 'Error>) (array : 'T[]) =
        // Preconditions
        checkNonNull "array" array

        let action = FSharpFunc<_,_,_>.Adapt action
        let len = array.Length

        let mutable index = 0
        let mutable error = None

        while index < len && Option.isNone error do
            match action.Invoke (index, array.[index]) with
            | Error err ->
                error <- Some err
            | Ok () ->
                index <- index + 1
            
        // If the error was set, return it.
        match error with
        | Some error ->
            Error error
        | None ->
            Ok ()

    //
    [<CompiledName("Reduce")>]
    let reduce (reduction : 'T -> 'T -> Result<'T, 'Error>) (array : 'T[]) =
        // Preconditions
        checkNonNull "array" array
        if Array.isEmpty array then
            invalidArg "array" "The array is empty."

        let reduction = FSharpFunc<_,_,_>.Adapt reduction
        let len = array.Length

        let mutable state = array.[0]   // The first (0-th) element is the initial state.
        let mutable index = 1   // Start at the *second* element (index = 1)
        let mutable error = None

        while index < len && Option.isNone error do
            match reduction.Invoke (state, array.[index]) with
            | Error err ->
                error <- Some err
            | Ok newState ->
                state <- newState
                index <- index + 1
            
        // If the error was set, return it.
        match error with
        | Some error ->
            Error error
        | None ->
            Ok state


/// The standard F# List module, lifted into the Choice monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module List =
    //
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> Result<unit, 'Error>) (list : 'T list) : Result<unit, 'Error> =
        // Preconditions
        checkNonNull "list" list

        let rec iterRec lst =
            match lst with
            | [] ->
                Ok ()
            | hd :: tl ->
                // Apply the action to the head of the list.
                // If the result is an error, return it immediately;
                // otherwise, continue processing the list recursively.
                match action hd with
                | Error error ->
                    Error error
                | Ok () ->
                    iterRec tl

        // Call the recursive implementation function.
        iterRec list

    //
    [<CompiledName("IterateIndexed")>]
    let iteri (action : int -> 'T -> Result<unit, 'Error>) (list : 'T list) : Result<unit, 'Error> =
        // Preconditions
        checkNonNull "list" list

        let action = FSharpFunc<_,_,_>.Adapt action

        let rec iterRec lst index =
            match lst with
            | [] ->
                Ok ()
            | hd :: tl ->
                // Apply the action to the head of the list.
                // If the result is an error, return it immediately;
                // otherwise, continue processing the list recursively.
                match action.Invoke (index, hd) with
                | Error error ->
                    Error error
                | Ok () ->
                    iterRec tl (index + 1)

        // Call the recursive implementation function.
        iterRec list 0

    //
    [<CompiledName("Iterate2")>]
    let iter2 (action : 'T1 -> 'T2 -> Result<unit, 'Error>)
            (list1 : 'T1 list) (list2 : 'T2 list) : Result<unit, 'Error> =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        let action = FSharpFunc<_,_,_>.Adapt action

        let rec iterRec (list1, list2) =
            match list1, list2 with
            | [], [] ->
                Ok ()
                
            | hd1 :: tl1, hd2 :: tl2 ->
                // Apply the function to the heads of the lists.
                // If the result is an error, return it;
                // otherwise continue processing recursively.
                match action.Invoke (hd1, hd2) with
                | Error error ->
                    Error error
                | Ok () ->
                    iterRec (tl1, tl2)

            | _, _ ->
                invalidArg "list2" "The lists have different lengths."
                        
        // Call the recursive implementation function. 
        iterRec (list1, list2)

    //
    [<CompiledName("IterateIndexed2")>]
    let iteri2 (action : int -> 'T1 -> 'T2 -> Result<unit, 'Error>)
            (list1 : 'T1 list) (list2 : 'T2 list) : Result<unit, 'Error> =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        let action = FSharpFunc<_,_,_,_>.Adapt action

        let rec iterRec (list1, list2, index) =
            match list1, list2 with
            | [], [] ->
                Ok ()
                
            | hd1 :: tl1, hd2 :: tl2 ->
                // Apply the function to the heads of the lists.
                // If the result is an error, return it;
                // otherwise continue processing recursively.
                match action.Invoke (index, hd1, hd2) with
                | Error error ->
                    Error error
                | Ok () ->
                    iterRec (tl1, tl2, index + 1)

            | _, _ ->
                invalidArg "list2" "The lists have different lengths."
                        
        // Call the recursive implementation function. 
        iterRec (list1, list2, 0)

    //
    [<CompiledName("Map")>]
    let map (mapping : 'T -> Result<'U, 'Error>) (list : 'T list) =
        // Preconditions
        checkNonNull "list" list

        let rec mapRec acc lst =
            match lst with
            | [] ->
                Ok <| List.rev acc
            | hd :: tl ->
                // Apply the mapping to the head of the list.
                // If the result is an error, return it immediately;
                // otherwise, cons the result onto the accumulator and recurse.
                match mapping hd with
                | Error error ->
                    Error error
                | Ok result ->
                    mapRec (result :: acc) tl

        // Call the recursive implementation function.
        mapRec [] list

    //
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> Result<'U, 'Error>) (list : 'T list) =
        // Preconditions
        checkNonNull "list" list

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        let rec mapRec acc lst index =
            match lst with
            | [] ->
                Ok <| List.rev acc
            | hd :: tl ->
                // Apply the mapping to the head of the list.
                // If the result is an error, return it immediately;
                // otherwise, cons the result onto the accumulator and recurse.
                match mapping.Invoke (index, hd) with
                | Error error ->
                    Error error
                | Ok result ->
                    mapRec (result :: acc) tl (index + 1)

        // Call the recursive implementation function.
        mapRec [] list 0
        
    //
    [<CompiledName("Map2")>]
    let map2 (mapping : 'T1 -> 'T2 -> Result<'U, 'Error>) (list1 : 'T1 list) (list2 : 'T2 list) =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        let rec mapRec (acc, list1, list2) =
            match list1, list2 with
            | [], [] ->
                Ok <| List.rev acc
                
            | hd1 :: tl1, hd2 :: tl2 ->
                // Apply the function to the heads of the lists.
                // If the result is an error, return it;
                // otherwise continue processing recursively.
                match mapping.Invoke (hd1, hd2) with
                | Error error ->
                    Error error
                | Ok result ->
                    mapRec (result :: acc, tl1, tl2)

            | _, _ ->
                invalidArg "list2" "The lists have different lengths."
                        
        // Call the recursive implementation function. 
        mapRec (List.empty, list1, list2)

    //
    [<CompiledName("MapIndexed2")>]
    let mapi2 (mapping : int -> 'T1 -> 'T2 -> Result<'U, 'Error>) (list1 : 'T1 list) (list2 : 'T2 list) =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping

        let rec mapRec (acc, index, list1, list2) =
            match list1, list2 with
            | [], [] ->
                Ok <| List.rev acc
                
            | hd1 :: tl1, hd2 :: tl2 ->
                // Apply the function to the heads of the lists.
                // If the result is an error, return it;
                // otherwise continue processing recursively.
                match mapping.Invoke (index, hd1, hd2) with
                | Error error ->
                    Error error
                | Ok result ->
                    mapRec (result :: acc, index + 1, tl1, tl2)

            | _, _ ->
                invalidArg "list2" "The lists have different lengths."
                        
        // Call the recursive implementation function. 
        mapRec (List.empty, 0, list1, list2)

    //
    [<CompiledName("Map3")>]
    let map3 (mapping : 'T1 -> 'T2 -> 'T3 -> Result<'U, 'Error>) (list1 : 'T1 list) (list2 : 'T2 list) (list3 : 'T3 list) =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2
        checkNonNull "list3" list3

        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping

        let rec mapRec (acc, list1, list2, list3) =
            match list1, list2, list3 with
            | [], [], [] ->
                Ok <| List.rev acc
                
            | hd1 :: tl1, hd2 :: tl2, hd3 :: tl3 ->
                // Apply the function to the heads of the lists.
                // If the result is an error, return it;
                // otherwise continue processing recursively.
                match mapping.Invoke (hd1, hd2, hd3) with
                | Error error ->
                    Error error
                | Ok result ->
                    mapRec (result :: acc, tl1, tl2, tl3)

            | _, _, _ ->
                // TODO : Perhaps provide a better error message here, so we can
                // easily tell which list was shorter than the others.
                invalidArg "list2" "The lists have different lengths."
                        
        // Call the recursive implementation function. 
        mapRec (List.empty, list1, list2, list3)

    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> Result<'State, 'Error>) (state : 'State) (lst : 'T list) =
        // Preconditions
        checkNonNull "lst" lst

        let folder = FSharpFunc<_,_,_>.Adapt folder
            
        let rec foldRec (state, lst) =
            match lst with
            | [] ->
                Ok state
            | hd :: tl ->
                // Apply the function to the head of the list.
                // If the result is an error, return it;
                // otherwise, continue processing recursively.
                match folder.Invoke (state, hd) with
                | (Error _) as error ->
                    error
                | Ok state ->
                    foldRec (state, tl)

        // Call the recursive implementation function.
        foldRec (state, lst)

    //
    [<CompiledName("Fold2")>]
    let fold2 (folder : 'State -> 'T1 -> 'T2 -> Result<'State, 'Error>) (state : 'State) (list1 : 'T1 list) (list2 : 'T2 list) =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        let rec foldRec (list1, list2, state) =
            match list1, list2 with
            | [], [] ->
                Ok state
                
            | hd1 :: tl1, hd2 :: tl2 ->
                // Apply the function to the heads of the lists.
                // If the result is an error, return it;
                // otherwise continue processing recursively.
                match folder.Invoke (state, hd1, hd2) with
                | Error error ->
                    Error error
                | Ok state ->
                    foldRec (tl1, tl2, state)

            | _, _ ->
                invalidArg "list2" "The lists have different lengths."
                        
        // Call the recursive implementation function. 
        foldRec (list1, list2, state)

    //
    [<CompiledName("FoldBack")>]
    let foldBack (folder : 'T -> 'State -> Result<'State, 'Error>) (list : 'T list) (state : 'State) =
        // Preconditions
        checkNonNull "list" list

        let folder = FSharpFunc<_,_,_>.Adapt folder
            
        let rec foldRec (lst, state) =
            match lst with
            | [] ->
                Ok state
            | hd :: tl ->
                // Apply the function to the head of the list.
                // If the result is an error, return it;
                // otherwise, continue processing recursively.
                match folder.Invoke (hd, state) with
                | (Error _) as error ->
                    error
                | Ok state ->
                    foldRec (tl, state)

        // Call the recursive implementation function.
        foldRec (List.rev list, state)

    //
    [<CompiledName("FoldBack2")>]
    let foldBack2 (folder : 'T1 -> 'T2 -> 'State -> Result<'State, 'Error>) (list1 : 'T1 list) (list2 : 'T2 list) (state : 'State) =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        let rec foldRec (list1, list2, state) =
            match list1, list2 with
            | [], [] ->
                Ok state
                
            | hd1 :: tl1, hd2 :: tl2 ->
                // Apply the function to the heads of the lists.
                // If the result is an error, return it;
                // otherwise continue processing recursively.
                match folder.Invoke (hd1, hd2, state) with
                | Error error ->
                    Error error
                | Ok state ->
                    foldRec (tl1, tl2, state)

            | _, _ ->
                invalidArg "list2" "The lists have different lengths."
                        
        // Call the recursive implementation function. 
        foldRec (List.rev list1, List.rev list2, state)

    /// <summary>Apply a function to each element of the collection, threading an accumulator argument
    /// through the computation. Apply the function to the first two elements of the list.
    /// Then feed this result into the function along with the third element and so on.
    /// Return the final result. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes
    /// <c>f (... (f i0 i1) i2 ...) iN</c>.
    /// </summary>
    /// <remarks>Raises <c>System.ArgumentException</c> if <c>list</c> is empty</remarks>
    /// <param name="reduction">The function to reduce two list elements to a single element.</param>
    /// <param name="list">The input list.</param>
    /// <exception cref="System.ArgumentException">Thrown when the list is empty.</exception>
    /// <returns>The final reduced value.</returns>
    [<CompiledName("Reduce")>]
    let reduce (reduction : 'T -> 'T -> Result<'T, 'Error>) (list : 'T list) =
        // Preconditions
        checkNonNull "list" list

        // Extract the first element in the list then fold over the tail, using the first element
        // as the initial state value. If the list contains only one element, we return immediately.
        match list with
        | [] ->
            invalidArg "list" "The input list was empty."
        | [x] ->
            Ok x
        | hd :: tl ->
            fold reduction hd tl

    /// <summary>Applies a function to each element of the collection, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes 
    /// <c>f i0 (...(f iN-1 iN))</c>.
    /// </summary>
    /// <remarks>Raises <c>System.ArgumentException</c> if <c>list</c> is empty</remarks>
    /// <param name="reduction">The function to reduce two list elements to a single element.</param>
    /// <param name="list">The input list.</param>
    /// <exception cref="System.ArgumentException">Thrown when the list is empty.</exception>
    /// <returns>The final reduced value.</returns>
    [<CompiledName("ReduceBack")>]
    let reduceBack (reduction : 'T -> 'T -> Result<'T, 'Error>) (list : 'T list) =
        // Preconditions
        checkNonNull "list" list

        // Extract the first element in the list then fold over the tail, using the first element
        // as the initial state value. If the list contains only one element, we return immediately.
        // NOTE : In order to reduce _backwards_ over the list, we reverse the list before calling fold.
        match List.rev list with
        | [] ->
            invalidArg "list" "The input list was empty."
        | [x] ->
            Ok x
        | hd :: tl ->
            fold reduction hd tl

    //
    [<CompiledName("Exists")>]
    let exists (predicate : 'T -> Result<bool, 'Error>) (list : 'T list) =
        // Preconditions
        checkNonNull "list" list

        let rec existsRec list =
            match list with
            | [] ->
                Ok false
            | hd :: tl ->
                // Apply the predicate to the head of the list.
                // If the result is an error, return it; otherwise, if the result value
                // is 'true', return immediately; otherwise, continue processing recursively.
                match predicate hd with
                | Error _ as error ->
                    error
                | Ok true as result ->
                    result
                | Ok false ->
                    existsRec tl

        // Call the recursive implementation function.
        existsRec list

    //
    [<CompiledName("Exists2")>]
    let exists2 (predicate : 'T1 -> 'T2 -> Result<bool, 'Error>) (list1 : 'T1 list) (list2 : 'T2 list) =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        let predicate = FSharpFunc<_,_,_>.Adapt predicate

        let rec existsRec (list1, list2) =
            match list1, list2 with
            | [], [] ->
                Ok false
            | hd1 :: tl1, hd2 :: tl2 ->
                // Apply the predicate to the heads of the lists.
                // If the result is an error, return it; otherwise, if the result value
                // is 'true', return immediately; otherwise, continue processing recursively.
                match predicate.Invoke (hd1, hd2) with
                | Error _ as error ->
                    error
                | Ok true as result ->
                    result
                | Ok false ->
                    existsRec (tl1, tl2)
            | _, _ ->
                invalidArg "list2" "The lists have different lengths."

        // Call the recursive implementation function.
        existsRec (list1, list2)

    //
    [<CompiledName("Forall")>]
    let forall (predicate : 'T -> Result<bool, 'Error>) (list : 'T list) =
        // Preconditions
        checkNonNull "list" list

        let rec existsRec list =
            match list with
            | [] ->
                Ok true
            | hd :: tl ->
                // Apply the predicate to the head of the list.
                // If the result is an error, return it; otherwise, if the result value
                // is 'false', return immediately; otherwise, continue processing recursively.
                match predicate hd with
                | Error _ as error ->
                    error
                | Ok false as result ->
                    result
                | Ok true ->
                    existsRec tl

        // Call the recursive implementation function.
        existsRec list

    //
    [<CompiledName("Forall2")>]
    let forall2 (predicate : 'T1 -> 'T2 -> Result<bool, 'Error>) (list1 : 'T1 list) (list2 : 'T2 list) =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        let predicate = FSharpFunc<_,_,_>.Adapt predicate

        let rec existsRec (list1, list2) =
            match list1, list2 with
            | [], [] ->
                Ok true
            | hd1 :: tl1, hd2 :: tl2 ->
                // Apply the predicate to the heads of the lists.
                // If the result is an error, return it; otherwise, if the result value
                // is 'false', return immediately; otherwise, continue processing recursively.
                match predicate.Invoke (hd1, hd2) with
                | Error _ as error ->
                    error
                | Ok false as result ->
                    result
                | Ok true ->
                    existsRec (tl1, tl2)
            | _, _ ->
                invalidArg "list2" "The lists have different lengths."

        // Call the recursive implementation function.
        existsRec (list1, list2)

    //
    [<CompiledName("Filter")>]
    let filter (predicate : 'T -> Result<bool, 'Error>) (list : 'T list) =
        // Preconditions
        checkNonNull "list" list

        let rec filterRec (acc, list) =
            match list with
            | [] ->
                Ok <| List.rev acc
            | hd :: tl ->
                // Apply the predicate to the head of the list.
                // If the result is an error, return it.
                // Otherwise, if the result value is 'true', cons the element onto the accumulator
                // and continue processing; otherwise, just continue processing.
                match predicate hd with
                | Error error ->
                    Error error
                | Ok true ->
                    filterRec (hd :: acc, tl)
                | Ok false ->
                    filterRec (acc, tl)

        // Call the recursive implementation function.
        filterRec ([], list)

    //
    [<CompiledName("Choose")>]
    let choose (chooser : 'T -> Result<'U option, 'Error>) (list : 'T list) =
        // Preconditions
        checkNonNull "list" list

        let rec chooseRec (acc, list) =
            match list with
            | [] ->
                Ok <| List.rev acc
            | hd :: tl ->
                // Apply the chooser to the head of the list.
                // If the result is an error, return it.
                // Otherwise, if the result value is 'Some', cons the element onto the accumulator
                // and continue processing; otherwise, just continue processing.
                match chooser hd with
                | Error error ->
                    Error error
                | Ok result ->
                    chooseRec (result %? acc, tl)

        // Call the recursive implementation function.
        chooseRec ([], list)

    //
    [<CompiledName("TryFind")>]
    let tryFind (predicate : 'T -> Result<bool, 'Error>) (list : 'T list) : Result<'T option, 'Error> =
        // Preconditions
        checkNonNull "list" list

        let rec tryFindRec list =
            match list with
            | [] ->
                Ok None
            | hd :: tl ->
                match predicate hd with
                | Error error ->
                    Error error
                | Ok true ->
                    Ok <| Some hd
                | Ok false ->
                    tryFindRec tl

        // Call the recursive implementation function.
        tryFindRec list

    //
    [<CompiledName("Find")>]
    let find (predicate : 'T -> Result<bool, 'Error>) (list : 'T list) : Result<'T, 'Error> =
        // Preconditions
        checkNonNull "list" list

        // Call tryFind -- if it returns None, raise an exception.
        match tryFind predicate list with
        | Error error ->
            Error error
        | Ok (Some result) ->
            Ok result
        | Ok None ->
            // TODO : Provide a better error message here.
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    //
    [<CompiledName("TryPick")>]
    let tryPick (picker : 'T -> Result<'U option, 'Error>) (list : 'T list) : Result<'U option, 'Error> =
        // Preconditions
        checkNonNull "list" list

        let rec tryPickRec list =
            match list with
            | [] ->
                Ok None
            | hd :: tl ->
                match picker hd with
                | Error error ->
                    Error error
                | Ok (Some _ as result) ->
                    Ok result
                | Ok None ->
                    tryPickRec tl

        // Call the recursive implementation function.
        tryPickRec list

    //
    [<CompiledName("Pick")>]
    let pick (picker : 'T -> Result<'U option, 'Error>) (list : 'T list) : Result<'U, 'Error> =
        // Preconditions
        checkNonNull "list" list

        // Call tryPick -- if it returns None, raise an exception.
        match tryPick picker list with
        | Error error ->
            Error error
        | Ok (Some result) ->
            Ok result
        | Ok None ->
            // TODO : Provide a better error message here.
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    //
    [<CompiledName("Partition")>]
    let partition (predicate : 'T -> Result<bool, 'Error>) (list : 'T list) : Result<'T list * 'T list, 'Error> =
        // Preconditions
        checkNonNull "list" list

        let rec partitionRec (trueAcc, falseAcc, list) =
            match list with
            | [] ->
                Ok (List.rev trueAcc, List.rev falseAcc)
            | hd :: tl ->
                match predicate hd with
                | Error error ->
                    Error error
                | Ok true ->
                    partitionRec (hd :: trueAcc, falseAcc, tl)
                | Ok false ->
                    partitionRec (trueAcc, hd :: falseAcc, tl)

        // Call the recursive implementation function.
        partitionRec ([], [], list)


/// The standard F# Seq module, lifted into the Choice monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Seq =
    //
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> Result<unit, 'Error>) (sequence : seq<'T>) : Result<unit, 'Error> =
        // Preconditions
        checkNonNull "sequence" sequence

        let mutable error = None

        use enumerator = sequence.GetEnumerator ()
        while enumerator.MoveNext () && Option.isNone error do
            match action enumerator.Current with
            | Error err ->
                error <- Some err
            | Ok () ->
                ()

        // If the error was set, return it.
        match error with
        | Some error ->
            Error error
        | None ->
            Ok ()

    [<CompiledName("Exists")>]
    let exists (predicate : 'T -> Result<bool, 'Error>) (sequence : seq<'T>) : Result<bool, 'Error> = 
        // Preconditions
        checkNonNull "sequence" sequence

        sequence
        |> Seq.map predicate
        |> Seq.tryPick (function 
               | Ok true -> Some(Ok true)
               | Ok false -> None
               | Error err -> Some(Error err))
        |> defaultArg <| Ok false


/// The standard F# Set module, lifted into the Choice monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Set =
    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> Result<'State, 'Error>) (state : 'State) (set : Set<'T>) =
        // Preconditions
        checkNonNull "set" set

        let folder = FSharpFunc<_,_,_>.Adapt folder

        (* TODO : Is there a better (more performant) way to implement this than using 'IEnumerable'? *)
        use setEnumerator =
            let setAsEnumerable = set :> System.Collections.Generic.IEnumerable<'T>
            setAsEnumerable.GetEnumerator ()

        let mutable state = state
        let mutable error = None

        while setEnumerator.MoveNext () && Option.isNone error do
            match folder.Invoke (state, setEnumerator.Current) with
            | Error err ->
                error <- Some err
            | Ok newState ->
                state <- newState

        // If the error was set, return it; otherwise, return the final state.
        match error with
        | Some error ->
            Error error
        | None ->
            Ok state

    //
    [<CompiledName("MapToArray")>]
    let mapToArray (mapping : 'T -> Result<'U, 'Error>) (set : Set<'T>) : Result<'U[], 'Error> =
        // Preconditions
        checkNonNull "set" set

        let results = Array.zeroCreate <| Set.count set            

        (* TODO : Is there a better (more performant) way to implement this than using 'IEnumerable'? *)
        use setEnumerator =
            let setAsEnumerable = set :> System.Collections.Generic.IEnumerable<'T>
            setAsEnumerable.GetEnumerator ()

        let mutable index = 0
        let mutable error = None

        while setEnumerator.MoveNext () && Option.isNone error do
            match mapping setEnumerator.Current with
            | Error err ->
                error <- Some err
            | Ok result ->
                results.[index] <- result
                index <- index + 1 

        // If the error was set, return it; otherwise, return the final state.
        match error with
        | Some error ->
            Error error
        | None ->
            Ok results


/// The ArrayView module, lifted into the Choice monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ArrayView =
    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> Result<'State, 'Error>)
            (state : 'State) (view : ArrayView<'T>) =
        let folder = FSharpFunc<_,_,_>.Adapt folder

        let array = view.Array
        let endExclusive = view.Offset + view.Count

        let mutable index = view.Offset
        let mutable state = state
        let mutable error = None

        while index < endExclusive && Option.isNone error do
            match folder.Invoke (state, array.[index]) with
            | Error err ->
                error <- Some err
            | Ok state' ->
                state <- state'
                index <- index + 1
            
        // If the error was set, return it.
        match error with
        | Some error ->
            Error error
        | None ->
            Ok state

