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
module ExtCore.Control.Collections.ReaderResult
    
open Microsoft.FSharp.Control
open OptimizedClosures
open ExtCore
open ExtCore.Collections


/// The standard F# Array module, lifted into the ReaderChoice monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array =
    //
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'Env -> Result<'U, 'Error>) (array : 'T[]) (env : 'Env) =
        // Preconditions
        checkNonNull "array" array

        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        let len = array.Length
        let results = Array.zeroCreate len

        let mutable index = 0
        let mutable error = None

        while index < len && Option.isNone error do
            match mapping.Invoke (array.[index], env) with
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
    let mapi (mapping : int -> 'T -> 'Env -> Result<'U, 'Error>) (array : 'T[]) (env : 'Env) =
        // Preconditions
        checkNonNull "array" array

        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
        let len = array.Length
        let results = Array.zeroCreate len

        let mutable index = 0
        let mutable error = None

        while index < len && Option.isNone error do
            match mapping.Invoke (index, array.[index], env) with
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
    let map2 (mapping : 'T1 -> 'T2 -> 'Env -> Result<'U, 'Error>)
            (array1 : 'T1[]) (array2 : 'T2[]) (env : 'Env) =
        // Preconditions
        checkNonNull "array1" array1
        checkNonNull "array2" array2

        let len = array1.Length
        if array2.Length <> len then
            invalidArg "array2" "The arrays have differing lengths."

        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
        let results = Array.zeroCreate len

        let mutable index = 0
        let mutable error = None

        while index < len && Option.isNone error do
            match mapping.Invoke (array1.[index], array2.[index], env) with
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
    let fold (folder : 'State -> 'T -> 'Env -> Result<'State, 'Error>)
            (state : 'State) (array : 'T[]) (env : 'Env) =
        // Preconditions
        checkNonNull "array" array

        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        let len = array.Length
        let mutable state = state

        let mutable index = 0
        let mutable error = None

        while index < len && Option.isNone error do
            match folder.Invoke (state, array.[index], env) with
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
    let foldi (folder : int -> 'State -> 'T -> 'Env -> Result<'State, 'Error>)
            (state : 'State) (array : 'T[]) (env : 'Env) =
        // Preconditions
        checkNonNull "array" array

        let folder = FSharpFunc<_,_,_,_,_>.Adapt folder
        let len = array.Length
        let mutable state = state

        let mutable index = 0
        let mutable error = None

        while index < len && Option.isNone error do
            match folder.Invoke (index, state, array.[index], env) with
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
    let init (count : int) (initializer : int -> 'Env -> Result<'T, 'Error>) (env : 'Env) =
        // Preconditions
        if count < 0 then
            invalidArg "count" "The count cannot be negative."

        let initializer = FSharpFunc<_,_,_>.Adapt initializer
        let results = Array.zeroCreate count
        let mutable currentIndex = 0
        let mutable error = None

        while currentIndex < count && Option.isNone error do
            match initializer.Invoke (currentIndex, env) with
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
    let iter (action : 'T -> 'Env -> Result<unit, 'Error>) (array : 'T[]) (env : 'Env) =
        // Preconditions
        checkNonNull "array" array

        let action = FSharpFunc<_,_,_>.Adapt action
        let len = array.Length
        let mutable index = 0
        let mutable error = None

        while index < len && Option.isNone error do
            match action.Invoke (array.[index], env) with
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
    let iteri (action : int -> 'T -> 'Env -> Result<unit, 'Error>) (array : 'T[]) (env : 'Env) =
        // Preconditions
        checkNonNull "array" array

        let action = FSharpFunc<_,_,_,_>.Adapt action
        let len = array.Length

        let mutable index = 0
        let mutable error = None

        while index < len && Option.isNone error do
            match action.Invoke (index, array.[index], env) with
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
    let reduce (reduction : 'T -> 'T -> 'Env -> Result<'T, 'Error>) (array : 'T[]) (env : 'Env) =
        // Preconditions
        checkNonNull "array" array
        if Array.isEmpty array then
            invalidArg "array" "The array is empty."

        let reduction = FSharpFunc<_,_,_,_>.Adapt reduction
        let len = array.Length

        let mutable state = array.[0]   // The first (0-th) element is the initial state.
        let mutable index = 1   // Start at the *second* element (index = 1)
        let mutable error = None

        while index < len && Option.isNone error do
            match reduction.Invoke (state, array.[index], env) with
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


/// The standard F# List module, lifted into the ReaderChoice monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module List =
    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> 'Env -> Result<'State, 'Error>)
            (state : 'State) (lst : 'T list) (env : 'Env) =
        // Preconditions
        checkNonNull "lst" lst

        let folder = FSharpFunc<_,_,_,_>.Adapt folder
            
        let rec foldRec (state, lst) =
            match lst with
            | [] ->
                Ok state
            | hd :: tl ->
                // Apply the function to the head of the list.
                // If the result is an error, return it;
                // otherwise, continue processing recursively.
                match folder.Invoke (state, hd, env) with
                | (Error _) as error ->
                    error
                | Ok state ->
                    foldRec (state, tl)

        // Call the recursive implementation function.
        foldRec (state, lst)
        
    //
    [<CompiledName("Map2")>]
    let map2 (mapping : 'T1 -> 'T2 -> 'Env -> Result<'U, 'Error>)
            (list1 : 'T1 list) (list2 : 'T2 list) (env : 'Env) =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2
        if List.length list1 <> List.length list2 then
            invalidArg "list2" "The lists have different lengths."

        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping

        let rec mapRec (acc, list1, list2) =
            match list1, list2 with
            | [], [] ->
                List.rev acc
                |> Ok
                
            | hd1 :: tl1, hd2 :: tl2 ->
                // Apply the function to the heads of the lists.
                // If the result is an error, return it;
                // otherwise continue processing recursively.
                match mapping.Invoke (hd1, hd2, env) with
                | Error error ->
                    Error error
                | Ok result ->
                    mapRec (result :: acc, tl1, tl2)

            | _, _ ->
                failwith "The lists have differing lengths -- they may have been modified in some invalid way."
                        
        // Call the recursive implementation function. 
        mapRec (List.empty, list1, list2)

    //
    [<CompiledName("MapIndexed2")>]
    let mapi2 (mapping : int -> 'T1 -> 'T2 -> 'Env -> Result<'U, 'Error>)
            (list1 : 'T1 list) (list2 : 'T2 list) (env : 'Env) =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2
        if List.length list1 <> List.length list2 then
            invalidArg "list2" "The lists have different lengths."

        let mapping = FSharpFunc<_,_,_,_,_>.Adapt mapping

        let rec mapRec (acc, index, list1, list2) =
            match list1, list2 with
            | [], [] ->
                List.rev acc
                |> Ok
                
            | hd1 :: tl1, hd2 :: tl2 ->
                // Apply the function to the heads of the lists.
                // If the result is an error, return it;
                // otherwise continue processing recursively.
                match mapping.Invoke (index, hd1, hd2, env) with
                | Error error ->
                    Error error
                | Ok result ->
                    mapRec (result :: acc, index + 1, tl1, tl2)

            | _, _ ->
                failwith "The lists have differing lengths -- they may have been modified in some invalid way."
                        
        // Call the recursive implementation function. 
        mapRec (List.empty, 0, list1, list2)

    //
    [<CompiledName("Iterate2")>]
    let iter2 (action : 'T1 -> 'T2 -> 'Env -> Result<unit, 'Error>)
            (list1 : 'T1 list) (list2 : 'T2 list) (env : 'Env) : Result<unit, 'Error> =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2
        if List.length list1 <> List.length list2 then
            invalidArg "list2" "The lists have different lengths."

        let action = FSharpFunc<_,_,_,_>.Adapt action

        let rec mapRec (list1, list2) =
            match list1, list2 with
            | [], [] ->
                Ok ()
                
            | hd1 :: tl1, hd2 :: tl2 ->
                // Apply the function to the heads of the lists.
                // If the result is an error, return it;
                // otherwise continue processing recursively.
                match action.Invoke (hd1, hd2, env) with
                | Error error ->
                    Error error
                | Ok () ->
                    mapRec (tl1, tl2)

            | _, _ ->
                failwith "The lists have differing lengths -- they may have been modified in some invalid way."
                        
        // Call the recursive implementation function. 
        mapRec (list1, list2)

/// The standard F# Seq module, lifted into the ReaderChoice monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Seq =
    //
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> 'Env -> Result<unit, 'Error>)
            (sequence : seq<'T>) (env : 'Env) : Result<unit, 'Error> =
        // Preconditions
        checkNonNull "seq" seq

        let action = FSharpFunc<_,_,_>.Adapt action
        let mutable error = None

        use enumerator = sequence.GetEnumerator ()
        while enumerator.MoveNext () && Option.isNone error do
            match action.Invoke (enumerator.Current, env) with
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


/// The standard F# Set module, lifted into the ReaderChoice monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Set =
    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> 'Env -> Result<'State, 'Error>)
            (state : 'State) (set : Set<'T>) (env : 'Env) =
        // Preconditions
        checkNonNull "set" set

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        (* TODO : Is there a better (more performant) way to implement this than using 'IEnumerable'? *)
        use setEnumerator =
            let setAsEnumerable = set :> System.Collections.Generic.IEnumerable<'T>
            setAsEnumerable.GetEnumerator ()

        let mutable state = state
        let mutable error = None

        while setEnumerator.MoveNext () && Option.isNone error do
            match folder.Invoke (state, setEnumerator.Current, env) with
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
    let mapToArray (mapping : 'T -> 'Env -> Result<'U, 'Error>)
            (set : Set<'T>) (env : 'Env) : Result<'U[], 'Error> =
        // Preconditions
        checkNonNull "set" set

        let results = Array.zeroCreate <| Set.count set            

        (* TODO : Is there a better (more performant) way to implement this than using 'IEnumerable'? *)
        use setEnumerator =
            let setAsEnumerable = set :> System.Collections.Generic.IEnumerable<'T>
            setAsEnumerable.GetEnumerator ()

        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        let mutable index = 0
        let mutable error = None

        while setEnumerator.MoveNext () && Option.isNone error do
            match mapping.Invoke (setEnumerator.Current, env) with
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


/// The ArrayView module, lifted into the ReaderChoice monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ArrayView =
    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> 'Env -> Result<'State, 'Error>)
            (state : 'State) (view : ArrayView<'T>) (env : 'Env) =
        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        let array = view.Array
        let endExclusive = view.Offset + view.Count

        let mutable index = view.Offset
        let mutable state = state
        let mutable error = None

        while index < endExclusive && Option.isNone error do
            match folder.Invoke (state, array.[index], env) with
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

