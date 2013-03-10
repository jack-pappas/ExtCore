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
module ExtCore.Control.Collections.Choice
    
open Microsoft.FSharp.Control
open OptimizedClosures
open ExtCore
open ExtCore.Collections


/// The standard F# Array module, lifted into the Choice monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array =
    //
    [<CompiledName("Map")>]
    let map (mapping : 'T -> Choice<'U, 'Error>) (array : 'T[]) =
        // Preconditions
        checkNonNull "array" array

        let len = array.Length
        let results = Array.zeroCreate len

        let mutable index = 0
        let mutable error = None

        while index < len && Option.isNone error do
            match mapping array.[index] with
            | Choice2Of2 err ->
                error <- Some err
            | Choice1Of2 result ->
                results.[index] <- result
                index <- index + 1
            
        // If the error was set, return it; otherwise, return the array of results.
        match error with
        | Some error ->
            Choice2Of2 error
        | None ->
            Choice1Of2 results

    //
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> Choice<'U, 'Error>) (array : 'T[]) =
        // Preconditions
        checkNonNull "array" array

        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        let len = array.Length
        let results = Array.zeroCreate len

        let mutable index = 0
        let mutable error = None

        while index < len && Option.isNone error do
            match mapping.Invoke (index, array.[index]) with
            | Choice2Of2 err ->
                error <- Some err
            | Choice1Of2 result ->
                results.[index] <- result
                index <- index + 1
            
        // If the error was set, return it; otherwise, return the array of results.
        match error with
        | Some error ->
            Choice2Of2 error
        | None ->
            Choice1Of2 results

    //
    [<CompiledName("Map2")>]
    let map2 (mapping : 'T1 -> 'T2 -> Choice<'U, 'Error>) (array1 : 'T1[]) (array2 : 'T2[]) =
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
            | Choice2Of2 err ->
                error <- Some err
            | Choice1Of2 result ->
                results.[index] <- result
                index <- index + 1                
            
        // If the error was set, return it; otherwise, return the array of results.
        match error with
        | Some error ->
            Choice2Of2 error
        | None ->
            Choice1Of2 results

    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> Choice<'State, 'Error>) (state : 'State) (array : 'T[]) =
        // Preconditions
        checkNonNull "array" array

        let folder = FSharpFunc<_,_,_>.Adapt folder
        let len = array.Length
        let mutable state = state

        let mutable index = 0
        let mutable error = None

        while index < len && Option.isNone error do
            match folder.Invoke (state, array.[index]) with
            | Choice2Of2 err ->
                error <- Some err
            | Choice1Of2 newState ->
                state <- newState
                index <- index + 1
            
        // If the error was set, return it; otherwise, return the final state.
        match error with
        | Some error ->
            Choice2Of2 error
        | None ->
            Choice1Of2 state

    //
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : int -> 'State -> 'T -> Choice<'State, 'Error>) (state : 'State) (array : 'T[]) =
        // Preconditions
        checkNonNull "array" array

        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        let len = array.Length
        let mutable state = state

        let mutable index = 0
        let mutable error = None

        while index < len && Option.isNone error do
            match folder.Invoke (index, state, array.[index]) with
            | Choice2Of2 err ->
                error <- Some err
            | Choice1Of2 newState ->
                state <- newState
                index <- index + 1
            
        // If the error was set, return it; otherwise, return the final state.
        match error with
        | Some error ->
            Choice2Of2 error
        | None ->
            Choice1Of2 state

    //
    [<CompiledName("Initialize")>]
    let init (count : int) (initializer : int -> Choice<'T, 'Error>) =
        // Preconditions
        if count < 0 then invalidArg "count" "The count cannot be negative."

        let results = Array.zeroCreate count
        let mutable currentIndex = 0
        let mutable error = None

        while currentIndex < count && Option.isNone error do
            match initializer currentIndex with
            | Choice2Of2 err ->
                error <- Some err

            | Choice1Of2 value ->
                results.[currentIndex] <- value
                currentIndex <- currentIndex + 1

        // If the error is set, return it; otherwise return the initialized array.
        match error with
        | None ->
            Choice1Of2 results
        | Some error ->
            Choice2Of2 error

    //
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> Choice<unit, 'Error>) (array : 'T[]) =
        // Preconditions
        checkNonNull "array" array

        let len = array.Length
        let mutable index = 0
        let mutable error = None

        while index < len && Option.isNone error do
            match action array.[index] with
            | Choice2Of2 err ->
                error <- Some err
            | Choice1Of2 () ->
                index <- index + 1
            
        // If the error was set, return it.
        match error with
        | Some error ->
            Choice2Of2 error
        | None ->
            Choice1Of2 ()

    //
    [<CompiledName("IterateIndexed")>]
    let iteri (action : int -> 'T -> Choice<unit, 'Error>) (array : 'T[]) =
        // Preconditions
        checkNonNull "array" array

        let action = FSharpFunc<_,_,_>.Adapt action
        let len = array.Length

        let mutable index = 0
        let mutable error = None

        while index < len && Option.isNone error do
            match action.Invoke (index, array.[index]) with
            | Choice2Of2 err ->
                error <- Some err
            | Choice1Of2 () ->
                index <- index + 1
            
        // If the error was set, return it.
        match error with
        | Some error ->
            Choice2Of2 error
        | None ->
            Choice1Of2 ()

    //
    [<CompiledName("Reduce")>]
    let reduce (reduction : 'T -> 'T -> Choice<'T, 'Error>) (array : 'T[]) =
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
            | Choice2Of2 err ->
                error <- Some err
            | Choice1Of2 newState ->
                state <- newState
                index <- index + 1
            
        // If the error was set, return it.
        match error with
        | Some error ->
            Choice2Of2 error
        | None ->
            Choice1Of2 state


/// The standard F# List module, lifted into the Choice monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module List =
    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> Choice<'State, 'Error>) (state : 'State) (lst : 'T list) =
        // Preconditions
        checkNonNull "lst" lst

        let folder = FSharpFunc<_,_,_>.Adapt folder
            
        let rec foldRec (state, lst) =
            match lst with
            | [] ->
                Choice1Of2 state
            | hd :: tl ->
                // Apply the function to the head of the list.
                // If the result is an error, return it;
                // otherwise, continue processing recursively.
                match folder.Invoke (state, hd) with
                | (Choice2Of2 _) as error ->
                    error
                | Choice1Of2 state ->
                    foldRec (state, tl)

        // Call the recursive implementation function.
        foldRec (state, lst)
        
    //
    [<CompiledName("Map2")>]
    let map2 (mapping : 'T1 -> 'T2 -> Choice<'U, 'Error>) (list1 : 'T1 list) (list2 : 'T2 list) =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2
        if List.length list1 <> List.length list2 then
            invalidArg "list2" "The lists have different lengths."

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        let rec mapRec (acc, list1, list2) =
            match list1, list2 with
            | [], [] ->
                Choice1Of2 <| List.rev acc
                
            | hd1 :: tl1, hd2 :: tl2 ->
                // Apply the function to the heads of the lists.
                // If the result is an error, return it;
                // otherwise continue processing recursively.
                match mapping.Invoke (hd1, hd2) with
                | Choice2Of2 error ->
                    Choice2Of2 error
                | Choice1Of2 result ->
                    mapRec (result :: acc, tl1, tl2)

            | _, _ ->
                failwith "The lists have differing lengths -- they may have been modified in some invalid way."
                        
        // Call the recursive implementation function. 
        mapRec (List.empty, list1, list2)

    //
    [<CompiledName("MapIndexed2")>]
    let mapi2 (mapping : int -> 'T1 -> 'T2 -> Choice<'U, 'Error>) (list1 : 'T1 list) (list2 : 'T2 list) =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2
        if List.length list1 <> List.length list2 then
            invalidArg "list2" "The lists have different lengths."

        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping

        let rec mapRec (acc, index, list1, list2) =
            match list1, list2 with
            | [], [] ->
                Choice1Of2 <| List.rev acc
                
            | hd1 :: tl1, hd2 :: tl2 ->
                // Apply the function to the heads of the lists.
                // If the result is an error, return it;
                // otherwise continue processing recursively.
                match mapping.Invoke (index, hd1, hd2) with
                | Choice2Of2 error ->
                    Choice2Of2 error
                | Choice1Of2 result ->
                    mapRec (result :: acc, index + 1, tl1, tl2)

            | _, _ ->
                failwith "The lists have differing lengths -- they may have been modified in some invalid way."
                        
        // Call the recursive implementation function. 
        mapRec (List.empty, 0, list1, list2)

    //
    [<CompiledName("Iterate2")>]
    let iter2 (action : 'T1 -> 'T2 -> Choice<unit, 'Error>)
            (list1 : 'T1 list) (list2 : 'T2 list) : Choice<unit, 'Error> =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2
        if List.length list1 <> List.length list2 then
            invalidArg "list2" "The lists have different lengths."

        let action = FSharpFunc<_,_,_>.Adapt action

        let rec mapRec (list1, list2) =
            match list1, list2 with
            | [], [] ->
                Choice1Of2 ()
                
            | hd1 :: tl1, hd2 :: tl2 ->
                // Apply the function to the heads of the lists.
                // If the result is an error, return it;
                // otherwise continue processing recursively.
                match action.Invoke (hd1, hd2) with
                | Choice2Of2 error ->
                    Choice2Of2 error
                | Choice1Of2 result ->
                    mapRec (tl1, tl2)

            | _, _ ->
                failwith "The lists have differing lengths -- they may have been modified in some invalid way."
                        
        // Call the recursive implementation function. 
        mapRec (list1, list2)

/// The standard F# Seq module, lifted into the Choice monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Seq =
    //
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> Choice<unit, 'Error>) (sequence : seq<'T>) : Choice<unit, 'Error> =
        // Preconditions
        checkNonNull "seq" seq

        let mutable error = None

        let enumerator = sequence.GetEnumerator ()
        while enumerator.MoveNext () && Option.isNone error do
            match action enumerator.Current with
            | Choice2Of2 err ->
                error <- Some err
            | Choice1Of2 () ->
                ()

        // If the error was set, return it.
        match error with
        | Some error ->
            Choice2Of2 error
        | None ->
            Choice1Of2 ()


/// The standard F# Set module, lifted into the Choice monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Set =
    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> Choice<'State, 'Error>) (state : 'State) (set : Set<'T>) =
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
            | Choice2Of2 err ->
                error <- Some err
            | Choice1Of2 newState ->
                state <- newState

        // If the error was set, return it; otherwise, return the final state.
        match error with
        | Some error ->
            Choice2Of2 error
        | None ->
            Choice1Of2 state

    //
    [<CompiledName("MapToArray")>]
    let mapToArray (mapping : 'T -> Choice<'U, 'Error>) (set : Set<'T>) : Choice<'U[], 'Error> =
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
            | Choice2Of2 err ->
                error <- Some err
            | Choice1Of2 result ->
                results.[index] <- result
                index <- index + 1 

        // If the error was set, return it; otherwise, return the final state.
        match error with
        | Some error ->
            Choice2Of2 error
        | None ->
            Choice1Of2 results


/// The ArraySegment module, lifted into the Choice monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ArraySegment =
    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> Choice<'State, 'Error>)
            (state : 'State) (segment : System.ArraySegment<'T>) =
        let folder = FSharpFunc<_,_,_>.Adapt folder

        let array = segment.Array
        let endExclusive = segment.Offset + segment.Count

        let mutable index = segment.Offset
        let mutable state = state
        let mutable error = None

        while index < endExclusive && Option.isNone error do
            match folder.Invoke (state, array.[index]) with
            | Choice2Of2 err ->
                error <- Some err
            | Choice1Of2 state' ->
                state <- state'
                index <- index + 1
            
        // If the error was set, return it.
        match error with
        | Some error ->
            Choice2Of2 error
        | None ->
            Choice1Of2 state

