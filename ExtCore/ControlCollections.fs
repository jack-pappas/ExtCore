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
namespace ExtCore.Control.Collections

open OptimizedClosures
open ExtCore
open ExtCore.Collections
open ExtCore.Control


//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Async =
    open Microsoft.FSharp.Control

    /// The standard F# Array module, lifted into the Async monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Array =
        //
        [<CompiledName("Initialize")>]
        let init (count : int) (initializer : int -> Async<'T>) : Async<'T[]> =
            // Preconditions
            if count < 0 then
                invalidArg "count" "The count cannot be negative."

            raise <| System.NotImplementedException "ExtCore.Control.Collections.Async.Array.init"

        //
        [<CompiledName("Map")>]
        let map (mapping : 'T -> Async<'U>) (array : 'T[]) : Async<'U[]> =
            // Preconditions
            checkNonNull "array" array

            raise <| System.NotImplementedException "ExtCore.Control.Collections.Async.Array.map"

        //
        [<CompiledName("MapIndexed")>]
        let mapi (mapping : int -> 'T -> Async<'U>) (array : 'T[]) : Async<'U[]> =
            // Preconditions
            checkNonNull "array" array

            raise <| System.NotImplementedException "ExtCore.Control.Collections.Async.Array.mapi"

        //
        [<CompiledName("Map2")>]
        let map2 (mapping : 'T1 -> 'T2 -> Async<'U>) (array1 : 'T1[]) (array2 : 'T2[]) : Async<'U[]> =
            // Preconditions
            checkNonNull "array1" array1
            checkNonNull "array2" array2

            raise <| System.NotImplementedException "ExtCore.Control.Collections.Async.Array.map2"

        //
        [<CompiledName("Fold")>]
        let fold (folder : 'State -> 'T -> Async<'State>) (state : 'State) (array : 'T[]) : Async<'State> =
            // Preconditions
            checkNonNull "array" array

            raise <| System.NotImplementedException "ExtCore.Control.Collections.Async.Array.fold"

        //
        [<CompiledName("FoldIndexed")>]
        let foldi (folder : int -> 'State -> 'T -> Async<'State>) (state : 'State) (array : 'T[]) : Async<'State> =
            // Preconditions
            checkNonNull "array" array

            raise <| System.NotImplementedException "ExtCore.Control.Collections.Async.Array.foldi"

        //
        [<CompiledName("Iterate")>]
        let iter (action : 'T -> Async<unit>) (array : 'T[]) : Async<unit> =
            // Preconditions
            checkNonNull "array" array

            raise <| System.NotImplementedException "ExtCore.Control.Collections.Async.Array.iter"

        //
        [<CompiledName("IterateIndexed")>]
        let iteri (action : int -> 'T -> Async<unit>) (array : 'T[]) : Async<unit> =
            // Preconditions
            checkNonNull "array" array

            raise <| System.NotImplementedException "ExtCore.Control.Collections.Async.Array.iteri"

        //
        [<CompiledName("Reduce")>]
        let reduce (reduction : 'T -> 'T -> Async<'T>) (array : 'T[]) : Async<'T> =
            // Preconditions
            checkNonNull "array" array
            if Array.isEmpty array then
                invalidArg "array" "The array is empty."

            raise <| System.NotImplementedException "ExtCore.Control.Collections.Async.Array.reduce"

        //
        [<CompiledName("Find")>]
        let find (predicate : 'T -> Async<bool>) (array : 'T[]) : Async<'T> =
            // Preconditions
            checkNonNull "array" array
            if Array.isEmpty array then
                invalidArg "array" "The array is empty."

            raise <| System.NotImplementedException "ExtCore.Control.Collections.Async.Array.find"

        //
        [<CompiledName("TryFind")>]
        let tryFind (predicate : 'T -> Async<bool>) (array : 'T[]) : Async<'T option> =
            // Preconditions
            checkNonNull "array" array
            if Array.isEmpty array then
                invalidArg "array" "The array is empty."

            raise <| System.NotImplementedException "ExtCore.Control.Collections.Async.Array.tryFind"


//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Either =
    /// The standard F# Array module, lifted into the Either monad.
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
            if isNull array1 then nullArg "array1"
            elif isNull array2 then nullArg "array2"

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


    /// The standard F# List module, lifted into the Either monad.
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
        let iter2 (action : 'T1 -> 'T2 -> Choice<unit, 'Error>) (list1 : 'T1 list) (list2 : 'T2 list) : Choice<unit, 'Error> =
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

    /// The standard F# Seq module, lifted into the Either monad.
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


    /// The standard F# Set module, lifted into the Either monad.
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


    /// The ArraySegment module, lifted into the Either monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ArraySegment =
        //
        [<CompiledName("Fold")>]
        let fold (folder : 'State -> 'T -> Choice<'State, 'Error>) (state : 'State) (segment : System.ArraySegment<'T>) =
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


//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal State =
    /// The standard F# Array module, lifted into the State monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Array =
        /// A specialization of Array.iter which threads an accumulator through the computation;
        /// this allows the use of actions requiring a (possibly mutable) state variable.
        [<CompiledName("Iterate")>]
        let iter (action : 'T -> 'State -> unit * 'State) (array : 'T[]) (state : 'State) : unit * 'State =
            // Preconditions
            checkNonNull "array" array

            let action = FSharpFunc<_,_,_>.Adapt action
            let len = array.Length
            let mutable state = state

            for i = 0 to len - 1 do
                state <- snd <| action.Invoke (array.[i], state)

            (), state

        /// A specialization of Array.iteri which threads an accumulator through the computation;
        /// this allows the use of actions requiring a (possibly mutable) state variable.
        [<CompiledName("IterateIndexed")>]
        let iteri (action : int -> 'T -> 'State -> unit * 'State) (array : 'T[]) (state : 'State) : unit * 'State =
            // Preconditions
            checkNonNull "array" array

            let action = FSharpFunc<_,_,_,_>.Adapt action
            let len = array.Length
            let mutable state = state

            for i = 0 to len - 1 do
                state <- snd <| action.Invoke (i, array.[i], state)

            (), state

        /// A specialization of Array.map which threads an accumulator through the computation;
        /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
        [<CompiledName("Map")>]
        let map (mapping : 'T -> 'State -> 'U * 'State) (array : 'T[]) (state : 'State) : 'U[] * 'State =
            // Preconditions
            checkNonNull "array" array

            let mapping = FSharpFunc<_,_,_>.Adapt mapping
            let len = array.Length
            let results = Array.zeroCreate len
            let mutable state = state

            for i = 0 to len - 1 do
                let result, state' = mapping.Invoke (array.[i], state)
                results.[i] <- result
                state <- state'

            results, state

        /// A specialization of Array.mapi which threads an accumulator through the computation;
        /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
        [<CompiledName("MapIndexed")>]
        let mapi (mapping : int -> 'T -> 'State -> 'U * 'State) (array : 'T[]) (state : 'State) : 'U[] * 'State =
            // Preconditions
            checkNonNull "array" array

            let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
            let len = array.Length
            let results = Array.zeroCreate len
            let mutable state = state

            for i = 0 to len - 1 do
                let result, state' = mapping.Invoke (i, array.[i], state)
                results.[i] <- result
                state <- state'

            results, state

        /// A specialization of Array.mapi which threads an accumulator through the computation;
        /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
        /// The index values are tagged with a unit-of-measure type before applying them to the mapping function.
        [<CompiledName("MapIndexedByTag")>]
        let mapti (mapping : int<'Tag> -> 'T -> 'State -> 'U * 'State) (array : 'T[]) (state : 'State) : 'U[] * 'State =
            // Preconditions
            checkNonNull "array" array

            let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
            let len = array.Length
            let results = Array.zeroCreate len
            let mutable state = state

            for i = 0 to len - 1 do
                let result, state' = mapping.Invoke (LanguagePrimitives.Int32WithMeasure<'Tag> i, array.[i], state)
                results.[i] <- result
                state <- state'

            results, state

        /// A specialization of Array.map which threads an accumulator through the computation;
        /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
        /// This function traverses the input array from right-to-left.
        [<CompiledName("MapBack")>]
        let mapBack (mapping : 'T -> 'State -> 'U * 'State) (array : 'T[]) (state : 'State) : 'U[] * 'State =
            // Preconditions
            checkNonNull "array" array

            let mapping = FSharpFunc<_,_,_>.Adapt mapping
            let len = array.Length
            let results = Array.zeroCreate len
            let mutable state = state

            for i = len - 1 downto 0 do
                let result, state' = mapping.Invoke (array.[i], state)
                results.[i] <- result
                state <- state'

            results, state

        /// A specialization of Array.mapi which threads an accumulator through the computation;
        /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
        /// This function traverses the input array from right-to-left.
        [<CompiledName("MapIndexedBack")>]
        let mapiBack (mapping : int -> 'T -> 'State -> 'U * 'State) (array : 'T[]) (state : 'State) : 'U[] * 'State =
            // Preconditions
            checkNonNull "array" array

            let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
            let len = array.Length
            let results = Array.zeroCreate len
            let mutable state = state

            for i = len - 1 downto 0 do
                let result, state' = mapping.Invoke (i, array.[i], state)
                results.[i] <- result
                state <- state'

            results, state

        /// A specialization of Array.mapi which threads an accumulator through the computation;
        /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
        /// This function traverses the input array from right-to-left.
        /// The index values are tagged with a unit-of-measure type before applying them to the mapping function.
        [<CompiledName("MapIndexedByTagBack")>]
        let maptiBack (mapping : int<'Tag> -> 'T -> 'State -> 'U * 'State) (array : 'T[]) (state : 'State) : 'U[] * 'State =
            // Preconditions
            checkNonNull "array" array

            let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
            let len = array.Length
            let results = Array.zeroCreate len
            let mutable state = state

            for i = len - 1 downto 0 do
                let result, state' = mapping.Invoke (LanguagePrimitives.Int32WithMeasure<'Tag> i, array.[i], state)
                results.[i] <- result
                state <- state'

            results, state

        /// A specialization of Array.map2 which threads an accumulator through the computation;
        /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
        /// This function traverses the input arrays from left-to-right.
        [<CompiledName("Map2")>]
        let map2 (mapping : 'T1 -> 'T2 -> 'State -> 'U * 'State) (array1 : 'T1[]) (array2 : 'T2[]) (state : 'State) : 'U[] * 'State =
            // Preconditions
            if isNull array1 then nullArg "array1"
            elif isNull array2 then nullArg "array2"

            let len = array1.Length

            if array2.Length <> len then
                invalidArg "array2" "The arrays have differing lengths."

            let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
            let results = Array.zeroCreate len
            let mutable state = state

            for i = 0 to len - 1 do
                let result, state' = mapping.Invoke (array1.[i], array2.[i], state)
                results.[i] <- result
                state <- state'

            results, state

        /// A specialization of Array.mapi2 which threads an accumulator through the computation;
        /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
        /// This function traverses the input arrays from left-to-right.
        [<CompiledName("MapIndexed2")>]
        let mapi2 (mapping : int -> 'T1 -> 'T2 -> 'State -> 'U * 'State) (array1 : 'T1[]) (array2 : 'T2[]) (state : 'State) : 'U[] * 'State =
            // Preconditions
            if isNull array1 then nullArg "array1"
            elif isNull array2 then nullArg "array2"

            let len = array1.Length

            if array2.Length <> len then
                invalidArg "array2" "The arrays have differing lengths."

            let mapping = FSharpFunc<_,_,_,_,_>.Adapt mapping
            let results = Array.zeroCreate len
            let mutable state = state

            for i = 0 to len - 1 do
                let result, state' = mapping.Invoke (i, array1.[i], array2.[i], state)
                results.[i] <- result
                state <- state'

            results, state

        /// Applies a function to each element of the collection, threading an accumulator argument through the computation.
        [<CompiledName("Fold")>]
        let fold (folder : 'InnerState -> 'T -> 'OuterState -> 'InnerState * 'OuterState) (innerState : 'InnerState) (array : 'T[]) (outerState : 'OuterState) : 'InnerState * 'OuterState =
            // Preconditions
            checkNonNull "array" array

            let folder = FSharpFunc<_,_,_,_>.Adapt folder
            let len = array.Length
            let mutable outerState = outerState
            let mutable innerState = innerState

            for i = 0 to len - 1 do
                let innerState', outerState' = folder.Invoke (innerState, array.[i], outerState)
                innerState <- innerState'
                outerState <- outerState'

            innerState, outerState

        /// Applies a function to each element of the collection, threading an accumulator argument through the computation.
        /// The integer index passed to the function indicates the array index of the element being transformed.
        [<CompiledName("FoldIndexed")>]
        let foldi (folder : int -> 'InnerState -> 'T -> 'OuterState -> 'InnerState * 'OuterState) (innerState : 'InnerState) (array : 'T[]) (outerState : 'OuterState) : 'InnerState * 'OuterState =
            // Preconditions
            checkNonNull "array" array

            let folder = FSharpFunc<_,_,_,_,_>.Adapt folder
            let len = array.Length
            let mutable outerState = outerState
            let mutable innerState = innerState

            for i = 0 to len - 1 do
                let innerState', outerState' = folder.Invoke (i, innerState, array.[i], outerState)
                innerState <- innerState'
                outerState <- outerState'

            innerState, outerState

        /// Applies a function to each element of the collection, threading an accumulator argument through the computation.
        /// The integer index passed to the function indicates the array index of the element being transformed.
        /// The index values are tagged with a unit-of-measure type before applying them to the folder function.
        [<CompiledName("FoldIndexedByTag")>]
        let foldti (folder : int<'Tag> -> 'InnerState -> 'T -> 'OuterState -> 'InnerState * 'OuterState) (innerState : 'InnerState) (array : 'T[]) (outerState : 'OuterState) : 'InnerState * 'OuterState =
            // Preconditions
            checkNonNull "array" array

            let folder = FSharpFunc<_,_,_,_,_>.Adapt folder
            let len = array.Length
            let mutable outerState = outerState
            let mutable innerState = innerState

            for i = 0 to len - 1 do
                let innerState', outerState' = folder.Invoke (LanguagePrimitives.Int32WithMeasure<'Tag> i, innerState, array.[i], outerState)
                innerState <- innerState'
                outerState <- outerState'

            innerState, outerState


    /// The TidePowerd.Core.Collections.ArraySegment module, lifted into the State monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ArraySegment =
        /// A specialization of ArraySegment.iter which threads an accumulator through the computation; this allows
        /// the use of actions requiring a (possibly mutable) state variable.
        [<CompiledName("Iterate")>]
        let iter (action : 'T -> 'State -> unit * 'State) (segment : System.ArraySegment<'T>) (state : 'State) : unit * 'State =
            let action = FSharpFunc<_,_,_>.Adapt action

            let array = segment.Array
            let endExclusive = segment.Offset + segment.Count
            let mutable state = state

            for i = segment.Offset to endExclusive - 1 do
                state <- snd <| action.Invoke (array.[i], state)

            (), state

        /// A specialization of ArraySegment.iteri which threads an accumulator through the computation; this allows
        /// the use of actions requiring a (possibly mutable) state variable.
        [<CompiledName("IterateIndexed")>]
        let iteri (action : int -> 'T -> 'State -> unit * 'State) (segment : System.ArraySegment<'T>) (state : 'State) : unit * 'State =
            let action = FSharpFunc<_,_,_,_>.Adapt action

            let array = segment.Array
            let endExclusive = segment.Offset + segment.Count
            let mutable state = state
            let mutable idx = 0

            for i = segment.Offset to endExclusive - 1 do
                state <- snd <| action.Invoke (idx, array.[i], state)
                idx <- idx + 1

            (), state

        /// A specialization of Array.map which threads an accumulator through the computation; this allows
        /// the use of mapping functions requiring a (possibly mutable) state variable.
        [<CompiledName("Map")>]
        let map (mapping : 'T -> 'State -> 'U * 'State) (segment : System.ArraySegment<'T>) (state : 'State) : 'U[] * 'State =
            let mapping = FSharpFunc<_,_,_>.Adapt mapping

            let array = segment.Array
            let offset = segment.Offset
            let count = segment.Count
            
            let results = Array.zeroCreate count
            let mutable state = state

            for i = 0 to count - 1 do
                let result, state' = mapping.Invoke (array.[offset + i], state)
                results.[i] <- result
                state <- state'

            results, state

        /// A specialization of Array.mapi which threads an accumulator through the computation; this allows
        /// the use of mapping functions requiring a (possibly mutable) state variable.
        [<CompiledName("MapIndexed")>]
        let mapi (mapping : int -> 'T -> 'State -> 'U * 'State) (segment : System.ArraySegment<'T>) (state : 'State) : 'U[] * 'State =
            let mapping = FSharpFunc<_,_,_,_>.Adapt mapping

            let array = segment.Array
            let offset = segment.Offset
            let count = segment.Count
            
            let results = Array.zeroCreate count
            let mutable state = state

            for i = 0 to count - 1 do
                let result, state' = mapping.Invoke (i, array.[offset + i], state)
                results.[i] <- result
                state <- state'

            results, state


//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal ProtectedState =
    /// The standard F# Array module, lifted into the ProtectedState monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Array =
        /// A specialization of Array.iter which threads an accumulator through the computation and which also
        /// short-circuits the computation if the mapping function returns an error when any element is applied to it.
        [<CompiledName("Iterate")>]
        let iter (action : 'T -> 'State -> Choice<unit * 'State, 'Error>) (array : 'T[]) (state : 'State) : Choice<unit * 'State, 'Error> =
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

        /// A specialization of Array.iteri which threads an accumulator through the computation and which also
        /// short-circuits the computation if the mapping function returns an error when any element is applied to it.
        [<CompiledName("IterateIndexed")>]
        let iteri (action : int -> 'T -> 'State -> Choice<unit * 'State, 'Error>) (array : 'T[]) (state : 'State) : Choice<unit * 'State, 'Error> =
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

        /// A specialization of Array.map which threads an accumulator through the computation and which also
        /// short-circuits the computation if the mapping function returns an error when any element is applied to it.
        [<CompiledName("Map")>]
        let map (mapping : 'T -> 'State -> Choice<'U * 'State, 'Error>) (array : 'T[]) (state : 'State) : Choice<'U[] * 'State, 'Error> =
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

        /// A specialization of Array.mapi which threads an accumulator through the computation and which also
        /// short-circuits the computation if the mapping function returns an error when any element is applied to it.
        [<CompiledName("MapIndexed")>]
        let mapi (mapping : int -> 'T -> 'State -> Choice<'U * 'State, 'Error>) (array : 'T[]) (state : 'State) : Choice<'U[] * 'State, 'Error> =
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
        //
        let rec private mapImpl (mapping : 'T -> 'State -> Choice<'U * 'State, 'Error>) (results, lst, state) =
            match lst with
            | [] ->
                Choice1Of2 (List.rev results, state)

            | hd :: tl ->
                // Apply the mapping function. If the function returns
                // an error value, short-circuit and return immediately.
                match mapping hd state with
                | Choice2Of2 error ->
                    Choice2Of2 error

                | Choice1Of2 (result, state) ->
                    // Cons the result onto the result list and continue processing.
                    mapImpl mapping (result :: results, tl, state)

        /// A specialization of List.map which threads an accumulator through the computation and which also
        /// short-circuits the computation if the mapping function returns an error when any element is applied to it.
        [<CompiledName("Map")>]
        let map (mapping : 'T -> 'State -> Choice<'U * 'State, 'Error>) (list : 'T list) (state : 'State) : Choice<'U list * 'State, 'Error> =
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


    /// The TidePowerd.Core.Collections.ArraySegment module, lifted into the ProtectedState monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ArraySegment =
        /// A specialization of ArraySegment.iter which threads an accumulator through the computation and which also
        /// short-circuits the computation if the mapping function returns an error when any element is applied to it.
        [<CompiledName("Iterate")>]
        let iter (action : 'T -> 'State -> Choice<unit * 'State, 'Error>) (segment : System.ArraySegment<'T>) (state : 'State) : Choice<unit * 'State, 'Error> =
            let action = FSharpFunc<_,_,_>.Adapt action

            let array = segment.Array
            let endExclusive = segment.Offset + segment.Count

            let mutable index = segment.Offset
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

        /// A specialization of ArraySegment.iteri which threads an accumulator through the computation and which also
        /// short-circuits the computation if the mapping function returns an error when any element is applied to it.
        [<CompiledName("IterateIndexed")>]
        let iteri (action : int -> 'T -> 'State -> Choice<unit * 'State, 'Error>) (segment : System.ArraySegment<'T>) (state : 'State) : Choice<unit * 'State, 'Error> =
            let action = FSharpFunc<_,_,_,_>.Adapt action

            let array = segment.Array
            let endExclusive = segment.Offset + segment.Count

            let mutable index = segment.Offset
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

        /// A specialization of ArraySegment.map which threads an accumulator through the computation and which also
        /// short-circuits the computation if the mapping function returns an error when any element is applied to it.
        [<CompiledName("Map")>]
        let map (mapping : 'T -> 'State -> Choice<'U * 'State, 'Error>) (segment : System.ArraySegment<'T>) (state : 'State) : Choice<'U[] * 'State, 'Error> =
            let mapping = FSharpFunc<_,_,_>.Adapt mapping

            let array = segment.Array
            let endExclusive = segment.Offset + segment.Count
            /// Holds the mapped results.
            let results = Array.zeroCreate segment.Count

            let mutable index = segment.Offset
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

        /// A specialization of ArraySegment.mapi which threads an accumulator through the computation and which also
        /// short-circuits the computation if the mapping function returns an error when any element is applied to it.
        [<CompiledName("MapIndexed")>]
        let mapi (mapping : int -> 'T -> 'State -> Choice<'U * 'State, 'Error>) (segment : System.ArraySegment<'T>) (state : 'State) : Choice<'U[] * 'State, 'Error> =
            let mapping = FSharpFunc<_,_,_,_>.Adapt mapping

            let array = segment.Array
            let endExclusive = segment.Offset + segment.Count
            /// Holds the mapped results.
            let results = Array.zeroCreate segment.Count

            let mutable index = segment.Offset
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


//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal StatefulChoice =
    /// The standard F# Array module, lifted into the StatefulChoice monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Array =
        /// A specialization of Array.map which threads an accumulator through the computation and which also
        /// short-circuits the computation if the mapping function returns an error when any element is applied to it.
        [<CompiledName("Map")>]
        let map (mapping : 'T -> 'State -> Choice<'U, 'Error> * 'State) (array : 'T[]) (state : 'State) : Choice<'U[], 'Error> * 'State =
            // Preconditions
            checkNonNull "array" array

            let mapping = FSharpFunc<_,_,_>.Adapt mapping
            let len = array.Length
            /// Holds the mapped results.
            let results = Array.zeroCreate len

            let mutable state = state
            let mutable error = None
            let mutable index = 0

            while index < len && Option.isNone error do
                let result, state' = mapping.Invoke (array.[index], state)
                
                // Update the state, even if the result was an error.
                state <- state'
                
                // Check the result; short-circuit if it's an error.
                match result with
                | Choice2Of2 err ->
                    error <- Some err
                | Choice1Of2 result ->
                    results.[index] <- result
                    index <- index + 1
                
            
            // Return the updated state along with the
            // result (or error, if set).
            match error with
            | Some error ->
                (Choice2Of2 error), state
            | None ->
                (Choice1Of2 results), state


