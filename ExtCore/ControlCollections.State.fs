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
module ExtCore.Control.Collections.State
    
open Microsoft.FSharp.Control
open OptimizedClosures
open ExtCore
open ExtCore.Collections


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

    /// A specialization of Array.map2 which threads an accumulator through the computation;
    /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
    /// This function traverses the input arrays from left-to-right.
    [<CompiledName("Map2")>]
    let map2 (mapping : 'T1 -> 'T2 -> 'State -> 'U * 'State) (array1 : 'T1[]) (array2 : 'T2[]) (state : 'State) : 'U[] * 'State =
        // Preconditions
        checkNonNull "array1" array1
        checkNonNull "array2" array2

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
    let mapi2 (mapping : int -> 'T1 -> 'T2 -> 'State -> 'U * 'State)
            (array1 : 'T1[]) (array2 : 'T2[]) (state : 'State) : 'U[] * 'State =
        // Preconditions
        checkNonNull "array1" array1
        checkNonNull "array2" array2

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
    let fold (folder : 'InnerState -> 'T -> 'OuterState -> 'InnerState * 'OuterState)
            (innerState : 'InnerState) (array : 'T[]) (outerState : 'OuterState) : 'InnerState * 'OuterState =
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
    let foldi (folder : int -> 'InnerState -> 'T -> 'OuterState -> 'InnerState * 'OuterState)
            (innerState : 'InnerState) (array : 'T[]) (outerState : 'OuterState)
            : 'InnerState * 'OuterState =
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
    /// Raises ArgumentException if the array is empty (has a length of zero (0)).
    [<CompiledName("Reduce")>]
    let reduce (reduction : 'T -> 'T -> 'State -> 'T * 'State) (array : 'T[]) (state : 'State) : 'T * 'State =
        // Preconditions
        checkNonNull "array" array
        if Array.isEmpty array then
            invalidArg "array" "The array is empty."

        let reduction = FSharpFunc<_,_,_,_>.Adapt reduction
        let len = array.Length
        let mutable result = Array.first array
        let mutable state = state

        for i = 1 to len - 1 do
            let result', state' = reduction.Invoke (result, array.[i], state)
            result <- result'
            state <- state'

        result, state

    /// Applies a function to each element of the collection, threading an accumulator argument through the computation.
    /// Raises ArgumentException if the array is empty (has a length of zero (0)).
    [<CompiledName("ReduceBack")>]
    let reduceBack (reduction : 'T -> 'T -> 'State -> 'T * 'State) (array : 'T[]) (state : 'State) : 'T * 'State =
        // Preconditions
        checkNonNull "array" array
        if Array.isEmpty array then
            invalidArg "array" "The array is empty."

        let reduction = FSharpFunc<_,_,_,_>.Adapt reduction
        let len = array.Length
        let mutable result = Array.last array
        let mutable state = state

        for i = len - 2 downto 0 do
            let result', state' = reduction.Invoke (result, array.[i], state)
            result <- result'
            state <- state'

        result, state


/// The standard F# List module, lifted into the State monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module List =
    /// A specialization of List.iter which threads an accumulator through the computation;
    /// this allows the use of actions requiring a (possibly mutable) state variable.
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> 'State -> unit * 'State) (list : 'T list) (state : 'State) : unit * 'State =
        // Preconditions
        checkNonNull "list" list

        // OPTIMIZATION : Return immediately if the list is empty.
        if List.isEmpty list then (), state
        else
            let action = FSharpFunc<_,_,_>.Adapt action
            let mutable list = list
            let mutable state = state

            while not <| List.isEmpty list do
                state <- snd <| action.Invoke (List.head list, state)
                list <- List.tail list

            (), state

    /// A specialization of List.iteri which threads an accumulator through the computation;
    /// this allows the use of actions requiring a (possibly mutable) state variable.
    [<CompiledName("IterateIndexed")>]
    let iteri (action : int -> 'T -> 'State -> unit * 'State) (list : 'T list) (state : 'State) : unit * 'State =
        // Preconditions
        checkNonNull "list" list

        // OPTIMIZATION : Return immediately if the list is empty.
        if List.isEmpty list then (), state
        else
            let action = FSharpFunc<_,_,_,_>.Adapt action
            let mutable list = list
            let mutable state = state
            let mutable index = 0

            while not <| List.isEmpty list do
                state <- snd <| action.Invoke (index, List.head list, state)
                list <- List.tail list
                index <- index + 1

            (), state

    /// A specialization of List.iterBack which threads an accumulator through the computation;
    /// this allows the use of actions requiring a (possibly mutable) state variable.
    [<CompiledName("IterateBack")>]
    let iterBack (action : 'T -> 'State -> unit * 'State) (list : 'T list) (state : 'State) : unit * 'State =
        // Preconditions
        checkNonNull "list" list

        // OPTIMIZATION : Return immediately if the list is empty.
        if List.isEmpty list then (), state
        else
            let action = FSharpFunc<_,_,_>.Adapt action
            let mutable list = List.rev list    // The list is reversed here so we iterate backwards over it.
            let mutable state = state

            while not <| List.isEmpty list do
                state <- snd <| action.Invoke (List.head list, state)
                list <- List.tail list

            (), state

    /// A specialization of List.map which threads an accumulator through the computation;
    /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'State -> 'U * 'State) (list : 'T list) (state : 'State) : 'U list * 'State =
        // Preconditions
        checkNonNull "list" list

        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        let mutable list = list
        let mutable results = []
        let mutable state = state

        while not <| List.isEmpty list do
            let result, state' = mapping.Invoke (List.head list, state)
            results <- result :: results
            state <- state'
            list <- List.tail list

        List.rev results, state

    /// A specialization of List.mapi which threads an accumulator through the computation;
    /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> 'State -> 'U * 'State) (list : 'T list) (state : 'State) : 'U list * 'State =
        // Preconditions
        checkNonNull "list" list

        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
        let mutable list = list
        let mutable results = []
        let mutable state = state
        let mutable index = 0

        while not <| List.isEmpty list do
            let result, state' = mapping.Invoke (index, List.head list, state)
            results <- result :: results
            state <- state'
            list <- List.tail list
            index <- index + 1

        List.rev results, state

    //
    [<CompiledName("Fold")>]
    let fold (folder : 'S1 -> 'T -> 'S2 -> 'S1 * 'S2) (innerState : 'S1) (list : 'T list) (outerState : 'S2) : 'S1 * 'S2 =
        // Preconditions
        checkNonNull "list" list

        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        let mutable list = list
        let mutable innerState = innerState
        let mutable outerState = outerState

        while not <| List.isEmpty list do
            let innerState', outerState' =
                folder.Invoke (innerState, List.head list, outerState)
            innerState <- innerState'
            outerState <- outerState'
            list <- List.tail list

        innerState, outerState


/// The ExtCore.Collections.TaggedArray module, lifted into the State monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TaggedArray =
    open LanguagePrimitives

    /// A specialization of Array.mapi which threads an accumulator through the computation;
    /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
    /// The index values are tagged with a unit-of-measure type before applying them to the mapping function.
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int<'Tag> -> 'T -> 'State -> 'U * 'State)
            (array : 'T[]) (state : 'State) : 'U[] * 'State =
        // Preconditions
        checkNonNull "array" array

        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
        let len = array.Length
        let results = Array.zeroCreate len
        let mutable state = state

        for i = 0 to len - 1 do
            let result, state' = mapping.Invoke (Int32WithMeasure<'Tag> i, array.[i], state)
            results.[i] <- result
            state <- state'

        results, state

    /// A specialization of Array.mapi which threads an accumulator through the computation;
    /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
    /// This function traverses the input array from right-to-left.
    /// The index values are tagged with a unit-of-measure type before applying them to the mapping function.
    [<CompiledName("MapIndexedBack")>]
    let mapiBack (mapping : int<'Tag> -> 'T -> 'State -> 'U * 'State)
            (array : 'T[]) (state : 'State) : 'U[] * 'State =
        // Preconditions
        checkNonNull "array" array

        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
        let len = array.Length
        let results = Array.zeroCreate len
        let mutable state = state

        for i = len - 1 downto 0 do
            let result, state' = mapping.Invoke (Int32WithMeasure<'Tag> i, array.[i], state)
            results.[i] <- result
            state <- state'

        results, state

    /// Applies a function to each element of the collection, threading an accumulator argument through the computation.
    /// The integer index passed to the function indicates the array index of the element being transformed.
    /// The index values are tagged with a unit-of-measure type before applying them to the folder function.
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : int<'Tag> -> 'InnerState -> 'T -> 'OuterState -> 'InnerState * 'OuterState)
            (innerState : 'InnerState) (array : 'T[]) (outerState : 'OuterState)
            : 'InnerState * 'OuterState =
        // Preconditions
        checkNonNull "array" array

        let folder = FSharpFunc<_,_,_,_,_>.Adapt folder
        let len = array.Length
        let mutable outerState = outerState
        let mutable innerState = innerState

        for i = 0 to len - 1 do
            let innerState', outerState' = folder.Invoke (Int32WithMeasure<'Tag> i, innerState, array.[i], outerState)
            innerState <- innerState'
            outerState <- outerState'

        innerState, outerState


/// The ExtCore.Collections.ArrayView module, lifted into the State monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ArrayView =
    /// A specialization of ArrayView.iter which threads an accumulator through the computation; this allows
    /// the use of actions requiring a (possibly mutable) state variable.
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> 'State -> unit * 'State)
            (view : ArrayView<'T>) (state : 'State) : unit * 'State =
        let action = FSharpFunc<_,_,_>.Adapt action

        let array = view.Array
        let endExclusive = view.Offset + view.Count
        let mutable state = state

        for i = view.Offset to endExclusive - 1 do
            state <- snd <| action.Invoke (array.[i], state)

        (), state

    /// A specialization of ArrayView.iteri which threads an accumulator through the computation; this allows
    /// the use of actions requiring a (possibly mutable) state variable.
    [<CompiledName("IterateIndexed")>]
    let iteri (action : int -> 'T -> 'State -> unit * 'State)
            (view : ArrayView<'T>) (state : 'State) : unit * 'State =
        let action = FSharpFunc<_,_,_,_>.Adapt action

        let array = view.Array
        let endExclusive = view.Offset + view.Count
        let mutable state = state
        let mutable idx = 0

        for i = view.Offset to endExclusive - 1 do
            state <- snd <| action.Invoke (idx, array.[i], state)
            idx <- idx + 1

        (), state

    /// A specialization of Array.map which threads an accumulator through the computation; this allows
    /// the use of mapping functions requiring a (possibly mutable) state variable.
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'State -> 'U * 'State)
            (view : ArrayView<'T>) (state : 'State) : 'U[] * 'State =
        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        let array = view.Array
        let offset = view.Offset
        let count = view.Count
            
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
    let mapi (mapping : int -> 'T -> 'State -> 'U * 'State)
            (view : ArrayView<'T>) (state : 'State) : 'U[] * 'State =
        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping

        let array = view.Array
        let offset = view.Offset
        let count = view.Count
            
        let results = Array.zeroCreate count
        let mutable state = state

        for i = 0 to count - 1 do
            let result, state' = mapping.Invoke (i, array.[offset + i], state)
            results.[i] <- result
            state <- state'

        results, state


/// The F# Set module, lifted into the State monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Set =
    //
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> 'State -> unit * 'State) (set : Set<'T>) (state : 'State) : unit * 'State =
        // Preconditions
        checkNonNull "set" set

        let action = FSharpFunc<_,_,_>.Adapt action

        let state =
            (state, set)
            ||> Set.fold (fun state el ->
                snd <| action.Invoke (el, state))

        (), state

    //
    [<CompiledName("IterateBack")>]
    let iterBack (action : 'T -> 'State -> unit * 'State) (set : Set<'T>) (state : 'State) : unit * 'State =
        // Preconditions
        checkNonNull "set" set

        let action = FSharpFunc<_,_,_>.Adapt action

        let state =
            (set, state)
            ||> Set.foldBack (fun el state ->
                snd <| action.Invoke (el, state))

        (), state

    //
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'State -> 'U * 'State) (set : Set<'T>) (state : 'State) : Set<'U> * 'State =
        // Preconditions
        checkNonNull "set" set

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        ((Set.empty, state), set)
        ||> Set.fold (fun (mappedSet, state) el ->
            // Apply the mapping to the current element and state value.
            let mappedEl, state = mapping.Invoke (el, state)

            // Add the mapped element to the mapped set and continue iterating.
            Set.add mappedEl mappedSet, state)

    //
    [<CompiledName("MapBack")>]
    let mapBack (mapping : 'T -> 'State -> 'U * 'State) (set : Set<'T>) (state : 'State) : Set<'U> * 'State =
        // Preconditions
        checkNonNull "set" set

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        (set, (Set.empty, state))
        ||> Set.foldBack (fun el (mappedSet, state) ->
            // Apply the mapping to the current element and state value.
            let mappedEl, state = mapping.Invoke (el, state)

            // Add the mapped element to the mapped set and continue iterating.
            Set.add mappedEl mappedSet, state)

    //
    [<CompiledName("Fold")>]
    let fold (folder : 'InnerState -> 'T -> 'OuterState -> 'InnerState * 'OuterState)
        (innerState : 'InnerState) (set : Set<'T>) (outerState : 'OuterState) : 'InnerState * 'OuterState =
        // Preconditions
        checkNonNull "set" set

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        ((innerState, outerState), set)
        ||> Set.fold (fun (innerState, outerState) el ->
            // Apply the folder to the current element and state values.
            folder.Invoke (innerState, el, outerState))

    //
    [<CompiledName("FoldBack")>]
    let foldBack (folder : 'T -> 'InnerState -> 'OuterState -> 'InnerState * 'OuterState) (set : Set<'T>)
        (innerState : 'InnerState) (outerState : 'OuterState) : 'InnerState * 'OuterState =
        // Preconditions
        checkNonNull "set" set

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        (set, (innerState, outerState))
        ||> Set.foldBack (fun el (innerState, outerState) ->
            // Apply the folder to the current element and state values.
            folder.Invoke (el, innerState, outerState))


/// The IntSet module, lifted into the State monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IntSet =
    //
    [<CompiledName("Iterate")>]
    let iter (action : int -> 'State -> unit * 'State) (set : IntSet) (state : 'State) : unit * 'State =
        // Preconditions
        checkNonNull "set" set

        let action = FSharpFunc<_,_,_>.Adapt action

        let state =
            (state, set)
            ||> IntSet.fold (fun state el ->
                snd <| action.Invoke (el, state))

        (), state

    //
    [<CompiledName("IterateBack")>]
    let iterBack (action : int -> 'State -> unit * 'State) (set : IntSet) (state : 'State) : unit * 'State =
        // Preconditions
        checkNonNull "set" set

        let action = FSharpFunc<_,_,_>.Adapt action

        let state =
            (set, state)
            ||> IntSet.foldBack (fun el state ->
                snd <| action.Invoke (el, state))

        (), state

    //
    [<CompiledName("Map")>]
    let map (mapping : int -> 'State -> int * 'State) (set : IntSet) (state : 'State) : IntSet * 'State =
        // Preconditions
        checkNonNull "set" set

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        ((IntSet.empty, state), set)
        ||> IntSet.fold (fun (mappedSet, state) el ->
            // Apply the mapping to the current element and state value.
            let mappedEl, state = mapping.Invoke (el, state)

            // Add the mapped element to the mapped set and continue iterating.
            IntSet.add mappedEl mappedSet, state)

    //
    [<CompiledName("MapBack")>]
    let mapBack (mapping : int -> 'State -> int * 'State) (set : IntSet) (state : 'State) : IntSet * 'State =
        // Preconditions
        checkNonNull "set" set

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        (set, (IntSet.empty, state))
        ||> IntSet.foldBack (fun el (mappedSet, state) ->
            // Apply the mapping to the current element and state value.
            let mappedEl, state = mapping.Invoke (el, state)

            // Add the mapped element to the mapped set and continue iterating.
            IntSet.add mappedEl mappedSet, state)

    //
    [<CompiledName("Fold")>]
    let fold (folder : 'InnerState -> int -> 'OuterState -> 'InnerState * 'OuterState)
        (innerState : 'InnerState) (set : IntSet) (outerState : 'OuterState) : 'InnerState * 'OuterState =
        // Preconditions
        checkNonNull "set" set

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        ((innerState, outerState), set)
        ||> IntSet.fold (fun (innerState, outerState) el ->
            // Apply the folder to the current element and state values.
            folder.Invoke (innerState, el, outerState))

    //
    [<CompiledName("FoldBack")>]
    let foldBack (folder : int -> 'InnerState -> 'OuterState -> 'InnerState * 'OuterState) (set : IntSet)
        (innerState : 'InnerState) (outerState : 'OuterState) : 'InnerState * 'OuterState =
        // Preconditions
        checkNonNull "set" set

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        (set, (innerState, outerState))
        ||> IntSet.foldBack (fun el (innerState, outerState) ->
            // Apply the folder to the current element and state values.
            folder.Invoke (el, innerState, outerState))


#if PROTO_COMPILER

/// The TagSet module, lifted into the State monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TagSet =
    //
    [<CompiledName("Iterate")>]
    let iter (action : int<'Tag> -> 'State -> unit * 'State) (set : TagSet<'Tag>) (state : 'State) : unit * 'State =
        // Preconditions
        checkNonNull "set" set

        let action = FSharpFunc<_,_,_>.Adapt action

        let state =
            (state, set)
            ||> TagSet.fold (fun state el ->
                snd <| action.Invoke (el, state))

        (), state

    //
    [<CompiledName("IterateBack")>]
    let iterBack (action : int<'Tag> -> 'State -> unit * 'State) (set : TagSet<'Tag>) (state : 'State) : unit * 'State =
        // Preconditions
        checkNonNull "set" set

        let action = FSharpFunc<_,_,_>.Adapt action

        let state =
            (set, state)
            ||> TagSet.foldBack (fun el state ->
                snd <| action.Invoke (el, state))

        (), state

    //
    [<CompiledName("Map")>]
    let map (mapping : int<'Tag1> -> 'State -> int<'Tag2> * 'State) (set : TagSet<'Tag1>) (state : 'State) : TagSet<'Tag2> * 'State =
        // Preconditions
        checkNonNull "set" set

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        ((TagSet.empty, state), set)
        ||> TagSet.fold (fun (mappedSet, state) el ->
            // Apply the mapping to the current element and state value.
            let mappedEl, state = mapping.Invoke (el, state)

            // Add the mapped element to the mapped set and continue iterating.
            TagSet.add mappedEl mappedSet, state)

    //
    [<CompiledName("MapBack")>]
    let mapBack (mapping : int<'Tag1> -> 'State -> int<'Tag2> * 'State) (set : TagSet<'Tag1>) (state : 'State) : TagSet<'Tag2> * 'State =
        // Preconditions
        checkNonNull "set" set

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        (set, (TagSet.empty, state))
        ||> TagSet.foldBack (fun el (mappedSet, state) ->
            // Apply the mapping to the current element and state value.
            let mappedEl, state = mapping.Invoke (el, state)

            // Add the mapped element to the mapped set and continue iterating.
            TagSet.add mappedEl mappedSet, state)

    //
    [<CompiledName("Fold")>]
    let fold (folder : 'InnerState -> int<'Tag> -> 'OuterState -> 'InnerState * 'OuterState)
        (innerState : 'InnerState) (set : TagSet<'Tag>) (outerState : 'OuterState) : 'InnerState * 'OuterState =
        // Preconditions
        checkNonNull "set" set

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        ((innerState, outerState), set)
        ||> TagSet.fold (fun (innerState, outerState) el ->
            // Apply the folder to the current element and state values.
            folder.Invoke (innerState, el, outerState))

    //
    [<CompiledName("FoldBack")>]
    let foldBack (folder : int<'Tag> -> 'InnerState -> 'OuterState -> 'InnerState * 'OuterState) (set : TagSet<'Tag>)
        (innerState : 'InnerState) (outerState : 'OuterState) : 'InnerState * 'OuterState =
        // Preconditions
        checkNonNull "set" set

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        (set, (innerState, outerState))
        ||> TagSet.foldBack (fun el (innerState, outerState) ->
            // Apply the folder to the current element and state values.
            folder.Invoke (el, innerState, outerState))

#endif

/// The HashSet module, lifted into the State monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HashSet =
    //
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> 'State -> unit * 'State) (set : HashSet<'T>) (state : 'State) : unit * 'State =
        // Preconditions
        checkNonNull "set" set

        let action = FSharpFunc<_,_,_>.Adapt action

        let state =
            (state, set)
            ||> HashSet.fold (fun state el ->
                snd <| action.Invoke (el, state))

        (), state

    //
    [<CompiledName("IterateBack")>]
    let iterBack (action : 'T -> 'State -> unit * 'State) (set : HashSet<'T>) (state : 'State) : unit * 'State =
        // Preconditions
        checkNonNull "set" set

        let action = FSharpFunc<_,_,_>.Adapt action

        let state =
            (set, state)
            ||> HashSet.foldBack (fun el state ->
                snd <| action.Invoke (el, state))

        (), state

    //
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'State -> 'U * 'State) (set : HashSet<'T>) (state : 'State) : HashSet<'U> * 'State =
        // Preconditions
        checkNonNull "set" set

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        ((HashSet.empty, state), set)
        ||> HashSet.fold (fun (mappedSet, state) el ->
            // Apply the mapping to the current element and state value.
            let mappedEl, state = mapping.Invoke (el, state)

            // Add the mapped element to the mapped set and continue iterating.
            HashSet.add mappedEl mappedSet, state)

    //
    [<CompiledName("MapBack")>]
    let mapBack (mapping : 'T -> 'State -> 'U * 'State) (set : HashSet<'T>) (state : 'State) : HashSet<'U> * 'State =
        // Preconditions
        checkNonNull "set" set

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        (set, (HashSet.empty, state))
        ||> HashSet.foldBack (fun el (mappedSet, state) ->
            // Apply the mapping to the current element and state value.
            let mappedEl, state = mapping.Invoke (el, state)

            // Add the mapped element to the mapped set and continue iterating.
            HashSet.add mappedEl mappedSet, state)

    //
    [<CompiledName("Fold")>]
    let fold (folder : 'InnerState -> 'T -> 'OuterState -> 'InnerState * 'OuterState)
        (innerState : 'InnerState) (set : HashSet<'T>) (outerState : 'OuterState) : 'InnerState * 'OuterState =
        // Preconditions
        checkNonNull "set" set

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        ((innerState, outerState), set)
        ||> HashSet.fold (fun (innerState, outerState) el ->
            // Apply the folder to the current element and state values.
            folder.Invoke (innerState, el, outerState))

    //
    [<CompiledName("FoldBack")>]
    let foldBack (folder : 'T -> 'InnerState -> 'OuterState -> 'InnerState * 'OuterState) (set : HashSet<'T>)
        (innerState : 'InnerState) (outerState : 'OuterState) : 'InnerState * 'OuterState =
        // Preconditions
        checkNonNull "set" set

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        (set, (innerState, outerState))
        ||> HashSet.foldBack (fun el (innerState, outerState) ->
            // Apply the folder to the current element and state values.
            folder.Invoke (el, innerState, outerState))

