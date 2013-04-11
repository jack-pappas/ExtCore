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
module ExtCore.Control.Collections.ReaderState
    
open Microsoft.FSharp.Control
open OptimizedClosures
open ExtCore
open ExtCore.Collections


/// The standard F# Array module, lifted into the ReaderState monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array =
    /// A specialization of Array.iter which threads an accumulator through the computation;
    /// this allows the use of actions requiring a (possibly mutable) state variable.
    [<CompiledName("Iter")>]
    let iter (action : 'T -> 'Env -> 'State -> unit * 'State)
            (array : 'T[]) (env : 'Env) (state : 'State) : unit * 'State =
        // Preconditions
        checkNonNull "array" array

        let action = FSharpFunc<_,_,_,_>.Adapt action
        let len = array.Length
        let mutable state = state

        for i = 0 to len - 1 do
            state <- snd <| action.Invoke (array.[i], env, state)

        (), state

    /// A specialization of Array.iteri which threads an accumulator through the computation;
    /// this allows the use of actions requiring a (possibly mutable) state variable.
    [<CompiledName("IterIndexed")>]
    let iteri (action : int -> 'T -> 'Env -> 'State -> unit * 'State)
            (array : 'T[]) (env : 'Env) (state : 'State) : unit * 'State =
        // Preconditions
        checkNonNull "array" array

        let action = FSharpFunc<_,_,_,_,_>.Adapt action
        let len = array.Length
        let mutable state = state

        for i = 0 to len - 1 do
            state <- snd <| action.Invoke (i, array.[i], env, state)

        (), state

    /// A specialization of Array.map which threads an accumulator through the computation;
    /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'Env -> 'State -> 'U * 'State)
            (array : 'T[]) (env : 'Env) (state : 'State) : 'U[] * 'State =
        // Preconditions
        checkNonNull "array" array

        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
        let len = array.Length
        let results = Array.zeroCreate len
        let mutable state = state

        for i = 0 to len - 1 do
            let result, state' = mapping.Invoke (array.[i], env, state)
            results.[i] <- result
            state <- state'

        results, state

    /// A specialization of Array.mapi which threads an accumulator through the computation;
    /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> 'Env -> 'State -> 'U * 'State)
            (array : 'T[]) (env : 'Env) (state : 'State) : 'U[] * 'State =
        // Preconditions
        checkNonNull "array" array

        let mapping = FSharpFunc<_,_,_,_,_>.Adapt mapping
        let len = array.Length
        let results = Array.zeroCreate len
        let mutable state = state

        for i = 0 to len - 1 do
            let result, state' = mapping.Invoke (i, array.[i], env, state)
            results.[i] <- result
            state <- state'

        results, state

    /// A specialization of Array.map which threads an accumulator through the computation;
    /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
    /// This function traverses the input array from right-to-left.
    [<CompiledName("MapBack")>]
    let mapBack (mapping : 'T -> 'Env -> 'State -> 'U * 'State)
            (array : 'T[]) (env : 'Env) (state : 'State) : 'U[] * 'State =
        // Preconditions
        checkNonNull "array" array

        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
        let len = array.Length
        let results = Array.zeroCreate len
        let mutable state = state

        for i = len - 1 downto 0 do
            let result, state' = mapping.Invoke (array.[i], env, state)
            results.[i] <- result
            state <- state'

        results, state

    /// A specialization of Array.mapi which threads an accumulator through the computation;
    /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
    /// This function traverses the input array from right-to-left.
    [<CompiledName("MapIndexedBack")>]
    let mapiBack (mapping : int -> 'T -> 'Env -> 'State -> 'U * 'State)
            (array : 'T[]) (env : 'Env) (state : 'State) : 'U[] * 'State =
        // Preconditions
        checkNonNull "array" array

        let mapping = FSharpFunc<_,_,_,_,_>.Adapt mapping
        let len = array.Length
        let results = Array.zeroCreate len
        let mutable state = state

        for i = len - 1 downto 0 do
            let result, state' = mapping.Invoke (i, array.[i], env, state)
            results.[i] <- result
            state <- state'

        results, state

    /// A specialization of Array.map2 which threads an accumulator through the computation;
    /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
    /// This function traverses the input arrays from left-to-right.
    [<CompiledName("Map2")>]
    let map2 (mapping : 'T1 -> 'T2 -> 'Env -> 'State -> 'U * 'State)
            (array1 : 'T1[]) (array2 : 'T2[]) (env : 'Env) (state : 'State) : 'U[] * 'State =
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
            let result, state' = mapping.Invoke (array1.[i], array2.[i], env, state)
            results.[i] <- result
            state <- state'

        results, state

    /// A specialization of Array.mapi2 which threads an accumulator through the computation;
    /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
    /// This function traverses the input arrays from left-to-right.
    [<CompiledName("MapIndexed2")>]
    let mapi2 (mapping : int -> 'T1 -> 'T2 -> 'Env -> 'State -> 'U * 'State)
            (array1 : 'T1[]) (array2 : 'T2[]) (env : 'Env) (state : 'State) : 'U[] * 'State =
        // Preconditions
        checkNonNull "array1" array1
        checkNonNull "array2" array2

        let len = array1.Length

        if array2.Length <> len then
            invalidArg "array2" "The arrays have differing lengths."

        let mapping = FSharpFunc<_,_,_,_,_,_>.Adapt mapping
        let results = Array.zeroCreate len
        let mutable state = state

        for i = 0 to len - 1 do
            let result, state' = mapping.Invoke (i, array1.[i], array2.[i], env, state)
            results.[i] <- result
            state <- state'

        results, state

    /// Applies a function to each element of the collection, threading an accumulator argument through the computation.
    [<CompiledName("Fold")>]
    let fold (folder : 'InnerState -> 'T -> 'Env -> 'OuterState -> 'InnerState * 'OuterState)
            (innerState : 'InnerState) (array : 'T[]) (env : 'Env) (outerState : 'OuterState)
            : 'InnerState * 'OuterState =
        // Preconditions
        checkNonNull "array" array

        let folder = FSharpFunc<_,_,_,_,_>.Adapt folder
        let len = array.Length
        let mutable outerState = outerState
        let mutable innerState = innerState

        for i = 0 to len - 1 do
            let innerState', outerState' = folder.Invoke (innerState, array.[i], env, outerState)
            innerState <- innerState'
            outerState <- outerState'

        innerState, outerState

    /// Applies a function to each element of the collection, threading an accumulator argument through the computation.
    /// The integer index passed to the function indicates the array index of the element being transformed.
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : int -> 'InnerState -> 'T -> 'Env -> 'OuterState -> 'InnerState * 'OuterState)
            (innerState : 'InnerState) (array : 'T[]) (env : 'Env) (outerState : 'OuterState)
            : 'InnerState * 'OuterState =
        // Preconditions
        checkNonNull "array" array

        let folder = FSharpFunc<_,_,_,_,_,_>.Adapt folder
        let len = array.Length
        let mutable outerState = outerState
        let mutable innerState = innerState

        for i = 0 to len - 1 do
            let innerState', outerState' = folder.Invoke (i, innerState, array.[i], env, outerState)
            innerState <- innerState'
            outerState <- outerState'

        innerState, outerState


/// The ExtCore.Collections.TaggedArray module, lifted into the ReaderState monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TaggedArray =
    open LanguagePrimitives

    /// A specialization of Array.mapi which threads an accumulator through the computation;
    /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
    /// The index values are tagged with a unit-of-measure type before applying them to the mapping function.
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int<'Tag> -> 'T -> 'Env -> 'State -> 'U * 'State)
            (array : 'T[]) (env : 'Env) (state : 'State) : 'U[] * 'State =
        // Preconditions
        checkNonNull "array" array

        let mapping = FSharpFunc<_,_,_,_,_>.Adapt mapping
        let len = array.Length
        let results = Array.zeroCreate len
        let mutable state = state

        for i = 0 to len - 1 do
            let result, state' = mapping.Invoke (Int32WithMeasure<'Tag> i, array.[i], env, state)
            results.[i] <- result
            state <- state'

        results, state

    /// A specialization of Array.mapi which threads an accumulator through the computation;
    /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
    /// This function traverses the input array from right-to-left.
    /// The index values are tagged with a unit-of-measure type before applying them to the mapping function.
    [<CompiledName("MapIndexedBack")>]
    let mapiBack (mapping : int<'Tag> -> 'T -> 'Env -> 'State -> 'U * 'State)
            (array : 'T[]) (env : 'Env) (state : 'State) : 'U[] * 'State =
        // Preconditions
        checkNonNull "array" array

        let mapping = FSharpFunc<_,_,_,_,_>.Adapt mapping
        let len = array.Length
        let results = Array.zeroCreate len
        let mutable state = state

        for i = len - 1 downto 0 do
            let result, state' = mapping.Invoke (Int32WithMeasure<'Tag> i, array.[i], env, state)
            results.[i] <- result
            state <- state'

        results, state

    /// Applies a function to each element of the collection, threading an accumulator argument through the computation.
    /// The integer index passed to the function indicates the array index of the element being transformed.
    /// The index values are tagged with a unit-of-measure type before applying them to the folder function.
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : int<'Tag> -> 'InnerState -> 'T -> 'Env -> 'OuterState -> 'InnerState * 'OuterState)
            (innerState : 'InnerState) (array : 'T[]) (env : 'Env) (outerState : 'OuterState)
            : 'InnerState * 'OuterState =
        // Preconditions
        checkNonNull "array" array

        let folder = FSharpFunc<_,_,_,_,_,_>.Adapt folder
        let len = array.Length
        let mutable outerState = outerState
        let mutable innerState = innerState

        for i = 0 to len - 1 do
            let innerState', outerState' =
                folder.Invoke (Int32WithMeasure<'Tag> i, innerState, array.[i], env, outerState)
            innerState <- innerState'
            outerState <- outerState'

        innerState, outerState


/// The ExtCore.Collections.ArrayView module, lifted into the ReaderState monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ArrayView =
    /// A specialization of ArrayView.iter which threads an accumulator through the computation;
    /// this allows the use of actions requiring a (possibly mutable) state variable.
    [<CompiledName("Iter")>]
    let iter (action : 'T -> 'Env -> 'State -> unit * 'State)
            (view : ArrayView<'T>) (env : 'Env) (state : 'State) : unit * 'State =
        let action = FSharpFunc<_,_,_,_>.Adapt action

        let array = view.Array
        let endExclusive = view.Offset + view.Count
        let mutable state = state

        for i = view.Offset to endExclusive - 1 do
            state <- snd <| action.Invoke (array.[i], env, state)

        (), state

    /// A specialization of ArrayView.iteri which threads an accumulator through the computation;
    /// this allows the use of actions requiring a (possibly mutable) state variable.
    [<CompiledName("IterIndexed")>]
    let iteri (action : int -> 'T -> 'Env -> 'State -> unit * 'State)
            (view : ArrayView<'T>) (env : 'Env) (state : 'State) : unit * 'State =
        let action = FSharpFunc<_,_,_,_,_>.Adapt action

        let array = view.Array
        let endExclusive = view.Offset + view.Count
        let mutable state = state
        let mutable idx = 0

        for i = view.Offset to endExclusive - 1 do
            state <- snd <| action.Invoke (idx, array.[i], env, state)
            idx <- idx + 1

        (), state

    /// A specialization of Array.map which threads an accumulator through the computation;
    /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'Env -> 'State -> 'U * 'State)
            (view : ArrayView<'T>) (env : 'Env) (state : 'State) : 'U[] * 'State =
        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping

        let array = view.Array
        let offset = view.Offset
        let count = view.Count
            
        let results = Array.zeroCreate count
        let mutable state = state

        for i = 0 to count - 1 do
            let result, state' = mapping.Invoke (array.[offset + i], env, state)
            results.[i] <- result
            state <- state'

        results, state

    /// A specialization of Array.mapi which threads an accumulator through the computation;
    /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> 'Env -> 'State -> 'U * 'State)
            (view : ArrayView<'T>) (env : 'Env) (state : 'State) : 'U[] * 'State =
        let mapping = FSharpFunc<_,_,_,_,_>.Adapt mapping

        let array = view.Array
        let offset = view.Offset
        let count = view.Count
            
        let results = Array.zeroCreate count
        let mutable state = state

        for i = 0 to count - 1 do
            let result, state' = mapping.Invoke (i, array.[offset + i], env, state)
            results.[i] <- result
            state <- state'

        results, state
