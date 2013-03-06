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


//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Async =
    open Microsoft.FSharp.Control

    /// The standard F# Array module, lifted into the Async monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Array =
        open System.Collections
        
        (* OPTIMIZE :   Some of the functions below build an Async workflow by folding forwards (left-to-right)
                        over the array. Depending on how async is implemented internally though, it might
                        be more efficient to fold backwards so that as each element is processed it can tail-call
                        the next element. *)

        //
        [<CompiledName("Initialize")>]
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
            let result1 = resultList1.ToArray ()
            let result2 = resultList2.ToArray ()
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
        let foldi (folder : int -> 'State -> 'T -> Async<'State>) (state : 'State) (array : 'T[]) : Async<'State> =
            // Preconditions
            checkNonNull "array" array

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
            ||> Array.foldi (fun index iterPrevious el ->
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


//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State =
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


    /// The standard F# List module, lifted into the State monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module List =
        /// A specialization of List.iter which threads an accumulator through the computation;
        /// this allows the use of actions requiring a (possibly mutable) state variable.
        [<CompiledName("Iterate")>]
        let iter (action : 'T -> 'State -> unit * 'State) (list : 'T list) (state : 'State) : unit * 'State =
            // Preconditions
            checkNonNull "list" list

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

            let action = FSharpFunc<_,_,_,_>.Adapt action
            let mutable list = list
            let mutable state = state
            let mutable index = 0

            while not <| List.isEmpty list do
                state <- snd <| action.Invoke (index, List.head list, state)
                list <- List.tail list
                index <- index + 1

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


    /// The ExtCore.Collections.ArraySegment module, lifted into the State monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ArraySegment =
        /// A specialization of ArraySegment.iter which threads an accumulator through the computation; this allows
        /// the use of actions requiring a (possibly mutable) state variable.
        [<CompiledName("Iterate")>]
        let iter (action : 'T -> 'State -> unit * 'State)
                (segment : System.ArraySegment<'T>) (state : 'State) : unit * 'State =
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
        let iteri (action : int -> 'T -> 'State -> unit * 'State)
                (segment : System.ArraySegment<'T>) (state : 'State) : unit * 'State =
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
        let map (mapping : 'T -> 'State -> 'U * 'State)
                (segment : System.ArraySegment<'T>) (state : 'State) : 'U[] * 'State =
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
        let mapi (mapping : int -> 'T -> 'State -> 'U * 'State)
                (segment : System.ArraySegment<'T>) (state : 'State) : 'U[] * 'State =
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
module Reader =
    /// The standard F# Array module, lifted into the Reader monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Array =
        //
        [<CompiledName("Iterate")>]
        let iter (action : 'T -> 'Env -> unit) (array : 'T[]) (env : 'Env) : unit =
            // Preconditions
            checkNonNull "array" array

            let action = FSharpFunc<_,_,_>.Adapt action
            let len = Array.length array
            /// Holds the mapped results.
            let results = Array.zeroCreate len

            for i = 0 to len - 1 do
                results.[i] <- action.Invoke (array.[i], env)

        //
        [<CompiledName("IterateIndexed")>]
        let iteri (action : int -> 'T -> 'Env -> unit) (array : 'T[]) (env : 'Env) : unit =
            // Preconditions
            checkNonNull "array" array

            let action = FSharpFunc<_,_,_,_>.Adapt action
            let len = Array.length array
            /// Holds the mapped results.
            let results = Array.zeroCreate len

            for i = 0 to len - 1 do
                results.[i] <- action.Invoke (i, array.[i], env)

        //
        [<CompiledName("Map")>]
        let map (mapping : 'T -> 'Env -> 'U) (array : 'T[]) (env : 'Env) : 'U[] =
            // Preconditions
            checkNonNull "array" array

            let mapping = FSharpFunc<_,_,_>.Adapt mapping
            let len = Array.length array
            /// Holds the mapped results.
            let results = Array.zeroCreate len

            for i = 0 to len - 1 do
                results.[i] <- mapping.Invoke (array.[i], env)
            results

        //
        [<CompiledName("MapIndexed")>]
        let mapi (mapping : int -> 'T -> 'Env -> 'U) (array : 'T[]) (env : 'Env) : 'U[] =
            // Preconditions
            checkNonNull "array" array

            let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
            let len = Array.length array
            /// Holds the mapped results.
            let results = Array.zeroCreate len

            for i = 0 to len - 1 do
                results.[i] <- mapping.Invoke (i, array.[i], env)
            results


//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ReaderState =
    /// The standard F# Array module, lifted into the ReaderState monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Array =
        /// A specialization of Array.iter which threads an accumulator through the computation;
        /// this allows the use of actions requiring a (possibly mutable) state variable.
        [<CompiledName("Iterate")>]
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
        [<CompiledName("IterateIndexed")>]
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
        [<CompiledName("MapIndexedByTag")>]
        let mapti (mapping : int<'Tag> -> 'T -> 'Env -> 'State -> 'U * 'State)
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
        [<CompiledName("MapIndexedByTagBack")>]
        let maptiBack (mapping : int<'Tag> -> 'T -> 'Env -> 'State -> 'U * 'State)
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
        [<CompiledName("FoldIndexedByTag")>]
        let foldti (folder : int<'Tag> -> 'InnerState -> 'T -> 'Env -> 'OuterState -> 'InnerState * 'OuterState)
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


    /// The ExtCore.Collections.ArraySegment module, lifted into the ReaderState monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ArraySegment =
        /// A specialization of ArraySegment.iter which threads an accumulator through the computation;
        /// this allows the use of actions requiring a (possibly mutable) state variable.
        [<CompiledName("Iterate")>]
        let iter (action : 'T -> 'Env -> 'State -> unit * 'State)
                (segment : System.ArraySegment<'T>) (env : 'Env) (state : 'State) : unit * 'State =
            let action = FSharpFunc<_,_,_,_>.Adapt action

            let array = segment.Array
            let endExclusive = segment.Offset + segment.Count
            let mutable state = state

            for i = segment.Offset to endExclusive - 1 do
                state <- snd <| action.Invoke (array.[i], env, state)

            (), state

        /// A specialization of ArraySegment.iteri which threads an accumulator through the computation;
        /// this allows the use of actions requiring a (possibly mutable) state variable.
        [<CompiledName("IterateIndexed")>]
        let iteri (action : int -> 'T -> 'Env -> 'State -> unit * 'State)
                (segment : System.ArraySegment<'T>) (env : 'Env) (state : 'State) : unit * 'State =
            let action = FSharpFunc<_,_,_,_,_>.Adapt action

            let array = segment.Array
            let endExclusive = segment.Offset + segment.Count
            let mutable state = state
            let mutable idx = 0

            for i = segment.Offset to endExclusive - 1 do
                state <- snd <| action.Invoke (idx, array.[i], env, state)
                idx <- idx + 1

            (), state

        /// A specialization of Array.map which threads an accumulator through the computation;
        /// this allows the use of mapping functions requiring a (possibly mutable) state variable.
        [<CompiledName("Map")>]
        let map (mapping : 'T -> 'Env -> 'State -> 'U * 'State)
                (segment : System.ArraySegment<'T>) (env : 'Env) (state : 'State) : 'U[] * 'State =
            let mapping = FSharpFunc<_,_,_,_>.Adapt mapping

            let array = segment.Array
            let offset = segment.Offset
            let count = segment.Count
            
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
                (segment : System.ArraySegment<'T>) (env : 'Env) (state : 'State) : 'U[] * 'State =
            let mapping = FSharpFunc<_,_,_,_,_>.Adapt mapping

            let array = segment.Array
            let offset = segment.Offset
            let count = segment.Count
            
            let results = Array.zeroCreate count
            let mutable state = state

            for i = 0 to count - 1 do
                let result, state' = mapping.Invoke (i, array.[offset + i], env, state)
                results.[i] <- result
                state <- state'

            results, state


//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Maybe =
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
        [<CompiledName("Initialize")>]
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
        [<CompiledName("Iterate")>]
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
        [<CompiledName("IterateIndexed")>]
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
        [<CompiledName("Iterate2")>]
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
        [<CompiledName("Iterate")>]
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


    /// The ArraySegment module, lifted into the Maybe monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ArraySegment =
        //
        [<CompiledName("Fold")>]
        let fold (folder : 'State -> 'T -> 'State option)
                (state : 'State) (segment : System.ArraySegment<'T>) : 'State option =
            let folder = FSharpFunc<_,_,_>.Adapt folder

            let array = segment.Array
            let endExclusive = segment.Offset + segment.Count

            let mutable index = segment.Offset
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


//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ReaderMaybe =
    /// The standard F# Array module, lifted into the ReaderMaybe monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Array =
        //
        [<CompiledName("Map")>]
        let map (mapping : 'T -> 'Env -> 'U option) (array : 'T[]) (env : 'Env) : 'U[] option =
            // Preconditions
            checkNonNull "array" array

            let mapping = FSharpFunc<_,_,_>.Adapt mapping
            let len = array.Length
            let results = Array.zeroCreate len

            let mutable index = 0
            let mutable foundError = false

            while index < len && not foundError do
                match mapping.Invoke (array.[index], env) with
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
        let mapi (mapping : int -> 'T -> 'Env -> 'U option) (array : 'T[]) (env : 'Env) : 'U[] option =
            // Preconditions
            checkNonNull "array" array

            let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
            let len = array.Length
            let results = Array.zeroCreate len

            let mutable index = 0
            let mutable foundError = false

            while index < len && not foundError do
                match mapping.Invoke (index, array.[index], env) with
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
        let map2 (mapping : 'T1 -> 'T2 -> 'Env -> 'U option)
                (array1 : 'T1[]) (array2 : 'T2[]) (env : 'Env) : 'U[] option =
            // Preconditions
            checkNonNull "array1" array1
            checkNonNull "array2" array2

            let len = array1.Length
            if array2.Length <> len then
                invalidArg "array2" "The arrays have differing lengths."

            let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
            let results = Array.zeroCreate len

            let mutable index = 0
            let mutable foundError = false

            while index < len && not foundError do
                match mapping.Invoke (array1.[index], array2.[index], env) with
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
        let fold (folder : 'State -> 'T -> 'Env -> 'State option)
                (state : 'State) (array : 'T[]) (env : 'Env) : 'State option =
            // Preconditions
            checkNonNull "array" array

            let folder = FSharpFunc<_,_,_,_>.Adapt folder
            let len = array.Length
            let mutable state = state

            let mutable index = 0
            let mutable foundError = false

            while index < len && not foundError do
                match folder.Invoke (state, array.[index], env) with
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
        let foldi (folder : int -> 'State -> 'T -> 'Env -> 'State option)
                (state : 'State) (array : 'T[]) (env : 'Env) : 'State option =
            // Preconditions
            checkNonNull "array" array

            let folder = FSharpFunc<_,_,_,_,_>.Adapt folder
            let len = array.Length
            let mutable state = state

            let mutable index = 0
            let mutable foundError = false

            while index < len && not foundError do
                match folder.Invoke (index, state, array.[index], env) with
                | None ->
                    foundError <- true
                | Some newState ->
                    state <- newState
                    index <- index + 1

            // If an error was encountered, return None.
            if foundError then None
            else Some state

        //
        [<CompiledName("Initialize")>]
        let init (count : int) (initializer : int -> 'Env -> 'T option) (env : 'Env) : 'T[] option =
            // Preconditions
            if count < 0 then
                invalidArg "count" "The count cannot be negative."

            let initializer = FSharpFunc<_,_,_>.Adapt initializer
            let results = Array.zeroCreate count
            let mutable currentIndex = 0
            let mutable foundError = false

            while currentIndex < count && not foundError do
                match initializer.Invoke (currentIndex, env) with
                | None ->
                    foundError <- true
                | Some value ->
                    results.[currentIndex] <- value
                    currentIndex <- currentIndex + 1

            // If an error was encountered, return None.
            if foundError then None
            else Some results

        //
        [<CompiledName("Iterate")>]
        let iter (action : 'T -> 'Env -> unit option) (array : 'T[]) (env : 'Env) : unit option =
            // Preconditions
            checkNonNull "array" array

            let action = FSharpFunc<_,_,_>.Adapt action
            let len = array.Length
            let mutable index = 0
            let mutable foundError = false

            while index < len && not foundError do
                match action.Invoke (array.[index], env) with
                | None ->
                    foundError <- true
                | Some () ->
                    index <- index + 1
            
            // If an error was encountered, return None.
            if foundError then None
            else Some ()

        //
        [<CompiledName("IterateIndexed")>]
        let iteri (action : int -> 'T -> 'Env -> unit option) (array : 'T[]) (env : 'Env) : unit option =
            // Preconditions
            checkNonNull "array" array

            let action = FSharpFunc<_,_,_,_>.Adapt action
            let len = array.Length

            let mutable index = 0
            let mutable foundError = false

            while index < len && not foundError do
                match action.Invoke (index, array.[index], env) with
                | None ->
                    foundError <- true
                | Some () ->
                    index <- index + 1
            
            // If an error was encountered, return None.
            if foundError then None
            else Some ()

        //
        [<CompiledName("Reduce")>]
        let reduce (reduction : 'T -> 'T -> 'Env -> 'T option) (array : 'T[]) (env : 'Env) : 'T option =
            // Preconditions
            checkNonNull "array" array
            if Array.isEmpty array then
                invalidArg "array" "The array is empty."

            let reduction = FSharpFunc<_,_,_,_>.Adapt reduction
            let len = array.Length

            let mutable state = array.[0]   // The first (0-th) element is the initial state.
            let mutable index = 1   // Start at the *second* element (index = 1)
            let mutable foundError = false

            while index < len && not foundError do
                match reduction.Invoke (state, array.[index], env) with
                | None ->
                    foundError <- true
                | Some newState ->
                    state <- newState
                    index <- index + 1
            
            // If an error was encountered, return None.
            if foundError then None
            else Some state


    /// The standard F# List module, lifted into the ReaderMaybe monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module List =
        //
        [<CompiledName("Fold")>]
        let fold (folder : 'State -> 'T -> 'Env -> 'State option)
                (state : 'State) (lst : 'T list) (env : 'Env) =
            // Preconditions
            checkNonNull "lst" lst

            let folder = FSharpFunc<_,_,_,_>.Adapt folder
            
            let rec foldRec (state, lst) =
                match lst with
                | [] ->
                    Some state
                | hd :: tl ->
                    // Apply the function to the head of the list.
                    // If the result is an error, return it;
                    // otherwise, continue processing recursively.
                    match folder.Invoke (state, hd, env) with
                    | None ->
                        None
                    | Some state ->
                        foldRec (state, tl)

            // Call the recursive implementation function.
            foldRec (state, lst)
        
        //
        [<CompiledName("Map2")>]
        let map2 (mapping : 'T1 -> 'T2 -> 'Env -> 'U option)
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
                    |> Some
                
                | hd1 :: tl1, hd2 :: tl2 ->
                    // Apply the function to the heads of the lists.
                    // If the result is an error, return it;
                    // otherwise continue processing recursively.
                    match mapping.Invoke (hd1, hd2, env) with
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
        let mapi2 (mapping : int -> 'T1 -> 'T2 -> 'Env -> 'U option)
                (list1 : 'T1 list) (list2 : 'T2 list) (env : 'Env) : 'U list option =
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
                    |> Some
                
                | hd1 :: tl1, hd2 :: tl2 ->
                    // Apply the function to the heads of the lists.
                    // If the result is an error, return it;
                    // otherwise continue processing recursively.
                    match mapping.Invoke (index, hd1, hd2, env) with
                    | None ->
                        None
                    | Some result ->
                        mapRec (result :: acc, index + 1, tl1, tl2)

                | _, _ ->
                    failwith "The lists have differing lengths -- they may have been modified in some invalid way."
                        
            // Call the recursive implementation function. 
            mapRec (List.empty, 0, list1, list2)

        //
        [<CompiledName("Iterate2")>]
        let iter2 (action : 'T1 -> 'T2 -> 'Env -> unit option)
                (list1 : 'T1 list) (list2 : 'T2 list) (env : 'Env) : unit option =
            // Preconditions
            checkNonNull "list1" list1
            checkNonNull "list2" list2
            if List.length list1 <> List.length list2 then
                invalidArg "list2" "The lists have different lengths."

            let action = FSharpFunc<_,_,_,_>.Adapt action

            let rec iterRec (list1, list2) =
                match list1, list2 with
                | [], [] ->
                    Some ()
                
                | hd1 :: tl1, hd2 :: tl2 ->
                    // Apply the function to the heads of the lists.
                    // If the result is an error, return it;
                    // otherwise continue processing recursively.
                    match action.Invoke (hd1, hd2, env) with
                    | None ->
                        None
                    | Some () ->
                        iterRec  (tl1, tl2)

                | _, _ ->
                    failwith "The lists have differing lengths -- they may have been modified in some invalid way."
                        
            // Call the recursive implementation function. 
            iterRec (list1, list2)

    /// The standard F# Seq module, lifted into the ReaderMaybe monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Seq =
        //
        [<CompiledName("Iterate")>]
        let iter (action : 'T -> 'Env -> unit option)
                (sequence : seq<'T>) (env : 'Env) : unit option =
            // Preconditions
            checkNonNull "seq" seq

            let action = FSharpFunc<_,_,_>.Adapt action
            let mutable foundError = false

            let enumerator = sequence.GetEnumerator ()
            while enumerator.MoveNext () && not foundError do
                match action.Invoke (enumerator.Current, env) with
                | None ->
                    foundError <- true
                | Some () ->
                    ()

            // If an error was encountered, return None.
            if foundError then None
            else Some ()


    /// The standard F# Set module, lifted into the ReaderMaybe monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Set =
        //
        [<CompiledName("Fold")>]
        let fold (folder : 'State -> 'T -> 'Env -> 'State option)
                (state : 'State) (set : Set<'T>) (env : 'Env) : 'State option =
            // Preconditions
            checkNonNull "set" set

            let folder = FSharpFunc<_,_,_,_>.Adapt folder

            (* TODO : Is there a better (more performant) way to implement this than using 'IEnumerable'? *)
            use setEnumerator =
                let setAsEnumerable = set :> System.Collections.Generic.IEnumerable<'T>
                setAsEnumerable.GetEnumerator ()

            let mutable state = state
            let mutable foundError = false

            while setEnumerator.MoveNext () && not foundError do
                match folder.Invoke (state, setEnumerator.Current, env) with
                | None ->
                    foundError <- true
                | Some state' ->
                    state <- state'

            // If an error was encountered, return None.
            if foundError then
                None
            else
                Some state

        //
        [<CompiledName("MapToArray")>]
        let mapToArray (mapping : 'T -> 'Env -> 'U option)
                (set : Set<'T>) (env : 'Env) : 'U[] option =
            // Preconditions
            checkNonNull "set" set

            let results = Array.zeroCreate <| Set.count set            

            (* TODO : Is there a better (more performant) way to implement this than using 'IEnumerable'? *)
            use setEnumerator =
                let setAsEnumerable = set :> System.Collections.Generic.IEnumerable<'T>
                setAsEnumerable.GetEnumerator ()

            let mapping = FSharpFunc<_,_,_>.Adapt mapping
            let mutable index = 0
            let mutable foundError = false

            while setEnumerator.MoveNext () && not foundError do
                match mapping.Invoke (setEnumerator.Current, env) with
                | None ->
                    foundError <- true
                | Some result ->
                    results.[index] <- result
                    index <- index + 1 

            // If an error was encountered, return None.
            if foundError then
                None
            else
                Some results


    /// The ArraySegment module, lifted into the ReaderMaybe monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ArraySegment =
        //
        [<CompiledName("Fold")>]
        let fold (folder : 'State -> 'T -> 'Env -> 'State option)
                (state : 'State) (segment : System.ArraySegment<'T>) (env : 'Env) : 'State option =
            let folder = FSharpFunc<_,_,_,_>.Adapt folder

            let array = segment.Array
            let endExclusive = segment.Offset + segment.Count

            let mutable index = segment.Offset
            let mutable state = state
            let mutable foundError = false

            while index < endExclusive && not foundError do
                match folder.Invoke (state, array.[index], env) with
                | None ->
                    foundError <- true
                | Some state' ->
                    state <- state'
                    index <- index + 1
            
            // If an error was encountered, return None.
            if foundError then
                None
            else
                Some state


//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Choice =
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


//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ReaderChoice =
    /// The standard F# Array module, lifted into the ReaderChoice monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Array =
        //
        [<CompiledName("Map")>]
        let map (mapping : 'T -> 'Env -> Choice<'U, 'Error>) (array : 'T[]) (env : 'Env) =
            // Preconditions
            checkNonNull "array" array

            let mapping = FSharpFunc<_,_,_>.Adapt mapping
            let len = array.Length
            let results = Array.zeroCreate len

            let mutable index = 0
            let mutable error = None

            while index < len && Option.isNone error do
                match mapping.Invoke (array.[index], env) with
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
        let mapi (mapping : int -> 'T -> 'Env -> Choice<'U, 'Error>) (array : 'T[]) (env : 'Env) =
            // Preconditions
            checkNonNull "array" array

            let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
            let len = array.Length
            let results = Array.zeroCreate len

            let mutable index = 0
            let mutable error = None

            while index < len && Option.isNone error do
                match mapping.Invoke (index, array.[index], env) with
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
        let map2 (mapping : 'T1 -> 'T2 -> 'Env -> Choice<'U, 'Error>)
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
        let fold (folder : 'State -> 'T -> 'Env -> Choice<'State, 'Error>)
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
        let foldi (folder : int -> 'State -> 'T -> 'Env -> Choice<'State, 'Error>)
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
        let init (count : int) (initializer : int -> 'Env -> Choice<'T, 'Error>) (env : 'Env) =
            // Preconditions
            if count < 0 then
                invalidArg "count" "The count cannot be negative."

            let initializer = FSharpFunc<_,_,_>.Adapt initializer
            let results = Array.zeroCreate count
            let mutable currentIndex = 0
            let mutable error = None

            while currentIndex < count && Option.isNone error do
                match initializer.Invoke (currentIndex, env) with
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
        let iter (action : 'T -> 'Env -> Choice<unit, 'Error>) (array : 'T[]) (env : 'Env) =
            // Preconditions
            checkNonNull "array" array

            let action = FSharpFunc<_,_,_>.Adapt action
            let len = array.Length
            let mutable index = 0
            let mutable error = None

            while index < len && Option.isNone error do
                match action.Invoke (array.[index], env) with
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
        let iteri (action : int -> 'T -> 'Env -> Choice<unit, 'Error>) (array : 'T[]) (env : 'Env) =
            // Preconditions
            checkNonNull "array" array

            let action = FSharpFunc<_,_,_,_>.Adapt action
            let len = array.Length

            let mutable index = 0
            let mutable error = None

            while index < len && Option.isNone error do
                match action.Invoke (index, array.[index], env) with
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
        let reduce (reduction : 'T -> 'T -> 'Env -> Choice<'T, 'Error>) (array : 'T[]) (env : 'Env) =
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


    /// The standard F# List module, lifted into the ReaderChoice monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module List =
        //
        [<CompiledName("Fold")>]
        let fold (folder : 'State -> 'T -> 'Env -> Choice<'State, 'Error>)
                (state : 'State) (lst : 'T list) (env : 'Env) =
            // Preconditions
            checkNonNull "lst" lst

            let folder = FSharpFunc<_,_,_,_>.Adapt folder
            
            let rec foldRec (state, lst) =
                match lst with
                | [] ->
                    Choice1Of2 state
                | hd :: tl ->
                    // Apply the function to the head of the list.
                    // If the result is an error, return it;
                    // otherwise, continue processing recursively.
                    match folder.Invoke (state, hd, env) with
                    | (Choice2Of2 _) as error ->
                        error
                    | Choice1Of2 state ->
                        foldRec (state, tl)

            // Call the recursive implementation function.
            foldRec (state, lst)
        
        //
        [<CompiledName("Map2")>]
        let map2 (mapping : 'T1 -> 'T2 -> 'Env -> Choice<'U, 'Error>)
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
                    |> Choice1Of2
                
                | hd1 :: tl1, hd2 :: tl2 ->
                    // Apply the function to the heads of the lists.
                    // If the result is an error, return it;
                    // otherwise continue processing recursively.
                    match mapping.Invoke (hd1, hd2, env) with
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
        let mapi2 (mapping : int -> 'T1 -> 'T2 -> 'Env -> Choice<'U, 'Error>)
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
                    |> Choice1Of2
                
                | hd1 :: tl1, hd2 :: tl2 ->
                    // Apply the function to the heads of the lists.
                    // If the result is an error, return it;
                    // otherwise continue processing recursively.
                    match mapping.Invoke (index, hd1, hd2, env) with
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
        let iter2 (action : 'T1 -> 'T2 -> 'Env -> Choice<unit, 'Error>)
                (list1 : 'T1 list) (list2 : 'T2 list) (env : 'Env) : Choice<unit, 'Error> =
            // Preconditions
            checkNonNull "list1" list1
            checkNonNull "list2" list2
            if List.length list1 <> List.length list2 then
                invalidArg "list2" "The lists have different lengths."

            let action = FSharpFunc<_,_,_,_>.Adapt action

            let rec mapRec (list1, list2) =
                match list1, list2 with
                | [], [] ->
                    Choice1Of2 ()
                
                | hd1 :: tl1, hd2 :: tl2 ->
                    // Apply the function to the heads of the lists.
                    // If the result is an error, return it;
                    // otherwise continue processing recursively.
                    match action.Invoke (hd1, hd2, env) with
                    | Choice2Of2 error ->
                        Choice2Of2 error
                    | Choice1Of2 result ->
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
        let iter (action : 'T -> 'Env -> Choice<unit, 'Error>)
                (sequence : seq<'T>) (env : 'Env) : Choice<unit, 'Error> =
            // Preconditions
            checkNonNull "seq" seq

            let action = FSharpFunc<_,_,_>.Adapt action
            let mutable error = None

            let enumerator = sequence.GetEnumerator ()
            while enumerator.MoveNext () && Option.isNone error do
                match action.Invoke (enumerator.Current, env) with
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


    /// The standard F# Set module, lifted into the ReaderChoice monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Set =
        //
        [<CompiledName("Fold")>]
        let fold (folder : 'State -> 'T -> 'Env -> Choice<'State, 'Error>)
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
        let mapToArray (mapping : 'T -> 'Env -> Choice<'U, 'Error>)
                (set : Set<'T>) (env : 'Env) : Choice<'U[], 'Error> =
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


    /// The ArraySegment module, lifted into the ReaderChoice monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ArraySegment =
        //
        [<CompiledName("Fold")>]
        let fold (folder : 'State -> 'T -> 'Env -> Choice<'State, 'Error>)
                (state : 'State) (segment : System.ArraySegment<'T>) (env : 'Env) =
            let folder = FSharpFunc<_,_,_,_>.Adapt folder

            let array = segment.Array
            let endExclusive = segment.Offset + segment.Count

            let mutable index = segment.Offset
            let mutable state = state
            let mutable error = None

            while index < endExclusive && Option.isNone error do
                match folder.Invoke (state, array.[index], env) with
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
module ProtectedState =
    /// The standard F# Array module, lifted into the ProtectedState monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Array =
        /// A specialization of Array.iter which threads an accumulator through the computation
        /// and which also short-circuits the computation if the mapping function returns an
        /// error when any element is applied to it.
        [<CompiledName("Iterate")>]
        let iter (action : 'T -> 'State -> Choice<unit * 'State, 'Error>)
                (array : 'T[]) (state : 'State) : Choice<unit * 'State, 'Error> =
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

        /// A specialization of Array.iteri which threads an accumulator through the computation
        /// and which also short-circuits the computation if the mapping function returns an
        /// error when any element is applied to it.
        [<CompiledName("IterateIndexed")>]
        let iteri (action : int -> 'T -> 'State -> Choice<unit * 'State, 'Error>)
                (array : 'T[]) (state : 'State) : Choice<unit * 'State, 'Error> =
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

        /// A specialization of Array.map which threads an accumulator through the computation
        /// and which also short-circuits the computation if the mapping function returns an
        /// error when any element is applied to it.
        [<CompiledName("Map")>]
        let map (mapping : 'T -> 'State -> Choice<'U * 'State, 'Error>)
                (array : 'T[]) (state : 'State) : Choice<'U[] * 'State, 'Error> =
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

        /// A specialization of Array.mapi which threads an accumulator through the computation
        /// and which also short-circuits the computation if the mapping function returns an
        /// error when any element is applied to it.
        [<CompiledName("MapIndexed")>]
        let mapi (mapping : int -> 'T -> 'State -> Choice<'U * 'State, 'Error>)
                (array : 'T[]) (state : 'State) : Choice<'U[] * 'State, 'Error> =
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
        /// A specialization of List.map which threads an accumulator through the computation
        /// and which also short-circuits the computation if the mapping function returns an
        /// error when any element is applied to it.
        [<CompiledName("Map")>]
        let map (mapping : 'T -> 'State -> Choice<'U * 'State, 'Error>)
                (list : 'T list) (state : 'State) : Choice<'U list * 'State, 'Error> =
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


    /// The ExtCore.Collections.ArraySegment module, lifted into the ProtectedState monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ArraySegment =
        /// A specialization of ArraySegment.iter which threads an accumulator through the
        /// computation and which also short-circuits the computation if the mapping function
        /// returns an error when any element is applied to it.
        [<CompiledName("Iterate")>]
        let iter (action : 'T -> 'State -> Choice<unit * 'State, 'Error>)
                (segment : System.ArraySegment<'T>) (state : 'State) : Choice<unit * 'State, 'Error> =
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

        /// A specialization of ArraySegment.iteri which threads an accumulator through the
        /// computation and which also short-circuits the computation if the mapping function
        /// returns an error when any element is applied to it.
        [<CompiledName("IterateIndexed")>]
        let iteri (action : int -> 'T -> 'State -> Choice<unit * 'State, 'Error>)
                (segment : System.ArraySegment<'T>) (state : 'State) : Choice<unit * 'State, 'Error> =
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

        /// A specialization of ArraySegment.map which threads an accumulator through the
        /// computation and which also short-circuits the computation if the mapping function
        /// returns an error when any element is applied to it.
        [<CompiledName("Map")>]
        let map (mapping : 'T -> 'State -> Choice<'U * 'State, 'Error>)
                (segment : System.ArraySegment<'T>) (state : 'State) : Choice<'U[] * 'State, 'Error> =
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

        /// A specialization of ArraySegment.mapi which threads an accumulator through the
        /// computation and which also short-circuits the computation if the mapping function
        /// returns an error when any element is applied to it.
        [<CompiledName("MapIndexed")>]
        let mapi (mapping : int -> 'T -> 'State -> Choice<'U * 'State, 'Error>)
                (segment : System.ArraySegment<'T>) (state : 'State) : Choice<'U[] * 'State, 'Error> =
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
module ReaderProtectedState =
    /// The standard F# Array module, lifted into the ReaderProtectedState monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Array =
        /// A specialization of Array.iter which threads an accumulator through the computation
        /// and which also short-circuits the computation if the mapping function returns an
        /// error when any element is applied to it.
        [<CompiledName("Iterate")>]
        let iter (action : 'T -> 'Env -> 'State -> Choice<unit * 'State, 'Error>)
                (array : 'T[]) (env : 'Env) (state : 'State) : Choice<unit * 'State, 'Error> =
            // Preconditions
            checkNonNull "array" array

            let action = FSharpFunc<_,_,_,_>.Adapt action
            let len = array.Length

            let mutable index = 0
            let mutable state = state
            let mutable error = None

            while index < len && Option.isNone error do
                match action.Invoke (array.[index], env, state) with
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

        /// A specialization of Array.iteri which threads an accumulator through the computation
        /// and which also short-circuits the computation if the mapping function returns an
        /// error when any element is applied to it.
        [<CompiledName("IterateIndexed")>]
        let iteri (action : int -> 'T -> 'Env -> 'State -> Choice<unit * 'State, 'Error>)
                (array : 'T[]) (env : 'Env) (state : 'State) : Choice<unit * 'State, 'Error> =
            // Preconditions
            checkNonNull "array" array

            let action = FSharpFunc<_,_,_,_,_>.Adapt action
            let len = array.Length

            let mutable index = 0
            let mutable state = state
            let mutable error = None

            while index < len && Option.isNone error do
                match action.Invoke (index, array.[index], env, state) with
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

        /// A specialization of Array.map which threads an accumulator through the computation
        /// and which also short-circuits the computation if the mapping function returns an
        /// error when any element is applied to it.
        [<CompiledName("Map")>]
        let map (mapping : 'T -> 'Env -> 'State -> Choice<'U * 'State, 'Error>)
                (array : 'T[]) (env : 'Env) (state : 'State) : Choice<'U[] * 'State, 'Error> =
            // Preconditions
            checkNonNull "array" array

            let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
            let len = array.Length
            let results = Array.zeroCreate len

            let mutable index = 0
            let mutable state = state
            let mutable error = None

            while index < len && Option.isNone error do
                match mapping.Invoke (array.[index], env, state) with
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

        /// A specialization of Array.mapi which threads an accumulator through the computation
        /// and which also short-circuits the computation if the mapping function returns an
        /// error when any element is applied to it.
        [<CompiledName("MapIndexed")>]
        let mapi (mapping : int -> 'T -> 'Env -> 'State -> Choice<'U * 'State, 'Error>)
                (array : 'T[]) (env : 'Env) (state : 'State) : Choice<'U[] * 'State, 'Error> =
            // Preconditions
            checkNonNull "array" array

            let mapping = FSharpFunc<_,_,_,_,_>.Adapt mapping
            let len = array.Length
            let results = Array.zeroCreate len

            let mutable index = 0
            let mutable state = state
            let mutable error = None

            while index < len && Option.isNone error do
                match mapping.Invoke (index, array.[index], env, state) with
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


    /// The standard F# List module, lifted into the ReaderProtectedState monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module List =
        /// A specialization of List.map which threads an accumulator through the computation
        /// and which also short-circuits the computation if the mapping function returns an
        /// error when any element is applied to it.
        [<CompiledName("Map")>]
        let map (mapping : 'T -> 'Env -> 'State -> Choice<'U * 'State, 'Error>)
                (list : 'T list) (env : 'Env) (state : 'State) : Choice<'U list * 'State, 'Error> =
            // Preconditions
            checkNonNull "list" list

            let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
            
            let rec mapRec (results, state, lst) =
                match lst with
                | [] ->
                    let results = List.rev results
                    Choice1Of2 (results, state)
                | hd :: tl ->
                    // Apply the function to the head of the list.
                    // If the result is an error, return it;
                    // otherwise, continue processing recursively.
                    match mapping.Invoke (hd, env, state) with
                    | Choice2Of2 error ->
                        Choice2Of2 error
                    | Choice1Of2 (result, state) ->
                        mapRec (result :: results, state, tl)

            // Call the recursive implementation function.
            mapRec ([], state, list)


    /// The ExtCore.Collections.ArraySegment module, lifted into the ReaderProtectedState monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ArraySegment =
        /// A specialization of ArraySegment.iter which threads an accumulator through the
        /// computation and which also short-circuits the computation if the mapping function
        /// returns an error when any element is applied to it.
        [<CompiledName("Iterate")>]
        let iter (action : 'T -> 'Env -> 'State -> Choice<unit * 'State, 'Error>)
                (segment : System.ArraySegment<'T>) (env : 'Env) (state : 'State)
                : Choice<unit * 'State, 'Error> =
            let action = FSharpFunc<_,_,_,_>.Adapt action

            let array = segment.Array
            let endExclusive = segment.Offset + segment.Count

            let mutable index = segment.Offset
            let mutable state = state
            let mutable error = None

            while index < endExclusive && Option.isNone error do
                match action.Invoke (array.[index], env, state) with
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

        /// A specialization of ArraySegment.iteri which threads an accumulator through the
        /// computation and which also short-circuits the computation if the mapping function
        /// returns an error when any element is applied to it.
        [<CompiledName("IterateIndexed")>]
        let iteri (action : int -> 'T -> 'Env -> 'State -> Choice<unit * 'State, 'Error>)
                (segment : System.ArraySegment<'T>) (env : 'Env) (state : 'State)
                : Choice<unit * 'State, 'Error> =
            let action = FSharpFunc<_,_,_,_,_>.Adapt action

            let array = segment.Array
            let endExclusive = segment.Offset + segment.Count

            let mutable index = segment.Offset
            let mutable state = state
            let mutable error = None

            while index < endExclusive && Option.isNone error do
                match action.Invoke (index, array.[index], env, state) with
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

        /// A specialization of ArraySegment.map which threads an accumulator through the
        /// computation and which also short-circuits the computation if the mapping function
        /// returns an error when any element is applied to it.
        [<CompiledName("Map")>]
        let map (mapping : 'T -> 'Env -> 'State -> Choice<'U * 'State, 'Error>)
                (segment : System.ArraySegment<'T>) (env : 'Env) (state : 'State)
                : Choice<'U[] * 'State, 'Error> =
            let mapping = FSharpFunc<_,_,_,_>.Adapt mapping

            let array = segment.Array
            let endExclusive = segment.Offset + segment.Count
            /// Holds the mapped results.
            let results = Array.zeroCreate segment.Count

            let mutable index = segment.Offset
            let mutable state = state
            let mutable error = None

            while index < endExclusive && Option.isNone error do
                match mapping.Invoke (array.[index], env, state) with
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

        /// A specialization of ArraySegment.mapi which threads an accumulator through the
        /// computation and which also short-circuits the computation if the mapping function
        /// returns an error when any element is applied to it.
        [<CompiledName("MapIndexed")>]
        let mapi (mapping : int -> 'T -> 'Env -> 'State -> Choice<'U * 'State, 'Error>)
                (segment : System.ArraySegment<'T>) (env : 'Env) (state : 'State)
                : Choice<'U[] * 'State, 'Error> =
            let mapping = FSharpFunc<_,_,_,_,_>.Adapt mapping

            let array = segment.Array
            let endExclusive = segment.Offset + segment.Count
            /// Holds the mapped results.
            let results = Array.zeroCreate segment.Count

            let mutable index = segment.Offset
            let mutable state = state
            let mutable error = None

            while index < endExclusive && Option.isNone error do
                match mapping.Invoke (index, array.[index], env, state) with
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
module StatefulChoice =
    /// The standard F# Array module, lifted into the StatefulChoice monad.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Array =
        /// A specialization of Array.map which threads an accumulator through the computation and which also
        /// short-circuits the computation if the mapping function returns an error when any element is applied to it.
        [<CompiledName("Map")>]
        let map (mapping : 'T -> 'State -> Choice<'U, 'Error> * 'State)
                (array : 'T[]) (state : 'State) : Choice<'U[], 'Error> * 'State =
            // Preconditions
            checkNonNull "array" array

            let mapping = FSharpFunc<_,_,_>.Adapt mapping
            let len = Array.length array
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


//
[<RequireQualifiedAccess>]
module Cps =
    //
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Cont =
        /// The standard F# Array module, lifted into the Cps.Cont monad.
        [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Array =
            //
            [<CompiledName("Fold")>]
            let fold (folder : 'State -> 'T -> ('State -> 'K) -> 'K)
                    (state : 'State) (array : 'T[]) (cont : 'State -> 'K) : 'K =
                // Preconditions
                checkNonNull "array" array

                // OPTIMIZATION : If the array is empty return immediately.
                if Array.isEmpty array then
                    cont state
                else
                    /// The number of array elements.
                    let len = Array.length array

                    let folder = FSharpFunc<_,_,_,_>.Adapt folder

                    /// Iterates backwards over the array elements, creating a chain of continuations
                    /// which'll process them in order (from left-to-right) when executed.
                    let rec buildCont idx cont =
                        // The first element needs to be handled specially.
                        if idx = 0 then
                            // Pass the initial state to the mapping function when processing the first element.
                            folder.Invoke (state, array.[0], cont)

                        else
                            // Pass a continuation which'll be called once the previous element
                            // (at index = (argIdx - 1)) is mapped and stored in the results array.
                            buildCont (idx - 1) <| fun (state : 'State) ->
                                // Call the continuation to process the next element.
                                folder.Invoke (state, array.[idx], cont)

                    // Create and return a continuation which performs a CPS-style
                    // fold over the array elements when called.
                    buildCont (len - 1) cont


    //
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module State =
        /// The standard F# Array module, lifted into the Cps.State monad.
        [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Array =
            //
            [<CompiledName("Map")>]
            let map (mapping : 'T -> 'State -> ('U * 'State -> 'K) -> 'K)
                    (array : 'T[]) (state : 'State) (cont : 'U[] * 'State -> 'K) : 'K =
                // Preconditions
                checkNonNull "array" array

                // OPTIMIZATION : If the array is empty return immediately.
                if Array.isEmpty array then
                    cont (Array.empty, state)
                else
                    /// The number of array elements.
                    let len = Array.length array

                    /// The mapped elements.
                    let results = Array.zeroCreate len

                    let mapping = FSharpFunc<_,_,_,_>.Adapt mapping

                    /// Iterates backwards over the array elements, creating a chain of continuations
                    /// which'll process them in order (from left-to-right) when executed.
                    let rec buildCont idx cont =
                        // The first element needs to be handled specially.
                        if idx = 0 then
                            // Pass the initial state to the mapping function when processing the first element.
                            mapping.Invoke (array.[0], state, cont)

                        else
                            // Pass a continuation which'll be called once the previous element
                            // (at index = (argIdx - 1)) is mapped and stored in the results array.
                            buildCont (idx - 1) <| fun (prevElementResult, state : 'State) ->
                                // Save the _previous_ element's accumulator into the array
                                results.[idx - 1] <- prevElementResult

                                // Call the continuation to process the next element.
                                mapping.Invoke (array.[idx], state, cont)

                    // Create and return a continuation will map the array elements when called.
                    // The function passed to 'buildCont' here is used to store the mapped
                    // last element of the array, then call the original continuation with the results.
                    buildCont (len - 1) <| fun (prevElementResult : 'U, state : 'State) ->
                        // Save the last argument's accumulator value.
                        results.[len - 1] <- prevElementResult

                        // Call the continuation with the mapped elements and the final state value.
                        cont (results, state)

            //
            [<CompiledName("MapIndexed")>]
            let mapi (mapping : int -> 'T -> 'State -> ('U * 'State -> 'K) -> 'K)
                    (array : 'T[]) (state : 'State) (cont : 'U[] * 'State -> 'K) : 'K =
                // Preconditions
                checkNonNull "array" array

                // OPTIMIZATION : If the array is empty return immediately.
                if Array.isEmpty array then
                    cont (Array.empty, state)
                else
                    /// The number of array elements.
                    let len = Array.length array

                    /// The mapped elements.
                    let results = Array.zeroCreate len

                    let mapping = FSharpFunc<_,_,_,_,_>.Adapt mapping

                    /// Iterates backwards over the array elements, creating a chain of continuations
                    /// which'll process them in order (from left-to-right) when executed.
                    let rec buildCont idx cont =
                        // The first element needs to be handled specially.
                        if idx = 0 then
                            // Pass the initial state to the mapping function when processing the first element.
                            mapping.Invoke (idx, array.[0], state, cont)

                        else
                            // Pass a continuation which'll be called once the previous element
                            // (at index = (argIdx - 1)) is mapped and stored in the results array.
                            buildCont (idx - 1) <| fun (prevElementResult, state : 'State) ->
                                // Save the _previous_ element's accumulator into the array
                                results.[idx - 1] <- prevElementResult

                                // Call the continuation to process the next element.
                                mapping.Invoke (idx, array.[idx], state, cont)

                    // Create and return a continuation will map the array elements when called.
                    // The function passed to 'buildCont' here is used to store the mapped
                    // last element of the array, then call the original continuation with the results.
                    buildCont (len - 1) <| fun (prevElementResult : 'U, state : 'State) ->
                        // Save the last argument's accumulator value.
                        results.[len - 1] <- prevElementResult

                        // Call the continuation with the mapped elements and the final state value.
                        cont (results, state)

