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
module ExtCore.Control.Collections.Cps
    
open Microsoft.FSharp.Control
open OptimizedClosures
open ExtCore
open ExtCore.Collections


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
module StateCont =
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

