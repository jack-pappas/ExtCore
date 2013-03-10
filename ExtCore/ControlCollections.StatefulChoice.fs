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
module ExtCore.Control.Collections.StatefulChoice
    
open Microsoft.FSharp.Control
open OptimizedClosures
open ExtCore
open ExtCore.Collections


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

