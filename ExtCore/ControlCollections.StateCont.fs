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
module ExtCore.Control.Collections.Cps.StateCont
    
open Microsoft.FSharp.Control
open OptimizedClosures
open ExtCore
open ExtCore.Collections
open ExtCore.Control.Cps


/// The standard F# Array module, adapted for use within 'stateCont' workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array =
    /// StateCont implementation of Array.map.
    let rec private mapImpl mapping (results : 'U[]) (array : 'T[]) currentIndex : StateContFunc<'State, 'U[], 'K> =
        stateCont {
        if currentIndex >= array.Length then
            // Return the mapped array and the final state value.
            return results
        else
            // Map the current array element, then store the result into the results array.
            let! result = mapping array.[currentIndex]
            results.[currentIndex] <- result

            // Map the remaining elements.
            return! mapImpl mapping results array (currentIndex + 1)
        }

    /// StateCont implementation of Array.map.
    [<CompiledName("Map")>]
    let map (mapping : 'T -> StateContFunc<'State, 'U, 'K>) (array : 'T[]) : StateContFunc<'State, 'U[], 'K> =
        // Preconditions
        checkNonNull "array" array

        let result = Array.zeroCreate <| Array.length array

        // Call the recursive implementation.
        mapImpl mapping result array 0

    /// StateCont implementation of Array.mapi.
    let rec private mapiImpl mapping (results : 'U[]) (array : 'T[]) currentIndex : StateContFunc<'State, 'U[], 'K> =
        stateCont {
        if currentIndex >= array.Length then
            // Return the mapped array and the final state value.
            return results
        else
            // Map the current array element, then store the result into the results array.
            let! result = mapping currentIndex array.[currentIndex]
            results.[currentIndex] <- result

            // Map the remaining elements.
            return! mapiImpl mapping results array (currentIndex + 1)
        }

    /// StateCont implementation of Array.mapi.
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> StateContFunc<'State, 'U, 'K>) (array : 'T[]) : StateContFunc<'State, 'U[], 'K> =
        // Preconditions
        checkNonNull "array" array

        let result = Array.zeroCreate <| Array.length array

        // Call the recursive implementation.
        mapiImpl mapping result array 0

