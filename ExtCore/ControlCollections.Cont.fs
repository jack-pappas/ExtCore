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
module ExtCore.Control.Collections.Cps.Cont
    
open Microsoft.FSharp.Control
open OptimizedClosures
open ExtCore
open ExtCore.Collections
open ExtCore.Control.Cps


/// The standard F# Array module, adapted for use within 'cont' workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array =
    /// Cont implementation of Array.fold.
    let rec private foldImpl (folder : FSharpFunc<_,_,_>, array : 'T[], state : 'State, currentIndex) : ContFunc<'State, 'K> =
        cont {
        if currentIndex >= array.Length then
            // We've reached the end of the array so return the final state value.
            return state
        else
            // Invoke the folder with the current array element and state value.
            let! state = folder.Invoke (state, array.[currentIndex])

            // Continue folding over the remaining array elements.
            return! foldImpl (folder, array, state, currentIndex + 1)
        }

    /// Cont implementation of Array.fold.
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> ContFunc<'State, 'K>) (state : 'State) (array : 'T[]) : ContFunc<'State, 'K> =
        // Preconditions
        checkNonNull "array" array

        // Call the recursive implementation.
        let folder = FSharpFunc<_,_,_>.Adapt folder
        foldImpl (folder, array, state, 0)

