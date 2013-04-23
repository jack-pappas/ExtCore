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
module ExtCore.Control.Collections.Reader
    
open Microsoft.FSharp.Control
open OptimizedClosures
open ExtCore
open ExtCore.Collections


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