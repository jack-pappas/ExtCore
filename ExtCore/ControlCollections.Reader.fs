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


/// The F# Set module, lifted into the Reader monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Set =
    //
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> 'Env -> unit) (set : Set<'T>) (env : 'Env) : unit =
        // Preconditions
        checkNonNull "set" set

        let action = FSharpFunc<_,_,_>.Adapt action

        set
        |> Set.iter (fun el ->
            action.Invoke (el, env))

    //
    [<CompiledName("IterateBack")>]
    let iterBack (action : 'T -> 'Env -> unit) (set : Set<'T>) (env : 'Env) : unit =
        // Preconditions
        checkNonNull "set" set

        let action = FSharpFunc<_,_,_>.Adapt action

        set
        |> Set.iterBack (fun el ->
            action.Invoke (el, env))

    //
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'Env -> 'U) (set : Set<'T>) (env : 'Env) : Set<'U> =
        // Preconditions
        checkNonNull "set" set

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        (Set.empty, set)
        ||> Set.fold (fun mappedSet el ->
            // Apply the mapping to the current element and state value.
            let mappedEl = mapping.Invoke (el, env)

            // Add the mapped element to the mapped set and continue iterating.
            Set.add mappedEl mappedSet)

    //
    [<CompiledName("MapBack")>]
    let mapBack (mapping : 'T -> 'Env -> 'U) (set : Set<'T>) (env : 'Env) : Set<'U> =
        // Preconditions
        checkNonNull "set" set

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        (set, Set.empty)
        ||> Set.foldBack (fun el mappedSet ->
            // Apply the mapping to the current element and state value.
            let mappedEl = mapping.Invoke (el, env)

            // Add the mapped element to the mapped set and continue iterating.
            Set.add mappedEl mappedSet)

    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> 'Env -> 'State)
        (state : 'State) (set : Set<'T>) (env : 'Env) : 'State =
        // Preconditions
        checkNonNull "set" set

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        (state, set)
        ||> Set.fold (fun state el ->
            // Apply the folder to the current element and state values.
            folder.Invoke (state, el, env))

    //
    [<CompiledName("FoldBack")>]
    let foldBack (folder : 'T -> 'State -> 'Env -> 'State) (set : Set<'T>)
        (state : 'State) (env : 'Env) : 'State =
        // Preconditions
        checkNonNull "set" set

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        (set, state)
        ||> Set.foldBack (fun el state ->
            // Apply the folder to the current element and state values.
            folder.Invoke (el, state, env))


/// The IntSet module, lifted into the Reader monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IntSet =
    //
    [<CompiledName("Iterate")>]
    let iter (action : int -> 'Env -> unit) (set : IntSet) (env : 'Env) : unit =
        // Preconditions
        checkNonNull "set" set

        let action = FSharpFunc<_,_,_>.Adapt action

        set
        |> IntSet.iter (fun el ->
            action.Invoke (el, env))

    //
    [<CompiledName("IterateBack")>]
    let iterBack (action : int -> 'Env -> unit) (set : IntSet) (env : 'Env) : unit =
        // Preconditions
        checkNonNull "set" set

        let action = FSharpFunc<_,_,_>.Adapt action

        set
        |> IntSet.iterBack (fun el ->
            action.Invoke (el, env))

    //
    [<CompiledName("Map")>]
    let map (mapping : int -> 'Env -> int) (set : IntSet) (env : 'Env) : IntSet =
        // Preconditions
        checkNonNull "set" set

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        (IntSet.empty, set)
        ||> IntSet.fold (fun mappedSet el ->
            // Apply the mapping to the current element and state value.
            let mappedEl = mapping.Invoke (el, env)

            // Add the mapped element to the mapped set and continue iterating.
            IntSet.add mappedEl mappedSet)

    //
    [<CompiledName("MapBack")>]
    let mapBack (mapping : int -> 'Env -> int) (set : IntSet) (env : 'Env) : IntSet =
        // Preconditions
        checkNonNull "set" set

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        (set, IntSet.empty)
        ||> IntSet.foldBack (fun el mappedSet ->
            // Apply the mapping to the current element and state value.
            let mappedEl = mapping.Invoke (el, env)

            // Add the mapped element to the mapped set and continue iterating.
            IntSet.add mappedEl mappedSet)

    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> int -> 'Env -> 'State)
        (state : 'State) (set : IntSet) (env : 'Env) : 'State =
        // Preconditions
        checkNonNull "set" set

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        (state, set)
        ||> IntSet.fold (fun state el ->
            // Apply the folder to the current element and state values.
            folder.Invoke (state, el, env))

    //
    [<CompiledName("FoldBack")>]
    let foldBack (folder : int -> 'State -> 'Env -> 'State) (set : IntSet)
        (state : 'State) (env : 'Env) : 'State =
        // Preconditions
        checkNonNull "set" set

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        (set, state)
        ||> IntSet.foldBack (fun el state ->
            // Apply the folder to the current element and state values.
            folder.Invoke (el, state, env))


#if PROTO_COMPILER

/// The TagSet module, lifted into the Reader monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TagSet =
    //
    [<CompiledName("Iterate")>]
    let iter (action : int<'Tag> -> 'Env -> unit) (set : TagSet<'Tag>) (env : 'Env) : unit =
        // Preconditions
        checkNonNull "set" set

        let action = FSharpFunc<_,_,_>.Adapt action

        set
        |> TagSet.iter (fun el ->
            action.Invoke (el, env))

    //
    [<CompiledName("IterateBack")>]
    let iterBack (action : int<'Tag> -> 'Env -> unit) (set : TagSet<'Tag>) (env : 'Env) : unit =
        // Preconditions
        checkNonNull "set" set

        let action = FSharpFunc<_,_,_>.Adapt action

        set
        |> TagSet.iterBack (fun el ->
            action.Invoke (el, env))

    //
    [<CompiledName("Map")>]
    let map (mapping : int<'Tag1> -> 'Env -> int<'Tag2>) (set : TagSet<'Tag1>) (env : 'Env) : TagSet<'Tag2> =
        // Preconditions
        checkNonNull "set" set

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        (TagSet.empty, set)
        ||> TagSet.fold (fun mappedSet el ->
            // Apply the mapping to the current element and state value.
            let mappedEl = mapping.Invoke (el, env)

            // Add the mapped element to the mapped set and continue iterating.
            TagSet.add mappedEl mappedSet)

    //
    [<CompiledName("MapBack")>]
    let mapBack (mapping : int<'Tag1> -> 'Env -> int<'Tag2>) (set : TagSet<'Tag1>) (env : 'Env) : TagSet<'Tag2> =
        // Preconditions
        checkNonNull "set" set

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        (set, TagSet.empty)
        ||> TagSet.foldBack (fun el mappedSet ->
            // Apply the mapping to the current element and state value.
            let mappedEl = mapping.Invoke (el, env)

            // Add the mapped element to the mapped set and continue iterating.
            TagSet.add mappedEl mappedSet)

    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> int<'Tag> -> 'Env -> 'State)
        (state : 'State) (set : TagSet<'Tag>) (env : 'Env) : 'State =
        // Preconditions
        checkNonNull "set" set

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        (state, set)
        ||> TagSet.fold (fun state el ->
            // Apply the folder to the current element and state values.
            folder.Invoke (state, el, env))

    //
    [<CompiledName("FoldBack")>]
    let foldBack (folder : int<'Tag> -> 'State -> 'Env -> 'State) (set : TagSet<'Tag>)
        (state : 'State) (env : 'Env) : 'State =
        // Preconditions
        checkNonNull "set" set

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        (set, state)
        ||> TagSet.foldBack (fun el state ->
            // Apply the folder to the current element and state values.
            folder.Invoke (el, state, env))

#endif

/// The HashSet module, lifted into the Reader monad.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HashSet =
    //
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> 'Env -> unit) (set : HashSet<'T>) (env : 'Env) : unit =
        // Preconditions
        checkNonNull "set" set

        let action = FSharpFunc<_,_,_>.Adapt action

        set
        |> HashSet.iter (fun el ->
            action.Invoke (el, env))

    //
    [<CompiledName("IterateBack")>]
    let iterBack (action : 'T -> 'Env -> unit) (set : HashSet<'T>) (env : 'Env) : unit =
        // Preconditions
        checkNonNull "set" set

        let action = FSharpFunc<_,_,_>.Adapt action

        set
        |> HashSet.iterBack (fun el ->
            action.Invoke (el, env))

    //
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'Env -> 'U) (set : HashSet<'T>) (env : 'Env) : HashSet<'U> =
        // Preconditions
        checkNonNull "set" set

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        (HashSet.empty, set)
        ||> HashSet.fold (fun mappedSet el ->
            // Apply the mapping to the current element and state value.
            let mappedEl = mapping.Invoke (el, env)

            // Add the mapped element to the mapped set and continue iterating.
            HashSet.add mappedEl mappedSet)

    //
    [<CompiledName("MapBack")>]
    let mapBack (mapping : 'T -> 'Env -> 'U) (set : HashSet<'T>) (env : 'Env) : HashSet<'U> =
        // Preconditions
        checkNonNull "set" set

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        (set, HashSet.empty)
        ||> HashSet.foldBack (fun el mappedSet ->
            // Apply the mapping to the current element and state value.
            let mappedEl = mapping.Invoke (el, env)

            // Add the mapped element to the mapped set and continue iterating.
            HashSet.add mappedEl mappedSet)

    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> 'Env -> 'State)
        (state : 'State) (set : HashSet<'T>) (env : 'Env) : 'State =
        // Preconditions
        checkNonNull "set" set

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        (state, set)
        ||> HashSet.fold (fun state el ->
            // Apply the folder to the current element and state values.
            folder.Invoke (state, el, env))

    //
    [<CompiledName("FoldBack")>]
    let foldBack (folder : 'T -> 'State -> 'Env -> 'State) (set : HashSet<'T>)
        (state : 'State) (env : 'Env) : 'State =
        // Preconditions
        checkNonNull "set" set

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        (set, state)
        ||> HashSet.foldBack (fun el state ->
            // Apply the folder to the current element and state values.
            folder.Invoke (el, state, env))

