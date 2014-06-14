(*

Copyright 2014 Jack Pappas

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

namespace ExtCore.Collections

open ExtCore


/// Immutable multimaps. Elements are ordered by F# generic comparison.
type Multimap<'T, 'Key
    when 'T : comparison
    and 'Key : comparison> =
    Map<'T, Set<'Key>>

/// Functional programming operators related to the Multimap<_,_> type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Multimap =
    /// The empty multimap.
    [<CompiledName("Empty")>]
    let empty : Multimap<'T, 'Key> =
        Map.empty

    /// Is the multimap empty?
    [<CompiledName("IsEmpty")>]
    let isEmpty (set : Multimap<'T, 'Key>) =
        // Preconditions
        checkNonNull "set" set

        Map.isEmpty set

    //
    [<CompiledName("Singleton")>]
    let singleton key value : Multimap<'T, 'Key> =
        Map.singleton key (Set.singleton value)

    //
    [<CompiledName("Add")>]
    let add key value (map : Multimap<'T, 'Key>) : Multimap<'T, 'Key> =
        // Preconditions
        checkNonNull "map" map

        // If the key already has a value set associated with it, update the set with the value;
        // otherwise, add a new value set to the map for this key.
        match Map.tryFind key map with
        | None ->
            Map.add key (Set.singleton value) map
        | Some valueSet ->
            Map.add key (Set.add value valueSet) map

    //
    [<CompiledName("Remove")>]
    let remove key value (map : Multimap<'T, 'Key>) : Multimap<'T, 'Key> =
        // Preconditions
        checkNonNull "map" map

        match Map.tryFind key map with
        | None ->
            map
        | Some valueSet ->
            // Remove the value from the value set.
            let valueSet = Set.remove value valueSet

            // If the value set is now empty, remove this key's entry from the map.
            // Otherwise, update the entry with the updated value set.
            if Set.isEmpty valueSet then
                Map.remove key map
            else
                Map.add key valueSet map
