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

/// Additional functional operators on maps.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExtCore.Collections.Map

open System.Collections.Generic
open LanguagePrimitives
open OptimizedClosures
open ExtCore


/// Determines the number of items in the map.
[<CompiledName("Count")>]
let inline count (map : Map<'Key, 'Value>) =
    map.Count

/// Lookup a key in the map, and if found, return it's corresponding value.
/// Otherwise, returns the given default value.
[<CompiledName("FindOrDefault")>]
let inline findOrDefault defaultValue key (map : Map<'Key, 'T>) =
    defaultArg (Map.tryFind key map) defaultValue

/// The Map containing the given binding.
[<CompiledName("Singleton")>]
let inline singleton key value : Map<'Key, 'T> =
    Map.empty
    |> Map.add key value

/// Creates a map with the same elements as the vector.
[<CompiledName("OfVector")>]
let ofVector (vector : vector<'Key * 'T>) : Map<'Key, 'T> =
    // Preconditions
    // TODO : Check that the vector is not equivalent to null (i.e., Unchecked.defaultOf<vector<_>>).

    (Map.empty, vector)
    ||> Vector.fold (fun map (k, v) ->
        Map.add k v map)

/// Builds a vector that contains the elements of the map in order.
[<CompiledName("ToVector")>]
let toVector (map : Map<'Key, 'T>) : vector<'Key * 'T> =
    // Preconditions
    checkNonNull "map" map

    vector.UnsafeCreate <| Map.toArray map

/// Returns the key set of the Map.
[<CompiledName("Keys")>]
let keys (map : Map<'Key, 'T>) =
    // Preconditions
    checkNonNull "map" map

    (Set.empty, map)
    ||> Map.fold (fun keys key _ ->
        Set.add key keys)

/// Returns the value set of the Map.
// TODO : Should this be changed to return a Multiset?
[<CompiledName("Values")>]
let values (map : Map<'Key, 'T>) =
    // Preconditions
    checkNonNull "map" map

    (Set.empty, map)
    ||> Map.fold (fun values _ value ->
        Set.add value values)

/// Adds the given KeyValuePair to the map.
[<CompiledName("AddKvp")>]
let inline addKvp (kvp : KeyValuePair<'Key, 'T>) map =
    // Preconditions
    checkNonNull "map" map

    Map.add kvp.Key kvp.Value map

/// Returns a new map created from a sequence of key-value pairs.
[<CompiledName("FromKvpSequence")>]
let ofKvpSeq (sequence : seq<KeyValuePair<'Key, 'T>>) =
    // Preconditions
    checkNonNull "sequence" sequence

    (Map.empty, sequence)
    ||> Seq.fold (fun map kvp ->
        addKvp kvp map)

/// Creates a new map from a set of keys by applying a mapping function to each
/// key to generate it's corresponding value.
[<CompiledName("FromKeys")>]
let ofKeys (mapping : 'Key -> 'T) (set : Set<'Key>) : Map<'Key, 'T> =
    // Preconditions
    checkNonNull "set" set

    // OPTIMIZATION : If the set is empty return immediately.
    if Set.isEmpty set then
        Map.empty
    else
        (Map.empty, set)
        ||> Set.fold (fun map key ->
            Map.add key (mapping key) map)

/// Creates a new map from a set of values by applying a mapping function to each
/// value to extract a key from it. If the key-mapping function returns the same
/// key for two or more values, the returned map will only contain a binding for
/// the greatest of those values.
[<CompiledName("FromValues")>]
let ofValues (mapping : 'T -> 'Key) (set : Set<'T>) : Map<'Key, 'T> =
    // Preconditions
    checkNonNull "set" set

    // OPTIMIZATION : If the set is empty return immediately.
    if Set.isEmpty set then
        Map.empty
    else
        (Map.empty, set)
        ||> Set.fold (fun map value ->
            Map.add (mapping value) value map)

/// Builds a new map containing only the bindings whose keys do not
/// belong to a given set of keys.
[<CompiledName("RemoveKeys")>]
let removeKeys keys (map : Map<'Key, 'T>) =
    // Preconditions
    checkNonNull "keys" keys
    checkNonNull "map" map

    // OPTIMIZATION : If either the key set or input map is empty return immediately.
    if Set.isEmpty keys then
        map
    elif Map.isEmpty map then
        Map.empty
    else
        (map, keys)
        ||> Set.fold (fun map key ->
            Map.remove key map)

/// Builds a new map containing only the bindings whose keys
/// belong to a given set of keys.
[<CompiledName("FilterKeys")>]
let filterKeys keys (map : Map<'Key, 'T>) =
    // Preconditions
    checkNonNull "keys" keys
    checkNonNull "map" map

    // OPTIMIZATION : If either the key set or input map is empty return immediately.
    if Set.isEmpty keys || Map.isEmpty map then
        Map.empty
    else
        map
        |> Map.filter (fun key _ ->
            Set.contains key keys)

/// Applies a function to each binding in the map, in decreasing key order.
[<CompiledName("IterBack")>]
let iterBack (action : 'Key -> 'T -> unit) (map : Map<'Key, 'T>) : unit =
    // Preconditions
    checkNonNull "map" map

    // WORKAROUND : We don't have access to the internals of Map, so this is really
    // the only feasible, reasonably performant way of implementing this function.
    (map, ())
    ||> Map.foldBack (fun key value () ->
        action key value)

/// <summary>
/// Applies the given function to each binding in the map.
/// Returns the map comprised of the results "x" for each binding
/// where the function returns <c>Some(x)</c>.
/// </summary>
[<CompiledName("Choose")>]
let choose (chooser : 'Key -> 'T -> 'U option) map =
    // Preconditions
    checkNonNull "map" map

    // OPTIMIZATION : If the input map is empty return immediately.
    if Map.isEmpty map then
        Map.empty
    else
        let chooser = FSharpFunc<_,_,_>.Adapt chooser

        (Map.empty, map)
        ||> Map.fold (fun chosenMap key value ->
            match chooser.Invoke (key, value) with
            | None ->
                chosenMap
            | Some newValue ->
                Map.add key newValue chosenMap)

/// <summary>
/// Like <c>Map.partition</c>, a function is applied to each binding in a map to
/// partition it into two parts. <c>mapPartition</c> differs in that it allows
/// the values in the returned maps to be different than the values in the input
/// map, and it also allows the values in the returned maps to have different types.
/// </summary>
[<CompiledName("MapPartition")>]
let mapPartition (partitioner : 'Key -> 'T -> Choice<'U, 'V>) map =
    // Preconditions
    checkNonNull "map" map

    // OPTIMIZATION : If the map is empty return immediately.
    if Map.isEmpty map then
        Map.empty, Map.empty
    else
        let partitioner = FSharpFunc<_,_,_>.Adapt partitioner

        let mutable resultMap1 = Map.empty
        let mutable resultMap2 = Map.empty

        for kvp in map do
            match partitioner.Invoke (kvp.Key, kvp.Value) with
            | Choice1Of2 value ->
                resultMap1 <- Map.add kvp.Key value resultMap1
            | Choice2Of2 value ->
                resultMap2 <- Map.add kvp.Key value resultMap2

        resultMap1, resultMap2

/// Combines two maps into a single map.
/// Whenever a key exists in both maps, the first map's entry will be added to the result map.
[<CompiledName("Union")>]
let union (map1 : Map<'Key, 'T>) (map2 : Map<'Key, 'T>) : Map<'Key, 'T> =
    // Preconditions
    checkNonNull "map1" map1
    checkNonNull "map2" map2

    match map1.Count, map2.Count with
    // Optimize for empty inputs
    | 0, 0 ->
        Map.empty
    | 0, _ ->
        map2
    | _, 0 ->
        map1
    | _, _ ->
        // Start with the second map.
        // Fold over the first map, adding it's entries to the second
        // and overwriting any existing entries.
        (map2, map1)
        ||> Map.fold (fun combinedMap key value ->
            Map.add key value combinedMap)

/// Combines two maps into a single map.
/// Whenever a key exists in both maps, the specified function is used to determine the
/// value to be used for that key in the combined map.
[<CompiledName("Join")>]
let join (joiner : 'Key -> 'T -> 'T -> 'T) (map1 : Map<'Key, 'T>) (map2 : Map<'Key, 'T>) : Map<'Key, 'T> =
    // Preconditions
    checkNonNull "map1" map1
    checkNonNull "map2" map2

    match map1.Count, map2.Count with
    // Optimize for empty inputs
    | 0, 0 ->
        Map.empty
    | 0, _ ->
        map2
    | _, 0 ->
        map1
    | _, _ ->
        let joiner = FSharpFunc<_,_,_,_>.Adapt joiner

        // Partition the second map into two maps -- one containing the entries which conflict
        // with the first map, and another whose entries don't exist in the first map.
        let conflicting, unique =
            map2
            |> Map.partition (fun key _ ->
                Map.containsKey key map1)

        // Add the unique entries from the second map to the first map.
        let joined =
            (map1, unique)
            ||> Map.fold (fun joined key value ->
                Map.add key value joined)

        // Now, add the conflicting entries into the joined map, using the joiner function
        // to determine which value should be used for the conflicting key.
        (joined, conflicting)
        ||> Map.fold (fun joined key value2 ->
            /// The first map's value for this conflicting key.
            let value1 = Map.find key map1

            /// The joined value for this key.
            let joinedValue = joiner.Invoke (key, value1, value2)

            // Add the joined value to the map.
            Map.add key joinedValue joined)

/// Creates a new Map by inverting the given Map. The keys are the values of the original
/// Map; the value associated with each key (original value) is the greatest-valued original
/// key associated with the original value.
[<CompiledName("Inverse")>]
let inverse (map : Map<'Key, 'T>) : Map<'T, 'Key> =
    // Preconditions
    checkNonNull "map" map

    // OPTIMIZATION : If the input map is empty return immediately.
    if Map.isEmpty map then
        Map.empty
    else
        (Map.empty, map)
        ||> Map.fold (fun inverseMap key value ->
            Map.add value key inverseMap)

/// Creates a new Map by inverting the given Map. The keys are the values of the
/// original Map; the corresponding value is a non-empty set containing the original keys
/// which pointed to the value.
[<CompiledName("Pivot")>]
let pivot (map : Map<'Key, 'T>) : Map<'T, Set<'Key>> =
    // Preconditions
    checkNonNull "map" map

    // OPTIMIZATION : If the input map is empty return immediately.
    if Map.isEmpty map then
        Map.empty
    else
        (Map.empty, map)
        ||> Map.fold (fun pivotMap key value ->
            /// The key set for this value.
            let keySet =
                match Map.tryFind value pivotMap with
                | Some keySet ->
                    Set.add key keySet
                | None ->
                    Set.singleton key

            // Add/update the pivot map entry for this value.
            Map.add value keySet pivotMap)

/// Combines Map.ofKeys and Map.pivot to avoid creating intermediate data structures.
[<CompiledName("PivotWith")>]
let pivotWith (mapping : 'Key -> 'T) (set : Set<'Key>) : Map<'T, Set<'Key>> =
    // Preconditions
    checkNonNull "set" set

    // OPTIMIZATION : If the set is empty return immediately.
    if Set.isEmpty set then
        Map.empty
    else
        (Map.empty, set)
        ||> Set.fold (fun pivotMap key ->
            /// The value for this key.
            let value = mapping key

            /// The key set for this value.
            let keySet =
                match Map.tryFind value pivotMap with
                | Some keySet ->
                    Set.add key keySet
                | None ->
                    Set.singleton key

            // Add/update the key-set for this value in the pivot map.
            Map.add value keySet pivotMap)

/// Like Map.add, but doesn't overwrite an existing entry.
[<CompiledName("TryAdd")>]
let tryAdd key value (map : Map<'Key, 'T>) : Map<'Key, 'T> =
    // Preconditions
    checkNonNull "map" map

    if Map.containsKey key map then
        // Return the original map.
        map
    else
        // Add the new entry.
        Map.add key value map

/// Like Map.add, but only overwrites the value of an existing entry
/// (i.e., it won't create a new entry in the map).
[<CompiledName("TryUpdate")>]
let tryUpdate key value (map : Map<'Key, 'T>) : Map<'Key, 'T> =
    // Preconditions
    checkNonNull "map" map

    if Map.containsKey key map then
        // Overwrite the value of the existing entry.
        Map.add key value map
    else
        // Return the original map.
        map

/// Like Map.add, but only overwrites the value of an existing entry.
/// If the map does not contain the given key, KeyNotFoundException is raised.
[<CompiledName("Update")>]
let update key value (map : Map<'Key, 'T>) : Map<'Key, 'T> =
    // Preconditions
    checkNonNull "map" map

    // OPTIMIZE : This function could be faster if we had access to the
    // MapTree within the map instance (because then we'd only need to traverse
    // it once). Maybe this function can be added to the Map module in a future version of F#.
    if Map.containsKey key map then
        // Overwrite the value of the existing entry.
        Map.add key value map
    else
        keyNotFound "The map does not contain the specified key."

/// Returns the number of map elements matching a given predicate.
// Map.countWith predicate map = (Map.filter predicate map |> Map.count)
[<CompiledName("CountWith")>]
let countWith (predicate : 'Key -> 'T -> bool) (map : Map<'Key, 'T>) : int =
    // Preconditions
    checkNonNull "map" map

    // OPTIMIZATION : If the map is empty, return immediately.
    if Map.isEmpty map then 0
    else
        let predicate = FSharpFunc<_,_,_>.Adapt predicate

        // Fold over the map, counting the number of elements which match the predicate.
        (0, map)
        ||> Map.fold (fun matchCount key value ->
            if predicate.Invoke (key, value) then
                matchCount + 1
            else matchCount)

/// Applies the specified predicate function to each binding in a Map,
/// returning a new Set containing the keys for which the predicate matched.
[<CompiledName("FindKeys")>]
let findKeys (predicate : 'Key -> 'T -> bool) (map : Map<'Key, 'T>) : Set<'Key> =
    // Preconditions
    checkNonNull "map" map

    // OPTIMIZATION : If the map is empty, return immmediately.
    if Map.isEmpty map then Set.empty
    else
        let predicate = FSharpFunc<_,_,_>.Adapt predicate

        // Fold over the map, applying the predicate to each binding;
        // whenever the binding returns 'true', add the key to the result set.
        (Set.empty, map)
        ||> Map.fold (fun resultKeys key value ->
            if predicate.Invoke (key, value) then
                Set.add key resultKeys
            else resultKeys)
