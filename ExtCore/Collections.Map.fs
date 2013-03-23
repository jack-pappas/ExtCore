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
    

/// Determines the number of items in the Map.
[<CompiledName("Count")>]
let inline count (map : Map<'Key, 'Value>) =
    map.Count

//
[<CompiledName("FindOrDefault")>]
let inline findOrDefault defaultValue key (map : Map<'Key, 'T>) =
    defaultArg (Map.tryFind key map) defaultValue

/// The Map containing the given binding.
[<CompiledName("Singleton")>]
let inline singleton key value : Map<'Key, 'T> =
    Map.empty
    |> Map.add key value

//
[<CompiledName("Keys")>]
let keys (map : Map<'Key, 'T>) =
    // Preconditions
    checkNonNull "map" map

    (Set.empty, map)
    ||> Map.fold (fun keys key _ ->
        Set.add key keys)

//
[<CompiledName("Values")>]
let values (map : Map<'Key, 'T>) =
    // Preconditions
    checkNonNull "map" map

    (Set.empty, map)
    ||> Map.fold (fun values _ value ->
        Set.add value values)

//
[<CompiledName("AddKvp")>]
let inline addKvp (kvp : KeyValuePair<'Key, 'T>) map =
    // Preconditions
    checkNonNull "map" map

    Map.add kvp.Key kvp.Value map

//
[<CompiledName("FromKvpSequence")>]
let ofKvpSeq (sequence : seq<KeyValuePair<'Key, 'T>>) =
    // Preconditions
    checkNonNull "sequence" sequence

    (Map.empty, sequence)
    ||> Seq.fold (fun map kvp ->
        addKvp kvp map)

//
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

//
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

//
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

//
[<CompiledName("SelectKeys")>]
let selectKeys keys (map : Map<'Key, 'T>) =
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

//
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

//
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

//
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

//
// Combines Map.ofKeys and Map.pivot to avoid creating intermediate data structures.
[<CompiledName("PivotKeySet")>]
let pivotKeySet (mapping : 'Key -> 'T) (set : Set<'Key>) : Map<'T, Set<'Key>> =
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

// Like Map.add, but doesn't overwrite an existing entry.
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

// Like Map.add, but only overwrites the value of an existing entry
// (i.e., it won't create a new entry in the map).
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

// TODO
// mapi, mapiBack
// foldi, foldiBack
// reduce, reduceBack
// update       // Similar to 'tryUpdate' but raises an exception if there's no existing entry.
