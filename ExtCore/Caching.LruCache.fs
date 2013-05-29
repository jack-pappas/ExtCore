(*

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

namespace ExtCore.Caching

open System.Collections.Generic
open LanguagePrimitives
open OptimizedClosures
open ExtCore
open ExtCore.Collections


(* OPTIMIZE :   In the HashMap used in the cache, change the KeyValuePair to a simple struct-tuple
                so we can flip the order the index and value are stored in; this'll provide better
                data alignment in most cases. (E.g., name the type ValueWithKeyIndex) *)
// TODO : Use TagMap instead of IntMap, and tag the key indices with a tag type like KeyIndex.

/// <summary>An immutable cache data structure with a Least-Recently-Used (LRU) eviction policy.</summary>
/// <typeparam name="Key">The type of key used by the cache.</typeparam>
/// <typeparam name="T">The type of the values stored in the cache.</typeparam>
[<Sealed>]
type LruCache<'Key, 'T when 'Key : comparison>
    private (cache : HashMap<'Key, KeyValuePair<int, 'T>>, indexedKeys : IntMap<'Key>,
             capacity : uint32, currentIndex : uint32) =
    /// The empty cache instance.
    static let empty = LruCache (HashMap.empty, IntMap.empty, 0u, 0u)

    /// The empty cache instance.
    static member internal Empty
        with get () = empty

(*
    /// Create an LruCache from a sequence of key-value pairs.
    new (capacity : uint32, elements : seq<'Key * 'T>) =
        // Preconditions
        checkNonNull "elements" elements

        // OPTIMIZATION : When the capacity is zero, we don't even need to consume the sequence.
        if capacity = 0u then
            // Unfortunately, we can't just return the empty cache instance,
            // we must actually call a constructor.
            LruCache (HashMap.empty, IntMap.empty, 0u, 0u)
        else
            // OPTIMIZE : Try to cast the sequence to array or list;
            // if it succeeds use the specialized method for that type for better performance.

            notImpl "LruCache::.ctor(uint32, seq<'Key, 'T>)"
            // TEMP : This is necessary because of the exception being raised (above).
            // It can be removed whenever this code path is implemented.
            LruCache (HashMap.empty, IntMap.empty, 0u, 0u)
*)

    /// Is the cache empty?
    member __.IsEmpty
        with get () =
            IntMap.isEmpty indexedKeys

    /// The maximum number of values which may be stored in the cache.
    member __.Capacity
        with get () = capacity

    /// The number of values stored in the cache.
    member __.Count
        with get () =
            IntMap.count indexedKeys

    /// Look up a key in the cache, returning Some with the associated value if
    /// if the key is in the domain of the cache and None if not. The (possibly)
    /// updated cache is also returned.
    member this.TryFind (key : 'Key) : 'T option * LruCache<'Key, 'T> =
        // Try extracting the key/value from the cache.
        let value, this' = this.Extract key

        // If the key was found (and the value extracted), re-insert the key/value
        // in the cache so the key's index will be updated.
        match value with
        | None ->
            None, this
        | Some keyIndexAndValue ->
            Some keyIndexAndValue.Value,
            this'.Add (key, keyIndexAndValue.Value)

    /// Tests if a key is in the domain of the cache.
    member __.ContainsKey (key : 'Key) : bool =
        HashMap.containsKey key cache

    //
    member this.Add (key : 'Key, value : 'T) : LruCache<'Key, 'T> =
        // OPTIMIZATION : If the capacity is zero (0), we don't need to do anything here.
        if capacity = 0u then this
        else
            // NOTE : It is very important that we use checked addition here with uint32 operands --
            // this data structure currently breaks if the index rolls over from 0xffffffff to 0x00000000.
            let newIndex = Checked.(+) currentIndex 1u
            let newCache = HashMap.add key (KeyValuePair (int newIndex, value)) cache

            let newIndexedKeys =
                // Update the key-index (if necessary, i.e., the key already exists in the cache).
                let newIndexedKeys =
                    match HashMap.tryFind key cache with
                    | None -> indexedKeys
                    | Some keyIndexAndValue ->
                        IntMap.remove keyIndexAndValue.Key indexedKeys

                // Add the key and it's new index to the key-index.
                IntMap.add (int newIndex) key newIndexedKeys

            // Evict the least-recently-used key (and it's associated value) from the cache
            // if this map (the original) is already full.
            if uint32 (IntMap.count newIndexedKeys) > capacity then
                // Remove the minimum key from the key-index and cache.
                // TODO : Fix this so it works correctly even when the index rolls over from 0xffffffff to 0x00000000
                // OPTIMIZE : Use IntMap.extractMin here once it's implemented.
                let minKeyIndex = IntMap.minKey newIndexedKeys
                let minKey = IntMap.find minKeyIndex newIndexedKeys
                let newIndexedKeys = IntMap.remove minKeyIndex newIndexedKeys
                let newCache = HashMap.remove minKey newCache

                // Return a new cache with the updated map and key-index.
                LruCache (newCache, newIndexedKeys, capacity, newIndex)
            else
                // Return a new cache with the updated map and key-index.
                LruCache (newCache, newIndexedKeys, capacity, newIndex)

    //
    member this.Remove (key : 'Key) : LruCache<'Key, 'T> =
        snd <| this.Extract key

    //
    member private this.Extract (key : 'Key) : KeyValuePair<'Key, 'T> option * LruCache<'Key, 'T> =
        match HashMap.tryFind key cache with
        | None ->
            None, this
        | Some keyIndexAndValue ->
            let cache' = HashMap.remove key cache
            let indexedKeys' = IntMap.remove keyIndexAndValue.Key indexedKeys

            // Return the new cache with the modified maps.
            // NOTE : The index is not updated here -- that is only done
            // when adding values to the cache.
            Some (KeyValuePair (key, keyIndexAndValue.Value)),
            LruCache (cache', indexedKeys', capacity, currentIndex)

    //
    member private this.Evict () : LruCache<'Key, 'T> =
        // If the cache is empty, there's nothing to do so just
        // return the cache without modifying it.
        if IntMap.isEmpty indexedKeys then this
        else
            // Remove the minimum key from the key-index and cache.
            // TODO : Fix this so it works correctly even when the index rolls over from 0xffffffff to 0x00000000
            // OPTIMIZE : Use IntMap.extractMin here once it's implemented.
            let minKeyIndex = IntMap.minKey indexedKeys
            let minKey = IntMap.find minKeyIndex indexedKeys
            let newIndexedKeys = IntMap.remove minKeyIndex indexedKeys
            let newCache = HashMap.remove minKey cache

            // Return a new cache with the updated map and key-index.
            LruCache (newCache, newIndexedKeys, capacity, currentIndex)

    //
    member private this.EvictMany (count : uint32) : LruCache<'Key, 'T> =
        if count = 0u then this
        else
            // Evict the next key/value from the cache.
            let this' = this.Evict ()

            // Continue evicting keys until we've evicted the specified number.
            this'.EvictMany (count - 1u)
    
    //
    member this.ChangeCapacity (newCapacity : uint32) : LruCache<'Key, 'T> =
        // If the new capacity is zero (0), return the empty instance.
        if newCapacity = 0u then empty
        elif newCapacity = capacity then
            this        // No change necessary.
        elif newCapacity > capacity then
            // The new capacity is larger than the existing capacity, so we can
            // just copy the data into a new cache with the increased capacity.
            LruCache (cache, indexedKeys, newCapacity, currentIndex)
        else
            // The new capacity is smaller than the old capacity, so we need to
            // evict (capacity - newCapacity) values.
            this.EvictMany (capacity - newCapacity)

    //
    member __.ToSeq () : seq<'Key * 'T> =
        indexedKeys
        |> IntMap.toSeq
        |> Seq.map (fun (_, key) ->
            // Find the value using the key.
            let kvp = HashMap.find key cache
            key, kvp.Value)

    //
    member __.ToList () : ('Key * 'T) list =
        // Fold backwards so we don't have to reverse the created list.
        (indexedKeys, [])
        ||> IntMap.foldBack (fun keyIndex key list ->
            // Find the value using the key.
            let kvp = HashMap.find key cache

            // DEBUG : Assert that the index in the key-value pair is
            // equal to the one from the key-index.
            assert (keyIndex = kvp.Key)

            // Cons the key and value onto the list.
            (key, kvp.Value) :: list)

    //
    member __.ToArray () : ('Key * 'T)[] =
        let kvps = ResizeArray ()

        indexedKeys
        |> IntMap.iter (fun keyIndex key ->
            // Find the value using the key.
            let kvp = HashMap.find key cache

            // DEBUG : Assert that the index in the key-value pair is
            // equal to the one from the key-index.
            assert (keyIndex = kvp.Key)

            // Add the key and value to the ResizeArray
            kvps.Add (key, kvp.Value))

        ResizeArray.toArray kvps

    //
    static member internal OfSeq (source : seq<'Key * 'T>) : LruCache<'Key, 'T> =
        // Preconditions
        checkNonNull "source" source

        (empty, source)
        ||> Seq.fold (fun cache (key, value) ->
            cache.Add (key, value))

    //
    static member internal OfList (source : ('Key * 'T) list) : LruCache<'Key, 'T> =
        // Preconditions
        checkNonNull "source" source

        (empty, source)
        ||> List.fold (fun cache (key, value) ->
            cache.Add (key, value))

    //
    static member internal OfArray (source : ('Key * 'T)[]) : LruCache<'Key, 'T> =
        // Preconditions
        checkNonNull "source" source
        
        (empty, source)
        ||> Array.fold (fun cache (key, value) ->
            cache.Add (key, value))

    /// Create a new LruCache with the specified capacity.
    static member internal Create (capacity : uint32) : LruCache<'Key, 'T> =
        LruCache (HashMap.empty, IntMap.empty, capacity, 0u)


//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LruCache =
    /// The empty cache.
    [<CompiledName("Empty")>]
    let empty<'Key, 'T when 'Key : comparison> =
        LruCache<'Key, 'T>.Empty

    //
    [<CompiledName("Count")>]
    let inline count (cache : LruCache<'Key, 'T>) : int =
        // Preconditions
        checkNonNull "cache" cache

        cache.Count

    //
    [<CompiledName("Capacity")>]
    let inline capacity (cache : LruCache<'Key, 'T>) : uint32 =
        // Preconditions
        checkNonNull "cache" cache

        cache.Capacity

    /// Create a new LruCache with the specified capacity.
    [<CompiledName("Create")>]
    let create (capacity : uint32) : LruCache<'Key, 'T> =
        LruCache.Create capacity

    //
    [<CompiledName("ChangeCapacity")>]
    let inline changeCapacity (cache : LruCache<'Key, 'T>) newCapacity : LruCache<'Key, 'T> =
        // Preconditions
        checkNonNull "cache" cache

        cache.ChangeCapacity newCapacity

    //
    [<CompiledName("ContainsKey")>]
    let inline containsKey (key : 'Key) (cache : LruCache<'Key, 'T>) : bool =
        // Preconditions
        checkNonNull "cache" cache

        cache.ContainsKey key

    //
    [<CompiledName("TryFind")>]
    let inline tryFind (key : 'Key) (cache : LruCache<'Key, 'T>) : 'T option * LruCache<'Key, 'T> =
        // Preconditions
        checkNonNull "cache" cache

        cache.TryFind key

    //
    [<CompiledName("Add")>]
    let inline add (key : 'Key) (value : 'T) (cache : LruCache<'Key, 'T>) : LruCache<'Key, 'T> =
        // Preconditions
        checkNonNull "cache" cache

        cache.Add (key, value)

    //
    [<CompiledName("Remove")>]
    let inline remove (key : 'Key) (cache : LruCache<'Key, 'T>) : LruCache<'Key, 'T> =
        // Preconditions
        checkNonNull "cache" cache

        cache.Remove key

    //
    [<CompiledName("OfSeq")>]
    let ofSeq (source : seq<'Key * 'T>) : LruCache<'Key, 'T> =
        // Preconditions checked within the member.
        LruCache.OfSeq source

    //
    [<CompiledName("OfList")>]
    let ofList (source : ('Key * 'T) list) : LruCache<'Key, 'T> =
        // Preconditions checked within the member.
        LruCache.OfList source

    //
    [<CompiledName("OfArray")>]
    let ofArray (source : ('Key * 'T)[]) : LruCache<'Key, 'T> =
        // Preconditions checked within the member.
        LruCache.OfArray source

    //
    [<CompiledName("OfMap")>]
    let ofMap (source : Map<'Key, 'T>) : LruCache<'Key, 'T> =
        // Preconditions
        checkNonNull "source" source

        source
        |> Map.toSeq
        |> LruCache.OfSeq

    //
    [<CompiledName("ToSeq")>]
    let inline toSeq (cache : LruCache<'Key, 'T>) : seq<'Key * 'T> =
        // Preconditions
        checkNonNull "cache" cache

        cache.ToSeq ()

    //
    [<CompiledName("ToList")>]
    let inline toList (cache : LruCache<'Key, 'T>) : ('Key * 'T) list =
        // Preconditions
        checkNonNull "cache" cache

        cache.ToList ()

    //
    [<CompiledName("ToArray")>]
    let inline toArray (cache : LruCache<'Key, 'T>) : ('Key * 'T)[] =
        // Preconditions
        checkNonNull "cache" cache

        cache.ToArray ()

    //
    [<CompiledName("ToMap")>]
    let toMap (cache : LruCache<'Key, 'T>) : Map<'Key, 'T> =
        // Preconditions
        checkNonNull "cache" cache

        cache.ToSeq ()
        |> Map.ofSeq
