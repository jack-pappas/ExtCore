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


/// An immutable cache data structure with a Least-Recently-Used (LRU) eviction policy.
type LruCache<'Key, 'T when 'Key : equality>
    private (cache : HashMap<'Key, KeyValuePair<int, 'T>>, indexedKeys : IntMap<'Key>,
             capacity : uint32, currentIndex : int) =
    /// The empty cache instance.
    static let empty = LruCache (HashMap.empty, IntMap.empty, 0u, 0)

    /// The empty cache instance.
    static member Empty
        with get () = empty

    /// Create an LruCache from a sequence of key-value pairs.
    new (capacity : uint32, elements : seq<'Key * 'T>) =
        // Preconditions
        checkNonNull "elements" elements

        // OPTIMIZATION : When the capacity is zero, we don't even need to consume the sequence.
        if capacity = 0u then
            // Unfortunately, we can't just return the empty cache instance,
            // we must actually call a constructor.
            LruCache (HashMap.empty, IntMap.empty, 0u, 0)
        else
            // OPTIMIZE : Try to cast the sequence to array or list;
            // if it succeeds use the specialized method for that type for better performance.

            notImpl "LruCache::.ctor(uint32, seq<'Key, 'T>)"
            // TEMP : This is necessary because of the exception being raised (above).
            // It can be removed whenever this code path is implemented.
            LruCache (HashMap.empty, IntMap.empty, 0u, 0)

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

    /// Look up a key in the cache, returning Some with the associated value and
    /// updated cache if the key is in the domain of the cache and None if not.
    member __.TryFind (key : 'Key) : ('T * LruCache<'Key, 'T>) option =
        notImpl "LruCache.TryFind"

    /// Tests if a key is in the domain of the cache.
    member __.ContainsKey (key : 'Key) : bool =
        notImpl "LruCache.ContainsKey"

    //
    member __.Add (key : 'Key, value : 'T) : LruCache<'Key, 'T> =
        // NOTE : Checked.add MUST be used when incrementing 'count'!
        notImpl "LruCache.Add"

    //
    member __.Remove (key : 'Key) : LruCache<'Key, 'T> =
        notImpl "LruCache.Remove"
    
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
            notImpl "LruCache.ChangeCapacity"

    //
    member __.ToSeq () : seq<'Key * 'T> =
        notImpl "LruCache.ToSeq"

    //
    member __.ToList () : ('Key * 'T) list =
        notImpl "LruCache.ToList"

    //
    member __.ToArray () : ('Key * 'T)[] =
        notImpl "LruCache.ToSeq"

    //
    static member OfSeq (source : seq<'Key * 'T>) : LruCache<'Key, 'T> =
        notImpl "LruCache.OfSeq"

    //
    static member OfList (source : ('Key * 'T) list) : LruCache<'Key, 'T> =
        notImpl "LruCache.OfList"

    //
    static member OfArray (source : ('Key * 'T)[]) : LruCache<'Key, 'T> =
        notImpl "LruCache.OfArray"


//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LruCache =
    /// The empty cache.
    [<CompiledName("Empty")>]
    let empty<'Key, 'T when 'Key : equality> =
        LruCache<'Key, 'T>.Empty

    //
    [<CompiledName("Count")>]
    let inline count (cache : LruCache<'Key, 'T>) : int =
        // Preconditions
        checkNonNull "cache" cache

        cache.Count

    //
    [<CompiledName("Count")>]
    let inline capacity (cache : LruCache<'Key, 'T>) : uint32 =
        // Preconditions
        checkNonNull "cache" cache

        cache.Capacity

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
    let inline tryFind (key : 'Key) (cache : LruCache<'Key, 'T>) : ('T * LruCache<'Key, 'T>) option =
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
    let inline ofSeq (source : seq<'Key * 'T>) : LruCache<'Key, 'T> =
        // Preconditions
        checkNonNull "source" source

        LruCache.OfSeq source

    //
    [<CompiledName("OfList")>]
    let inline ofList (source : ('Key * 'T) list) : LruCache<'Key, 'T> =
        // Preconditions
        checkNonNull "source" source

        LruCache.OfList source

    //
    [<CompiledName("OfArray")>]
    let inline ofArray (source : ('Key * 'T)[]) : LruCache<'Key, 'T> =
        // Preconditions
        checkNonNull "source" source

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



