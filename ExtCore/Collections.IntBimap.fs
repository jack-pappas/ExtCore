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

namespace ExtCore.Collections

open System.Collections.Generic
open System.Diagnostics
open LanguagePrimitives
open OptimizedClosures
open ExtCore


/// <summary>A bi-directional IntMap.</summary>
/// <typeparam name="Value">The type of the values.</typeparam>
[<Sealed; CompiledName("FSharpIntBimap`1")>]
//[<StructuredFormatDisplay("")>]
[<DebuggerDisplay("Count = {Count}")>]
[<DebuggerTypeProxy(typedefof<IntBimapDebuggerProxy<int>>)>]
type IntBimap<'Value when 'Value : comparison>
    private (map : IntMap<'Value>, inverseMap : Map<'Value, int>) =
    //
    static let empty = IntBimap (IntMap.Empty, Map.empty)

    //
    static member Empty
        with get () = empty

    //
    member private __.ForwardMap
        with get () = map

    //
    member private __.InverseMap
        with get () = inverseMap

    //
    static member private Equals (left : IntBimap<'Value>, right : IntBimap<'Value>) =
        left.ForwardMap = right.ForwardMap
        && left.InverseMap = right.InverseMap

    //
    static member private Compare (left : IntBimap<'Value>, right : IntBimap<'Value>) =
        match Unchecked.compare left.ForwardMap right.ForwardMap with
        | 0 ->
            compare left.InverseMap right.InverseMap
        | x -> x

    /// <inherit />
    override this.Equals other =
        match other with
        | :? IntBimap<'Value> as other ->
            IntBimap<_>.Equals (this, other)
        | _ ->
            false

    /// <inherit />
    override __.GetHashCode () =
        map.GetHashCode ()

    //
    member __.Count
        with get () =
            IntMap.count map

    //
    member __.IsEmpty
        with get () =
            IntMap.isEmpty map

    //
    member __.ContainsKey key =
        IntMap.containsKey key map

    //
    member __.ContainsValue value =
        Map.containsKey value inverseMap

    //
    member __.Find key =
        IntMap.find key map

    //
    member __.FindValue key =
        Map.find key inverseMap

    //
    member __.Paired (key, value) =
        // NOTE : We only need to check one of the maps, because all
        // Bimap functions maintain the invariant.
        match IntMap.tryFind key map with
        | None ->
            false
        | Some v ->
            v = value

    //
    member this.Remove key =
        // Use the key to find its corresponding value.
        match IntMap.tryFind key map with
        | None ->
            // The key doesn't exist. No changes are needed, so return this Bimap.
            this
        | Some value ->
            // Remove the values from both maps.
            IntBimap (
                IntMap.remove key map,
                Map.remove value inverseMap)

    //
    member this.RemoveValue key =
        // Use the key to find its corresponding value.
        match Map.tryFind key inverseMap with
        | None ->
            // The key doesn't exist. No changes are needed, so return this Bimap.
            this
        | Some value ->
            // Remove the values from both maps.
            IntBimap (
                IntMap.remove value map,
                Map.remove key inverseMap)

    //
    member __.TryFind key =
        IntMap.tryFind key map

    //
    member __.TryFindValue key =
        Map.tryFind key inverseMap

    //
    static member Singleton (key, value) : IntBimap<'Value> =
        IntBimap (
            IntMap.singleton key value,
            Map.singleton value key)

    //
    member this.Add (key, value) =
        // Add the values to both maps.
        // As in Map, we overwrite any existing entry; however, we have to be
        // a bit more thorough here to ensure the invariant is maintained.
        // OPTIMIZE : This could be implemented more efficiently to remove
        // unnecessary or duplicated checks while still maintaining the invariant.
        // It'd also be nice if we could do this in a way that detects if the values
        // are already present and bound to each other, so we don't need to alter the Bimap at all...
        // TODO : Create a private "AddUnsafe" method to avoid the lookups in TryAdd
        this.Remove(key)
            .RemoveValue(value)
            .TryAdd (key, value)

    //
    member this.TryAdd (key, value) =
        // Check that neither value is already bound in the Bimap
        // before adding them; if either already belongs to the map
        // return the original Bimap.
        match IntMap.tryFind key map, Map.tryFind value inverseMap with
        | None, None ->
            IntBimap (
                IntMap.add key value map,
                Map.add value key inverseMap)

        | _, _ ->
            // NOTE : We also return the original map when *both* values already
            // belong to the Bimap and are bound to each other -- because adding
            // them again wouldn't have any effect!
            this

    //
    member __.Iterate (action : int -> 'Value -> unit) : unit =
        IntMap.iter action map

    //
    member __.Fold (folder : 'State -> int -> 'Value -> 'State, state : 'State) : 'State =
        IntMap.fold folder state map

    //
    member __.FoldBack (folder : int -> 'Value -> 'State -> 'State, state : 'State) : 'State =
        IntMap.foldBack folder map state

    //
    member this.Filter (predicate : int -> 'Value -> bool) =
        let predicate = FSharpFunc<_,_,_>.Adapt predicate

        this.Fold ((fun (bimap : IntBimap<_>) key value ->
            if predicate.Invoke (key, value) then
                bimap
            else
                bimap.Remove key), this)

    //
    member this.Partition (predicate : int -> 'Value -> bool) =
        let predicate = FSharpFunc<_,_,_>.Adapt predicate

        // Partition efficiently by removing elements from the original map
        // and adding them to a new map when the predicate returns false
        // (instead of creating two new maps).
        this.Fold ((fun (trueBimap : IntBimap<_>, falseBimap : IntBimap<_>) key value ->
            if predicate.Invoke (key, value) then
                trueBimap, falseBimap
            else
                trueBimap.Remove key,
                falseBimap.Add (key, value)), (this, empty))

    //
    static member OfSeq (sequence : seq<int * 'Value>) : IntBimap<'Value> =
        // Preconditions
        checkNonNull "sequence" sequence

        (empty, sequence)
        ||> Seq.fold (fun bimap (key, value) ->
            bimap.Add (key, value))

    //
    static member OfList list : IntBimap<'Value> =
        // Preconditions
        checkNonNull "list" list

        (empty, list)
        ||> List.fold (fun bimap (key, value) ->
            bimap.Add (key, value))

    //
    static member OfArray array : IntBimap<'Value> =
        // Preconditions
        checkNonNull "array" array

        (empty, array)
        ||> Array.fold (fun bimap (key, value) ->
            bimap.Add (key, value))

    //
    member this.ToSeq () =
        map.ToSeq ()

    //
    member this.ToList () =
        map.ToList ()

    //
    member this.ToArray () =
        map.ToArray ()

    //
    member internal this.LeftKvpArray () : KeyValuePair<int, 'Value>[] =
        let elements = ResizeArray (1024)

        this.Iterate <| fun key value ->
            elements.Add (
                KeyValuePair (key, value))

        elements.ToArray ()

    //
    member internal this.RightKvpArray () : KeyValuePair<'Value, int>[] =
        let elements = ResizeArray (1024)

        this.Iterate <| fun key value ->
            elements.Add (
                KeyValuePair (value, key))

        elements.ToArray ()

    interface System.IComparable with
        member this.CompareTo other =
            match other with
            | :? IntBimap<'Value> as other ->
                IntBimap<_>.Compare (this, other)
            | _ ->
                invalidArg "other" "The object cannot be compared to FSharpIntBimap`1."

    interface System.IEquatable<IntBimap<'Value>> with
        member this.Equals other =
            IntBimap<_>.Equals (this, other)

    interface System.IComparable<IntBimap<'Value>> with
        member this.CompareTo other =
            IntBimap<_>.Compare (this, other)

//
and [<Sealed>]
    internal IntBimapDebuggerProxy<'Value when 'Value : comparison> (bimap : IntBimap<'Value>) =

    [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
    member __.Left
        with get () : KeyValuePair<int, 'Value>[] =
            bimap.LeftKvpArray ()

    [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
    member __.Right
        with get () : KeyValuePair<'Value, int>[] =
            bimap.RightKvpArray ()


/// Functional programming operators related to the IntBimap type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IntBimap =
    /// The empty IntBimap.
    [<CompiledName("Empty")>]
    let empty<'T when 'T : comparison> : IntBimap<'T> =
        IntBimap<'T>.Empty

    /// The map containing the given binding.
    [<CompiledName("Singleton")>]
    let inline singleton key value : IntBimap<'T> =
        IntBimap<'T>.Singleton (key, value)

    /// Is the map empty?
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (bimap : IntBimap<'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.IsEmpty

    /// Returns the number of bindings in the map.
    [<CompiledName("Count")>]
    let inline count (bimap : IntBimap<'T>) : int =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Count

    /// Tests if an element is in the domain of the map.
    [<CompiledName("ContainsKey")>]
    let inline containsKey key (bimap : IntBimap<'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ContainsKey key

    /// Tests if a value is in the range of the map.
    [<CompiledName("ContainsValue")>]
    let inline containsValue value (bimap : IntBimap<'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ContainsValue value

    /// Tests if an element and a value are bound to each other in the map.
    [<CompiledName("Paired")>]
    let inline paired key value (bimap : IntBimap<'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Paired (key, value)

    /// Lookup an element in the map, returning a Some value if the
    /// element is in the domain of the map and None if not.
    [<CompiledName("TryFind")>]
    let inline tryFind key (bimap : IntBimap<'T>) : 'T option =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryFind key

    /// Lookup a value in the map, returning a Some value if the
    /// element is in the range of the map and None if not.
    [<CompiledName("TryFindValue")>]
    let inline tryFindValue value (bimap : IntBimap<'T>) : int option =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryFindValue value

    /// Lookup an element in the map, raising KeyNotFoundException
    /// if no binding exists in the map.
    [<CompiledName("Find")>]
    let inline find key (bimap : IntBimap<'T>) : 'T =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Find key

    /// Lookup a value in the map, raising KeyNotFoundException
    /// if no binding exists in the map.
    [<CompiledName("FindValue")>]
    let inline findValue value (bimap : IntBimap<'T>) : int =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.FindValue value

    /// Removes an element from the domain of the map.
    /// No exception is raised if the element is not present.
    [<CompiledName("Remove")>]
    let inline remove key (bimap : IntBimap<'T>) : IntBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Remove key

    /// Removes a value from the range of the map.
    /// No exception is raised if the value is not present.
    [<CompiledName("RemoveValue")>]
    let inline removeValue key (bimap : IntBimap<'T>) : IntBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.RemoveValue key

    /// Returns a new map with the binding added to the given map.
    [<CompiledName("Add")>]
    let inline add key value (bimap : IntBimap<'T>) : IntBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Add (key, value)

    /// Returns a new map with the binding added to the given map, but only when
    /// neither the key nor the value are already bound. If the key and/or value
    /// are already bound, the map is returned unchanged.
    [<CompiledName("TryAdd")>]
    let inline tryAdd key value (bimap : IntBimap<'T>) : IntBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryAdd (key, value)

    /// Applies the given function to each binding in the map.
    [<CompiledName("Iterate")>]
    let inline iter (action : int -> 'T -> unit) (bimap : IntBimap<'T>) : unit =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Iterate action

    /// Folds over the bindings in the map.
    [<CompiledName("Fold")>]
    let inline fold (folder : 'State -> int -> 'T -> 'State)
            (state : 'State) (bimap : IntBimap<'T>) : 'State =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Fold (folder, state)

    /// Folds over the bindings in the map.
    [<CompiledName("FoldBack")>]
    let inline foldBack (folder : int -> 'T -> 'State -> 'State)
            (bimap : IntBimap<'T>) (state : 'State) : 'State =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.FoldBack (folder, state)

    /// Builds a new map containing only the bindings for which
    /// the given predicate returns 'true'.
    [<CompiledName("Filter")>]
    let inline filter (predicate : int -> 'T -> bool) (bimap : IntBimap<'T>) : IntBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Filter predicate

    /// Builds two new maps, one containing the bindings for which the given predicate
    /// returns 'true', and the other the remaining bindings.
    [<CompiledName("Partition")>]
    let inline partition (predicate : int -> 'T -> bool) (bimap : IntBimap<'T>)
            : IntBimap<'T> * IntBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Partition predicate

    /// Returns a new map made from the given bindings.
    [<CompiledName("OfSeq")>]
    let inline ofSeq (sequence : seq<int * 'T>) : IntBimap<'T> =
        // Preconditions checked by the member.
        IntBimap<'T>.OfSeq sequence

    /// Returns a new map made from the given bindings.
    [<CompiledName("OfList")>]
    let inline ofList list : IntBimap<'T> =
        // Preconditions checked by the member.
        IntBimap<_>.OfList list

    /// Returns a new map made from the given bindings.
    [<CompiledName("OfArray")>]
    let inline ofArray array : IntBimap<'T> =
        // Preconditions checked by the member.
        IntBimap<_>.OfArray array

    /// Views the collection as an enumerable sequence of pairs.
    /// The sequence will be ordered by the keys of the map.
    [<CompiledName("ToSeq")>]
    let inline toSeq (bimap : IntBimap<'T>) : seq<int * 'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToSeq ()

    /// Returns a list of all key-value pairs in the mapping.
    /// The list will be ordered by the keys of the map.
    [<CompiledName("ToList")>]
    let inline toList (bimap : IntBimap<'T>) : (int * 'T) list =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToList ()

    /// Returns an array of all key-value pairs in the mapping.
    /// The array will be ordered by the keys of the map.
    [<CompiledName("ToArray")>]
    let inline toArray (bimap : IntBimap<'T>) : (int * 'T)[] =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToArray ()


#if PROTO_COMPILER

/// <summary>A bi-directional TagMap.</summary>
/// <typeparam name="Tag">The tag (measure) type of the keys.</typeparam>
/// <typeparam name="Value">The type of the values.</typeparam>
[<MeasureAnnotatedAbbreviation>]
type TagBimap< [<Measure>] 'Tag, 'Value when 'Value : comparison > = IntBimap<'Value>

/// Functional programming operators related to the TagBimap<_,_> type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TagBimap =
    /// Retypes a value without emitting any IL instructions.
    /// WARNING: This should be used with EXTREME CAUTION.
    [<NoDynamicInvocation>]
    [<CompiledName("RetypeInlined")>]
    let inline private retype<'T,'U> (x:'T) : 'U = (# "" x : 'U #)

    /// The empty TagBimap.
    [<CompiledName("Empty")>]
    let empty<[<Measure>]'Tag, 'T when 'T : comparison> : TagBimap<'Tag, 'T> =
        retype IntBimap<'T>.Empty

    /// The map containing the given binding.
    [<CompiledName("Singleton")>]
    let inline singleton (key : int<'Tag>) value : TagBimap<'Tag, 'T> =
        IntBimap.Singleton (retype key, value)
        |> retype

    /// Is the map empty?
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (bimap : TagBimap<'Tag, 'T>) : bool =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.IsEmpty

    /// Returns the number of bindings in the map.
    [<CompiledName("Count")>]
    let inline count (bimap : TagBimap<'Tag, 'T>) : int =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Count

    /// Tests if an element is in the domain of the map.
    [<CompiledName("ContainsKey")>]
    let inline containsKey (key : int<'Tag>) (bimap : TagBimap<'Tag, 'T>) : bool =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap
        
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ContainsKey (retype key)

    /// Tests if a value is in the range of the map.
    [<CompiledName("ContainsValue")>]
    let inline containsValue key (bimap : TagBimap<'Tag, 'T>) : bool =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ContainsValue key

    /// Tests if an element and a value are bound to each other in the map.
    [<CompiledName("Paired")>]
    let inline paired (key : int<'Tag>) value (bimap : TagBimap<'Tag, 'T>) : bool =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Paired (retype key, value)

    /// Lookup an element in the map, returning a Some value if the
    /// element is in the domain of the map and None if not.
    [<CompiledName("TryFind")>]
    let inline tryFind (key : int<'Tag>) (bimap : TagBimap<'Tag, 'T>) : 'T option =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryFind (retype key)

    /// Lookup a value in the map, returning a Some value if the
    /// element is in the range of the map and None if not.
    [<CompiledName("TryFindValue")>]
    let inline tryFindValue key (bimap : TagBimap<'Tag, 'T>) : int<'Tag> option =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryFindValue key
        |> retype

    /// Lookup an element in the map, raising KeyNotFoundException
    /// if no binding exists in the map.
    [<CompiledName("Find")>]
    let inline find (key : int<'Tag>) (bimap : TagBimap<'Tag, 'T>) : 'T =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap
        
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Find (retype key)

    /// Lookup a value in the map, raising KeyNotFoundException
    /// if no binding exists in the map.
    [<CompiledName("FindValue")>]
    let inline findValue key (bimap : TagBimap<'Tag, 'T>) : int<'Tag> =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.FindValue key
        |> retype

    /// Removes an element from the domain of the map.
    /// No exception is raised if the element is not present.
    [<CompiledName("Remove")>]
    let inline remove (key : int<'Tag>) (bimap : TagBimap<'Tag, 'T>) : TagBimap<'Tag, 'T> =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Remove (retype key)
        |> retype

    /// Removes a value from the range of the map.
    /// No exception is raised if the value is not present.
    [<CompiledName("RemoveValue")>]
    let inline removeValue key (bimap : TagBimap<'Tag, 'T>) : TagBimap<'Tag, 'T> =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.RemoveValue key
        |> retype

    /// Returns a new map with the binding added to the given map.
    [<CompiledName("Add")>]
    let inline add (key : int<'Tag>) value (bimap : TagBimap<'Tag, 'T>) : TagBimap<'Tag, 'T> =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Add (retype key, value)
        |> retype

    /// Returns a new map with the binding added to the given map, but only when
    /// neither the key nor the value are already bound. If the key and/or value
    /// are already bound, the map is returned unchanged.
    [<CompiledName("TryAdd")>]
    let inline tryAdd (key : int<'Tag>) value (bimap : TagBimap<'Tag, 'T>) : TagBimap<'Tag, 'T> =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryAdd (retype key, value)
        |> retype

    /// Applies the given function to each binding in the map.
    [<CompiledName("Iterate")>]
    let inline iter (action : int<'Tag> -> 'T -> unit) (bimap : TagBimap<'Tag, 'T>) : unit =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Iterate (retype action)

    /// Folds over the bindings in the map.
    [<CompiledName("Fold")>]
    let inline fold (folder : 'State -> int<'Tag> -> 'T -> 'State)
            (state : 'State) (bimap : TagBimap<'Tag, 'T>) : 'State =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Fold (retype folder, state)

    /// Folds over the bindings in the map.
    [<CompiledName("FoldBack")>]
    let inline foldBack (folder : int<'Tag> -> 'T -> 'State -> 'State)
            (bimap : TagBimap<'Tag, 'T>) (state : 'State) : 'State =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.FoldBack (retype folder, state)

    /// Builds a new map containing only the bindings for which
    /// the given predicate returns 'true'.
    [<CompiledName("Filter")>]
    let inline filter (predicate : int<'Tag> -> 'T -> bool) (bimap : TagBimap<'Tag, 'T>) : TagBimap<'Tag, 'T> =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Filter (retype predicate)
        |> retype

    /// Builds two new maps, one containing the bindings for which the given predicate
    /// returns 'true', and the other the remaining bindings.
    [<CompiledName("Partition")>]
    let inline partition (predicate : int<'Tag> -> 'T -> bool) (bimap : TagBimap<'Tag, 'T>)
            : TagBimap<'Tag, 'T> * TagBimap<'Tag, 'T> =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        let trueMap, falseMap = bimap.Partition (retype predicate)
        (retype trueMap), (retype falseMap)

    /// Returns a new map made from the given bindings.
    [<CompiledName("OfSeq")>]
    let inline ofSeq (sequence : seq<int<'Tag> * 'T>) : TagBimap<'Tag, 'T> =
        // Preconditions
        checkNonNull "sequence" sequence

        IntBimap<_>.OfSeq (retype sequence)
        |> retype

    /// Returns a new map made from the given bindings.
    [<CompiledName("OfList")>]
    let inline ofList (list : (int<'Tag> * 'T) list) : TagBimap<'Tag, 'T> =
        // Preconditions
        checkNonNull "list" list

        IntBimap<_>.OfList (retype list)
        |> retype

    /// Returns a new map made from the given bindings.
    [<CompiledName("OfArray")>]
    let inline ofArray (array : (int<'Tag> * 'T)[]) : TagBimap<'Tag, 'T> =
        // Preconditions
        checkNonNull "array" array

        IntBimap<_>.OfArray (retype array)
        |> retype

    /// Views the collection as an enumerable sequence of pairs.
    /// The sequence will be ordered by the keys of the map.
    [<CompiledName("ToSeq")>]
    let inline toSeq (bimap : TagBimap<'Tag, 'T>) : seq<int<'Tag> * 'T> =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToSeq ()
        |> retype

    /// Returns a list of all key-value pairs in the mapping.
    /// The list will be ordered by the keys of the map.
    [<CompiledName("ToList")>]
    let inline toList (bimap : TagBimap<'Tag, 'T>) : (int<'Tag> * 'T) list =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToList ()
        |> retype

    /// Returns an array of all key-value pairs in the mapping.
    /// The array will be ordered by the keys of the map.
    [<CompiledName("ToArray")>]
    let inline toArray (bimap : TagBimap<'Tag, 'T>) : (int<'Tag> * 'T)[] =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToArray ()
        |> retype

#endif
