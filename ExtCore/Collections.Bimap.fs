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

//
namespace ExtCore.Collections

open System.Collections.Generic
open System.Diagnostics
open LanguagePrimitives
open OptimizedClosures
open ExtCore


/// <summary>
/// An immutable, bi-directional map. Keys are ordered by F# generic comparison.
/// </summary>
/// <typeparam name="Key">The type of the keys.</typeparam>
/// <typeparam name="Value">The type of the values.</typeparam>
[<Sealed; CompiledName("FSharpBimap`2")>]
//[<StructuredFormatDisplay("")>]
[<DebuggerDisplay("Count = {Count}")>]
[<DebuggerTypeProxy(typedefof<BimapDebuggerProxy<int,int>>)>]
type Bimap<'Key, 'Value when 'Key : comparison and 'Value : comparison>
    private (map : Map<'Key, 'Value>, inverseMap : Map<'Value, 'Key>) =
    
    /// The empty Bimap instance.
    static let empty = Bimap (Map.empty, Map.empty)
    
    /// The empty Bimap.
    static member Empty
        with get () = empty

    //
    member private __.ForwardMap
        with get () = map

    //
    member private __.InverseMap
        with get () = inverseMap

    //
    static member private Equals
        (left : Bimap<'Key, 'Value>, right : Bimap<'Key, 'Value>) =
        left.ForwardMap = right.ForwardMap
        && left.InverseMap = right.InverseMap

    //
    static member private Compare
        (left : Bimap<'Key, 'Value>, right : Bimap<'Key, 'Value>) =
        match compare left.ForwardMap right.ForwardMap with
        | 0 ->
            compare left.InverseMap right.InverseMap
        | x -> x

    /// <inherit />
    override this.Equals other =
        match other with
        | :? Bimap<'Key, 'Value> as other ->
            Bimap<_,_>.Equals (this, other)
        | _ ->
            false

    /// <inherit />
    override __.GetHashCode () =
        map.GetHashCode ()

    /// The number of items in the Bimap.
    member __.Count
        with get () =
            Map.count map

    /// Is the Bimap empty?
    member __.IsEmpty
        with get () =
            Map.isEmpty map

    //
    member __.ContainsKey key =
        Map.containsKey key map

    //
    member __.ContainsValue value =
        Map.containsKey value inverseMap

    //
    member __.Find key =
        Map.find key map

    //
    member __.FindValue value =
        Map.find value inverseMap

    //
    member __.Paired (x, y) =
        // NOTE : We only need to check one of the maps, because all
        // Bimap functions maintain the invariant.
        match Map.tryFind x map with
        | None ->
            false
        | Some value ->
            value = y

    //
    member this.Remove key =
        // Use the key to find its corresponding value.
        match Map.tryFind key map with
        | None ->
            // The key doesn't exist. No changes are needed, so return this Bimap.
            this
        | Some value ->
            // Remove the values from both maps.
            Bimap (
                Map.remove key map,
                Map.remove value inverseMap)

    //
    member this.RemoveValue value =
        // Use the key to find its corresponding value.
        match Map.tryFind value inverseMap with
        | None ->
            // The key doesn't exist. No changes are needed, so return this Bimap.
            this
        | Some key ->
            // Remove the values from both maps.
            Bimap (
                Map.remove key map,
                Map.remove value inverseMap)

    //
    member __.TryFind key =
        Map.tryFind key map

    //
    member __.TryFindValue value =
        Map.tryFind value inverseMap

    //
    static member Singleton (x, y) : Bimap<'Key, 'Value> =
        Bimap (
            Map.singleton x y,
            Map.singleton y x)

    //
    member this.Add (x, y) =
        // Add the values to both maps.
        // As in Map, we overwrite any existing entry; however, we have to be
        // a bit more thorough here to ensure the invariant is maintained.
        // OPTIMIZE : This could be implemented more efficiently to remove
        // unnecessary or duplicated checks while still maintaining the invariant.
        // It'd also be nice if we could do this in a way that detects if the values
        // are already present and bound to each other, so we don't need to alter the Bimap at all...
        // TODO : Create a private "AddUnsafe" method to avoid the lookups in TryAdd
        this.Remove(x).RemoveValue(y).TryAdd (x, y)

    //
    member this.TryAdd (x, y) =
        // Check that neither value is already bound in the Bimap
        // before adding them; if either already belongs to the map
        // return the original Bimap.
        match Map.tryFind x map, Map.tryFind y inverseMap with
        | None, None ->
            Bimap (
                Map.add x y map,
                Map.add y x inverseMap)

        | _, _ ->
            // NOTE : We also return the original map when *both* values already
            // belong to the Bimap and are bound to each other -- because adding
            // them again wouldn't have any effect!
            this

    //
    member __.Iterate (action : 'Key -> 'Value -> unit) : unit =
        Map.iter action map

    //
    member __.Fold (folder : 'State -> 'Key -> 'Value -> 'State, state : 'State) : 'State =
        Map.fold folder state map

    //
    member __.FoldBack (folder : 'Key -> 'Value -> 'State -> 'State, state : 'State) : 'State =
        Map.foldBack folder map state

    //
    member this.Filter (predicate : 'Key -> 'Value -> bool) =
        let predicate = FSharpFunc<_,_,_>.Adapt predicate

        this.Fold ((fun (bimap : Bimap<_,_>) x y ->
            if predicate.Invoke (x, y) then
                bimap
            else
                bimap.Remove x), this)

    //
    member this.Partition (predicate : 'Key -> 'Value -> bool) =
        let predicate = FSharpFunc<_,_,_>.Adapt predicate

        // Partition efficiently by removing elements from the original map
        // and adding them to a new map when the predicate returns false
        // (instead of creating two new maps).
        this.Fold ((fun (trueBimap : Bimap<_,_>, falseBimap : Bimap<_,_>) x y ->
            if predicate.Invoke (x, y) then
                trueBimap, falseBimap
            else
                trueBimap.Remove x,
                falseBimap.Add (x, y)), (this, empty))

    //
    static member OfSeq sequence : Bimap<'Key, 'Value> =
        // Preconditions
        //checkNonNull "sequence" sequence

        (empty, sequence)
        ||> Seq.fold (fun bimap (x, y) ->
            bimap.Add (x, y))

    //
    static member OfList list : Bimap<'Key, 'Value> =
        // Preconditions
        checkNonNull "list" list

        (empty, list)
        ||> List.fold (fun bimap (x, y) ->
            bimap.Add (x, y))

    //
    static member OfArray array : Bimap<'Key, 'Value> =
        // Preconditions
        checkNonNull "array" array

        (empty, array)
        ||> Array.fold (fun bimap (x, y) ->
            bimap.Add (x, y))

    //
    static member OfMap map : Bimap<'Key, 'Value> =
        // Preconditions
        checkNonNull "map" map

        /// The inverse map.
        let inverseMap = Map.inverse map

        // Remove any bindings from the original map which don't exist in the inverse map.
        let map =
            (map, map)
            ||> Map.fold (fun map k v ->
                match Map.tryFind v inverseMap with
                | None ->
                    Map.remove k map
                | Some k' ->
                    if k = k' then map
                    else Map.remove k map)

        Bimap (map, Map.inverse map)

    //
    member this.ToSeq () : seq<'Key * 'Value> =
        Map.toSeq map

    //
    member this.ToList () : ('Key * 'Value) list =
        Map.toList map

    //
    member this.ToArray () : ('Key * 'Value)[] =
        Map.toArray map

    //
    member this.ToMap () : Map<'Key, 'Value> =
        map

    //
    member internal this.LeftKvpArray () : KeyValuePair<'Key, 'Value>[] =
        let elements = ResizeArray (1024)

        this.Iterate <| fun x y ->
            elements.Add (
                KeyValuePair (x, y))

        elements.ToArray ()

    //
    member internal this.RightKvpArray () : KeyValuePair<'Value, 'Key>[] =
        let elements = ResizeArray (1024)

        this.Iterate <| fun x y ->
            elements.Add (
                KeyValuePair (y, x))

        elements.ToArray ()

    interface System.IEquatable<Bimap<'Key, 'Value>> with
        member this.Equals other =
            Bimap<_,_>.Equals (this, other)

    interface System.IComparable<Bimap<'Key, 'Value>> with
        member this.CompareTo other =
            Bimap<_,_>.Compare (this, other)


/// Internal. Debugger proxy type for Bimap.
and [<Sealed>]
    internal BimapDebuggerProxy<'Key, 'Value
        when 'Key : comparison and 'Value : comparison> (bimap : Bimap<'Key, 'Value>) =

    [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
    member __.Left
        with get () : KeyValuePair<'Key, 'Value>[] =
            bimap.LeftKvpArray ()

    [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
    member __.Right
        with get () : KeyValuePair<'Value, 'Key>[] =
            bimap.RightKvpArray ()

/// Functional programming operators related to the Bimap<_,_> type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Bimap =
    /// The empty Bimap.
    [<CompiledName("Empty")>]
    let empty<'Key, 'T
        when 'Key : comparison
        and 'T : comparison> : Bimap<'Key, 'T> =
        Bimap<'Key, 'T>.Empty

    //
    [<CompiledName("Singleton")>]
    let inline singleton x y : Bimap<'Key, 'T> =
        Bimap.Singleton (x, y)

    //
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (bimap : Bimap<'Key, 'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.IsEmpty

    //
    [<CompiledName("Count")>]
    let inline count (bimap : Bimap<'Key, 'T>) : int =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Count

    //
    [<CompiledName("ContainsKey")>]
    let inline containsKey key (bimap : Bimap<'Key, 'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ContainsKey key

    //
    [<CompiledName("ContainsValue")>]
    let inline containsValue value (bimap : Bimap<'Key, 'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ContainsValue value

    //
    [<CompiledName("Paired")>]
    let inline paired key value (bimap : Bimap<'Key, 'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Paired (key, value)

    //
    [<CompiledName("TryFind")>]
    let inline tryFind key (bimap : Bimap<'Key, 'T>) : 'T option =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryFind key

    //
    [<CompiledName("TryFindValue")>]
    let inline tryFindValue value (bimap : Bimap<'Key, 'T>) : 'Key option =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryFindValue value

    //
    [<CompiledName("Find")>]
    let inline find key (bimap : Bimap<'Key, 'T>) : 'T =
        // Preconditions
        checkNonNull "bimap" bimap
        
        bimap.Find key

    //
    [<CompiledName("FindValue")>]
    let inline findValue value (bimap : Bimap<'Key, 'T>) : 'Key =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.FindValue value

    //
    [<CompiledName("Remove")>]
    let inline remove key (bimap : Bimap<'Key, 'T>) : Bimap<'Key, 'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Remove key

    //
    [<CompiledName("RemoveValue")>]
    let inline removeValue value (bimap : Bimap<'Key, 'T>) : Bimap<'Key, 'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.RemoveValue value

    //
    [<CompiledName("Add")>]
    let inline add x y (bimap : Bimap<'Key, 'T>) : Bimap<'Key, 'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Add (x, y)

    //
    [<CompiledName("TryAdd")>]
    let inline tryAdd x y (bimap : Bimap<'Key, 'T>) : Bimap<'Key, 'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryAdd (x, y)

    //
    [<CompiledName("OfSeq")>]
    let inline ofSeq sequence : Bimap<'Key, 'T> =
        // Preconditions checked by the member.
        Bimap<_,_>.OfSeq sequence

    //
    [<CompiledName("OfList")>]
    let inline ofList list : Bimap<'Key, 'T> =
        // Preconditions checked by the member.
        Bimap<_,_>.OfList list

    //
    [<CompiledName("OfArray")>]
    let inline ofArray array : Bimap<'Key, 'T> =
        // Preconditions checked by the member.
        Bimap<_,_>.OfArray array

    //
    [<CompiledName("OfMap")>]
    let inline ofMap map : Bimap<'Key, 'T> =
        // Preconditions checked by the member.
        Bimap<_,_>.OfMap map

    //
    [<CompiledName("ToSeq")>]
    let inline toSeq (bimap : Bimap<'Key, 'T>) : seq<'Key * 'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToSeq ()

    //
    [<CompiledName("ToList")>]
    let inline toList (bimap : Bimap<'Key, 'T>) : ('Key * 'T) list =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToList ()

    //
    [<CompiledName("ToArray")>]
    let inline toArray (bimap : Bimap<'Key, 'T>) : ('Key * 'T)[] =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToArray ()

    //
    [<CompiledName("ToMap")>]
    let inline toMap (bimap : Bimap<'Key, 'T>) : Map<'Key, 'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToMap ()

    //
    [<CompiledName("Iter")>]
    let inline iter (action : 'Key -> 'T -> unit) (bimap : Bimap<'Key, 'T>) : unit =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Iterate action

    //
    [<CompiledName("Fold")>]
    let inline fold (folder : 'State -> 'Key -> 'T -> 'State)
            (state : 'State) (bimap : Bimap<'Key, 'T>) : 'State =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Fold (folder, state)

    //
    [<CompiledName("FoldBack")>]
    let inline foldBack (folder : 'Key -> 'T -> 'State -> 'State)
            (bimap : Bimap<'Key, 'T>) (state : 'State) : 'State =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.FoldBack (folder, state)

    //
    [<CompiledName("Filter")>]
    let inline filter (predicate : 'Key -> 'T -> bool) (bimap : Bimap<'Key, 'T>) : Bimap<'Key, 'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Filter predicate

    //
    [<CompiledName("Partition")>]
    let inline partition (predicate : 'Key -> 'T -> bool) (bimap : Bimap<'Key, 'T>)
            : Bimap<'Key, 'T> * Bimap<'Key, 'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Partition predicate

