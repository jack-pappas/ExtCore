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

open System.Diagnostics
open LanguagePrimitives
open OptimizedClosures
open ExtCore


/// A bi-directional map.
[<Sealed>]
[<NoEquality; NoComparison>]
//[<DebuggerTypeProxy(typedefof<BimapDebuggerProxy<int,int>>)>]
[<DebuggerDisplay("Count = {Count}")>]
type Bimap<'T1, 'T2
    when 'T1 : comparison
    and 'T2 : comparison> private (left : Map<'T1, 'T2>, right : Map<'T2, 'T1>) =
    /// The empty Bimap instance.
    static let empty = Bimap (Map.empty, Map.empty)
    
    //
    static member Empty
        with get () = empty

    //
    member __.Count
        with get () =
            Map.count left

    //
    member __.IsEmpty
        with get () =
            Map.isEmpty left

    //
    member __.ContainsKey key =
        Map.containsKey key left

    //
    member __.ContainsKeyBack key =
        Map.containsKey key right

    //
    member __.Find key =
        Map.find key left

    //
    member __.FindBack key =
        Map.find key right

    //
    member __.Paired (x, y) =
        // NOTE : We only need to check one of the maps, because all
        // Bimap functions maintain the invariant.
        match Map.tryFind x left with
        | None ->
            false
        | Some value ->
            value = y

    //
    member this.Remove key =
        // Use the key to find its corresponding value.
        match Map.tryFind key left with
        | None ->
            // The key doesn't exist. No changes are needed, so return this Bimap.
            this
        | Some value ->
            // Remove the values from both maps.
            Bimap (
                Map.remove key left,
                Map.remove value right)

    //
    member this.RemoveBack key =
        // Use the key to find its corresponding value.
        match Map.tryFind key right with
        | None ->
            // The key doesn't exist. No changes are needed, so return this Bimap.
            this
        | Some value ->
            // Remove the values from both maps.
            Bimap (
                Map.remove value left,
                Map.remove key right)

    //
    member __.TryFind key =
        Map.tryFind key left

    //
    member __.TryFindBack key =
        Map.tryFind key right

    //
    static member Singleton (x, y) : Bimap<'T1, 'T2> =
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
        let bimap = this.Remove x
        let bimap = this.RemoveBack y
        bimap.TryAdd (x, y)    // TODO : Create a private "AddUnsafe" method to avoid the lookups in TryAdd

    //
    member this.TryAdd (x, y) =
        // Check that neither value is already bound in the Bimap
        // before adding them; if either already belongs to the map
        // return the original Bimap.
        match Map.tryFind x left, Map.tryFind y right with
        | None, None ->
            Bimap (
                Map.add x y left,
                Map.add y x right)

        | _, _ ->
            // NOTE : We also return the original map when *both* values already
            // belong to the Bimap and are bound to each other -- because adding
            // them again wouldn't have any effect!
            this

    //
    member __.Iter (action : 'T1 -> 'T2 -> unit) : unit =
        Map.iter action left

    //
    member __.Fold (folder : 'State -> 'T1 -> 'T2 -> 'State, state : 'State) : 'State =
        Map.fold folder state left

    //
    member __.FoldBack (folder : 'T1 -> 'T2 -> 'State -> 'State, state : 'State) : 'State =
        Map.foldBack folder left state

    //
    member this.Filter (predicate : 'T1 -> 'T2 -> bool) =
        let predicate = FSharpFunc<_,_,_>.Adapt predicate

        this.Fold ((fun (bimap : Bimap<_,_>) x y ->
            if predicate.Invoke (x, y) then
                bimap
            else
                bimap.Remove x), this)

    //
    member this.Partition (predicate : 'T1 -> 'T2 -> bool) =
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
    static member OfList list =
        (empty, list)
        ||> List.fold (fun bimap (x, y) ->
            bimap.Add (x, y))

    //
    member this.ToList () =
        this.FoldBack ((fun x y list ->
            (x, y) :: list), [])

    //
    static member OfArray array =
        (empty, array)
        ||> Array.fold (fun bimap (x, y) ->
            bimap.Add (x, y))

    //
    member this.ToArray () =
        let elements = ResizeArray ()

        this.Iter (fun x y ->
            elements.Add (x, y))

        ResizeArray.toArray elements

/// Functional programming operators related to the Bimap<_,_> type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Bimap =
    /// The empty Bimap.
    [<CompiledName("Empty")>]
    [<GeneralizableValue>]
    let empty<'T1, 'T2
        when 'T1 : comparison
        and 'T2 : comparison> : Bimap<'T1, 'T2> =
        Bimap<'T1, 'T2>.Empty

    //
    [<CompiledName("Singleton")>]
    let inline singleton x y : Bimap<'T1, 'T2> =
        Bimap.Singleton (x, y)

    //
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (bimap : Bimap<'T1, 'T2>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.IsEmpty

    //
    [<CompiledName("Count")>]
    let inline count (bimap : Bimap<'T1, 'T2>) : int =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Count

    //
    [<CompiledName("ContainsKey")>]
    let inline containsKey key (bimap : Bimap<'T1, 'T2>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ContainsKey key

    //
    [<CompiledName("ContainsKeyBack")>]
    let inline containsKeyBack key (bimap : Bimap<'T1, 'T2>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ContainsKeyBack key

    //
    [<CompiledName("Paired")>]
    let inline paired x y (bimap : Bimap<'T1, 'T2>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Paired (x, y)

    //
    [<CompiledName("TryFind")>]
    let inline tryFind key (bimap : Bimap<'T1, 'T2>) : 'T2 option =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryFind key

    //
    [<CompiledName("TryFindBack")>]
    let inline tryFindBack key (bimap : Bimap<'T1, 'T2>) : 'T1 option =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryFindBack key

    //
    [<CompiledName("Find")>]
    let inline find key (bimap : Bimap<'T1, 'T2>) : 'T2 =
        // Preconditions
        checkNonNull "bimap" bimap
        
        bimap.Find key

    //
    [<CompiledName("FindBack")>]
    let inline findBack key (bimap : Bimap<'T1, 'T2>) : 'T1 =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.FindBack key

    //
    [<CompiledName("Remove")>]
    let inline remove key (bimap : Bimap<'T1, 'T2>) : Bimap<'T1, 'T2> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Remove key

    //
    [<CompiledName("RemoveBack")>]
    let inline removeBack key (bimap : Bimap<'T1, 'T2>) : Bimap<'T1, 'T2> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.RemoveBack key

    //
    [<CompiledName("Add")>]
    let inline add x y (bimap : Bimap<'T1, 'T2>) : Bimap<'T1, 'T2> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Add (x, y)

    //
    [<CompiledName("TryAdd")>]
    let inline tryAdd x y (bimap : Bimap<'T1, 'T2>) : Bimap<'T1, 'T2> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryAdd (x, y)

    //
    [<CompiledName("Iterate")>]
    let inline iter (action : 'T1 -> 'T2 -> unit) (bimap : Bimap<'T1, 'T2>) : unit =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Iter action

    //
    [<CompiledName("Fold")>]
    let inline fold (folder : 'State -> 'T1 -> 'T2 -> 'State)
            (state : 'State) (bimap : Bimap<'T1, 'T2>) : 'State =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Fold (folder, state)

    //
    [<CompiledName("FoldBack")>]
    let inline foldBack (folder : 'T1 -> 'T2 -> 'State -> 'State)
            (bimap : Bimap<'T1, 'T2>) (state : 'State) : 'State =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.FoldBack (folder, state)

    //
    [<CompiledName("Filter")>]
    let inline filter (predicate : 'T1 -> 'T2 -> bool) (bimap : Bimap<'T1, 'T2>) : Bimap<'T1, 'T2> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Filter predicate

    //
    [<CompiledName("Partition")>]
    let inline partition (predicate : 'T1 -> 'T2 -> bool) (bimap : Bimap<'T1, 'T2>)
            : Bimap<'T1, 'T2> * Bimap<'T1, 'T2> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Partition predicate

    //
    [<CompiledName("OfList")>]
    let inline ofList list : Bimap<'T1, 'T2> =
        // Preconditions
        checkNonNull "list" list

        Bimap<_,_>.OfList list

    //
    [<CompiledName("ToList")>]
    let inline toList (bimap : Bimap<'T1, 'T2>) : _ list =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToList ()

    //
    [<CompiledName("OfArray")>]
    let inline ofArray array : Bimap<'T1, 'T2> =
        // Preconditions
        checkNonNull "array" array

        Bimap<_,_>.OfArray array

    //
    [<CompiledName("ToArray")>]
    let inline toArray (bimap : Bimap<'T1, 'T2>) =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToArray ()

    // TODO
    // ofMap, toMap, tryOfMap


/// <summary>A bi-directional IntMap.</summary>
[<NoEquality; NoComparison>]
type IntBimap<'T when 'T : comparison> = {
    //
    Left : IntMap<'T>;
    //
    Right : Map<'T, int>;
}

/// Functional programming operators related to the IntBimap type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IntBimap =
    /// The empty Bimap.
    [<CompiledName("Empty")>]
    let empty<'T when 'T : comparison> : IntBimap<'T> =
        { Left = IntMap.empty; Right = Map.empty; }

    //
    [<CompiledName("Singleton")>]
    let singleton x y : IntBimap<'T> =
        { Left = IntMap.singleton x y;
          Right = Map.singleton y x; }

    //
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (bimap : IntBimap<'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        IntMap.isEmpty bimap.Left

    //
    [<CompiledName("Count")>]
    let inline count (bimap : IntBimap<'T>) : int =
        // Preconditions
        checkNonNull "bimap" bimap

        IntMap.count bimap.Left

    //
    [<CompiledName("ContainsKey")>]
    let inline containsKey key (bimap : IntBimap<'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        IntMap.containsKey key bimap.Left

    //
    [<CompiledName("ContainsKeyBack")>]
    let inline containsKeyBack key (bimap : IntBimap<'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        Map.containsKey key bimap.Right

    //
    [<CompiledName("Paired")>]
    let paired x y (bimap : IntBimap<'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        // NOTE : We only need to check one of the maps, because all
        // Bimap functions maintain the invariant.
        match IntMap.tryFind x bimap.Left with
        | None ->
            false
        | Some value ->
            value = y

    //
    [<CompiledName("TryFind")>]
    let tryFind key (bimap : IntBimap<'T>) : 'T option =
        // Preconditions
        checkNonNull "bimap" bimap

        IntMap.tryFind key bimap.Left

    //
    [<CompiledName("TryFindBack")>]
    let tryFindBack key (bimap : IntBimap<'T>) : int option =
        // Preconditions
        checkNonNull "bimap" bimap

        Map.tryFind key bimap.Right

    //
    [<CompiledName("Find")>]
    let find key (bimap : IntBimap<'T>) : 'T =
        // Preconditions
        checkNonNull "bimap" bimap

        IntMap.find key bimap.Left

    //
    [<CompiledName("FindBack")>]
    let findBack key (bimap : IntBimap<'T>) : int =
        // Preconditions
        checkNonNull "bimap" bimap

        Map.find key bimap.Right

    //
    [<CompiledName("Remove")>]
    let remove key (bimap : IntBimap<'T>) : IntBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        // Use the key to find its corresponding value.
        match IntMap.tryFind key bimap.Left with
        | None ->
            // The key doesn't exist, so return the original Bimap.
            bimap
        | Some value ->
            // Remove the values from both maps.
            { bimap with
                Left = IntMap.remove key bimap.Left;
                Right = Map.remove value bimap.Right; }

    //
    [<CompiledName("RemoveBack")>]
    let removeBack key (bimap : IntBimap<'T>) : IntBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        // Use the key to find its corresponding value.
        match Map.tryFind key bimap.Right with
        | None ->
            // The key doesn't exist, so return the original Bimap.
            bimap
        | Some value ->
            // Remove the values from both maps.
            { bimap with
                Left = IntMap.remove value bimap.Left;
                Right = Map.remove key bimap.Right; }

    //
    [<CompiledName("Add")>]
    let add x y (bimap : IntBimap<'T>) : IntBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        // Add the values to both maps.
        // As in Map, we overwrite any existing entry; however, we have to be
        // a bit more thorough here to ensure the invariant is maintained.
        // OPTIMIZE : This could be implemented more efficiently to remove
        // unnecessary or duplicated checks while still maintaining the invariant.
        // It'd also be nice if we could do this in a way that detects if the values
        // are already present and bound to each other, so we don't need to alter the Bimap at all...
        let bimap = remove x bimap
        let bimap = removeBack y bimap
        { bimap with
            Left = IntMap.add x y bimap.Left;
            Right = Map.add y x bimap.Right; }

    //
    [<CompiledName("TryAdd")>]
    let tryAdd x y (bimap : IntBimap<'T>) : IntBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        // Check that neither value is already bound in the Bimap
        // before adding them; if either already belongs to the map
        // return the original Bimap.
        match IntMap.tryFind x bimap.Left, Map.tryFind y bimap.Right with
        | None, None ->
            { bimap with
                Left = IntMap.add x y bimap.Left;
                Right = Map.add y x bimap.Right; }

        | _, _ ->
            // NOTE : We also return the original map when *both* values already
            // belong to the Bimap and are bound to each other -- because adding
            // them again wouldn't have any effect!
            bimap

    //
    [<CompiledName("Iterate")>]
    let iter (action : int -> 'T -> unit) (bimap : IntBimap<'T>) : unit =
        // Preconditions
        checkNonNull "bimap" bimap

        IntMap.iter action bimap.Left

    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> int -> 'T -> 'State)
            (state : 'State) (bimap : IntBimap<'T>) : 'State =
        // Preconditions
        checkNonNull "bimap" bimap

        IntMap.fold folder state bimap.Left

    //
    [<CompiledName("FoldBack")>]
    let foldBack (folder : int -> 'T -> 'State -> 'State)
            (bimap : IntBimap<'T>) (state : 'State) : 'State =
        // Preconditions
        checkNonNull "bimap" bimap

        IntMap.foldBack folder bimap.Left state

    //
    [<CompiledName("Filter")>]
    let filter (predicate : int -> 'T -> bool) (bimap : IntBimap<'T>) : IntBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        let predicate = FSharpFunc<_,_,_>.Adapt predicate

        (bimap, bimap)
        ||> fold (fun bimap x y ->
            if predicate.Invoke (x, y) then
                bimap
            else
                remove x bimap)

    //
    [<CompiledName("Partition")>]
    let partition (predicate : int -> 'T -> bool) (bimap : IntBimap<'T>)
            : IntBimap<'T> * IntBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        let predicate = FSharpFunc<_,_,_>.Adapt predicate

        // Partition efficiently by removing elements from the original map
        // and adding them to a new map when the predicate returns false
        // (instead of creating two new maps).
        ((bimap, empty), bimap)
        ||> fold (fun (trueBimap, falseBimap) x y ->
            if predicate.Invoke (x, y) then
                trueBimap, falseBimap
            else
                remove x trueBimap,
                add x y falseBimap)

    //
    [<CompiledName("OfList")>]
    let ofList list : IntBimap<'T> =
        // Preconditions
        checkNonNull "list" list

        (empty, list)
        ||> List.fold (fun bimap (x, y) ->
            add x y bimap)

    //
    [<CompiledName("ToList")>]
    let toList (bimap : IntBimap<'T>) : _ list =
        // Preconditions
        checkNonNull "bimap" bimap

        (bimap, [])
        ||> foldBack (fun x y list ->
            (x, y) :: list)

    //
    [<CompiledName("OfArray")>]
    let ofArray array : IntBimap<'T> =
        // Preconditions
        checkNonNull "array" array

        (empty, array)
        ||> Array.fold (fun bimap (x, y) ->
            add x y bimap)

    //
    [<CompiledName("ToArray")>]
    let toArray (bimap : IntBimap<'T>) =
        // Preconditions
        checkNonNull "bimap" bimap

        let elements = ResizeArray ()

        bimap
        |> iter (fun x y ->
            elements.Add (x, y))

        ResizeArray.toArray elements

    // TODO
    // ofMap, toMap, tryOfMap


#if PROTO_COMPILER

/// <summary>A bi-directional TagMap.</summary>
/// <typeparam name="Tag">The tag (measure) type for the first set of values.</typeparam>
/// <typeparam name="T">The type of the second set of values.</typeparam>
[<MeasureAnnotatedAbbreviation>]
type TagBimap< [<Measure>] 'Tag, 'T when 'T : comparison > = IntBimap<'T>

#endif

