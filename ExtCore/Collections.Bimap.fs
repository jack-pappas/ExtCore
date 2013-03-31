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
/// <typeparam name="T1">The first key type.</typeparam>
/// <typeparam name="T2">The second key type.</typeparam>
[<Sealed>]
[<NoEquality; NoComparison>]
[<DebuggerTypeProxy(typedefof<BimapDebuggerProxy<int,int>>)>]
[<DebuggerDisplay("Count = {Count}")>]
type Bimap<'T1, 'T2 when 'T1 : comparison and 'T2 : comparison>
    private (left : Map<'T1, 'T2>, right : Map<'T2, 'T1>) =
    
    /// The empty Bimap instance.
    static let empty = Bimap (Map.empty, Map.empty)
    
    /// The empty Bimap.
    static member Empty
        with get () = empty

    /// The number of items in the Bimap.
    member __.Count
        with get () =
            Map.count left

    /// Is the Bimap empty?
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
    member __.Iterate (action : 'T1 -> 'T2 -> unit) : unit =
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

        this.Iterate <| fun x y ->
            elements.Add (x, y)

        ResizeArray.toArray elements

    //
    member internal this.LeftKvpArray () : KeyValuePair<'T1, 'T2>[] =
        let elements = ResizeArray (1024)

        this.Iterate <| fun x y ->
            elements.Add (
                KeyValuePair (x, y))

        elements.ToArray ()

    //
    member internal this.RightKvpArray () : KeyValuePair<'T2, 'T1>[] =
        let elements = ResizeArray (1024)

        this.Iterate <| fun x y ->
            elements.Add (
                KeyValuePair (y, x))

        elements.ToArray ()

/// Internal. Debugger proxy type for Bimap.
and [<Sealed>]
    internal BimapDebuggerProxy<'T1, 'T2
        when 'T1 : comparison and 'T2 : comparison> (bimap : Bimap<'T1, 'T2>) =

    [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
    member __.Left
        with get () : KeyValuePair<'T1, 'T2>[] =
            bimap.LeftKvpArray ()

    [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
    member __.Right
        with get () : KeyValuePair<'T2, 'T1>[] =
            bimap.RightKvpArray ()

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
    [<CompiledName("Iter")>]
    let inline iter (action : 'T1 -> 'T2 -> unit) (bimap : Bimap<'T1, 'T2>) : unit =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Iterate action

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
