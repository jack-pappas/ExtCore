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

open LanguagePrimitives
open OptimizedClosures
open ExtCore


/// A bi-directional map.
type Bimap<'T1, 'T2
    when 'T1 : comparison
    and 'T2 : comparison> = {
    //
    Left : Map<'T1, 'T2>;
    //
    Right : Map<'T2, 'T1>;
}

//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Bimap =
    /// The empty Bimap.
    let empty<'T1, 'T2
        when 'T1 : comparison
        and 'T2 : comparison> : Bimap<'T1, 'T2> =
        { Left = Map.empty; Right = Map.empty; }

    //
    [<CompiledName("Singleton")>]
    let singleton x y : Bimap<'T1, 'T2> =
        { Left = Map.singleton x y;
          Right = Map.singleton y x; }

    //
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (bimap : Bimap<'T1, 'T2>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        Map.isEmpty bimap.Left

    //
    [<CompiledName("Count")>]
    let inline count (bimap : Bimap<'T1, 'T2>) : int =
        // Preconditions
        checkNonNull "bimap" bimap

        Map.count bimap.Left

    //
    [<CompiledName("ContainsKey")>]
    let inline containsKey key (bimap : Bimap<'T1, 'T2>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        Map.containsKey key bimap.Left

    //
    [<CompiledName("ContainsKeyBack")>]
    let inline containsKeyBack key (bimap : Bimap<'T1, 'T2>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        Map.containsKey key bimap.Right

    //
    [<CompiledName("Paired")>]
    let paired x y (bimap : Bimap<'T1, 'T2>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        // NOTE : We only need to check one of the maps, because all
        // Bimap functions maintain the invariant.
        match Map.tryFind x bimap.Left with
        | None ->
            false
        | Some value ->
            value = y

    //
    [<CompiledName("TryFind")>]
    let tryFind key (bimap : Bimap<'T1, 'T2>) : 'T2 option =
        // Preconditions
        checkNonNull "bimap" bimap

        Map.tryFind key bimap.Left

    //
    [<CompiledName("TryFindBack")>]
    let tryFindBack key (bimap : Bimap<'T1, 'T2>) : 'T1 option =
        // Preconditions
        checkNonNull "bimap" bimap

        Map.tryFind key bimap.Right

    //
    [<CompiledName("Find")>]
    let find key (bimap : Bimap<'T1, 'T2>) : 'T2 =
        // Preconditions
        checkNonNull "bimap" bimap

        Map.find key bimap.Left

    //
    [<CompiledName("FindBack")>]
    let findBack key (bimap : Bimap<'T1, 'T2>) : 'T1 =
        // Preconditions
        checkNonNull "bimap" bimap

        Map.find key bimap.Right

    //
    [<CompiledName("Remove")>]
    let remove key (bimap : Bimap<'T1, 'T2>) : Bimap<'T1, 'T2> =
        // Preconditions
        checkNonNull "bimap" bimap

        // Use the key to find its corresponding value.
        match Map.tryFind key bimap.Left with
        | None ->
            // The key doesn't exist, so return the original Bimap.
            bimap
        | Some value ->
            // Remove the values from both maps.
            { bimap with
                Left = Map.remove key bimap.Left;
                Right = Map.remove value bimap.Right; }

    //
    [<CompiledName("RemoveBack")>]
    let removeBack key (bimap : Bimap<'T1, 'T2>) : Bimap<'T1, 'T2> =
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
                Left = Map.remove value bimap.Left;
                Right = Map.remove key bimap.Right; }

    //
    [<CompiledName("Add")>]
    let add x y (bimap : Bimap<'T1, 'T2>) : Bimap<'T1, 'T2> =
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
            Left = Map.add x y bimap.Left;
            Right = Map.add y x bimap.Right; }

    //
    [<CompiledName("TryAdd")>]
    let tryAdd x y (bimap : Bimap<'T1, 'T2>) : Bimap<'T1, 'T2> =
        // Preconditions
        checkNonNull "bimap" bimap

        // Check that neither value is already bound in the Bimap
        // before adding them; if either already belongs to the map
        // return the original Bimap.
        match Map.tryFind x bimap.Left, Map.tryFind y bimap.Right with
        | None, None ->
            { bimap with
                Left = Map.add x y bimap.Left;
                Right = Map.add y x bimap.Right; }

        | _, _ ->
            // NOTE : We also return the original map when *both* values already
            // belong to the Bimap and are bound to each other -- because adding
            // them again wouldn't have any effect!
            bimap

    //
    [<CompiledName("Iterate")>]
    let iter (action : 'T1 -> 'T2 -> unit) (bimap : Bimap<'T1, 'T2>) : unit =
        // Preconditions
        checkNonNull "bimap" bimap

        Map.iter action bimap.Left

    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T1 -> 'T2 -> 'State)
            (state : 'State) (bimap : Bimap<'T1, 'T2>) : 'State =
        // Preconditions
        checkNonNull "bimap" bimap

        Map.fold folder state bimap.Left

    //
    [<CompiledName("FoldBack")>]
    let foldBack (folder : 'T1 -> 'T2 -> 'State -> 'State)
            (bimap : Bimap<'T1, 'T2>) (state : 'State) : 'State =
        // Preconditions
        checkNonNull "bimap" bimap

        Map.foldBack folder bimap.Left state

    //
    [<CompiledName("Filter")>]
    let filter (predicate : 'T1 -> 'T2 -> bool) (bimap : Bimap<'T1, 'T2>) : Bimap<'T1, 'T2> =
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
    let partition (predicate : 'T1 -> 'T2 -> bool) (bimap : Bimap<'T1, 'T2>)
            : Bimap<'T1, 'T2> * Bimap<'T1, 'T2> =
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
    let ofList list : Bimap<'T1, 'T2> =
        // Preconditions
        checkNonNull "list" list

        (empty, list)
        ||> List.fold (fun bimap (x, y) ->
            add x y bimap)

    //
    [<CompiledName("ToList")>]
    let toList (bimap : Bimap<'T1, 'T2>) : _ list =
        // Preconditions
        checkNonNull "bimap" bimap

        (bimap, [])
        ||> foldBack (fun x y list ->
            (x, y) :: list)

    //
    [<CompiledName("OfArray")>]
    let ofArray array : Bimap<'T1, 'T2> =
        // Preconditions
        checkNonNull "array" array

        (empty, array)
        ||> Array.fold (fun bimap (x, y) ->
            add x y bimap)

    //
    [<CompiledName("ToArray")>]
    let toArray (bimap : Bimap<'T1, 'T2>) =
        // Preconditions
        checkNonNull "bimap" bimap

        let elements = ResizeArray ()

        bimap
        |> iter (fun x y ->
            elements.Add (x, y))

        ResizeArray.toArray elements

    // TODO
    // ofMap, toMap, tryOfMap


/// <summary>A bi-directional TagMap.</summary>
/// <typeparam name="Tag1">The tag (measure) type for the first set of values.</typeparam>
/// <typeparam name="Tag2">The tag (measure) type for the second set of values.</typeparam>
type TagBimap< [<Measure>] 'Tag1, [<Measure>] 'Tag2 > = {
    //
    Left : Map<int<'Tag1>, int<'Tag2>>;
    //
    Right : Map<int<'Tag2>, int<'Tag1>>;
}

