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
    member __.Paired (x, y) =
        // NOTE : We only need to check one of the maps, because all
        // Bimap functions maintain the invariant.
        match IntMap.tryFind x map with
        | None ->
            false
        | Some value ->
            value = y

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
    static member Singleton (x, y) : IntBimap<'Value> =
        IntBimap (
            IntMap.singleton x y,
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
        match IntMap.tryFind x map, Map.tryFind y inverseMap with
        | None, None ->
            IntBimap (
                IntMap.add x y map,
                Map.add y x inverseMap)

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

        this.Fold ((fun (bimap : IntBimap<_>) x y ->
            if predicate.Invoke (x, y) then
                bimap
            else
                bimap.Remove x), this)

    //
    member this.Partition (predicate : int -> 'Value -> bool) =
        let predicate = FSharpFunc<_,_,_>.Adapt predicate

        // Partition efficiently by removing elements from the original map
        // and adding them to a new map when the predicate returns false
        // (instead of creating two new maps).
        this.Fold ((fun (trueBimap : IntBimap<_>, falseBimap : IntBimap<_>) x y ->
            if predicate.Invoke (x, y) then
                trueBimap, falseBimap
            else
                trueBimap.Remove x,
                falseBimap.Add (x, y)), (this, empty))

    //
    static member OfSeq (sequence : seq<int * 'Value>) : IntBimap<'Value> =
        // Preconditions
        checkNonNull "sequence" sequence

        (empty, sequence)
        ||> Seq.fold (fun bimap (x, y) ->
            bimap.Add (x, y))

    //
    static member OfList list : IntBimap<'Value> =
        // Preconditions
        checkNonNull "list" list

        (empty, list)
        ||> List.fold (fun bimap (x, y) ->
            bimap.Add (x, y))

    //
    static member OfArray array : IntBimap<'Value> =
        // Preconditions
        checkNonNull "array" array

        (empty, array)
        ||> Array.fold (fun bimap (x, y) ->
            bimap.Add (x, y))

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

        this.Iterate <| fun x y ->
            elements.Add (
                KeyValuePair (x, y))

        elements.ToArray ()

    //
    member internal this.RightKvpArray () : KeyValuePair<'Value, int>[] =
        let elements = ResizeArray (1024)

        this.Iterate <| fun x y ->
            elements.Add (
                KeyValuePair (y, x))

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

    //
    [<CompiledName("Singleton")>]
    let inline singleton x y : IntBimap<'T> =
        IntBimap<'T>.Singleton (x, y)

    //
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (bimap : IntBimap<'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.IsEmpty

    //
    [<CompiledName("Count")>]
    let inline count (bimap : IntBimap<'T>) : int =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Count

    //
    [<CompiledName("ContainsKey")>]
    let inline containsKey key (bimap : IntBimap<'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ContainsKey key

    //
    [<CompiledName("ContainsValue")>]
    let inline containsValue value (bimap : IntBimap<'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ContainsValue value

    //
    [<CompiledName("Paired")>]
    let inline paired x y (bimap : IntBimap<'T>) : bool =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Paired (x, y)

    //
    [<CompiledName("TryFind")>]
    let inline tryFind key (bimap : IntBimap<'T>) : 'T option =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryFind key

    //
    [<CompiledName("TryFindValue")>]
    let inline tryFindValue value (bimap : IntBimap<'T>) : int option =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryFindValue value

    //
    [<CompiledName("Find")>]
    let inline find key (bimap : IntBimap<'T>) : 'T =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Find key

    //
    [<CompiledName("FindValue")>]
    let inline findValue value (bimap : IntBimap<'T>) : int =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.FindValue value

    //
    [<CompiledName("Remove")>]
    let inline remove key (bimap : IntBimap<'T>) : IntBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Remove key

    //
    [<CompiledName("RemoveValue")>]
    let inline removeValue key (bimap : IntBimap<'T>) : IntBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.RemoveValue key

    //
    [<CompiledName("Add")>]
    let inline add x y (bimap : IntBimap<'T>) : IntBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Add (x, y)

    //
    [<CompiledName("TryAdd")>]
    let inline tryAdd x y (bimap : IntBimap<'T>) : IntBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryAdd (x, y)

    //
    [<CompiledName("Iter")>]
    let inline iter (action : int -> 'T -> unit) (bimap : IntBimap<'T>) : unit =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Iterate action

    //
    [<CompiledName("Fold")>]
    let inline fold (folder : 'State -> int -> 'T -> 'State)
            (state : 'State) (bimap : IntBimap<'T>) : 'State =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Fold (folder, state)

    //
    [<CompiledName("FoldBack")>]
    let inline foldBack (folder : int -> 'T -> 'State -> 'State)
            (bimap : IntBimap<'T>) (state : 'State) : 'State =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.FoldBack (folder, state)

    //
    [<CompiledName("Filter")>]
    let inline filter (predicate : int -> 'T -> bool) (bimap : IntBimap<'T>) : IntBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Filter predicate

    //
    [<CompiledName("Partition")>]
    let inline partition (predicate : int -> 'T -> bool) (bimap : IntBimap<'T>)
            : IntBimap<'T> * IntBimap<'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Partition predicate

    //
    [<CompiledName("OfSeq")>]
    let inline ofSeq (sequence : seq<int * 'T>) : IntBimap<'T> =
        // Preconditions checked by the member.
        IntBimap<'T>.OfSeq sequence

    //
    [<CompiledName("OfList")>]
    let inline ofList list : IntBimap<'T> =
        // Preconditions checked by the member.
        IntBimap<_>.OfList list

    //
    [<CompiledName("OfArray")>]
    let inline ofArray array : IntBimap<'T> =
        // Preconditions checked by the member.
        IntBimap<_>.OfArray array

    //
    [<CompiledName("ToSeq")>]
    let inline toSeq (bimap : IntBimap<'T>) : seq<int * 'T> =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToSeq ()

    //
    [<CompiledName("ToList")>]
    let inline toList (bimap : IntBimap<'T>) : (int * 'T) list =
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToList ()

    //
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

    //
    [<CompiledName("Singleton")>]
    let inline singleton (x : int<'Tag>) y : TagBimap<'Tag, 'T> =
        IntBimap.Singleton (retype x, y)
        |> retype

    //
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (bimap : TagBimap<'Tag, 'T>) : bool =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.IsEmpty

    //
    [<CompiledName("Count")>]
    let inline count (bimap : TagBimap<'Tag, 'T>) : int =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Count

    //
    [<CompiledName("ContainsKey")>]
    let inline containsKey (key : int<'Tag>) (bimap : TagBimap<'Tag, 'T>) : bool =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap
        
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ContainsKey (retype key)

    //
    [<CompiledName("ContainsValue")>]
    let inline containsValue key (bimap : TagBimap<'Tag, 'T>) : bool =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ContainsValue key

    //
    [<CompiledName("Paired")>]
    let inline paired (x : int<'Tag>) y (bimap : TagBimap<'Tag, 'T>) : bool =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Paired (retype x, y)

    //
    [<CompiledName("TryFind")>]
    let inline tryFind (key : int<'Tag>) (bimap : TagBimap<'Tag, 'T>) : 'T option =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryFind (retype key)

    //
    [<CompiledName("TryFindValue")>]
    let inline tryFindValue key (bimap : TagBimap<'Tag, 'T>) : int<'Tag> option =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryFindValue key
        |> retype

    //
    [<CompiledName("Find")>]
    let inline find (key : int<'Tag>) (bimap : TagBimap<'Tag, 'T>) : 'T =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap
        
        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Find (retype key)

    //
    [<CompiledName("FindValue")>]
    let inline findValue key (bimap : TagBimap<'Tag, 'T>) : int<'Tag> =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.FindValue key
        |> retype

    //
    [<CompiledName("Remove")>]
    let inline remove (key : int<'Tag>) (bimap : TagBimap<'Tag, 'T>) : TagBimap<'Tag, 'T> =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Remove (retype key)
        |> retype

    //
    [<CompiledName("RemoveValue")>]
    let inline removeValue key (bimap : TagBimap<'Tag, 'T>) : TagBimap<'Tag, 'T> =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.RemoveValue key
        |> retype

    //
    [<CompiledName("Add")>]
    let inline add (x : int<'Tag>) y (bimap : TagBimap<'Tag, 'T>) : TagBimap<'Tag, 'T> =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Add (retype x, y)
        |> retype

    //
    [<CompiledName("TryAdd")>]
    let inline tryAdd (x : int<'Tag>) y (bimap : TagBimap<'Tag, 'T>) : TagBimap<'Tag, 'T> =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.TryAdd (retype x, y)
        |> retype

    //
    [<CompiledName("Iter")>]
    let inline iter (action : int<'Tag> -> 'T -> unit) (bimap : TagBimap<'Tag, 'T>) : unit =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Iterate (retype action)

    //
    [<CompiledName("Fold")>]
    let inline fold (folder : 'State -> int<'Tag> -> 'T -> 'State)
            (state : 'State) (bimap : TagBimap<'Tag, 'T>) : 'State =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Fold (retype folder, state)

    //
    [<CompiledName("FoldBack")>]
    let inline foldBack (folder : int<'Tag> -> 'T -> 'State -> 'State)
            (bimap : TagBimap<'Tag, 'T>) (state : 'State) : 'State =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.FoldBack (retype folder, state)

    //
    [<CompiledName("Filter")>]
    let inline filter (predicate : int<'Tag> -> 'T -> bool) (bimap : TagBimap<'Tag, 'T>) : TagBimap<'Tag, 'T> =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.Filter (retype predicate)
        |> retype

    //
    [<CompiledName("Partition")>]
    let inline partition (predicate : int<'Tag> -> 'T -> bool) (bimap : TagBimap<'Tag, 'T>)
            : TagBimap<'Tag, 'T> * TagBimap<'Tag, 'T> =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        let trueMap, falseMap = bimap.Partition (retype predicate)
        (retype trueMap), (retype falseMap)

    //
    [<CompiledName("OfList")>]
    let inline ofList (list : (int<'Tag> * 'T) list) : TagBimap<'Tag, 'T> =
        // Preconditions
        checkNonNull "list" list

        IntBimap<_>.OfList (retype list)
        |> retype

    //
    [<CompiledName("ToList")>]
    let inline toList (bimap : TagBimap<'Tag, 'T>) : (int<'Tag> * 'T) list =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToList ()
        |> retype

    //
    [<CompiledName("OfArray")>]
    let inline ofArray (array : (int<'Tag> * 'T)[]) : TagBimap<'Tag, 'T> =
        // Preconditions
        checkNonNull "array" array

        IntBimap<_>.OfArray (retype array)
        |> retype

    //
    [<CompiledName("ToArray")>]
    let inline toArray (bimap : TagBimap<'Tag, 'T>) : (int<'Tag> * 'T)[] =
        // Retype as IntBimap.
        let bimap : IntBimap<'T> = retype bimap

        // Preconditions
        checkNonNull "bimap" bimap

        bimap.ToArray ()
        |> retype

#endif
