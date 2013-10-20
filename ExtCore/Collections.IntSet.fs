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


/// Constants used to tune certain operations on Patricia tries.
module internal PatriciaTrieConstants =
    /// The default capacity used to when creating mutable stacks
    /// within the optimized traversal methods for Patricia tries.
    // TODO : Run some experiments to determine if this is a good initial capacity,
    // or if some other value would provide better average-case performance.
    let [<Literal>] defaultTraversalStackSize = 16


type internal Mask32 = uint32
type internal Key32 = uint32
type internal Prefix32 = uint32

/// Bitwise operations (32-bit) necessary for implementing Patricia tries.
module internal BitOps32 =
    #if LITTLE_ENDIAN_TRIES
    
    //
    let inline private leastSignificantSetBit (x : uint32) : uint32 =
        x &&& (uint32 -(int x))

    /// Finds the last (least-significant) bit at which p0 and p1 disagree.
    /// Returns a power-of-two value containing this (and only this) bit.
    let inline branchingBit (p0 : Prefix32, p1 : Prefix32) : Mask32 =
        leastSignificantSetBit (p0 ^^^ p1)

    /// Clears the indicated bit and sets all lower bits.
    let inline mask (key : Key32, mask : Mask32) : Prefix32 =
        key &&& (mask - 1u)

    //
    let (*inline*) shorter (m1 : Mask32, m2 : Mask32) : bool =
        notImpl "BitOps.shorter (Little-Endian)"

    #else
    
    // http://aggregate.org/MAGIC/#Most%20Significant%201%20Bit
    // OPTIMIZE : This could be even faster if we could take advantage of a built-in
    // CPU instruction here (such as 'bsr', 'ffs', or 'clz').
    // Could we expose these instructions through Mono?
    let inline private mostSignificantSetBit (x1 : uint32) =
        let x2 = x1 ||| (x1 >>> 1)
        let x3 = x2 ||| (x2 >>> 2)
        let x4 = x3 ||| (x3 >>> 4)
        let x5 = x4 ||| (x4 >>> 8)
        let x6 = x5 ||| (x5 >>> 16)
        // OPTIMIZATION : p AND (NOT q) <-> p XOR q
        x6 ^^^ (x6 >>> 1)

    /// Finds the first (most-significant) bit at which p0 and p1 disagree.
    /// Returns a power-of-two value containing this (and only this) bit.
    let inline branchingBit (p0 : Prefix32, p1 : Prefix32) : Mask32 =
        mostSignificantSetBit (p0 ^^^ p1)

    /// Clears the indicated bit and sets all lower bits.
    let inline mask (key : Key32, mask : Mask32) : Prefix32 =
        key &&& (~~~(mask - 1u) ^^^ mask)

    //
    let inline shorter (m1 : Mask32, m2 : Mask32) : bool =
        // NOTE : This must be an *unsigned* comparison for the results to be correct.
        m1 > m2

    #endif

    //
    let inline matchPrefix (key : Key32, prefix : Prefix32, mask' : Mask32) : bool =
        mask (key, mask') = prefix

    /// <summary>Determines if all specified bits are cleared (not set) in a value.</summary>
    /// <param name="value">The value to test.</param>
    /// <param name="bitValue">The bits to test in 'value'.</param>
    /// <returns>true if all bits which are set in 'bitValue' are *not* set in 'value'.</returns>
    let inline zeroBit (key : Key32, mask : Mask32) : bool =
        key &&& mask = 0u


open PatriciaTrieConstants
open BitOps32


/// A Patricia trie implementation.
/// Used as the underlying data structure for IntSet (and TagSet).
[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type private PatriciaSet32 =
    | Empty
    // Key
    | Lf of Key32
    // Prefix * Mask * Left-Child * Right-Child
    | Br of Prefix32 * Mask32 * PatriciaSet32 * PatriciaSet32

    /// Determines if the set contains the given value.
    static member Contains (key, set : PatriciaSet32) =
        match set with
        | Empty ->
            false
        | Lf j ->
            key = j
        | Br (_, m, t0, t1) ->
            PatriciaSet32.Contains
                (key, (if zeroBit (key, m) then t0 else t1))

    /// The number of items (i.e., the cardinality) of the set.
    static member Count (set : PatriciaSet32) : int =
        match set with
        | Empty -> 0
        | Lf _ -> 1
        | Br (_, _, left, right) ->
            // Count the number of items in the left and right subtrees.
            PatriciaSet32.Count left + PatriciaSet32.Count right

    /// Retrieve the minimum element of the set.
    static member MinElement (set : PatriciaSet32) =
        match set with
        | Empty ->
            invalidArg "set" "The set is empty."
        | Lf j -> j
        | Br (_, _, t0, _) ->
            PatriciaSet32.MinElement t0

    /// Retrieve the minimum element of the set, treating the
    /// elements of the set as signed values.
    static member MinElementSigned (set : PatriciaSet32) =
        match set with
        | Empty ->
            invalidArg "set" "The set is empty."
        | Lf j -> j
        | Br (_, m, t0, t1) ->
            // OPTIMIZE : Just use a bitmask here to check the top bit?
            if (int m) < 0 then t1 else t0
            |> PatriciaSet32.MinElement

    /// Retrieve the maximum element of the set.
    static member MaxElement (set : PatriciaSet32) =
        match set with
        | Empty ->
            invalidArg "set" "The set is empty."
        | Lf j -> j
        | Br (_, _, _, t1) ->
            PatriciaSet32.MaxElement t1

    /// Retrieve the maximum element of the set, treating the
    /// elements of the set as signed values.
    static member MaxElementSigned (set : PatriciaSet32) =
        match set with
        | Empty ->
            invalidArg "set" "The set is empty."
        | Lf j -> j
        | Br (_, m, t0, t1) ->
            // OPTIMIZE : Just use a bitmask here to check the top bit?
            if (int m) < 0 then t0 else t1
            |> PatriciaSet32.MaxElement

    /// Remove an item from the set.
    static member Remove (key, set : PatriciaSet32) =
        match set with
        | Empty ->
            Empty
        | Lf j ->
            if j = key then Empty
            else set
        
        | Br (p, m, t0, t1) ->
            if matchPrefix (key, p, m) then
                if zeroBit (key, m) then
                    match PatriciaSet32.Remove (key, t0) with
                    | Empty -> t1
                    | left ->
                        // Only create a new tree when the value was actually removed
                        // (i.e., the tree was modified).
                        if left == t0 then set
                        else Br (p, m, left, t1)
                else
                    match PatriciaSet32.Remove (key, t1) with
                    | Empty -> t0
                    | right ->
                        // Only create a new tree when the value was actually removed
                        // (i.e., the tree was modified).
                        if right == t1 then set
                        else Br (p, m, t0, right)
            else set

    //
    static member inline private Join (p0, t0 : PatriciaSet32, p1, t1) =
        let m = branchingBit (p0, p1)
        let p = mask (p0, m)
        if zeroBit (p0, m) then
            Br (p, m, t0, t1)
        else
            Br (p, m, t1, t0)

    /// Insert a binding (key-value pair) into a set. returning a new, updated set.
    static member Add (key, set) =
        match set with
        | Empty ->
            Lf key
        | Lf j ->
            if j = key then
                // The value already exists in the set, so return the
                // existing set without modifying it.
                set
            else
                PatriciaSet32.Join (key, Lf key, j, set)

        | Br (p, m, t0, t1) ->
            if matchPrefix (key, p, m) then
                if zeroBit (key, m) then
                    let left = PatriciaSet32.Add (key, t0)

                    // Only create a new tree when the value was actually added
                    // (i.e., the tree was modified).
                    if left == t0 then set
                    else Br (p, m, left, t1)
                else
                    let right = PatriciaSet32.Add (key, t1)

                    // Only create a new tree when the value was actually added
                    // (i.e., the tree was modified).
                    if right == t1 then set
                    else Br (p, m, t0, right)
            else
                PatriciaSet32.Join (key, Lf key, p, set)

    /// Computes the union of two PatriciaSet32s.
    static member Union (s, t) : PatriciaSet32 =
        // If the sets are identical, return immediately.
        if s == t then s else
        match s, t with
        | Br (p, m, s0, s1), Br (q, n, t0, t1) ->
            if m = n then
                if p = q then
                    // The trees have the same prefix. Merge the subtrees.
                    let left = PatriciaSet32.Union (s0, t0)
                    let right = PatriciaSet32.Union (s1, t1)
                    
                    // Only create a new tree if some values were actually added
                    // (i.e., the tree was modified).
                    if left == s0 && right == s1 then s
                    elif left == t0 && right == t1 then t
                    else Br (p, m, left, right)
                else
                    // The prefixes disagree.
                    PatriciaSet32.Join (p, s, q, t)
            
            #if LITTLE_ENDIAN_TRIES
            elif m < n then
            #else
            elif m > n then
            #endif
                if matchPrefix (q, p, m) then
                    // q contains p. Merge t with a subtree of s.
                    if zeroBit (q, m) then
                        let left = PatriciaSet32.Union (s0, t)
                        
                        // Only create a new tree when the subtree is actually modified.
                        if left == s0 then s
                        else Br (p, m, left, s1)
                    else
                        let right = PatriciaSet32.Union (s1, t)

                        // Only create a new tree when the subtree is actually modified.
                        if right == s1 then s
                        else Br (p, m, s0, right)
                else
                    // The prefixes disagree.
                    PatriciaSet32.Join (p, s, q, t)

            else
                if matchPrefix (p, q, n) then
                    // p contains q. Merge s with a subtree of t.
                    if zeroBit (p, n) then
                        let left = PatriciaSet32.Union (s, t0)
                        
                        // Only create a new tree when the subtree is actually modified.
                        if left == t0 then t
                        else Br (q, n, left, t1)
                    else
                        let right = PatriciaSet32.Union (s, t1)
                        
                        // Only create a new tree when the subtree is actually modified.
                        if right == t1 then t
                        else Br (q, n, t0, right)
                else
                    // The prefixes disagree.
                    PatriciaSet32.Join (p, s, q, t)

        | Br (p, m, s0, s1), Lf k ->
            if matchPrefix (k, p, m) then
                if zeroBit (k, m) then
                    let left = PatriciaSet32.Add (k, s0)
                    
                    // Only create a new tree when the subtree is actually modified.
                    if left == s0 then s
                    else Br (p, m, left, s1)
                else
                    let right = PatriciaSet32.Add (k, s1)
                    
                    // Only create a new tree when the subtree is actually modified.
                    if right == s1 then s
                    else Br (p, m, s0, right)
            else
                PatriciaSet32.Join (k, t, p, s)

        | Br (_,_,_,_), Empty ->
            s
        | Lf k, t ->
            PatriciaSet32.Add (k, t)
        | Empty, t ->
            t

    /// Compute the intersection of two PatriciaSet32s.
    static member Intersect (s, t) : PatriciaSet32 =
        // If the sets are identical, return immediately.
        if s == t then s else
        match s, t with
        | Br (p, m, s0, s1), Br (q, n, t0, t1) ->
            if m = n then
                if p <> q then Empty
                else
                    let left = PatriciaSet32.Intersect (s0, t0)
                    let right = PatriciaSet32.Intersect (s1, t1)
                    match left, right with
                    | Empty, r
                    | r, Empty -> r
                    | left, right ->
                        // Only create a new tree if some values were actually removed
                        // (i.e., the tree was modified).
                        if left == s0 && right == s1 then s
                        elif left == t0 && right == t1 then t
                        else Br (p, m, left, right)

            #if LITTLE_ENDIAN_TRIES
            elif m < n then
            #else
            elif m > n then
            #endif
                if matchPrefix (q, p, m) then
                    if zeroBit (q, m) then
                        PatriciaSet32.Intersect (s0, t)
                    else
                        PatriciaSet32.Intersect (s1, t)
                else
                    Empty

            else
                if matchPrefix (p, q, n) then
                    if zeroBit (p, n) then
                        PatriciaSet32.Intersect (s, t0)
                    else
                        PatriciaSet32.Intersect (s, t1)
                else
                    Empty

        | Br (_, m, s0, s1), Lf k ->
            let s' = if zeroBit (k, m) then s0 else s1
            if PatriciaSet32.Contains (k, s') then t
            else Empty
            
        | Br (_,_,_,_), Empty ->
            Empty
            
        | Lf k, t ->
            if PatriciaSet32.Contains (k, t) then s
            else Empty

        | Empty, _ ->
            Empty

    /// Compute the difference of two PatriciaSet32s.
    static member Difference (s, t) : PatriciaSet32 =
        // If the sets are identical, return immediately.
        if s == t then Empty else
        match s, t with
        | Br (p, m, s0, s1), Br (q, n, t0, t1) ->
            if m = n then
                if p <> q then s
                else
                    let left = PatriciaSet32.Difference (s0, t0)
                    let right = PatriciaSet32.Difference (s1, t1)
                    match left, right with
                    | Empty, r
                    | r, Empty -> r
                    | left, right ->
                        // Only create a new tree if some values were actually removed
                        // (i.e., the tree was modified).
                        if left == s0 && right == s1 then s
                        else Br (p, m, left, right)

            #if LITTLE_ENDIAN_TRIES
            elif m < n then
            #else
            elif m > n then
            #endif
                if matchPrefix (q, p, m) then
                    if zeroBit (q, m) then
                        match PatriciaSet32.Difference (s0, t) with
                        | Empty -> s1
                        | left ->
                            // Only create a new tree some values were actually removed
                            // (i.e., the tree was modified).
                            if left == s0 then s
                            else Br (p, m, left, s1)
                    else
                        match PatriciaSet32.Difference (s1, t) with
                        | Empty -> s0
                        | right ->
                            // Only create a new tree some values were actually removed
                            // (i.e., the tree was modified).
                            if right == s1 then s
                            else Br (p, m, s0, right)
                else s

            else
                if matchPrefix (p, q, n) then
                    if zeroBit (p, n) then
                        PatriciaSet32.Difference (s, t0)
                    else
                        PatriciaSet32.Difference (s, t1)
                else s

        | Br (p, m, s0, s1), Lf k ->
            if matchPrefix (k, p, m) then
                if zeroBit (k, m) then
                    match PatriciaSet32.Remove (k, s0) with
                    | Empty -> s1
                    | left ->
                        match s1 with
                        | Empty -> left
                        | _ ->
                            // Only create a new tree if the value was actually removed
                            // (i.e., the tree was modified).
                            if left == s0 then s
                            else Br (p, m, left, s1)
                else
                    match PatriciaSet32.Remove (k, s1) with
                    | Empty -> s0
                    | right ->
                        match s0 with
                        | Empty -> right
                        | _ ->
                            // Only create a new tree if the value was actually removed
                            // (i.e., the tree was modified).
                            if right == s1 then s
                            else Br (p, m, s0, right)
            else s
            
        | Br (_,_,_,_), Empty ->
            s
        | Lf k, t ->
            if PatriciaSet32.Contains (k, t) then Empty
            else s
        | Empty, _ ->
            Empty

    /// Computes the containment ordering for two sets (i.e., determines if one set includes the other).
    // Return values:
    //  -1 : All values in 't1' are in 't2', and at least one value of 't2' is not in 't1'.
    //   0 : The sets contain _exactly_ the same values.
    //   1 : The sets are disjoint, i.e., 't1' contains at least one value which is not in 't2'.
    static member private SubsetCompare (t1 : PatriciaSet32, t2 : PatriciaSet32) : int =
        match t1, t2 with
        | Br (p1, m1, l1, r1), Br (p2, m2, l2, r2) ->
            if shorter (m1, m2) then 1
            elif shorter (m2, m1) then
                if not <| matchPrefix (p1, p2, m2) then 1
                elif zeroBit (p1, m2) then
                    match PatriciaSet32.SubsetCompare (t1, l2) with 1 -> 1 | _ -> -1
                else
                    match PatriciaSet32.SubsetCompare (t1, r2) with 1 -> 1 | _ -> -1
            elif p1 = p2 then
                let left = PatriciaSet32.SubsetCompare (l1, l2)
                let right = PatriciaSet32.SubsetCompare (r1, r2)
                match left, right with
                | 1, _
                | _, 1 -> 1
                | 0, 0 -> 0
                | _ -> -1
            else
                // The maps are disjoint.
                1

        | Br (_,_,_,_), (Empty | Lf _) -> 1
        | Lf x, Lf y ->
            if x = y then 0
            else 1

        | Lf x, Br (p, m, l, r) ->
            if not <| matchPrefix (x, p, m) then 1
            elif zeroBit (x, m) then
                match PatriciaSet32.SubsetCompare (t1, l) with 1 -> 1 | _ -> -1
            else
                match PatriciaSet32.SubsetCompare (t1, r) with 1 -> 1 | _ -> -1

        | Lf _, Empty ->
            // The maps are disjoint.
            1

        | Empty, Empty -> 0
        | Empty, _ -> -1

    /// Is 'set1' a proper subset of 'set2'?
    /// IsProperSubset (set1, set2) returns true if all keys in set1 are in set2,
    /// and at least one element in set2 is not in set1.
    static member IsProperSubsetOf (set1 : PatriciaSet32, set2 : PatriciaSet32) : bool =
        match PatriciaSet32.SubsetCompare (set1, set2) with
        | -1 -> true
        | _ -> false

    /// Is 'set1' a subset of 'set2'?
    /// IsSubset (set1, set2) returns true if all keys in set1 are in set2.
    static member IsSubsetOf (set1 : PatriciaSet32, set2 : PatriciaSet32) : bool =
        match PatriciaSet32.SubsetCompare (set1, set2) with
        | -1 | 0 -> true
        | _ -> false

    //
    static member OfSeq (source : seq<int>) : PatriciaSet32 =
        (Empty, source)
        ||> Seq.fold (fun trie el ->
            PatriciaSet32.Add (uint32 el, trie))

    //
    static member OfList (source : int list) : PatriciaSet32 =
        // Preconditions
        checkNonNull "source" source

        (Empty, source)
        ||> List.fold (fun trie el ->
            PatriciaSet32.Add (uint32 el, trie))

    //
    static member OfArray (source : int[]) : PatriciaSet32 =
        // Preconditions
        checkNonNull "source" source

        (Empty, source)
        ||> Array.fold (fun trie el ->
            PatriciaSet32.Add (uint32 el, trie))

    //
    static member OfSet (source : Set<int>) : PatriciaSet32 =
        // Preconditions
        checkNonNull "source" source

        (Empty, source)
        ||> Set.fold (fun trie el ->
            PatriciaSet32.Add (uint32 el, trie))

    //
    static member Iterate (action : int -> unit, set) : unit =
        match set with
        | Empty -> ()
        | Lf k ->
            action (int k)
        | Br (_, _, left, right) ->
            // Iterate over the left and right subtrees.
            PatriciaSet32.Iterate (action, left)
            PatriciaSet32.Iterate (action, right)

    //
    static member IterateBack (action : int -> unit, set) : unit =
        match set with
        | Empty -> ()
        | Lf k ->
            action (int k)
        | Br (_, _, left, right) ->
            // Iterate over the right and left subtrees.
            PatriciaSet32.Iterate (action, right)
            PatriciaSet32.Iterate (action, left)

    //
    static member Fold (folder : FSharpFunc<'State, int, 'State>, state : 'State, set) : 'State =
        match set with
        | Empty ->
            state
        | Lf k ->
            folder.Invoke (state, int k)
        | Br (_, _, left, right) ->
            // Fold over the left subtree, then the right subtree.
            let state = PatriciaSet32.Fold (folder, state, left)
            PatriciaSet32.Fold (folder, state, right)

    //
    static member FoldBack (folder : FSharpFunc<int, 'State, 'State>, state : 'State, set) : 'State =
        match set with
        | Empty ->
            state
        | Lf k ->
            folder.Invoke (int k, state)
        | Br (_, _, left, right) ->
            // Fold over the right subtree, then the left subtree.
            let state = PatriciaSet32.FoldBack (folder, state, right)
            PatriciaSet32.FoldBack (folder, state, left)

    //
    static member TryPick (picker : int -> 'T option, set) : 'T option =
        match set with
        | Empty ->
            None
        | Lf k ->
            picker (int k)
        | Br (_, _, left, right) ->
            // Visit the left subtree, then the right subtree if necessary.
            match PatriciaSet32.TryPick (picker, left) with
            | None ->
                PatriciaSet32.TryPick (picker, right)
            | res ->
                res

    //
    static member TryFind (predicate : int -> bool, set) : int option =
        match set with
        | Empty ->
            None
        | Lf k ->
            if predicate (int k) then Some (int k) else None
        | Br (_, _, left, right) ->
            // Visit the left subtree, then the right subtree if necessary.
            match PatriciaSet32.TryFind (predicate, left) with
            | None ->
                PatriciaSet32.TryFind (predicate, right)
            | res ->
                res

    //
    static member ToSeq set =
        seq {
        match set with
        | Empty -> ()
        | Lf k ->
            yield int k
        | Br (_, _, left, right) ->
            // Recursively visit the children.
            yield! PatriciaSet32.ToSeq left
            yield! PatriciaSet32.ToSeq right
        }

//
[<Sealed; CompiledName("FSharpIntSet")>]
//[<StructuredFormatDisplay("")>]
[<DebuggerDisplay("Count = {Count}")>]
[<DebuggerTypeProxy(typedefof<IntSetDebuggerProxy>)>]
type IntSet private (trie : PatriciaSet32) =
    /// The empty IntSet instance.
    static let empty = IntSet Empty

    /// The empty IntSet.
    static member Empty
        with get () = empty

    //
    new (elements : seq<int>) =
        // Preconditions
        checkNonNull "elements" elements

        // OPTIMIZE : Try to cast the sequence to array or list;
        // if it succeeds use the specialized method for that type for better performance.
        IntSet (PatriciaSet32.OfSeq elements)

    //
    member private __.Trie
        with get () = trie

    /// The number of elements in the IntSet.
    member __.Count
        with get () : int =
            PatriciaSet32.Count trie

    /// Is the map empty?
    member __.IsEmpty
        with get () =
            match trie with
            | Empty -> true
            | _ -> false

    /// Tests if an element is in the domain of the IntSet.
    member __.Contains (key : int) : bool =
        PatriciaSet32.Contains (uint32 key, trie)

    /// The minimum unsigned value stored in the set.
#if FX_NO_DEBUG_DISPLAYS
#else
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
#endif
    member __.MinimumElement
        with get () : int =
            int <| PatriciaSet32.MinElement trie

    /// The minimum signed value stored in the set.
#if FX_NO_DEBUG_DISPLAYS
#else
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
#endif
    member __.MinimumElementSigned
        with get () : int =
            int <| PatriciaSet32.MinElementSigned trie

    /// The maximum unsigned value stored in the set.
#if FX_NO_DEBUG_DISPLAYS
#else
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
#endif
    member __.MaximumElement
        with get () : int =
            int <| PatriciaSet32.MaxElement trie

    /// The maximum signed value stored in the set.
#if FX_NO_DEBUG_DISPLAYS
#else
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
#endif
    member __.MaximumElementSigned
        with get () : int =
            int <| PatriciaSet32.MaxElementSigned trie

    /// The IntSet containing the given element.
    static member Singleton (element : int) : IntSet =
        IntSet (Lf <| uint32 element)

    /// Returns a new IntSet with the element added to this IntSet.
    member this.Add (key : int) : IntSet =
        // If the trie isn't modified, just return this IntSet instead of creating a new one.
        let trie' = PatriciaSet32.Add (uint32 key, trie)
        if trie == trie' then this
        else IntSet (trie')

    /// Removes an element from the domain of the IntSet.
    /// No exception is raised if the element is not present.
    member this.Remove (key : int) : IntSet =
        // If the trie isn't modified, just return this IntSet instead of creating a new one.
        let trie' = PatriciaSet32.Remove (uint32 key, trie)
        if trie == trie' then this
        else IntSet (trie')

    /// Computes the union of two IntSets.
    member this.Union (otherSet : IntSet) : IntSet =
        // If this set's trie is the same as the other set's trie, we can return immediately.
        if trie == otherSet.Trie then this
        else
            // If the result is the same (physical equality) to one of the inputs,
            // return that input instead of creating a new IntSet.
            let trie' = PatriciaSet32.Union (trie, otherSet.Trie)
            if trie == trie' then this
            elif otherSet.Trie == trie' then otherSet
            else IntSet (trie')

    /// Computes the intersection of two IntSets.
    member this.Intersect (otherSet : IntSet) : IntSet =
        // If this set's trie is the same as the other set's trie, we can return immediately.
        if trie == otherSet.Trie then this
        else
            // If the result is the same (physical equality) to one of the inputs,
            // return that input instead of creating a new IntSet.
            let trie' = PatriciaSet32.Intersect (trie, otherSet.Trie)
            if trie == trie' then this
            elif otherSet.Trie == trie' then otherSet
            else IntSet (trie')

    /// Removes the elements of the specified IntSet from this IntSet.
    member this.Difference (otherSet : IntSet) : IntSet =
        // If this set's trie is the same as the other set's trie, we can return immediately.
        if trie == otherSet.Trie then IntSet.Empty
        else
            // If the result is the same (physical equality) to one of the inputs,
            // return that input instead of creating a new IntSet.
            let trie' = PatriciaSet32.Difference (trie, otherSet.Trie)
            if trie == trie' then this
            elif otherSet.Trie == trie' then otherSet
            else IntSet (trie')

    /// Computes the union of a sequence of IntSets.
    static member internal UnionMany (sets : seq<IntSet>) : IntSet =
        // Preconditions
        checkNonNull "sets" sets

        let result =
            (PatriciaSet32.Empty, sets)
            ||> Seq.fold (fun combinedSetTree set ->
                PatriciaSet32.Union (combinedSetTree, set.Trie))

        // If the resulting trie is empty, return the empty IntSet instance
        // instead of creating a new one to allow for better structure sharing.
        match result with
        | Empty ->
            IntSet.Empty
        | _ ->
            IntSet (result)

    /// Computes the intersection of a sequence of IntSets.
    static member internal IntersectMany (sets : seq<IntSet>) : IntSet =
        // Preconditions
        checkNonNull "sets" sets

        // For compatibility with Set, don't check for an empty sequence --
        // just allow Seq.reduce to raise an exception.
        Seq.reduce (fun s1 s2 -> s1.Intersect s2) sets

    /// Determines if this set is a subset of the given set.
    // IsSubsetOf (otherSet) returns true if all values in this set are in 'otherSet'.
    member __.IsSubsetOf (otherSet : IntSet) : bool =
        PatriciaSet32.IsSubsetOf (trie, otherSet.Trie)

    /// Determines if set1 is a proper subset of set2.
    // IsProperSubsetOf (otherSet) returns true if all values in this set are in 'otherSet',
    // and 'otherSet' contains at least one value which is not in this set.
    member __.IsProperSubsetOf (otherSet : IntSet) : bool =
        PatriciaSet32.IsProperSubsetOf (trie, otherSet.Trie)

    //
    member __.IsSupersetOf (otherSet : IntSet) : bool =
        PatriciaSet32.IsSubsetOf (otherSet.Trie, trie)

    //
    member __.IsProperSupersetOf (otherSet : IntSet) : bool =
        PatriciaSet32.IsProperSubsetOf (otherSet.Trie, trie)

    /// Returns a new IntSet made from the given elements.
    static member internal OfSeq (source : seq<int>) : IntSet =
        // Preconditions
        checkNonNull "source" source

        IntSet (PatriciaSet32.OfSeq source)

    /// Returns a new IntSet made from the given elements.
    static member internal OfList (source : int list) : IntSet =
        // Preconditions
        checkNonNull "source" source

        // OPTIMIZATION : If the source is empty return immediately.
        if List.isEmpty source then
            IntSet.Empty
        else
            IntSet (PatriciaSet32.OfList source)

    /// Returns a new IntSet made from the given elements.
    static member internal OfArray (source : int[]) : IntSet =
        // Preconditions
        checkNonNull "source" source

        // OPTIMIZATION : If the source is empty return immediately.
        if Array.isEmpty source then
            IntSet.Empty
        else
            IntSet (PatriciaSet32.OfArray source)

    /// Returns a new IntSet made from the given elements.
    static member internal OfSet (source : Set<int>) : IntSet =
        // Preconditions
        checkNonNull "source" source

        // OPTIMIZATION : If the source is empty return immediately.
        if Set.isEmpty source then
            IntSet.Empty
        else
            IntSet (PatriciaSet32.OfSet source)

    //
    member __.ToSeq () =
        PatriciaSet32.ToSeq trie

    //
    member __.ToList () : int list =
        let folder = FSharpFunc<_,_,_>.Adapt (fun el list -> el :: list)
        PatriciaSet32.FoldBack (folder, [], trie)

    //
    member __.ToArray () : int[] =
        let elements = ResizeArray ()
        PatriciaSet32.Iterate (elements.Add, trie)
        elements.ToArray ()

    //
    member __.ToSet () : Set<int> =
        let folder = FSharpFunc<_,_,_>.Adapt Set.add
        PatriciaSet32.FoldBack (folder, Set.empty, trie)

    //
    member __.Iterate (action : int -> unit) : unit =
        PatriciaSet32.Iterate (action, trie)

    //
    member __.IterateBack (action : int -> unit) : unit =
        PatriciaSet32.IterateBack (action, trie)

    //
    member __.Fold (folder : 'State -> int -> 'State, state : 'State) : 'State =
        let folder = FSharpFunc<_,_,_>.Adapt folder
        PatriciaSet32.Fold (folder, state, trie)

    //
    member __.FoldBack (folder : int -> 'State -> 'State, state : 'State) : 'State =
        let folder = FSharpFunc<_,_,_>.Adapt folder
        PatriciaSet32.FoldBack (folder, state, trie)

    //
    member __.TryPick (picker : int -> 'T option) : 'T option =
        PatriciaSet32.TryPick (picker, trie)

    //
    member this.Pick (picker : int -> 'T option) : 'T =
        match this.TryPick picker with
        | Some value ->
            value
        | None ->
            // TODO : Provide a better error message
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    //
    member __.TryFind (predicate : int -> bool) : int option =
        PatriciaSet32.TryFind (predicate, trie)

    //
    member this.Find (predicate : int -> bool) : int =
        match this.TryFind predicate with
        | Some value ->
            value
        | None ->
            // TODO : Provide a better error message
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    //
    member this.Exists (predicate : int -> bool) : bool =
        this.TryPick (fun el ->
            if predicate el then Some () else None)
        |> Option.isSome

    //
    member this.Forall (predicate : int -> bool) : bool =
        this.TryPick (fun el ->
            if not (predicate el) then Some () else None)
        |> Option.isNone

    //
    member this.Choose (chooser : int -> int option) : IntSet =
        this.Fold ((fun chosenSet el ->
            match chooser el with
            | None ->
                chosenSet
            | Some newValue ->
                chosenSet.Add newValue), IntSet.Empty)

    //
    member this.Filter (predicate : int -> bool) : IntSet =
        this.Fold ((fun filteredSet el ->
            if predicate el then
                filteredSet
            else
                filteredSet.Remove el), this)

    //
    member this.Map (mapping : int -> int) : IntSet =
        this.Fold ((fun map el ->
            map.Add (mapping el)), IntSet.Empty)

    //
    // OPTIMIZE : Replace this with an optimized implementation instead of using Fold.
    member this.Partition (predicate : int -> bool) : IntSet * IntSet =
        this.Fold ((fun (trueSet, falseSet) el ->
            if predicate el then
                trueSet.Add el,
                falseSet
            else
                trueSet,
                falseSet.Add el), (IntSet.Empty, IntSet.Empty))

    /// Formats an element value for use within the ToString() method.
    static member (*inline*) private ElementString (element : obj) =
        match box element with
        | null -> "null"
        | :? System.IFormattable as formattable ->
            formattable.ToString (
                null, System.Globalization.CultureInfo.InvariantCulture)
        | _ ->
            element.ToString ()

    override this.ToString () =
        (* NOTE :   Like Set, we have specific cases for 0, 1, 2, 3, and 4+ elements. *)
        match List.ofSeq (Seq.truncate 4 this) with
        | [] -> "intSet []"
        | [h1] ->
            System.Text.StringBuilder()
                .Append("intSet [")
                .Append(IntSet.ElementString h1)
                .Append("]")
                .ToString()
        | [h1; h2] ->
            System.Text.StringBuilder()
                .Append("intSet [")
                .Append(IntSet.ElementString h1)
                .Append("; ")
                .Append(IntSet.ElementString h2)
                .Append("]")
                .ToString()
        | [h1; h2; h3] ->
            System.Text.StringBuilder()
                .Append("intSet [")
                .Append(IntSet.ElementString h1)
                .Append("; ")
                .Append(IntSet.ElementString h2)
                .Append("; ")
                .Append(IntSet.ElementString h3)
                .Append("]")
                .ToString()
        | h1 :: h2 :: h3 :: _ ->
            System.Text.StringBuilder()
                .Append("intSet [")
                .Append(IntSet.ElementString h1)
                .Append("; ")
                .Append(IntSet.ElementString h2)
                .Append("; ")
                .Append(IntSet.ElementString h3)
                .Append("; ... ]")
                .ToString()

    /// Compute the union of two sets.
    static member op_Addition (set1 : IntSet, set2 : IntSet) : IntSet =
        set1.Union set2

    /// Remove the elements of the second set from the first.
    static member op_Subtraction (set1 : IntSet, set2 : IntSet) : IntSet =
        set1.Difference set2

    /// <inherit />
    override __.Equals other =
        match other with
        | :? IntSet as other ->
            trie = other.Trie
        | _ ->
            false

    /// <inherit />
    override __.GetHashCode () =
        // TODO : Come up with a better hash code implementation.
        match trie with
        | Empty -> 0
        | _ ->
            trie.GetHashCode ()

    interface System.IEquatable<IntSet> with
        /// <inherit />
        member __.Equals other =
            trie = other.Trie

    interface System.IComparable with
        /// <inherit />
        member __.CompareTo other =
            compare trie (other :?> IntSet).Trie

    interface System.IComparable<IntSet> with
        /// <inherit />
        member __.CompareTo other =
            compare trie other.Trie

    interface System.Collections.IEnumerable with
        /// <inherit />
        member __.GetEnumerator () =
            (PatriciaSet32.ToSeq trie).GetEnumerator ()
            :> System.Collections.IEnumerator

    interface IEnumerable<int> with
        /// <inherit />
        member __.GetEnumerator () =
            (PatriciaSet32.ToSeq trie).GetEnumerator ()

    interface ICollection<int> with
        /// <inherit />
        member __.Count
            with get () =
                PatriciaSet32.Count trie

        /// <inherit />
        member __.IsReadOnly
            with get () = true

        /// <inherit />
        member __.Add _ =
            notSupported "IntSets cannot be mutated."

        /// <inherit />
        member __.Clear () =
            notSupported "IntSets cannot be mutated."

        /// <inherit />
        member __.Contains (item : int) =
            PatriciaSet32.Contains (uint32 item, trie)

        /// <inherit />
        member this.CopyTo (array, arrayIndex) =
            // Preconditions
            checkNonNull "array" array
            if arrayIndex < 0 then
                raise <| System.ArgumentOutOfRangeException "arrayIndex"

            let count = PatriciaSet32.Count trie
            if arrayIndex + count > Array.length array then
                invalidArg "arrayIndex"
                    "There is not enough room in the array to copy the \
                     elements when starting at the specified index."

            this.Fold ((fun index el ->
                array.[index] <- el
                index + 1), arrayIndex)
            |> ignore

        /// <inherit />
        member __.Remove _ : bool =
            notSupported "IntSets cannot be mutated."

//
and [<Sealed>]
    internal IntSetDebuggerProxy (set : IntSet) =

    [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
    member __.Items
        with get () : int[] =
            set.ToArray ()


/// Functional programming operators related to the IntSet type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IntSet =
    /// The empty set.
    [<CompiledName("Empty")>]
    let empty = IntSet.Empty

    /// Is the set empty?
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (set : IntSet) : bool =
        // Preconditions
        checkNonNull "set" set

        set.IsEmpty

    /// Returns the number of elements in the set.
    [<CompiledName("Count")>]
    let inline count (set : IntSet) : int =
        // Preconditions
        checkNonNull "set" set
        
        set.Count

    /// The set containing the given element.
    [<CompiledName("Singleton")>]
    let inline singleton (key : int) : IntSet =
        IntSet.Singleton key

    /// Returns a new set with an element added to the set.
    /// No exception is raised if the set already contains the element.
    [<CompiledName("Add")>]
    let inline add (key : int) (set : IntSet) : IntSet =
        // Preconditions
        checkNonNull "set" set
        
        set.Add key

    /// Returns a new set with the given element removed.
    /// No exception is raised if the set does not contain the element.
    [<CompiledName("Remove")>]
    let inline remove (key : int) (set : IntSet) : IntSet =
        // Preconditions
        checkNonNull "set" set
        
        set.Remove key

    /// <summary>
    /// Evaluates to &quot;true&quot; if the given element is in the given set.
    /// </summary>
    [<CompiledName("Contains")>]
    let inline contains (key : int) (set : IntSet) : bool =
        // Preconditions
        checkNonNull "set" set
        
        set.Contains key

    /// Returns the lowest element in the set according to an unsigned integer comparison.
    [<CompiledName("MinElement")>]
    let inline minElement (set : IntSet) : int =
        // Preconditions
        checkNonNull "set" set

        set.MinimumElement

    /// Returns the lowest element in the set according to a signed integer comparison.
    [<CompiledName("MinElementSigned")>]
    let inline minElementSigned (set : IntSet) : int =
        // Preconditions
        checkNonNull "set" set

        set.MinimumElementSigned

    /// Returns the highest element in the set according to an unsigned integer comparison.
    [<CompiledName("MaxElement")>]
    let inline maxElement (set : IntSet) : int =
        // Preconditions
        checkNonNull "set" set

        set.MaximumElement

    /// Returns the highest element in the set according to a signed integer comparison.
    [<CompiledName("MaxElementSigned")>]
    let inline maxElementSigned (set : IntSet) : int =
        // Preconditions
        checkNonNull "set" set

        set.MaximumElementSigned

    /// Computes the union of the two sets.
    [<CompiledName("Union")>]
    let inline union (set1 : IntSet) (set2 : IntSet) : IntSet =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2
        
        set1.Union set2

    /// Computes the union of a sequence of sets.
    [<CompiledName("UnionMany")>]
    let unionMany sets =
        // Preconditions checked by the target method.
        IntSet.UnionMany sets

    /// Computes the intersection of the two sets.
    [<CompiledName("Intersect")>]
    let inline intersect (set1 : IntSet) (set2 : IntSet) : IntSet =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2
        
        set1.Intersect set2

    /// Computes the intersection of a sequence of sets.
    [<CompiledName("IntersectMany")>]
    let intersectMany sets =
        // Preconditions checked by the target method.
        IntSet.IntersectMany sets

    /// Returns a new set with the elements of the second set removed from the first.
    [<CompiledName("Difference")>]
    let inline difference (set1 : IntSet) (set2 : IntSet) : IntSet =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2
        
        set1.Difference set2

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the first set are in the second.
    /// </summary>
    [<CompiledName("IsSubset")>]
    let inline isSubset (set1 : IntSet) (set2 : IntSet) : bool =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.IsSubsetOf set2

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the first set are in the second,
    /// and at least one element of the second is not in the first.
    /// </summary>
    [<CompiledName("IsProperSubset")>]
    let inline isProperSubset (set1 : IntSet) (set2 : IntSet) : bool =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2
        
        set1.IsProperSubsetOf set2

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the second set are in the first.
    /// </summary>
    [<CompiledName("IsSuperset")>]
    let inline isSuperset (set1 : IntSet) (set2 : IntSet) : bool =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2
        
        set1.IsSupersetOf set2

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the second set are in the first,
    /// and at least one element of the first is not in the second.
    /// </summary>
    [<CompiledName("IsProperSuperset")>]
    let inline isProperSuperset (set1 : IntSet) (set2 : IntSet) : bool =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.IsProperSupersetOf set2

    /// Builds a new collection from the given enumerable object.
    [<CompiledName("OfSeq")>]
    let (*inline*) ofSeq source : IntSet =
        // Preconditions are checked by the member.
        IntSet.OfSeq source

    /// Builds a set that contains the same elements as the given list.
    [<CompiledName("OfList")>]
    let (*inline*) ofList source : IntSet =
        // Preconditions are checked by the member.
        IntSet.OfList source

    /// Builds a set that contains the same elements as the given array.
    [<CompiledName("OfArray")>]
    let (*inline*) ofArray source : IntSet =
        // Preconditions are checked by the member.
        IntSet.OfArray source

    /// Builds an IntSet that contains the same elements as the given Set.
    [<CompiledName("OfSet")>]
    let (*inline*) ofSet source : IntSet =
        // Preconditions are checked by the member.
        IntSet.OfSet source

    /// Returns an ordered view of the collection as an enumerable object.
    [<CompiledName("ToSeq")>]
    let inline toSeq (set : IntSet) =
        // Preconditions
        checkNonNull "set" set
        
        set.ToSeq ()

    /// Builds a list that contains the elements of the set in order.
    [<CompiledName("ToList")>]
    let inline toList (set : IntSet) =
        // Preconditions
        checkNonNull "set" set
        
        set.ToList ()

    /// Builds an array that contains the elements of the set in order.
    [<CompiledName("ToArray")>]
    let inline toArray (set : IntSet) =
        // Preconditions
        checkNonNull "set" set
        
        set.ToArray ()

    /// Builds a Set that contains the same elements as the given IntSet.
    [<CompiledName("ToSet")>]
    let inline toSet (set : IntSet) =
        // Preconditions
        checkNonNull "set" set

        set.ToSet ()

    /// Applies the given function to each element of the set,
    /// in order according to the comparison function.
    [<CompiledName("Iterate")>]
    let inline iter (action : int -> unit) (set : IntSet) =
        // Preconditions
        checkNonNull "set" set
        
        set.Iterate action

    /// Applies the given function to each element of the set,
    /// in reverse order according to the comparison function.
    [<CompiledName("IterateBack")>]
    let inline iterBack (action : int -> unit) (set : IntSet) =
        // Preconditions
        checkNonNull "set" set
        
        set.IterateBack action

    /// Applies the given accumulating function to each element of the set,
    /// in order according to the comparison function.
    [<CompiledName("Fold")>]
    let inline fold (folder : 'State -> int -> 'State) (state : 'State) (set : IntSet) =
        // Preconditions
        checkNonNull "set" set
        
        set.Fold (folder, state)

    /// Applies the given accumulating function to each element of the set,
    /// in reverse order according to the comparison function.
    [<CompiledName("FoldBack")>]
    let inline foldBack (folder : int -> 'State -> 'State) (set : IntSet) (state : 'State) =
        // Preconditions
        checkNonNull "set" set

        set.FoldBack (folder, state)

    /// <summary>
    /// Applies the given function to each element of the set.
    /// Returns the set comprised of the results <c>x</c> for each element where the
    /// function returns <c>Some(x)</c>.
    /// </summary>
    [<CompiledName("Choose")>]
    let inline choose (chooser : int -> int option) (set : IntSet) : IntSet =
        // Preconditions
        checkNonNull "set" set
        
        set.Choose chooser

    /// <summary>
    /// Returns a new collection containing only the elements of the collection
    /// for which the given predicate returns &quot;true&quot;.
    /// </summary>
    [<CompiledName("Filter")>]
    let inline filter (predicate : int -> bool) (set : IntSet) : IntSet =
        // Preconditions
        checkNonNull "set" set
        
        set.Filter predicate

    /// Returns a new collection containing the results of applying the given function
    /// to each element of the input set.
    [<CompiledName("Map")>]
    let inline map (mapping : int -> int) (set : IntSet) : IntSet =
        // Preconditions
        checkNonNull "set" set
        
        set.Map mapping

    /// <summary>
    /// Splits the set into two sets containing the elements for which the given
    /// predicate returns &quot;true&quot; and &quot;false&quot;, respectively.
    /// </summary>
    [<CompiledName("Partition")>]
    let inline partition (predicate : int -> bool) (set : IntSet) : IntSet * IntSet =
        // Preconditions
        checkNonNull "set" set
        
        set.Partition predicate

    /// Tests if any element of the collection satisfies the given predicate.
    [<CompiledName("Exists")>]
    let inline exists (predicate : int -> bool) (set : IntSet) : bool =
        // Preconditions
        checkNonNull "set" set
        
        set.Exists predicate

    /// Tests if all elements of the collection satisfy the given predicate.
    [<CompiledName("Forall")>]
    let inline forall (predicate : int -> bool) (set : IntSet) : bool =
        // Preconditions
        checkNonNull "set" set

        set.Forall predicate

    /// <summary>
    /// Applies the given function to each element of the set, returning the first result
    /// where the function returns <c>Some(x)</c>. If the function never returns <c>Some(x)</c>
    /// then None is returned.
    /// </summary>
    [<CompiledName("TryPick")>]
    let inline tryPick (picker : int -> 'T option) (set : IntSet) : 'T option =
        // Preconditions
        checkNonNull "set" set
        
        set.TryPick picker

    /// <summary>
    /// Applies the given function to each element of the set, returning the first result
    /// where the function returns <c>Some(x)</c>. If the function never returns <c>Some(x)</c>
    /// then a KeyNotFoundException is raised.
    /// </summary>
    [<CompiledName("Pick")>]
    let inline pick (picker : int -> 'T option) (set : IntSet) : 'T =
        // Preconditions
        checkNonNull "set" set

        set.Pick picker

    /// <summary>
    /// Returns the first (least) element for which the given predicate returns &quot;true&quot;.
    /// Returns None if no such element exists.
    /// </summary>
    [<CompiledName("TryFind")>]
    let inline tryFind (predicate : int -> bool) (set : IntSet) : int option =
        // Preconditions
        checkNonNull "set" set
        
        set.TryFind predicate

    /// <summary>
    /// Returns the first (least) element for which the given predicate returns &quot;true&quot;.
    /// Raise KeyNotFoundException if no such element exists.
    /// </summary>
    [<CompiledName("Find")>]
    let inline find (predicate : int -> bool) (set : IntSet) : int =
        // Preconditions
        checkNonNull "set" set

        set.Find predicate


#if PROTO_COMPILER

/// An IntSet whose values are tagged with a unit-of-measure type.
[<MeasureAnnotatedAbbreviation>]
type TagSet<[<Measure>] 'Tag> = IntSet

/// Functional programming operators related to the TagMap<_> type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TagSet =
    /// Retypes a value without emitting any IL instructions.
    /// WARNING: This should be used with EXTREME CAUTION.
    [<NoDynamicInvocation>]
    [<CompiledName("RetypeInlined")>]
    let inline private retype<'T,'U> (x:'T) : 'U = (# "" x : 'U #)

    /// The empty set
    [<CompiledName("Empty")>]
    let empty<[<Measure>] 'Tag> : TagSet<'Tag> =
        retype IntSet.Empty

    /// Is the set empty?
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (set : TagSet<'Tag>) : bool =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.IsEmpty

    /// Returns the number of elements in the set
    [<CompiledName("Count")>]
    let inline count (set : TagSet<'Tag>) : int =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set
        
        set.Count

    /// The set containing the given element.
    [<CompiledName("Singleton")>]
    let inline singleton (element : int<'Tag>) : TagSet<'Tag> =
        IntSet.Singleton (int element)
        |> retype

    /// Returns a new set with an element added to the set.
    /// No exception is raised if the set already contains the element.
    [<CompiledName("Add")>]
    let inline add (key : int<'Tag>) (set : TagSet<'Tag>) : TagSet<'Tag> =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Add (int key)
        |> retype

    /// Returns a new set with the given element removed.
    /// No exception is raised if the set does not contain the element.
    [<CompiledName("Remove")>]
    let inline remove (key : int<'Tag>) (set : TagSet<'Tag>) : TagSet<'Tag> =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Remove (int key)
        |> retype

    /// <summary>
    /// Evaluates to &quot;true&quot; if the given element is in the given set.
    /// </summary>
    [<CompiledName("Contains")>]
    let inline contains (key : int<'Tag>) (set : TagSet<'Tag>) : bool =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Contains (int key)

    /// Returns the lowest element in the set according to an unsigned integer comparison.
    [<CompiledName("MinElement")>]
    let inline minElement (set : TagSet<'Tag>) : int<'Tag> =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.MinimumElement
        |> retype

    /// Returns the lowest element in the set according to a signed integer comparison.
    [<CompiledName("MinElementSigned")>]
    let inline minElementSigned (set : TagSet<'Tag>) : int<'Tag> =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.MinimumElementSigned
        |> retype

    /// Returns the highest element in the set according to an unsigned integer comparison.
    [<CompiledName("MaxElement")>]
    let inline maxElement (set : TagSet<'Tag>) : int<'Tag> =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.MaximumElement
        |> retype

    /// Returns the highest element in the set according to a signed integer comparison.
    [<CompiledName("MaxElementSigned")>]
    let inline maxElementSigned (set : TagSet<'Tag>) : int<'Tag> =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.MaximumElementSigned
        |> retype

    /// Computes the union of the two sets.
    [<CompiledName("Union")>]
    let inline union (set1 : TagSet<'Tag>) (set2 : TagSet<'Tag>) : TagSet<'Tag> =
        // Retype as IntSet
        let set1 : IntSet = retype set1
        let set2 : IntSet = retype set2

        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.Union set2
        |> retype

    /// Computes the union of a sequence of sets.
    [<CompiledName("UnionMany")>]
    let unionMany (sets : seq<TagSet<'Tag>>) : TagSet<'Tag> =
        // Retype as seq<IntSet>
        let sets : seq<IntSet> = retype sets

        // Preconditions checked by the target method.
        IntSet.UnionMany sets
        |> retype

    /// Computes the intersection of the two sets.
    [<CompiledName("Intersect")>]
    let inline intersect (set1 : TagSet<'Tag>) (set2 : TagSet<'Tag>) : TagSet<'Tag> =
        // Retype as IntSet
        let set1 : IntSet = retype set1
        let set2 : IntSet = retype set2

        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.Intersect set2
        |> retype

    /// Computes the intersection of a sequence of sets.
    [<CompiledName("IntersectMany")>]
    let intersectMany (sets : seq<TagSet<'Tag>>) : TagSet<'Tag> =
        // Retype as seq<IntSet>
        let sets : seq<IntSet> = retype sets

        // Preconditions checked by the target method.
        IntSet.IntersectMany sets
        |> retype

    /// Returns a new set with the elements of the second set removed from the first.
    [<CompiledName("Difference")>]
    let inline difference (set1 : TagSet<'Tag>) (set2 : TagSet<'Tag>) : TagSet<'Tag> =
        // Retype as IntSet
        let set1 : IntSet = retype set1
        let set2 : IntSet = retype set2

        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.Difference set2
        |> retype

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the first set are in the second.
    /// </summary>
    [<CompiledName("IsSubset")>]
    let inline isSubset (set1 : TagSet<'Tag>) (set2 : TagSet<'Tag>) : bool =
        // Retype as IntSet
        let set1 : IntSet = retype set1
        let set2 : IntSet = retype set2

        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.IsSubsetOf set2

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the first set are in the second,
    /// and at least one element of the second is not in the first.
    /// </summary>
    [<CompiledName("IsProperSubset")>]
    let inline isProperSubset (set1 : TagSet<'Tag>) (set2 : TagSet<'Tag>) : bool =
        // Retype as IntSet
        let set1 : IntSet = retype set1
        let set2 : IntSet = retype set2

        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.IsProperSubsetOf set2

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the second set are in the first.
    /// </summary>
    [<CompiledName("IsSuperset")>]
    let inline isSuperset (set1 : TagSet<'Tag>) (set2 : TagSet<'Tag>) : bool =
        // Retype as IntSet
        let set1 : IntSet = retype set1
        let set2 : IntSet = retype set2

        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.IsSupersetOf set2

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the second set are in the first,
    /// and at least one element of the first is not in the second.
    /// </summary>
    [<CompiledName("IsProperSuperset")>]
    let inline isProperSuperset (set1 : TagSet<'Tag>) (set2 : TagSet<'Tag>) : bool =
        // Retype as IntSet
        let set1 : IntSet = retype set1
        let set2 : IntSet = retype set2

        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.IsProperSupersetOf set2

    /// Builds a new collection from the given enumerable object.
    [<CompiledName("OfSeq")>]
    let (*inline*) ofSeq (source : seq<int<'Tag>>) : TagSet<'Tag> =
        // Preconditions are checked by the member.
        IntSet.OfSeq (retype source)
        |> retype

    /// Builds a set that contains the same elements as the given list.
    [<CompiledName("OfList")>]
    let (*inline*) ofList (source : int<'Tag> list) : TagSet<'Tag> =
        // Preconditions are checked by the member.
        IntSet.OfList (retype source)
        |> retype

    /// Builds a set that contains the same elements as the given array.
    [<CompiledName("OfArray")>]
    let (*inline*) ofArray (source : int<'Tag>[]) : TagSet<'Tag> =
        // Preconditions are checked by the member.
        IntSet.OfArray (retype source)
        |> retype

    /// Builds an set that contains the same elements as the given Set.
    [<CompiledName("OfSet")>]
    let (*inline*) ofSet (source : Set<int<'Tag>>) : TagSet<'Tag> =
        // Preconditions are checked by the member.
        IntSet.OfSet (retype source)
        |> retype

    /// Returns an ordered view of the collection as an enumerable object.
    [<CompiledName("ToSeq")>]
    let inline toSeq (set : TagSet<'Tag>) : seq<int<'Tag>> =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.ToSeq ()
        |> retype

    /// Builds a list that contains the elements of the set in order.
    [<CompiledName("ToList")>]
    let inline toList (set : TagSet<'Tag>) : int<'Tag> list =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.ToList ()
        |> retype

    /// Builds an array that contains the elements of the set in order.
    [<CompiledName("ToArray")>]
    let inline toArray (set : TagSet<'Tag>) : int<'Tag>[] =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.ToArray ()
        |> retype

    /// Builds a Set that contains the same elements as the given TagSet.
    [<CompiledName("ToSet")>]
    let inline toSet (set : TagSet<'Tag>) : Set<int<'Tag>> =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.ToSet ()
        |> retype

    /// Applies the given function to each element of the set,
    /// in order according to the comparison function.
    [<CompiledName("Iterate")>]
    let inline iter (action : int<'Tag> -> unit) (set : TagSet<'Tag>) =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Iterate (retype action)

    /// Applies the given function to each element of the set,
    /// in reverse order according to the comparison function.
    [<CompiledName("IterateBack")>]
    let inline iterBack (action : int<'Tag> -> unit) (set : TagSet<'Tag>) =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.IterateBack (retype action)

    /// Applies the given accumulating function to each element of the set,
    /// in order according to the comparison function.
    [<CompiledName("Fold")>]
    let inline fold (folder : 'State -> int<'Tag> -> 'State) (state : 'State) (set : TagSet<'Tag>) =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Fold (retype folder, state)

    /// Applies the given accumulating function to each element of the set,
    /// in reverse order according to the comparison function.
    [<CompiledName("FoldBack")>]
    let inline foldBack (folder : int<'Tag> -> 'State -> 'State) (set : TagSet<'Tag>) (state : 'State) =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.FoldBack (retype folder, state)

    /// <summary>
    /// Applies the given function to each element of the set.
    /// Returns the set comprised of the results <c>x</c> for each element where the
    /// function returns <c>Some(x)</c>.
    /// </summary>
    [<CompiledName("Choose")>]
    let inline choose (chooser : int<'Tag1> -> int<'Tag2> option) (set : TagSet<'Tag1>) : TagSet<'Tag2> =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Choose (retype chooser)
        |> retype

    /// <summary>
    /// Returns a new collection containing only the elements of the collection
    /// for which the given predicate returns &quot;true&quot;.
    /// </summary>
    [<CompiledName("Filter")>]
    let inline filter (predicate : int<'Tag> -> bool) (set : TagSet<'Tag>) : TagSet<'Tag> =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Filter (retype predicate)
        |> retype

    /// Returns a new collection containing the results of applying the given function
    /// to each element of the input set.
    [<CompiledName("Map")>]
    let inline map (mapping : int<'Tag1> -> int<'Tag2>) (set : TagSet<'Tag1>) : TagSet<'Tag2> =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Map (retype mapping)
        |> retype

    /// <summary>
    /// Splits the set into two sets containing the elements for which the given
    /// predicate returns &quot;true&quot; and &quot;false&quot;, respectively.
    /// </summary>
    [<CompiledName("Partition")>]
    let inline partition (predicate : int<'Tag> -> bool) (set : TagSet<'Tag>) : TagSet<'Tag> * TagSet<'Tag> =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        let set1, set2 = set.Partition (retype predicate)
        (retype set1), (retype set2)

    /// Tests if any element of the collection satisfies the given predicate.
    [<CompiledName("Exists")>]
    let inline exists (predicate : int<'Tag> -> bool) (set : TagSet<'Tag>) : bool =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Exists (retype predicate)

    /// Tests if all elements of the collection satisfy the given predicate.
    [<CompiledName("Forall")>]
    let inline forall (predicate : int<'Tag> -> bool) (set : TagSet<'Tag>) : bool =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Forall (retype predicate)

    /// <summary>
    /// Applies the given function to each element of the set, returning the first result
    /// where the function returns <c>Some(x)</c>. If the function never returns <c>Some(x)</c>
    /// then None is returned.
    /// </summary>
    [<CompiledName("TryPick")>]
    let inline tryPick (picker : int<'Tag> -> 'T option) (set : TagSet<'Tag>) : 'T option =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.TryPick (retype picker)

    /// <summary>
    /// Applies the given function to each element of the set, returning the first result
    /// where the function returns <c>Some(x)</c>. If the function never returns <c>Some(x)</c>
    /// then a KeyNotFoundException is raised.
    /// </summary>
    [<CompiledName("Pick")>]
    let inline pick (picker : int<'Tag> -> 'T option) (set : TagSet<'Tag>) : 'T =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Pick (retype picker)

    /// <summary>
    /// Returns the first (least) element for which the given predicate returns &quot;true&quot;.
    /// Returns None if no such element exists.
    /// </summary>
    [<CompiledName("TryFind")>]
    let inline tryFind (predicate : int<'Tag> -> bool) (set : TagSet<'Tag>) : int<'Tag> option =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.TryFind (retype predicate)
        |> retype

    /// <summary>
    /// Returns the first (least) element for which the given predicate returns &quot;true&quot;.
    /// Raise KeyNotFoundException if no such element exists.
    /// </summary>
    [<CompiledName("Find")>]
    let inline find (predicate : int<'Tag> -> bool) (set : TagSet<'Tag>) : int<'Tag> =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Find (retype predicate)
        |> retype

#endif

