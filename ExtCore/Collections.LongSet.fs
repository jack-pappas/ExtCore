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

type internal Mask64 = uint64
type internal Key64 = uint64
type internal Prefix64 = uint64

/// Bitwise operations (64-bit) necessary for implementing Patricia tries.
module internal BitOps64 =
    #if LITTLE_ENDIAN_TRIES
    
    //
    let inline private leastSignificantSetBit (x : uint64) : uint64 =
        x &&& (uint64 -(int64 x))

    /// Finds the last (least-significant) bit at which p0 and p1 disagree.
    /// Returns a power-of-two value containing this (and only this) bit.
    let inline branchingBit (p0 : Prefix64, p1 : Prefix64) : Mask64 =
        leastSignificantSetBit (p0 ^^^ p1)

    /// Clears the indicated bit and sets all lower bits.
    let inline mask (key : Key64, mask : Mask64) : Prefix64 =
        key &&& (mask - 1u)

    //
    let (*inline*) shorter (m1 : Mask64, m2 : Mask64) : bool =
        notImpl "BitOps.shorter (Little-Endian)"

    #else
    
    // http://aggregate.org/MAGIC/#Most%20Significant%201%20Bit
    // OPTIMIZE : This could be even faster if we could take advantage of a built-in
    // CPU instruction here (such as 'bsr', 'ffs', or 'clz').
    // Could we expose these instructions through Mono?
    let inline private mostSignificantSetBit (x1 : uint64) =
        let x2 = x1 ||| (x1 >>> 1)
        let x3 = x2 ||| (x2 >>> 2)
        let x4 = x3 ||| (x3 >>> 4)
        let x5 = x4 ||| (x4 >>> 8)
        let x6 = x5 ||| (x5 >>> 16)
        let x7 = x6 ||| (x6 >>> 32)
        // OPTIMIZATION : p AND (NOT q) <-> p XOR q
        x7 ^^^ (x7 >>> 1)

    /// Finds the first (most-significant) bit at which p0 and p1 disagree.
    /// Returns a power-of-two value containing this (and only this) bit.
    let inline branchingBit (p0 : Prefix64, p1 : Prefix64) : Mask64 =
        mostSignificantSetBit (p0 ^^^ p1)

    /// Clears the indicated bit and sets all lower bits.
    let inline mask (key : Key64, mask : Mask64) : Prefix64 =
        key &&& (~~~(mask - 1UL) ^^^ mask)

    //
    let inline shorter (m1 : Mask64, m2 : Mask64) : bool =
        // NOTE : This must be an *unsigned* comparison for the results to be correct.
        m1 > m2

    #endif

    //
    let inline matchPrefix (key : Key64, prefix : Prefix64, mask' : Mask64) : bool =
        mask (key, mask') = prefix

    /// <summary>Determines if all specified bits are cleared (not set) in a value.</summary>
    /// <param name="value">The value to test.</param>
    /// <param name="bitValue">The bits to test in 'value'.</param>
    /// <returns>true if all bits which are set in 'bitValue' are *not* set in 'value'.</returns>
    let inline zeroBit (key : Key64, mask : Mask64) : bool =
        key &&& mask = 0UL


open PatriciaTrieConstants
open BitOps64


/// A Patricia trie implementation.
/// Used as the underlying data structure for LongSet (and LongTagSet).
[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type private PatriciaSet64 =
    | Empty
    // Key
    | Lf of Key64
    // Prefix * Mask * Left-Child * Right-Child
    | Br of Prefix64 * Mask64 * PatriciaSet64 * PatriciaSet64

    /// Determines if the set contains the given value.
    static member Contains (key, set : PatriciaSet64) =
        match set with
        | Empty ->
            false
        | Lf j ->
            key = j
        | Br (_, m, t0, t1) ->
            PatriciaSet64.Contains
                (key, (if zeroBit (key, m) then t0 else t1))

    /// The number of items (i.e., the cardinality) of the set.
    static member Count (set : PatriciaSet64) : int64 =
        match set with
        | Empty -> 0L
        | Lf _ -> 1L
        | Br (_,_,_,_) ->
            /// The stack of trie nodes pending traversal.
            let stack = Stack (defaultTraversalStackSize)

            // Add the initial tree to the stack.
            stack.Push set

            // Traverse the tree, counting the elements by using the mutable stack.
            let mutable count = 0UL

            while stack.Count <> 0 do
                match stack.Pop () with
                | Empty -> ()
                | Lf _ ->
                    count <- count + 1UL
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf _, Lf _) ->
                    count <- count + 2UL
                    
                | Br (_, _, Lf _, child)
                | Br (_, _, child, Lf _) ->
                    count <- count + 1UL
                    stack.Push child

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push right
                    stack.Push left

            // Return the computed element count.
            Checked.int64 count

    /// Retrieve the minimum element of the set.
    static member MinElement (set : PatriciaSet64) =
        match set with
        | Empty ->
            invalidArg "set" "The set is empty."
        | Lf j -> j
        | Br (_, _, t0, _) ->
            PatriciaSet64.MinElement t0

    /// Retrieve the minimum element of the set, treating the
    /// elements of the set as signed values.
    static member MinElementSigned (set : PatriciaSet64) =
        match set with
        | Empty ->
            invalidArg "set" "The set is empty."
        | Lf j -> j
        | Br (_, m, t0, t1) ->
            // OPTIMIZE : Just use a bitmask here to check the top bit?
            if (int64 m) < 0L then t1 else t0
            |> PatriciaSet64.MinElement

    /// Retrieve the maximum element of the set.
    static member MaxElement (set : PatriciaSet64) =
        match set with
        | Empty ->
            invalidArg "set" "The set is empty."
        | Lf j -> j
        | Br (_, _, _, t1) ->
            PatriciaSet64.MaxElement t1

    /// Retrieve the maximum element of the set, treating the
    /// elements of the set as signed values.
    static member MaxElementSigned (set : PatriciaSet64) =
        match set with
        | Empty ->
            invalidArg "set" "The set is empty."
        | Lf j -> j
        | Br (_, m, t0, t1) ->
            // OPTIMIZE : Just use a bitmask here to check the top bit?
            if (int64 m) < 0L then t0 else t1
            |> PatriciaSet64.MaxElement

    /// Remove an item from the set.
    static member Remove (key, set : PatriciaSet64) =
        match set with
        | Empty ->
            Empty
        | Lf j ->
            if j = key then Empty
            else set
        
        | Br (p, m, t0, t1) ->
            if matchPrefix (key, p, m) then
                if zeroBit (key, m) then
                    match PatriciaSet64.Remove (key, t0) with
                    | Empty -> t1
                    | left ->
                        // Only create a new tree when the value was actually removed
                        // (i.e., the tree was modified).
                        if left === t0 then set
                        else Br (p, m, left, t1)
                else
                    match PatriciaSet64.Remove (key, t1) with
                    | Empty -> t0
                    | right ->
                        // Only create a new tree when the value was actually removed
                        // (i.e., the tree was modified).
                        if right === t1 then set
                        else Br (p, m, t0, right)
            else set

    //
    static member inline private Join (p0, t0 : PatriciaSet64, p1, t1) =
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
                PatriciaSet64.Join (key, Lf key, j, set)

        | Br (p, m, t0, t1) ->
            if matchPrefix (key, p, m) then
                if zeroBit (key, m) then
                    let left = PatriciaSet64.Add (key, t0)

                    // Only create a new tree when the value was actually added
                    // (i.e., the tree was modified).
                    if left === t0 then set
                    else Br (p, m, left, t1)
                else
                    let right = PatriciaSet64.Add (key, t1)

                    // Only create a new tree when the value was actually added
                    // (i.e., the tree was modified).
                    if right === t1 then set
                    else Br (p, m, t0, right)
            else
                PatriciaSet64.Join (key, Lf key, p, set)

    /// Computes the union of two PatriciaSet64s.
    static member Union (s, t) : PatriciaSet64 =
        // If the sets are identical, return immediately.
        if s === t then s else
        match s, t with
        | Br (p, m, s0, s1), Br (q, n, t0, t1) ->
            if m = n then
                if p = q then
                    // The trees have the same prefix. Merge the subtrees.
                    let left = PatriciaSet64.Union (s0, t0)
                    let right = PatriciaSet64.Union (s1, t1)
                    
                    // Only create a new tree if some values were actually added
                    // (i.e., the tree was modified).
                    if left === s0 && right === s1 then s
                    elif left === t0 && right === t1 then t
                    else Br (p, m, left, right)
                else
                    // The prefixes disagree.
                    PatriciaSet64.Join (p, s, q, t)
            
            #if LITTLE_ENDIAN_TRIES
            elif m < n then
            #else
            elif m > n then
            #endif
                if matchPrefix (q, p, m) then
                    // q contains p. Merge t with a subtree of s.
                    if zeroBit (q, m) then
                        let left = PatriciaSet64.Union (s0, t)
                        
                        // Only create a new tree when the subtree is actually modified.
                        if left === s0 then s
                        else Br (p, m, left, s1)
                    else
                        let right = PatriciaSet64.Union (s1, t)

                        // Only create a new tree when the subtree is actually modified.
                        if right === s1 then s
                        else Br (p, m, s0, right)
                else
                    // The prefixes disagree.
                    PatriciaSet64.Join (p, s, q, t)

            else
                if matchPrefix (p, q, n) then
                    // p contains q. Merge s with a subtree of t.
                    if zeroBit (p, n) then
                        let left = PatriciaSet64.Union (s, t0)
                        
                        // Only create a new tree when the subtree is actually modified.
                        if left === t0 then t
                        else Br (q, n, left, t1)
                    else
                        let right = PatriciaSet64.Union (s, t1)
                        
                        // Only create a new tree when the subtree is actually modified.
                        if right === t1 then t
                        else Br (q, n, t0, right)
                else
                    // The prefixes disagree.
                    PatriciaSet64.Join (p, s, q, t)

        | Br (p, m, s0, s1), Lf k ->
            if matchPrefix (k, p, m) then
                if zeroBit (k, m) then
                    let left = PatriciaSet64.Add (k, s0)
                    
                    // Only create a new tree when the subtree is actually modified.
                    if left === s0 then s
                    else Br (p, m, left, s1)
                else
                    let right = PatriciaSet64.Add (k, s1)
                    
                    // Only create a new tree when the subtree is actually modified.
                    if right === s1 then s
                    else Br (p, m, s0, right)
            else
                PatriciaSet64.Join (k, t, p, s)

        | Br (_,_,_,_), Empty ->
            s
        | Lf k, t ->
            PatriciaSet64.Add (k, t)
        | Empty, t ->
            t

    /// Compute the intersection of two PatriciaSet64s.
    static member Intersect (s, t) : PatriciaSet64 =
        // If the sets are identical, return immediately.
        if s === t then s else
        match s, t with
        | Br (p, m, s0, s1), Br (q, n, t0, t1) ->
            if m = n then
                if p <> q then Empty
                else
                    let left = PatriciaSet64.Intersect (s0, t0)
                    let right = PatriciaSet64.Intersect (s1, t1)
                    match left, right with
                    | Empty, r
                    | r, Empty -> r
                    | left, right ->
                        // Only create a new tree if some values were actually removed
                        // (i.e., the tree was modified).
                        if left === s0 && right === s1 then s
                        elif left === t0 && right === t1 then t
                        else Br (p, m, left, right)

            #if LITTLE_ENDIAN_TRIES
            elif m < n then
            #else
            elif m > n then
            #endif
                if matchPrefix (q, p, m) then
                    if zeroBit (q, m) then
                        PatriciaSet64.Intersect (s0, t)
                    else
                        PatriciaSet64.Intersect (s1, t)
                else
                    Empty

            else
                if matchPrefix (p, q, n) then
                    if zeroBit (p, n) then
                        PatriciaSet64.Intersect (s, t0)
                    else
                        PatriciaSet64.Intersect (s, t1)
                else
                    Empty

        | Br (_, m, s0, s1), Lf k ->
            let s' = if zeroBit (k, m) then s0 else s1
            if PatriciaSet64.Contains (k, s') then t
            else Empty
            
        | Br (_,_,_,_), Empty ->
            Empty
            
        | Lf k, t ->
            if PatriciaSet64.Contains (k, t) then s
            else Empty

        | Empty, _ ->
            Empty

    /// Compute the difference of two PatriciaSet64s.
    static member Difference (s, t) : PatriciaSet64 =
        // If the sets are identical, return immediately.
        if s === t then Empty else
        match s, t with
        | Br (p, m, s0, s1), Br (q, n, t0, t1) ->
            if m = n then
                if p <> q then s
                else
                    let left = PatriciaSet64.Difference (s0, t0)
                    let right = PatriciaSet64.Difference (s1, t1)
                    match left, right with
                    | Empty, r
                    | r, Empty -> r
                    | left, right ->
                        // Only create a new tree if some values were actually removed
                        // (i.e., the tree was modified).
                        if left === s0 && right === s1 then s
                        else Br (p, m, left, right)

            #if LITTLE_ENDIAN_TRIES
            elif m < n then
            #else
            elif m > n then
            #endif
                if matchPrefix (q, p, m) then
                    if zeroBit (q, m) then
                        match PatriciaSet64.Difference (s0, t) with
                        | Empty -> s1
                        | left ->
                            // Only create a new tree some values were actually removed
                            // (i.e., the tree was modified).
                            if left === s0 then s
                            else Br (p, m, left, s1)
                    else
                        match PatriciaSet64.Difference (s1, t) with
                        | Empty -> s0
                        | right ->
                            // Only create a new tree some values were actually removed
                            // (i.e., the tree was modified).
                            if right === s1 then s
                            else Br (p, m, s0, right)
                else s

            else
                if matchPrefix (p, q, n) then
                    if zeroBit (p, n) then
                        PatriciaSet64.Difference (s, t0)
                    else
                        PatriciaSet64.Difference (s, t1)
                else s

        | Br (p, m, s0, s1), Lf k ->
            if matchPrefix (k, p, m) then
                if zeroBit (k, m) then
                    match PatriciaSet64.Remove (k, s0) with
                    | Empty -> s1
                    | left ->
                        match s1 with
                        | Empty -> left
                        | _ ->
                            // Only create a new tree if the value was actually removed
                            // (i.e., the tree was modified).
                            if left === s0 then s
                            else Br (p, m, left, s1)
                else
                    match PatriciaSet64.Remove (k, s1) with
                    | Empty -> s0
                    | right ->
                        match s0 with
                        | Empty -> right
                        | _ ->
                            // Only create a new tree if the value was actually removed
                            // (i.e., the tree was modified).
                            if right === s1 then s
                            else Br (p, m, s0, right)
            else s
            
        | Br (_,_,_,_), Empty ->
            s
        | Lf k, t ->
            if PatriciaSet64.Contains (k, t) then Empty
            else s
        | Empty, _ ->
            Empty

    /// Computes the containment ordering for two sets (i.e., determines if one set includes the other).
    // Return values:
    //  -1 : All values in 't1' are in 't2', and at least one value of 't2' is not in 't1'.
    //   0 : The sets contain _exactly_ the same values.
    //   1 : The sets are disjoint, i.e., 't1' contains at least one value which is not in 't2'.
    static member private SubsetCompare (t1 : PatriciaSet64, t2 : PatriciaSet64) : int =
        match t1, t2 with
        | Br (p1, m1, l1, r1), Br (p2, m2, l2, r2) ->
            if shorter (m1, m2) then 1
            elif shorter (m2, m1) then
                if not <| matchPrefix (p1, p2, m2) then 1
                elif zeroBit (p1, m2) then
                    match PatriciaSet64.SubsetCompare (t1, l2) with 1 -> 1 | _ -> -1
                else
                    match PatriciaSet64.SubsetCompare (t1, r2) with 1 -> 1 | _ -> -1
            elif p1 = p2 then
                let left = PatriciaSet64.SubsetCompare (l1, l2)
                let right = PatriciaSet64.SubsetCompare (r1, r2)
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
                match PatriciaSet64.SubsetCompare (t1, l) with 1 -> 1 | _ -> -1
            else
                match PatriciaSet64.SubsetCompare (t1, r) with 1 -> 1 | _ -> -1

        | Lf _, Empty ->
            // The maps are disjoint.
            1

        | Empty, Empty -> 0
        | Empty, _ -> -1

    /// Is 'set1' a proper subset of 'set2'?
    /// IsProperSubset (set1, set2) returns true if all keys in set1 are in set2,
    /// and at least one element in set2 is not in set1.
    static member IsProperSubsetOf (set1 : PatriciaSet64, set2 : PatriciaSet64) : bool =
        match PatriciaSet64.SubsetCompare (set1, set2) with
        | -1 -> true
        | _ -> false

    /// Is 'set1' a subset of 'set2'?
    /// IsSubset (set1, set2) returns true if all keys in set1 are in set2.
    static member IsSubsetOf (set1 : PatriciaSet64, set2 : PatriciaSet64) : bool =
        match PatriciaSet64.SubsetCompare (set1, set2) with
        | -1 | 0 -> true
        | _ -> false

    //
    static member OfSeq (source : seq<int64>) : PatriciaSet64 =
        (Empty, source)
        ||> Seq.fold (fun trie el ->
            PatriciaSet64.Add (uint64 el, trie))

    //
    static member OfList (source : int64 list) : PatriciaSet64 =
        // Preconditions
        checkNonNull "source" source

        (Empty, source)
        ||> List.fold (fun trie el ->
            PatriciaSet64.Add (uint64 el, trie))

    //
    static member OfArray (source : int64[]) : PatriciaSet64 =
        // Preconditions
        checkNonNull "source" source

        (Empty, source)
        ||> Array.fold (fun trie el ->
            PatriciaSet64.Add (uint64 el, trie))

    //
    static member OfSet (source : Set<int64>) : PatriciaSet64 =
        // Preconditions
        checkNonNull "source" source

        (Empty, source)
        ||> Set.fold (fun trie el ->
            PatriciaSet64.Add (uint64 el, trie))

    //
    static member Iterate (action : int64 -> unit, set) : unit =
        match set with
        | Empty -> ()
        | Lf k ->
            action (int64 k)
        | Br (_,_,_,_) ->
            /// The stack of trie nodes pending traversal.
            let stack = Stack (defaultTraversalStackSize)

            // Add the initial tree to the stack.
            stack.Push set

            // Loop until we've processed the entire tree.
            while stack.Count <> 0 do
                match stack.Pop () with
                | Empty -> ()
                | Lf k ->
                    action (int64 k)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf k, Lf j) ->
                    action (int64 k)
                    action (int64 j)
                    
                | Br (_, _, Lf k, right) ->
                    // Only handle the case where the left child is a leaf
                    // -- otherwise the traversal order would be altered.
                    action (int64 k)
                    stack.Push right

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push right
                    stack.Push left

    //
    static member IterateBack (action : int64 -> unit, set) : unit =
        match set with
        | Empty -> ()
        | Lf k ->
            action (int64 k)
        | Br (_,_,_,_) ->
            /// The stack of trie nodes pending traversal.
            let stack = Stack (defaultTraversalStackSize)

            // Add the initial tree to the stack.
            stack.Push set

            // Loop until we've processed the entire tree.
            while stack.Count <> 0 do
                match stack.Pop () with
                | Empty -> ()
                | Lf k ->
                    action (int64 k)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf k, Lf j) ->
                    action (int64 j)
                    action (int64 k)
                    
                | Br (_, _, left, Lf k) ->
                    // Only handle the case where the right child is a leaf
                    // -- otherwise the traversal order would be altered.
                    action (int64 k)
                    stack.Push left

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push left
                    stack.Push right

    //
    static member Fold (folder : 'State -> int64 -> 'State, state : 'State, set) : 'State =
        match set with
        | Empty ->
            state
        | Lf k ->
            folder state (int64 k)
        | Br (_,_,_,_) ->
            let folder = FSharpFunc<_,_,_>.Adapt folder

            /// The stack of trie nodes pending traversal.
            let stack = Stack (defaultTraversalStackSize)

            // Add the initial tree to the stack.
            stack.Push set

            /// Loop until we've processed the entire tree.
            let mutable state = state
            while stack.Count <> 0 do
                match stack.Pop () with
                | Empty -> ()
                | Lf k ->
                    state <- folder.Invoke (state, int64 k)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf k, Lf j) ->
                    state <- folder.Invoke (state, int64 k)
                    state <- folder.Invoke (state, int64 j)
                    
                | Br (_, _, Lf k, right) ->
                    // Only handle the case where the left child is a leaf
                    // -- otherwise the traversal order would be altered.
                    state <- folder.Invoke (state, int64 k)
                    stack.Push right

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push right
                    stack.Push left

            // Return the final state value.
            state

    //
    static member FoldBack (folder : int64 -> 'State -> 'State, state : 'State, set) : 'State =
        match set with
        | Empty ->
            state
        | Lf k ->
            folder (int64 k) state
        | Br (_,_,_,_) ->
            let folder = FSharpFunc<_,_,_>.Adapt folder

            /// The stack of trie nodes pending traversal.
            let stack = Stack (defaultTraversalStackSize)

            // Add the initial tree to the stack.
            stack.Push set

            /// Loop until we've processed the entire tree.
            let mutable state = state
            while stack.Count <> 0 do
                match stack.Pop () with
                | Empty -> ()
                | Lf k ->
                    state <- folder.Invoke (int64 k, state)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf k, Lf j) ->
                    state <- folder.Invoke (int64 j, state)
                    state <- folder.Invoke (int64 k, state)
                    
                | Br (_, _, left, Lf k) ->
                    // Only handle the case where the right child is a leaf
                    // -- otherwise the traversal order would be altered.
                    state <- folder.Invoke (int64 k, state)
                    stack.Push left

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push left
                    stack.Push right

            // Return the final state value.
            state

    //
    static member TryPick (picker : int64 -> 'T option, set) : 'T option =
        match set with
        | Empty ->
            None
        | Lf k ->
            picker (int64 k)
        | Br (_,_,_,_) ->
            /// The stack of trie nodes pending traversal.
            let stack = Stack (defaultTraversalStackSize)

            // Add the initial tree to the stack.
            stack.Push set

            /// Loop until we find a key/value that matches the picker,
            /// or until we've processed the entire tree.
            let mutable pickedValue = None
            while stack.Count <> 0 && Option.isNone pickedValue do
                match stack.Pop () with
                | Empty -> ()
                | Lf k ->
                    pickedValue <- picker <| int64 k
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf k, Lf j) ->
                    pickedValue <-
                        match picker (int64 k) with
                        | (Some _) as value ->
                            value
                        | None ->
                            picker (int64 j)
                    
                | Br (_, _, Lf k, right) ->
                    // Only handle the case where the left child is a leaf
                    // -- otherwise the traversal order would be altered.
                    pickedValue <- picker (int64 k)
                    stack.Push right

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push right
                    stack.Push left

            // Return the picked value.
            pickedValue

    //
    static member ToSeq set =
        seq {
        match set with
        | Empty -> ()
        | Lf k ->
            yield int64 k
        
        (* OPTIMIZATION :   When one or both children of this node are leaves,
                            we handle them directly since it's a little faster. *)
        | Br (_, _, Lf k, Lf j) ->
            yield int64 k
            yield int64 j
                    
        | Br (_, _, Lf k, right) ->
            // Only handle the case where the left child is a leaf
            // -- otherwise the traversal order would be altered.
            yield int64 k
            yield! PatriciaSet64.ToSeq right

        | Br (_, _, left, right) ->
            // Recursively visit the children.
            yield! PatriciaSet64.ToSeq left
            yield! PatriciaSet64.ToSeq right
        }

//
[<Sealed>]
//[<StructuredFormatDisplay("")>]
[<DebuggerDisplay("Count = {Count}")>]
[<DebuggerTypeProxy(typedefof<LongSetDebuggerProxy>)>]
type LongSet private (trie : PatriciaSet64) =
    /// The empty LongSet instance.
    static let empty = LongSet Empty

    /// The empty LongSet.
    static member Empty
        with get () = empty

    //
    new (elements : seq<int64>) =
        // Preconditions
        checkNonNull "elements" elements

        // OPTIMIZE : Try to cast the sequence to array or list;
        // if it succeeds use the specialized method for that type for better performance.
        LongSet (PatriciaSet64.OfSeq elements)

    //
    member private __.Trie
        with get () = trie

    /// The number of elements in the LongSet.
    member __.Count
        with get () : int64 =
            PatriciaSet64.Count trie

    /// Is the map empty?
    member __.IsEmpty
        with get () =
            match trie with
            | Empty -> true
            | _ -> false

    /// Tests if an element is in the domain of the LongSet.
    member __.Contains (key : int64) : bool =
        PatriciaSet64.Contains (uint64 key, trie)

    /// The minimum unsigned value stored in the set.
#if FX_NO_DEBUG_DISPLAYS
#else
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
#endif
    member __.MinimumElement
        with get () : int64 =
            int64 <| PatriciaSet64.MinElement trie

    /// The minimum signed value stored in the set.
#if FX_NO_DEBUG_DISPLAYS
#else
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
#endif
    member __.MinimumElementSigned
        with get () : int64 =
            int64 <| PatriciaSet64.MinElementSigned trie

    /// The maximum unsigned value stored in the set.
#if FX_NO_DEBUG_DISPLAYS
#else
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
#endif
    member __.MaximumElement
        with get () : int64 =
            int64 <| PatriciaSet64.MaxElement trie

    /// The maximum signed value stored in the set.
#if FX_NO_DEBUG_DISPLAYS
#else
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
#endif
    member __.MaximumElementSigned
        with get () : int64 =
            int64 <| PatriciaSet64.MaxElementSigned trie

    /// The LongSet containing the given element.
    static member Singleton (element : int64) : LongSet =
        LongSet (Lf <| uint64 element)

    /// Returns a new LongSet with the element added to this LongSet.
    member this.Add (key : int64) : LongSet =
        // If the trie isn't modified, just return this LongSet instead of creating a new one.
        let trie' = PatriciaSet64.Add (uint64 key, trie)
        if trie === trie' then this
        else LongSet (trie')

    /// Removes an element from the domain of the LongSet.
    /// No exception is raised if the element is not present.
    member this.Remove (key : int64) : LongSet =
        // If the trie isn't modified, just return this LongSet instead of creating a new one.
        let trie' = PatriciaSet64.Remove (uint64 key, trie)
        if trie === trie' then this
        else LongSet (trie')

    /// Computes the union of two LongSets.
    member this.Union (otherSet : LongSet) : LongSet =
        // If this set's trie is the same as the other set's trie, we can return immediately.
        if trie === otherSet.Trie then this
        else
            // If the result is the same (physical equality) to one of the inputs,
            // return that input instead of creating a new LongSet.
            let trie' = PatriciaSet64.Union (trie, otherSet.Trie)
            if trie === trie' then this
            elif otherSet.Trie === trie' then otherSet
            else LongSet (trie')

    /// Computes the intersection of two LongSets.
    member this.Intersect (otherSet : LongSet) : LongSet =
        // If this set's trie is the same as the other set's trie, we can return immediately.
        if trie === otherSet.Trie then this
        else
            // If the result is the same (physical equality) to one of the inputs,
            // return that input instead of creating a new LongSet.
            let trie' = PatriciaSet64.Intersect (trie, otherSet.Trie)
            if trie === trie' then this
            elif otherSet.Trie === trie' then otherSet
            else LongSet (trie')

    /// Removes the elements of the specified LongSet from this LongSet.
    member this.Difference (otherSet : LongSet) : LongSet =
        // If this set's trie is the same as the other set's trie, we can return immediately.
        if trie === otherSet.Trie then LongSet.Empty
        else
            // If the result is the same (physical equality) to one of the inputs,
            // return that input instead of creating a new LongSet.
            let trie' = PatriciaSet64.Difference (trie, otherSet.Trie)
            if trie === trie' then this
            elif otherSet.Trie === trie' then otherSet
            else LongSet (trie')

    /// Computes the union of a sequence of LongSets.
    static member internal UnionMany (sets : seq<LongSet>) : LongSet =
        // Preconditions
        checkNonNull "sets" sets

        let result =
            (PatriciaSet64.Empty, sets)
            ||> Seq.fold (fun combinedSetTree set ->
                PatriciaSet64.Union (combinedSetTree, set.Trie))

        // If the resulting trie is empty, return the empty LongSet instance
        // instead of creating a new one to allow for better structure sharing.
        match result with
        | Empty ->
            LongSet.Empty
        | _ ->
            LongSet (result)

    /// Computes the intersection of a sequence of LongSets.
    static member internal IntersectMany (sets : seq<LongSet>) : LongSet =
        // Preconditions
        checkNonNull "sets" sets

        // For compatibility with Set, don't check for an empty sequence --
        // just allow Seq.reduce to raise an exception.
        Seq.reduce (fun s1 s2 -> s1.Intersect s2) sets

    /// Determines if this set is a subset of the given set.
    // IsSubsetOf (otherSet) returns true if all values in this set are in 'otherSet'.
    member __.IsSubsetOf (otherSet : LongSet) : bool =
        PatriciaSet64.IsSubsetOf (trie, otherSet.Trie)

    /// Determines if set1 is a proper subset of set2.
    // IsProperSubsetOf (otherSet) returns true if all values in this set are in 'otherSet',
    // and 'otherSet' contains at least one value which is not in this set.
    member __.IsProperSubsetOf (otherSet : LongSet) : bool =
        PatriciaSet64.IsProperSubsetOf (trie, otherSet.Trie)

    //
    member __.IsSupersetOf (otherSet : LongSet) : bool =
        PatriciaSet64.IsSubsetOf (otherSet.Trie, trie)

    //
    member __.IsProperSupersetOf (otherSet : LongSet) : bool =
        PatriciaSet64.IsProperSubsetOf (otherSet.Trie, trie)

    /// Returns a new LongSet made from the given elements.
    static member internal OfSeq (source : seq<int64>) : LongSet =
        // Preconditions
        checkNonNull "source" source

        LongSet (PatriciaSet64.OfSeq source)

    /// Returns a new LongSet made from the given elements.
    static member internal OfList (source : int64 list) : LongSet =
        // Preconditions
        checkNonNull "source" source

        // OPTIMIZATION : If the source is empty return immediately.
        if List.isEmpty source then
            LongSet.Empty
        else
            LongSet (PatriciaSet64.OfList source)

    /// Returns a new LongSet made from the given elements.
    static member internal OfArray (source : int64[]) : LongSet =
        // Preconditions
        checkNonNull "source" source

        // OPTIMIZATION : If the source is empty return immediately.
        if Array.isEmpty source then
            LongSet.Empty
        else
            LongSet (PatriciaSet64.OfArray source)

    /// Returns a new LongSet made from the given elements.
    static member internal OfSet (source : Set<int64>) : LongSet =
        // Preconditions
        checkNonNull "source" source

        // OPTIMIZATION : If the source is empty return immediately.
        if Set.isEmpty source then
            LongSet.Empty
        else
            LongSet (PatriciaSet64.OfSet source)

    //
    member __.ToSeq () =
        PatriciaSet64.ToSeq trie

    //
    member __.ToList () : int64 list =
        PatriciaSet64.FoldBack ((fun el list -> el :: list), [], trie)

    //
    member __.ToArray () : int64[] =
        let elements = ResizeArray ()
        PatriciaSet64.Iterate (elements.Add, trie)
        elements.ToArray ()

    //
    member __.ToSet () : Set<int64> =
        PatriciaSet64.FoldBack (Set.add, Set.empty, trie)

    //
    member __.Iterate (action : int64 -> unit) : unit =
        PatriciaSet64.Iterate (action, trie)

    //
    member __.IterateBack (action : int64 -> unit) : unit =
        PatriciaSet64.IterateBack (action, trie)

    //
    member __.Fold (folder : 'State -> int64 -> 'State, state : 'State) : 'State =
        PatriciaSet64.Fold (folder, state, trie)

    //
    member __.FoldBack (folder : int64 -> 'State -> 'State, state : 'State) : 'State =
        PatriciaSet64.FoldBack (folder, state, trie)

    //
    member __.TryPick (picker : int64 -> 'T option) : 'T option =
        PatriciaSet64.TryPick (picker, trie)

    //
    member this.Pick (picker : int64 -> 'T option) : 'T =
        match this.TryPick picker with
        | Some value ->
            value
        | None ->
            // TODO : Provide a better error message
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    //
    member this.Exists (predicate : int64 -> bool) : bool =
        this.TryPick (fun el ->
            if predicate el then Some () else None)
        |> Option.isSome

    //
    member this.Forall (predicate : int64 -> bool) : bool =
        this.TryPick (fun el ->
            if not (predicate el) then Some () else None)
        |> Option.isNone

    //
    member this.Choose (chooser : int64 -> int64 option) : LongSet =
        this.Fold ((fun chosenSet el ->
            match chooser el with
            | None ->
                chosenSet
            | Some newValue ->
                chosenSet.Add newValue), LongSet.Empty)

    //
    member this.Filter (predicate : int64 -> bool) : LongSet =
        this.Fold ((fun filteredSet el ->
            if predicate el then
                filteredSet
            else
                filteredSet.Remove el), this)

    //
    member this.Map (mapping : int64 -> int64) : LongSet =
        this.Fold ((fun map el ->
            map.Add (mapping el)), LongSet.Empty)

    //
    // OPTIMIZE : Replace this with an optimized implementation instead of using Fold.
    member this.Partition (predicate : int64 -> bool) : LongSet * LongSet =
        this.Fold ((fun (trueSet, falseSet) el ->
            if predicate el then
                trueSet.Add el,
                falseSet
            else
                trueSet,
                falseSet.Add el), (LongSet.Empty, LongSet.Empty))

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
        | [] -> "longSet []"
        | [h1] ->
            System.Text.StringBuilder()
                .Append("longSet [")
                .Append(LongSet.ElementString h1)
                .Append("]")
                .ToString()
        | [h1; h2] ->
            System.Text.StringBuilder()
                .Append("longSet [")
                .Append(LongSet.ElementString h1)
                .Append("; ")
                .Append(LongSet.ElementString h2)
                .Append("]")
                .ToString()
        | [h1; h2; h3] ->
            System.Text.StringBuilder()
                .Append("longSet [")
                .Append(LongSet.ElementString h1)
                .Append("; ")
                .Append(LongSet.ElementString h2)
                .Append("; ")
                .Append(LongSet.ElementString h3)
                .Append("]")
                .ToString()
        | h1 :: h2 :: h3 :: _ ->
            System.Text.StringBuilder()
                .Append("longSet [")
                .Append(LongSet.ElementString h1)
                .Append("; ")
                .Append(LongSet.ElementString h2)
                .Append("; ")
                .Append(LongSet.ElementString h3)
                .Append("; ... ]")
                .ToString()

    /// Compute the union of two sets.
    static member op_Addition (set1 : LongSet, set2 : LongSet) : LongSet =
        set1.Union set2

    /// Remove the elements of the second set from the first.
    static member op_Subtraction (set1 : LongSet, set2 : LongSet) : LongSet =
        set1.Difference set2

    /// <inherit />
    override __.Equals other =
        match other with
        | :? LongSet as other ->
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

    interface System.IEquatable<LongSet> with
        /// <inherit />
        member __.Equals other =
            trie = other.Trie

    interface System.IComparable with
        /// <inherit />
        member __.CompareTo other =
            compare trie (other :?> LongSet).Trie

    interface System.IComparable<LongSet> with
        /// <inherit />
        member __.CompareTo other =
            compare trie other.Trie

    interface System.Collections.IEnumerable with
        /// <inherit />
        member __.GetEnumerator () =
            (PatriciaSet64.ToSeq trie).GetEnumerator ()
            :> System.Collections.IEnumerator

    interface IEnumerable<int64> with
        /// <inherit />
        member __.GetEnumerator () =
            (PatriciaSet64.ToSeq trie).GetEnumerator ()

    interface ICollection<int64> with
        /// <inherit />
        member __.Count
            with get () =
                Checked.int32 <| PatriciaSet64.Count trie

        /// <inherit />
        member __.IsReadOnly
            with get () = true

        /// <inherit />
        member __.Add _ =
            notSupported "LongSets cannot be mutated."

        /// <inherit />
        member __.Clear () =
            notSupported "LongSets cannot be mutated."

        /// <inherit />
        member __.Contains (item : int64) =
            PatriciaSet64.Contains (uint64 item, trie)

        /// <inherit />
        member this.CopyTo (array, arrayIndex) =
            // Preconditions
            checkNonNull "array" array
            if arrayIndex < 0 then
                raise <| System.ArgumentOutOfRangeException "arrayIndex"

            let count = Checked.int32 <| PatriciaSet64.Count trie
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
            notSupported "LongSets cannot be mutated."

//
and [<Sealed>]
    internal LongSetDebuggerProxy (set : LongSet) =

    [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
    member __.Items
        with get () : int64[] =
            set.ToArray ()


/// Functional programming operators related to the LongSet type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LongSet =
    /// The empty set.
    [<CompiledName("Empty")>]
    let empty = LongSet.Empty

    /// Is the set empty?
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (set : LongSet) : bool =
        // Preconditions
        checkNonNull "set" set

        set.IsEmpty

    /// Returns the number of elements in the set.
    [<CompiledName("Count")>]
    let inline count (set : LongSet) : int64 =
        // Preconditions
        checkNonNull "set" set
        
        set.Count

    /// The set containing the given element.
    [<CompiledName("Singleton")>]
    let inline singleton (key : int64) : LongSet =
        LongSet.Singleton key

    /// Returns a new set with an element added to the set.
    /// No exception is raised if the set already contains the element.
    [<CompiledName("Add")>]
    let inline add (key : int64) (set : LongSet) : LongSet =
        // Preconditions
        checkNonNull "set" set
        
        set.Add key

    /// Returns a new set with the given element removed.
    /// No exception is raised if the set does not contain the element.
    [<CompiledName("Remove")>]
    let inline remove (key : int64) (set : LongSet) : LongSet =
        // Preconditions
        checkNonNull "set" set
        
        set.Remove key

    /// <summary>
    /// Evaluates to &quot;true&quot; if the given element is in the given set.
    /// </summary>
    [<CompiledName("Contains")>]
    let inline contains (key : int64) (set : LongSet) : bool =
        // Preconditions
        checkNonNull "set" set
        
        set.Contains key

    /// Returns the lowest element in the set according to an unsigned integer comparison.
    [<CompiledName("MinElement")>]
    let inline minElement (set : LongSet) : int64 =
        // Preconditions
        checkNonNull "set" set

        set.MinimumElement

    /// Returns the lowest element in the set according to a signed integer comparison.
    [<CompiledName("MinElementSigned")>]
    let inline minElementSigned (set : LongSet) : int64 =
        // Preconditions
        checkNonNull "set" set

        set.MinimumElementSigned

    /// Returns the highest element in the set according to an unsigned integer comparison.
    [<CompiledName("MaxElement")>]
    let inline maxElement (set : LongSet) : int64 =
        // Preconditions
        checkNonNull "set" set

        set.MaximumElement

    /// Returns the highest element in the set according to a signed integer comparison.
    [<CompiledName("MaxElementSigned")>]
    let inline maxElementSigned (set : LongSet) : int64 =
        // Preconditions
        checkNonNull "set" set

        set.MaximumElementSigned

    /// Computes the union of the two sets.
    [<CompiledName("Union")>]
    let inline union (set1 : LongSet) (set2 : LongSet) : LongSet =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2
        
        set1.Union set2

    /// Computes the union of a sequence of sets.
    [<CompiledName("UnionMany")>]
    let unionMany sets =
        // Preconditions checked by the target method.
        LongSet.UnionMany sets

    /// Computes the intersection of the two sets.
    [<CompiledName("Intersect")>]
    let inline intersect (set1 : LongSet) (set2 : LongSet) : LongSet =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2
        
        set1.Intersect set2

    /// Computes the intersection of a sequence of sets.
    [<CompiledName("IntersectMany")>]
    let intersectMany sets =
        // Preconditions checked by the target method.
        LongSet.IntersectMany sets

    /// Returns a new set with the elements of the second set removed from the first.
    [<CompiledName("Difference")>]
    let inline difference (set1 : LongSet) (set2 : LongSet) : LongSet =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2
        
        set1.Difference set2

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the first set are in the second.
    /// </summary>
    [<CompiledName("IsSubset")>]
    let inline isSubset (set1 : LongSet) (set2 : LongSet) : bool =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.IsSubsetOf set2

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the first set are in the second,
    /// and at least one element of the second is not in the first.
    /// </summary>
    [<CompiledName("IsProperSubset")>]
    let inline isProperSubset (set1 : LongSet) (set2 : LongSet) : bool =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2
        
        set1.IsProperSubsetOf set2

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the second set are in the first.
    /// </summary>
    [<CompiledName("IsSuperset")>]
    let inline isSuperset (set1 : LongSet) (set2 : LongSet) : bool =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2
        
        set1.IsSupersetOf set2

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the second set are in the first,
    /// and at least one element of the first is not in the second.
    /// </summary>
    [<CompiledName("IsProperSuperset")>]
    let inline isProperSuperset (set1 : LongSet) (set2 : LongSet) : bool =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.IsProperSupersetOf set2

    /// Builds a new collection from the given enumerable object.
    [<CompiledName("OfSeq")>]
    let (*inline*) ofSeq source : LongSet =
        // Preconditions are checked by the member.
        LongSet.OfSeq source

    /// Builds a set that contains the same elements as the given list.
    [<CompiledName("OfList")>]
    let (*inline*) ofList source : LongSet =
        // Preconditions are checked by the member.
        LongSet.OfList source

    /// Builds a set that contains the same elements as the given array.
    [<CompiledName("OfArray")>]
    let (*inline*) ofArray source : LongSet =
        // Preconditions are checked by the member.
        LongSet.OfArray source

    /// Builds an LongSet that contains the same elements as the given Set.
    [<CompiledName("OfSet")>]
    let (*inline*) ofSet source : LongSet =
        // Preconditions are checked by the member.
        LongSet.OfSet source

    /// Returns an ordered view of the collection as an enumerable object.
    [<CompiledName("ToSeq")>]
    let inline toSeq (set : LongSet) =
        // Preconditions
        checkNonNull "set" set
        
        set.ToSeq ()

    /// Builds a list that contains the elements of the set in order.
    [<CompiledName("ToList")>]
    let inline toList (set : LongSet) =
        // Preconditions
        checkNonNull "set" set
        
        set.ToList ()

    /// Builds an array that contains the elements of the set in order.
    [<CompiledName("ToArray")>]
    let inline toArray (set : LongSet) =
        // Preconditions
        checkNonNull "set" set
        
        set.ToArray ()

    /// Builds a Set that contains the same elements as the given LongSet.
    [<CompiledName("ToSet")>]
    let inline toSet (set : LongSet) =
        // Preconditions
        checkNonNull "set" set

        set.ToSet ()

    /// Applies the given function to each element of the set,
    /// in order according to the comparison function.
    [<CompiledName("Iterate")>]
    let inline iter (action : int64 -> unit) (set : LongSet) =
        // Preconditions
        checkNonNull "set" set
        
        set.Iterate action

    /// Applies the given function to each element of the set,
    /// in reverse order according to the comparison function.
    [<CompiledName("IterateBack")>]
    let inline iterBack (action : int64 -> unit) (set : LongSet) =
        // Preconditions
        checkNonNull "set" set
        
        set.IterateBack action

    /// Applies the given accumulating function to each element of the set,
    /// in order according to the comparison function.
    [<CompiledName("Fold")>]
    let inline fold (folder : 'State -> int64 -> 'State) (state : 'State) (set : LongSet) =
        // Preconditions
        checkNonNull "set" set
        
        set.Fold (folder, state)

    /// Applies the given accumulating function to each element of the set,
    /// in reverse order according to the comparison function.
    [<CompiledName("FoldBack")>]
    let inline foldBack (folder : int64 -> 'State -> 'State) (set : LongSet) (state : 'State) =
        // Preconditions
        checkNonNull "set" set

        set.FoldBack (folder, state)

    /// <summary>
    /// Applies the given function to each element of the set.
    /// Returns the set comprised of the results <c>x</c> for each element where the
    /// function returns <c>Some(x)</c>.
    /// </summary>
    [<CompiledName("Choose")>]
    let inline choose (chooser : int64 -> int64 option) (set : LongSet) : LongSet =
        // Preconditions
        checkNonNull "set" set
        
        set.Choose chooser

    /// <summary>
    /// Returns a new collection containing only the elements of the collection
    /// for which the given predicate returns &quot;true&quot;.
    /// </summary>
    [<CompiledName("Filter")>]
    let inline filter (predicate : int64 -> bool) (set : LongSet) : LongSet =
        // Preconditions
        checkNonNull "set" set
        
        set.Filter predicate

    /// Returns a new collection containing the results of applying the given function
    /// to each element of the input set.
    [<CompiledName("Map")>]
    let inline map (mapping : int64 -> int64) (set : LongSet) : LongSet =
        // Preconditions
        checkNonNull "set" set
        
        set.Map mapping

    /// <summary>
    /// Splits the set into two sets containing the elements for which the given
    /// predicate returns &quot;true&quot; and &quot;false&quot;, respectively.
    /// </summary>
    [<CompiledName("Partition")>]
    let inline partition (predicate : int64 -> bool) (set : LongSet) : LongSet * LongSet =
        // Preconditions
        checkNonNull "set" set
        
        set.Partition predicate

    /// Tests if any element of the collection satisfies the given predicate.
    [<CompiledName("Exists")>]
    let inline exists (predicate : int64 -> bool) (set : LongSet) : bool =
        // Preconditions
        checkNonNull "set" set
        
        set.Exists predicate

    /// Tests if all elements of the collection satisfy the given predicate.
    [<CompiledName("Forall")>]
    let inline forall (predicate : int64 -> bool) (set : LongSet) : bool =
        // Preconditions
        checkNonNull "set" set

        set.Forall predicate

    /// <summary>
    /// Applies the given function to each element of the set, returning the first result
    /// where the function returns <c>Some(x)</c>. If the function never returns <c>Some(x)</c>
    /// then None is returned.
    /// </summary>
    [<CompiledName("TryPick")>]
    let inline tryPick (picker : int64 -> 'T option) (set : LongSet) : 'T option =
        // Preconditions
        checkNonNull "set" set
        
        set.TryPick picker

    /// <summary>
    /// Applies the given function to each element of the set, returning the first result
    /// where the function returns <c>Some(x)</c>. If the function never returns <c>Some(x)</c>
    /// then a KeyNotFoundException is raised.
    /// </summary>
    [<CompiledName("Pick")>]
    let inline pick (picker : int64 -> 'T option) (set : LongSet) : 'T =
        // Preconditions
        checkNonNull "set" set

        set.Pick picker


#if PROTO_COMPILER

/// An LongSet whose values are tagged with a unit-of-measure type.
[<MeasureAnnotatedAbbreviation>]
type LongTagSet<[<Measure>] 'Tag> = LongSet

/// Functional programming operators related to the TagMap<_> type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LongTagSet =
    /// Retypes a value without emitting any IL instructions.
    /// WARNING: This should be used with EXTREME CAUTION.
    [<NoDynamicInvocation>]
    [<CompiledName("RetypeInlined")>]
    let inline private retype<'T,'U> (x:'T) : 'U = (# "" x : 'U #)

    /// The empty set
    [<CompiledName("Empty")>]
    let empty<[<Measure>] 'Tag> : LongTagSet<'Tag> =
        retype LongSet.Empty

    /// Is the set empty?
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (set : LongTagSet<'Tag>) : bool =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.IsEmpty

    /// Returns the number of elements in the set
    [<CompiledName("Count")>]
    let inline count (set : LongTagSet<'Tag>) : int64 =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set
        
        set.Count

    /// The set containing the given element.
    [<CompiledName("Singleton")>]
    let inline singleton (element : int64<'Tag>) : LongTagSet<'Tag> =
        LongSet.Singleton (int64 element)
        |> retype

    /// Returns a new set with an element added to the set.
    /// No exception is raised if the set already contains the element.
    [<CompiledName("Add")>]
    let inline add (key : int64<'Tag>) (set : LongTagSet<'Tag>) : LongTagSet<'Tag> =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Add (int64 key)
        |> retype

    /// Returns a new set with the given element removed.
    /// No exception is raised if the set does not contain the element.
    [<CompiledName("Remove")>]
    let inline remove (key : int64<'Tag>) (set : LongTagSet<'Tag>) : LongTagSet<'Tag> =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Remove (int64 key)
        |> retype

    /// <summary>
    /// Evaluates to &quot;true&quot; if the given element is in the given set.
    /// </summary>
    [<CompiledName("Contains")>]
    let inline contains (key : int64<'Tag>) (set : LongTagSet<'Tag>) : bool =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Contains (int64 key)

    /// Returns the lowest element in the set according to an unsigned integer comparison.
    [<CompiledName("MinElement")>]
    let inline minElement (set : LongTagSet<'Tag>) : int64<'Tag> =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.MinimumElement
        |> retype

    /// Returns the lowest element in the set according to a signed integer comparison.
    [<CompiledName("MinElementSigned")>]
    let inline minElementSigned (set : LongTagSet<'Tag>) : int64<'Tag> =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.MinimumElementSigned
        |> retype

    /// Returns the highest element in the set according to an unsigned integer comparison.
    [<CompiledName("MaxElement")>]
    let inline maxElement (set : LongTagSet<'Tag>) : int64<'Tag> =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.MaximumElement
        |> retype

    /// Returns the highest element in the set according to a signed integer comparison.
    [<CompiledName("MaxElementSigned")>]
    let inline maxElementSigned (set : LongTagSet<'Tag>) : int64<'Tag> =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.MaximumElementSigned
        |> retype

    /// Computes the union of the two sets.
    [<CompiledName("Union")>]
    let inline union (set1 : LongTagSet<'Tag>) (set2 : LongTagSet<'Tag>) : LongTagSet<'Tag> =
        // Retype as LongSet
        let set1 : LongSet = retype set1
        let set2 : LongSet = retype set2

        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.Union set2
        |> retype

    /// Computes the union of a sequence of sets.
    [<CompiledName("UnionMany")>]
    let unionMany (sets : seq<LongTagSet<'Tag>>) : LongTagSet<'Tag> =
        // Retype as seq<LongSet>
        let sets : seq<LongSet> = retype sets

        // Preconditions checked by the target method.
        LongSet.UnionMany sets
        |> retype

    /// Computes the intersection of the two sets.
    [<CompiledName("Intersect")>]
    let inline intersect (set1 : LongTagSet<'Tag>) (set2 : LongTagSet<'Tag>) : LongTagSet<'Tag> =
        // Retype as LongSet
        let set1 : LongSet = retype set1
        let set2 : LongSet = retype set2

        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.Intersect set2
        |> retype

    /// Computes the intersection of a sequence of sets.
    [<CompiledName("IntersectMany")>]
    let intersectMany (sets : seq<LongTagSet<'Tag>>) : LongTagSet<'Tag> =
        // Retype as seq<LongSet>
        let sets : seq<LongSet> = retype sets

        // Preconditions checked by the target method.
        LongSet.IntersectMany sets
        |> retype

    /// Returns a new set with the elements of the second set removed from the first.
    [<CompiledName("Difference")>]
    let inline difference (set1 : LongTagSet<'Tag>) (set2 : LongTagSet<'Tag>) : LongTagSet<'Tag> =
        // Retype as LongSet
        let set1 : LongSet = retype set1
        let set2 : LongSet = retype set2

        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.Difference set2
        |> retype

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the first set are in the second.
    /// </summary>
    [<CompiledName("IsSubset")>]
    let inline isSubset (set1 : LongTagSet<'Tag>) (set2 : LongTagSet<'Tag>) : bool =
        // Retype as LongSet
        let set1 : LongSet = retype set1
        let set2 : LongSet = retype set2

        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.IsSubsetOf set2

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the first set are in the second,
    /// and at least one element of the second is not in the first.
    /// </summary>
    [<CompiledName("IsProperSubset")>]
    let inline isProperSubset (set1 : LongTagSet<'Tag>) (set2 : LongTagSet<'Tag>) : bool =
        // Retype as LongSet
        let set1 : LongSet = retype set1
        let set2 : LongSet = retype set2

        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.IsProperSubsetOf set2

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the second set are in the first.
    /// </summary>
    [<CompiledName("IsSuperset")>]
    let inline isSuperset (set1 : LongTagSet<'Tag>) (set2 : LongTagSet<'Tag>) : bool =
        // Retype as LongSet
        let set1 : LongSet = retype set1
        let set2 : LongSet = retype set2

        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.IsSupersetOf set2

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the second set are in the first,
    /// and at least one element of the first is not in the second.
    /// </summary>
    [<CompiledName("IsProperSuperset")>]
    let inline isProperSuperset (set1 : LongTagSet<'Tag>) (set2 : LongTagSet<'Tag>) : bool =
        // Retype as LongSet
        let set1 : LongSet = retype set1
        let set2 : LongSet = retype set2

        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.IsProperSupersetOf set2

    /// Builds a new collection from the given enumerable object.
    [<CompiledName("OfSeq")>]
    let (*inline*) ofSeq (source : seq<int64<'Tag>>) : LongTagSet<'Tag> =
        // Preconditions are checked by the member.
        LongSet.OfSeq (retype source)
        |> retype

    /// Builds a set that contains the same elements as the given list.
    [<CompiledName("OfList")>]
    let (*inline*) ofList (source : int64<'Tag> list) : LongTagSet<'Tag> =
        // Preconditions are checked by the member.
        LongSet.OfList (retype source)
        |> retype

    /// Builds a set that contains the same elements as the given array.
    [<CompiledName("OfArray")>]
    let (*inline*) ofArray (source : int64<'Tag>[]) : LongTagSet<'Tag> =
        // Preconditions are checked by the member.
        LongSet.OfArray (retype source)
        |> retype

    /// Builds an set that contains the same elements as the given Set.
    [<CompiledName("OfSet")>]
    let (*inline*) ofSet (source : Set<int64<'Tag>>) : LongTagSet<'Tag> =
        // Preconditions are checked by the member.
        LongSet.OfSet (retype source)
        |> retype

    /// Returns an ordered view of the collection as an enumerable object.
    [<CompiledName("ToSeq")>]
    let inline toSeq (set : LongTagSet<'Tag>) : seq<int64<'Tag>> =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.ToSeq ()
        |> retype

    /// Builds a list that contains the elements of the set in order.
    [<CompiledName("ToList")>]
    let inline toList (set : LongTagSet<'Tag>) : int64<'Tag> list =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.ToList ()
        |> retype

    /// Builds an array that contains the elements of the set in order.
    [<CompiledName("ToArray")>]
    let inline toArray (set : LongTagSet<'Tag>) : int64<'Tag>[] =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.ToArray ()
        |> retype

    /// Builds a Set that contains the same elements as the given LongTagSet.
    [<CompiledName("ToSet")>]
    let inline toSet (set : LongTagSet<'Tag>) : Set<int64<'Tag>> =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.ToSet ()
        |> retype

    /// Applies the given function to each element of the set,
    /// in order according to the comparison function.
    [<CompiledName("Iterate")>]
    let inline iter (action : int64<'Tag> -> unit) (set : LongTagSet<'Tag>) =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Iterate (retype action)

    /// Applies the given function to each element of the set,
    /// in reverse order according to the comparison function.
    [<CompiledName("IterateBack")>]
    let inline iterBack (action : int64<'Tag> -> unit) (set : LongTagSet<'Tag>) =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.IterateBack (retype action)

    /// Applies the given accumulating function to each element of the set,
    /// in order according to the comparison function.
    [<CompiledName("Fold")>]
    let inline fold (folder : 'State -> int64<'Tag> -> 'State) (state : 'State) (set : LongTagSet<'Tag>) =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Fold (retype folder, state)

    /// Applies the given accumulating function to each element of the set,
    /// in reverse order according to the comparison function.
    [<CompiledName("FoldBack")>]
    let inline foldBack (folder : int64<'Tag> -> 'State -> 'State) (set : LongTagSet<'Tag>) (state : 'State) =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.FoldBack (retype folder, state)

    /// <summary>
    /// Applies the given function to each element of the set.
    /// Returns the set comprised of the results <c>x</c> for each element where the
    /// function returns <c>Some(x)</c>.
    /// </summary>
    [<CompiledName("Choose")>]
    let inline choose (chooser : int<'Tag1> -> int<'Tag2> option) (set : LongTagSet<'Tag1>) : LongTagSet<'Tag2> =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Choose (retype chooser)
        |> retype

    /// <summary>
    /// Returns a new collection containing only the elements of the collection
    /// for which the given predicate returns &quot;true&quot;.
    /// </summary>
    [<CompiledName("Filter")>]
    let inline filter (predicate : int64<'Tag> -> bool) (set : LongTagSet<'Tag>) : LongTagSet<'Tag> =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Filter (retype predicate)
        |> retype

    /// Returns a new collection containing the results of applying the given function
    /// to each element of the input set.
    [<CompiledName("Map")>]
    let inline map (mapping : int<'Tag1> -> int<'Tag2>) (set : LongTagSet<'Tag1>) : LongTagSet<'Tag2> =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Map (retype mapping)
        |> retype

    /// <summary>
    /// Splits the set into two sets containing the elements for which the given
    /// predicate returns &quot;true&quot; and &quot;false&quot;, respectively.
    /// </summary>
    [<CompiledName("Partition")>]
    let inline partition (predicate : int64<'Tag> -> bool) (set : LongTagSet<'Tag>) : LongTagSet<'Tag> * LongTagSet<'Tag> =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        let set1, set2 = set.Partition (retype predicate)
        (retype set1), (retype set2)

    /// Tests if any element of the collection satisfies the given predicate.
    [<CompiledName("Exists")>]
    let inline exists (predicate : int64<'Tag> -> bool) (set : LongTagSet<'Tag>) : bool =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Exists (retype predicate)

    /// Tests if all elements of the collection satisfy the given predicate.
    [<CompiledName("Forall")>]
    let inline forall (predicate : int64<'Tag> -> bool) (set : LongTagSet<'Tag>) : bool =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Forall (retype predicate)

    /// <summary>
    /// Applies the given function to each element of the set, returning the first result
    /// where the function returns <c>Some(x)</c>. If the function never returns <c>Some(x)</c>
    /// then None is returned.
    /// </summary>
    [<CompiledName("TryPick")>]
    let inline tryPick (picker : int64<'Tag> -> 'T option) (set : LongTagSet<'Tag>) : 'T option =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.TryPick (retype picker)

    /// <summary>
    /// Applies the given function to each element of the set, returning the first result
    /// where the function returns <c>Some(x)</c>. If the function never returns <c>Some(x)</c>
    /// then a KeyNotFoundException is raised.
    /// </summary>
    [<CompiledName("Pick")>]
    let inline pick (picker : int64<'Tag> -> 'T option) (set : LongTagSet<'Tag>) : 'T =
        // Retype as LongSet
        let set : LongSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Pick (retype picker)

#endif


