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
    let [<Literal>] defaultTraversalStackSize = 32


type internal Mask = uint32
type internal Key = uint32
type internal Prefix = uint32

/// Bitwise operations necessary for implementing Patricia tries.
module internal BitOps =
    #if LITTLE_ENDIAN_TRIES
    
    //
    let inline private leastSignificantSetBit (x : uint32) : uint32 =
        x &&& (uint32 -(int x))

    /// Finds the last (least-significant) bit at which p0 and p1 disagree.
    /// Returns a power-of-two value containing this (and only this) bit.
    let inline branchingBit (p0 : Prefix, p1 : Prefix) : Mask =
        leastSignificantSetBit (p0 ^^^ p1)

    /// Clears the indicated bit and sets all lower bits.
    let inline mask (key : Key, mask : Mask) : Prefix =
        key &&& (mask - 1u)

    //
    let (*inline*) shorter (m1 : Mask, m2 : Mask) : bool =
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
        //x &&& ~~~(x >>> 1)
        // OPTIMIZATION : p AND (NOT q) <-> p XOR q
        x6 ^^^ (x6 >>> 1)

    /// Finds the first (most-significant) bit at which p0 and p1 disagree.
    /// Returns a power-of-two value containing this (and only this) bit.
    let inline branchingBit (p0 : Prefix, p1 : Prefix) : Mask =
        mostSignificantSetBit (p0 ^^^ p1)

    /// Clears the indicated bit and sets all lower bits.
    let inline mask (key : Key, mask : Mask) : Prefix =
        key &&& (~~~(mask - 1u) ^^^ mask)

    //
    let inline shorter (m1 : Mask, m2 : Mask) : bool =
        // NOTE : This must be an *unsigned* comparison for the results to be correct.
        m1 > m2

    #endif

    //
    let inline matchPrefix (key : Key, prefix : Prefix, mask' : Mask) : bool =
        mask (key, mask') = prefix

    /// <summary>Determines if all specified bits are cleared (not set) in a value.</summary>
    /// <param name="value">The value to test.</param>
    /// <param name="bitValue">The bits to test in 'value'.</param>
    /// <returns>true if all bits which are set in 'bitValue' are *not* set in 'value'.</returns>
    let inline zeroBit (key : Key, mask : Mask) : bool =
        key &&& mask = 0u


open PatriciaTrieConstants
open BitOps


/// A Patricia trie implementation.
/// Used as the underlying data structure for IntSet (and TagSet).
[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type private PatriciaSet =
    | Empty
    // Key
    | Lf of uint32
    // Prefix * Mask * Left-Child * Right-Child
    | Br of uint32 * uint32 * PatriciaSet * PatriciaSet

    /// Determines if the set contains the given value.
    static member Contains (key, set : PatriciaSet) =
        match set with
        | Empty ->
            false
        | (Lf j) ->
            key = j
        | Br (_, m, t0, t1) ->
            PatriciaSet.Contains
                (key, (if zeroBit (key, m) then t0 else t1))

    /// The number of items (i.e., the cardinality) of the set.
    static member Count (set : PatriciaSet) : int =
        match set with
        | Empty -> 0
        | Lf _ -> 1
        | Br (_,_,_,_) as t ->
            /// The stack of trie nodes pending traversal.
            let stack = Stack (defaultTraversalStackSize)

            // Add the initial tree to the stack.
            stack.Push t

            // Traverse the tree, counting the elements by using the mutable stack.
            let mutable count = 0u

            while stack.Count <> 0 do
                match stack.Pop () with
                | Empty -> ()
                | Lf _ ->
                    count <- count + 1u
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf _, Lf _) ->
                    count <- count + 2u
                    
                | Br (_, _, Lf _, child)
                | Br (_, _, child, Lf _) ->
                    count <- count + 1u
                    stack.Push child

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push right
                    stack.Push left

            // Return the computed element count.
            int count

    /// Retrieve the minimum element of the set.
    static member MinElement (set : PatriciaSet) =
        match set with
        | Empty ->
            invalidArg "set" "The set is empty."
        | Lf j -> j
        | Br (_, _, t0, _) ->
            PatriciaSet.MinElement t0

    /// Retrieve the minimum element of the set, treating the
    /// elements of the set as signed values.
    static member MinElementSigned (set : PatriciaSet) =
        match set with
        | Empty ->
            invalidArg "set" "The set is empty."
        | Lf j -> j
        | Br (_, m, t0, t1) ->
            // OPTIMIZE : Just use a bitmask here to check the top bit?
            if (int m) < 0 then t1 else t0
            |> PatriciaSet.MinElement

    /// Retrieve the maximum element of the set.
    static member MaxElement (set : PatriciaSet) =
        match set with
        | Empty ->
            invalidArg "set" "The set is empty."
        | Lf j -> j
        | Br (_, _, _, t1) ->
            PatriciaSet.MaxElement t1

    /// Retrieve the maximum element of the set, treating the
    /// elements of the set as signed values.
    static member MaxElementSigned (set : PatriciaSet) =
        match set with
        | Empty ->
            invalidArg "set" "The set is empty."
        | Lf j -> j
        | Br (_, m, t0, t1) ->
            // OPTIMIZE : Just use a bitmask here to check the top bit?
            if (int m) < 0 then t0 else t1
            |> PatriciaSet.MaxElement

    /// Remove an item from the set.
    static member Remove (key, set : PatriciaSet) =
        match set with
        | Empty ->
            Empty
        | (Lf j) as t ->
            if j = key then Empty
            else t
        
        | Br (p, m, t0, t1) as t ->
            if matchPrefix (key, p, m) then
                if zeroBit (key, m) then
                    match PatriciaSet.Remove (key, t0) with
                    | Empty -> t1
                    | t0 ->
                        Br (p, m, t0, t1)
                else
                    match PatriciaSet.Remove (key, t1) with
                    | Empty -> t0
                    | t1 ->
                        Br (p, m, t0, t1)
            else t

    //
    static member inline private Join (p0, t0 : PatriciaSet, p1, t1) =
        let m = branchingBit (p0, p1)
        let p = mask (p0, m)
        if zeroBit (p0, m) then
            Br (p, m, t0, t1)
        else
            Br (p, m, t1, t0)

    //
    static member Add (key, set) =
        match set with
        | Empty ->
            Lf key
        | (Lf j) as t ->
            if j = key then
                // The value already exists in the set, so return the
                // existing set without modifying it.
                t
            else
                PatriciaSet.Join (key, Lf key, j, t)

        | Br (p, m, t0, t1) as t ->
            if matchPrefix (key, p, m) then
                if zeroBit (key, m) then
                    let left = PatriciaSet.Add (key, t0)
                    Br (p, m, left, t1)
                else
                    let right = PatriciaSet.Add (key, t1)
                    Br (p, m, t0, right)
            else
                PatriciaSet.Join (key, Lf key, p, t)

    //
    static member Union (s, t) : PatriciaSet =
        match s, t with
        | (Br (p, m, s0, s1) as s), (Br (q, n, t0, t1) as t) ->
            if m = n then
                if p = q then
                    // The trees have the same prefix. Merge the subtrees.
                    let left = PatriciaSet.Union (s0, t0)
                    let right = PatriciaSet.Union (s1, t1)
                    Br (p, m, left, right)
                else
                    // The prefixes disagree.
                    PatriciaSet.Join (p, s, q, t)
            
            #if LITTLE_ENDIAN_TRIES
            elif m < n then
            #else
            elif m > n then
            #endif
                if matchPrefix (q, p, m) then
                    // q contains p. Merge t with a subtree of s.
                    if zeroBit (q, m) then
                        let left = PatriciaSet.Union (s0, t)
                        Br (p, m, left, s1)
                    else
                        let right = PatriciaSet.Union (s1, t)
                        Br (p, m, s0, right)
                else
                    // The prefixes disagree.
                    PatriciaSet.Join (p, s, q, t)

            else
                if matchPrefix (p, q, n) then
                    // p contains q. Merge s with a subtree of t.
                    if zeroBit (p, n) then
                        let left = PatriciaSet.Union (s, t0)
                        Br (q, n, left, t1)
                    else
                        let right = PatriciaSet.Union (s, t1)
                        Br (q, n, t0, right)
                else
                    // The prefixes disagree.
                    PatriciaSet.Join (p, s, q, t)

        | (Br (p, m, s0, s1) as s), (Lf k as t) ->
            if matchPrefix (k, p, m) then
                if zeroBit (k, m) then
                    let left = PatriciaSet.Add (k, s0)
                    Br (p, m, left, s1)
                else
                    let right = PatriciaSet.Add (k, s1)
                    Br (p, m, s0, right)
            else
                PatriciaSet.Join (k, t, p, s)

        | (Br (_,_,_,_) as s), Empty ->
            s
        | Lf k, t ->
            PatriciaSet.Add (k, t)
        | Empty, t ->
            t

    /// Compute the intersection of two PatriciaMaps.
    /// If both maps contain a binding with the same key, the binding from
    /// the first map will be used.
    static member Intersect (s, t) : PatriciaSet =
        match s, t with
        | (Br (p, m, s0, s1) as s), (Br (q, n, t0, t1) as t) ->
            if m = n then
                if p <> q then Empty
                else
                    let left = PatriciaSet.Intersect (s0, t0)
                    let right = PatriciaSet.Intersect (s1, t1)
                    match left, right with
                    | Empty, t
                    | t, Empty -> t
                    | left, right ->
                        Br (p, m, left, right)

            #if LITTLE_ENDIAN_TRIES
            elif m < n then
            #else
            elif m > n then
            #endif
                if matchPrefix (q, p, m) then
                    if zeroBit (q, m) then
                        PatriciaSet.Intersect (s0, t)
                    else
                        PatriciaSet.Intersect (s1, t)
                else
                    Empty

            else
                if matchPrefix (p, q, n) then
                    if zeroBit (p, n) then
                        PatriciaSet.Intersect (s, t0)
                    else
                        PatriciaSet.Intersect (s, t1)
                else
                    Empty

        | Br (_, m, s0, s1), (Lf k as t) ->
            let s' = if zeroBit (k, m) then s0 else s1
            if PatriciaSet.Contains (k, s') then t
            else Empty
            
        | Br (_,_,_,_), Empty ->
            Empty
            
        | (Lf k as s), t ->
            if PatriciaSet.Contains (k, t) then s
            else Empty

        | Empty, _ ->
            Empty

    //
    static member private Delete (key, map) : PatriciaSet =
        match map with
        | Empty ->
            Empty
        | Lf j as t ->
            if key = j then Empty
            else t
        | Br (p, m, t0, t1) as t ->
            if matchPrefix (key, p, m) then
                if zeroBit (key, m) then
                    match PatriciaSet.Delete (key, t0) with
                    | Empty -> t1
                    | left ->
                        Br (p, m, left, t1)
                else
                    match PatriciaSet.Delete (key, t1) with
                    | Empty -> t0
                    | right ->
                        Br (p, m, t0, right)
            else t

    /// Compute the difference of two PatriciaMaps.
    static member Difference (s, t) : PatriciaSet =
        match s, t with
        | (Br (p, m, s0, s1) as s), (Br (q, n, t0, t1) as t) ->
            if m = n then
                if p <> q then s
                else
                    let left = PatriciaSet.Difference (s0, t0)
                    let right = PatriciaSet.Difference (s1, t1)
                    match left, right with
                    | Empty, t
                    | t, Empty -> t
                    | left, right ->
                        Br (p, m, left, right)

            #if LITTLE_ENDIAN_TRIES
            elif m < n then
            #else
            elif m > n then
            #endif
                if matchPrefix (q, p, m) then
                    if zeroBit (q, m) then
                        match PatriciaSet.Difference (s0, t) with
                        | Empty -> s1
                        | left ->
                            Br (p, m, left, s1)
                    else
                        match PatriciaSet.Difference (s1, t) with
                        | Empty -> s0
                        | right ->
                            Br (p, m, s0, right)
                else s

            else
                if matchPrefix (p, q, n) then
                    if zeroBit (p, n) then
                        PatriciaSet.Difference (s, t0)
                    else
                        PatriciaSet.Difference (s, t1)
                else s

        | Br (p, m, s0, s1), Lf k ->
            if matchPrefix (k, p, m) then
                if zeroBit (k, m) then
                    match PatriciaSet.Delete (k, s0) with
                    | Empty -> s1
                    | left ->
                        Br (p, m, left, s1)
                else
                    match PatriciaSet.Delete (k, s1) with
                    | Empty -> s0
                    | right ->
                        Br (p, m, s0, right)
            else s
            
        | Br (_,_,_,_) as s, Empty ->
            s
        | (Lf k as s), t ->
            if PatriciaSet.Contains (k, t) then Empty
            else s
        | Empty, _ ->
            Empty

    /// Is 'set1' a subset of 'set2'?
    // IsSubsetOf (set1, set2) returns true if all keys in set1 are in set2.
    static member IsSubset (set1 : PatriciaSet, set2 : PatriciaSet) : bool =
        match set1, set2 with
        | (Br (p1, m1, l1, r1) as t1), (Br (p2, m2, l2, r2) as t2) ->
            if shorter (m1, m2) then
                false
            elif shorter (m2, m1) then
                matchPrefix (p1, p2, m2) && (
                    if zeroBit (p1, m2) then
                        PatriciaSet.IsSubset (t1, l2)
                    else
                        PatriciaSet.IsSubset (t1, r2))
            else
                p1 = p2
                && PatriciaSet.IsSubset (l1, l2)
                && PatriciaSet.IsSubset (r1, r2)
                
        | Br (_,_,_,_), _ ->
            false
        | Lf k1, Lf k2 ->
            k1 = k2
        | (Lf k as t1), Br (p, m, l, r) ->
            if not <| matchPrefix (k, p, m) then
                false
            elif zeroBit (k, m) then
                PatriciaSet.IsSubset (t1, l)
            else
                PatriciaSet.IsSubset (t1, r)
        | Lf _, Empty ->
            false
        | Empty, _ ->
            true

    /// Is 'set1' a superset of 'set2'?
    static member IsSuperset (set1 : PatriciaSet, set2 : PatriciaSet) : bool =
        PatriciaSet.IsSubset (set2, set1)

    //
    static member private SubmapCmp (t1 : PatriciaSet, t2 : PatriciaSet) : int =
        match t1, t2 with
        | (Br (p1, m1, l1, r1) as t1), (Br (p2, m2, l2, r2) as t2) ->
            if shorter (m1, m2) then 1
            elif shorter (m2, m1) then
                if not <| matchPrefix (p1, p2, m2) then 1
                elif zeroBit (p1, m2) then
                    PatriciaSet.SubmapCmp (t1, l2)
                else
                    PatriciaSet.SubmapCmp (t1, r2)
            elif p1 = p2 then
                let left = PatriciaSet.SubmapCmp (l1, l2)
                let right = PatriciaSet.SubmapCmp (r1, r2)
                match left, right with
                | 1, _
                | _, 1 -> 1
                | 0, 0 -> 0
                | _ -> -1
            else
                // The maps are disjoint.
                1

        | Br (_,_,_,_), _ -> 1
        | Lf x, Lf y ->
            if x = y then 0
            else 1  // The maps are disjoint.
        | Lf x, t ->
            if PatriciaSet.Contains (x, t) then -1
            else 1  // The maps are disjoint.

        | Empty, Empty -> 0
        | Empty, _ -> -1

    /// Is 'set1' a proper subset of 'set2'?
    static member IsProperSubset (set1 : PatriciaSet, set2 : PatriciaSet) : bool =
        match PatriciaSet.SubmapCmp (set1, set2) with
        | -1 -> true
        | _ -> false

    /// Is 'set1' a proper superset of 'set2'?
    static member IsProperSuperset (set1 : PatriciaSet, set2 : PatriciaSet) : bool =
        match PatriciaSet.SubmapCmp (set2, set1) with
        | 1 -> true
        | _ -> false

    //
    static member OfSeq (source : seq<int>) : PatriciaSet =
        (Empty, source)
        ||> Seq.fold (fun trie el ->
            PatriciaSet.Add (uint32 el, trie))

    //
    static member OfList (source : int list) : PatriciaSet =
        // Preconditions
        checkNonNull "source" source

        (Empty, source)
        ||> List.fold (fun trie el ->
            PatriciaSet.Add (uint32 el, trie))

    //
    static member OfArray (source : int[]) : PatriciaSet =
        // Preconditions
        checkNonNull "source" source

        (Empty, source)
        ||> Array.fold (fun trie el ->
            PatriciaSet.Add (uint32 el, trie))

    //
    static member OfSet (source : Set<int>) : PatriciaSet =
        // Preconditions
        checkNonNull "source" source

        (Empty, source)
        ||> Set.fold (fun trie el ->
            PatriciaSet.Add (uint32 el, trie))

    //
    static member Iterate (action : int -> unit, set) : unit =
        match set with
        | Empty -> ()
        | Lf k ->
            action (int k)
        | Br (_,_,_,_) as t ->
            /// The stack of trie nodes pending traversal.
            let stack = Stack (defaultTraversalStackSize)

            // Add the initial tree to the stack.
            stack.Push t

            // Loop until we've processed the entire tree.
            while stack.Count <> 0 do
                match stack.Pop () with
                | Empty -> ()
                | Lf k ->
                    action (int k)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf k, Lf j) ->
                    action (int k)
                    action (int j)
                    
                | Br (_, _, Lf k, right) ->
                    // Only handle the case where the left child is a leaf
                    // -- otherwise the traversal order would be altered.
                    action (int k)
                    stack.Push right

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push right
                    stack.Push left

    //
    static member IterateBack (action : int -> unit, set) : unit =
        match set with
        | Empty -> ()
        | Lf k ->
            action (int k)
        | Br (_,_,_,_) as t ->
            /// The stack of trie nodes pending traversal.
            let stack = Stack (defaultTraversalStackSize)

            // Add the initial tree to the stack.
            stack.Push t

            // Loop until we've processed the entire tree.
            while stack.Count <> 0 do
                match stack.Pop () with
                | Empty -> ()
                | Lf k ->
                    action (int k)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf k, Lf j) ->
                    action (int j)
                    action (int k)
                    
                | Br (_, _, left, Lf k) ->
                    // Only handle the case where the right child is a leaf
                    // -- otherwise the traversal order would be altered.
                    action (int k)
                    stack.Push left

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push left
                    stack.Push right

    //
    static member Fold (folder : 'State -> int -> 'State, state : 'State, set) : 'State =
        match set with
        | Empty ->
            state
        | Lf k ->
            folder state (int k)
        | Br (_,_,_,_) as t ->
            let folder = FSharpFunc<_,_,_>.Adapt folder

            /// The stack of trie nodes pending traversal.
            let stack = Stack (defaultTraversalStackSize)

            // Add the initial tree to the stack.
            stack.Push t

            /// Loop until we've processed the entire tree.
            let mutable state = state
            while stack.Count <> 0 do
                match stack.Pop () with
                | Empty -> ()
                | Lf k ->
                    state <- folder.Invoke (state, int k)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf k, Lf j) ->
                    state <- folder.Invoke (state, int k)
                    state <- folder.Invoke (state, int j)
                    
                | Br (_, _, Lf k, right) ->
                    // Only handle the case where the left child is a leaf
                    // -- otherwise the traversal order would be altered.
                    state <- folder.Invoke (state, int k)
                    stack.Push right

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push right
                    stack.Push left

            // Return the final state value.
            state

    //
    static member FoldBack (folder : int -> 'State -> 'State, state : 'State, set) : 'State =
        match set with
        | Empty ->
            state
        | Lf k ->
            folder (int k) state
        | Br (_,_,_,_) as t ->
            let folder = FSharpFunc<_,_,_>.Adapt folder

            /// The stack of trie nodes pending traversal.
            let stack = Stack (defaultTraversalStackSize)

            // Add the initial tree to the stack.
            stack.Push t

            /// Loop until we've processed the entire tree.
            let mutable state = state
            while stack.Count <> 0 do
                match stack.Pop () with
                | Empty -> ()
                | Lf k ->
                    state <- folder.Invoke (int k, state)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf k, Lf j) ->
                    state <- folder.Invoke (int j, state)
                    state <- folder.Invoke (int k, state)
                    
                | Br (_, _, left, Lf k) ->
                    // Only handle the case where the right child is a leaf
                    // -- otherwise the traversal order would be altered.
                    state <- folder.Invoke (int k, state)
                    stack.Push left

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push left
                    stack.Push right

            // Return the final state value.
            state

    //
    static member TryPick (picker : int -> 'T option, set) : 'T option =
        match set with
        | Empty ->
            None
        | Lf k ->
            picker (int k)
        | Br (_,_,_,_) as t ->
            /// The stack of trie nodes pending traversal.
            let stack = Stack (defaultTraversalStackSize)

            // Add the initial tree to the stack.
            stack.Push t

            /// Loop until we find a key/value that matches the picker,
            /// or until we've processed the entire tree.
            let mutable pickedValue = None
            while stack.Count <> 0 && Option.isNone pickedValue do
                match stack.Pop () with
                | Empty -> ()
                | Lf k ->
                    pickedValue <- picker <| int k
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf k, Lf j) ->
                    pickedValue <-
                        match picker (int k) with
                        | (Some _) as value ->
                            value
                        | None ->
                            picker (int j)
                    
                | Br (_, _, Lf k, right) ->
                    // Only handle the case where the left child is a leaf
                    // -- otherwise the traversal order would be altered.
                    pickedValue <- picker (int k)
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
            yield int k
        
        (* OPTIMIZATION :   When one or both children of this node are leaves,
                            we handle them directly since it's a little faster. *)
        | Br (_, _, Lf k, Lf j) ->
            yield int k
            yield int j
                    
        | Br (_, _, Lf k, right) ->
            // Only handle the case where the left child is a leaf
            // -- otherwise the traversal order would be altered.
            yield int k
            yield! PatriciaSet.ToSeq right

        | Br (_, _, left, right) ->
            // Recursively visit the children.
            yield! PatriciaSet.ToSeq left
            yield! PatriciaSet.ToSeq right
        }

//
[<Sealed; CompiledName("FSharpIntSet")>]
//[<StructuredFormatDisplay("")>]
[<DebuggerDisplay("Count = {Count}")>]
[<DebuggerTypeProxy(typedefof<IntSetDebuggerProxy>)>]
type IntSet private (trie : PatriciaSet) =
    /// The empty IntSet instance.
    static let empty = IntSet Empty

    /// The empty IntSet.
    static member Empty
        with get () = empty

    //
    member private __.Trie
        with get () = trie

    /// The number of elements in the IntSet.
    member __.Count
        with get () : int =
            PatriciaSet.Count trie

    /// Is the map empty?
    member __.IsEmpty
        with get () =
            match trie with
            | Empty -> true
            | _ -> false

    /// Tests if an element is in the domain of the IntSet.
    member __.Contains (key : int) : bool =
        PatriciaSet.Contains (uint32 key, trie)

    /// The minimum unsigned value stored in the set.
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member __.MinElement
        with get () : int =
            int <| PatriciaSet.MinElement trie

    /// The minimum signed value stored in the set.
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member __.MinElementSigned
        with get () : int =
            int <| PatriciaSet.MinElementSigned trie

    /// The maximum unsigned value stored in the set.
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member __.MaxElement
        with get () : int =
            int <| PatriciaSet.MaxElement trie

    /// The maximum signed value stored in the set.
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member __.MaxElementSigned
        with get () : int =
            int <| PatriciaSet.MaxElementSigned trie

    /// The IntSet containing the given element.
    static member Singleton (element : int) : IntSet =
        IntSet (Lf <| uint32 element)

    /// Returns a new IntSet with the element added to this IntSet.
    member this.Add (key : int) : IntSet =
        // If the trie isn't modified, just return this IntSet instead of creating a new one.
        let trie' = PatriciaSet.Add (uint32 key, trie)
        if trie === trie' then this
        else IntSet (trie')

    /// Removes an element from the domain of the IntSet.
    /// No exception is raised if the element is not present.
    member this.Remove (key : int) : IntSet =
        // If the trie isn't modified, just return this IntSet instead of creating a new one.
        let trie' = PatriciaSet.Remove (uint32 key, trie)
        if trie === trie' then this
        else IntSet (trie')

    /// Computes the union of two IntSets.
    member this.Union (otherSet : IntSet) : IntSet =
        // If the result is the same (physical equality) to one of the inputs,
        // return that input instead of creating a new IntSet.
        let trie' = PatriciaSet.Union (trie, otherSet.Trie)
        if trie === trie' then this
        elif otherSet.Trie === trie' then otherSet
        else IntSet (trie')

    /// Computes the intersection of two IntSets.
    member this.Intersect (otherSet : IntSet) : IntSet =
        // If the result is the same (physical equality) to one of the inputs,
        // return that input instead of creating a new IntSet.
        let trie' = PatriciaSet.Intersect (trie, otherSet.Trie)
        if trie === trie' then this
        elif otherSet.Trie === trie' then otherSet
        else IntSet (trie')

    /// Removes the elements of the specified IntSet from this IntSet.
    member this.Difference (otherSet : IntSet) : IntSet =
        // If the result is the same (physical equality) to one of the inputs,
        // return that input instead of creating a new IntSet.
        let trie' = PatriciaSet.Difference (trie, otherSet.Trie)
        if trie === trie' then this
        elif otherSet.Trie === trie' then otherSet
        else IntSet (trie')

    /// Computes the union of a sequence of IntSets.
    static member internal UnionMany (sets : seq<IntSet>) : IntSet =
        // Preconditions
        checkNonNull "sets" sets

        let combinedTrie =
            (PatriciaSet.Empty, sets)
            ||> Seq.fold (fun combinedSetTree set ->
                PatriciaSet.Union (combinedSetTree, set.Trie))

        IntSet (combinedTrie)

    /// Computes the intersection of a sequence of IntSets.
    static member internal IntersectMany (sets : seq<IntSet>) : IntSet =
        // Preconditions
        checkNonNull "sets" sets

        Seq.reduce (fun s1 s2 -> s1.Intersect s2) sets

    /// Determines if the given set is a subset of this set.
    member this.IsSubset (otherSet : IntSet) : bool =
        PatriciaSet.IsSubset (otherSet.Trie, trie)

    /// Determines if the given set is a proper subset of this set.
    member this.IsProperSubset (otherSet : IntSet) : bool =
        PatriciaSet.IsProperSubset (otherSet.Trie, trie)

    /// Determines if the given set is a superset of this set.
    member this.IsSuperset (otherSet : IntSet) : bool =
        PatriciaSet.IsSuperset (otherSet.Trie, trie)

    /// Determines if the given set is a proper superset of this set.
    member this.IsProperSuperset (otherSet : IntSet) : bool =
        PatriciaSet.IsProperSuperset (otherSet.Trie, trie)

    /// Returns a new IntSet made from the given elements.
    static member internal OfSeq (source : seq<int>) : IntSet =
        // Preconditions
        checkNonNull "source" source

        IntSet (PatriciaSet.OfSeq source)

    /// Returns a new IntSet made from the given elements.
    static member internal OfList (source : int list) : IntSet =
        // Preconditions
        checkNonNull "source" source

        // OPTIMIZATION : If the source is empty return immediately.
        if List.isEmpty source then
            IntSet.Empty
        else
            IntSet (PatriciaSet.OfList source)

    /// Returns a new IntSet made from the given elements.
    static member internal OfArray (source : int[]) : IntSet =
        // Preconditions
        checkNonNull "source" source

        // OPTIMIZATION : If the source is empty return immediately.
        if Array.isEmpty source then
            IntSet.Empty
        else
            IntSet (PatriciaSet.OfArray source)

    /// Returns a new IntSet made from the given elements.
    static member internal OfSet (source : Set<int>) : IntSet =
        // Preconditions
        checkNonNull "source" source

        // OPTIMIZATION : If the source is empty return immediately.
        if Set.isEmpty source then
            IntSet.Empty
        else
            IntSet (PatriciaSet.OfSet source)

    //
    member __.ToSeq () =
        PatriciaSet.ToSeq trie

    //
    member __.ToList () : int list =
        PatriciaSet.FoldBack ((fun el list -> el :: list), [], trie)

    //
    member __.ToArray () : int[] =
        let elements = ResizeArray ()
        PatriciaSet.Iterate (elements.Add, trie)
        elements.ToArray ()

    //
    member __.ToSet () : Set<int> =
        PatriciaSet.FoldBack (Set.add, Set.empty, trie)

    //
    member __.Iterate (action : int -> unit) : unit =
        PatriciaSet.Iterate (action, trie)

    //
    member __.IterateBack (action : int -> unit) : unit =
        PatriciaSet.IterateBack (action, trie)

    //
    member __.Fold (folder : 'State -> int -> 'State, state : 'State) : 'State =
        PatriciaSet.Fold (folder, state, trie)

    //
    member __.FoldBack (folder : int -> 'State -> 'State, state : 'State) : 'State =
        PatriciaSet.FoldBack (folder, state, trie)

    //
    member __.TryPick (picker : int -> 'T option) : 'T option =
        PatriciaSet.TryPick (picker, trie)

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

    (* OPTIMIZE : The methods below should be replaced with optimized implementations where possible. *)

    //
    member this.Partition (predicate : int -> bool) : IntSet * IntSet =
        this.Fold ((fun (trueSet, falseSet) el ->
            if predicate el then
                trueSet.Add el,
                falseSet
            else
                trueSet,
                falseSet.Add el), (IntSet.Empty, IntSet.Empty))

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
            (PatriciaSet.ToSeq trie).GetEnumerator ()
            :> System.Collections.IEnumerator

    interface IEnumerable<int> with
        /// <inherit />
        member __.GetEnumerator () =
            (PatriciaSet.ToSeq trie).GetEnumerator ()

    interface ICollection<int> with
        /// <inherit />
        member __.Count
            with get () =
                PatriciaSet.Count trie

        /// <inherit />
        member __.IsReadOnly
            with get () = true

        /// <inherit />
        member __.Add x =
            notSupported "IntSets cannot be mutated."

        /// <inherit />
        member __.Clear () =
            notSupported "IntSets cannot be mutated."

        /// <inherit />
        member __.Contains (item : int) =
            PatriciaSet.Contains (uint32 item, trie)

        /// <inherit />
        member this.CopyTo (array, arrayIndex) =
            // Preconditions
            checkNonNull "array" array
            if arrayIndex < 0 then
                raise <| System.ArgumentOutOfRangeException "arrayIndex"

            let count = PatriciaSet.Count trie
            if arrayIndex + count > Array.length array then
                invalidArg "arrayIndex"
                    "There is not enough room in the array to copy the \
                     elements when starting at the specified index."

            this.Fold ((fun index el ->
                array.[index] <- el
                index + 1), arrayIndex)
            |> ignore

        /// <inherit />
        member __.Remove item : bool =
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

    //
    [<CompiledName("MinElement")>]
    let inline minElement (set : IntSet) : int =
        // Preconditions
        checkNonNull "set" set

        set.MinElement

    //
    [<CompiledName("MinElementSigned")>]
    let inline minElementSigned (set : IntSet) : int =
        // Preconditions
        checkNonNull "set" set

        set.MinElementSigned

    //
    [<CompiledName("MaxElement")>]
    let inline maxElement (set : IntSet) : int =
        // Preconditions
        checkNonNull "set" set

        set.MaxElement

    //
    [<CompiledName("MaxElementSigned")>]
    let inline maxElementSigned (set : IntSet) : int =
        // Preconditions
        checkNonNull "set" set

        set.MaxElementSigned

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
        
        set1.IsSubset set2

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the first set are in the second,
    /// and at least one element of the second is not in the first.
    /// </summary>
    [<CompiledName("IsProperSubset")>]
    let inline isProperSubset (set1 : IntSet) (set2 : IntSet) : bool =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2
        
        set1.IsProperSubset set2

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the second set are in the first.
    /// </summary>
    [<CompiledName("IsSuperset")>]
    let inline isSuperset (set1 : IntSet) (set2 : IntSet) : bool =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2
        
        set1.IsSuperset set2

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the second set are in the first,
    /// and at least one element of the first is not in the second.
    /// </summary>
    [<CompiledName("IsProperSuperset")>]
    let inline isProperSuperset (set1 : IntSet) (set2 : IntSet) : bool =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.IsProperSuperset set2

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
    [<CompiledName("Iter")>]
    let inline iter (action : int -> unit) (set : IntSet) =
        // Preconditions
        checkNonNull "set" set
        
        set.Iterate action

    /// Applies the given function to each element of the set,
    /// in reverse order according to the comparison function.
    [<CompiledName("IterBack")>]
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

        set1.IsSubset set2

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

        set1.IsProperSubset set2

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

        set1.IsSuperset set2

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

        set1.IsProperSuperset set2

    /// Builds a new collection from the given enumerable object.
    [<CompiledName("OfSeq")>]
    let inline ofSeq source : TagSet<'Tag> =
        // Preconditions are checked by the member.
        IntSet.OfSeq source
        |> retype

    /// Builds a set that contains the same elements as the given list.
    [<CompiledName("OfList")>]
    let inline ofList source : TagSet<'Tag> =
        // Preconditions are checked by the member.
        IntSet.OfList source
        |> retype

    /// Builds a set that contains the same elements as the given array.
    [<CompiledName("OfArray")>]
    let inline ofArray source : TagSet<'Tag> =
        // Preconditions are checked by the member.
        IntSet.OfArray source
        |> retype

    /// Builds an IntSet that contains the same elements as the given Set.
    [<CompiledName("OfSet")>]
    let inline ofSet source : TagSet<'Tag> =
        // Preconditions are checked by the member.
        IntSet.OfSet source
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
    let inline toList (set : TagSet<'Tag>) =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.ToList ()

    /// Builds an array that contains the elements of the set in order.
    [<CompiledName("ToArray")>]
    let inline toArray (set : TagSet<'Tag>) =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.ToArray ()

    /// Builds a Set that contains the same elements as the given IntSet.
    [<CompiledName("ToSet")>]
    let inline toSet (set : TagSet<'Tag>) =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.ToSet ()

    /// Applies the given function to each element of the set,
    /// in order according to the comparison function.
    [<CompiledName("Iter")>]
    let inline iter (action : int<'Tag> -> unit) (set : TagSet<'Tag>) =
        // Retype as IntSet
        let set : IntSet = retype set

        // Preconditions
        checkNonNull "set" set

        set.Iterate (retype action)

    /// Applies the given function to each element of the set,
    /// in reverse order according to the comparison function.
    [<CompiledName("IterBack")>]
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

#endif

