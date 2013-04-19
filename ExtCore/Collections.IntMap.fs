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
open PatriciaTrieConstants
open BitOps


(* OPTIMIZE :   Some of the functional-style operations on IntMap use direct non-tail-recursion;
                performance may be improved if we modify these to use CPS instead. *)

/// A Patricia trie implementation.
/// Used as the underlying data structure for IntMap (and TagMap).
[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type private PatriciaMap< [<EqualityConditionalOn; ComparisonConditionalOn>] 'T> =
    | Empty
    // Key * Value
    | Lf of uint32 * 'T
    // Prefix * Mask * Left-Child * Right-Child
    | Br of uint32 * uint32 * PatriciaMap<'T> * PatriciaMap<'T>

    //
    static member TryFind (key, map : PatriciaMap<'T>) =
        match map with
        | Empty ->
            None
        | Lf (j, x) ->
            if j = key then Some x
            else None
        | Br (p, m, t0, t1) ->
            PatriciaMap.TryFind (
                key, (if zeroBit (key, m) then t0 else t1))

    //
    static member ContainsKey (key, map : PatriciaMap<'T>) =
        match map with
        | Empty ->
            false
        | Lf (j, _) ->
            key = j
        | Br (_, m, t0, t1) ->
            PatriciaMap.ContainsKey (
                key, (if zeroBit (key, m) then t0 else t1))

    //
    static member Count (map : PatriciaMap<'T>) : int =
        match map with
        | Empty -> 0
        | Lf (_,_) -> 1
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
                | Lf (_,_) ->
                    count <- count + 1u
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf (_,_), Lf (_,_)) ->
                    count <- count + 2u
                    
                | Br (_, _, Lf (_,_), child)
                | Br (_, _, child, Lf (_,_)) ->
                    count <- count + 1u
                    stack.Push child

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push right
                    stack.Push left

            // Return the computed element count.
            int count

    /// Remove the binding with the specified key from the map.
    /// No exception is thrown if the map does not contain a binding for the key.
    static member Remove (key, map : PatriciaMap<'T>) =
        match map with
        | Empty ->
            Empty
        | Lf (j, _) as t ->
            if j = key then Empty
            else t
        
        | Br (p, m, t0, t1) as t ->
            if matchPrefix (key, p, m) then
                if zeroBit (key, m) then
                    match PatriciaMap.Remove (key, t0) with
                    | Empty -> t1
                    | t0 ->
                        Br (p, m, t0, t1)
                else
                    match PatriciaMap.Remove (key, t1) with
                    | Empty -> t0
                    | t1 ->
                        Br (p, m, t0, t1)
            else t

    //
    static member inline private Join (p0, t0 : PatriciaMap<'T>, p1, t1) =
        let m = branchingBit (p0, p1)
        let p = mask (p0, m)
        if zeroBit (p0, m) then
            Br (p, m, t0, t1)
        else
            Br (p, m, t1, t0)

    /// Insert a binding (key-value pair) into a map, returning a new, updated map.
    static member Add (key, value : 'T, map) =
        match map with
        | Empty ->
            Lf (key, value)
        | Lf (j, y) as t ->
            if j = key then
                // A binding already exists with the given key.
                // This method 'overwrites' the existing value, by returning
                // a new node with the inserted value.
                // OPTIMIZE : Try calling Object.Equals or similar -- if the existing
                // value is the same as the new value, we can return this node instead
                // of creating a new one.
                Lf (key, value)
            else
                PatriciaMap.Join (key, Lf (key, value), j, t)
        | Br (p, m, t0, t1) as t ->
            if matchPrefix (key, p, m) then
                if zeroBit (key, m) then
                    let left = PatriciaMap.Add (key, value, t0)
                    Br (p, m, left, t1)
                else
                    let right = PatriciaMap.Add (key, value, t1)
                    Br (p, m, t0, right)
            else
                PatriciaMap.Join (key, Lf (key, value), p, t)

    /// Insert a binding (key-value pair) into a map, returning a new, updated map.
    /// If a binding already exists for the same key, the map is not altered.
    static member TryAdd (key, value : 'T, map) =
        match map with
        | Empty ->
            Lf (key, value)
        | Lf (j, y) as t ->
            if j = key then
                // A binding already exists with the given key.
                // This method does not overwrite the existing value, so we can
                // just return the existing map instead of creating an identical new one.
                t
            else
                PatriciaMap.Join (key, Lf (key, value), j, t)
        | Br (p, m, t0, t1) as t ->
            if matchPrefix (key, p, m) then
                if zeroBit (key, m) then
                    let left = PatriciaMap.TryAdd (key, value, t0)
                    Br (p, m, left, t1)
                else
                    let right = PatriciaMap.TryAdd (key, value, t1)
                    Br (p, m, t0, right)
            else
                PatriciaMap.Join (key, Lf (key, value), p, t)

    /// Computes the union of two PatriciaMaps.
    static member Union (s, t) : PatriciaMap<'T> =
        match s, t with
        | (Br (p, m, s0, s1) as s), (Br (q, n, t0, t1) as t) ->
            if m = n then
                if p = q then
                    // The trees have the same prefix. Merge the subtrees.
                    let left = PatriciaMap.Union (s0, t0)
                    let right = PatriciaMap.Union (s1, t1)
                    Br (p, m, left, right)
                else
                    // The prefixes disagree.
                    PatriciaMap.Join (p, s, q, t)

            #if LITTLE_ENDIAN_TRIES
            elif m < n then
            #else
            elif m > n then
            #endif
                if matchPrefix (q, p, m) then
                    // q contains p. Merge t with a subtree of s.
                    if zeroBit (q, m) then
                        let left = PatriciaMap.Union (s0, t)
                        Br (p, m, left, s1)
                    else
                        let right = PatriciaMap.Union (s1, t)
                        Br (p, m, s0, right)
                else
                    // The prefixes disagree.
                    PatriciaMap.Join (p, s, q, t)

            else
                if matchPrefix (p, q, n) then
                    // p contains q. Merge s with a subtree of t.
                    if zeroBit (p, n) then
                        let left = PatriciaMap.Union (s, t0)
                        Br (q, n, left, t1)
                    else
                        let right = PatriciaMap.Union (s, t1)
                        Br (q, n, t0, right)
                else
                    // The prefixes disagree.
                    PatriciaMap.Join (p, s, q, t)

        | (Br (p, m, s0, s1) as s), Lf (k, x) ->
            if matchPrefix (k, p, m) then
                if zeroBit (k, m) then
                    let left = PatriciaMap.TryAdd (k, x, s0)
                    Br (p, m, left, s1)
                else
                    let right = PatriciaMap.TryAdd (k, x, s1)
                    Br (p, m, s0, right)
            else
                PatriciaMap.Join (k, Lf (k, x), p, s)

        | (Br (_,_,_,_) as s), Empty ->
            s
        | Lf (k, x), t ->
            PatriciaMap.Add (k, x, t)
        | Empty, t -> t

    /// Compute the intersection of two PatriciaMaps.
    /// If both maps contain a binding with the same key, the binding from
    /// the first map will be used.
    static member Intersect (s, t) : PatriciaMap<'T> =
        match s, t with
        | (Br (p, m, s0, s1) as s), (Br (q, n, t0, t1) as t) ->
            if m = n then
                if p <> q then Empty
                else
                    let left = PatriciaMap.Intersect (s0, t0)
                    let right = PatriciaMap.Intersect (s1, t1)
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
                        PatriciaMap.Intersect (s0, t)
                    else
                        PatriciaMap.Intersect (s1, t)
                else
                    Empty

            else
                if matchPrefix (p, q, n) then
                    if zeroBit (p, n) then
                        PatriciaMap.Intersect (s, t0)
                    else
                        PatriciaMap.Intersect (s, t1)
                else
                    Empty

        | Br (_, m, s0, s1), Lf (k, y) ->
            let s' = if zeroBit (k, m) then s0 else s1
            match PatriciaMap.TryFind (k, s') with
            | Some x ->
                Lf (k, x)
            | None ->
                Empty
            
        | Br (_,_,_,_), Empty ->
            Empty
            
        | (Lf (k, x) as s), t ->
            // Here, we always use the value from the left tree, so as long as the
            // right tree contains a binding with the same key, we just return the left tree.
            if PatriciaMap.ContainsKey (k, t) then s
            else Empty

        | Empty, _ ->
            Empty

    //
    static member private Delete (key, map) : PatriciaMap<'T> =
        match map with
        | Empty ->
            Empty
        | Lf (j, _) as t ->
            if key = j then Empty
            else t
        | Br (p, m, t0, t1) as t ->
            if matchPrefix (key, p, m) then
                if zeroBit (key, m) then
                    match PatriciaMap.Delete (key, t0) with
                    | Empty -> t1
                    | left ->
                        Br (p, m, left, t1)
                else
                    match PatriciaMap.Delete (key, t1) with
                    | Empty -> t0
                    | right ->
                        Br (p, m, t0, right)
            else t

    /// Compute the difference of two PatriciaMaps.
    static member Difference (s, t) : PatriciaMap<'T> =
        match s, t with
        | (Br (p, m, s0, s1) as s), (Br (q, n, t0, t1) as t) ->
            if m = n then
                if p <> q then s
                else
                    let left = PatriciaMap.Difference (s0, t0)
                    let right = PatriciaMap.Difference (s1, t1)
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
                        match PatriciaMap.Difference (s0, t) with
                        | Empty -> s1
                        | left ->
                            Br (p, m, left, s1)
                    else
                        match PatriciaMap.Difference (s1, t) with
                        | Empty -> s0
                        | right ->
                            Br (p, m, s0, right)
                else s

            else
                if matchPrefix (p, q, n) then
                    if zeroBit (p, n) then
                        PatriciaMap.Difference (s, t0)
                    else
                        PatriciaMap.Difference (s, t1)
                else s

        | Br (p, m, s0, s1), Lf (k, y) ->
            if matchPrefix (k, p, m) then
                if zeroBit (k, m) then
                    match PatriciaMap.Delete (k, s0) with
                    | Empty -> s1
                    | left ->
                        Br (p, m, left, s1)
                else
                    match PatriciaMap.Delete (k, s1) with
                    | Empty -> s0
                    | right ->
                        Br (p, m, s0, right)
            else s
            
        | Br (_,_,_,_) as s, Empty ->
            s
        | (Lf (k, x) as s), t ->
            if PatriciaMap.ContainsKey (k, t) then Empty
            else s
        | Empty, _ ->
            Empty

    /// <c>IsSubmapOfBy f t1 t2</c> returns <c>true</c> if all keys in t1 are in t2,
    /// and when 'f' returns <c>true</c> when applied to their respective values.
    static member IsSubmapOfBy (predicate : 'T -> 'T -> bool) (t1 : PatriciaMap<'T>) (t2 : PatriciaMap<'T>) : bool =
        match t1, t2 with
        | (Br (p1, m1, l1, r1) as t1), (Br (p2, m2, l2, r2) as t2) ->
            if shorter (m1, m2) then
                false
            elif shorter (m2, m1) then
                matchPrefix (p1, p2, m2) && (
                    if zeroBit (p1, m2) then
                        PatriciaMap.IsSubmapOfBy predicate t1 l2
                    else
                        PatriciaMap.IsSubmapOfBy predicate t1 r2)
            else
                p1 = p2
                && PatriciaMap.IsSubmapOfBy predicate l1 l2
                && PatriciaMap.IsSubmapOfBy predicate r1 r2
                
        | Br (_,_,_,_), _ ->
            false
        | Lf (k, x), t ->
            match PatriciaMap.TryFind (k, t) with
            | None ->
                false
            | Some y ->
                predicate x y
        | Empty, _ ->
            true

    //
    static member private SubmapCmp (predicate : 'T -> 'T -> bool) (t1 : PatriciaMap<'T>) (t2 : PatriciaMap<'T>) : int =
        match t1, t2 with
        | (Br (p1, m1, l1, r1) as t1), (Br (p2, m2, l2, r2) as t2) ->
            if shorter (m1, m2) then 1
            elif shorter (m2, m1) then
                if not <| matchPrefix (p1, p2, m2) then 1
                elif zeroBit (p1, m2) then
                    PatriciaMap.SubmapCmp predicate t1 l2
                else
                    PatriciaMap.SubmapCmp predicate t1 r2
            elif p1 = p2 then
                let left = PatriciaMap.SubmapCmp predicate l1 l2
                let right = PatriciaMap.SubmapCmp predicate r1 r2
                match left, right with
                | 1, _
                | _, 1 -> 1
                | 0, 0 -> 0
                | _ -> -1
            else
                // The maps are disjoint.
                1

        | Br (_,_,_,_), _ -> 1
        | Lf (kx, x), Lf (ky, y) ->
            if kx = ky && predicate x y then 0
            else 1  // The maps are disjoint.
        | Lf (k, x), t ->
            match PatriciaMap.TryFind (k, t) with
            | Some y when predicate x y -> -1
            | _ -> 1    // The maps are disjoint.

        | Empty, Empty -> 0
        | Empty, _ -> -1

    //
    static member OfSeq (source : seq<int * 'T>) : PatriciaMap<'T> =
        (Empty, source)
        ||> Seq.fold (fun trie (key, value) ->
            PatriciaMap.Add (uint32 key, value, trie))

    //
    static member OfList (source : (int * 'T) list) : PatriciaMap<'T> =
        // Preconditions
        checkNonNull "source" source

        (Empty, source)
        ||> List.fold (fun trie (key, value) ->
            PatriciaMap.Add (uint32 key, value, trie))

    //
    static member OfArray (source : (int * 'T)[]) : PatriciaMap<'T> =
        // Preconditions
        checkNonNull "source" source

        (Empty, source)
        ||> Array.fold (fun trie (key, value) ->
            PatriciaMap.Add (uint32 key, value, trie))

    //
    static member OfMap (source : Map<int, 'T>) : PatriciaMap<'T> =
        // Preconditions
        checkNonNull "source" source

        (Empty, source)
        ||> Map.fold (fun trie key value ->
            PatriciaMap.Add (uint32 key, value, trie))

    //
    static member Iterate (action : int -> 'T -> unit, map) : unit =
        match map with
        | Empty -> ()
        | Lf (k, x) ->
            action (int k) x
        | Br (_,_,_,_) as t ->
            let action = FSharpFunc<_,_,_>.Adapt action

            /// The stack of trie nodes pending traversal.
            let stack = Stack (defaultTraversalStackSize)

            // Add the initial tree to the stack.
            stack.Push t

            // Loop until we've processed the entire tree.
            while stack.Count <> 0 do
                match stack.Pop () with
                | Empty -> ()
                | Lf (k, x) ->
                    action.Invoke (int k, x)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf (k, x), Lf (j, y)) ->
                    action.Invoke (int k, x)
                    action.Invoke (int j, y)
                    
                | Br (_, _, Lf (k, x), right) ->
                    // Only handle the case where the left child is a leaf
                    // -- otherwise the traversal order would be altered.
                    action.Invoke (int k, x)
                    stack.Push right

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push right
                    stack.Push left

    //
    static member IterateBack (action : int -> 'T -> unit, map) : unit =
        match map with
        | Empty -> ()
        | Lf (k, x) ->
            action (int k) x
        | Br (_,_,_,_) as t ->
            let action = FSharpFunc<_,_,_>.Adapt action

            /// The stack of trie nodes pending traversal.
            let stack = Stack (defaultTraversalStackSize)

            // Add the initial tree to the stack.
            stack.Push t

            // Loop until we've processed the entire tree.
            while stack.Count <> 0 do
                match stack.Pop () with
                | Empty -> ()
                | Lf (k, x) ->
                    action.Invoke (int k, x)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf (k, x), Lf (j, y)) ->
                    action.Invoke (int j, y)
                    action.Invoke (int k, x)
                    
                | Br (_, _, left, Lf (k, x)) ->
                    // Only handle the case where the right child is a leaf
                    // -- otherwise the traversal order would be altered.
                    action.Invoke (int k, x)
                    stack.Push left

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push left
                    stack.Push right

    //
    static member Fold (folder : 'State -> int -> 'T -> 'State, state : 'State, map) : 'State =
        match map with
        | Empty ->
            state
        | Lf (k, x) ->
            folder state (int k) x
        | Br (_,_,_,_) as t ->
            let folder = FSharpFunc<_,_,_,_>.Adapt folder

            /// The stack of trie nodes pending traversal.
            let stack = Stack (defaultTraversalStackSize)

            // Add the initial tree to the stack.
            stack.Push t

            /// Loop until we've processed the entire tree.
            let mutable state = state
            while stack.Count <> 0 do
                match stack.Pop () with
                | Empty -> ()
                | Lf (k, x) ->
                    state <- folder.Invoke (state, int k, x)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf (k, x), Lf (j, y)) ->
                    state <- folder.Invoke (state, int k, x)
                    state <- folder.Invoke (state, int j, y)
                    
                | Br (_, _, Lf (k, x), right) ->
                    // Only handle the case where the left child is a leaf
                    // -- otherwise the traversal order would be altered.
                    state <- folder.Invoke (state, int k, x)
                    stack.Push right

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push right
                    stack.Push left

            // Return the final state value.
            state

    //
    static member FoldBack (folder : int -> 'T -> 'State -> 'State, state : 'State, map) : 'State =
        match map with
        | Empty ->
            state
        | Lf (k, x) ->
            folder (int k) x state
        | Br (_,_,_,_) as t ->
            let folder = FSharpFunc<_,_,_,_>.Adapt folder

            /// The stack of trie nodes pending traversal.
            let stack = Stack (defaultTraversalStackSize)

            // Add the initial tree to the stack.
            stack.Push t

            /// Loop until we've processed the entire tree.
            let mutable state = state
            while stack.Count <> 0 do
                match stack.Pop () with
                | Empty -> ()
                | Lf (k, x) ->
                    state <- folder.Invoke (int k, x, state)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf (k, x), Lf (j, y)) ->
                    state <- folder.Invoke (int j, y, state)
                    state <- folder.Invoke (int k, x, state)
                    
                | Br (_, _, left, Lf (k, x)) ->
                    // Only handle the case where the right child is a leaf
                    // -- otherwise the traversal order would be altered.
                    state <- folder.Invoke (int k, x, state)
                    stack.Push left

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push left
                    stack.Push right

            // Return the final state value.
            state

    //
    static member TryFindKey (predicate : int -> 'T -> bool, map) : int option =
        match map with
        | Empty ->
            None
        | Lf (k, x) ->
            if predicate (int k) x then
                Some (int k)
            else
                None
        | Br (_,_,_,_) as t ->
            let predicate = FSharpFunc<_,_,_>.Adapt predicate

            /// The stack of trie nodes pending traversal.
            let stack = Stack (defaultTraversalStackSize)

            // Add the initial tree to the stack.
            stack.Push t

            /// Loop until we find a key/value that matches the predicate,
            /// or until we've processed the entire tree.
            let mutable matchingKey = None
            while stack.Count <> 0 && Option.isNone matchingKey do
                match stack.Pop () with
                | Empty -> ()
                | Lf (k, x) ->
                    if predicate.Invoke (int k, x) then
                        matchingKey <- Some (int k)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf (k, x), Lf (j, y)) ->
                    if predicate.Invoke (int k, x) then
                        matchingKey <- Some (int k)
                    elif predicate.Invoke (int j, y) then
                        matchingKey <- Some (int j)
                    
                | Br (_, _, Lf (k, x), right) ->
                    // Only handle the case where the left child is a leaf
                    // -- otherwise the traversal order would be altered.
                    if predicate.Invoke (int k, x) then
                        matchingKey <- Some (int k)

                    stack.Push right

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push right
                    stack.Push left

            // Return the matching key, if one was found.
            matchingKey

    //
    static member TryPick (picker : int -> 'T -> 'U option, map) : 'U option =
        match map with
        | Empty ->
            None
        | Lf (k, x) ->
            picker (int k) x
        | Br (_,_,_,_) as t ->
            let picker = FSharpFunc<_,_,_>.Adapt picker

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
                | Lf (k, x) ->
                    pickedValue <- picker.Invoke (int k, x)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf (k, x), Lf (j, y)) ->
                    pickedValue <-
                        match picker.Invoke (int k, x) with
                        | (Some _) as value ->
                            value
                        | None ->
                            picker.Invoke (int j, y)
                    
                | Br (_, _, Lf (k, x), right) ->
                    // Only handle the case where the left child is a leaf
                    // -- otherwise the traversal order would be altered.
                    pickedValue <- picker.Invoke (int k, x)
                    stack.Push right

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push right
                    stack.Push left

            // Return the picked value.
            pickedValue

    //
    static member ToSeq (map : PatriciaMap<'T>) =
        seq {
        match map with
        | Empty -> ()
        | Lf (k, x) ->
            yield (int k, x)
        
        (* OPTIMIZATION :   When one or both children of this node are leaves,
                            we handle them directly since it's a little faster. *)
        | Br (_, _, Lf (k, x), Lf (j, y)) ->
            yield (int k, x)
            yield (int j, y)
                    
        | Br (_, _, Lf (k, x), right) ->
            // Only handle the case where the left child is a leaf
            // -- otherwise the traversal order would be altered.
            yield (int k, x)
            yield! PatriciaMap.ToSeq right

        | Br (_, _, left, right) ->
            // Recursively visit the children.
            yield! PatriciaMap.ToSeq left
            yield! PatriciaMap.ToSeq right
        }

/// <summary>Immutable maps with integer keys.</summary>
/// <typeparam name="T">The type of the values stored in the IntMap.</typeparam>
[<Sealed; CompiledName("FSharpIntMap`1")>]
//[<StructuredFormatDisplay("")>]
[<DebuggerDisplay("Count = {Count}")>]
[<DebuggerTypeProxy(typedefof<IntMapDebuggerProxy<int>>)>]
type IntMap< [<EqualityConditionalOn; ComparisonConditionalOn>] 'T> private (trie : PatriciaMap<'T>) =
    /// The empty IntMap instance.
    static let empty : IntMap<'T> =
        IntMap Empty

    /// The empty IntMap.
    static member Empty
        with get () = empty

    /// The internal representation of the IntMap.
    member private __.Trie
        with get () = trie

    //
    static member private Equals (left : IntMap<'T>, right : IntMap<'T>) =
        Unchecked.equals left.Trie right.Trie

    //
    static member private Compare (left : IntMap<'T>, right : IntMap<'T>) =
        Unchecked.compare left.Trie right.Trie

    /// <inherit />
    override __.Equals other =
        match other with
        | :? IntMap<'T> as other ->
            Unchecked.equals trie other.Trie
        | _ ->
            false

    /// <inherit />
    override __.GetHashCode () =
        Unchecked.hash trie

    //
    member __.Item
        with get key =
            match PatriciaMap.TryFind (key, trie) with
            | Some v -> v
            | None ->
                keyNotFound "The map does not contain a binding for the specified key."

    /// The number of bindings in the IntMap.
    member __.Count
        with get () : int =
            PatriciaMap.Count trie

    /// Is the map empty?
    member __.IsEmpty
        with get () =
            match trie with
            | Empty -> true
            | _ -> false

    /// Look up an element in the IntMap returning a Some value if the
    /// element is in the domain of the IntMap and None if not.
    member __.TryFind (key : int) : 'T option =
        PatriciaMap.TryFind (uint32 key, trie)

    /// Look up an element in the IntMap, raising KeyNotFoundException
    /// if no binding exists in the IntMap.
    member __.Find (key : int) : 'T =
        match PatriciaMap.TryFind (uint32 key, trie) with
        | Some x -> x
        | None ->
            // TODO : Add a better error message which includes the key.
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    /// Tests if an element is in the domain of the IntMap.
    member __.ContainsKey (key : int) : bool =
        PatriciaMap.ContainsKey (uint32 key, trie)

    /// Returns a new IntMap with the binding added to this IntMap.
    member this.Add (key : int, value : 'T) : IntMap<'T> =
        // If the trie isn't modified, just return this IntMap instead of creating a new one.
        let trie' = PatriciaMap.Add (uint32 key, value, trie)
        if trie === trie' then this
        else IntMap (trie')

    /// Returns a new IntMap with the binding added to this IntMap.
    member this.TryAdd (key : int, value : 'T) : IntMap<'T> =
        // If the trie isn't modified, just return this IntMap instead of creating a new one.
        let trie' = PatriciaMap.TryAdd (uint32 key, value, trie)
        if trie === trie' then this
        else IntMap (trie')

    /// Removes an element from the domain of the IntMap.
    /// No exception is raised if the element is not present.
    member this.Remove (key : int) : IntMap<'T> =
        // If the trie isn't modified, just return this IntMap instead of creating a new one.
        let trie' = PatriciaMap.Remove (uint32 key, trie)
        if trie === trie' then this
        else IntMap (trie')

    /// Returns a new IntMap created by merging the two specified IntMaps.
    member this.Union (otherMap : IntMap<'T>) : IntMap<'T> =
        // If the result is the same (physical equality) to one of the inputs,
        // return that input instead of creating a new IntMap.
        let trie' = PatriciaMap.Union (trie, otherMap.Trie)
        if trie === trie' then this
        elif otherMap.Trie === trie' then otherMap
        else IntMap (trie')

    /// Returns the intersection of two IntMaps.
    member this.Intersect (otherMap : IntMap<'T>) : IntMap<'T> =
        // If the result is the same (physical equality) to one of the inputs,
        // return that input instead of creating a new IntMap.
        let trie' = PatriciaMap.Intersect (trie, otherMap.Trie)
        if trie === trie' then this
        elif otherMap.Trie === trie' then otherMap
        else IntMap (trie')

    /// Returns a new IntMap created by removing the second IntMap from the first.
    member this.Difference (otherMap : IntMap<'T>) : IntMap<'T> =
        // If the result is the same (physical equality) to one of the inputs,
        // return that input instead of creating a new IntMap.
        let trie' = PatriciaMap.Difference (trie, otherMap.Trie)
        if trie === trie' then this
        elif otherMap.Trie === trie' then otherMap
        else IntMap (trie')

    /// Returns true if 'other' is a submap of this map.
    member this.IsSubmapOfBy (predicate, other : IntMap<'T>) : bool =
        PatriciaMap.IsSubmapOfBy predicate other.Trie trie

    /// The IntMap containing the given binding.
    static member Singleton (key : int, value : 'T) : IntMap<'T> =
        IntMap (
            Lf (uint32 key, value))

    /// Returns a new IntMap made from the given bindings.
    static member OfSeq (source : seq<int * 'T>) : IntMap<'T> =
        // Preconditions
        checkNonNull "source" source

        IntMap (PatriciaMap.OfSeq source)

    /// Returns a new IntMap made from the given bindings.
    static member OfList (source : (int * 'T) list) : IntMap<'T> =
        // Preconditions
        checkNonNull "source" source

        // OPTIMIZATION : If the source is empty return immediately.
        if List.isEmpty source then
            IntMap.Empty
        else
            IntMap (PatriciaMap.OfList source)

    /// Returns a new IntMap made from the given bindings.
    static member OfArray (source : (int * 'T)[]) : IntMap<'T> =
        // Preconditions
        checkNonNull "source" source

        // OPTIMIZATION : If the source is empty return immediately.
        if Array.isEmpty source then
            IntMap.Empty
        else
            IntMap (PatriciaMap.OfArray source)

    /// Returns a new IntMap made from the given bindings.
    static member OfMap (source : Map<int, 'T>) : IntMap<'T> =
        // Preconditions
        checkNonNull "source" source

        // OPTIMIZATION : If the source is empty return immediately.
        if Map.isEmpty source then
            IntMap.Empty
        else
            IntMap (PatriciaMap.OfMap source)

    //
    member __.ToSeq () =
        PatriciaMap.ToSeq trie

    //
    member __.ToList () : (int * 'T) list =
        PatriciaMap.FoldBack ((fun k v list -> (k, v) :: list), [], trie)

    //
    member __.ToArray () : (int * 'T)[] =
        let elements = ResizeArray ()
        PatriciaMap.Iterate (FuncConvert.FuncFromTupled<_,_,_> elements.Add, trie)
        elements.ToArray ()

    //
    member __.ToMap () : Map<int, 'T> =
        PatriciaMap.FoldBack (Map.add, Map.empty, trie)

    //
    member internal __.ToKvpArray () : KeyValuePair<int, 'T>[] =
        let elements = ResizeArray (1024)

        PatriciaMap.Iterate ((fun key value ->
            elements.Add (
                KeyValuePair (key, value))), trie)

        elements.ToArray ()

    //
    member __.Iterate (action : int -> 'T -> unit) : unit =
        PatriciaMap.Iterate (action, trie)

    //
    member __.IterateBack (action : int -> 'T -> unit) : unit =
        PatriciaMap.IterateBack (action, trie)

    //
    member __.Fold (folder : 'State -> int -> 'T -> 'State, state : 'State) : 'State =
        PatriciaMap.Fold (folder, state, trie)

    //
    member __.FoldBack (folder : int -> 'T -> 'State -> 'State, state : 'State) : 'State =
        PatriciaMap.FoldBack (folder, state, trie)

    //
    member __.TryFindKey (predicate : int -> 'T -> bool) : int option =
        PatriciaMap.TryFindKey (predicate, trie)

    //
    member this.FindKey (predicate : int -> 'T -> bool) : int =
        match this.TryFindKey predicate with
        | Some key ->
            key
        | None ->
            // TODO : Add a better error message
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    //
    member this.Exists (predicate : int -> 'T -> bool) : bool =
        this.TryFindKey predicate
        |> Option.isSome

    //
    member this.Forall (predicate : int -> 'T -> bool) : bool =
        this.TryFindKey (fun k v ->
            not <| predicate k v)
        |> Option.isNone

    //
    member __.TryPick (picker : int -> 'T -> 'U option) : 'U option =
        PatriciaMap.TryPick (picker, trie)

    //
    member this.Pick (picker : int -> 'T -> 'U option) : 'U =
        match this.TryPick picker with
        | Some value ->
            value
        | None ->
            // TODO : Add a better error message
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    //
    member this.Choose (chooser : int -> 'T -> 'U option) : IntMap<'U> =
        let chooser = FSharpFunc<_,_,_>.Adapt chooser

        this.Fold ((fun chosenMap key value ->
            match chooser.Invoke (key, value) with
            | None ->
                chosenMap
            | Some newValue ->
                chosenMap.Add (key, newValue)), IntMap.Empty)

    //
    member this.Filter (predicate : int -> 'T -> bool) : IntMap<'T> =
        let predicate = FSharpFunc<_,_,_>.Adapt predicate

        this.Fold ((fun filteredMap key value ->
            if predicate.Invoke (key, value) then
                filteredMap
            else
                filteredMap.Remove key), this)

    (* OPTIMIZE : The methods below should be replaced with optimized implementations where possible. *)    

    //
    // OPTIMIZE : We should be able to implement an optimized version
    // of Map which builds the mapped trie from the bottom-up; this works
    // because the mapped trie will have the same structure as the original,
    // just with different values in the leaves.
    member this.Map (mapping : int -> 'T -> 'U) : IntMap<'U> =
        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        this.Fold ((fun map key value ->
            map.Add (key, mapping.Invoke (key, value))), IntMap.Empty)

    //
    member this.Partition (predicate : int -> 'T -> bool) : IntMap<'T> * IntMap<'T> =
        let predicate = FSharpFunc<_,_,_>.Adapt predicate

        this.Fold ((fun (trueMap, falseMap) key value ->
            if predicate.Invoke (key, value) then
                trueMap.Add (key, value),
                falseMap
            else
                trueMap,
                falseMap.Add (key, value)), (IntMap.Empty, IntMap.Empty))

    //
    member this.MapPartition (partitioner : int -> 'T -> Choice<'U, 'V>) : IntMap<'U> * IntMap<'V> =
        let partitioner = FSharpFunc<_,_,_>.Adapt partitioner

        this.Fold ((fun (map1, map2) key value ->
            match partitioner.Invoke (key, value) with
            | Choice1Of2 value ->
                map1.Add (key, value),
                map2
            | Choice2Of2 value ->
                map1,
                map2.Add (key, value)), (IntMap.Empty, IntMap.Empty))

    interface System.IEquatable<IntMap<'T>> with
        /// <inherit />
        member this.Equals other =
            IntMap<_>.Equals (this, other)

    interface System.IComparable with
        /// <inherit />
        member this.CompareTo other =
            IntMap<_>.Compare (this, other :?> IntMap<'T>)

    interface System.IComparable<IntMap<'T>> with
        /// <inherit />
        member this.CompareTo other =
            IntMap<_>.Compare (this, other)

    interface System.Collections.IEnumerable with
        /// <inherit />
        member __.GetEnumerator () =
            (PatriciaMap.ToSeq trie |> Seq.map (fun (k, v) ->
                System.Collections.DictionaryEntry (k, v))).GetEnumerator ()
            :> System.Collections.IEnumerator

    interface IEnumerable<KeyValuePair<int, 'T>> with
        /// <inherit />
        member __.GetEnumerator () =
            (PatriciaMap.ToSeq trie |> Seq.map (fun (k, v) ->
                KeyValuePair (k, v))).GetEnumerator ()

    interface ICollection<KeyValuePair<int, 'T>> with
        /// <inherit />
        member __.Count
            with get () =
                PatriciaMap.Count trie

        /// <inherit />
        member __.IsReadOnly
            with get () = true

        /// <inherit />
        member __.Add x =
            notSupported "IntMaps cannot be mutated."

        /// <inherit />
        member __.Clear () =
            notSupported "IntMaps cannot be mutated."

        /// <inherit />
        member __.Contains (item : KeyValuePair<int, 'T>) =
            match PatriciaMap.TryFind (uint32 item.Key, trie) with
            | None ->
                false
            | Some value ->
                Unchecked.equals value item.Value

        /// <inherit />
        member this.CopyTo (array, arrayIndex) =
            // Preconditions
            checkNonNull "array" array
            if arrayIndex < 0 then
                raise <| System.ArgumentOutOfRangeException "arrayIndex"

            let count = PatriciaMap.Count trie
            if arrayIndex + count > Array.length array then
                invalidArg "arrayIndex"
                    "There is not enough room in the array to copy the \
                     elements when starting at the specified index."

            this.Fold ((fun index key value ->
                array.[index] <- KeyValuePair (key, value)
                index + 1), arrayIndex)
            |> ignore

        /// <inherit />
        member __.Remove item : bool =
            notSupported "IntMaps cannot be mutated."

    interface IDictionary<int, 'T> with
        /// <inherit />
        member __.Item
            with get key =
                match PatriciaMap.TryFind (uint32 key, trie) with
                | Some value ->
                    value
                | None ->
                    // TODO : Provide a better error message here.
                    //keyNotFound ""
                    raise <| System.Collections.Generic.KeyNotFoundException ()
            and set key value =
                notSupported "IntMaps cannot be mutated."

        /// <inherit />
        member __.Keys
            with get () =
                // TODO : Return the keys as an ICollection<int>
                notImpl "IDictionary`2.Keys"

        /// <inherit />
        member __.Values
            with get () =
                // TODO : Return the values as an ICollection<'T>
                notImpl "IDictionary`2.Values"

        /// <inherit />
        member __.Add (key, value) =
            notSupported "IntMaps cannot be mutated."

        /// <inherit />
        member __.ContainsKey key =
            PatriciaMap.ContainsKey (uint32 key, trie)

        /// <inherit />
        member __.Remove key =
            notSupported "IntMaps cannot be mutated."

        /// <inherit />
        member __.TryGetValue (key, value) =
            match PatriciaMap.TryFind (uint32 key, trie) with
            | None ->
                false
            | Some v ->
                value <- v
                true

//
and [<Sealed>]
    internal IntMapDebuggerProxy<'T> (map : IntMap<'T>) =

    [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
    member __.Items
        with get () : KeyValuePair<int, 'T>[] =
            map.ToKvpArray ()


/// Functional programming operators related to the IntMap<_> type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IntMap =
    /// The empty map.
    [<CompiledName("Empty")>]
    let empty<'T> =
        IntMap<'T>.Empty

    /// Is the map empty?
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (map : IntMap<'T>) : bool =
        // Preconditions
        checkNonNull "map" map

        map.IsEmpty

    /// Returns the number of bindings in the map.
    [<CompiledName("Count")>]
    let inline count (map : IntMap<'T>) : int =
        // Preconditions
        checkNonNull "map" map
        
        map.Count

    /// The IntMap containing the given binding.
    [<CompiledName("Singleton")>]
    let inline singleton (key : int) (value : 'T) : IntMap<'T> =
        IntMap.Singleton (key, value)

    /// Tests if an element is in the domain of the map.
    [<CompiledName("ContainsKey")>]
    let inline containsKey (key : int) (map : IntMap<'T>) : bool =
        // Preconditions
        checkNonNull "map" map

        map.ContainsKey key

    /// Look up an element in the map returning a Some value if the
    /// element is in the domain of the map and None if not.
    [<CompiledName("TryFind")>]
    let inline tryFind (key : int) (map : IntMap<'T>) : 'T option =
        // Preconditions
        checkNonNull "map" map
        
        map.TryFind key
        
    /// Look up an element in the map, raising KeyNotFoundException
    /// if the map does not contain a binding for the key.
    [<CompiledName("Find")>]
    let inline find (key : int) (map : IntMap<'T>) : 'T =
        // Preconditions
        checkNonNull "map" map

        map.Find key

    /// Look up an element in the map, returning the given default value
    /// if the map does not contain a binding for the key.
    [<CompiledName("FindOrDefault")>]
    let inline findOrDefault defaultValue (key : int) (map : IntMap<'T>) =
        defaultArg (map.TryFind key) defaultValue

    /// Returns the key of the first mapping in the collection which satisfies the given
    /// predicate. Returns None if no such mapping is found.
    [<CompiledName("TryFindKey")>]
    let inline tryFindKey (predicate : int -> 'T -> bool) (map : IntMap<'T>) : int option =
        // Preconditions
        checkNonNull "map" map
        
        map.TryFindKey predicate

    /// Returns a new map with the binding added to this map.
    [<CompiledName("Add")>]
    let inline add (key : int) (value : 'T) (map : IntMap<'T>) : IntMap<'T> =
        // Preconditions
        checkNonNull "map" map

        map.Add (key, value)

    //
    [<CompiledName("TryAdd")>]
    let inline tryAdd (key : int) (value : 'T) (map : IntMap<'T>) : IntMap<'T> =
        // Preconditions
        checkNonNull "map" map

        map.TryAdd (key, value)

    /// Removes an element from the domain of the map.
    /// No exception is raised if the element is not present.
    [<CompiledName("Remove")>]
    let inline remove (key : int) (map : IntMap<'T>) : IntMap<'T> =
        // Preconditions
        checkNonNull "map" map

        map.Remove key

    /// Returns a new map created by merging the two specified maps.
    [<CompiledName("Union")>]
    let inline union (map1 : IntMap<'T>) (map2 : IntMap<'T>) : IntMap<'T> =
        // Preconditions
        checkNonNull "map1" map1
        checkNonNull "map2" map2

        map1.Union map2

    /// Returns the intersection of two maps.
    [<CompiledName("Intersect")>]
    let inline intersect (map1 : IntMap<'T>) (map2 : IntMap<'T>) : IntMap<'T> =
        // Preconditions
        checkNonNull "map1" map1
        checkNonNull "map2" map2

        map1.Intersect map2

    /// Returns a new map created by removing the second map from the first.
    [<CompiledName("Difference")>]
    let inline difference (map1 : IntMap<'T>) (map2 : IntMap<'T>) : IntMap<'T> =
        // Preconditions
        checkNonNull "map1" map1
        checkNonNull "map2" map2

        map1.Difference map2

    /// Returns a new map made from the given bindings.
    [<CompiledName("OfSeq")>]
    let inline ofSeq source : IntMap<'T> =
        // Preconditions are checked by the member.
        IntMap.OfSeq source

    /// Returns a new map made from the given bindings.
    [<CompiledName("OfList")>]
    let inline ofList source : IntMap<'T> =
        // Preconditions are checked by the member.
        IntMap.OfList source

    /// Returns a new map made from the given bindings.
    [<CompiledName("OfArray")>]
    let inline ofArray source : IntMap<'T> =
        // Preconditions are checked by the member.
        IntMap.OfArray source

    /// Returns a new map made from the given bindings.
    [<CompiledName("OfMap")>]
    let inline ofMap source : IntMap<'T> =
        // Preconditions are checked by the member.
        IntMap.OfMap source

    /// Views the collection as an enumerable sequence of pairs.
    /// The sequence will be ordered by the keys of the map.
    [<CompiledName("ToSeq")>]
    let inline toSeq (map : IntMap<'T>) =
        // Preconditions
        checkNonNull "map" map
        
        map.ToSeq ()

    /// Returns a list of all key-value pairs in the mapping.
    /// The list will be ordered by the keys of the map.
    [<CompiledName("ToList")>]
    let inline toList (map : IntMap<'T>) =
        // Preconditions
        checkNonNull "map" map
        
        map.ToList ()

    /// Returns an array of all key-value pairs in the mapping.
    /// The list will be ordered by the keys of the map.
    [<CompiledName("ToArray")>]
    let inline toArray (map : IntMap<'T>) =
        // Preconditions
        checkNonNull "map" map

        map.ToArray ()

    /// Returns a new Map created from the given IntMap.
    [<CompiledName("ToMap")>]
    let inline toMap (map : IntMap<'T>) =
        // Preconditions
        checkNonNull "map" map

        map.ToMap ()

    /// Searches the map looking for the first element where the given function
    /// returns a Some value. If no such element is found, returns None.
    [<CompiledName("TryPick")>]
    let inline tryPick (picker : int -> 'T -> 'U option) (map : IntMap<'T>) : 'U option =
        // Preconditions
        checkNonNull "map" map
        
        map.TryPick picker

    /// Searches the map looking for the first element where the given function
    /// returns a Some value.
    [<CompiledName("Pick")>]
    let inline pick (picker : int -> 'T -> 'U option) (map : IntMap<'T>) : 'U =
        // Preconditions
        checkNonNull "map" map

        map.Pick picker

    /// Builds a new collection whose elements are the results of applying the given function
    /// to each of the elements of the collection. The key passed to the function indicates
    /// the key of the element being transformed.
    [<CompiledName("Map")>]
    let inline map (mapping : int -> 'T -> 'U) (map : IntMap<'T>) : IntMap<'U> =
        // Preconditions
        checkNonNull "map" map
        
        map.Map mapping

    /// <summary>
    /// Builds a new map containing only the bindings for which the given
    /// predicate returns &quot;true&quot;.
    /// </summary>
    [<CompiledName("Filter")>]
    let inline filter (predicate : int -> 'T -> bool) (map : IntMap<'T>) : IntMap<'T> =
        // Preconditions
        checkNonNull "map" map
        
        map.Filter predicate

    /// <summary>
    /// Applies the given function to each binding in the map.
    /// Returns the map comprised of the results "x" for each binding
    /// where the function returns <c>Some(x)</c>.
    /// </summary>
    [<CompiledName("Choose")>]
    let inline choose (chooser : int -> 'T -> 'U option) (map : IntMap<'T>) : IntMap<'U> =
        // Preconditions
        checkNonNull "map" map
        
        map.Choose chooser

    /// Applies the given function to each binding in the map.
    [<CompiledName("Iter")>]
    let inline iter (action : int -> 'T -> unit) (map : IntMap<'T>) : unit =
        // Preconditions
        checkNonNull "map" map
        
        map.Iterate action

    /// Applies the given function to each binding in the map.
    [<CompiledName("IterBack")>]
    let inline iterBack (action : int -> 'T -> unit) (map : IntMap<'T>) : unit =
        // Preconditions
        checkNonNull "map" map

        map.IterateBack action

    /// Folds over the bindings in the map.
    [<CompiledName("Fold")>]
    let inline fold (folder : 'State -> int -> 'T -> 'State)
            (state : 'State) (map : IntMap<'T>) : 'State =
        // Preconditions
        checkNonNull "map" map
        
        map.Fold (folder, state)

    /// Folds over the bindings in the map.
    [<CompiledName("FoldBack")>]
    let inline foldBack
            (folder : int -> 'T -> 'State -> 'State) (map : IntMap<'T>) (state : 'State) : 'State =
        // Preconditions
        checkNonNull "map" map

        map.FoldBack (folder, state)

    /// Determines if any binding in the map matches the specified predicate.
    [<CompiledName("Exists")>]
    let inline exists (predicate : int -> 'T -> bool) (map : IntMap<'T>) : bool =
        // Preconditions
        checkNonNull "map" map
        
        map.Exists predicate

    /// Determines if all bindings in the map match the specified predicate.
    [<CompiledName("Forall")>]
    let inline forall (predicate : int -> 'T -> bool) (map : IntMap<'T>) : bool =
        // Preconditions
        checkNonNull "map" map

        map.Forall predicate

    /// Splits the map into two maps containing the bindings for which the given
    /// predicate returns true and false, respectively.
    [<CompiledName("Partition")>]
    let inline partition (predicate : int -> 'T -> bool) (map : IntMap<'T>) : IntMap<'T> * IntMap<'T> =
        // Preconditions
        checkNonNull "map" map
        
        map.Partition predicate

    /// Splits the map into two maps by applying the given partitioning function
    /// to each binding in the map.
    [<CompiledName("MapPartition")>]
    let inline mapPartition (partitioner : int -> 'T -> Choice<'U, 'V>)
            (map : IntMap<'T>) : IntMap<'U> * IntMap<'V> =
        // Preconditions
        checkNonNull "map" map

        map.MapPartition partitioner


#if PROTO_COMPILER

/// <summary>Immutable maps with 'tagged' integer keys.</summary>
/// <typeparam name="Tag">The measure type used to 'tag' the integer keys of the map.</typeparam>
/// <typeparam name="T">The type of the values stored in the map.</typeparam>
[<MeasureAnnotatedAbbreviation>]
type TagMap<[<Measure>] 'Tag, 'T> =
    IntMap<'T>

/// Functional programming operators related to the TagMap<_,_> type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TagMap =
    /// Retypes a value without emitting any IL instructions.
    /// WARNING: This should be used with EXTREME CAUTION.
    [<NoDynamicInvocation>]
    [<CompiledName("RetypeInlined")>]
    let inline private retype<'T,'U> (x:'T) : 'U = (# "" x : 'U #)

    /// The empty map.
    [<CompiledName("Empty")>]
    let empty<[<Measure>] 'Tag, 'T> : TagMap<'Tag, 'T> =
        retype IntMap<'T>.Empty

    /// Is the map empty?
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (map : TagMap<'Tag, 'T>) : bool =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.IsEmpty

    /// Returns the number of bindings in the map.
    [<CompiledName("Count")>]
    let inline count (map : TagMap<'Tag, 'T>) : int =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map
        
        map.Count

    /// The map containing the given binding.
    [<CompiledName("Singleton")>]
    let inline singleton (key : int<'Tag>) (value : 'T) : TagMap<'Tag, 'T> =
        IntMap.Singleton (int key, value)
        |> retype

    /// Tests if an element is in the domain of the map.
    [<CompiledName("ContainsKey")>]
    let inline containsKey (key : int<'Tag>) (map : TagMap<'Tag, 'T>) : bool =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.ContainsKey (int key)
        |> retype

    /// Look up an element in the map, returning a Some value if the
    /// element is in the domain of the map and None if not.
    [<CompiledName("TryFind")>]
    let inline tryFind (key : int<'Tag>) (map : TagMap<'Tag, 'T>) : 'T option =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map
        
        map.TryFind (int key)
        
    /// Look up an element in the map, raising KeyNotFoundException
    /// if the map does not contain a binding for the key.
    [<CompiledName("Find")>]
    let inline find (key : int<'Tag>) (map : TagMap<'Tag, 'T>) : 'T =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Find (int key)

    /// Look up an element in the map, returning the given default value
    /// if the map does not contain a binding for the key.
    [<CompiledName("FindOrDefault")>]
    let inline findOrDefault defaultValue (key : int<'Tag>) (map : TagMap<'Tag, 'T>) : 'T =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        defaultArg (map.TryFind key) defaultValue

    /// Returns the key of the first mapping in the collection which satisfies the given
    /// predicate. Returns None if no such mapping is found.
    [<CompiledName("TryFindKey")>]
    let inline tryFindKey (predicate : int<'Tag> -> 'T -> bool) (map : TagMap<'Tag, 'T>) : int<'Tag> option =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.TryFindKey (retype predicate)
        |> retype

    /// Returns a new map with the binding added to this map.
    [<CompiledName("Add")>]
    let inline add (key : int<'Tag>) (value : 'T) (map : TagMap<'Tag, 'T>) : TagMap<'Tag, 'T> =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Add (int key, value)
        |> retype

    //
    [<CompiledName("TryAdd")>]
    let inline tryAdd (key : int<'Tag>) (value : 'T) (map : TagMap<'Tag, 'T>) : TagMap<'Tag, 'T> =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map
        
        // Preconditions
        checkNonNull "map" map

        map.TryAdd (key, value)
        |> retype

    /// Removes an element from the domain of the map.
    /// No exception is raised if the element is not present.
    [<CompiledName("Remove")>]
    let inline remove (key : int<'Tag>) (map : TagMap<'Tag, 'T>) : TagMap<'Tag, 'T> =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Remove (int key)
        |> retype

    /// Returns a new map created by merging the two specified maps.
    [<CompiledName("Union")>]
    let inline union (map1 : TagMap<'Tag, 'T>) (map2 : TagMap<'Tag, 'T>) : TagMap<'Tag, 'T> =
        // Retype as IntMap.
        let map1 : IntMap<'T> = retype map1
        let map2 : IntMap<'T> = retype map2

        // Preconditions
        checkNonNull "map1" map1
        checkNonNull "map2" map2

        map1.Union map2
        |> retype

    /// Returns the intersection of two maps.
    [<CompiledName("Intersect")>]
    let inline intersect (map1 : TagMap<'Tag, 'T>) (map2 : TagMap<'Tag, 'T>) : TagMap<'Tag, 'T> =
        // Retype as IntMap.
        let map1 : IntMap<'T> = retype map1
        let map2 : IntMap<'T> = retype map2

        // Preconditions
        checkNonNull "map1" map1
        checkNonNull "map2" map2

        map1.Intersect map2
        |> retype

    /// Returns a new map created by removing the second map from the first.
    [<CompiledName("Difference")>]
    let inline difference (map1 : TagMap<'Tag, 'T>) (map2 : TagMap<'Tag, 'T>) : TagMap<'Tag, 'T> =
        // Retype as IntMap.
        let map1 : IntMap<'T> = retype map1
        let map2 : IntMap<'T> = retype map2

        // Preconditions
        checkNonNull "map1" map1
        checkNonNull "map2" map2

        map1.Difference map2
        |> retype

    /// Returns a new map made from the given bindings.
    [<CompiledName("OfSeq")>]
    let inline ofSeq (source : seq<int<'Tag> * 'T>) : TagMap<'Tag, 'T> =
        // Preconditions are checked by the member.
        IntMap.OfSeq (retype source)
        |> retype

    /// Returns a new map made from the given bindings.
    [<CompiledName("OfList")>]
    let inline ofList (source : (int<'Tag> * 'T) list) : TagMap<'Tag, 'T> =
        // Preconditions are checked by the member.
        IntMap.OfList (retype source)
        |> retype

    /// Returns a new map made from the given bindings.
    [<CompiledName("OfArray")>]
    let inline ofArray (source : (int<'Tag> * 'T)[]) : TagMap<'Tag, 'T> =
        // Preconditions are checked by the member.
        IntMap.OfArray (retype source)
        |> retype

    /// Returns a new map made from the given bindings.
    [<CompiledName("OfMap")>]
    let inline ofMap (source : Map<int<'Tag>, 'T>) : TagMap<'Tag, 'T> =
        // Preconditions are checked by the member.
        IntMap.OfMap (retype source)
        |> retype

    /// Views the collection as an enumerable sequence of pairs.
    /// The sequence will be ordered by the keys of the map.
    [<CompiledName("ToSeq")>]
    let inline toSeq (map : TagMap<'Tag, 'T>) : seq<int<'Tag> * 'T> =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.ToSeq ()
        |> retype

    /// Returns a list of all key-value pairs in the mapping.
    /// The list will be ordered by the keys of the map.
    [<CompiledName("ToList")>]
    let inline toList (map : TagMap<'Tag, 'T>) : (int<'Tag> * 'T) list =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.ToList ()
        |> retype

    /// Returns an array of all key-value pairs in the mapping.
    /// The list will be ordered by the keys of the map.
    [<CompiledName("ToArray")>]
    let inline toArray (map : TagMap<'Tag, 'T>) : (int<'Tag> * 'T)[] =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.ToArray ()
        |> retype

    /// Returns a new Map created from the given IntMap.
    [<CompiledName("ToMap")>]
    let inline toMap (map : TagMap<'Tag, 'T>) : Map<int<'Tag>, 'T> =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.ToMap ()
        |> retype

    /// Searches the map looking for the first element where the given function
    /// returns a Some value. If no such element is found, returns None.
    [<CompiledName("TryPick")>]
    let inline tryPick (picker : int<'Tag> -> 'T -> 'U option) (map : TagMap<'Tag, 'T>) : 'U option =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.TryPick (retype picker)

    /// Searches the map looking for the first element where the given function
    /// returns a Some value.
    [<CompiledName("Pick")>]
    let inline pick (picker : int<'Tag> -> 'T -> 'U option) (map : TagMap<'Tag, 'T>) : 'U =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Pick (retype picker)

    /// Builds a new collection whose elements are the results of applying the given function
    /// to each of the elements of the collection. The key passed to the function indicates
    /// the key of the element being transformed.
    [<CompiledName("Map")>]
    let inline map (mapping : int<'Tag> -> 'T -> 'U) (map : TagMap<'Tag, 'T>) : TagMap<'Tag, 'U> =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Map (retype mapping)
        |> retype

    /// <summary>
    /// Builds a new map containing only the bindings for which the given
    /// predicate returns &quot;true&quot;.
    /// </summary>
    [<CompiledName("Filter")>]
    let inline filter (predicate : int<'Tag> -> 'T -> bool) (map : TagMap<'Tag, 'T>) : TagMap<'Tag, 'U> =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Filter (retype predicate)
        |> retype

    /// <summary>
    /// Applies the given function to each binding in the map.
    /// Returns the map comprised of the results "x" for each binding
    /// where the function returns <c>Some(x)</c>.
    /// </summary>
    [<CompiledName("Choose")>]
    let inline choose (chooser : int<'Tag> -> 'T -> 'U option) (map : TagMap<'Tag, 'T>) : TagMap<'Tag, 'U> =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Choose (retype chooser)
        |> retype

    /// Applies the given function to each binding in the map.
    [<CompiledName("Iter")>]
    let inline iter (action : int<'Tag> -> 'T -> unit) (map : TagMap<'Tag, 'T>) : unit =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Iterate (retype action)

    /// Applies the given function to each binding in the map.
    [<CompiledName("IterBack")>]
    let inline iterBack (action : int<'Tag> -> 'T -> unit) (map : TagMap<'Tag, 'T>) : unit =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.IterateBack (retype action)

    /// Folds over the bindings in the map.
    [<CompiledName("Fold")>]
    let inline fold (folder : 'State -> int<'Tag> -> 'T -> 'State) (state : 'State) (map : TagMap<'Tag, 'T>) : 'State =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Fold (retype folder, state)

    /// Folds over the bindings in the map.
    [<CompiledName("FoldBack")>]
    let inline foldBack (folder : int<'Tag> -> 'T -> 'State -> 'State) (map : TagMap<'Tag, 'T>) (state : 'State) : 'State =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.FoldBack (retype folder, state)

    /// Determines if any binding in the map matches the specified predicate.
    [<CompiledName("Exists")>]
    let inline exists (predicate : int<'Tag> -> 'T -> bool) (map : TagMap<'Tag, 'T>) : bool =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Exists (retype predicate)

    /// Determines if all bindings in the map match the specified predicate.
    [<CompiledName("Forall")>]
    let inline forall (predicate : int<'Tag> -> 'T -> bool) (map : TagMap<'Tag, 'T>) : bool =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Forall (retype predicate)

    /// Splits the map into two maps containing the bindings for which the given
    /// predicate returns true and false, respectively.
    [<CompiledName("Partition")>]
    let inline partition (predicate : int<'Tag> -> 'T -> bool) (map : TagMap<'Tag, 'T>) : TagMap<'Tag, 'T> * TagMap<'Tag, 'T> =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        let map1, map2 = map.Partition (retype predicate)
        (retype map1), (retype map2)

    /// Splits the map into two maps by applying the given partitioning function
    /// to each binding in the map.
    [<CompiledName("MapPartition")>]
    let inline mapPartition (partitioner : int<'Tag> -> 'T -> Choice<'U, 'V>) (map : TagMap<'Tag, 'T>) : TagMap<'Tag, 'U> * TagMap<'Tag, 'V> =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        let map1, map2 = map.MapPartition (retype partitioner)
        (retype map1), (retype map2)

#endif

