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


/// A list-based data structure implementing a set.
/// This is used within PatriciaHashSet to handle hash collisions in a manner reminiscent of a hashtable.
type private ListSet<[<ComparisonConditionalOn>] 'T when 'T : equality> =
    /// The tail (last element) in the list.
    | Tail of 'T
    /// A list element along with the rest of the list.
    | Element of ListSet<'T> * 'T

    /// Returns the number of bindings in the set.
    static member private CountImpl (listSet : ListSet<'T>, acc) : int =
        match listSet with
        | Tail _ ->
            acc + 1
        | Element (listSet,_) ->
            ListSet.CountImpl (listSet, acc + 1)

    /// Returns the number of bindings in the set.
    static member Count (listSet : ListSet<'T>) : int =
        // Call the recursive implementation.
        ListSet.CountImpl (listSet, 0)

    //
    static member Contains (value : 'T, listSet : ListSet<'T>) : bool =
        match listSet with
        | Tail x ->
            x = value
        | Element (listSet, x) ->
            x = value || ListSet.Contains (value, listSet)

    //
    static member private Rewind (stack : 'T list, listSet : ListSet<'T>) : ListSet<'T> =
        match stack with
        | [] ->
            listSet
        | value :: stack ->
            ListSet.Rewind (
                stack,
                Element (listSet, value))

    //
    static member RemoveImpl (stack : 'T list, listSet : ListSet<'T>, original, value : 'T) : ListSet<'T> option =
        match listSet with
        | Tail x ->
            if x = value then
                match stack with
                | [] -> None
                | v :: stack ->
                    ListSet.Rewind (stack, Tail v)
                    |> Some
            else
                // The specified key isn't in the set. so return the original set.without changing it.
                Some original

        | Element (listSet, x) ->
            if x = value then
                Some <| ListSet.Rewind (stack, listSet)
            else
                ListSet.RemoveImpl (x :: stack, listSet, original, value)

    //
    static member Remove (value : 'T, listSet : ListSet<'T>) : ListSet<'T> option =
        match listSet with
        | Tail x ->
            if x = value then None
            else Some listSet

        | Element (listSet, x) ->
            if x = value then Some listSet
            else
                ListSet.RemoveImpl ([x], listSet, listSet, value)

    //
    static member private AddImpl (stack : 'T list, listSet : ListSet<'T>, original, value : 'T) : ListSet<'T> =
        match listSet with
        | Tail x ->
            // OPTIMIZATION : If the value being inserted is _identical_ to the tail element,
            // the ListSet doesn't need to be modified -- so just return the original.
            if System.Object.ReferenceEquals (box x, value) then original
            else
                // Compare the value being inserted to the tail element and insert
                // the new element so that the list is still ordered.
                let c = Unchecked.compare x value
                if c = 0 then
                    original
                elif c < 0 then
                    // Add the new element to the end of the listSet.
                    let listSet = Element (Tail value, x)
                    ListSet.Rewind (stack, listSet)
                else
                    // Insert the new element before the tail.
                    let listSet = Element (listSet, x)
                    ListSet.Rewind (stack, listSet)

        | Element (listSet, x) ->
            // OPTIMIZATION : If the value being inserted is _identical_ to the tail element,
            // the ListSet doesn't need to be modified -- so just return the original.
            if System.Object.ReferenceEquals (box x, value) then original
            else
                // Compare the value being inserted to the current element and insert
                // the new element so that the list is still ordered.
                let c = Unchecked.compare x value
                if c = 0 then
                    original
                elif c < 0 then
                    // The new binding is inserted at some "later" point in the list.
                    ListSet.AddImpl (x :: stack, listSet, original, value)
                else
                    // Add the new binding prior to this binding, then rewind the stack onto
                    // the result to produce the new set.
                    let listSet = Element (listSet, value)
                    ListSet.Rewind (stack, listSet)

    //
    static member Add (value : 'T, listSet : ListSet<'T>) : ListSet<'T> =
        ListSet.AddImpl ([], listSet, listSet, value)

    //
    static member ToSeq (listSet : ListSet<'T>) =
        seq {
        match listSet with
        | Tail x ->
            yield x
        | Element (listSet, x) ->
            yield x
            yield! ListSet.ToSeq listSet
        }

    //
    static member private Reverse (reversed : ListSet<'T>, listSet : ListSet<'T>) =
        match listSet with
        | Tail x ->
            Element (reversed, x)
        | Element (listSet, x) ->
            ListSet.Reverse (Element (reversed, x), listSet)

    //
    static member private Reverse (listSet : ListSet<'T>) =
        match listSet with
        | Tail _ ->
            listSet
        | Element (listSet, x) ->
            ListSet.Reverse (Tail x, listSet)

    //
    static member Iterate (action : 'T -> unit, listSet : ListSet<'T>) =
        match listSet with
        | Tail x ->
            action x
        | Element (listSet, x) ->
            action x
            ListSet.Iterate (action, listSet)

    //
    static member IterateBack (action, listSet : ListSet<'T>) =
        ListSet.Iterate (action, ListSet.Reverse listSet)

    //
    static member Fold (folder : FSharpFunc<_,_,_>, listSet : ListSet<'T>, state : 'State) =
        match listSet with
        | Tail x ->
            folder.Invoke (state, x)
        | Element (listSet, x) ->
            let state = folder.Invoke (state, x)
            ListSet.Fold (folder, listSet, state)

    //
    static member FoldBack (folder : FSharpFunc<_,_,_>, listSet : ListSet<'T>, state : 'State) =
        let folder state x =
            folder.Invoke (x, state)
        let folder = FSharpFunc<_,_,_>.Adapt folder
        ListSet.Fold (folder, ListSet.Reverse listSet, state)

    //
    static member TryFind (predicate : 'T -> bool, listSet : ListSet<'T>) =
        match listSet with
        | Tail x ->
            if predicate x then
                Some x
            else None
        | Element (listSet, x) ->
            if predicate x then
                Some x
            else
                ListSet.TryFind (predicate, listSet)

    //
    static member TryPick (picker : 'T -> 'U option, listSet : ListSet<'T>) : 'U option =
        match listSet with
        | Tail x ->
            picker x
        | Element (listSet, x) ->
            match picker x with
            | None ->
                ListSet.TryPick (picker, listSet)
            | result ->
                result


(* OPTIMIZE :   Some of the functional-style operations on PatriciaHashSet use direct non-tail-recursion;
                performance may be improved if we modify these to use CPS instead. *)

/// A Patricia trie implementation, modified so each of it's 'values'
/// is actually a set implemented with a list.
/// Used as the underlying data structure for HashSet.
[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type private PatriciaHashSet<[<ComparisonConditionalOn>] 'T when 'T : equality> =
    | Empty
    // Key-HashCode * Value
    | Lf of uint32 * ListSet<'T>
    // Prefix * Mask * Left-Child * Right-Child
    | Br of uint32 * uint32 * PatriciaHashSet<'T> * PatriciaHashSet<'T>

    //
    static member Contains (valueHash, value : 'T, set : PatriciaHashSet<'T>) =
        match set with
        | Empty ->
            false
        | Lf (j, listSet) ->
            j = valueHash
            && ListSet.Contains (value, listSet)
        | Br (_, m, t0, t1) ->
            PatriciaHashSet.Contains (
                valueHash, value, (if zeroBit (valueHash, m) then t0 else t1))

    //
    static member Count (set : PatriciaHashSet<'T>) : int =
        match set with
        | Empty -> 0
        | Lf (_, listSet) ->
            ListSet.Count listSet
        | Br (_,_,_,_) ->
            /// The stack of trie nodes pending traversal.
            let stack = Stack (defaultTraversalStackSize)

            // Add the initial tree to the stack.
            stack.Push set

            // Traverse the tree, counting the elements by using the mutable stack.
            let mutable count = 0u

            while stack.Count <> 0 do
                match stack.Pop () with
                | Empty -> ()
                | Lf (_, listSet) ->
                    count <- count + (uint32 <| ListSet.Count listSet)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf (_, listSet1), Lf (_, listSet2)) ->
                    count <-
                        count
                        + (uint32 <| ListSet.Count listSet1)
                        + (uint32 <| ListSet.Count listSet2)
                    
                | Br (_, _, Lf (_, listSet), child)
                | Br (_, _, child, Lf (_, listSet)) ->
                    count <- count + (uint32 <| ListSet.Count listSet)
                    stack.Push child

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push right
                    stack.Push left

            // Return the computed element count.
            int count

    /// Remove the binding with the specified key from the set.
    /// No exception is thrown if the set.does not contain a binding for the key.
    static member Remove (valueHash, value : 'T, set : PatriciaHashSet<'T>) =
        match set with
        | Empty ->
            Empty
        | Lf (j, listSet) ->
            if j = valueHash then
                match ListSet.Remove (value, listSet) with
                | None ->
                    Empty
                | Some result ->
                    // OPTIMIZATION : If the result is the same as the input, return the
                    // original set.since it wasn't modified.
                    if result === listSet then set
                    else
                        Lf (j, result)
            else set
        
        | Br (p, m, t0, t1) ->
            if matchPrefix (valueHash, p, m) then
                if zeroBit (valueHash, m) then
                    match PatriciaHashSet.Remove (valueHash, value, t0) with
                    | Empty -> t1
                    | left ->
                        // OPTIMIZE : If left === t0 then return the original set.instead of creating a new one.
                        Br (p, m, left, t1)
                else
                    match PatriciaHashSet.Remove (valueHash, value, t1) with
                    | Empty -> t0
                    | right ->
                        // OPTIMIZE : If right === t1 then return the original set.instead of creating a new one.
                        Br (p, m, t0, right)
            else set

    //
    static member inline private Join (p0, t0 : PatriciaHashSet<'T>, p1, t1) =
        let m = branchingBit (p0, p1)
        let p = mask (p0, m)
        if zeroBit (p0, m) then
            Br (p, m, t0, t1)
        else
            Br (p, m, t1, t0)

    /// Insert a binding (key-value pair) into a set. returning a new, updated set.
    static member Add (valueHash, value : 'T, set) =
        match set with
        | Empty ->
            Lf (valueHash, Tail value)
        | Lf (j, listSet) ->
            if j = valueHash then
                let result = ListSet.Add (value, listSet)

                // OPTIMIZATION : If the result is the same as the input, return the original
                // set.instead since it wasn't modified.
                if result === listSet then set
                else
                    Lf (j, result)
            else
                PatriciaHashSet.Join (valueHash, Lf (valueHash, Tail value), j, set)
        | Br (p, m, t0, t1) ->
            if matchPrefix (valueHash, p, m) then
                if zeroBit (valueHash, m) then
                    let left = PatriciaHashSet.Add (valueHash, value, t0)

                    // OPTIMIZE : If left === t0 then return the original set.instead of creating a new one.
                    Br (p, m, left, t1)
                else
                    let right = PatriciaHashSet.Add (valueHash, value, t1)

                    // OPTIMIZE : If right === t1 then return the original set.instead of creating a new one.
                    Br (p, m, t0, right)
            else
                PatriciaHashSet.Join (valueHash, Lf (valueHash, Tail value), p, set)

(*
    /// Computes the union of two PatriciaHashSets.
    static member Union (s, t) : PatriciaHashSet<'T> =
        match s, t with
        | Br (p, m, s0, s1), Br (q, n, t0, t1) ->
            if m = n then
                if p = q then
                    // The trees have the same prefix. Merge the subtrees.
                    let left = PatriciaHashSet.Union (s0, t0)
                    let right = PatriciaHashSet.Union (s1, t1)
                    Br (p, m, left, right)
                else
                    // The prefixes disagree.
                    PatriciaHashSet.Join (p, s, q, t)

            #if LITTLE_ENDIAN_TRIES
            elif m < n then
            #else
            elif m > n then
            #endif
                if matchPrefix (q, p, m) then
                    // q contains p. Merge t with a subtree of s.
                    if zeroBit (q, m) then
                        let left = PatriciaHashSet.Union (s0, t)
                        Br (p, m, left, s1)
                    else
                        let right = PatriciaHashSet.Union (s1, t)
                        Br (p, m, s0, right)
                else
                    // The prefixes disagree.
                    PatriciaHashSet.Join (p, s, q, t)

            else
                if matchPrefix (p, q, n) then
                    // p contains q. Merge s with a subtree of t.
                    if zeroBit (p, n) then
                        let left = PatriciaHashSet.Union (s, t0)
                        Br (q, n, left, t1)
                    else
                        let right = PatriciaHashSet.Union (s, t1)
                        Br (q, n, t0, right)
                else
                    // The prefixes disagree.
                    PatriciaHashSet.Join (p, s, q, t)

        | Br (p, m, s0, s1), Lf (k, x) ->
            if matchPrefix (k, p, m) then
                if zeroBit (k, m) then
                    let left = PatriciaHashSet.TryAdd (k, x, s0)
                    Br (p, m, left, s1)
                else
                    let right = PatriciaHashSet.TryAdd (k, x, s1)
                    Br (p, m, s0, right)
            else
                PatriciaHashSet.Join (k, Lf (k, x), p, s)

        | Br (_,_,_,_), Empty ->
            s
        | Lf (k, x), _ ->
            PatriciaHashSet.Add (k, x, t)
        | Empty, _ -> t

    /// Compute the intersection of two PatriciaHashSets.
    static member Intersect (s, t) : PatriciaHashSet<'T> =
        match s, t with
        | Br (p, m, s0, s1), Br (q, n, t0, t1) ->
            if m = n then
                if p <> q then Empty
                else
                    let left = PatriciaHashSet.Intersect (s0, t0)
                    let right = PatriciaHashSet.Intersect (s1, t1)
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
                        PatriciaHashSet.Intersect (s0, t)
                    else
                        PatriciaHashSet.Intersect (s1, t)
                else
                    Empty

            else
                if matchPrefix (p, q, n) then
                    if zeroBit (p, n) then
                        PatriciaHashSet.Intersect (s, t0)
                    else
                        PatriciaHashSet.Intersect (s, t1)
                else
                    Empty

        | Br (_, m, s0, s1), Lf (k, y) ->
            let s' = if zeroBit (k, m) then s0 else s1
            match PatriciaHashSet.TryFind (k, s') with
            | Some x ->
                Lf (k, x)
            | None ->
                Empty
            
        | Br (_,_,_,_), Empty ->
            Empty
            
        | Lf (k, x), _ ->
            // Here, we always use the value from the left tree, so as long as the
            // right tree contains a binding with the same key, we just return the left tree.
            if PatriciaHashSet.ContainsKey (k, t) then s
            else Empty

        | Empty, _ ->
            Empty

    /// Compute the difference of two PatriciaHashSets.
    static member Difference (s, t) : PatriciaHashSet<'T> =
        match s, t with
        | Br (p, m, s0, s1), Br (q, n, t0, t1) ->
            if m = n then
                if p <> q then s
                else
                    let left = PatriciaHashSet.Difference (s0, t0)
                    let right = PatriciaHashSet.Difference (s1, t1)
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
                        match PatriciaHashSet.Difference (s0, t) with
                        | Empty -> s1
                        | left ->
                            Br (p, m, left, s1)
                    else
                        match PatriciaHashSet.Difference (s1, t) with
                        | Empty -> s0
                        | right ->
                            Br (p, m, s0, right)
                else s

            else
                if matchPrefix (p, q, n) then
                    if zeroBit (p, n) then
                        PatriciaHashSet.Difference (s, t0)
                    else
                        PatriciaHashSet.Difference (s, t1)
                else s

        | Br (p, m, s0, s1), Lf (k, y) ->
            if matchPrefix (k, p, m) then
                if zeroBit (k, m) then
                    match PatriciaHashSet.Remove (k, s0) with
                    | Empty -> s1
                    | left ->
                        Br (p, m, left, s1)
                else
                    match PatriciaHashSet.Remove (k, s1) with
                    | Empty -> s0
                    | right ->
                        Br (p, m, s0, right)
            else s
            
        | Br (_,_,_,_), Empty ->
            s
        | Lf (k, x), _ ->
            if PatriciaHashSet.ContainsKey (k, t) then Empty
            else s
        | Empty, _ ->
            Empty
*)
(*
    /// <c>IsSubmapOfBy f t1 t2</c> returns <c>true</c> if all keys in t1 are in t2,
    /// and when 'f' returns <c>true</c> when applied to their respective values.
    static member IsSubmapOfBy (predicate : 'T -> 'T -> bool) (t1 : PatriciaHashSet<'T>) (t2 : PatriciaHashSet<'T>) : bool =
        match t1, t2 with
        | (Br (p1, m1, l1, r1) as t1), (Br (p2, m2, l2, r2) as t2) ->
            if shorter (m1, m2) then
                false
            elif shorter (m2, m1) then
                matchPrefix (p1, p2, m2) && (
                    if zeroBit (p1, m2) then
                        PatriciaHashSet.IsSubmapOfBy predicate t1 l2
                    else
                        PatriciaHashSet.IsSubmapOfBy predicate t1 r2)
            else
                p1 = p2
                && PatriciaHashSet.IsSubmapOfBy predicate l1 l2
                && PatriciaHashSet.IsSubmapOfBy predicate r1 r2
                
        | Br (_,_,_,_), _ ->
            false
        | Lf (k, x), t ->
            match PatriciaHashSet.TryFind (k, t) with
            | None ->
                false
            | Some y ->
                predicate x y
        | Empty, _ ->
            true

    //
    static member private SubsetCmp (predicate : 'T -> 'T -> bool) (t1 : PatriciaHashSet<'T>) (t2 : PatriciaHashSet<'T>) : int =
        match t1, t2 with
        | (Br (p1, m1, l1, r1) as t1), (Br (p2, m2, l2, r2) as t2) ->
            if shorter (m1, m2) then 1
            elif shorter (m2, m1) then
                if not <| matchPrefix (p1, p2, m2) then 1
                elif zeroBit (p1, m2) then
                    PatriciaHashSet.SubsetCmp predicate t1 l2
                else
                    PatriciaHashSet.SubsetCmp predicate t1 r2
            elif p1 = p2 then
                let left = PatriciaHashSet.SubsetCmp predicate l1 l2
                let right = PatriciaHashSet.SubsetCmp predicate r1 r2
                match left, right with
                | 1, _
                | _, 1 -> 1
                | 0, 0 -> 0
                | _ -> -1
            else
                // The set. are disjoint.
                1

        | Br (_,_,_,_), _ -> 1
        | Lf (kx, x), Lf (ky, y) ->
            if kx = ky && predicate x y then 0
            else 1  // The sets are disjoint.
        | Lf (k, x), t ->
            match PatriciaHashSet.TryFind (k, t) with
            | Some y when predicate x y -> -1
            | _ -> 1    // The sets are disjoint.

        | Empty, Empty -> 0
        | Empty, _ -> -1
*)

    //
    static member OfSeq (source : seq<'T>) : PatriciaHashSet<'T> =
        (Empty, source)
        ||> Seq.fold (fun trie value ->
            let valueHash = uint32 <| hash value
            PatriciaHashSet.Add (valueHash, value, trie))

    //
    static member OfList (source : 'T list) : PatriciaHashSet<'T> =
        // Preconditions
        checkNonNull "source" source

        (Empty, source)
        ||> List.fold (fun trie value ->
            let valueHash = uint32 <| hash value
            PatriciaHashSet.Add (valueHash, value, trie))

    //
    static member OfArray (source : 'T[]) : PatriciaHashSet<'T> =
        // Preconditions
        checkNonNull "source" source

        (Empty, source)
        ||> Array.fold (fun trie value ->
            let valueHash = uint32 <| hash value
            PatriciaHashSet.Add (valueHash, value, trie))

    //
    static member Iterate (action : 'T -> unit, set) : unit =
        match set with
        | Empty -> ()
        | Lf (_, listSet) ->
            ListSet.Iterate (action, listSet)
        | Br (_,_,_,_) ->
            /// The stack of trie nodes pending traversal.
            let stack = Stack (defaultTraversalStackSize)

            // Add the initial tree to the stack.
            stack.Push set

            // Loop until we've processed the entire tree.
            while stack.Count <> 0 do
                match stack.Pop () with
                | Empty -> ()
                | Lf (_, listSet) ->
                    ListSet.Iterate (action, listSet)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf (_, listSet1), Lf (_, listSet2)) ->
                    ListSet.Iterate (action, listSet1)
                    ListSet.Iterate (action, listSet2)
                    
                | Br (_, _, Lf (_, listSet), right) ->
                    // Only handle the case where the left child is a leaf
                    // -- otherwise the traversal order would be altered.
                    ListSet.Iterate (action, listSet)
                    stack.Push right

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push right
                    stack.Push left

    //
    static member IterateBack (action : 'T -> unit, set) : unit =
        match set with
        | Empty -> ()
        | Lf (_, listSet) ->
            ListSet.IterateBack (action, listSet)
        | Br (_,_,_,_) ->
            /// The stack of trie nodes pending traversal.
            let stack = Stack (defaultTraversalStackSize)

            // Add the initial tree to the stack.
            stack.Push set

            // Loop until we've processed the entire tree.
            while stack.Count <> 0 do
                match stack.Pop () with
                | Empty -> ()
                | Lf (_, listSet) ->
                    ListSet.IterateBack (action, listSet)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf (_, listSet1), Lf (_, listSet2)) ->
                    ListSet.IterateBack (action, listSet1)
                    ListSet.IterateBack (action, listSet2)
                    
                | Br (_, _, left, Lf (_, listSet)) ->
                    // Only handle the case where the right child is a leaf
                    // -- otherwise the traversal order would be altered.
                    ListSet.IterateBack (action, listSet)
                    stack.Push left

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push left
                    stack.Push right

    //
    static member Fold (folder : 'State -> 'T -> 'State, state : 'State, set) : 'State =
        match set with
        | Empty ->
            state
        | Lf (_, listSet) ->
            let folder = FSharpFunc<_,_,_>.Adapt folder
            ListSet.Fold (folder, listSet, state)
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
                | Lf (_, listSet) ->
                    state <- ListSet.Fold (folder, listSet, state)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf (_, listSet1), Lf (_, listSet2)) ->
                    state <- ListSet.Fold (folder, listSet1, state)
                    state <- ListSet.Fold (folder, listSet2, state)
                    
                | Br (_, _, Lf (_, listSet), right) ->
                    // Only handle the case where the left child is a leaf
                    // -- otherwise the traversal order would be altered.
                    state <- ListSet.Fold (folder, listSet, state)
                    stack.Push right

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push right
                    stack.Push left

            // Return the final state value.
            state

    //
    static member FoldBack (folder : 'T -> 'State -> 'State, state : 'State, set) : 'State =
        match set with
        | Empty ->
            state
        | Lf (_, listSet) ->
            let folder = FSharpFunc<_,_,_>.Adapt folder
            ListSet.FoldBack (folder, listSet, state)
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
                | Lf (_, listSet) ->
                    state <- ListSet.FoldBack (folder, listSet, state)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf (_, listSet1), Lf (_, listSet2)) ->
                    state <- ListSet.FoldBack (folder, listSet1, state)
                    state <- ListSet.FoldBack (folder, listSet2, state)
                    
                | Br (_, _, left, Lf (_, listSet)) ->
                    // Only handle the case where the right child is a leaf
                    // -- otherwise the traversal order would be altered.
                    state <- ListSet.FoldBack (folder, listSet, state)
                    stack.Push left

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push left
                    stack.Push right

            // Return the final state value.
            state

    //
    static member TryFind (predicate : 'T -> bool, set) : 'T option =
        match set with
        | Empty ->
            None
        | Lf (_, listSet) ->
            ListSet.TryFind (predicate, listSet)
        | Br (_,_,_,_) ->
            /// The stack of trie nodes pending traversal.
            let stack = Stack (defaultTraversalStackSize)

            // Add the initial tree to the stack.
            stack.Push set

            /// Loop until we find a key/value that matches the predicate,
            /// or until we've processed the entire tree.
            let mutable matchingKey = None
            while stack.Count <> 0 && Option.isNone matchingKey do
                match stack.Pop () with
                | Empty -> ()
                | Lf (_, listSet) ->
                    matchingKey <- ListSet.TryFind (predicate, listSet)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf (_, listSet1), Lf (_, listSet2)) ->
                    matchingKey <- ListSet.TryFind (predicate, listSet1)
                    if Option.isNone matchingKey then
                        matchingKey <- ListSet.TryFind (predicate, listSet2)
                    
                | Br (_, _, Lf (_, listSet), right) ->
                    // Only handle the case where the left child is a leaf
                    // -- otherwise the traversal order would be altered.
                    matchingKey <- ListSet.TryFind (predicate, listSet)
                    stack.Push right

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push right
                    stack.Push left

            // Return the matching key, if one was found.
            matchingKey

    //
    static member TryPick (picker : 'T -> 'U option, set) : 'U option =
        match set with
        | Empty ->
            None
        | Lf (_, listSet) ->
            ListSet.TryPick (picker, listSet)
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
                | Lf (_, listSet) ->
                    pickedValue <- ListSet.TryPick (picker, listSet)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf (_, listSet1), Lf (_, listSet2)) ->
                    pickedValue <-
                        match ListSet.TryPick (picker, listSet1) with
                        | (Some _) as value ->
                            value
                        | None ->
                            ListSet.TryPick (picker, listSet2)
                    
                | Br (_, _, Lf (_, listSet), right) ->
                    // Only handle the case where the left child is a leaf
                    // -- otherwise the traversal order would be altered.
                    pickedValue <- ListSet.TryPick (picker, listSet)
                    stack.Push right

                | Br (_, _, left, right) ->
                    // Push both children onto the stack and recurse to process them.
                    // NOTE : They're pushed in the opposite order we want to visit them!
                    stack.Push right
                    stack.Push left

            // Return the picked value.
            pickedValue

    //
    static member ToSeq (set : PatriciaHashSet<'T>) =
        seq {
        match set with
        | Empty -> ()
        | Lf (_, listSet) ->
            yield! ListSet.ToSeq listSet
        
        (* OPTIMIZATION :   When one or both children of this node are leaves,
                            we handle them directly since it's a little faster. *)
        | Br (_, _, Lf (_, listSet1), Lf (_, listSet2)) ->
            yield! ListSet.ToSeq listSet1
            yield! ListSet.ToSeq listSet2

        | Br (_, _, Lf (_, listSet), right) ->
            // Only handle the case where the left child is a leaf
            // -- otherwise the traversal order would be altered.
            yield! ListSet.ToSeq listSet
            yield! PatriciaHashSet.ToSeq right

        | Br (_, _, left, right) ->
            // Recursively visit the children.
            yield! PatriciaHashSet.ToSeq left
            yield! PatriciaHashSet.ToSeq right
        }


/// <summary>Immutable, unordered set..</summary>
/// <typeparam name="Key">The type of key used by the set.</typeparam>
/// <typeparam name="T">The type of the values stored in the set.</typeparam>
[<Sealed; CompiledName("FSharpHashSet`1")>]
//[<StructuredFormatDisplay("")>]
[<DebuggerDisplay("Count = {Count}")>]
[<DebuggerTypeProxy(typedefof<HashSetDebuggerProxy<int>>)>]
type HashSet<[<ComparisonConditionalOn>] 'T when 'T : equality>
    private (trie : PatriciaHashSet<'T>) =
    /// The empty HashSet instance.
    static let empty : HashSet<'T> =
        HashSet Empty

    /// The empty HashSet.
    static member Empty
        with get () = empty

    //
    new (elements : seq<'T>) =
        // Preconditions
        // TODO : Check for null input.

        // OPTIMIZE : Try to cast the sequence to array or list;
        // if it succeeds use the specialized method for that type for better performance.
        HashSet (PatriciaHashSet.OfSeq elements)

    /// The internal representation of the HashSet.
    member private __.Trie
        with get () = trie

    //
    static member private Equals (left : HashSet<'T>, right : HashSet<'T>) =
        Unchecked.equals left.Trie right.Trie

    //
    static member private Compare (left : HashSet<'T>, right : HashSet<'T>) =
        Unchecked.compare left.Trie right.Trie

    /// <inherit />
    override __.Equals other =
        match other with
        | :? HashSet<'T> as other ->
            Unchecked.equals trie other.Trie
        | _ ->
            false

    /// <inherit />
    override __.GetHashCode () =
        Unchecked.hash trie

    /// The number of bindings in the HashSet.
    member __.Count
        with get () : int =
            PatriciaHashSet.Count trie

    /// Is the set empty?
    member __.IsEmpty
        with get () =
            match trie with
            | Empty -> true
            | _ -> false

    /// Tests if an element is in the domain of the HashSet.
    member __.Contains (value : 'T) : bool =
        let valueHash = uint32 <| hash value
        PatriciaHashSet.Contains (valueHash, value, trie)

    /// Returns a new HashSet with the binding added to this HashSet.
    member this.Add (value : 'T) : HashSet<'T> =
        // If the trie isn't modified, just return this HashSet instead of creating a new one.
        let valueHash = uint32 <| hash value
        let trie' = PatriciaHashSet.Add (valueHash, value, trie)
        if trie === trie' then this
        else HashSet (trie')

    /// Removes an element from the domain of the HashSet.
    /// No exception is raised if the element is not present.
    member this.Remove (value : 'T) : HashSet<'T> =
        // If the trie isn't modified, just return this HashSet instead of creating a new one.
        let valueHash = uint32 <| hash value
        let trie' = PatriciaHashSet.Remove (valueHash, value, trie)
        if trie === trie' then this
        else HashSet (trie')
(*
    /// Returns a new HashSet created by merging the two specified HashSets.
    member this.Union (otherMap : HashSet<'T>) : HashSet<'T> =
        // If the result is the same (physical equality) to one of the inputs,
        // return that input instead of creating a new HashSet.
        let trie' = PatriciaHashSet.Union (trie, otherMap.Trie)
        if trie === trie' then this
        elif otherMap.Trie === trie' then otherMap
        else HashSet (trie')

    /// Returns the intersection of two HashSets.
    member this.Intersect (otherMap : HashSet<'T>) : HashSet<'T> =
        // If the result is the same (physical equality) to one of the inputs,
        // return that input instead of creating a new HashSet.
        let trie' = PatriciaHashSet.Intersect (trie, otherMap.Trie)
        if trie === trie' then this
        elif otherMap.Trie === trie' then otherMap
        else HashSet (trie')

    /// Returns a new HashSet created by removing the second HashSet from the first.
    member this.Difference (otherMap : HashSet<'T>) : HashSet<'T> =
        // If the result is the same (physical equality) to one of the inputs,
        // return that input instead of creating a new HashSet.
        let trie' = PatriciaHashSet.Difference (trie, otherMap.Trie)
        if trie === trie' then this
        elif otherMap.Trie === trie' then otherMap
        else HashSet (trie')

    /// Returns true if 'other' is a subset of this set.
    member this.IsSubsetOfBy (predicate, other : HashSet<'T>) : bool =
        PatriciaHashSet.IsSubsetOfBy predicate other.Trie trie
*)
    /// The HashSet containing the given binding.
    static member Singleton (value : 'T) : HashSet<'T> =
        let valueHash = uint32 <| hash value
        HashSet (
            Lf (valueHash, Tail value))

    /// Returns a new HashSet made from the given bindings.
    static member OfSeq (source : seq<'T>) : HashSet<'T> =
        // Preconditions
        checkNonNull "source" source

        HashSet (PatriciaHashSet.OfSeq source)

    /// Returns a new HashSet made from the given bindings.
    static member OfList (source : 'T list) : HashSet<'T> =
        // Preconditions
        checkNonNull "source" source

        // OPTIMIZATION : If the source is empty return immediately.
        if List.isEmpty source then
            HashSet.Empty
        else
            HashSet (PatriciaHashSet.OfList source)

    /// Returns a new HashSet made from the given bindings.
    static member OfArray (source : 'T[]) : HashSet<'T> =
        // Preconditions
        checkNonNull "source" source

        // OPTIMIZATION : If the source is empty return immediately.
        if Array.isEmpty source then
            HashSet.Empty
        else
            HashSet (PatriciaHashSet.OfArray source)

    //
    member __.ToSeq () =
        PatriciaHashSet.ToSeq trie

    //
    member __.ToList () : 'T list =
        PatriciaHashSet.FoldBack ((fun x list -> x :: list), [], trie)

    //
    member __.ToArray () : 'T[] =
        let elements = ResizeArray ()
        PatriciaHashSet.Iterate (elements.Add, trie)
        elements.ToArray ()

    //
    member __.Iterate (action : 'T -> unit) : unit =
        PatriciaHashSet.Iterate (action, trie)

    //
    member __.IterateBack (action : 'T -> unit) : unit =
        PatriciaHashSet.IterateBack (action, trie)

    //
    member __.Fold (folder : 'State -> 'T -> 'State, state : 'State) : 'State =
        PatriciaHashSet.Fold (folder, state, trie)

    //
    member __.FoldBack (folder : 'T -> 'State -> 'State, state : 'State) : 'State =
        PatriciaHashSet.FoldBack (folder, state, trie)

    //
    member this.Exists (predicate : 'T -> bool) : bool =
        this.TryFind predicate
        |> Option.isSome

    //
    member this.Forall (predicate : 'T -> bool) : bool =
        this.TryFind (predicate >> not)
        |> Option.isNone

    //
    member __.TryFind (predicate : 'T -> bool) : 'T option =
        PatriciaHashSet.TryFind (predicate, trie)

    //
    member this.Find (predicate : 'T -> bool) : 'T =
        match this.TryFind predicate with
        | Some value ->
            value
        | None ->
            // TODO : Add a better error message
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    //
    member __.TryPick (picker : 'T -> 'U option) : 'U option =
        PatriciaHashSet.TryPick (picker, trie)

    //
    member this.Pick (picker : 'T -> 'U option) : 'U =
        match this.TryPick picker with
        | Some value ->
            value
        | None ->
            // TODO : Add a better error message
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    //
    member this.Choose (chooser : 'T -> 'U option) : HashSet<'U> =
        this.Fold ((fun chosenSet value ->
            match chooser value with
            | None ->
                chosenSet
            | Some newValue ->
                chosenSet.Add newValue), HashSet.Empty)

    //
    member this.Filter (predicate : 'T -> bool) : HashSet<'T> =
        this.Fold ((fun filteredSet value ->
            if predicate value then
                filteredSet
            else
                filteredSet.Remove value), this)

    (* OPTIMIZE : The methods below should be replaced with optimized implementations where possible. *)    

    //
    // OPTIMIZE : We should be able to implement an optimized version
    // of Map which builds the mapped trie from the bottom-up; this works
    // because the mapped trie will have the same structure as the original,
    // just with different values in the leaves.
    member this.Map (mapping : 'T -> 'U) : HashSet<'U> =
        this.Fold ((fun set value ->
            set.Add (mapping value)), HashSet.Empty)

    //
    member this.Partition (predicate : 'T -> bool) : HashSet<'T> * HashSet<'T> =
        this.Fold ((fun (trueSet, falseSet) value ->
            if predicate value then
                trueSet.Add value,
                falseSet
            else
                trueSet,
                falseSet.Add value), (HashSet.Empty, HashSet.Empty))

    //
    member this.MapPartition (partitioner : 'T -> Choice<'U, 'V>) : HashSet<'U> * HashSet<'V> =
        this.Fold ((fun (set1, set2) value ->
            match partitioner value with
            | Choice1Of2 result ->
                set1.Add result,
                set2
            | Choice2Of2 result ->
                set1,
                set2.Add result), (HashSet.Empty, HashSet.Empty))

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
        | [] -> "hashSet []"
        | [h1] ->
            System.Text.StringBuilder()
                .Append("hashSet [")
                .Append(HashSet<_>.ElementString h1)
                .Append("]")
                .ToString()
        | [h1; h2] ->
            System.Text.StringBuilder()
                .Append("hashSet [")
                .Append(HashSet<_>.ElementString h1)
                .Append("; ")
                .Append(HashSet<_>.ElementString h2)
                .Append("]")
                .ToString()
        | [h1; h2; h3] ->
            System.Text.StringBuilder()
                .Append("hashSet [")
                .Append(HashSet<_>.ElementString h1)
                .Append("; ")
                .Append(HashSet<_>.ElementString h2)
                .Append("; ")
                .Append(HashSet<_>.ElementString h3)
                .Append("]")
                .ToString()
        | h1 :: h2 :: h3 :: _ ->
            System.Text.StringBuilder()
                .Append("hashSet [")
                .Append(HashSet<_>.ElementString h1)
                .Append("; ")
                .Append(HashSet<_>.ElementString h2)
                .Append("; ")
                .Append(HashSet<_>.ElementString h3)
                .Append("; ... ]")
                .ToString()

    interface System.IEquatable<HashSet<'T>> with
        /// <inherit />
        member this.Equals other =
            HashSet<_>.Equals (this, other)

    interface System.IComparable with
        /// <inherit />
        member this.CompareTo other =
            HashSet<_>.Compare (this, other :?> HashSet<'T>)

    interface System.IComparable<HashSet<'T>> with
        /// <inherit />
        member this.CompareTo other =
            HashSet<_>.Compare (this, other)

    interface System.Collections.IEnumerable with
        /// <inherit />
        member __.GetEnumerator () =
            (PatriciaHashSet.ToSeq trie).GetEnumerator ()
            :> System.Collections.IEnumerator

    interface IEnumerable<'T> with
        /// <inherit />
        member __.GetEnumerator () =
            (PatriciaHashSet.ToSeq trie).GetEnumerator ()

    interface ICollection<'T> with
        /// <inherit />
        member __.Count
            with get () =
                PatriciaHashSet.Count trie

        /// <inherit />
        member __.IsReadOnly
            with get () = true

        /// <inherit />
        member __.Add _ =
            notSupported "HashSets cannot be mutated."

        /// <inherit />
        member __.Clear () =
            notSupported "HashSets cannot be mutated."

        /// <inherit />
        member __.Contains (item : 'T) =
            let valueHash = uint32 <| hash item
            PatriciaHashSet.Contains (valueHash, item, trie)

        /// <inherit />
        member this.CopyTo (array, arrayIndex) =
            // Preconditions
            checkNonNull "array" array
            if arrayIndex < 0 then
                argOutOfRange "arrayIndex" "The target array index cannot be negative."

            let count = PatriciaHashSet.Count trie
            if arrayIndex + count > Array.length array then
                invalidArg "arrayIndex"
                    "There is not enough room in the array to copy the \
                     elements when starting at the specified index."

            this.Fold ((fun index value ->
                array.[index] <- value
                index + 1), arrayIndex)
            |> ignore

        /// <inherit />
        member __.Remove _ : bool =
            notSupported "HashSets cannot be mutated."

//
and [<Sealed>]
    internal HashSetDebuggerProxy<'T when 'T : equality> (set : HashSet<'T>) =

    [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
    member __.Items
        with get () : 'T[] =
            set.ToArray ()


/// Functional programming operators related to the HashSet type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HashSet =
    /// The empty set.
    [<CompiledName("Empty")>]
    let empty<'T when 'T : equality> =
        HashSet<'T>.Empty

    /// Is the set empty?
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (set : HashSet<'T>) : bool =
        // Preconditions
        checkNonNull "set" set

        set.IsEmpty

    /// Returns the number of bindings in the set.
    [<CompiledName("Count")>]
    let inline count (set : HashSet<'T>) : int =
        // Preconditions
        checkNonNull "set" set
        
        set.Count

    /// The HashSet containing the given binding.
    [<CompiledName("Singleton")>]
    let inline singleton (value : 'T) : HashSet<'T> =
        HashSet.Singleton value

    /// Tests if an element is in the domain of the set.
    [<CompiledName("ContainsKey")>]
    let inline contains (value : 'T) (set : HashSet<'T>) : bool =
        // Preconditions
        checkNonNull "set" set

        set.Contains value

    /// Returns the key of the first element in the collection which satisfies the given
    /// predicate. Returns None if no such element is found.
    [<CompiledName("TryFind")>]
    let inline tryFind (predicate : 'T -> bool) (set : HashSet<'T>) : 'T option =
        // Preconditions
        checkNonNull "set" set
        
        set.TryFind predicate

    //
    [<CompiledName("Find")>]
    let inline find (predicate : 'T -> bool) (set : HashSet<'T>) : 'T =
        // Preconditions
        checkNonNull "set" set

        set.Find predicate

    /// Returns a new set with the binding added to this set.
    [<CompiledName("Add")>]
    let inline add (value : 'T) (set : HashSet<'T>) : HashSet<'T> =
        // Preconditions
        checkNonNull "set" set

        set.Add value

    /// Removes an element from the domain of the set.
    /// No exception is raised if the element is not present.
    [<CompiledName("Remove")>]
    let inline remove (value : 'T) (set : HashSet<'T>) : HashSet<'T> =
        // Preconditions
        checkNonNull "set" set

        set.Remove value
(*
    /// Returns a new set created by merging the two specified sets.
    [<CompiledName("Union")>]
    let inline union (set1 : HashSet<'T>) (set2 : HashSet<'T>) : HashSet<'T> =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.Union set2

    /// Returns the intersection of two sets.
    [<CompiledName("Intersect")>]
    let inline intersect (set1 : HashSet<'T>) (set2 : HashSet<'T>) : HashSet<'T> =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.Intersect set2

    /// Returns a new set created by removing the second set from the first.
    [<CompiledName("Difference")>]
    let inline difference (set1 : HashSet<'T>) (set2 : HashSet<'T>) : HashSet<'T> =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.Difference set2
*)
    /// Returns a new set made from the given bindings.
    [<CompiledName("OfSeq")>]
    let inline ofSeq source : HashSet<'T> =
        // Preconditions are checked by the member.
        HashSet.OfSeq source

    /// Returns a new set made from the given bindings.
    [<CompiledName("OfList")>]
    let inline ofList source : HashSet<'T> =
        // Preconditions are checked by the member.
        HashSet.OfList source

    /// Returns a new set made from the given bindings.
    [<CompiledName("OfArray")>]
    let inline ofArray source : HashSet<'T> =
        // Preconditions are checked by the member.
        HashSet.OfArray source

    /// Returns a new set containing the given elements.
    [<CompiledName("OfSet")>]
    let ofSet source : HashSet<'T> =
        // Preconditions
        checkNonNull "source" source

        (HashSet.Empty, source)
        ||> Set.fold (fun hashSet value ->
            hashSet.Add value)

    /// Views the collection as an enumerable sequence of pairs.
    /// The sequence will be ordered by the keys of the set.
    [<CompiledName("ToSeq")>]
    let inline toSeq (set : HashSet<'T>) =
        // Preconditions
        checkNonNull "set" set
        
        set.ToSeq ()

    /// Returns a list of all elements in the set.
    /// The list will be ordered using generic comparison on the elements.
    [<CompiledName("ToList")>]
    let inline toList (set : HashSet<'T>) =
        // Preconditions
        checkNonNull "set" set
        
        set.ToList ()

    /// Returns an array of all elements in the set.
    /// The array will be ordered using generic comparison on the elements.
    [<CompiledName("ToArray")>]
    let inline toArray (set : HashSet<'T>) =
        // Preconditions
        checkNonNull "set" set

        set.ToArray ()

    /// Returns a new Set created from the given HashSet.
    [<CompiledName("ToSet")>]
    let toSet (set : HashSet<'T>) =
        // Preconditions
        checkNonNull "set" set

        set.Fold (flip Set.add, Set.empty)

    /// Searches the set looking for the first element where the given function
    /// returns a Some value. If no such element is found, returns None.
    [<CompiledName("TryPick")>]
    let inline tryPick (picker : 'T -> 'U option) (set : HashSet<'T>) : 'U option =
        // Preconditions
        checkNonNull "set" set
        
        set.TryPick picker

    /// Searches the set looking for the first element where the given function
    /// returns a Some value.
    [<CompiledName("Pick")>]
    let inline pick (picker : 'T -> 'U option) (set : HashSet<'T>) : 'U =
        // Preconditions
        checkNonNull "set" set

        set.Pick picker

    /// Builds a new collection whose elements are the results of applying the given function
    /// to each of the elements of the collection. The key passed to the function indicates
    /// the key of the element being transformed.
    [<CompiledName("Map")>]
    let inline map (mapping : 'T -> 'U) (set : HashSet<'T>) : HashSet<'U> =
        // Preconditions
        checkNonNull "set" set
        
        set.Map mapping

    /// <summary>
    /// Builds a new set containing only the bindings for which the given
    /// predicate returns &quot;true&quot;.
    /// </summary>
    [<CompiledName("Filter")>]
    let inline filter (predicate : 'T -> bool) (set : HashSet<'T>) : HashSet<'T> =
        // Preconditions
        checkNonNull "set" set
        
        set.Filter predicate

    /// <summary>
    /// Applies the given function to each binding in the set.
    /// Returns the set comprised of the results "x" for each binding
    /// where the function returns <c>Some(x)</c>.
    /// </summary>
    [<CompiledName("Choose")>]
    let inline choose (chooser : 'T -> 'U option) (set : HashSet<'T>) : HashSet<'U> =
        // Preconditions
        checkNonNull "set" set
        
        set.Choose chooser

    /// Applies the given function to each binding in the set.
    [<CompiledName("Iterate")>]
    let inline iter (action : 'T -> unit) (set : HashSet<'T>) : unit =
        // Preconditions
        checkNonNull "set" set
        
        set.Iterate action

    /// Applies the given function to each binding in the set.
    [<CompiledName("IterateBack")>]
    let inline iterBack (action : 'T -> unit) (set : HashSet<'T>) : unit =
        // Preconditions
        checkNonNull "set" set

        set.IterateBack action

    /// Folds over the bindings in the set.
    [<CompiledName("Fold")>]
    let inline fold (folder : 'State -> 'T -> 'State) (state : 'State) (set : HashSet<'T>) : 'State =
        // Preconditions
        checkNonNull "set" set
        
        set.Fold (folder, state)

    /// Folds over the bindings in the set.
    [<CompiledName("FoldBack")>]
    let inline foldBack (folder : 'T -> 'State -> 'State) (set : HashSet<'T>) (state : 'State) : 'State =
        // Preconditions
        checkNonNull "set" set

        set.FoldBack (folder, state)

    /// Determines if any binding in the set matches the specified predicate.
    [<CompiledName("Exists")>]
    let inline exists (predicate : 'T -> bool) (set : HashSet<'T>) : bool =
        // Preconditions
        checkNonNull "set" set
        
        set.Exists predicate

    /// Determines if all bindings in the set match the specified predicate.
    [<CompiledName("Forall")>]
    let inline forall (predicate : 'T -> bool) (set : HashSet<'T>) : bool =
        // Preconditions
        checkNonNull "set" set

        set.Forall predicate

    /// Splits the set into two sets containing the bindings for which the given
    /// predicate returns true and false, respectively.
    [<CompiledName("Partition")>]
    let inline partition (predicate : 'T -> bool) (set : HashSet<'T>) : HashSet<'T> * HashSet<'T> =
        // Preconditions
        checkNonNull "set" set
        
        set.Partition predicate

    /// Splits the set into two sets by applying the given partitioning function
    /// to each binding in the set.
    [<CompiledName("MapPartition")>]
    let inline mapPartition (partitioner : 'T -> Choice<'U, 'V>) (set : HashSet<'T>) : HashSet<'U> * HashSet<'V> =
        // Preconditions
        checkNonNull "set" set

        set.MapPartition partitioner

