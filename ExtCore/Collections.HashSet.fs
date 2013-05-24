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
type private ListSet<[<ComparisonConditionalOn>] 'T when 'T : equality> = 'T list

//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module private ListSet =
    //
    [<CompiledName("Count")>]
    let inline count (set : ListSet<'T>) : int =
        List.length set

    /// "Rewinds" an accumulator and a list like a zipper whose context
    /// is being moved all the way to the beginning (head) of the list.
    let rec rewind path (tail : 'T list) =
        match path with
        | [] -> tail
        | el :: path ->
            rewind path (el :: tail)

    /// Zipper-like implementation of 'add'.
    let rec private addImpl path tail orig (value : 'T) =
        match tail with
        | [] ->
            // We can use List.rev instead of 'rewind' since we're adding the
            // value to the end of the list.
            List.rev (value :: path)

        | el :: tl ->
            // Should the value be inserted before 'el'?
            let c = Unchecked.compare value el
            if c = 0 then
                orig    // value = el, so we don't need to modify the list. Return the original list.
            elif c < 0 then
                // Cons the element onto the tail -- that is, prior to 'el' -- then rewind the zipper.
                rewind path (value :: tail)
            else
                // The value needs to be inserted at some later point.
                // Add 'el' to the path and continue traversing the list.
                addImpl (el :: path) tl orig value

    //
    [<CompiledName("Add")>]
    let add (value : 'T) (set : ListSet<'T>) : ListSet<'T> =
        // Call the recursive implementation.
        addImpl [] set set value

    /// Zipper-like implementation of 'remove'.
    let rec private removeImpl path tail orig (value : 'T) =
        match tail with
        | [] ->
            // The value wasn't found in the list, so there's nothing
            // to modify -- return the original list.
            orig
        | el :: tl ->
            // Is 'el' the value we want to remove?
            if el = value then
                // Rewind the zipper, leaving out 'el'.
                rewind path tl
            else
                // Add 'el' to the path and continue traversing the list.
                removeImpl (el :: path) tl orig value

    //
    [<CompiledName("Remove")>]
    let remove (value : 'T) (set : ListSet<'T>) : ListSet<'T> =
        // Call the recursive implementation.
        removeImpl [] set set value

    /// Recursive implementation of 'union'.
    let rec private unionImpl acc set1 set2 : ListSet<'T> =
        match set1, set2 with
        | [], [] ->
            // Reverse the combined list before returning so
            // the results are in the correct order.
            List.rev acc
        | hd :: tl, []
        | [], hd :: tl ->
            unionImpl (hd :: acc) tl []
        | hd1 :: tl1, hd2 :: tl2 ->
            let c = Unchecked.compare hd1 hd2

            // Based on the comparison, cons the "lesser" element onto the accumulator
            // (i.e., the merged list) then recurse to continue processing.
            if c = 0 then   // hd1 = hd2
                unionImpl (hd1 :: acc) tl1 tl2
            elif c < 0 then
                unionImpl (hd1 :: acc) tl1 set2
            else
                unionImpl (hd2 :: acc) set1 tl2

    //
    [<CompiledName("Union")>]
    let union (set1 : ListSet<'T>) (set2 : ListSet<'T>) : ListSet<'T> =
        // Call the recursive implementation.
        unionImpl [] set1 set2

    /// Recursive implementation of 'intersect'.
    let rec private intersectImpl acc set1 set2 : ListSet<'T> =
        match set1, set2 with
        | [], _
        | _, [] ->
            // Reverse the combined list before returning so
            // the results are in the correct order.
            List.rev acc
        | hd1 :: tl1, hd2 :: tl2 ->
            let c = Unchecked.compare hd1 hd2

            // If the heads of the lists are equal, add that element to the accumulator
            // (i.e., the merged list); otherwise, drop the "lesser" of the two head elements.
            if c = 0 then   // hd1 = hd2
                intersectImpl (hd1 :: acc) tl1 tl2
            elif c < 0 then
                intersectImpl acc tl1 set2
            else
                intersectImpl acc set1 tl2

    //
    [<CompiledName("Intersect")>]
    let intersect (set1 : ListSet<'T>) (set2 : ListSet<'T>) : ListSet<'T> =
        // Call the recursive implementation.
        intersectImpl [] set1 set2

    /// Recursive implementation of 'difference'.
    let rec private differenceImpl acc set1 set2 : ListSet<'T> =
        match set1, set2 with
        | [], _
        | _, [] ->
            // Reverse the combined list before returning so
            // the results are in the correct order.
            List.rev acc
        | hd1 :: tl1, hd2 :: tl2 ->
            let c = Unchecked.compare hd1 hd2

            // If hd1 = hd2 then we drop both of them, effectively "removing" the value from set1.
            if c = 0 then
                differenceImpl acc tl1 tl2
            elif c < 0 then
                differenceImpl (hd1 :: acc) tl1 tl2
            else
                differenceImpl acc set1 tl2

    //
    [<CompiledName("Difference")>]
    let difference (set1 : ListSet<'T>) (set2 : ListSet<'T>) : ListSet<'T> =
        // Call the recursive implementation.
        differenceImpl [] set1 set2

    /// <summary>
    /// Is <paramref name="set1"/> a subset of <paramref name="set2"/>?
    /// <c>isSubset set1 set2</c> returns <c>true</c> if all keys in <paramref name="set1"/> are in <paramref name="set2"/>.
    /// </summary>
    [<CompiledName("IsSubset")>]
    let rec isSubset (set1 : ListSet<'T>) (set2 : ListSet<'T>) : bool =
        match set1, set2 with
        | [], _ -> true
        | _, [] -> false
        | hd1 :: tl1, hd2 :: tl2 ->
            let c = Unchecked.compare hd1 hd2

            if c = 0 then isSubset tl1 tl2
            elif c < 0 then false
            else isSubset set1 tl2

    /// <summary>
    /// Is <paramref name="set1"/> a proper subset of <paramref name="set2"/>?
    /// <c>isProperSubset set1 set2</c> returns <c>true</c> if all keys in <paramref name="set1"/> are in <paramref name="set2"/>,
    /// and at least one element in <paramref name="set2"/> is not in <paramref name="set1"/>.
    /// </summary>
    [<CompiledName("IsProperSubset")>]
    let rec isProperSubset (set1 : ListSet<'T>) (set2 : ListSet<'T>) : bool =
        match set1, set2 with
        | _, [] -> false
        | [], _ -> true
        | hd1 :: tl1, hd2 :: tl2 ->
            let c = Unchecked.compare hd1 hd2

            if c = 0 then isProperSubset tl1 tl2
            elif c < 0 then false
            else isSubset set1 tl2

    /// Computes a containment ordering for two sets.
    /// For use with PatriciaHashSet.SubsetCompare.
    // Return values:
    //  -1 : All values in 't1' are in 't2', and at least one value of 't2' is not in 't1'.
    //   0 : The sets contain _exactly_ the same values.
    //   1 : The sets are disjoint, i.e., 't1' contains at least one value which is not in 't2'.
    [<CompiledName("SubsetCompare")>]
    let rec subsetCompare (set1 : ListSet<'T>) (set2 : ListSet<'T>) : int =
        match set1, set2 with
        | [], [] -> 0
        | _, [] -> 1
        | [], _ -> -1
        | hd1 :: tl1, hd2 :: tl2 ->
            let c = Unchecked.compare hd1 hd2

            if c = 0 then subsetCompare tl1 tl2
            elif c < 0 then 1
            else subsetCompare set1 tl2


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
            && List.contains value listSet
        | Br (_, m, t0, t1) ->
            PatriciaHashSet.Contains (
                valueHash, value, (if zeroBit (valueHash, m) then t0 else t1))

    /// Retrieve the first element of the set.
    static member First (set : PatriciaHashSet<'T>) =
        match set with
        | Empty ->
            invalidArg "set" "The set is empty."
        | Lf (_, listSet) ->
            List.head listSet
        | Br (_, _, t0, _) ->
            PatriciaHashSet.First t0

    /// Retrieve the last element of the set.
    static member Last (set : PatriciaHashSet<'T>) =
        match set with
        | Empty ->
            invalidArg "set" "The set is empty."
        | Lf (_, listSet) ->
            List.last listSet
        | Br (_, _, _, t1) ->
            PatriciaHashSet.Last t1

    //
    static member Count (set : PatriciaHashSet<'T>) : int =
        match set with
        | Empty -> 0
        | Lf (_, listSet) ->
            List.length listSet
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
                    count <- count + (uint32 <| List.length listSet)
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf (_, listSet1), Lf (_, listSet2)) ->
                    count <-
                        count
                        + (uint32 <| List.length listSet1)
                        + (uint32 <| List.length listSet2)
                    
                | Br (_, _, Lf (_, listSet), child)
                | Br (_, _, child, Lf (_, listSet)) ->
                    count <- count + (uint32 <| List.length listSet)
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
                match ListSet.remove value listSet with
                | [] ->
                    Empty
                | result ->
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
                        // Only create a new tree when the value was actually removed
                        // (i.e., the tree was modified).
                        if left === t0 then set
                        else Br (p, m, left, t1)
                else
                    match PatriciaHashSet.Remove (valueHash, value, t1) with
                    | Empty -> t0
                    | right ->
                        // Only create a new tree when the value was actually removed
                        // (i.e., the tree was modified).
                        if right === t1 then set
                        else Br (p, m, t0, right)
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
            Lf (valueHash, [value])
        | Lf (j, listSet) ->
            if j = valueHash then
                let result = ListSet.add value listSet

                // OPTIMIZATION : If the result is the same as the input,
                // return the original set instead since it wasn't modified.
                if result === listSet then set
                else
                    Lf (j, result)
            else
                PatriciaHashSet.Join (valueHash, Lf (valueHash, [value]), j, set)
        | Br (p, m, t0, t1) ->
            if matchPrefix (valueHash, p, m) then
                if zeroBit (valueHash, m) then
                    let left = PatriciaHashSet.Add (valueHash, value, t0)

                    // Only create a new tree when the value was actually added
                    // (i.e., the tree was modified).
                    if left === t0 then set
                    else Br (p, m, left, t1)
                else
                    let right = PatriciaHashSet.Add (valueHash, value, t1)

                    // Only create a new tree when the value was actually added
                    // (i.e., the tree was modified).
                    if right === t1 then set
                    else Br (p, m, t0, right)
            else
                PatriciaHashSet.Join (valueHash, Lf (valueHash, [value]), p, set)

    /// Computes the union of two PatriciaHashSets.
    static member Union (s, t) : PatriciaHashSet<'T> =
        // If the sets are identical, return immediately.
        if s === t then s else
        match s, t with
        | Br (p, m, s0, s1), Br (q, n, t0, t1) ->
            if m = n then
                if p = q then
                    // The trees have the same prefix. Merge the subtrees.
                    let left = PatriciaHashSet.Union (s0, t0)
                    let right = PatriciaHashSet.Union (s1, t1)
                    
                    // Only create a new tree if some values were actually added
                    // (i.e., the tree was modified).
                    if left === s0 && right === s1 then s
                    elif left === t0 && right === t1 then t
                    else Br (p, m, left, right)
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
                        
                        // Only create a new tree when the subtree is actually modified.
                        if left === s0 then s
                        else Br (p, m, left, s1)
                    else
                        let right = PatriciaHashSet.Union (s1, t)
                        
                        // Only create a new tree when the subtree is actually modified.
                        if right === s1 then s
                        else Br (p, m, s0, right)
                else
                    // The prefixes disagree.
                    PatriciaHashSet.Join (p, s, q, t)

            else
                if matchPrefix (p, q, n) then
                    // p contains q. Merge s with a subtree of t.
                    if zeroBit (p, n) then
                        let left = PatriciaHashSet.Union (s, t0)
                        
                        // Only create a new tree when the subtree is actually modified.
                        if left === t0 then t
                        else Br (q, n, left, t1)
                    else
                        let right = PatriciaHashSet.Union (s, t1)
                        
                        // Only create a new tree when the subtree is actually modified.
                        if right === t1 then t
                        else Br (q, n, t0, right)
                else
                    // The prefixes disagree.
                    PatriciaHashSet.Join (p, s, q, t)

        | Br (p, m, s0, s1), Lf (k, _) ->
            if matchPrefix (k, p, m) then
                if zeroBit (k, m) then
                    let left = PatriciaHashSet.Union (s0, t)
                    
                    // Only create a new tree when the subtree is actually modified.
                    if left === s0 then s
                    else Br (p, m, left, s1)
                else
                    let right = PatriciaHashSet.Union (s1, t)
                    
                    // Only create a new tree when the subtree is actually modified.
                    if right === s1 then s
                    else Br (p, m, s0, right)
            else
                PatriciaHashSet.Join (k, t, p, s)

        | Lf (k, _), Br (q, n, t0, t1) ->
            if matchPrefix (k, q, n) then
                if zeroBit (k, n) then
                    let left = PatriciaHashSet.Union (s, t0)

                    // Only create a new tree when the subtree is actually modified.
                    if left === t0 then t
                    else Br (q, n, left, t1)
                else
                    let right = PatriciaHashSet.Union (s, t1)

                    // Only create a new tree when the subtree is actually modified.
                    if right === t1 then t
                    else Br (q, n, t0, right)
            else
                PatriciaHashSet.Join (k, s, q, t)

        | Lf (j, listSet1), Lf (k, listSet2) ->
            if j = k then
                // Combine the sets into a single set.
                let result = ListSet.union listSet1 listSet2
                
                // Only create a new tree if we can't re-use one of the input trees.
                if result === listSet1 then s
                elif result === listSet2 then t
                else
                    Lf (j, result)
            else
                PatriciaHashSet.Join (j, s, k, t)

        | _, Empty -> s
        | Empty, _ -> t

    /// Compute the intersection of two PatriciaHashSets.
    static member Intersect (s, t) : PatriciaHashSet<'T> =
        // If the sets are identical, return immediately.
        if s === t then s else
        match s, t with
        | Br (p, m, s0, s1), Br (q, n, t0, t1) ->
            if m = n then
                if p <> q then Empty
                else
                    let left = PatriciaHashSet.Intersect (s0, t0)
                    let right = PatriciaHashSet.Intersect (s1, t1)
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

        | Br (p, m, s0, s1), Lf (k, _) ->
            if matchPrefix (k, p, m) then
                let s' = if zeroBit (k, m) then s0 else s1
                PatriciaHashSet.Intersect (s', t)
            else Empty

        | Lf (j, _), Br (q, n, t0, t1) ->
            if matchPrefix (j, q, n) then
                let t' = if zeroBit (j, n) then t0 else t1
                PatriciaHashSet.Intersect (s, t')
            else Empty

        | Lf (j, listSet1), Lf (k, listSet2) ->
            if j <> k then Empty
            else
                match ListSet.intersect listSet1 listSet2 with
                | [] -> Empty
                | result ->
                    // Only create a new tree if we can't re-use one of the input trees.
                    if result === listSet1 then s
                    elif result === listSet2 then t
                    else
                        Lf (j, result)

        | Empty, _
        | _, Empty ->
            Empty

    /// Compute the difference of two PatriciaHashSets.
    static member Difference (s, t) : PatriciaHashSet<'T> =
        // If the sets are identical, return immediately.
        if s === t then Empty else
        match s, t with
        | Br (p, m, s0, s1), Br (q, n, t0, t1) ->
            if m = n then
                if p <> q then s
                else
                    let left = PatriciaHashSet.Difference (s0, t0)
                    let right = PatriciaHashSet.Difference (s1, t1)
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
                        match PatriciaHashSet.Difference (s0, t) with
                        | Empty -> s1
                        | left ->
                            // Only create a new tree some values were actually removed
                            // (i.e., the tree was modified).
                            if left === s0 then s
                            else Br (p, m, left, s1)
                    else
                        match PatriciaHashSet.Difference (s1, t) with
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
                        PatriciaHashSet.Difference (s, t0)
                    else
                        PatriciaHashSet.Difference (s, t1)
                else s

        | Br (p, m, s0, s1), Lf (k, _) ->
            if matchPrefix (k, p, m) then
                if zeroBit (k, m) then
                    match PatriciaHashSet.Difference (s0, t) with
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
                    match PatriciaHashSet.Difference (s1, t) with
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

        | Lf (j, _), Br (q, n, t0, t1) ->
            if matchPrefix (j, q, n) then
                let t' = if zeroBit (j, n) then t0 else t1
                PatriciaHashSet.Difference (s, t')
            else s
        
        | Lf (j, listSet1), Lf (k, listSet2) ->
            if j <> k then s
            else
                match ListSet.difference listSet1 listSet2 with
                | [] -> Empty
                | result ->
                    // Only create a new tree if we can't re-use one of the input trees.
                    if result === listSet1 then s
                    elif result === listSet2 then t
                    else
                        Lf (j, result)
        
        | _, Empty -> s
        | Empty, _ ->
            Empty

    /// Computes the containment ordering for two sets (i.e., determines if one set includes the other).
    // Return values:
    //  -1 : All values in 't1' are in 't2', and at least one value of 't2' is not in 't1'.
    //   0 : The sets contain _exactly_ the same values.
    //   1 : The sets are disjoint, i.e., 't1' contains at least one value which is not in 't2'.
    static member private SubsetCompare (t1 : PatriciaHashSet<'T>, t2 : PatriciaHashSet<'T>) : int =
        match t1, t2 with
        | Br (p1, m1, l1, r1), Br (p2, m2, l2, r2) ->
            if shorter (m1, m2) then 1
            elif shorter (m2, m1) then
                if not <| matchPrefix (p1, p2, m2) then 1
                elif zeroBit (p1, m2) then
                    match PatriciaHashSet.SubsetCompare (t1, l2) with 1 -> 1 | _ -> -1
                else
                    match PatriciaHashSet.SubsetCompare (t1, r2) with 1 -> 1 | _ -> -1
            elif p1 = p2 then
                let left = PatriciaHashSet.SubsetCompare (l1, l2)
                let right = PatriciaHashSet.SubsetCompare (r1, r2)
                match left, right with
                | 1, _
                | _, 1 -> 1
                | 0, 0 -> 0
                | _ -> -1
            else
                // The maps are disjoint.
                1

        | Lf (k, _), Br (p, m, l, r) ->
            if not <| matchPrefix (k, p, m) then 1
            elif zeroBit (k, m) then
                match PatriciaHashSet.SubsetCompare (t1, l) with 1 -> 1 | _ -> -1
            else
                match PatriciaHashSet.SubsetCompare (t1, r) with 1 -> 1 | _ -> -1

        | Lf (j, listSet1), Lf (k, listSet2) ->
            if j <> k then 1
            else
                // Compare the two sets.
                ListSet.subsetCompare listSet1 listSet2

        | Br (_,_,_,_), (Empty | Lf (_,_))
        | Lf _, Empty ->
            // The maps are disjoint.
            1

        | Empty, Empty -> 0
        | Empty, _ -> -1

    /// Is 'set1' a proper subset of 'set2'?
    /// IsProperSubset (set1, set2) returns true if all keys in set1 are in set2,
    /// and at least one element in set2 is not in set1.
    static member IsProperSubsetOf (set1 : PatriciaHashSet<'T>, set2 : PatriciaHashSet<'T>) : bool =
        match PatriciaHashSet.SubsetCompare (set1, set2) with
        | -1 -> true
        | _ -> false

    /// Is 'set1' a subset of 'set2'?
    /// IsSubset (set1, set2) returns true if all keys in set1 are in set2.
    static member IsSubsetOf (set1 : PatriciaHashSet<'T>, set2 : PatriciaHashSet<'T>) : bool =
        match PatriciaHashSet.SubsetCompare (set1, set2) with
        | -1 | 0 -> true
        | _ -> false

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
            List.iter action listSet
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
                    List.iter action listSet
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf (_, listSet1), Lf (_, listSet2)) ->
                    List.iter action listSet1
                    List.iter action listSet2
                    
                | Br (_, _, Lf (_, listSet), right) ->
                    // Only handle the case where the left child is a leaf
                    // -- otherwise the traversal order would be altered.
                    List.iter action listSet
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
            List.iter action <| List.rev listSet
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
                    List.iter action <| List.rev listSet
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf (_, listSet1), Lf (_, listSet2)) ->
                    List.iter action <| List.rev listSet1
                    List.iter action <| List.rev listSet2
                    
                | Br (_, _, left, Lf (_, listSet)) ->
                    // Only handle the case where the right child is a leaf
                    // -- otherwise the traversal order would be altered.
                    List.iter action <| List.rev listSet
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
            List.fold folder state listSet
        | Br (_,_,_,_) ->
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
                    state <- List.fold folder state listSet
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf (_, listSet1), Lf (_, listSet2)) ->
                    state <- List.fold folder state listSet1
                    state <- List.fold folder state listSet2
                    
                | Br (_, _, Lf (_, listSet), right) ->
                    // Only handle the case where the left child is a leaf
                    // -- otherwise the traversal order would be altered.
                    state <- List.fold folder state listSet
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
            List.foldBack folder listSet state
        | Br (_,_,_,_) ->
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
                    state <- List.foldBack folder listSet state
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf (_, listSet1), Lf (_, listSet2)) ->
                    state <- List.foldBack folder listSet1 state
                    state <- List.foldBack folder listSet2 state
                    
                | Br (_, _, left, Lf (_, listSet)) ->
                    // Only handle the case where the right child is a leaf
                    // -- otherwise the traversal order would be altered.
                    state <- List.foldBack folder listSet state
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
            List.tryFind predicate listSet
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
                    matchingKey <- List.tryFind predicate listSet
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf (_, listSet1), Lf (_, listSet2)) ->
                    matchingKey <- List.tryFind predicate listSet1
                    if Option.isNone matchingKey then
                        matchingKey <- List.tryFind predicate listSet2
                    
                | Br (_, _, Lf (_, listSet), right) ->
                    // Only handle the case where the left child is a leaf
                    // -- otherwise the traversal order would be altered.
                    matchingKey <- List.tryFind predicate listSet
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
            List.tryPick picker listSet
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
                    pickedValue <- List.tryPick picker listSet
                    
                (* OPTIMIZATION :   When one or both children of this node are leaves,
                                    we handle them directly since it's a little faster. *)
                | Br (_, _, Lf (_, listSet1), Lf (_, listSet2)) ->
                    pickedValue <-
                        match List.tryPick picker listSet1 with
                        | (Some _) as value ->
                            value
                        | None ->
                            List.tryPick picker listSet2
                    
                | Br (_, _, Lf (_, listSet), right) ->
                    // Only handle the case where the left child is a leaf
                    // -- otherwise the traversal order would be altered.
                    pickedValue <- List.tryPick picker listSet
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
            yield! List.toSeq listSet
        
        (* OPTIMIZATION :   When one or both children of this node are leaves,
                            we handle them directly since it's a little faster. *)
        | Br (_, _, Lf (_, listSet1), Lf (_, listSet2)) ->
            yield! List.toSeq listSet1
            yield! List.toSeq listSet2

        | Br (_, _, Lf (_, listSet), right) ->
            // Only handle the case where the left child is a leaf
            // -- otherwise the traversal order would be altered.
            yield! List.toSeq listSet
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

    /// The first element in the set.
    /// If the element type has suitable hash and comparison functions for the set
    /// to be ordered, then this will be the minimum element of the set.
#if FX_NO_DEBUG_DISPLAYS
#else
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
#endif
    member __.First
        with get () =
            PatriciaHashSet.First trie

    /// The last element in the set.
    /// If the element type has suitable hash and comparison functions for the set
    /// to be ordered, then this will be the maximum element of the set.
#if FX_NO_DEBUG_DISPLAYS
#else
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
#endif
    member __.Last
        with get () =
            PatriciaHashSet.Last trie

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

    /// Returns a new HashSet created by merging the two specified HashSets.
    member this.Union (otherSet : HashSet<'T>) : HashSet<'T> =
        // If this set's trie is the same as the other set's trie, we can return immediately.
        if trie === otherSet.Trie then this
        else
            // If the result is the same (physical equality) to one of the inputs,
            // return that input instead of creating a new HashSet.
            let trie' = PatriciaHashSet.Union (trie, otherSet.Trie)
            if trie === trie' then this
            elif otherSet.Trie === trie' then otherSet
            else HashSet (trie')

    /// Returns the intersection of two HashSets.
    member this.Intersect (otherSet : HashSet<'T>) : HashSet<'T> =
        // If this set's trie is the same as the other set's trie, we can return immediately.
        if trie === otherSet.Trie then this
        else
            // If the result is the same (physical equality) to one of the inputs,
            // return that input instead of creating a new HashSet.
            let trie' = PatriciaHashSet.Intersect (trie, otherSet.Trie)
            if trie === trie' then this
            elif otherSet.Trie === trie' then otherSet
            else HashSet (trie')

    /// Returns a new HashSet created by removing the second HashSet from the first.
    member this.Difference (otherSet : HashSet<'T>) : HashSet<'T> =
        // If this set's trie is the same as the other set's trie, we can return immediately.
        if trie === otherSet.Trie then HashSet.Empty
        else
            // If the result is the same (physical equality) to one of the inputs,
            // return that input instead of creating a new HashSet.
            let trie' = PatriciaHashSet.Difference (trie, otherSet.Trie)
            if trie === trie' then this
            elif otherSet.Trie === trie' then otherSet
            else HashSet (trie')

    /// Computes the union of a sequence of HashSets.
    static member internal UnionMany (sets : seq<HashSet<'T>>) : HashSet<'T> =
        // Preconditions
        checkNonNull "sets" sets

        let result =
            (PatriciaHashSet.Empty, sets)
            ||> Seq.fold (fun combinedSetTree set ->
                PatriciaHashSet.Union (combinedSetTree, set.Trie))

        // If the resulting trie is empty, return the empty HashSet instance
        // instead of creating a new one to allow for better structure sharing.
        match result with
        | Empty ->
            HashSet.Empty
        | _ ->
            HashSet (result)

    /// Computes the intersection of a sequence of HashSets.
    static member internal IntersectMany (sets : seq<HashSet<'T>>) : HashSet<'T> =
        // Preconditions
        checkNonNull "sets" sets

        // For compatibility with Set, don't check for an empty sequence --
        // just allow Seq.reduce to raise an exception.
        Seq.reduce (fun s1 s2 -> s1.Intersect s2) sets

    /// Determines if this set is a subset of the given set.
    // IsSubsetOf (otherSet) returns true if all values in this set are in 'otherSet'.
    member __.IsSubsetOf (otherSet : HashSet<'T>) : bool =
        PatriciaHashSet.IsSubsetOf (trie, otherSet.Trie)

    /// Determines if set1 is a proper subset of set2.
    // IsProperSubsetOf (otherSet) returns true if all values in this set are in 'otherSet',
    // and 'otherSet' contains at least one value which is not in this set.
    member __.IsProperSubsetOf (otherSet : HashSet<'T>) : bool =
        PatriciaHashSet.IsProperSubsetOf (trie, otherSet.Trie)

    //
    member __.IsSupersetOf (otherSet : HashSet<'T>) : bool =
        PatriciaHashSet.IsSubsetOf (otherSet.Trie, trie)

    //
    member __.IsProperSupersetOf (otherSet : HashSet<'T>) : bool =
        PatriciaHashSet.IsProperSubsetOf (otherSet.Trie, trie)

    /// The HashSet containing the given binding.
    static member Singleton (value : 'T) : HashSet<'T> =
        let valueHash = uint32 <| hash value
        HashSet (
            Lf (valueHash, [value]))

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
        PatriciaHashSet.FoldBack (flip List.cons, [], trie)

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

    /// Computes the union of two sets.
    static member op_Addition (set1 : HashSet<'T>, set2 : HashSet<'T>) : HashSet<'T> =
        set1.Union set2

    /// Computes the difference of two sets.
    static member op_Subtraction (set1 : HashSet<'T>, set2 : HashSet<'T>) : HashSet<'T> =
        set1.Difference set2

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

    /// Tests if an element is in the domain of the set.
    [<CompiledName("Contains")>]
    let inline contains (value : 'T) (set : HashSet<'T>) : bool =
        // Preconditions
        checkNonNull "set" set

        set.Contains value

    (* maxElement, minElement *)

    /// Computes the union of the two sets.
    [<CompiledName("Union")>]
    let inline union (set1 : HashSet<'T>) (set2 : HashSet<'T>) : HashSet<'T> =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2
        
        set1.Union set2

    /// Computes the union of a sequence of sets.
    [<CompiledName("UnionMany")>]
    let unionMany sets =
        // Preconditions checked by the target method.
        HashSet.UnionMany sets

    /// Computes the intersection of the two sets.
    [<CompiledName("Intersect")>]
    let inline intersect (set1 : HashSet<'T>) (set2 : HashSet<'T>) : HashSet<'T> =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2
        
        set1.Intersect set2

    /// Computes the intersection of a sequence of sets.
    [<CompiledName("IntersectMany")>]
    let intersectMany sets =
        // Preconditions checked by the target method.
        HashSet.IntersectMany sets

    /// Returns a new set with the elements of the second set removed from the first.
    [<CompiledName("Difference")>]
    let inline difference (set1 : HashSet<'T>) (set2 : HashSet<'T>) : HashSet<'T> =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2
        
        set1.Difference set2

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the first set are in the second.
    /// </summary>
    [<CompiledName("IsSubset")>]
    let inline isSubset (set1 : HashSet<'T>) (set2 : HashSet<'T>) : bool =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.IsSubsetOf set2

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the first set are in the second,
    /// and at least one element of the second is not in the first.
    /// </summary>
    [<CompiledName("IsProperSubset")>]
    let inline isProperSubset (set1 : HashSet<'T>) (set2 : HashSet<'T>) : bool =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2
        
        set1.IsProperSubsetOf set2

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the second set are in the first.
    /// </summary>
    [<CompiledName("IsSuperset")>]
    let inline isSuperset (set1 : HashSet<'T>) (set2 : HashSet<'T>) : bool =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2
        
        set1.IsSupersetOf set2

    /// <summary>
    /// Evaluates to &quot;true&quot; if all elements of the second set are in the first,
    /// and at least one element of the first is not in the second.
    /// </summary>
    [<CompiledName("IsProperSuperset")>]
    let inline isProperSuperset (set1 : HashSet<'T>) (set2 : HashSet<'T>) : bool =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        set1.IsProperSupersetOf set2

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

    /// <summary>
    /// Builds a new set containing only the bindings for which the given
    /// predicate returns &quot;true&quot;.
    /// </summary>
    [<CompiledName("Filter")>]
    let inline filter (predicate : 'T -> bool) (set : HashSet<'T>) : HashSet<'T> =
        // Preconditions
        checkNonNull "set" set
        
        set.Filter predicate

    /// Builds a new collection whose elements are the results of applying the given function
    /// to each of the elements of the collection. The key passed to the function indicates
    /// the key of the element being transformed.
    [<CompiledName("Map")>]
    let inline map (mapping : 'T -> 'U) (set : HashSet<'T>) : HashSet<'U> =
        // Preconditions
        checkNonNull "set" set
        
        set.Map mapping

    /// Splits the set into two sets containing the bindings for which the given
    /// predicate returns true and false, respectively.
    [<CompiledName("Partition")>]
    let inline partition (predicate : 'T -> bool) (set : HashSet<'T>) : HashSet<'T> * HashSet<'T> =
        // Preconditions
        checkNonNull "set" set
        
        set.Partition predicate

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

    /// Splits the set into two sets by applying the given partitioning function
    /// to each binding in the set.
    [<CompiledName("MapPartition")>]
    let inline mapPartition (partitioner : 'T -> Choice<'U, 'V>) (set : HashSet<'T>) : HashSet<'U> * HashSet<'V> =
        // Preconditions
        checkNonNull "set" set

        set.MapPartition partitioner
