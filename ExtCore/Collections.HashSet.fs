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
open BitOps32


/// SetTree which serves as the internal representation of the Set type.
[<NoEquality; NoComparison>]
[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type internal SetTree<'T when 'T : comparison> =
    /// Empty tree.
    | Empty
    /// Node.
    // Left-Child, Right-Child, Value, Height
    | Node of SetTree<'T> * SetTree<'T> * 'T * uint32

    //
    static member private CompareStacks (comparer : IComparer<'T>, l1 : SetTree<'T> list, l2 : SetTree<'T> list) : int =
        match l1, l2 with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        
        // OPTIMIZATION : If two trees are identical, there's no need to compare them.
        | t1 :: l1, t2 :: l2
            when System.Object.ReferenceEquals (t1, t2) ->
            // Continue comparing the lists.
            SetTree.CompareStacks (comparer, l1, l2)
        
        | (Empty :: t1), (Empty :: t2) ->
            SetTree.CompareStacks (comparer, t1, t2)
        | (Node (Empty, Empty, n1k, _) :: t1), (Node (Empty, Empty, n2k, _) :: t2) ->
            match comparer.Compare (n1k, n2k) with
            | 0 ->
                SetTree.CompareStacks (comparer, t1, t2)
            | c -> c

        | (Node (Empty, Empty, n1k, _) :: t1), (Node (Empty, n2r, n2k, _) :: t2) ->
            match comparer.Compare (n1k, n2k) with
            | 0 ->
                SetTree.CompareStacks (comparer, Empty :: t1, n2r :: t2)
            | c -> c

        | (Node (Empty, n1r, n1k, _) :: t1), (Node (Empty, Empty, n2k, _) :: t2) ->
            match comparer.Compare (n1k, n2k) with
            | 0 ->
                SetTree.CompareStacks (comparer, n1r :: t1, Empty :: t2)
            | c -> c

        | (Node (Empty, n1r, n1k, _) :: t1), (Node (Empty, n2r, n2k, _) :: t2) ->
            match comparer.Compare (n1k, n2k) with
            | 0 ->
                SetTree.CompareStacks (comparer, n1r :: t1, n2r :: t2)
            | c -> c

        | ((Node (Empty, Empty, n1k, _) :: t1) as l1), _ ->
            SetTree.CompareStacks (comparer, Empty :: l1, l2)
        
        | (Node (n1l, n1r, n1k, _) :: t1), _ ->
            SetTree.CompareStacks (comparer, n1l :: Node (Empty, n1r, n1k, 0u) :: t1, l2)
        
        | _, ((Node (Empty, Empty, n2k, _) :: t2) as l2) ->
            SetTree.CompareStacks (comparer, l1, Empty :: l2)
        
        | _, (Node (n2l, n2r, n2k, _) :: t2) ->
            SetTree.CompareStacks (comparer, l1, n2l :: Node (Empty, n2r, n2k, 0u) :: t2)
                
    //
    static member Compare (comparer : IComparer<'T>, s1 : SetTree<'T>, s2 : SetTree<'T>) : int =
        // OPTIMIZATION : If two trees are identical, there's no need to compare them.
        if System.Object.ReferenceEquals (s1, s2) then 0
        else
            match s1, s2 with
            | Empty, Empty -> 0
            | Empty, _ -> -1
            | _, Empty -> 1
            | _ ->
                SetTree<'T>.CompareStacks (comparer, [s1], [s2])

    /// Computes the height of a SetTree (rather than returning the height value stored in it's root).
    //[<Pure>]
    static member private ComputeHeight (tree : SetTree<'T>) =
        match tree with
        | Empty -> 0u
        | Node (l, r, _, _) ->
            1u + max (SetTree.ComputeHeight l) (SetTree.ComputeHeight r)
        
    /// Determines if a SetTree is correctly formed, i.e., it respects the AVL balancing rules.
    //[<Pure; ContractInvariantMethod>]
    static member private AvlInvariant (tree : SetTree<'T>) =
        match tree with
        | Empty -> true
        | Node (l, r, _, h) ->
            let height_l = SetTree.ComputeHeight l
            let height_r = SetTree.ComputeHeight r
            height_l = height_r
            || (height_l = (1u + height_r) || height_r = (1u + height_l))
            && h = ((max height_l height_r) + 1u)
            && SetTree.AvlInvariant l
            && SetTree.AvlInvariant r

    /// Returns the height of a SetTree.
    //[<Pure>]
    static member (*inline*) Height (tree : SetTree<'T>) =
        match tree with
        | Empty -> 0u
        | Node (_,_,_,h) -> h

    /// Returns the absolute difference in heights between two SetTrees.
    //[<Pure>]
    static member private HeightDiff (t1, t2 : SetTree<'T>) =
        (max (SetTree.Height t1) (SetTree.Height t2)) - (min (SetTree.Height t1) (SetTree.Height t2))

    /// Determines if a SetTree is empty.
    //[<Pure>]
    static member (*inline*) IsEmptyTree (tree : SetTree<'T>) =
        match tree with
        | Empty -> true
        | Node (_,_,_,_) -> false

    /// Gets the maximum (greatest) value stored in the SetTree.
    static member MaxElement (tree : SetTree<'T>) =
        match tree with
        | Empty ->
            invalidArg "tree" "The tree is empty."
        | Node (_, Empty, n, _) ->
            n
        | Node (_, right, _, _) ->
            SetTree.MaxElement right

    /// Gets the minimum (least) value stored in the SetTree.
    static member MinElement (tree : SetTree<'T>) =
        match tree with
        | Empty ->
            invalidArg "tree" "The tree is empty."
        | Node (Empty, _, n, _) ->
            n
        | Node (left, _, _, _) ->
            SetTree.MinElement left

    /// Determines if a SetTree contains a specified value.
    //[<Pure>]
    static member Contains (comparer : IComparer<'T>, tree : SetTree<'T>, value : 'T) =
        match tree with
        | Empty ->
            false
        | Node (l, r, n, _) ->
            let comparison = comparer.Compare (value, n)
            if comparison = 0 then      // value = n
                true
            elif comparison < 0 then    // value < n
                SetTree.Contains (comparer, l, value)
            else                        // value > n
                SetTree.Contains (comparer, r, value)

    /// Creates a SetTree whose root node holds the specified value
    /// and the specified left and right subtrees.
    static member inline Create (value, l, r : SetTree<'T>) =
        // Preconditions
        // TODO : Add assertions for debugging/testing
        Node (l, r, value, (max (SetTree.Height l) (SetTree.Height r)) + 1u)

    /// Creates a SetTree containing the specified value.
    static member Singleton value : SetTree<'T> =
        SetTree.Create (value, Empty, Empty)

    //
    static member private mkt_bal_l (n, l, r : SetTree<'T>) =
        // Preconditions
        // TODO : Add assertions for debugging/testing

        if SetTree.Height l = SetTree.Height r + 2u then
            match l with
            | Empty ->
                failwith "mkt_bal_l"
            | Node (ll, lr, ln, _) ->
                if SetTree.Height ll < SetTree.Height lr then
                    match lr with
                    | Empty ->
                        failwith "mkt_bal_l"
                    | Node (lrl, lrr, lrn, _) ->
                        SetTree.Create (lrn, SetTree.Create (ln, ll, lrl), SetTree.Create (n, lrr, r))
                else
                    SetTree.Create (ln, ll, SetTree.Create (n, lr, r))
        else
            SetTree.Create (n, l, r)

    //
    static member private mkt_bal_r (n, l, r : SetTree<'T>) =
        // Preconditions
        // TODO : Add assertions for debugging/testing

        if SetTree.Height r = SetTree.Height l + 2u then
            match r with
            | Empty ->
                failwith "mkt_bal_r"
            | Node (rl, rr, rn, _) ->
                if SetTree.Height rr < SetTree.Height rl then
                    match rl with
                    | Empty ->
                        failwith "mkt_bal_r"
                    | Node (rll, rlr, rln, _) ->
                        SetTree.Create (rln, SetTree.Create (n, l, rll), SetTree.Create (rn, rlr, rr))
                else
                    SetTree.Create (rn, SetTree.Create (n, l, rl), rr)
        else
            SetTree.Create (n, l, r)

(* NOTE :   The DeleteMin, DeleteMax, DeleteRoot, TryDeleteMin, and TryDeleteMax methods use the
            'private' modifier here because they're not implemented/exposed by the standard F# Set type,
            but some of them are used by this implementation.
            If you are using a custom FSharp.Core implementation and would like to expose these
            functions for use in your own code, just remove the 'private' modifiers. *)

    /// Removes the minimum (least) value from an SetTree,
    /// returning the value along with the updated tree.
    static member private DeleteMin (tree : SetTree<'T>) =
        // Preconditions
        // TODO : Add assertions for debugging/testing.

        match tree with
        | Empty ->
            invalidArg "tree" "Cannot delete the minimum value from an empty tree."
        | Node (l, Empty, n, _) ->
            n, l
        | Node (left, r, n, _) ->
            let na, l = SetTree.DeleteMin left
            na, SetTree.mkt_bal_r (n, l, r)

    /// Removes the maximum (greatest) value from an SetTree,
    /// returning the value along with the updated tree.
    static member private DeleteMax (tree : SetTree<'T>) =
        // Preconditions
        // TODO : Add assertions for debugging/testing.

        match tree with
        | Empty ->
            invalidArg "tree" "Cannot delete the maximum value from an empty tree."
        | Node (l, Empty, n, _) ->
            n, l
        | Node (l, right, n, _) ->
            let na, r = SetTree.DeleteMax right
            na, SetTree.mkt_bal_l (n, l, r)

    /// Removes the root (median) value from an SetTree,
    /// returning the value along with the updated tree.
    static member private DeleteRoot (tree : SetTree<'T>) =
        // Preconditions
        // TODO : Add assertions for debugging/testing.

        match tree with
        | Empty ->
            invalidArg "tree" "Cannot delete the root of an empty tree."
        | Node (Empty, r, _, _) -> r
        | Node (left, Empty, _, _) ->
            left
        | Node (left, r, _, _) ->
            let root, l = SetTree.DeleteMax left
            SetTree.mkt_bal_r (root, l, r)

    /// Removes the minimum (least) value from a SetTree,
    /// returning the value along with the updated tree.
    /// No exception is thrown if the tree is empty.
    static member private TryDeleteMin (tree : SetTree<'T>) =
        // Preconditions
        // TODO : Add assertions for debugging/testing.

        match tree with
        | Empty ->
            None, tree
        | Node (l, Empty, n, _) ->
            Some n, l
        | Node (left, r, n, _) ->
            let na, l = SetTree.TryDeleteMin left
            match na with
            | None ->
                na, l
            | Some _ ->
                na, SetTree.mkt_bal_r (n, l, r)

    /// Removes the maximum (greatest) value from a SetTree,
    /// returning the value along with the updated tree.
    /// No exception is thrown if the tree is empty.
    static member private TryDeleteMax (tree : SetTree<'T>) =
        // Preconditions
        // TODO : Add assertions for debugging/testing.

        match tree with
        | Empty ->
            None, tree
        | Node (l, Empty, n, _) ->
            Some n, l
        | Node (l, right, n, _) ->
            let na, r = SetTree.TryDeleteMax right
            match na with
            | None ->
                na, l
            | Some _ ->
                na, SetTree.mkt_bal_l (n, l, r)

    /// Removes the specified value from the tree.
    /// If the tree doesn't contain the value, no exception is thrown;
    /// the tree will be returned without modification.
    static member Delete (comparer : IComparer<'T>, tree : SetTree<'T>, value : 'T) =
        // Preconditions
        // TODO : Add assertions for debugging/testing.

        match tree with
        | Empty ->
            Empty
        | Node (l, r, n, _) ->
            let comparison = comparer.Compare (value, n)
            if comparison = 0 then
                // x = n
                SetTree.DeleteRoot tree
            elif comparison < 0 then
                // x < n
                let l' = SetTree.Delete (comparer, l, value)

                // Only rebalance the tree if an element was actually deleted.
                if System.Object.ReferenceEquals (l', l) then tree
                else SetTree.mkt_bal_r (n, l', r)
            else
                // x > n
                let r' = SetTree.Delete (comparer, r, value)
                
                // Only rebalance the tree if an element was actually deleted.
                if System.Object.ReferenceEquals (r', r) then tree
                else SetTree.mkt_bal_l (n, l, r')

    /// Adds a value to a SetTree.
    /// If the tree already contains the value, no exception is thrown;
    /// the tree will be returned without modification.
    static member Insert (comparer : IComparer<'T>, tree : SetTree<'T>, value : 'T) =
        // Preconditions
        // TODO : Add assertions for debugging/testing.

        match tree with
        | Empty ->
            Node (Empty, Empty, value, 1u)
        | Node (l, r, n, _) ->
            let comparison = comparer.Compare (value, n)
            if comparison = 0 then
                // x = n
                tree
            elif comparison < 0 then
                // x < n
                let l' = SetTree.Insert (comparer, l, value)

                // Only rebalance the tree if an element was actually inserted.
                if System.Object.ReferenceEquals (l', l) then tree
                else SetTree.mkt_bal_l (n, l', r)
            else
                // x > n
                let r' = SetTree.Insert (comparer, r, value)
                
                // Only rebalance the tree if an element was actually inserted.
                if System.Object.ReferenceEquals (r', r) then tree
                else SetTree.mkt_bal_r (n, l, r')

    /// Counts the number of elements in the tree.
    static member Count (tree : SetTree<'T>) =
        match tree with
        | Empty -> 0u
        | Node (Empty, Empty, _, _) -> 1u
        | Node (l, r, _, _) ->
            // Count the number of elements in the left and right children.
            let leftCount = SetTree<_>.Count l
            let rightCount = SetTree<_>.Count r

            // Return the sum of the counts of the children, plus one for this node.
            leftCount + rightCount + 1u

    //
    static member Iterate (action : 'T -> unit) (tree : SetTree<'T>) : unit =
        match tree with
        | Empty -> ()
        | Node (Empty, Empty, x, _) ->
            // Invoke the action with this single element.
            action x
        | Node (l, r, x, _) ->
            // Iterate over the elements in the left subtree.
            SetTree<_>.Iterate action l

            // Apply the action to the element stored in this node.
            action x

            // Iterate over the elements in the right subtree.
            SetTree<_>.Iterate action r

    //
    static member IterateBack (action : 'T -> unit) (tree : SetTree<'T>) : unit =
        match tree with
        | Empty -> ()
        | Node (Empty, Empty, x, _) ->
            // Invoke the action with this single element.
            action x
        | Node (l, r, x, _) ->
            // Iterate over the elements in the right subtree.
            SetTree<_>.IterateBack action r

            // Apply the action to the element stored in this node.
            action x

            // Iterate over the elements in the left subtree.
            SetTree<_>.IterateBack action l

    /// Applies the given accumulating function to all elements in a SetTree.
    static member Fold (folder : 'State -> 'T -> 'State) (state : 'State) (tree : SetTree<'T>) =
        match tree with
        | Empty -> state
        | Node (Empty, Empty, x, _) ->
            // Invoke the folder function on this single element and return the result.
            folder state x
        | Node (l, r, x, _) ->
            // Fold over the elements in the left subtree.
            let state = SetTree<_>.Fold folder state l

            // Apply the folder function to the element stored in this node.
            let state = folder state x

            // Fold over the elements in the right subtree.
            SetTree<_>.Fold folder state r

    /// Applies the given accumulating function to all elements in a SetTree.
    static member FoldBack (folder : 'T -> 'State -> 'State) (tree : SetTree<'T>) (state : 'State) =
        match tree with
        | Empty -> state
        | Node (Empty, Empty, x, _) ->
            // Invoke the folder function on this single element and return the result.
            folder x state
        | Node (l, r, x, _) ->
            // Fold over the elements in the right subtree.
            let state = SetTree<_>.FoldBack folder r state

            // Apply the folder function to the element stored in this node.
            let state = folder x state

            // Fold over the elements in the left subtree.
            SetTree<_>.FoldBack folder l state

    /// Tests if any element of the collection satisfies the given predicate.
    static member Exists (predicate : 'T -> bool) (tree : SetTree<'T>) : bool =
        match tree with
        | Empty -> false
        | Node (Empty, Empty, x, _) ->
            // Apply the predicate function to this element and return the result.
            predicate x
        | Node (l, r, x, _) ->
            // Try to find a matching element in the left subtree.
            SetTree<_>.Exists predicate l
            // Does the element stored in this node match the predicate?
            || predicate x
            // Try to find a matching element in the right subtree.
            || SetTree<_>.Exists predicate r

    /// Tests if all elements of the collection satisfy the given predicate.
    static member Forall (predicate : 'T -> bool) (tree : SetTree<'T>) : bool =
        match tree with
        | Empty -> true
        | Node (Empty, Empty, x, _) ->
            // Apply the predicate function to this element and return the result.
            predicate x
        | Node (l, r, x, _) ->
            // Try to find a non-matching element in the left subtree.
            SetTree<_>.Forall predicate l
            // Does the element stored in this node match the predicate?
            && predicate x
            // Try to find a non-matching element in the right subtree.
            && SetTree<_>.Forall predicate r

    //
    //[<Pure>]
    static member TryFind (predicate : 'T -> bool) (tree : SetTree<'T>) : 'T option =
        match tree with
        | Empty -> None
        | Node (Empty, Empty, x, _) ->
            // Apply the predicate to this element; if the element matches, return it.
            if predicate x then Some x else None
        | Node (l, r, x, _) ->
            // Try to find a matching element in the left subtree.
            match SetTree<_>.TryFind predicate l with
            | Some _ as result ->
                result
            | None ->
                // Does the element stored in the current node match the predicate?
                if predicate x then Some x
                else
                    // Try to pick a result from the elements of the right subtree.
                    SetTree<_>.TryFind predicate r

    //
    //[<Pure>]
    static member TryPick (picker : 'T -> 'U option) (tree : SetTree<'T>) : 'U option =
        match tree with
        | Empty -> None
        | Node (Empty, Empty, x, _) ->
            // Apply the picker function to this element and return the result.
            picker x
        | Node (l, r, x, _) ->
            // Try to pick a result from the elements of the left subtree.
            match SetTree<_>.TryPick picker l with
            | Some _ as result ->
                result
            | None ->
                // Try to pick a result from the element stored in the current node.
                match picker x with
                | Some _ as result ->
                    result
                | None ->
                    // Try to pick a result from the elements of the right subtree.
                    SetTree<_>.TryPick picker r

    /// Builds a new SetTree from the elements of a sequence.
    static member OfSeq (comparer : IComparer<'T>, sequence : seq<'T>) : SetTree<'T> =
        (Empty, sequence)
        ||> Seq.fold (fun tree el ->
            SetTree.Insert (comparer, tree, el))

    /// Builds a new SetTree from the elements of an list.
    static member OfList (comparer : IComparer<'T>, list : 'T list) : SetTree<'T> =
        (Empty, list)
        ||> List.fold (fun tree el ->
            SetTree.Insert (comparer, tree, el))

    /// Builds a new SetTree from the elements of an array.
    static member OfArray (comparer : IComparer<'T>, array : 'T[]) : SetTree<'T> =
        (Empty, array)
        ||> Array.fold (fun tree el ->
            SetTree.Insert (comparer, tree, el))

    /// Returns a sequence containing the elements stored
    /// in a SetTree, ordered from least to greatest.
    static member ToSeq (tree : SetTree<'T>) =
        seq {
        match tree with
        | Empty -> ()
        | Node (l, r, n, _) ->
            yield! SetTree.ToSeq l
            yield n
            yield! SetTree.ToSeq r
        }

    /// Returns a list containing the elements stored in
    /// a SetTree, ordered from least to greatest.
    static member ToList (tree : SetTree<'T>) =
        (tree, [])
        ||> SetTree.FoldBack (fun el lst ->
            el :: lst)

    /// Returns an array containing the elements stored in
    /// a SetTree, ordered from least to greatest.
    static member ToArray (tree : SetTree<'T>) =
        let elements = ResizeArray ()
        SetTree.Iterate elements.Add tree
        elements.ToArray ()

    //
    // TODO : This could be replaced by 'mkt_bal_l' and 'mkt_bal_r'.
    static member private Rebalance (t1, t2, k) : SetTree<'T> =
        let t1h = SetTree.Height t1
        let t2h = SetTree.Height t2
        if t2h > t1h + 2u then // right is heavier than left
            match t2 with
            | Node (t2l, t2r, t2k, _) ->
                // one of the nodes must have height > height t1 + 1
                if SetTree.Height t2l > t1h + 1u then  // balance left: combination
                    match t2l with
                    | Node (t2ll, t2lr, t2lk, _) ->
                        SetTree.Create (
                            t2lk,
                            SetTree.Create (k, t1, t2ll),
                            SetTree.Create (t2k, t2lr, t2r))
                    | _ -> failwith "rebalance"
                else // rotate left
                    SetTree.Create (
                        t2k,
                        SetTree.Create (k, t1, t2l),
                        t2r)
            | _ -> failwith "rebalance"

        elif t1h > t2h + 2u then // left is heavier than right
            match t1 with
            | Node (t1l, t1r, t1k, _) ->
                // one of the nodes must have height > height t2 + 1
                if SetTree.Height t1r > t2h + 1u then
                    // balance right: combination
                    match t1r with
                    | Node (t1rl, t1rr, t1rk, _) ->
                        SetTree.Create (
                            t1rk,
                            SetTree.Create (t1k, t1l, t1rl),
                            SetTree.Create (k, t1rr, t2))
                    | _ -> failwith "rebalance"
                else
                    SetTree.Create (
                        t1k,
                        t1l,
                        SetTree.Create (k, t1r, t2))
            | _ -> failwith "rebalance"

        else
            SetTree.Create (k, t1, t2)

    //
    static member private Balance (comparer : IComparer<'T>, t1, t2, k) =
        // Given t1 < k < t2 where t1 and t2 are "balanced",
        // return a balanced tree for <t1,k,t2>.
        // Recall: balance means subtrees heights differ by at most "tolerance"
        match t1, t2 with
        // TODO : The first two patterns can be merged to use the same handler.
        | Empty, t2 ->
            // drop t1 = empty
            SetTree.Insert (comparer, t2, k)
        | t1, Empty ->
            // drop t2 = empty
            SetTree.Insert (comparer, t1, k)

        // TODO : The next two patterns can be merged to use the same handler.
        | Node (Empty, Empty, k1, _), t2 ->
            let t' = SetTree.Insert (comparer, t2, k1)
            SetTree.Insert (comparer, t', k)
        | t1, Node (Empty, Empty, k2, _) ->
            let t' = SetTree.Insert (comparer, t1, k2)
            SetTree.Insert (comparer, t', k)

        | Node (t11, t12, k1, h1), Node (t21, t22, k2, h2) ->
            // Have:  (t11 < k1 < t12) < k < (t21 < k2 < t22)
            // Either (a) h1,h2 differ by at most 2 - no rebalance needed.
            //        (b) h1 too small, i.e. h1+2 < h2
            //        (c) h2 too small, i.e. h2+2 < h1
            if   h1+2u < h2 then
                // case: b, h1 too small
                // push t1 into low side of t2, may increase height by 1 so rebalance
                SetTree.Rebalance (SetTree.Balance (comparer, t1, t21, k), t22, k2)
            elif h2+2u < h1 then
                // case: c, h2 too small
                // push t2 into high side of t1, may increase height by 1 so rebalance
                SetTree.Rebalance (t11, SetTree.Balance (comparer, t12, t2, k), k1)
            else
                // case: a, h1 and h2 meet balance requirement
                SetTree.Create (k, t1, t2)

    //
    static member private Split (comparer : IComparer<'T>, t, pivot) : SetTree<'T> * bool * SetTree<'T> =
        // Given a pivot and a set t
        // Return { x in t s.t. x < pivot }, pivot in t? , { x in t s.t. x > pivot }
        match t with
        | Empty  ->
            Empty, false, Empty
        | Node (Empty, Empty, k1, _) ->
            let c = comparer.Compare (k1, pivot)
            if   c < 0 then t    ,false,Empty // singleton under pivot
            elif c = 0 then Empty,true ,Empty // singleton is    pivot
            else            Empty,false,t     // singleton over  pivot
        | Node (t11, t12, k1, _) ->
            let c = comparer.Compare (pivot, k1)
            if   c < 0 then // pivot t1
                let t11Lo, havePivot, t11Hi = SetTree.Split (comparer, t11, pivot)
                t11Lo, havePivot, SetTree.Balance (comparer, t11Hi, t12, k1)
            elif c = 0 then // pivot is k1
                t11,true,t12
            else            // pivot t2
                let t12Lo, havePivot, t12Hi = SetTree.Split (comparer, t12, pivot)
                SetTree.Balance (comparer, t11, t12Lo, k1), havePivot, t12Hi

    /// Computes the union of two SetTrees.
    static member Union (comparer : IComparer<'T>, t1 : SetTree<'T>, t2 : SetTree<'T>) : SetTree<'T> =
        // If the two trees are identical (physical equality), there's no need to do any work.
        if System.Object.ReferenceEquals (t1, t2) then t1 else
        
        // Perf: tried bruteForce for low heights, but nothing significant
        match t1, t2 with
        | Empty, t -> t
        | t, Empty -> t
        | Node (Empty, Empty, k1, _), t2 ->
            SetTree.Insert (comparer, t2, k1)
        | t1, Node (Empty, Empty, k2, _) ->
            SetTree.Insert (comparer, t1, k2)

        | Node (t11, t12, k1, h1), Node (t21, t22, k2, h2) -> // (t11 < k < t12) AND (t21 < k2 < t22) 
            // Divide and Quonquer:
            //   Suppose t1 is largest.
            //   Split t2 using pivot k1 into lo and hi.
            //   Union disjoint subproblems and then combine. 
            if h1 > h2 then
                let lo, _, hi = SetTree.Split (comparer, t2, k1)

                let lo' = SetTree.Union (comparer, t11, lo)
                let hi' = SetTree.Union (comparer, t12, hi)

                // OPTIMIZE : After the union operations, lo' and hi' could possibly be the same
                // (physically equal) as their originals (lo and hi). Can we exploit this to avoid
                // balancing the tree? What would that tell us about the two sets?
                SetTree.Balance (comparer, lo', hi', k1)
            else
                let lo, _, hi = SetTree.Split (comparer, t1, k2)

                let lo' = SetTree.Union (comparer, t21, lo)
                let hi' = SetTree.Union (comparer, t22, hi)

                // OPTIMIZE : After the union operations, lo' and hi' could possibly be the same
                // (physically equal) as their originals (lo and hi). Can we exploit this to avoid
                // balancing the tree? What would that tell us about the two sets?
                SetTree.Balance (comparer, lo', hi', k2)

    /// Implementation. Computes the intersection of two SetTrees.
    static member private IntersectionAux (comparer : IComparer<'T>, b, m, acc) : SetTree<'T> =
        match m with
        | Empty -> acc
        | Node (Empty, Empty, x, _) ->
            if SetTree.Contains (comparer, b, x) then
                SetTree.Insert (comparer, acc, x)
            else acc
        | Node (l, r, x, _) ->
            let acc =
                let acc = SetTree.IntersectionAux (comparer, b, r, acc)
                if SetTree.Contains (comparer, b, x) then
                    SetTree.Insert (comparer, acc, x)
                else acc 
            SetTree.IntersectionAux (comparer, b, l, acc)

    /// Computes the intersection of two SetTrees.
    static member Intersection (comparer : IComparer<'T>, tree1 : SetTree<'T>, tree2 : SetTree<'T>) : SetTree<'T> =
        // If the two trees are identical (physical equality), there's no need to do any work.
        if System.Object.ReferenceEquals (tree1, tree2) then tree1
        else
            // If either of the two trees are empty, the intersection is empty.
            match tree1, tree2 with
            | Empty, _
            | _, Empty ->
                Empty
            | _, _ ->
                SetTree.IntersectionAux (comparer, tree2, tree1, Empty)

    /// Returns a new SetTree created by removing the elements of the second SetTree from the first.
    static member Difference (comparer : IComparer<'T>, tree1 : SetTree<'T>, tree2 : SetTree<'T>) : SetTree<'T> =
        // If the two trees are identical (physical equality), there's no need to do any work.
        if System.Object.ReferenceEquals (tree1, tree2) then SetTree.Empty
        else
            // If the first tree is empty, the result is empty;
            // If the second tree is empty, the result is the first tree.
            match tree1, tree2 with
            | Empty, _ ->
                Empty
            | _, Empty ->
                tree1
            | _, _ ->
                (* OPTIMIZE :   This function should be re-implemented to use the linear-time
                                algorithm which traverses both trees simultaneously and merges
                                them in a single pass. *)

                // Fold over tree2, removing it's elements from tree1
                (tree1, tree2)
                ||> SetTree.Fold (fun tree el ->
                    SetTree.Delete (comparer, tree, el))

    //
    static member IsSubset (comparer : IComparer<'T>, set1 : SetTree<'T>, set2 : SetTree<'T>) : bool =
        SetTree.Forall (fun x -> SetTree.Contains (comparer, set2, x)) set1

    //
    static member IsProperSubset (comparer : IComparer<'T>, set1 : SetTree<'T>, set2 : SetTree<'T>) : bool =
        SetTree.Forall (fun x -> SetTree.Contains (comparer, set2, x)) set1
        && SetTree.Exists (fun x -> not (SetTree.Contains (comparer, set1, x))) set2


/// A Patricia trie implementation, modified so each of it's 'values' is actually a comparison-based set implementation.
/// Used as the underlying data structure for HashSet.
[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type private PatriciaHashSet<'T when 'T : comparison> =
    | Empty
    // Key-HashCode * Value
    | Lf of Key32 * SetTree<'T>
    // Prefix * Mask * Left-Child * Right-Child
    | Br of Prefix32 * Mask32 * PatriciaHashSet<'T> * PatriciaHashSet<'T>

    //
    static member Contains (comparer, valueHash, value : 'T, set : PatriciaHashSet<'T>) =
        match set with
        | Empty ->
            false
        | Lf (j, valueSet) ->
            j = valueHash
            && SetTree<_>.Contains (comparer, valueSet, value)
        | Br (_, m, t0, t1) ->
            PatriciaHashSet.Contains (
                comparer, valueHash, value, (if zeroBit (valueHash, m) then t0 else t1))

    /// Retrieve the first element of the set.
    static member First (set : PatriciaHashSet<'T>) =
        match set with
        | Empty ->
            invalidArg "set" "The set is empty."
        | Lf (_, valueSet) ->
            SetTree<_>.MinElement valueSet
        | Br (_, _, t0, _) ->
            PatriciaHashSet.First t0

    /// Retrieve the last element of the set.
    static member Last (set : PatriciaHashSet<'T>) =
        match set with
        | Empty ->
            invalidArg "set" "The set is empty."
        | Lf (_, valueSet) ->
            SetTree<_>.MaxElement valueSet
        | Br (_, _, _, t1) ->
            PatriciaHashSet.Last t1

    //
    static member Count (set : PatriciaHashSet<'T>) : uint32 =
        match set with
        | Empty ->
            GenericZero
        | Lf (_, valueSet) ->
            SetTree<_>.Count valueSet
        | Br (_, _, left, right) ->
            // Count the number of items in the left and right subtrees.
            PatriciaHashSet.Count left + PatriciaHashSet.Count right

    /// Remove the binding with the specified key from the set.
    /// No exception is thrown if the set.does not contain a binding for the key.
    static member Remove (comparer, valueHash, value : 'T, set : PatriciaHashSet<'T>) =
        match set with
        | Empty ->
            Empty
        | Lf (j, valueSet) ->
            if j = valueHash then
                match SetTree<_>.Delete (comparer, valueSet, value) with
                | SetTree.Empty -> Empty
                | result ->
                    // OPTIMIZATION : If the result is the same as the input, return the
                    // original set.since it wasn't modified.
                    if result == valueSet then set
                    else
                        Lf (j, result)
            else set
        
        | Br (p, m, t0, t1) ->
            if matchPrefix (valueHash, p, m) then
                if zeroBit (valueHash, m) then
                    match PatriciaHashSet.Remove (comparer, valueHash, value, t0) with
                    | Empty -> t1
                    | left ->
                        // Only create a new tree when the value was actually removed
                        // (i.e., the tree was modified).
                        if left == t0 then set
                        else Br (p, m, left, t1)
                else
                    match PatriciaHashSet.Remove (comparer, valueHash, value, t1) with
                    | Empty -> t0
                    | right ->
                        // Only create a new tree when the value was actually removed
                        // (i.e., the tree was modified).
                        if right == t1 then set
                        else Br (p, m, t0, right)
            else set

    //
    static member inline private Singleton (valueHash, value : 'T) =
        Lf (valueHash, SetTree<_>.Singleton value)

    //
    static member inline private Join (p0, t0 : PatriciaHashSet<'T>, p1, t1) =
        let m = branchingBit (p0, p1)
        let p = mask (p0, m)
        if zeroBit (p0, m) then
            Br (p, m, t0, t1)
        else
            Br (p, m, t1, t0)

    /// Insert a binding (key-value pair) into a set. returning a new, updated set.
    static member Add (comparer, valueHash, value : 'T, set) =
        match set with
        | Empty ->
            PatriciaHashSet.Singleton (valueHash, value)
        | Lf (j, valueSet) ->
            if j = valueHash then
                let result = SetTree<_>.Insert (comparer, valueSet, value)

                // OPTIMIZATION : If the result is the same as the input,
                // return the original set instead since it wasn't modified.
                if result == valueSet then set
                else
                    Lf (j, result)
            else
                PatriciaHashSet.Join (valueHash, PatriciaHashSet.Singleton (valueHash, value), j, set)
        | Br (p, m, t0, t1) ->
            if matchPrefix (valueHash, p, m) then
                if zeroBit (valueHash, m) then
                    let left = PatriciaHashSet.Add (comparer, valueHash, value, t0)

                    // Only create a new tree when the value was actually added
                    // (i.e., the tree was modified).
                    if left == t0 then set
                    else Br (p, m, left, t1)
                else
                    let right = PatriciaHashSet.Add (comparer, valueHash, value, t1)

                    // Only create a new tree when the value was actually added
                    // (i.e., the tree was modified).
                    if right == t1 then set
                    else Br (p, m, t0, right)
            else
                PatriciaHashSet.Join (valueHash, PatriciaHashSet.Singleton (valueHash, value), p, set)

    /// Computes the union of two PatriciaHashSets.
    static member Union (comparer, s, t) : PatriciaHashSet<'T> =
        // If the sets are identical, return immediately.
        if s == t then s else
        match s, t with
        | Br (p, m, s0, s1), Br (q, n, t0, t1) ->
            if m = n then
                if p = q then
                    // The trees have the same prefix. Merge the subtrees.
                    let left = PatriciaHashSet.Union (comparer, s0, t0)
                    let right = PatriciaHashSet.Union (comparer, s1, t1)
                    
                    // Only create a new tree if some values were actually added
                    // (i.e., the tree was modified).
                    if left == s0 && right == s1 then s
                    elif left == t0 && right == t1 then t
                    else Br (p, m, left, right)
                else
                    // The prefixes disagree.
                    PatriciaHashSet.Join (p, s, q, t)

            elif m > n then
                if matchPrefix (q, p, m) then
                    // q contains p. Merge t with a subtree of s.
                    if zeroBit (q, m) then
                        let left = PatriciaHashSet.Union (comparer, s0, t)
                        
                        // Only create a new tree when the subtree is actually modified.
                        if left == s0 then s
                        else Br (p, m, left, s1)
                    else
                        let right = PatriciaHashSet.Union (comparer, s1, t)
                        
                        // Only create a new tree when the subtree is actually modified.
                        if right == s1 then s
                        else Br (p, m, s0, right)
                else
                    // The prefixes disagree.
                    PatriciaHashSet.Join (p, s, q, t)

            else
                if matchPrefix (p, q, n) then
                    // p contains q. Merge s with a subtree of t.
                    if zeroBit (p, n) then
                        let left = PatriciaHashSet.Union (comparer, s, t0)
                        
                        // Only create a new tree when the subtree is actually modified.
                        if left == t0 then t
                        else Br (q, n, left, t1)
                    else
                        let right = PatriciaHashSet.Union (comparer, s, t1)
                        
                        // Only create a new tree when the subtree is actually modified.
                        if right == t1 then t
                        else Br (q, n, t0, right)
                else
                    // The prefixes disagree.
                    PatriciaHashSet.Join (p, s, q, t)

        | Br (p, m, s0, s1), Lf (k, _) ->
            if matchPrefix (k, p, m) then
                if zeroBit (k, m) then
                    let left = PatriciaHashSet.Union (comparer, s0, t)
                    
                    // Only create a new tree when the subtree is actually modified.
                    if left == s0 then s
                    else Br (p, m, left, s1)
                else
                    let right = PatriciaHashSet.Union (comparer, s1, t)
                    
                    // Only create a new tree when the subtree is actually modified.
                    if right == s1 then s
                    else Br (p, m, s0, right)
            else
                PatriciaHashSet.Join (k, t, p, s)

        | Lf (k, _), Br (q, n, t0, t1) ->
            if matchPrefix (k, q, n) then
                if zeroBit (k, n) then
                    let left = PatriciaHashSet.Union (comparer, s, t0)

                    // Only create a new tree when the subtree is actually modified.
                    if left == t0 then t
                    else Br (q, n, left, t1)
                else
                    let right = PatriciaHashSet.Union (comparer, s, t1)

                    // Only create a new tree when the subtree is actually modified.
                    if right == t1 then t
                    else Br (q, n, t0, right)
            else
                PatriciaHashSet.Join (k, s, q, t)

        | Lf (j, valueSet1), Lf (k, valueSet2) ->
            if j = k then
                // Combine the sets into a single set.
                let result = SetTree<_>.Union (comparer, valueSet1, valueSet2)
                
                // Only create a new tree if we can't re-use one of the input trees.
                if result == valueSet1 then s
                elif result == valueSet2 then t
                else
                    Lf (j, result)
            else
                PatriciaHashSet.Join (j, s, k, t)

        | _, Empty -> s
        | Empty, _ -> t

    /// Compute the intersection of two PatriciaHashSets.
    static member Intersect (comparer, s, t) : PatriciaHashSet<'T> =
        // If the sets are identical, return immediately.
        if s == t then s else
        match s, t with
        | Br (p, m, s0, s1), Br (q, n, t0, t1) ->
            if m = n then
                if p <> q then Empty
                else
                    let left = PatriciaHashSet.Intersect (comparer, s0, t0)
                    let right = PatriciaHashSet.Intersect (comparer, s1, t1)
                    match left, right with
                    | Empty, r
                    | r, Empty -> r
                    | left, right ->
                        // Only create a new tree if some values were actually removed
                        // (i.e., the tree was modified).
                        if left == s0 && right == s1 then s
                        elif left == t0 && right == t1 then t
                        else Br (p, m, left, right)

            elif m > n then
                if matchPrefix (q, p, m) then
                    if zeroBit (q, m) then
                        PatriciaHashSet.Intersect (comparer, s0, t)
                    else
                        PatriciaHashSet.Intersect (comparer, s1, t)
                else
                    Empty

            else
                if matchPrefix (p, q, n) then
                    if zeroBit (p, n) then
                        PatriciaHashSet.Intersect (comparer, s, t0)
                    else
                        PatriciaHashSet.Intersect (comparer, s, t1)
                else
                    Empty

        | Br (p, m, s0, s1), Lf (k, _) ->
            if matchPrefix (k, p, m) then
                let s' = if zeroBit (k, m) then s0 else s1
                PatriciaHashSet.Intersect (comparer, s', t)
            else Empty

        | Lf (j, _), Br (q, n, t0, t1) ->
            if matchPrefix (j, q, n) then
                let t' = if zeroBit (j, n) then t0 else t1
                PatriciaHashSet.Intersect (comparer, s, t')
            else Empty

        | Lf (j, valueSet1), Lf (k, valueSet2) ->
            if j <> k then Empty
            else
                match SetTree<_>.Intersection (comparer, valueSet1, valueSet2) with
                | SetTree.Empty -> Empty
                | result ->
                    // Only create a new tree if we can't re-use one of the input trees.
                    if result == valueSet1 then s
                    elif result == valueSet2 then t
                    else
                        Lf (j, result)

        | Empty, _
        | _, Empty ->
            Empty

    /// Compute the difference of two PatriciaHashSets.
    static member Difference (comparer, s, t) : PatriciaHashSet<'T> =
        // If the sets are identical, return immediately.
        if s == t then Empty else
        match s, t with
        | Br (p, m, s0, s1), Br (q, n, t0, t1) ->
            if m = n then
                if p <> q then s
                else
                    let left = PatriciaHashSet.Difference (comparer, s0, t0)
                    let right = PatriciaHashSet.Difference (comparer, s1, t1)
                    match left, right with
                    | Empty, r
                    | r, Empty -> r
                    | left, right ->
                        // Only create a new tree if some values were actually removed
                        // (i.e., the tree was modified).
                        if left == s0 && right == s1 then s
                        else Br (p, m, left, right)

            elif m > n then
                if matchPrefix (q, p, m) then
                    if zeroBit (q, m) then
                        match PatriciaHashSet.Difference (comparer, s0, t) with
                        | Empty -> s1
                        | left ->
                            // Only create a new tree some values were actually removed
                            // (i.e., the tree was modified).
                            if left == s0 then s
                            else Br (p, m, left, s1)
                    else
                        match PatriciaHashSet.Difference (comparer, s1, t) with
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
                        PatriciaHashSet.Difference (comparer, s, t0)
                    else
                        PatriciaHashSet.Difference (comparer, s, t1)
                else s

        | Br (p, m, s0, s1), Lf (k, _) ->
            if matchPrefix (k, p, m) then
                if zeroBit (k, m) then
                    match PatriciaHashSet.Difference (comparer, s0, t) with
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
                    match PatriciaHashSet.Difference (comparer, s1, t) with
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

        | Lf (j, _), Br (q, n, t0, t1) ->
            if matchPrefix (j, q, n) then
                let t' = if zeroBit (j, n) then t0 else t1
                PatriciaHashSet.Difference (comparer, s, t')
            else s
        
        | Lf (j, valueSet1), Lf (k, valueSet2) ->
            if j <> k then s
            else
                match SetTree<_>.Difference (comparer, valueSet1, valueSet2) with
                | SetTree.Empty -> Empty
                | result ->
                    // Only create a new tree if we can't re-use one of the input trees.
                    if result == valueSet1 then s
                    elif result == valueSet2 then t
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
    static member private SubsetCompare (comparer, t1 : PatriciaHashSet<'T>, t2 : PatriciaHashSet<'T>) : int =
        match t1, t2 with
        | Br (p1, m1, l1, r1), Br (p2, m2, l2, r2) ->
            if shorter (m1, m2) then 1
            elif shorter (m2, m1) then
                if not <| matchPrefix (p1, p2, m2) then 1
                elif zeroBit (p1, m2) then
                    match PatriciaHashSet.SubsetCompare (comparer, t1, l2) with 1 -> 1 | _ -> -1
                else
                    match PatriciaHashSet.SubsetCompare (comparer, t1, r2) with 1 -> 1 | _ -> -1
            elif p1 = p2 then
                let left = PatriciaHashSet.SubsetCompare (comparer, l1, l2)
                let right = PatriciaHashSet.SubsetCompare (comparer, r1, r2)
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
                match PatriciaHashSet.SubsetCompare (comparer, t1, l) with 1 -> 1 | _ -> -1
            else
                match PatriciaHashSet.SubsetCompare (comparer, t1, r) with 1 -> 1 | _ -> -1

        | Lf (j, valueSet1), Lf (k, valueSet2) ->
            if j <> k then 1
            else
                // Compare the two sets.
                // TODO : Make sure the value returned by the 'compare' function used here
                // adheres to the semantics of this function; if not, we'll need to call
                // Set.isSubset and Set.isProperSubset which'll be much slower.
                SetTree<_>.Compare (comparer, valueSet1, valueSet2)

        | Br (_,_,_,_), (Empty | Lf (_,_))
        | Lf _, Empty ->
            // The maps are disjoint.
            1

        | Empty, Empty -> 0
        | Empty, _ -> -1

    /// Is 'set1' a proper subset of 'set2'?
    /// IsProperSubset (set1, set2) returns true if all keys in set1 are in set2,
    /// and at least one element in set2 is not in set1.
    static member IsProperSubsetOf (comparer, set1 : PatriciaHashSet<'T>, set2 : PatriciaHashSet<'T>) : bool =
        match PatriciaHashSet.SubsetCompare (comparer, set1, set2) with
        | -1 -> true
        | _ -> false

    /// Is 'set1' a subset of 'set2'?
    /// IsSubset (set1, set2) returns true if all keys in set1 are in set2.
    static member IsSubsetOf (comparer, set1 : PatriciaHashSet<'T>, set2 : PatriciaHashSet<'T>) : bool =
        match PatriciaHashSet.SubsetCompare (comparer, set1, set2) with
        | -1 | 0 -> true
        | _ -> false

    //
    static member OfSeq (comparer, source : seq<'T>) : PatriciaHashSet<'T> =
        (Empty, source)
        ||> Seq.fold (fun trie value ->
            let valueHash = uint32 <| hash value
            PatriciaHashSet.Add (comparer, valueHash, value, trie))

    //
    static member OfList (comparer, source : 'T list) : PatriciaHashSet<'T> =
        // Preconditions
        checkNonNull "source" source

        (Empty, source)
        ||> List.fold (fun trie value ->
            let valueHash = uint32 <| hash value
            PatriciaHashSet.Add (comparer, valueHash, value, trie))

    //
    static member OfArray (comparer, source : 'T[]) : PatriciaHashSet<'T> =
        // Preconditions
        checkNonNull "source" source

        (Empty, source)
        ||> Array.fold (fun trie value ->
            let valueHash = uint32 <| hash value
            PatriciaHashSet.Add (comparer, valueHash, value, trie))

    //
    static member Iterate (action : 'T -> unit, set) : unit =
        match set with
        | Empty -> ()
        | Lf (_, valueSet) ->
            SetTree<_>.Iterate action valueSet
        | Br (_, _, left, right) ->
            // Iterate over the left and right subtrees.
            PatriciaHashSet.Iterate (action, left)
            PatriciaHashSet.Iterate (action, right)

    //
    static member IterateBack (action : 'T -> unit, set) : unit =
        match set with
        | Empty -> ()
        | Lf (_, valueSet) ->
            SetTree.IterateBack action valueSet
        | Br (_, _, left, right) ->
            // Iterate over the right and left subtrees.
            PatriciaHashSet.Iterate (action, right)
            PatriciaHashSet.Iterate (action, left)

    //
    static member Fold (folder : 'State -> 'T -> 'State, state : 'State, set) : 'State =
        match set with
        | Empty ->
            state
        | Lf (_, valueSet) ->
            SetTree<_>.Fold folder state valueSet
        | Br (_, _, left, right) ->
            // Fold over the left subtree, then the right subtree.
            let state = PatriciaHashSet.Fold (folder, state, left)
            PatriciaHashSet.Fold (folder, state, right)

    //
    static member FoldBack (folder : 'T -> 'State -> 'State, state : 'State, set) : 'State =
        match set with
        | Empty ->
            state
        | Lf (_, valueSet) ->
            SetTree<_>.FoldBack folder valueSet state
        | Br (_, _, left, right) ->
            // Fold over the right subtree, then the left subtree.
            let state = PatriciaHashSet.FoldBack (folder, state, right)
            PatriciaHashSet.FoldBack (folder, state, left)

    //
    static member TryFind (predicate : 'T -> bool, set) : 'T option =
        match set with
        | Empty ->
            None
        | Lf (_, valueSet) ->
            SetTree<_>.TryFind predicate valueSet
        | Br (_, _, left, right) ->
            // Visit the left subtree, then the right subtree if necessary.
            match PatriciaHashSet.TryFind (predicate, left) with
            | None ->
                PatriciaHashSet.TryFind (predicate, right)
            | res ->
                res

    //
    static member TryPick (picker : 'T -> 'U option, set) : 'U option =
        match set with
        | Empty ->
            None
        | Lf (_, valueSet) ->
            SetTree<_>.TryPick picker valueSet
        | Br (_, _, left, right) ->
            // Visit the left subtree, then the right subtree if necessary.
            match PatriciaHashSet.TryPick (picker, left) with
            | None ->
                PatriciaHashSet.TryPick (picker, right)
            | res ->
                res

    //
    static member ToSeq (set : PatriciaHashSet<'T>) =
        seq {
        match set with
        | Empty -> ()
        | Lf (_, valueSet) ->
            yield! SetTree<_>.ToSeq valueSet

        | Br (_, _, left, right) ->
            // Recursively visit the children.
            yield! PatriciaHashSet.ToSeq left
            yield! PatriciaHashSet.ToSeq right
        }


/// <summary>Immutable, unordered sets based on PATRICIA tries and binary tries.</summary>
/// <typeparam name="T">The type of the values stored in the set.</typeparam>
[<Sealed>]
//[<StructuredFormatDisplay("")>]
[<DebuggerDisplay("Count = {Count}")>]
[<DebuggerTypeProxy(typedefof<HashSetDebuggerProxy<int>>)>]
type HashSet<'T when 'T : comparison> private (trie : PatriciaHashSet<'T>) =
    /// The empty HashSet instance.
    static let empty : HashSet<'T> =
        HashSet Empty

    /// The comparer for the type of the values contained in this collection.
    /// It is cached here for fast access.
    //[<System.NonSerialized>]
    static let comparer = LanguagePrimitives.FastGenericComparer<'T>

    /// The empty HashSet.
    static member Empty
        with get () = empty

    //
    new (elements : seq<'T>) =
        // Preconditions
        checkNonNull "elements" elements

        // OPTIMIZE : Try to cast the sequence to array or list;
        // if it succeeds use the specialized method for that type for better performance.
        HashSet (PatriciaHashSet.OfSeq (comparer, elements))

    /// The internal representation of the HashSet.
    member private __.Trie
        with get () = trie

    //
    static member private Equals (left : HashSet<'T>, right : HashSet<'T>) =
        // OPTIMIZE : Would it be significantly faster if we re-implemented this to work
        // directly on the SetTrees instead of using enumerators? Or, at least using an
        // imperative loop instead of a recursive function?
        use e1 = (left :> seq<_>).GetEnumerator ()
        use e2 = (right :> seq<_>).GetEnumerator ()
        let rec loop () =
            let m1 = e1.MoveNext ()
            let m2 = e2.MoveNext ()
            (m1 = m2) && (not m1 || ((e1.Current = e2.Current) && loop ()))
        loop ()

    //
    static member private Compare (left : HashSet<'T>, right : HashSet<'T>) =
        // OPTIMIZE : Re-implement this to operate directly on the SetTrees instead of using enumerators.
        (PatriciaHashSet.ToSeq left.Trie, PatriciaHashSet.ToSeq right.Trie)
        ||> Seq.compareWith (fun x y ->
            comparer.Compare (x, y))

    /// <inherit />
    override this.Equals other =
        match other with
        | :? HashSet<'T> as other ->
            HashSet<_>.Equals (this, other)
        | _ ->
            false

    /// The number of bindings in the HashSet.
    member __.Count
        with get () : int =
            PatriciaHashSet.Count trie |> Checked.int

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
        PatriciaHashSet.Contains (comparer, valueHash, value, trie)

    /// Returns a new HashSet with the binding added to this HashSet.
    member this.Add (value : 'T) : HashSet<'T> =
        // If the trie isn't modified, just return this HashSet instead of creating a new one.
        let valueHash = uint32 <| hash value
        let trie' = PatriciaHashSet.Add (comparer, valueHash, value, trie)
        if trie == trie' then this
        else HashSet (trie')

    /// Removes an element from the domain of the HashSet.
    /// No exception is raised if the element is not present.
    member this.Remove (value : 'T) : HashSet<'T> =
        // If the trie isn't modified, just return this HashSet instead of creating a new one.
        let valueHash = uint32 <| hash value
        let trie' = PatriciaHashSet.Remove (comparer, valueHash, value, trie)
        if trie == trie' then this
        else HashSet (trie')

    /// Returns a new HashSet created by merging the two specified HashSets.
    member this.Union (otherSet : HashSet<'T>) : HashSet<'T> =
        // If this set's trie is the same as the other set's trie, we can return immediately.
        if trie == otherSet.Trie then this
        else
            // If the result is the same (physical equality) to one of the inputs,
            // return that input instead of creating a new HashSet.
            let trie' = PatriciaHashSet.Union (comparer, trie, otherSet.Trie)
            if trie == trie' then this
            elif otherSet.Trie == trie' then otherSet
            else HashSet (trie')

    /// Returns the intersection of two HashSets.
    member this.Intersect (otherSet : HashSet<'T>) : HashSet<'T> =
        // If this set's trie is the same as the other set's trie, we can return immediately.
        if trie == otherSet.Trie then this
        else
            // If the result is the same (physical equality) to one of the inputs,
            // return that input instead of creating a new HashSet.
            let trie' = PatriciaHashSet.Intersect (comparer, trie, otherSet.Trie)
            if trie == trie' then this
            elif otherSet.Trie == trie' then otherSet
            else HashSet (trie')

    /// Returns a new HashSet created by removing the second HashSet from the first.
    member this.Difference (otherSet : HashSet<'T>) : HashSet<'T> =
        // If this set's trie is the same as the other set's trie, we can return immediately.
        if trie == otherSet.Trie then HashSet.Empty
        else
            // If the result is the same (physical equality) to one of the inputs,
            // return that input instead of creating a new HashSet.
            let trie' = PatriciaHashSet.Difference (comparer, trie, otherSet.Trie)
            if trie == trie' then this
            elif otherSet.Trie == trie' then otherSet
            else HashSet (trie')

    /// Computes the union of a sequence of HashSets.
    static member internal UnionMany (sets : seq<HashSet<'T>>) : HashSet<'T> =
        // Preconditions
        checkNonNull "sets" sets

        let result =
            (PatriciaHashSet.Empty, sets)
            ||> Seq.fold (fun combinedSetTree set ->
                PatriciaHashSet.Union (comparer, combinedSetTree, set.Trie))

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
        PatriciaHashSet.IsSubsetOf (comparer, trie, otherSet.Trie)

    /// Determines if set1 is a proper subset of set2.
    // IsProperSubsetOf (otherSet) returns true if all values in this set are in 'otherSet',
    // and 'otherSet' contains at least one value which is not in this set.
    member __.IsProperSubsetOf (otherSet : HashSet<'T>) : bool =
        PatriciaHashSet.IsProperSubsetOf (comparer, trie, otherSet.Trie)

    //
    member __.IsSupersetOf (otherSet : HashSet<'T>) : bool =
        PatriciaHashSet.IsSubsetOf (comparer, otherSet.Trie, trie)

    //
    member __.IsProperSupersetOf (otherSet : HashSet<'T>) : bool =
        PatriciaHashSet.IsProperSubsetOf (comparer, otherSet.Trie, trie)

    /// The HashSet containing the given binding.
    static member Singleton (value : 'T) : HashSet<'T> =
        let valueHash = uint32 <| hash value
        HashSet (
            Lf (valueHash, SetTree<_>.Singleton value))

    /// Returns a new HashSet made from the given bindings.
    static member OfSeq (source : seq<'T>) : HashSet<'T> =
        // Preconditions
        checkNonNull "source" source

        HashSet (PatriciaHashSet.OfSeq (comparer, source))

    /// Returns a new HashSet made from the given bindings.
    static member OfList (source : 'T list) : HashSet<'T> =
        // Preconditions
        checkNonNull "source" source

        // OPTIMIZATION : If the source is empty return immediately.
        if List.isEmpty source then
            HashSet.Empty
        else
            HashSet (PatriciaHashSet.OfList (comparer, source))

    /// Returns a new HashSet made from the given bindings.
    static member OfArray (source : 'T[]) : HashSet<'T> =
        // Preconditions
        checkNonNull "source" source

        // OPTIMIZATION : If the source is empty return immediately.
        if Array.isEmpty source then
            HashSet.Empty
        else
            HashSet (PatriciaHashSet.OfArray (comparer, source))

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

    // OPTIMIZE : Instead of computing this repeatedly -- this type is immutable so we should
    // lazily compute the hashcode once instead. However, we do need to account for the case
    // where an instance is created via deserialization, so implement this with a mutable backing field
    // which is initially set to zero (0); when GetHashCode() is called, it'll check the value
    // and if equal to zero, it'll compute with ComputeHashCode() then store the value using Interlocked.Exchange().
    member this.ComputeHashCode () =
        let inline combineHash x y = (x <<< 1) + y + 631
        this.Fold ((fun res x ->
            combineHash res (hash x)), 0)
        |> abs

    override this.GetHashCode () =
        this.ComputeHashCode ()

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
                PatriciaHashSet.Count trie |> Checked.int

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
            PatriciaHashSet.Contains (comparer, valueHash, item, trie)

        /// <inherit />
        member this.CopyTo (array, arrayIndex) =
            // Preconditions
            checkNonNull "array" array
            if arrayIndex < 0 then
                argOutOfRange "arrayIndex" "The target array index cannot be negative."

            if arrayIndex + this.Count > Array.length array then
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
    internal HashSetDebuggerProxy<'T when 'T : comparison> (set : HashSet<'T>) =

    [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
    member __.Items
        with get () : 'T[] =
            set.ToArray ()


/// Functional programming operators related to the HashSet type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HashSet =
    /// The empty set.
    [<CompiledName("Empty")>]
    let empty<'T when 'T : comparison> =
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
