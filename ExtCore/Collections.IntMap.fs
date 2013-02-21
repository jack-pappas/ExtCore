(*

Copyright 2010-2012 TidePowerd Ltd.
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


(* TODO :   Implement Okasaki and Gill's data structure from:
            "Fast Mergeable Integer Maps".

            Also implement a "tagged" version which uses ints with measure types.
            (Call this a TagMap.)
            Maybe also implement an EnumMap?
            
            This data structure should implement IDictionary<_,_> like the standard F# Map. *)
(* TODO :   Modify some of the operations below to use CPS to avoid overhead of stack frames. *)

//
module internal BitOps =
    (* TODO :   Re-implement the 'highestBit' function using the Count Leading Zeros (clz) operation.
                This should be significantly faster than our current implementation (or even Okasaki's
                original implementation, since 'clz' can be implemented by just a few operations
                (or a CPU intrinsic) using the algorithm based on de Bruijn sequences.

                It may be possible to further optimize this code by replacing the table with
                a 'match' (which would be compiled into a 'switch' IL OpCode). This change
                won't be made until we can profile and confirm it's actually faster. *)

    //
    let inline zeroBit (k, m) : bool =
        k &&& m = GenericZero

    //
    let inline lowestBit (x : int) : uint32 =
        uint32 (x &&& -x)

    // Returns the greatest power-of-two value contained within 'x'.
    let highestBit (x, m) =
        // Zero all bits below m
        let x' = x &&& ~~~(m - 1u)

        let rec highb x =
            let m = lowestBit (int x)
            if x = m then m
            else highb (x - m)

        highb x'

    /// Finds the first bit at which p0 and p1 disagree.
    let (*inline*) branchingBit (p0, p1) =
        highestBit (p0 ^^^ p1, 1u)

    //
    let inline mask (k, m) =
        (k ||| (m - GenericOne)) &&& ~~~m

    //
    let inline matchPrefix (k, p, m) =
        mask (k, m) = p

open BitOps


/// A Patricia trie implementation.
/// Used as the underlying data structure for IntMap (and TagMap).
type internal PatriciaMap<'T> =
    | Empty
    | Lf of uint32 * 'T
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
                key, (if key <= p then t0 else t1))

    //
    static member ContainsKey (key, map : PatriciaMap<'T>) =
        match map with
        | Empty ->
            false
        | Lf (j, _) ->
            key = j
        | Br (_, m, t0, t1) ->
            PatriciaMap.ContainsKey
                (key, (if zeroBit (key, m) then t0 else t1))

    //
    static member Count (map : PatriciaMap<'T>) : int =
        // OPTIMIZE : We can 'strictify' this by adding an accumulator argument, so we'll
        // only need to use continuations for one side of the tree (left or right child).
        // We may also be able to eliminate continuations altogether by using an explicit
        // stack (either an F# list or System.Collections.Generic.Stack<'T>).
        let rec count (t : PatriciaMap<'T>) cont =
            match t with
            | Empty ->
                cont 0
            | Lf (_,_) ->
                cont 1
            | Br (_, _, left, right) ->
                count left <| fun leftCount ->
                count right <| fun rightCount ->
                    leftCount + rightCount
                    |> cont

        count map id

    // TODO : This is an experimental version of count which is optimized for performance.
    // It should work, but we need to do some tests to be sure. Once we're confident it's correct
    // we'll remove the original 'Count' function and use this once instead.
    static member FastCount t =
        match t with
        | Empty -> 0u
        | Lf (_,_) -> 1u
        | Br (_,_,_,_) as t ->
            // TODO : Run some experiments to determine if this is a good initial capacity,
            // or if there is a more suitable value.
            let stack = System.Collections.Generic.Stack (64)

            // Add the initial tree to the stack.
            stack.Push t

            /// Recursively processes the tree using the mutable stack.
            let rec fastCount acc =
                if stack.Count = 0 then acc
                else
                    match stack.Pop () with
                    | Empty ->
                        fastCount acc
                    | Lf (_,_) ->
                        fastCount (acc + 1u)
                    
                    (* OPTIMIZATION :   When one or both children of this node are leaves, we handle
                                        them directly since it's a little faster. *)
                    | Br (_, _, Lf (_,_), Lf (_,_)) ->
                        fastCount (acc + 2u)
                    
                    | Br (_, _, Lf (_,_), child)
                    | Br (_, _, child, Lf (_,_)) ->
                        stack.Push child
                        fastCount (acc + 1u)

                    | Br (_, _, left, right) ->
                        // Push both children onto the stack and recurse to process them.
                        stack.Push left
                        stack.Push right
                        fastCount acc

            fastCount 0u

    //
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
    static member private Join (p0, t0 : PatriciaMap<'T>, p1, t1) =
        let m = branchingBit (p0, p1)
        if zeroBit (p0, m) then
            Br (mask (p0, m), m, t0, t1)
        else
            Br (mask (p0, m), m, t1, t0)

    //
    static member Insert (key, value : 'T, map) =
        // TODO : Lift this recursive function out into a private member
        let rec ins = function
            | Empty ->
                Lf (key, value)
            | Lf (j, y) as t ->
                let newLeaf = Lf (key, value)
                if j = key then
                    newLeaf
                else
                    PatriciaMap.Join (key, newLeaf, j, t)

            | Br (p, m, t0, t1) as t ->
                if matchPrefix (key, p, m) then
                    if zeroBit (key, m) then
                        Br (p, m, ins t0, t1)
                    else
                        Br (p, m, t0, ins t1)
                else
                    PatriciaMap.Join (key, Lf (key, value), p, t)

        // Call the implementation function.
        ins map

    //
    static member Merge (s, t) : PatriciaMap<'T> =
        match s, t with
        | Empty, t
        | t, Empty ->
            t
        | Lf (k, x), t ->
            PatriciaMap.Insert (k, x, t)
        | (Br (p, m, s0, s1) as s), Lf (k, x) ->
            if matchPrefix (k, p, m) then
                if zeroBit (k, m) then
                    let left = PatriciaMap.Insert (k, x, s0)
                    Br (p, m, left, s1)
                else
                    let right = PatriciaMap.Insert (k, x, s1)
                    Br (p, m, s0, right)
            else
                PatriciaMap.Join (k, Lf (k, x), p, s)

        | (Br (p, m, s0, s1) as s), (Br (q, n, t0, t1) as r) ->
            if m = n && p = q then
                // The trees have the same prefix. Merge the subtrees.
                let left = PatriciaMap.Merge (s0, t0)
                let right = PatriciaMap.Merge (s1, t1)
                Br (p, m, left, right)
                
            elif m < n && matchPrefix (q, p, m) then
                // q contains p. Merge t with a subtree of s.
                if zeroBit (q, m) then
                    let left = PatriciaMap.Merge (s0, t)
                    Br (p, m, left, s1)
                else
                    let right = PatriciaMap.Merge (s1, t)
                    Br (p, m, s0, right)

            elif m > n && matchPrefix (p, q, n) then
                // p contains q. Merge s with a subtree of t.
                if zeroBit (p, n) then
                    let left = PatriciaMap.Merge (s, t0)
                    Br (q, n, left, t1)
                else
                    let right = PatriciaMap.Merge (s, t1)
                    Br (q, n, t0, right)
            else
                // The prefixes disagree.
                PatriciaMap.Join (p, s, q, t)

//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal PatriciaMap =
    //
    let rec lookup (k, t) =
        match t with
        | Empty ->
            None
        | Lf (j, x) ->
            if j = k then Some x
            else None
        | Br (p, m, t0, t1) ->
            lookup (k, (if k <= p then t0 else t1))

    //
    let rec containsKey (k, t) =
        match t with
        | Empty ->
            false
        | Lf (j, _) ->
            k = j
        | Br (_, m, t0, t1) ->
            containsKey
                (k, (if zeroBit (k, m) then t0 else t1))

    //
    let count t =
        // OPTIMIZE : We can 'strictify' this by adding an accumulator argument, so we'll
        // only need to use continuations for one side of the tree (left or right child).
        // We may also be able to eliminate continuations altogether by using an explicit
        // stack (either an F# list or System.Collections.Generic.Stack<'T>).
        let rec count (t : PatriciaMap<'T>) cont =
            match t with
            | Empty ->
                cont 0
            | Lf (_,_) ->
                cont 1
            | Br (_, _, left, right) ->
                count left <| fun leftCount ->
                count right <| fun rightCount ->
                    leftCount + rightCount
                    |> cont

        // Call the recursive implementation.
        count t id

    // TODO : This is an experimental version of count which is optimized for performance.
    // It should work, but we need to do some tests to be sure. Once we're confident it's correct
    // we'll remove the original 'count' function and use this once instead.
    let fastCount t =
        match t with
        | Empty -> 0u
        | Lf (_,_) -> 1u
        | Br (_,_,_,_) as t ->
            // TODO : Run some experiments to determine if this is a good initial capacity,
            // or if there is a more suitable value.
            let stack = System.Collections.Generic.Stack (64)

            // Add the initial tree to the stack.
            stack.Push t

            /// Recursively processes the tree using the mutable stack.
            let rec fastCount acc =
                if stack.Count = 0 then acc
                else
                    match stack.Pop () with
                    | Empty ->
                        fastCount acc
                    | Lf (_,_) ->
                        fastCount (acc + 1u)
                    
                    (* OPTIMIZATION :   When one or both children of this node are leaves, we handle
                                        them directly since it's a little faster. *)
                    | Br (_, _, Lf (_,_), Lf (_,_)) ->
                        fastCount (acc + 2u)
                    
                    | Br (_, _, Lf (_,_), child)
                    | Br (_, _, child, Lf (_,_)) ->
                        stack.Push child
                        fastCount (acc + 1u)

                    | Br (_, _, left, right) ->
                        // Push both children onto the stack and recurse to process them.
                        stack.Push left
                        stack.Push right
                        fastCount acc

            fastCount 0u

    //
    let rec remove (k, t) =
        match t with
        | Empty ->
            Empty
        | Lf (j, _) ->
            if j = k then Empty
            else t
        
        | Br (p, m, t0, t1) ->
            if matchPrefix (k, p, m) then
                if zeroBit (k, m) then
                    match remove (k, t0) with
                    | Empty -> t1
                    | t0 ->
                        Br (p, m, t0, t1)
                else
                    match remove (k, t1) with
                    | Empty -> t0
                    | t1 ->
                        Br (p, m, t0, t1)
            else t

    //
    let private join (p0, t0 : PatriciaMap<'T>, p1, t1) =
        let m = branchingBit (p0, p1)
        if zeroBit (p0, m) then
            Br (mask (p0, m), m, t0, t1)
        else
            Br (mask (p0, m), m, t1, t0)

    //
    let insert (k, x : 'T, t) =
        let rec ins = function
            | Empty ->
                Lf (k, x)
            | Lf (j, y) as t ->
                let newLeaf = Lf (k, x)
                if j = k then
                    newLeaf
                else
                    join (k, newLeaf, j, t)

            | Br (p, m, t0, t1) as t ->
                if matchPrefix (k, p, m) then
                    if zeroBit (k, m) then
                        Br (p, m, ins t0, t1)
                    else
                        Br (p, m, t0, ins t1)
                else
                    join (k, Lf (k, x), p, t)

        // Call the implementation function.
        ins t

    //
    let rec merge (s, t) : PatriciaMap<'T> =
        match s, t with
        | Empty, t
        | t, Empty ->
            t
        | Lf (k, x), t ->
            insert (k, x, t)
        | (Br (p, m, s0, s1) as s), Lf (k, x) ->
            if matchPrefix (k, p, m) then
                if zeroBit (k, m) then
                    Br (p, m, insert (k, x, s0), s1)
                else
                    Br (p, m, s0, insert (k, x, s1))
            else
                join (k, Lf (k, x), p, s)

        | (Br (p, m, s0, s1) as s), (Br (q, n, t0, t1) as r) ->
            if m = n && p = q then
                // The trees have the same prefix. Merge the subtrees.
                Br (p, m, merge (s0, t0), merge (s1, t1))
                
            elif m < n && matchPrefix (q, p, m) then
                // q contains p. Merge t with a subtree of s.
                if zeroBit (q, m) then
                    Br (p, m, merge (s0, t), s1)
                else
                    Br (p, m, s0, merge (s1, t))

            elif m > n && matchPrefix (p, q, n) then
                // p contains q. Merge s with a subtree of t.
                if zeroBit (p, n) then
                    Br (q, n, merge (s, t0), t1)
                else
                    Br (q, n, t0, merge (s, t1))
            else
                // The prefixes disagree.
                join (p, s, q, t)


//
[<Sealed>]
type IntMap<'T> internal (trie : PatriciaMap<'T>) =
    /// The empty IntMap.
    static member Empty
        with get () : IntMap<'T> =
            IntMap Empty

    /// The internal representation of the IntMap.
    member internal __.Trie
        with get () : PatriciaMap<'T> =
            trie

    /// Is the map empty?
    member __.IsEmpty
        with get () =
            match trie with
            | Empty -> true
            | _ -> false

//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IntMap =
    /// The empty IntMap.
    [<GeneralizableValue>]
    [<CompiledName("Empty")>]
    let empty<'T> =
        IntMap<'T>.Empty

    /// Is the map empty?
    [<CompiledName("IsEmpty")>]
    let isEmpty (map : IntMap<'T>) : bool =
        // Preconditions
        checkNonNull "map" map

        map.IsEmpty

    /// Returns an IntMap with a single entry created
    /// from the specified key and value.
    [<CompiledName("Singleton")>]
    let singleton (key : int) (value : 'T) : IntMap<'T> =
        IntMap (Lf (uint32 key, value))

    //
    [<CompiledName("Count")>]
    let count (map : IntMap<'T>) : int =
        // Preconditions
        checkNonNull "map" map

        PatriciaMap.count map.Trie

    /// Look up an element in the IntMap returning a Some value if the
    /// element is in the domain of the IntMap and None if not.
    [<CompiledName("TryFind")>]
    let tryFind (key : int) (map : IntMap<'T>) : 'T option =
        // Preconditions
        checkNonNull "map" map
        
        PatriciaMap.lookup (
            uint32 key, map.Trie)
        
    //
    [<CompiledName("Find")>]
    let find (key : int) (map : IntMap<'T>) : 'T =
        // Preconditions
        checkNonNull "map" map

        let result =
            PatriciaMap.lookup (
                uint32 key, map.Trie)

        match result with
        | Some x -> x
        | None ->
            // TODO : Add a better error message which includes the key.
            raise <| System.Collections.Generic.KeyNotFoundException ()

    //
    [<CompiledName("Add")>]
    let add (key : int) (value : 'T) (map : IntMap<'T>) : IntMap<'T> =
        // Preconditions
        checkNonNull "map" map

        //
        let newTrie =
            PatriciaMap.insert (
                uint32 key, value, map.Trie)
        IntMap (newTrie)

    //
    [<CompiledName("Remove")>]
    let remove (key : int) (map : IntMap<'T>) : IntMap<'T> =
        // Preconditions
        checkNonNull "map" map

        let newTrie =
            PatriciaMap.remove (uint32 key, map.Trie)
        IntMap (newTrie)

    //
    [<CompiledName("ContainsKey")>]
    let containsKey (key : int) (map : IntMap<'T>) : bool =
        // Preconditions
        checkNonNull "map" map

        PatriciaMap.containsKey (uint32 key, map.Trie)


    // TODO
    // filter
    // partition
    // fold, foldBack
    // iter, iterBack
    // ofArray, toArray
    // ofSeq, toSeq
    // ofList, toList
    

