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
open BitOps
open PatriciaTrieConstants


(* OPTIMIZE :   Some of the functional-style operations on IntMap use direct non-tail-recursion;
                performance may be improved if we modify these to use CPS instead. *)

/// A Patricia trie implementation.
/// Used as the underlying data structure for IntMap (and TagMap).
//[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type internal PatriciaMap<'T> =
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
    static member (*inline*) private Join (p0, t0 : PatriciaMap<'T>, p1, t1) =
        let m = branchingBit (p0, p1)
        let p = mask (p0, m)
        if zeroBit (p0, m) then
            Br (p, m, t0, t1)
        else
            Br (p, m, t1, t0)

    /// Resolves a collision between two values. Used by the 'Insert' and 'Merge' methods
    /// to handle the case where a binding is inserted into a map which already contains
    /// a binding with the same key.
    // TODO : Rename to something like 'ResolveCollision' for clarity.
    static member (*inline*) private Combine (x : 'T, y : 'T) : 'T =
        x   // fst
        //y   // snd

    /// Insert a binding (key-value pair) into a map, returning a new, updated map.
    static member Insert (key, value : 'T, map) =
        // TODO : There's no need to have a separate function here (which gets lifted out
        // by the compiler); instead, we can make some small modifications to the code within
        // it then recurse on the InsertRec method here.
        let rec ins = function
            | Empty ->
                Lf (key, value)
            | Lf (j, y) as t ->
                if j = key then
                    Lf (key, PatriciaMap.Combine (value, y))
                else
                    PatriciaMap.Join (key, Lf (key, value), j, t)

            | Br (p, m, t0, t1) as t ->
                if matchPrefix (key, p, m) then
                    if zeroBit (key, m) then
                        let left = ins t0
                        Br (p, m, left, t1)
                    else
                        let right = ins t1
                        Br (p, m, t0, right)
                else
                    PatriciaMap.Join (key, Lf (key, value), p, t)

        // Call the implementation function.
        ins map

    // TEMP : A special version of Insert which swaps the order of the arguments
    // when calling 'Combine'.
    // TODO : Figure out a better way to handle this -- we shouldn't need to duplicate the method.
    static member private InsertFlipped (key, value : 'T, map) =
        // TODO : There's no need to have a separate function here (which gets lifted out
        // by the compiler); instead, we can make some small modifications to the code within
        // it then recurse on the InsertRec method here.
        let rec ins = function
            | Empty ->
                Lf (key, value)
            | Lf (j, y) as t ->
                if j = key then
                    Lf (key, PatriciaMap.Combine (y, value))
                else
                    PatriciaMap.Join (key, Lf (key, value), j, t)

            | Br (p, m, t0, t1) as t ->
                if matchPrefix (key, p, m) then
                    if zeroBit (key, m) then
                        let left = ins t0
                        Br (p, m, left, t1)
                    else
                        let right = ins t1
                        Br (p, m, t0, right)
                else
                    PatriciaMap.Join (key, Lf (key, value), p, t)

        // Call the implementation function.
        ins map

    /// Computes the union of two PatriciaMaps.
    static member Union (s, t) : PatriciaMap<'T> =
        match s, t with
        | Empty, t
        | t, Empty ->
            t
        | Lf (k, x), t ->
            PatriciaMap.Insert (k, x, t)
        | t, Lf (k, x) ->
            PatriciaMap.InsertFlipped (k, x, t)

        | (Br (p, m, s0, s1) as s), (Br (q, n, t0, t1) as r) ->
            if m = n && p = q then
                // The trees have the same prefix. Merge the subtrees.
                let left = PatriciaMap.Union (s0, t0)
                let right = PatriciaMap.Union (s1, t1)
                Br (p, m, left, right)
                
            elif m < n && matchPrefix (q, p, m) then
                // q contains p. Merge t with a subtree of s.
                if zeroBit (q, m) then
                    let left = PatriciaMap.Union (s0, t)
                    Br (p, m, left, s1)
                else
                    let right = PatriciaMap.Union (s1, t)
                    Br (p, m, s0, right)

            elif m > n && matchPrefix (p, q, n) then
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

    //
    static member OfSeq (source : seq<int * 'T>) : PatriciaMap<'T> =
        (Empty, source)
        ||> Seq.fold (fun trie (key, value) ->
            PatriciaMap.Insert (uint32 key, value, trie))

    //
    static member OfList (source : (int * 'T) list) : PatriciaMap<'T> =
        // Preconditions
        checkNonNull "source" source

        (Empty, source)
        ||> List.fold (fun trie (key, value) ->
            PatriciaMap.Insert (uint32 key, value, trie))

    //
    static member OfArray (source : (int * 'T)[]) : PatriciaMap<'T> =
        // Preconditions
        checkNonNull "source" source

        (Empty, source)
        ||> Array.fold (fun trie (key, value) ->
            PatriciaMap.Insert (uint32 key, value, trie))

    //
    static member OfMap (source : Map<int, 'T>) : PatriciaMap<'T> =
        // Preconditions
        checkNonNull "source" source

        (Empty, source)
        ||> Map.fold (fun trie key value ->
            PatriciaMap.Insert (uint32 key, value, trie))

    //
    member this.Iterate (action : int -> 'T -> unit) : unit =
        match this with
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
    member this.IterateBack (action : int -> 'T -> unit) : unit =
        match this with
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
    member this.Fold (folder : 'State -> int -> 'T -> 'State, state : 'State) : 'State =
        match this with
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
    member this.FoldBack (folder : int -> 'T -> 'State -> 'State, state : 'State) : 'State =
        match this with
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
    member this.TryFindKey (predicate : int -> 'T -> bool) : int option =
        match this with
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
    member this.TryPick (picker : int -> 'T -> 'U option) : 'U option =
        match this with
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
    member this.ToSeq () =
        seq {
        match this with
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
            yield! right.ToSeq ()

        | Br (_, _, left, right) ->
            // Recursively visit the children.
            yield! left.ToSeq ()
            yield! right.ToSeq ()
        }

/// <summary>Immutable maps with integer keys.</summary>
/// <typeparam name="T">The type of the values stored in the IntMap.</typeparam>
[<Sealed>]
[<DebuggerTypeProxy(typedefof<IntMapDebuggerProxy<int>>)>]
[<DebuggerDisplay("Count = {Count}")>]
type IntMap< [<EqualityConditionalOn>] 'T> private (trie : PatriciaMap<'T>) =
    /// The empty IntMap.
    static member Empty
        with get () : IntMap<'T> =
            IntMap Empty

    /// The internal representation of the IntMap.
    member private __.Trie
        with get () = trie

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

    /// Returns a new IntMap with the binding added to this IntMap.
    member __.Add (key : int, value : 'T) : IntMap<'T> =
        IntMap (
            PatriciaMap.Insert (uint32 key, value, trie))

    /// Removes an element from the domain of the IntMap.
    /// No exception is raised if the element is not present.
    member __.Remove (key : int) : IntMap<'T> =
        IntMap (
            PatriciaMap.Remove (uint32 key, trie))

    /// Tests if an element is in the domain of the IntMap.
    member __.ContainsKey (key : int) : bool =
        PatriciaMap.ContainsKey (uint32 key, trie)

    /// Returns a new IntMap created by merging the two specified IntMaps.
    member __.Union (otherMap : IntMap<'T>) : IntMap<'T> =
        IntMap (
            PatriciaMap.Union (trie, otherMap.Trie))

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
        trie.ToSeq ()

    //
    member __.ToList () : (int * 'T) list =
        trie.FoldBack ((fun k v list -> (k, v) :: list), [])

    //
    member __.ToArray () : (int * 'T)[] =
        let elements = ResizeArray ()
        trie.Iterate (FuncConvert.FuncFromTupled<_,_,_> elements.Add)
        elements.ToArray ()

    //
    member __.ToMap () : Map<int, 'T> =
        trie.FoldBack (Map.add, Map.empty)

    //
    member internal __.ToKvpArray () : KeyValuePair<int, 'T>[] =
        let elements = ResizeArray (1024)

        trie.Iterate <| fun key value ->
            elements.Add (
                KeyValuePair (key, value))

        elements.ToArray ()

    //
    member __.Iterate (action : int -> 'T -> unit) : unit =
        trie.Iterate action

    //
    member __.IterateBack (action : int -> 'T -> unit) : unit =
        trie.IterateBack action

    //
    member __.Fold (folder : 'State -> int -> 'T -> 'State, state : 'State) : 'State =
        trie.Fold (folder, state)

    //
    member __.FoldBack (folder : int -> 'T -> 'State -> 'State, state : 'State) : 'State =
        trie.FoldBack (folder, state)

    //
    member __.TryFindKey (predicate : int -> 'T -> bool) : int option =
        trie.TryFindKey predicate

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
        trie.TryPick picker

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

    interface System.Collections.IEnumerable with
        /// <inherit />
        member __.GetEnumerator () =
            (trie.ToSeq () |> Seq.map (fun (k, v) ->
                System.Collections.DictionaryEntry (k, v))).GetEnumerator ()
            :> System.Collections.IEnumerator

    interface IEnumerable<KeyValuePair<int, 'T>> with
        /// <inherit />
        member __.GetEnumerator () =
            (trie.ToSeq () |> Seq.map (fun (k, v) ->
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
            notImpl "ICollection`1.Contains"
//            match PatriciaMap.TryFind (uint32 item.Key, trie) with
//            | None ->
//                false
//            | Some value ->
//                LanguagePrimitives.HashCompare.GenericEqualityIntrinsic<'T>(value, item.Value)

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
    /// The empty IntMap.
    [<GeneralizableValue>]
    [<CompiledName("Empty")>]
    let empty<'T> =
        IntMap<'T>.Empty

    /// Is the map empty?
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (map : IntMap<'T>) : bool =
        // Preconditions
        checkNonNull "map" map

        map.IsEmpty

    /// Returns the number of bindings in the IntMap.
    [<CompiledName("Count")>]
    let inline count (map : IntMap<'T>) : int =
        // Preconditions
        checkNonNull "map" map
        
        map.Count

    /// The IntMap containing the given binding.
    [<CompiledName("Singleton")>]
    let inline singleton (key : int) (value : 'T) : IntMap<'T> =
        IntMap.Singleton (key, value)

    /// Tests if an element is in the domain of the IntMap.
    [<CompiledName("ContainsKey")>]
    let inline containsKey (key : int) (map : IntMap<'T>) : bool =
        // Preconditions
        checkNonNull "map" map

        map.ContainsKey key

    /// Look up an element in the IntMap returning a Some value if the
    /// element is in the domain of the IntMap and None if not.
    [<CompiledName("TryFind")>]
    let inline tryFind (key : int) (map : IntMap<'T>) : 'T option =
        // Preconditions
        checkNonNull "map" map
        
        map.TryFind key
        
    /// Look up an element in the IntMap, raising KeyNotFoundException
    /// if no binding exists in the IntMap.
    [<CompiledName("Find")>]
    let inline find (key : int) (map : IntMap<'T>) : 'T =
        // Preconditions
        checkNonNull "map" map

        map.Find key

    //
    [<CompiledName("FindOrDefault")>]
    let inline findOrDefault defaultValue (key : int) (map : IntMap<'T>) =
        defaultArg (map.TryFind key) defaultValue

    //
    [<CompiledName("TryFindKey")>]
    let inline tryFindKey (predicate : int -> 'T -> bool) (map : IntMap<'T>) : int option =
        // Preconditions
        checkNonNull "map" map

        map.TryFindKey predicate

    /// Returns a new IntMap with the binding added to this IntMap.
    [<CompiledName("Add")>]
    let inline add (key : int) (value : 'T) (map : IntMap<'T>) : IntMap<'T> =
        // Preconditions
        checkNonNull "map" map

        map.Add (key, value)

    /// Removes an element from the domain of the IntMap.
    /// No exception is raised if the element is not present.
    [<CompiledName("Remove")>]
    let inline remove (key : int) (map : IntMap<'T>) : IntMap<'T> =
        // Preconditions
        checkNonNull "map" map

        map.Remove key

    /// Returns a new IntMap created by merging the two specified IntMaps.
    [<CompiledName("Union")>]
    let inline union (map1 : IntMap<'T>) (map2 : IntMap<'T>) : IntMap<'T> =
        // Preconditions
        checkNonNull "map1" map1
        checkNonNull "map2" map2

        map1.Union map2

    /// Returns a new IntMap made from the given bindings.
    [<CompiledName("OfSeq")>]
    let inline ofSeq source : IntMap<'T> =
        // Preconditions are checked by the member.
        IntMap.OfSeq source

    /// Returns a new IntMap made from the given bindings.
    [<CompiledName("OfList")>]
    let inline ofList source : IntMap<'T> =
        // Preconditions are checked by the member.
        IntMap.OfList source

    /// Returns a new IntMap made from the given bindings.
    [<CompiledName("OfArray")>]
    let inline ofArray source : IntMap<'T> =
        // Preconditions are checked by the member.
        IntMap.OfArray source

    /// Returns a new IntMap made from the given bindings.
    [<CompiledName("OfMap")>]
    let inline ofMap source : IntMap<'T> =
        // Preconditions are checked by the member.
        IntMap.OfMap source

    //
    [<CompiledName("ToSeq")>]
    let inline toSeq (map : IntMap<'T>) =
        // Preconditions
        checkNonNull "map" map

        map.ToSeq ()

    //
    [<CompiledName("ToList")>]
    let inline toList (map : IntMap<'T>) =
        // Preconditions
        checkNonNull "map" map

        map.ToList ()

    //
    [<CompiledName("ToArray")>]
    let inline toArray (map : IntMap<'T>) =
        // Preconditions
        checkNonNull "map" map

        map.ToArray ()

    //
    [<CompiledName("ToMap")>]
    let inline toMap (map : IntMap<'T>) =
        // Preconditions
        checkNonNull "map" map

        map.ToMap ()

    //
    [<CompiledName("TryPick")>]
    let inline tryPick (picker : int -> 'T -> 'U option) (map : IntMap<'T>) : 'U option =
        // Preconditions
        checkNonNull "map" map

        map.TryPick picker

    //
    [<CompiledName("Pick")>]
    let inline pick (picker : int -> 'T -> 'U option) (map : IntMap<'T>) : 'U =
        // Preconditions
        checkNonNull "map" map

        map.Pick picker

    //
    [<CompiledName("Map")>]
    let inline map (mapping : int -> 'T -> 'U) (map : IntMap<'T>) : IntMap<'U> =
        // Preconditions
        checkNonNull "map" map

        map.Map mapping

    //
    [<CompiledName("Filter")>]
    let inline filter (predicate : int -> 'T -> bool) (map : IntMap<'T>) : IntMap<'T> =
        // Preconditions
        checkNonNull "map" map

        map.Filter predicate

    //
    [<CompiledName("Choose")>]
    let inline choose (chooser : int -> 'T -> 'U option) (map : IntMap<'T>) : IntMap<'U> =
        // Preconditions
        checkNonNull "map" map

        map.Choose chooser

    //
    [<CompiledName("Iter")>]
    let inline iter (action : int -> 'T -> unit) (map : IntMap<'T>) =
        // Preconditions
        checkNonNull "map" map

        map.Iterate action

    //
    [<CompiledName("IterBack")>]
    let inline iterBack (action : int -> 'T -> unit) (map : IntMap<'T>) =
        // Preconditions
        checkNonNull "map" map

        map.IterateBack action

    //
    [<CompiledName("Fold")>]
    let inline fold (folder : 'State -> int -> 'T -> 'State) (state : 'State) (map : IntMap<'T>) =
        // Preconditions
        checkNonNull "map" map

        map.Fold (folder, state)

    //
    [<CompiledName("FoldBack")>]
    let inline foldBack (folder : int -> 'T -> 'State -> 'State) (map : IntMap<'T>) (state : 'State) =
        // Preconditions
        checkNonNull "map" map

        map.FoldBack (folder, state)

    //
    [<CompiledName("Exists")>]
    let inline exists (predicate : int -> 'T -> bool) (map : IntMap<'T>) : bool =
        // Preconditions
        checkNonNull "map" map

        map.Exists predicate

    //
    [<CompiledName("Forall")>]
    let inline forall (predicate : int -> 'T -> bool) (map : IntMap<'T>) : bool =
        // Preconditions
        checkNonNull "map" map

        map.Forall predicate

    //
    [<CompiledName("Partition")>]
    let inline partition (predicate : int -> 'T -> bool) (map : IntMap<'T>) : IntMap<'T> * IntMap<'T> =
        // Preconditions
        checkNonNull "map" map

        map.Partition predicate

    //
    [<CompiledName("MapPartition")>]
    let inline mapPartition (partitioner : int -> 'T -> Choice<'U, 'V>) (map : IntMap<'T>) : IntMap<'U> * IntMap<'V> =
        // Preconditions
        checkNonNull "map" map

        map.MapPartition partitioner


#if PROTO_COMPILER

//
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

    /// The empty TagMap.
    [<GeneralizableValue>]
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

    /// Returns the number of bindings in the TagMap.
    [<CompiledName("Count")>]
    let inline count (map : TagMap<'Tag, 'T>) : int =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map
        
        map.Count

    /// The TagMap containing the given binding.
    [<CompiledName("Singleton")>]
    let inline singleton (key : int<'Tag>) (value : 'T) : TagMap<'Tag, 'T> =
        IntMap.Singleton (int key, value)
        |> retype

    /// Look up an element in the TagMap returning a Some value if the
    /// element is in the domain of the TagMap and None if not.
    [<CompiledName("TryFind")>]
    let inline tryFind (key : int<'Tag>) (map : TagMap<'Tag, 'T>) : 'T option =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map
        
        map.TryFind (int key)
        
    //
    [<CompiledName("Find")>]
    let inline find (key : int<'Tag>) (map : TagMap<'Tag, 'T>) : 'T =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Find (int key)

    //
    [<CompiledName("Add")>]
    let inline add (key : int<'Tag>) (value : 'T) (map : TagMap<'Tag, 'T>) : TagMap<'Tag, 'T> =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Add (int key, value)
        |> retype

    //
    [<CompiledName("Remove")>]
    let inline remove (key : int<'Tag>) (map : TagMap<'Tag, 'T>) : TagMap<'Tag, 'T> =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Remove (int key)
        |> retype

    //
    [<CompiledName("ContainsKey")>]
    let inline containsKey (key : int<'Tag>) (map : TagMap<'Tag, 'T>) : bool =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.ContainsKey (int key)
        |> retype

    //
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

    //
    [<CompiledName("OfSeq")>]
    let inline ofSeq (source : seq<int<'Tag> * 'T>) : TagMap<'Tag, 'T> =
        // Preconditions are checked by the member.
        IntMap.OfSeq (retype source)
        |> retype

    //
    [<CompiledName("OfList")>]
    let inline ofList (source : (int<'Tag> * 'T) list) : TagMap<'Tag, 'T> =
        // Preconditions are checked by the member.
        IntMap.OfList (retype source)
        |> retype

    //
    [<CompiledName("OfArray")>]
    let inline ofArray (source : (int<'Tag> * 'T)[]) : TagMap<'Tag, 'T> =
        // Preconditions are checked by the member.
        IntMap.OfArray (retype source)
        |> retype

    //
    [<CompiledName("OfMap")>]
    let inline ofMap (source : Map<int<'Tag>, 'T>) : TagMap<'Tag, 'T> =
        // Preconditions are checked by the member.
        IntMap.OfMap (retype source)
        |> retype

    //
    [<CompiledName("ToSeq")>]
    let inline toSeq (map : TagMap<'Tag, 'T>) : seq<int<'Tag> * 'T> =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.ToSeq ()
        |> retype

    //
    [<CompiledName("ToList")>]
    let inline toList (map : TagMap<'Tag, 'T>) : (int<'Tag> * 'T) list =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.ToList ()
        |> retype

    //
    [<CompiledName("ToArray")>]
    let inline toArray (map : TagMap<'Tag, 'T>) : (int<'Tag> * 'T)[] =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.ToArray ()
        |> retype

    //
    [<CompiledName("ToMap")>]
    let inline toMap (map : TagMap<'Tag, 'T>) : Map<int<'Tag>, 'T> =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.ToMap ()
        |> retype

    //
    [<CompiledName("Iter")>]
    let inline iter (action : int<'Tag> -> 'T -> unit) (map : TagMap<'Tag, 'T>) : unit =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Iterate (retype action)

    //
    [<CompiledName("IterBack")>]
    let inline iterBack (action : int<'Tag> -> 'T -> unit) (map : TagMap<'Tag, 'T>) : unit =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.IterateBack (retype action)

    //
    [<CompiledName("Fold")>]
    let inline fold (folder : 'State -> int<'Tag> -> 'T -> 'State) (state : 'State) (map : TagMap<'Tag, 'T>) : 'State =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Fold (retype folder, state)

    //
    [<CompiledName("FoldBack")>]
    let inline foldBack (folder : int<'Tag> -> 'T -> 'State -> 'State) (map : TagMap<'Tag, 'T>) (state : 'State) : 'State =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.FoldBack (retype folder, state)

    //
    [<CompiledName("Choose")>]
    let inline choose (chooser : int<'Tag> -> 'T -> 'U option) (map : TagMap<'Tag, 'T>) : TagMap<'Tag, 'U> =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Choose (retype chooser)
        |> retype

    //
    [<CompiledName("Filter")>]
    let inline filter (predicate : int<'Tag> -> 'T -> bool) (map : TagMap<'Tag, 'T>) : TagMap<'Tag, 'U> =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Filter (retype predicate)
        |> retype

    //
    [<CompiledName("Map")>]
    let inline map (mapping : int<'Tag> -> 'T -> 'U) (map : TagMap<'Tag, 'T>) : TagMap<'Tag, 'U> =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Map (retype mapping)
        |> retype

    //
    [<CompiledName("Partition")>]
    let inline partition (predicate : int<'Tag> -> 'T -> bool) (map : TagMap<'Tag, 'T>) : TagMap<'Tag, 'T> * TagMap<'Tag, 'T> =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        let map1, map2 = map.Partition (retype predicate)
        (retype map1), (retype map2)

    //
    [<CompiledName("MapPartition")>]
    let inline mapPartition (partitioner : int<'Tag> -> 'T -> Choice<'U, 'V>) (map : TagMap<'Tag, 'T>) : TagMap<'Tag, 'U> * TagMap<'Tag, 'V> =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        let map1, map2 = map.MapPartition (retype partitioner)
        (retype map1), (retype map2)

    //
    [<CompiledName("TryFindKey")>]
    let inline tryFindKey (predicate : int<'Tag> -> 'T -> bool) (map : TagMap<'Tag, 'T>) : int<'Tag> option =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.TryFindKey (retype predicate)
        |> retype

    //
    [<CompiledName("Exists")>]
    let inline exists (predicate : int<'Tag> -> 'T -> bool) (map : TagMap<'Tag, 'T>) : bool =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Exists (retype predicate)

    //
    [<CompiledName("Forall")>]
    let inline forall (predicate : int<'Tag> -> 'T -> bool) (map : TagMap<'Tag, 'T>) : bool =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Forall (retype predicate)

    //
    [<CompiledName("TryPick")>]
    let inline tryPick (picker : int<'Tag> -> 'T -> 'U option) (map : TagMap<'Tag, 'T>) : 'U option =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.TryPick (retype picker)

    //
    [<CompiledName("Pick")>]
    let inline pick (picker : int<'Tag> -> 'T -> 'U option) (map : TagMap<'Tag, 'T>) : 'U =
        // Retype as IntMap.
        let map : IntMap<'T> = retype map

        // Preconditions
        checkNonNull "map" map

        map.Pick (retype picker)

#endif

