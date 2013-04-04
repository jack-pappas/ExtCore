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

/// Additional functional operators on sets.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExtCore.Collections.Set

open LanguagePrimitives
open OptimizedClosures
open ExtCore


/// Creates a set with the same elements as the ArrayView.
[<CompiledName("OfArrayView")>]
let ofArrayView (view : ArrayView<'T>) : Set<'T> =
    (Set.empty, view)
    ||> ArrayView.fold (fun set el ->
        Set.add el set)

/// Applies a function to each element of the set, threading an accumulator argument
/// through the computation. The integer index passed to the function indicates the
/// index of the element within the set.
[<CompiledName("FoldIndexed")>]
let foldi (folder : 'State -> int -> 'T -> 'State) (state : 'State) (set : Set<'T>) : 'State =
    // Preconditions
    checkNonNull "set" set

    let folder = FSharpFunc<_,_,_,_>.Adapt folder
    let mutable state = state
    let mutable idx = 0
    for el in set do
        state <- folder.Invoke (state, idx, el)
        idx <- idx + 1
    state

/// Applies a function to each element of the set, returning the results in an array.
[<CompiledName("MapToArray")>]
let mapToArray (mapping : 'T -> 'U) (set : Set<'T>) : 'U[] =
    // Preconditions
    checkNonNull "set" set

    let results = Array.zeroCreate <| Set.count set
    let mutable idx = 0
    for el in set do
        results.[idx] <- mapping el
        idx <- idx + 1
    results

/// Creates a set given a number of items in the set and a generator function.
[<CompiledName("Init")>]
let init count (initializer : int -> 'T) : Set<'T> =
    // Preconditions
    if count < 0 then
        invalidArg "count" "The number of items cannot be negative."

    // Optimize the empty set case.
    if count = 0 then
        Set.empty
    else
        let mutable result = Set.empty
        for i = 0 to (count - 1) do
            result <- Set.add (initializer i) result
        result

/// Extracts the minimum element from a set, returning the element
/// along with the updated set.
[<CompiledName("TryExtractMin")>]
let tryExtractMin (set : Set<'T>) : 'T option * Set<'T> =
    // Preconditions
    checkNonNull "set" set

    if Set.isEmpty set then
        None, set
    else
        let minElement = Set.minElement set
        let set = Set.remove minElement set
        Some minElement, set

/// Extracts the maximum element from a set, returning the element
/// along with the updated set.
[<CompiledName("TryExtractMax")>]
let tryExtractMax (set : Set<'T>) : 'T option * Set<'T> =
    // Preconditions
    checkNonNull "set" set

    if Set.isEmpty set then
        None, set
    else
        let maxElement = Set.maxElement set
        let set = Set.remove maxElement set
        Some maxElement, set

/// Extracts the minimum element from a set, returning the element
/// along with the updated set.
[<CompiledName("ExtractMinimum")>]
let extractMin (set : Set<'T>) : 'T * Set<'T> =
    // Preconditions
    checkNonNull "set" set
    if Set.isEmpty set then
        invalidArg "set" "The set is empty."

    let minElement = Set.minElement set
    let set = Set.remove minElement set
    minElement, set

/// Extracts the maximum element from a set, returning the element
/// along with the updated set.
[<CompiledName("ExtractMaximum")>]
let extractMax (set : Set<'T>) : 'T * Set<'T> =
    // Preconditions
    checkNonNull "set" set
    if Set.isEmpty set then
        invalidArg "set" "The set is empty."

    let maxElement = Set.maxElement set
    let set = Set.remove maxElement set
    maxElement, set

/// Reduces elements in a set in order from the least (minimum) element
/// to the greatest (maximum) element.
[<CompiledName("Reduce")>]
let reduce (reduction : 'T -> 'T -> 'T) (set : Set<'T>) : 'T =
    // Preconditions
    checkNonNull "set" set
    if Set.isEmpty set then
        invalidArg "set" "The set is empty."

    let minElement, set = extractMin set
    Set.fold reduction minElement set

/// Reduces elements in a set in order from the greatest (maximum) element
/// to the least (minimum) element.
[<CompiledName("ReduceBack")>]
let reduceBack (reduction : 'T -> 'T -> 'T) (set : Set<'T>) : 'T =
    // Preconditions
    checkNonNull "set" set
    if Set.isEmpty set then
        invalidArg "set" "The set is empty."

    let maxElement, set = extractMax set
    Set.foldBack reduction set maxElement

/// <summary>
/// Applies the given function to each element of the set.
/// Returns the set comprised of the results <c>x</c> for each element where the
/// function returns <c>Some(x)</c>.
/// </summary>
[<CompiledName("Choose")>]
let choose (chooser : 'T -> 'U option) (set : Set<'T>) : Set<'U> =
    // Preconditions
    checkNonNull "set" set

    // OPTIMIZATION : If the input set is empty return immediately.
    if Set.isEmpty set then
        Set.empty
    else
        (Set.empty, set)
        ||> Set.fold (fun chosen el ->
            match chooser el with
            | None ->
                chosen
            | Some result ->
                Set.add result chosen)

/// Applies the given function to each element of the set, returning the first result
/// where the function returns <c>Some(x)</c>. If the function never returns <c>Some(x)</c>
/// then None is returned.
[<CompiledName("TryPick")>]
let tryPick (picker : 'T -> 'U option) (set : Set<'T>) : 'U option =
    // Preconditions
    checkNonNull "set" set

    // OPTIMIZATION : If the input set is empty return immediately.
    if Set.isEmpty set then None
    else
        // Use an iterator over the set here since we don't
        // have access to the internal structure of F# Sets.
        Set.toSeq set
        |> Seq.tryPick picker

/// Applies the given function to each element of the set, returning the first result
/// where the function returns <c>Some(x)</c>. If the function never returns <c>Some(x)</c>
/// then a KeyNotFoundException is raised.
[<CompiledName("Pick")>]
let pick (picker : 'T -> 'U option) (set : Set<'T>) : 'U =
    // Preconditions
    checkNonNull "set" set

    // Use tryPick to find a matching element and
    // raise an exception if it can't.
    match tryPick picker set with
    | Some result ->
        result
    | None ->
        // TODO : Return a better error message
        //keyNotFound ""
        raise <| System.Collections.Generic.KeyNotFoundException ()

/// Returns the first (least) element for which the given predicate returns &quot;true&quot;.
/// Returns None if no such element exists.
[<CompiledName("TryFind")>]
let tryFind (predicate : 'T -> bool) (set : Set<'T>) : 'T option =
    // Preconditions
    checkNonNull "set" set

    // OPTIMIZATION : If the input set is empty return immediately.
    if Set.isEmpty set then None
    else
        // Use an iterator over the set here since we don't
        // have access to the internal structure of F# Sets.
        Set.toSeq set
        |> Seq.tryFind predicate

/// Returns the first (least) element for which the given predicate returns &quot;true&quot;.
/// Raise KeyNotFoundException if no such element exists.
[<CompiledName("Find")>]
let find (predicate : 'T -> bool) (set : Set<'T>) : 'T =
    // Preconditions
    checkNonNull "set" set

    // Use tryFind to find a matching element and
    // raise an exception if it can't.
    match tryFind predicate set with
    | Some result ->
        result
    | None ->
        // TODO : Return a better error message
        //keyNotFound ""
        raise <| System.Collections.Generic.KeyNotFoundException ()

/// Splits the collection into two (2) collections, containing the elements for which the given
/// function returns Choice1Of2 or Choice2Of2, respectively. This function is similar to
/// Set.partition, but it allows the returned collections to have different types.
[<CompiledName("MapPartition")>]
let mapPartition (partitioner : 'T -> Choice<'U, 'V>) set : Set<'U> * Set<'V> =
    // Preconditions
    checkNonNull "set" set

    // OPTIMIZATION : If the set is empty return immediately.
    if Set.isEmpty set then
        Set.empty, Set.empty
    else
        let mutable resultSet1 = Set.empty
        let mutable resultSet2 = Set.empty

        for el in set do
            match partitioner el with
            | Choice1Of2 value ->
                resultSet1 <- Set.add value resultSet1
            | Choice2Of2 value ->
                resultSet2 <- Set.add value resultSet2

        resultSet1, resultSet2

/// <summary>Computes the symmetric difference of two sets; that is, the set of elements
/// which are in either of the sets and not in their intersection.</summary>
/// <remarks>
/// The symmetric difference is similar to the XOR ("exclusive-or") operation, except that
/// XOR returns 'true' when an element is not in both sets. This requires the "universe"
/// (set of all possible domain elements) to be known. The symmetric difference operation
/// does not have this requirement and so can be used when the universe is not known or infinite.
/// </remarks>
[<CompiledName("SymmetricDifference")>]
let symmetricDifference (set1 : Set<'T>) (set2 : Set<'T>) : Set<'T> =
    // Preconditions
    checkNonNull "set1" set1
    checkNonNull "set2" set2

    // OPTIMIZATION : If either set is empty return immediately.
    if Set.isEmpty set1 then
        set2
    elif Set.isEmpty set2 then
        set1
    else
        // OPTIMIZE : Re-implement this in a way which traverses both
        // sets linearly to avoid using multiple slower operations.

        // Union the sets, then remove the common elements to compute the symmetric difference.
        Set.intersect set1 set2
        |> Set.difference (Set.union set1 set2)

/// The Cartesian product of two sets.
[<CompiledName("Cartesian")>]
let cartesian (set1 : Set<'T>) (set2 : Set<'U>) : Set<'T * 'U> =
    // Preconditions
    checkNonNull "set1" set1
    checkNonNull "set2" set2

    // OPTIMIZATION : If either set is empty return immediately.
    if Set.isEmpty set1 || Set.isEmpty set2 then
        Set.empty
    else
        (Set.empty, set1)
        ||> Set.fold (fun product x ->
            (product, set2)
            ||> Set.fold (fun product y ->
                Set.add (x, y) product))

/// For each element of the set, applies the given function to produce a set of results.
/// Computes the union of all result sets and returns the combined set.
[<CompiledName("Collect")>]
let collect (mapping : 'T -> Set<'U>) set : Set<'U> =
    // Preconditions
    checkNonNull "set" set

    // OPTIMIZATION : If the set is empty return immediately.
    if Set.isEmpty set then
        Set.empty
    else
        (Set.empty, set)
        ||> Set.fold (fun union el ->
            mapping el
            |> Set.union union)

/// For each element of the set, applies the given function to produce a set of results.
/// Computes the intersection of all result sets and returns the combined set.
[<CompiledName("Condense")>]
let condense (mapping : 'T -> Set<'U>) set : Set<'U> =
    // Preconditions
    checkNonNull "set" set
    
    // If the set is empty, return immediately.
    // This avoids throwing an exception when calling 'extractMin' below.
    if Set.isEmpty set then
        Set.empty
    else
        // To compute the intersection, we must use a map-reduction instead of a fold;
        // if we used a fold with Set.empty as the initial state, the result would
        // always be an empty set.
        let minElement, set = extractMin set
        let mappedMinElement = mapping minElement

        (mappedMinElement, set)
        ||> Set.fold (fun intersection el ->
            mapping el
            |> Set.intersect intersection)

/// Determines if two sets are disjoint, i.e., whether they have no elements in common.
[<CompiledName("Disjoint")>]
let disjoint (set1 : Set<'T>) set2 : bool =
    // Preconditions
    checkNonNull "set1" set1
    checkNonNull "set2" set2

    // OPTIMIZATION : If either set is empty, return immediately.
    if Set.isEmpty set1 || Set.isEmpty set2 then true
    else
        (set1 |> Set.exists (fun el -> Set.contains el set2) ||
         set2 |> Set.exists (fun el -> Set.contains el set1))
        |> not


/// Functions operating over the Cartesian product of two sets.
/// These functions can offer a large memory savings since they avoid creating the product set.
[<RequireQualifiedAccess>]
module Cartesian =
    /// Applies a function to each element in the Cartesian product of two sets,
    /// threading an accumulator argument through the computation.
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> 'U -> 'State) state set1 set2 : 'State =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        let folder = FSharpFunc<_,_,_,_>.Adapt folder
            
        (state, set1)
        ||> Set.fold (fun state x ->
            (state, set2)
            ||> Set.fold (fun state y ->
                folder.Invoke (state, x, y)))

    /// Applies a function to each element in the Cartesian product of two sets,
    /// threading an accumulator argument through the computation.
    [<CompiledName("FoldBack")>]
    let foldBack (folder : 'T -> 'U -> 'State -> 'State) set1 set2 state : 'State =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        (set1, state)
        ||> Set.foldBack (fun x state ->
            (set2, state)
            ||> Set.foldBack (fun y state ->
                folder.Invoke (x, y, state)))

    /// Applies the given function to each element in the Cartesian product of two sets.
    [<CompiledName("Iter")>]
    let iter (action : 'T -> 'U -> unit) set1 set2 : unit =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        let action = FSharpFunc<_,_,_>.Adapt action

        set1
        |> Set.iter (fun x ->
            set2
            |> Set.iter (fun y ->
                action.Invoke (x, y)))

    /// Builds a new collection whose elements are the results of applying the given function
    /// to the elements in the Cartesian product of two sets.
    [<CompiledName("Map")>]
    let map (mapping : 'T1 -> 'T2 -> 'U) set1 set2 : Set<'U> =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        let mapping = FSharpFunc<_,_,_>.Adapt mapping
            
        (Set.empty, set1)
        ||> Set.fold (fun mappedSet x ->
            (mappedSet, set2)
            ||> Set.fold (fun mappedSet y ->
                let mapped = mapping.Invoke (x, y)
                Set.add mapped mappedSet))

    /// <summary>
    /// Applies the given function to each element in the Cartesian product of two sets. 
    /// Returns the set comprised of the results <c>x</c> for each element where the
    /// function returns <c>Some(x)</c>.
    /// </summary>
    [<CompiledName("Choose")>]
    let choose (chooser : 'T1 -> 'T2 -> 'U option) set1 set2 : Set<'U> =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        let chooser = FSharpFunc<_,_,_>.Adapt chooser
            
        (Set.empty, set1)
        ||> Set.fold (fun mappedSet x ->
            (mappedSet, set2)
            ||> Set.fold (fun mappedSet y ->
                match chooser.Invoke (x, y) with
                | None ->
                    mappedSet
                | Some value ->
                    Set.add value mappedSet))
