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

/// Additional functional operators on immutable lists.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExtCore.Collections.List

open LanguagePrimitives
open OptimizedClosures
open ExtCore


/// A curried "cons" operator.
[<CompiledName("Cons")>]
let inline cons (list : 'T list) value =
    value :: list

/// A curried "optional-cons" operator.
[<CompiledName("ConsOption")>]
let inline optcons (list : 'T list) value =
    match value with
    | None -> list
    | Some x -> x :: list

/// Attempt to retrieve the first element of the list.
[<CompiledName("TryHead")>]
let inline tryHead (list : 'T list) =
    match list with
    | [] -> None
    | hd :: _ ->
        Some hd

/// Builds a list from the given option value.
[<CompiledName("OfOption")>]
let inline ofOption (value : 'T option) =
    match value with
    | None -> []
    | Some x -> [x]

/// Create a list containing the given value.
[<CompiledName("Singleton")>]
let inline singleton (value : 'T) =
    [value]

/// Builds a list that contains the elements of the set in order.
[<CompiledName("OfSet")>]
let inline ofSet (set : Set<'T>) =
    Set.toList set

/// Builds a set that contains the same elements as the given list.
[<CompiledName("ToSet")>]
let inline toSet (list : 'T list) =
    Set.ofList list

/// Determines if a given value is contained in a list.
[<CompiledName("Contains")>]
let rec contains (value : 'T) (set : 'T list) : bool =
    match set with
    | [] -> false
    | el :: set ->
        el = value || contains value set

/// Returns the last element of a list.
[<CompiledName("Last")>]
let rec last (list : 'T list) =
    match list with
    | [] ->
        invalidArg "list" "Cannot get the last element of an empty list."
    | [x] -> x
    | _ :: list ->
        last list

/// Drops the last element of a list, returning the remaining list.
[<CompiledName("DropLast")>]
let dropLast (list : 'T list) : 'T list =
    match list with
    | [] ->
        invalidArg "list" "The list is empty."
    | [_] ->
        []  // Fast path for single-element lists.
    | _ ->
        list
        |> List.rev
        |> List.tail
        |> List.rev

/// Creates a new list by combining each element of the list with it's index.
[<CompiledName("Indexed")>]
let indexed (list : 'T list) =
    // Preconditions
    checkNonNull "list" list

    list |> List.mapi (fun i x -> i, x)

/// Applies a function to each element of the array, returning a new array whose elements are
/// tuples of the original element and the function result for that element.
[<CompiledName("ProjectValues")>]
let projectValues (projection : 'Key -> 'T) (list : 'Key list) =
    // Preconditions
    checkNonNull "list" list

    list |> List.map (fun x -> x, projection x)

/// Applies a function to each element of the array, returning a new array whose elements are
/// tuples of the original element and the function result for that element.
[<CompiledName("ProjectKeys")>]
let projectKeys (projection : 'T -> 'Key) (list : 'T list) =
    // Preconditions
    checkNonNull "list" list

    list |> List.map (fun x -> projection x, x)

/// Converts a list into an array (similar to List.toArray) but copies the elements into
/// the array from right-to-left, so there's no need to call List.rev before List.toArray.
[<CompiledName("ReverseIntoArray")>]
let revIntoArray (list : 'T list) =
    // Preconditions
    checkNonNull "list" list

    let len = List.length list
    let results = Array.zeroCreate len

    let rec loop idx lst =
        match lst with
        | [] -> results
        | hd :: tl ->
            results.[idx] <- hd
            loop (idx - 1) tl

    loop (len - 1) list

/// <summary>Applies the given function to each element of a list,
/// and copies the results into an array from right-to-left so the
/// produced array represents the mapped original list in reverse order.</summary>
/// <remarks><para>This represents an optimized version of:
/// <c>fun mapping -> (List.map mapping) >> List.rev >> List.toArray</c>.</para></remarks>
[<CompiledName("ReverseAndMapIntoArray")>]
let revMapIntoArray (mapping : 'T -> 'U) (list : 'T list) =
    // Preconditions
    checkNonNull "list" list

    let len = List.length list
    let results = Array.zeroCreate len

    let rec loop idx lst =
        match lst with
        | [] -> results
        | hd :: tl ->
            results.[idx] <- mapping hd
            loop (idx - 1) tl

    loop (len - 1) list

/// <summary>
/// Applies the given function to each element of the list.
/// Returns the list comprised of the results <c>x</c> for each element where
/// the function returns <c>Some(x)</c>. The integer index passed to the function
/// indicates the index of the element being transformed.
/// </summary>
[<CompiledName("ChooseIndexed")>]
let choosei (chooser : int -> 'T -> 'U option) list : 'U list =
    // Preconditions
    checkNonNull "list" list
    
    let chooser = FSharpFunc<_,_,_>.Adapt chooser

    let rec choosei chosen index list =
        match list with
        | [] ->
            // Reverse the list of chosen elements before returning it.
            List.rev chosen
        | hd :: tl ->
            // Invoke the chooser function; if it returns Some, cons the result onto the results list.
            let chosen =
                match chooser.Invoke (index, hd) with
                | None ->
                    chosen
                | Some result ->
                    result :: chosen

            // Process the rest of the list elements.
            choosei chosen (index + 1) tl

    // Process the list.
    choosei [] 0 list

/// <summary>
/// Applies the given function pairwise to the two lists.
/// Returns the list comprised of the results <c>x</c> for each element where
/// the function returns <c>Some(x)</c>.
/// </summary>
[<CompiledName("Choose2")>]
let choose2 (chooser : 'T1 -> 'T2 -> 'U option) list1 list2 : 'U list =
    // Preconditions
    checkNonNull "list1" list1
    checkNonNull "list2" list2
    
    let chooser = FSharpFunc<_,_,_>.Adapt chooser

    let rec choose2 chosen list1 list2 =
        match list1, list2 with
        | [], [] ->
            // Reverse the list of chosen elements before returning it.
            List.rev chosen
        | [], _ ->
            // The lists have unequal lengths.
            invalidArg "list2" "The lists have different lengths. 'list2' is longer than 'list1'."
        | _, [] ->
            // The lists have unequal lengths.
            invalidArg "list2" "The lists have different lengths. 'list2' is shorter than 'list1'."
        | hd1 :: tl1, hd2 :: tl2 ->
            // Invoke the chooser function; if it returns Some, cons the result onto the results list.
            let chosen =
                match chooser.Invoke (hd1, hd2) with
                | None ->
                    chosen
                | Some result ->
                    result :: chosen

            // Process the rest of the list elements.
            choose2 chosen tl1 tl2

    // Process the lists recursively.
    choose2 [] list1 list2

/// Takes a specified number of items from a list, returning them (as a new list) along with the remaining list.
[<CompiledName("Take")>]
let take count (list : 'T list) =
    // Preconditions
    checkNonNull "list" list
    if count < 0 then
        invalidArg "count" "The number of items to take from the list is negative."
    elif count > List.length list then
        invalidArg "count" "The number of items to take from the list is greater than the length of the list."

    // OPTIMIZATION : If count = 0 return immediately.
    if count = 0 then
        [], list
    else
        /// The result list.
        let mutable taken = []
        let mutable list = list
            
        // Take the elements from the input list and cons them onto the 'taken' list.
        for i = 0 to count - 1 do
            taken <- (List.head list) :: taken
            list <- List.tail list

        // Return the 'taken' list and the remaining part of the list.
        // Reverse the 'taken' list so it's in the correct order.
        List.rev taken, list

/// Takes a specified number of items from a list, returning them (in an array) along with the remaining list.
[<CompiledName("TakeArray")>]
let takeArray count (list : 'T list) =
    // Preconditions
    checkNonNull "list" list
    if count < 0 then
        invalidArg "count" "The number of items to take from the list is negative."
    elif count > List.length list then
        invalidArg "count" "The number of items to take from the list is greater than the length of the list."

    // OPTIMIZATION : If count = 0 return immediately.
    if count = 0 then
        Array.empty, list
    else
        /// The result array.
        let takenElements = Array.zeroCreate count

        let mutable list = list
            
        // Take the elements from the list and store them in the array.
        for i = 0 to count - 1 do
            takenElements.[i] <- List.head list
            list <- List.tail list

        // Return the taken elements and the remaining part of the list.
        takenElements, list

/// Applies a function to each element of the collection and the element which
/// follows it, threading an accumulator argument through the computation.
[<CompiledName("FoldPairwise")>]
let foldPairwise (folder : 'State -> 'T -> 'T -> 'State) state list =
    // Preconditions
    checkNonNull "list" list
    
    // OPTIMIZATION : If the list is empty or contains just one element,
    // immediately return the input state.
    match list with
    | []
    | [_] ->
        state
    | hd :: tl ->
        // OPTIMIZATION : Imperative-style implementation for maximum performance.            
        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        let mutable previousElement = hd
        let mutable list = tl
        let mutable state = state

        while not <| List.isEmpty list do
            let currentElement = List.head list
            state <- folder.Invoke (state, previousElement, currentElement)
            previousElement <- currentElement
            list <- List.tail list

        // Return the final state value
        state

/// Applies a function to each element of the collection and the element which
/// proceeds it, threading an accumulator argument through the computation.
[<CompiledName("FoldBackPairwise")>]
let foldBackPairwise (folder : 'T -> 'T -> 'State -> 'State) list state =
    // Preconditions
    checkNonNull "list" list

    // OPTIMIZATION : If the list is empty or contains just one element,
    // immediately return the input state.
    match list with
    | []
    | [_] ->
        state
    | list ->
        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        // OPTIMIZATION : To fold backwards over a single-linked list, we normally need to traverse
        // it and create a reversed copy -- this means O(n) time and memory complexity.
        // Here, we squeeze out a bit more performance by using an array to hold the reversed list
        // so we benefit from memory locality.
        let reversed = revIntoArray list
        let mutable state = state
            
        let len = Array.length reversed
        for i = 1 to len - 1 do
            state <- folder.Invoke (reversed.[i - 1], reversed.[i], state)

        // Return the final state value
        state

/// Splits the collection into two (2) collections, containing the elements for which the given
/// function returns Choice1Of2 or Choice2Of2, respectively. This function is similar to
/// List.partition, but it allows the returned collections to have different types.
[<CompiledName("MapPartition")>]
let mapPartition (partitioner : 'T -> Choice<'U1, 'U2>) list : 'U1 list * 'U2 list =
    // Preconditions
    checkNonNull "list" list

    // OPTIMIZATION : If the input list is empty, immediately return empty results.
    if List.isEmpty list then
        [], []
    else
        // Mutable variables are used here instead of List.fold for maximum performance.
        let mutable list = list
        let mutable resultList1 = []
        let mutable resultList2 = []

        // Partition the list, consing the elements onto the list
        // specified by the partition function.
        while not <| List.isEmpty list do
            match partitioner <| List.head list with
            | Choice1Of2 element ->
                resultList1 <- element :: resultList1
            | Choice2Of2 element ->
                resultList2 <- element :: resultList2

            // Remove the first element from the input list.
            list <- List.tail list

        // Reverse the result lists and return them.
        List.rev resultList1,
        List.rev resultList2

/// Splits the collection into two (3) collections, containing the elements for which the given
/// function returns Choice1Of3, Choice2Of3, or Choice3Of3, respectively. This function is similar
/// to List.partition, but it allows the returned collections to have different types.
[<CompiledName("MapPartition3")>]
let mapPartition3 (partitioner : 'T -> Choice<'U1, 'U2, 'U3>) list : 'U1 list * 'U2 list * 'U3 list =
    // Preconditions
    checkNonNull "list" list

    // OPTIMIZATION : If the input list is empty, immediately return empty results.
    if List.isEmpty list then
        [], [], []
    else
        // Mutable variables are used here instead of List.fold for maximum performance.
        let mutable list = list
        let mutable resultList1 = []
        let mutable resultList2 = []
        let mutable resultList3 = []

        // Partition the list, consing the elements onto the list
        // specified by the partition function.
        while not <| List.isEmpty list do
            match partitioner <| List.head list with
            | Choice1Of3 element ->
                resultList1 <- element :: resultList1
            | Choice2Of3 element ->
                resultList2 <- element :: resultList2
            | Choice3Of3 element ->
                resultList3 <- element :: resultList3

            // Remove the first element from the input list.
            list <- List.tail list

        // Reverse the result lists and return them.
        List.rev resultList1,
        List.rev resultList2,
        List.rev resultList3

/// Returns a list containing the elements generated by the given computation.
/// The given initial state argument is passed to the element generator, which is applied
/// repeatedly until a None value is returned. Each call to the element generator returns
/// a new residual state.
[<CompiledName("Unfold")>]
let unfold (generator : 'State -> ('T * 'State) option) (state : 'State) : 'T list =
    let mutable resultList = []
    let mutable state = state
    let mutable finished = false

    // Generate elements and cons them onto the result list.
    while not finished do
        match generator state with
        | Some (result, state') ->
            resultList <- result :: resultList
            state <- state'
        | None ->
            finished <- true

    // Reverse the result list before returning.
    List.rev resultList

/// Applies the given function to each element of the list and creates two (2) lists
/// from the components of the returned tuple.
[<CompiledName("UnzipWith")>]
let unzipWith (mapping : 'T -> 'U * 'V) list : 'U list * 'V list =
    // Preconditions
    checkNonNull "list" list

    // OPTIMIZATION : If the input list is empty return immediately.
    if List.isEmpty list then
        [], []
    else
        let mutable list = list
        let mutable resultList1 = []
        let mutable resultList2 = []

        while not <| List.isEmpty list do
            let result1, result2 =
                mapping <| List.head list
            list <- List.tail list
            resultList1 <- result1 :: resultList1
            resultList2 <- result2 :: resultList2

        // Reverse the result lists before returning.
        List.rev resultList1,
        List.rev resultList2

/// Returns the number of list elements matching a given predicate.
// List.countWith predicate list = (List.filter predicate list |> List.length)
[<CompiledName("CountWith")>]
let countWith (predicate : 'T -> bool) (list : 'T list) : int =
    // Preconditions
    checkNonNull "list" list

    let rec count acc list =
        match list with
        | [] -> acc
        | hd :: tl ->
            // TODO : Should we use checked addition here? It is unlikely that anyone
            // will have a list with more than Int32.MaxValue elements, but maybe we should
            // use checked addition just in case...
            let acc = if predicate hd then acc + 1 else acc
            count acc tl

    // Call the recursive implementation to compute the number of matching elements.
    count 0 list
