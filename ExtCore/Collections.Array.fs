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

/// Additional functional operators on arrays.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExtCore.Collections.Array

open LanguagePrimitives
open OptimizedClosures
open ExtCore


#if PROTO_COMPILER
// TODO : Re-implement this using inline IL; it should simply provide a way
// to emit an 'ldlen' instruction given an array.
/// Returns the length of the array as an unsigned integer.
[<CompiledName("RawLength")>]
let inline rawLength (arr : 'T[]) : unativeint =
    unativeint arr.Length
#endif

/// Given an element, creates an array containing just that element.
[<CompiledName("Singleton")>]
let inline singleton (value : 'T) =
    [| value |]

/// Wraps the array in a ReadOnlyCollection<'T> to only allow
/// read access while still providing O(n) lookup.
[<CompiledName("Readonly")>]
let inline readonly (arr : 'T[]) =
    System.Array.AsReadOnly arr

/// Builds an array that contains the elements of the set in order.
[<CompiledName("OfSet")>]
let inline ofSet (set : Set<'T>) : 'T[] =
    Set.toArray set

/// Builds a set that contains the same elements as the given array.
[<CompiledName("ToSet")>]
let inline toSet (array : 'T[]) : Set<'T> =
    Set.ofArray array

/// Applies a function to each element of the array, returning a new array whose elements are
/// tuples of the original element and the function result for that element.
[<CompiledName("ProjectValues")>]
let projectValues (projection : 'Key -> 'U) (array : 'Key[]) =
    // Preconditions
    checkNonNull "array" array

    array |> Array.map (fun x -> x, projection x)

//
[<CompiledName("ProjectKeys")>]
let projectKeys (projection : 'T -> 'Key) (array : 'T[]) =
    // Preconditions
    checkNonNull "array" array

    array |> Array.map (fun x -> projection x, x)

/// Returns the first element in the array.
[<CompiledName("First")>]
let inline first (array : 'T[]) =
    if Array.isEmpty array then
        invalidOp "Cannot retrieve the first element of an empty array."
    else array.[0]

/// Returns the index of the last element in the array.
[<CompiledName("LastIndex")>]
let inline lastIndex (array : 'T[]) =
    if Array.isEmpty array then
        invalidOp "The array is empty."
    else array.Length - 1

/// Returns the last element in the array.
[<CompiledName("Last")>]
let inline last (array : 'T[]) =
    if Array.isEmpty array then
        invalidOp "Cannot retrieve the last element of an empty array."
    else array.[array.Length - 1]

/// Determines if an array contains a specified value.
[<CompiledName("Contains")>]
let contains value (array : 'T[]) : bool =
    // Preconditions
    checkNonNull "array" array

    System.Array.FindIndex (
        array,
        System.Predicate ((=) value)) <> -1

/// Applies a function to each element of the collection, threading an accumulator argument through the computation.
/// The integer index passed to the function indicates the array index of the element being transformed.
[<CompiledName("FoldIndexed")>]
let foldi (folder : int -> 'State -> 'T -> 'State) (state : 'State) (array : 'T[]) =
    // Preconditions
    checkNonNull "array" array

    let folder = FSharpFunc<_,_,_,_>.Adapt folder
    let mutable state = state
    let len = array.Length
    for i = 0 to len - 1 do
        state <- folder.Invoke (i, state, array.[i])
    state

//
[<CompiledName("FoldBackIndexed")>]
let foldiBack (folder : int -> 'T -> 'State -> 'State) (array : 'T[]) (state : 'State) : 'State =
    // Preconditions
    checkNonNull "array" array

    let folder = FSharpFunc<_,_,_,_>.Adapt folder
        
    let mutable state = state
    for i = Array.length array - 1 downto 0 do
        state <- folder.Invoke (i, array.[i], state)
    state

/// Splits an array into one or more arrays; the specified predicate is applied
/// to each element in the array, and whenever it returns true, that element will
/// be the first element in one of the "subarrays".
[<CompiledName("Split")>]
let split (predicate : 'T -> bool) (array : 'T[]) =
    // Preconditions
    checkNonNull "array" array

    let segments = ResizeArray<_> ()
    let mutable currentSegment = ResizeArray<_> ()

    let len = array.Length
    for i = 0 to len - 1 do
        let el = array.[i]
        if currentSegment.Count > 0 && predicate el then
            segments.Add <| currentSegment.ToArray ()
            currentSegment <- ResizeArray<_> ()

        currentSegment.Add el

    // Append the last segment to the segment list,
    // then return the segment list as an array.
    segments.Add <| currentSegment.ToArray ()
    segments.ToArray ()

/// Splits an array into one or more segments by applying the specified predicate
/// to each element of the array and starting a new view at each element where
/// the predicate returns true.
[<CompiledName("Segment")>]
let segment (predicate : 'T -> bool) (array : 'T[]) : ArrayView<'T>[] =
    // Preconditions
    checkNonNull "array" array
        
    let segments = ResizeArray<_> ()

    let len = Array.length array
    let mutable segmentLength = 0
    for i = 0 to len - 1 do
        //
        if segmentLength > 0 && predicate array.[i] then
            // NOTE : The current element is the first element in the *new* segment!
            let offset = i - segmentLength

            ArrayView<_> (array, offset, segmentLength)
            |> segments.Add

            segmentLength <- 1
        else
            // The existing segment is empty, or the predicate returned false --
            // so "append" the element to the existing array segment.
            segmentLength <- segmentLength + 1

    // Finish the last/current segment, then return the list of segments as an array.
    let offset = len - segmentLength

    ArrayView<_> (array, offset, segmentLength)
    |> segments.Add

    segments.ToArray()

/// Splits two arrays into one or more segments by applying the specified predicate
/// to the each pair of array elements and starting a new view whenever the
/// predicate returns true.
[<CompiledName("Segment2")>]
let segment2 (predicate : 'T -> 'U -> bool) (array1 : 'T[]) (array2 : 'U[])
    : ArrayView<'T>[] * ArrayView<'U>[] =
    // Preconditions
    checkNonNull "array1" array1
    checkNonNull "array2" array2

    let predicate = FSharpFunc<_,_,_>.Adapt predicate
    let len1 = array1.Length 
    if len1 <> array2.Length then
        invalidArg "array2" "The arrays have differing lengths."

    let segments1 = ResizeArray<_> ()
    let segments2 = ResizeArray<_> ()

    let mutable segmentLength = 0
    for i = 0 to len1 - 1 do
        //
        if segmentLength > 0 && predicate.Invoke (array1.[i], array2.[i]) then
            // NOTE : The current element is the first element in the *new* segment!
            let offset = i - segmentLength

            ArrayView<_> (array1, offset, segmentLength)
            |> segments1.Add
            ArrayView<_> (array2, offset, segmentLength)
            |> segments2.Add

            segmentLength <- 1
        else
            // The existing segment is empty, or the predicate returned false --
            // so "append" the element to the existing array segment.
            segmentLength <- segmentLength + 1

    // Finish the last/current segment, then return the list of segments as an array.
    let offset = len1 - segmentLength

    ArrayView<_> (array1, offset, segmentLength)
    |> segments1.Add
    ArrayView<_> (array2, offset, segmentLength)
    |> segments2.Add

    segments1.ToArray(), segments2.ToArray()

/// Expands an array by creating a copy of it which has
/// the specified number of empty elements appended to it.
[<CompiledName("ExpandRight")>]
let expandRight count (array : 'T[]) =
    // Preconditions
    checkNonNull "array" array
    if count < 0 then
        invalidArg "count" "The number of elements to expand the array by is negative."

    // Create the new "expanded" array. Copy the elements from the original array
    // into the "left" side of the new array, then return the expanded array.
    let expandedArr = Array.zeroCreate (array.Length + count)        
    Array.blit array 0 expandedArr 0 array.Length
    expandedArr

/// Expands an array by creating a copy of it which has
/// the specified number of empty elements prepended to it.
[<CompiledName("ExpandLeft")>]
let expandLeft count (array : 'T[]) =
    // Preconditions
    checkNonNull "array" array
    if count < 0 then
        invalidArg "count" "The number of elements to expand the array by is negative."

    // Create the new "expanded" array. Copy the elements from the original array
    // into the "right" side of the new array, then return the expanded array.
    let expandedArr = Array.zeroCreate (array.Length + count)
    Array.blit array 0 expandedArr count array.Length
    expandedArr

//
[<CompiledName("MapPartition")>]
let mapPartition (partitioner : 'T -> Choice<'U1, 'U2>) array : 'U1[] * 'U2[] =
    // Preconditions
    checkNonNull "array" array

    // OPTIMIZATION : If the input array is empty, immediately return empty results.
    if Array.isEmpty array then
        Array.empty, Array.empty
    else
        // Use ResizeArrays to hold the mapped values.
        let resultList1 = ResizeArray ()
        let resultList2 = ResizeArray ()

        // Partition the array, adding each element to the ResizeArray
        // specific by the partition function.
        array
        |> Array.iter (fun el ->
            match partitioner el with
            | Choice1Of2 value ->
                resultList1.Add value
            | Choice2Of2 value ->
                resultList2.Add value)

        // Convert the ResizeArrays to arrays and return them.
        resultList1.ToArray (),
        resultList2.ToArray ()

//
[<CompiledName("MapPartition")>]
let mapPartition3 (partitioner : 'T -> Choice<'U1, 'U2, 'U3>) array : 'U1[] * 'U2[] * 'U3[] =
    // Preconditions
    checkNonNull "array" array

    // OPTIMIZATION : If the input array is empty, immediately return empty results.
    if Array.isEmpty array then
        Array.empty, Array.empty, Array.empty
    else
        // Use ResizeArrays to hold the mapped values.
        let resultList1 = ResizeArray ()
        let resultList2 = ResizeArray ()
        let resultList3 = ResizeArray ()

        // Partition the array, adding each element to the ResizeArray
        // specific by the partition function.
        array
        |> Array.iter (fun el ->
            match partitioner el with
            | Choice1Of3 value ->
                resultList1.Add value
            | Choice2Of3 value ->
                resultList2.Add value
            | Choice3Of3 value ->
                resultList3.Add value)

        // Convert the ResizeArrays to arrays and return them.
        resultList1.ToArray (),
        resultList2.ToArray (),
        resultList3.ToArray ()

//
[<CompiledName("MapReduce")>]
let mapReduce (mapReduction : IMapReduction<'Key, 'T>) (array : 'Key[]) : 'T =
    // Preconditions
    checkNonNull "array" array
    if Array.isEmpty array then
        invalidArg "array" "The array is empty."

    // Map the first element of the array so it can be
    // used as the seed for the fold.
    let mutable state = mapReduction.Map array.[0]

    // Implement an imperative-style fold, mapping each element
    // then reducing it with the current state to get the new state.
    let len = Array.length array
    for i = 1 to len - 1 do
        state <-
            mapReduction.Reduce state (mapReduction.Map array.[i])

    // Return the final state.
    state

//
[<CompiledName("FindIndices")>]
let findIndices (predicate : 'T -> bool) array =
    // Preconditions
    checkNonNull "array" array

    let indices = ResizeArray ()
    array |> Array.iteri (fun idx el ->
        if predicate el then indices.Add idx)
    indices.ToArray ()

//
[<CompiledName("ChooseIndexed")>]
let choosei (chooser : int -> 'T -> 'U option) array =
    // Preconditions
    checkNonNull "array" array

    let chooser = FSharpFunc<_,_,_>.Adapt chooser

    let chosen = ResizeArray ()
    let len = Array.length array

    for i = 0 to len - 1 do
        match chooser.Invoke (i, array.[i]) with
        | None -> ()
        | Some value ->
            chosen.Add value

    chosen.ToArray ()
