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

/// Additional functional operators on vectors (immutable arrays).
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExtCore.Collections.Vector

open LanguagePrimitives
open OptimizedClosures
open ExtCore


//
[<CompiledName("OfArray")>]
let inline ofArray (array : 'T[]) : vector<'T> =
    System.Array.AsReadOnly array

//
[<CompiledName("Length")>]
let inline length (vector : vector<'T>) : int =
    // Preconditions
    checkNonNull "vector" vector

    vector.Count

//
[<CompiledName("IsEmpty")>]
let inline isEmpty (vector : vector<'T>) : bool =
    // Preconditions
    checkNonNull "vector" vector
    
    vector.Count = 0

//
[<CompiledName("Map")>]
let map (mapping : 'T -> 'U) (vector : vector<'T>) : vector<'U> =
    // Preconditions
    checkNonNull "vector" vector

    let len = vector.Count
    let results = Array.zeroCreate len

    for i = 0 to len - 1 do
        results.[i] <- mapping vector.[i]

    // Return the results as a vector.
    ofArray results

// average
// averageBy
// blit
// collect
// concat
// copy
// create
// tryPick
// pick
// choose
// empty
// exists
// exists2
// filter
// find
// findIndex
// forall
// forall2
// fold
// foldBack
// fold2
// foldBack2
// get
// init
// iter
// iter2
// iteri
// iteri2
// map
// map2
// mapi
// mapi2
// max
// maxBy
// min
// minBy
// ofList
// ofSeq
// partition
// permute
// reduce
// reduceBack
// rev
// scan
// scanBack
// sub
// sort
// sortBy
// sortWith
// sum
// sumBy
// toList
// toSeq
// tryFind
// tryFindIndex
// unzip
// unzip3
// zip
// zip3


/// Given an element, creates an array containing just that element.
[<CompiledName("Singleton")>]
let inline singleton (value : 'T) : vector<'T> =
    vector [| value |]

/// Builds a vector that contains the elements of the set in order.
[<CompiledName("OfSet")>]
let inline ofSet (set : Set<'T>) : vector<'T> =
    vector (Set.toArray set)

/// Builds a set that contains the same elements as the given vector.
[<CompiledName("ToSet")>]
let toSet (vector : vector<'T>) : Set<'T> =
    // Preconditions
    checkNonNull "vector" vector

    notImpl "vector.toSet"
    //Set.ofArray array

/// Applies a function to each element of the array, returning a new array whose elements are
/// tuples of the original element and the function result for that element.
[<CompiledName("ProjectValues")>]
let projectValues (projection : 'Key -> 'T) (vector : vector<'Key>) : vector<'Key * 'T> =
    // Preconditions
    checkNonNull "vector" vector

    vector |> map (fun x -> x, projection x)

/// Applies a function to each element of the array, returning a new array whose elements are
/// tuples of the original element and the function result for that element.
[<CompiledName("ProjectKeys")>]
let projectKeys (projection : 'T -> 'Key) (vector : vector<'T>) : vector<'Key * 'T> =
    // Preconditions
    checkNonNull "vector" vector

    vector |> map (fun x -> projection x, x)

/// Returns the first element in the array.
[<CompiledName("First")>]
let inline first (vector : vector<'T>) : 'T =
    if isEmpty vector then
        invalidOp "Cannot retrieve the first element of an empty vector."
    else vector.[0]

/// Returns the index of the last element in the array.
[<CompiledName("LastIndex")>]
let inline lastIndex (vector : vector<'T>) : int =
    if isEmpty vector then
        invalidOp "The array is empty."
    else vector.Count - 1

/// Returns the last element in the array.
[<CompiledName("Last")>]
let inline last (vector : vector<'T>) : 'T =
    if isEmpty vector then
        invalidOp "Cannot retrieve the last element of an empty vector."
    else vector.[vector.Count - 1]

/// Determines if an array contains a specified value.
[<CompiledName("Contains")>]
let contains value (vector : vector<'T>) : bool =
    // Preconditions
    checkNonNull "vector" vector

    vector.IndexOf value <> -1

(*
/// <summary>
/// Returns a new collection containing the indices of the elements for which
/// the given predicate returns &quot;true&quot;.
/// </summary>
[<CompiledName("FindIndices")>]
let findIndices (predicate : 'T -> bool) (vector : vector<'T>) : int[] =
    // Preconditions
    checkNonNull "vector" vector

    let indices = ResizeArray ()
    array |> Array.iteri (fun idx el ->
        if predicate el then indices.Add idx)
    indices.ToArray ()

/// <summary>
/// Applies the given function to each element of the array.
/// Returns the array comprised of the results <c>x</c> for each element where the function
/// returns <c>Some(x)</c>. The integer index passed to the function indicates the index
/// of the element being transformed.
/// </summary>
[<CompiledName("ChooseIndexed")>]
let choosei (chooser : int -> 'T -> 'U option) (vector : vector<'T>) : vector<'U> =
    // Preconditions
    checkNonNull "vector" vector

    let chooser = FSharpFunc<_,_,_>.Adapt chooser

    let chosen = ResizeArray ()
    let len = Array.length array

    for i = 0 to len - 1 do
        match chooser.Invoke (i, array.[i]) with
        | None -> ()
        | Some value ->
            chosen.Add value

    chosen.ToArray ()

/// <summary>
/// Applies the given function pairwise to the two arrays.
/// Returns the array comprised of the results <c>x</c> for each element where the function
/// returns <c>Some(x)</c>.
/// </summary>
[<CompiledName("Choose2")>]
let choose2 (chooser : 'T1 -> 'T2 -> 'U option) array1 array2 : vector<'U> =
    // Preconditions
    checkNonNull "array1" array1
    checkNonNull "array2" array2
    if Array.length array1 <> Array.length array2 then
        invalidArg "array2" "The arrays have different lengths."

    let chooser = FSharpFunc<_,_,_>.Adapt chooser

    let chosen = ResizeArray ()
    let len = Array.length array1

    for i = 0 to len - 1 do
        match chooser.Invoke (array1.[i], array2.[i]) with
        | None -> ()
        | Some value ->
            chosen.Add value

    chosen.ToArray ()

/// Applies a function to each element of the collection, threading an accumulator argument through the computation.
/// The integer index passed to the function indicates the array index of the element being transformed.
[<CompiledName("FoldIndexed")>]
let foldi (folder : 'State -> int -> 'T -> 'State) (state : 'State) (vector : vector<'T>) =
    // Preconditions
    checkNonNull "vector" vector

    let folder = FSharpFunc<_,_,_,_>.Adapt folder
    let mutable state = state
    let len = array.Length
    for i = 0 to len - 1 do
        state <- folder.Invoke (state, i, array.[i])
    state

/// Applies a function to each element of the collection, threading an accumulator argument through the computation.
/// The integer index passed to the function indicates the array index of the element being transformed.
[<CompiledName("FoldBackIndexed")>]
let foldiBack (folder : int -> 'T -> 'State -> 'State) (vector : vector<'T>) (state : 'State) : 'State =
    // Preconditions
    checkNonNull "vector" vector

    let folder = FSharpFunc<_,_,_,_>.Adapt folder
        
    let mutable state = state
    for i = Array.length array - 1 downto 0 do
        state <- folder.Invoke (i, array.[i], state)
    state

/// Splits an array into one or more arrays; the specified predicate is applied
/// to each element in the array, and whenever it returns true, that element will
/// be the first element in one of the "subarrays".
[<CompiledName("Split")>]
let split (predicate : 'T -> bool) (vector : vector<'T>) =
    // Preconditions
    checkNonNull "vector" vector

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
let segment (predicate : 'T -> bool) (vector : vector<'T>) : ArrayView<'T>[] =
    // Preconditions
    checkNonNull "vector" vector
        
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
let segment2 (predicate : 'T -> 'U -> bool) (array1 : vector<'T>) (array2 : vector<'U>)
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

/// Splits the collection into two (2) collections, containing the elements for which the given
/// function returns Choice1Of2 or Choice2Of2, respectively. This function is similar to
/// Array.partition, but it allows the returned collections to have different types.
[<CompiledName("MapPartition")>]
let mapPartition (partitioner : 'T -> Choice<'U1, 'U2>) vector : 'U1[] * 'U2[] =
    // Preconditions
    checkNonNull "vector" vector
    
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

/// Splits the collection into two (3) collections, containing the elements for which the given
/// function returns Choice1Of3, Choice2Of3, or Choice3Of3, respectively. This function is similar
/// to Array.partition, but it allows the returned collections to have different types.
[<CompiledName("MapPartition")>]
let mapPartition3 (partitioner : 'T -> Choice<'U1, 'U2, 'U3>) vector : 'U1[] * 'U2[] * 'U3[] =
    // Preconditions
    checkNonNull "vector" vector

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

/// Applies a mapping function to each element of the array, then repeatedly applies
/// a reduction function to each pair of results until one (1) result value remains.
[<CompiledName("MapReduce")>]
let mapReduce (mapReduction : IMapReduction<'Key, 'T>) (vector : 'Key[]) : 'T =
    // Preconditions
    checkNonNull "mapReduction" mapReduction
    checkNonNull "vector" vector
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
*)
