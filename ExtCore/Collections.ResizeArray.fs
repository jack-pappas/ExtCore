(*

Copyright 2005-2009 Microsoft Corporation
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

/// <summary>
/// Functional operators related to the System.Collections.Generic.List&lt;T&gt; type (called ResizeArray in F#).
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExtCore.Collections.ResizeArray

open System.Collections.Generic
open LanguagePrimitives
open OptimizedClosures
open ExtCore


/// Return the length of the collection.
[<CompiledName("Length")>]
let inline length (resizeArray : ResizeArray<'T>) : int =
    resizeArray.Count

/// Return true if the given array is empty, otherwise false.
[<CompiledName("IsEmpty")>]
let inline isEmpty (resizeArray : ResizeArray<'T>) : bool =
    resizeArray.Count = 0

/// Fetch an element from the collection.
[<CompiledName("Get")>]
let inline get (resizeArray : ResizeArray<'T>) index : 'T =
    resizeArray.[index]

/// Set the value of an element in the collection.
[<CompiledName("Set")>]
let inline set (resizeArray : ResizeArray<'T>) index value : unit =
    resizeArray.[index] <- value

/// Create a ResizeArray whose elements are all initially the given value.
[<CompiledName("Create")>]
let create count value : ResizeArray<'T> =
    // Preconditions
    if count < 0 then
        invalidArg "count" "The number of elements may not be negative."

    let resizeArray = ResizeArray (count)
    for i = 0 to count - 1 do
        resizeArray.Add value
    resizeArray

/// Create a ResizeArray by calling the given generator on each index.
[<CompiledName("Init")>]
let init count initializer : ResizeArray<'T> =
    // Preconditions
    if count < 0 then
        invalidArg "count" "The number of elements may not be negative."

    let resizeArray = ResizeArray (count)
    for i = 0 to count - 1 do
        resizeArray.Add <| initializer i
    resizeArray

/// Adds an object to the end of the ResizeArray.
[<CompiledName("Add")>]
let inline add item (resizeArray : ResizeArray<'T>) : unit =
    resizeArray.Add item

/// Determines whether an element is in the ResizeArray.
[<CompiledName("Contains")>]
let inline contains (value : 'T) (resizeArray : ResizeArray<'T>) : bool =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    resizeArray.Contains value

/// Build a ResizeArray from the given sequence.
[<CompiledName("OfSeq")>]
let inline ofSeq (sequence : seq<'T>) : ResizeArray<'T> =
    ResizeArray (sequence)

/// Build a ResizeArray from the given list.
[<CompiledName("OfList")>]
let ofList (list : 'T list) : ResizeArray<'T> =
    // Preconditions
    checkNonNull "list" list

    let len = list.Length
    let res = ResizeArray<_>(len)
    let rec add = function
        | [] -> ()
        | e::l -> res.Add(e); add l
    add list
    res

/// Build a ResizeArray from the given array.
[<CompiledName("OfArray")>]
let inline ofArray (arr : 'T[]) : ResizeArray<'T> =
    ResizeArray (arr)

/// Build a ResizeArray from the given vector.
[<CompiledName("OfVector")>]
let inline ofVector (vector : vector<'T>) : ResizeArray<'T> =
    ResizeArray (vector)

/// Return a view of the ResizeArray as an enumerable object.
[<CompiledName("ToSeq")>]
let toSeq (resizeArray : ResizeArray<'T>) : seq<'T> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    Seq.readonly resizeArray

/// Build a list from the given ResizeArray.
[<CompiledName("ToList")>]
let toList (resizeArray : ResizeArray<'T>) : 'T list =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let mutable res = []
    for i = length resizeArray - 1 downto 0 do
        res <- resizeArray.[i] :: res
    res

/// Return a fixed-length array containing the elements of the input ResizeArray.
[<CompiledName("ToArray")>]
let inline toArray (resizeArray : ResizeArray<'T>) : 'T[] =
    resizeArray.ToArray ()

/// Build a vector from the given ResizeArray.
[<CompiledName("ToVector")>]
let toVector (resizeArray : ResizeArray<'T>) : vector<'T> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    resizeArray.ToArray ()
    |> vector.UnsafeCreate

/// Sorts the elements of the ResizeArray by mutating the ResizeArray in-place.
/// Elements are compared using Operators.compare.
[<CompiledName("SortInPlace")>]
let inline sortInPlace<'T when 'T : comparison> (resizeArray : ResizeArray<'T>) : unit =
    resizeArray.Sort ()
        
/// Sort the elements using the key extractor and generic comparison on the keys.
[<CompiledName("SortInPlaceBy")>]
let inline sortInPlaceBy<'T, 'Key when 'Key : comparison>
        (projection : 'T -> 'Key) (resizeArray : ResizeArray<'T>) =
    resizeArray.Sort (fun x y ->
        compare (projection x) (projection y))

/// Sort the elements using the given comparison function.
[<CompiledName("SortInPlaceWith")>]
let inline sortInPlaceWith (comparer : 'T -> 'T -> int) (resizeArray : ResizeArray<'T>) : unit =
    resizeArray.Sort (comparer)

/// Build a new ResizeArray that contains the elements of the given ResizeArray.
[<CompiledName("Copy")>]
let inline copy (resizeArray : ResizeArray<'T>) : ResizeArray<'T> =
    ResizeArray (resizeArray)

/// Return an array containing the given element.
[<CompiledName("Singleton")>]
let singleton value : ResizeArray<'T> =
    let resizeArray = ResizeArray ()
    resizeArray.Add value
    resizeArray

/// Build a new ResizeArray that contains the elements of each of the given sequence of ResizeArrays.
[<CompiledName("Concat")>]
let concat (resizeArrays : seq<ResizeArray<'T>>) : ResizeArray<'T> =
    // Preconditions
    checkNonNull "resizeArrays" resizeArrays

    let flattened = ResizeArray ()
    for resizeArray in resizeArrays do
        flattened.AddRange resizeArray
    flattened
    
/// Build a new ResizeArray that contains the elements of the first ResizeArray followed by
/// the elements of the second ResizeArray.
[<CompiledName("Append")>]
let append (resizeArray1 : ResizeArray<'T>) (resizeArray2 : ResizeArray<'T>) : ResizeArray<'T> =
    // Preconditions
    checkNonNull "resizeArray1" resizeArray1
    checkNonNull "resizeArray2" resizeArray2

    let combined = ResizeArray (resizeArray1.Count + resizeArray2.Count)
    combined.AddRange resizeArray1
    combined.AddRange resizeArray2
    combined

/// Build a new ResizeArray that contains the given subrange specified by
/// starting index and length.
[<CompiledName("Sub")>]
let sub (resizeArray : ResizeArray<'T>) start count : ResizeArray<'T> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray
    if start < 0 then
        invalidArg "start" "The start index cannot be less than zero (0)."
    elif count < 0 then
        invalidArg "count" "The number of elements to copy cannot be less than zero (0)."
    elif start + count > length resizeArray then
        invalidArg "count" "There are fewer than 'count' elements between the 'start' index and the end of the collection."
    
    resizeArray.GetRange (start, count)

/// Fill a range of the collection with the given element.
[<CompiledName("Fill")>]
let fill (resizeArray : ResizeArray<'T>) start count value : unit =
    // Preconditions
    checkNonNull "resizeArray" resizeArray
    if start < 0 then
        invalidArg "start" "The start index cannot be less than zero (0)."
    elif count < 0 then
        invalidArg "count" "The number of elements to copy cannot be less than zero (0)."
    elif start + count > length resizeArray then
        invalidArg "count" "There are fewer than 'count' elements between the 'start' index and the end of the collection."
    
    // Overwrite the items within the range using the specified value.
    for i = start to start + count - 1 do
        resizeArray.[i] <- value

/// Return a new ResizeArray with the elements in reverse order.
[<CompiledName("Rev")>]
let rev (resizeArray : ResizeArray<'T>) : ResizeArray<'T> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let len = length resizeArray
    let result = ResizeArray (len)
    for i = len - 1 downto 0 do
        result.Add resizeArray.[i]
    result

/// Read a range of elements from the first ResizeArray and write them into the second.
[<CompiledName("Blit")>]
let blit (source : ResizeArray<'T>) sourceIndex (target : ResizeArray<'T>) targetIndex count : unit =
    // Preconditions
    checkNonNull "source" source
    checkNonNull "target" target
    if sourceIndex < 0 then
        invalidArg "sourceIndex" "The source index cannot be negative."
    elif targetIndex < 0 then
        invalidArg "targetIndex" "The target index cannot be negative."
    elif count < 0 then
        invalidArg "count" "Cannot copy a negative number of items."
    elif sourceIndex + count > length source then
        invalidArg "sourceIndex" "There are fewer than 'count' elements between 'sourceIndex' and the end of the source ResizeArray."
    elif targetIndex + count > length target then
        invalidArg "sourceIndex" "There are fewer than 'count' elements between 'sourceIndex' and the end of the target ResizeArray."

    for i = 0 to count - 1 do
        target.[targetIndex + i] <- source.[sourceIndex + i]

/// Combine the two ResizeArrays into a ResizeArray of pairs.
/// The two arrays must have equal lengths, otherwise an <c>ArgumentException</c> is raised.
[<CompiledName("Zip")>]
let zip (resizeArray1 : ResizeArray<'T1>) (resizeArray2 : ResizeArray<'T2>)
    : ResizeArray<'T1 * 'T2> =
    // Preconditions
    checkNonNull "resizeArray1" resizeArray1
    checkNonNull "resizeArray2" resizeArray2

    let len = length resizeArray1
    if len <> length resizeArray2 then
        invalidArg "resizeArray2" "The ResizeArrays have different lengths."

    let results = ResizeArray (len)
    for i = 0 to len - 1 do
        results.Add (resizeArray1.[i], resizeArray2.[i])
    results

/// Split a ResizeArray of pairs into two ResizeArrays.
[<CompiledName("Unzip")>]
let unzip (resizeArray : ResizeArray<'T1 * 'T2>) : ResizeArray<'T1> * ResizeArray<'T2> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let len = length resizeArray
    let results1 = ResizeArray (len)
    let results2 = ResizeArray (len)

    for i = 0 to len - 1 do
        let x, y = resizeArray.[i]
        results1.Add x
        results2.Add y

    results1, results2

/// Test if any element of the array satisfies the given predicate.
/// If the input function is <c>f</c> and the elements are <c>i0...iN</c> 
/// then computes <c>p i0 or ... or p iN</c>.
[<CompiledName("Exists")>]
let inline exists (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : bool =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    #if FX_ATLEAST_PORTABLE
    // Simple implementation for portable libraries.
    let mutable currentIndex = 0
    let mutable foundMatch = false
    while currentIndex < resizeArray.Count && not foundMatch do
        if predicate resizeArray.[currentIndex] then
            foundMatch <- true
        else
            currentIndex <- currentIndex + 1
    foundMatch

    #else
    resizeArray.Exists (System.Predicate predicate)
    #endif

/// Test elements of the two arrays pairwise to see if any pair of element satisfies the given predicate.
/// Raise ArgumentException if the arrays have different lengths.
[<CompiledName("Exists2")>]
let exists2 predicate (resizeArray1 : ResizeArray<'T1>) (resizeArray2 : ResizeArray<'T2>) : bool =
    // Preconditions
    checkNonNull "resizeArray1" resizeArray1
    checkNonNull "resizeArray2" resizeArray2

    let len = length resizeArray1
    if len <> length resizeArray2 then
        invalidArg "resizeArray2" "The ResizeArrays have different lengths."

    let predicate = FSharpFunc<_,_,_>.Adapt predicate
    
    let mutable index = 0
    let mutable foundMatch = false
    while index < len && not foundMatch do
        foundMatch <- predicate.Invoke (resizeArray1.[index], resizeArray2.[index])
        index <- index + 1
    foundMatch

/// Test if all elements of the array satisfy the given predicate.
/// If the input function is <c>f</c> and the elements are <c>i0...iN</c> and "j0...jN"
/// then computes <c>p i0 && ... && p iN</c>.
[<CompiledName("Forall")>]
let inline forall (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : bool =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    #if FX_ATLEAST_PORTABLE
    // Simple implementation for portable libraries.
    let mutable currentIndex = 0
    let mutable foundNonMatch = false
    while currentIndex < resizeArray.Count && not foundNonMatch do
        if not <| predicate resizeArray.[currentIndex] then
            foundNonMatch <- true
        else
            currentIndex <- currentIndex + 1
    not foundNonMatch

    #else
    resizeArray.TrueForAll (System.Predicate predicate)
    #endif

/// Test elements of the two arrays pairwise to see if all pairs of elements satisfy the given predicate.
/// Raise ArgumentException if the arrays have different lengths.
[<CompiledName("Forall2")>]
let forall2 predicate (resizeArray1 : ResizeArray<'T1>) (resizeArray2 : ResizeArray<'T2>) : bool =
    // Preconditions
    checkNonNull "resizeArray1" resizeArray1
    checkNonNull "resizeArray2" resizeArray2

    let len = length resizeArray1
    if len <> length resizeArray2 then
        invalidArg "resizeArray2" "The ResizeArrays have different lengths."

    let predicate = FSharpFunc<_,_,_>.Adapt predicate
    
    let mutable index = 0
    let mutable allMatch = true
    while index < len && allMatch do
        allMatch <- predicate.Invoke (resizeArray1.[index], resizeArray2.[index])
        index <- index + 1
    allMatch

/// Return a new collection containing only the elements of the collection
/// for which the given predicate returns <c>true</c>.
[<CompiledName("Filter")>]
let inline filter (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : ResizeArray<'T> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    #if FX_ATLEAST_PORTABLE
    // Simple implementation for portable libraries.
    let results = ResizeArray ()
    for i = 0 to resizeArray.Count - 1 do
        let el = resizeArray.[i]
        if predicate el then
            results.Add el
    results

    #else
    resizeArray.FindAll (System.Predicate predicate)
    #endif

/// <summary>
/// Apply the given function to each element of the array. Return
/// the array comprised of the results "x" for each element where
/// the function returns <c>Some(x)</c>.
/// </summary>
[<CompiledName("Choose")>]
let choose (chooser : 'T -> 'U option) (resizeArray : ResizeArray<'T>) : ResizeArray<'U> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    // OPTIMIZATION : If the input list is empty return immediately.
    if isEmpty resizeArray then
        ResizeArray ()
    else
        let result = ResizeArray ()
        let count = resizeArray.Count

        for i = 0 to count - 1 do
            match chooser resizeArray.[i] with
            | None -> ()
            | Some value ->
                result.Add value

        result

/// <summary>
/// Return the first element for which the given function returns <c>true</c>.
/// Return <c>None</c> if no such element exists.
/// </summary>
[<CompiledName("TryFind")>]
let tryFind (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : 'T option =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let elementIndex =
        #if FX_ATLEAST_PORTABLE
        let mutable elementIndex = -1
        let mutable currentIndex = 0
        while currentIndex < resizeArray.Count && elementIndex = -1 do
            if predicate resizeArray.[currentIndex] then
                elementIndex <- currentIndex
            else
                currentIndex <- currentIndex + 1
        elementIndex
        #else
        resizeArray.FindIndex (System.Predicate predicate)
        #endif

    match elementIndex with
    | -1 ->
        None
    | index ->
        Some resizeArray.[index]

/// <summary>
/// Return the first element for which the given function returns <c>true</c>.
/// Raise <c>KeyNotFoundException</c> if no such element exists.
/// </summary>
[<CompiledName("Find")>]
let find (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : 'T =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let elementIndex =
        #if FX_ATLEAST_PORTABLE
        let mutable elementIndex = -1
        let mutable currentIndex = 0
        while currentIndex < resizeArray.Count && elementIndex = -1 do
            if predicate resizeArray.[currentIndex] then
                elementIndex <- currentIndex
            else
                currentIndex <- currentIndex + 1
        elementIndex
        #else
        resizeArray.FindIndex (System.Predicate predicate)
        #endif

    match elementIndex with
    | -1 ->
        // TODO : Add a better error message.
        // keyNotFound ""
        raise <| System.Collections.Generic.KeyNotFoundException ()
    | index ->
        resizeArray.[index]

/// Return the index of the first element in the array
/// that satisfies the given predicate.
[<CompiledName("TryFindIndex")>]
let tryFindIndex (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : int option =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let elementIndex =
        #if FX_ATLEAST_PORTABLE
        let mutable elementIndex = -1
        let mutable currentIndex = 0
        while currentIndex < resizeArray.Count && elementIndex = -1 do
            if predicate resizeArray.[currentIndex] then
                elementIndex <- currentIndex
            else
                currentIndex <- currentIndex + 1
        elementIndex
        #else
        resizeArray.FindIndex (System.Predicate predicate)
        #endif

    match elementIndex with
    | -1 ->
        None
    | index ->
        Some index
        
/// <summary>
/// Return the index of the first element in the array
/// that satisfies the given predicate. Raise <c>KeyNotFoundException</c> if 
/// none of the elements satisfy the predicate.
/// </summary>
[<CompiledName("FindIndex")>]
let findIndex (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : int =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let elementIndex =
        #if FX_ATLEAST_PORTABLE
        let mutable elementIndex = -1
        let mutable currentIndex = 0
        while currentIndex < resizeArray.Count && elementIndex = -1 do
            if predicate resizeArray.[currentIndex] then
                elementIndex <- currentIndex
            else
                currentIndex <- currentIndex + 1
        elementIndex
        #else
        resizeArray.FindIndex (System.Predicate predicate)
        #endif

    match elementIndex with
    | -1 ->
        // TODO : Add a better error message.
        // keyNotFound ""
        raise <| System.Collections.Generic.KeyNotFoundException ()
    | index ->
        index

/// Return the index of the first element in the array
/// that satisfies the given predicate.
[<CompiledName("TryFindIndexIndexed")>]
let tryFindIndexi predicate (resizeArray : ResizeArray<'T>) : int option =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let predicate = FSharpFunc<_,_,_>.Adapt predicate

    let lastIndex = length resizeArray - 1
    let mutable index = -1
    let mutable foundMatch = false
    while index < lastIndex && not foundMatch do
        let i = index + 1
        index <- i
        foundMatch <- predicate.Invoke (i, resizeArray.[i])

    if foundMatch then
        Some index
    else None

/// <summary>
/// Return the index of the first element in the array
/// that satisfies the given predicate. Raise <c>KeyNotFoundException</c> if 
/// none of the elements satisfy the predicate.
/// </summary>
[<CompiledName("FindIndexIndexed")>]
let findIndexi predicate (resizeArray : ResizeArray<'T>) : int =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    match tryFindIndexi predicate resizeArray with
    | Some index ->
        index
    | None ->
        keyNotFound "An element satisfying the predicate was not found in the collection."

/// <summary>
/// Applies the given function to successive elements, returning the first
/// result where function returns <c>Some(x)</c> for some x. If the function
/// never returns <c>Some(x)</c>, returns <c>None</c>.
/// </summary>
[<CompiledName("TryPick")>]
let tryPick (picker : 'T -> 'U option) (resizeArray : ResizeArray<'T>) : 'U option =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let count = resizeArray.Count
    let mutable result = None
    let mutable index = 0

    while index < count && Option.isNone result do
        result <- picker resizeArray.[index]
        index <- index + 1
    result

/// <summary>
/// Applies the given function to successive elements, returning the first
/// result where function returns <c>Some(x)</c> for some x. If the function
/// never returns <c>Some(x)</c>, raises KeyNotFoundException.
/// </summary>
[<CompiledName("Pick")>]
let pick (picker : 'T -> 'U option) (resizeArray : ResizeArray<'T>) : 'U =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let count = resizeArray.Count
    let mutable result = None
    let mutable index = 0

    while index < count && Option.isNone result do
        result <- picker resizeArray.[index]
        index <- index + 1

    match result with
    | Some result ->
        result
    | None ->
        // TODO : Return a better error message
        //keyNotFound ""
        raise <| System.Collections.Generic.KeyNotFoundException ()

/// Apply the given function to each element of the array.
[<CompiledName("Iterate")>]
let iter (action : 'T -> unit) (resizeArray : ResizeArray<'T>) : unit =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let count = resizeArray.Count
    for i = 0 to count - 1 do
        action resizeArray.[i]

/// Apply the given function to each element of the array. The integer passed to the
/// function indicates the index of element.
[<CompiledName("IterateIndexed")>]
let iteri (action : int -> 'T -> unit) (resizeArray : ResizeArray<'T>) : unit =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let action = FSharpFunc<_,_,_>.Adapt action

    let count = resizeArray.Count
    for i = 0 to count - 1 do
        action.Invoke (i, resizeArray.[i])

/// <summary>
/// Apply the given function to two arrays simultaneously. The two arrays
/// must have the same lengths, otherwise an <c>ArgumentException</c> is raised.
/// </summary>
[<CompiledName("Iterate2")>]
let iter2 action (resizeArray1 : ResizeArray<'T1>) (resizeArray2 : ResizeArray<'T1>) : unit =
    // Preconditions
    checkNonNull "resizeArray1" resizeArray1
    checkNonNull "resizeArray2" resizeArray2

    let len = length resizeArray1
    if len <> length resizeArray2 then
        invalidArg "resizeArray2" "The ResizeArrays have different lengths."

    let action = FSharpFunc<_,_,_>.Adapt action

    for i = 0 to len - 1 do
        action.Invoke (resizeArray1.[i], resizeArray2.[i])

/// <summary>
/// Apply the given function to pair of elements drawn from matching indices in two arrays,
/// also passing the index of the elements. The two arrays must have the same lengths, 
/// otherwise an <c>ArgumentException</c> is raised.
/// </summary>
[<CompiledName("IterateIndexed2")>]
let iteri2 action (resizeArray1 : ResizeArray<'T1>) (resizeArray2 : ResizeArray<'T2>) : unit =
    // Preconditions
    checkNonNull "resizeArray1" resizeArray1
    checkNonNull "resizeArray2" resizeArray2

    let len = length resizeArray1
    if len <> length resizeArray2 then
        invalidArg "resizeArray2" "The ResizeArrays have different lengths."

    let action = FSharpFunc<_,_,_,_>.Adapt action

    for i = 0 to len - 1 do
        action.Invoke (i, resizeArray1.[i], resizeArray2.[i])

/// <summary>
/// Build a new array whose elements are the results of applying the given function
/// to each of the elements of the array.
/// </summary>
/// <param name="mapping"></param>
/// <param name="resizeArray"></param>
/// <returns></returns>
[<CompiledName("Map")>]
let inline map (mapping : 'T -> 'U) (resizeArray : ResizeArray<'T>) : ResizeArray<'U> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    #if FX_ATLEAST_PORTABLE || NETSTANDARD1_6
    // Simple implementation for portable libraries.
    let results = ResizeArray (resizeArray.Count)
    for i = 0 to resizeArray.Count - 1 do
        results.Add (mapping resizeArray.[i])
    results

    #else
    resizeArray.ConvertAll (System.Converter mapping)
    #endif

/// <summary>
/// Build a new array whose elements are the results of applying the given function
/// to each of the elements of the array. The integer index passed to the
/// function indicates the index of element being transformed.
/// </summary>
/// <param name="mapping"></param>
/// <param name="resizeArray"></param>
/// <returns></returns>
[<CompiledName("MapIndexed")>]
let mapi (mapping : int -> 'T -> 'U) (resizeArray : ResizeArray<'T>) : ResizeArray<'U> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let mapping = FSharpFunc<_,_,_>.Adapt mapping
    let count = resizeArray.Count
    let result = ResizeArray (count)

    for i = 0 to count - 1 do
        result.Add <| mapping.Invoke (i, resizeArray.[i])
    result

/// <summary>
/// Build a new collection whose elements are the results of applying the given function
/// to the corresponding elements of the two collections pairwise. The two input
/// arrays must have the same lengths.
/// </summary>
/// <param name="mapping"></param>
/// <param name="resizeArray1"></param>
/// <param name="resizeArray2"></param>
/// <returns></returns>
[<CompiledName("Map2")>]
let map2 mapping (resizeArray1 : ResizeArray<'T1>) (resizeArray2 : ResizeArray<'T2>) : ResizeArray<'U> =
    // Preconditions
    checkNonNull "resizeArray1" resizeArray1
    checkNonNull "resizeArray2" resizeArray2

    let len = length resizeArray1
    if len <> length resizeArray2 then
        invalidArg "resizeArray2" "The ResizeArrays have different lengths."

    let mapping = FSharpFunc<_,_,_>.Adapt mapping
    let results = ResizeArray (len)

    for i = 0 to len - 1 do
        mapping.Invoke (resizeArray1.[i], resizeArray2.[i])
        |> results.Add
    results

/// <summary>
/// Build a new collection whose elements are the results of applying the given function
/// to the corresponding elements of the two collections pairwise. The two input
/// arrays must have the same lengths, otherwise an <c>ArgumentException</c> is raised.
/// </summary>
/// <summary></summary>
/// <param name="mapping"></param>
/// <param name="resizeArray1"></param>
/// <param name="resizeArray2"></param>
/// <returns></returns>
[<CompiledName("MapIndexed2")>]
let mapi2 mapping (resizeArray1 : ResizeArray<'T1>) (resizeArray2 : ResizeArray<'T2>) : ResizeArray<'U> =
    // Preconditions
    checkNonNull "resizeArray1" resizeArray1
    checkNonNull "resizeArray2" resizeArray2

    let len = length resizeArray1
    if len <> length resizeArray2 then
        invalidArg "resizeArray2" "The ResizeArrays have different lengths."

    let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
    let results = ResizeArray (len)

    for i = 0 to len - 1 do
        mapping.Invoke (i, resizeArray1.[i], resizeArray2.[i])
        |> results.Add
    results

/// <summary>
/// Apply a function to each element of the collection, threading an accumulator argument
/// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> 
/// then computes <c>f (... (f s i0)...) iN</c>.
/// </summary>
/// <param name="folder"></param>
/// <param name="state"></param>
/// <param name="resizeArray"></param>
/// <returns></returns>
[<CompiledName("Fold")>]
let fold (folder : 'State -> 'T -> 'State) state (resizeArray : ResizeArray<'T>) : 'State =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let folder = FSharpFunc<_,_,_>.Adapt folder

    let mutable state = state
    let count = resizeArray.Count
    for i = 0 to count - 1 do
        state <- folder.Invoke (state, resizeArray.[i])
    state

/// <summary></summary>
/// <param name="folder"></param>
/// <param name="state"></param>
/// <param name="resizeArray"></param>
/// <param name="startIndex"></param>
/// <param name="endIndex"></param>
/// <returns></returns>
[<CompiledName("FoldSub")>]
let foldSub folder (state : 'State) (resizeArray : ResizeArray<'T>) startIndex endIndex : 'State =
    // Preconditions
    checkNonNull "resizeArray" resizeArray
    if startIndex < 0 then
        argOutOfRange "startIndex" "The starting index cannot be negative."
    elif endIndex > 0 then
        argOutOfRange "endIndex" "The ending index cannot be negative."
    
    let len = length resizeArray
    if startIndex >= len then
        argOutOfRange "startIndex" "The starting index is outside the bounds of the ResizeArray."
    elif endIndex >= len then
        argOutOfRange "endIndex" "The ending index is outside the bounds of the ResizeArray."

    let folder = FSharpFunc<_,_,_>.Adapt folder

    // Fold over the specified range of items.
    let mutable state = state
    for i = startIndex to endIndex do
        state <- folder.Invoke (state, resizeArray.[i])
    state

/// <summary>
/// Applies a function to each element of the collection, threading an accumulator argument
/// through the computation. The integer index passed to the function indicates the
/// index of the element within the collection.
/// </summary>
/// <param name="folder"></param>
/// <param name="state"></param>
/// <param name="resizeArray"></param>
/// <returns></returns>
[<CompiledName("FoldIndexed")>]
let foldi (folder : 'State -> int -> 'T -> 'State) state (resizeArray : ResizeArray<'T>) : 'State =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let folder = FSharpFunc<_,_,_,_>.Adapt folder

    let mutable state = state
    let count = resizeArray.Count
    for i = 0 to count - 1 do
        state <- folder.Invoke (state, i, resizeArray.[i])
    state

/// <summary>
/// Apply a function to pairs of elements drawn from the two collections, left-to-right,
/// threading an accumulator argument through the computation.  The two input arrays must
/// have the same lengths, otherwise an <c>ArgumentException</c> is raised.
/// </summary>
/// <param name="folder"></param>
/// <param name="state"></param>
/// <param name="resizeArray1"></param>
/// <param name="resizeArray2"></param>
/// <returns></returns>
[<CompiledName("Fold2")>]
let fold2 folder (state : 'State) (resizeArray1 : ResizeArray<'T1>) (resizeArray2 : ResizeArray<'T2>) : 'State =
    // Preconditions
    checkNonNull "resizeArray1" resizeArray1
    checkNonNull "resizeArray2" resizeArray2

    let len = length resizeArray1
    if len <> length resizeArray2 then
        invalidArg "resizeArray2" "The arrays have different lengths."

    let folder = FSharpFunc<_,_,_,_>.Adapt folder
    let mutable state = state
    for i = 0 to len - 1 do
        state <- folder.Invoke (state, resizeArray1.[i], resizeArray2.[i])
    state

/// <summary>
/// Apply a function to each element of the array, threading an accumulator argument
/// through the computation. If the input function is <c>f</c> and the elements are
/// <c>i0...iN</c> then computes <c>f i0 (...(f iN s))</c>.
/// </summary>
/// <param name="folder"></param>
/// <param name="resizeArray"></param>
/// <param name="state"></param>
/// <returns></returns>
[<CompiledName("FoldBack")>]
let foldBack (folder : 'T -> 'State -> 'State) (resizeArray : ResizeArray<'T>) state : 'State =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let folder = FSharpFunc<_,_,_>.Adapt folder

    let mutable state = state
    for i = resizeArray.Count - 1 downto 0 do
        state <- folder.Invoke (resizeArray.[i], state)
    state

/// <summary></summary>
/// <param name="folder"></param>
/// <param name="resizeArray"></param>
/// <param name="startIndex"></param>
/// <param name="endIndex"></param>
/// <param name="state"></param>
/// <returns></returns>
[<CompiledName("FoldBackSub")>]
let foldBackSub folder (resizeArray : ResizeArray<'T>) startIndex endIndex (state : 'State) : 'State =
    // Preconditions
    checkNonNull "resizeArray" resizeArray
    if startIndex < 0 then
        argOutOfRange "startIndex" "The starting index cannot be negative."
    elif endIndex > 0 then
        argOutOfRange "endIndex" "The ending index cannot be negative."
    
    let len = length resizeArray
    if startIndex >= len then
        argOutOfRange "startIndex" "The starting index is outside the bounds of the ResizeArray."
    elif endIndex >= len then
        argOutOfRange "endIndex" "The ending index is outside the bounds of the ResizeArray."

    let folder = FSharpFunc<_,_,_>.Adapt folder

    // Fold over the specified range of items.
    let mutable state = state
    for i = endIndex downto startIndex do
        state <- folder.Invoke (resizeArray.[i], state)
    state

/// <summary></summary>
/// <param name="folder"></param>
/// <param name="resizeArray"></param>
/// <param name="state"></param>
/// <returns></returns>
[<CompiledName("FoldBackIndexed")>]
let foldiBack (folder : int -> 'T -> 'State -> 'State) (resizeArray : ResizeArray<'T>) state : 'State =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let folder = FSharpFunc<_,_,_,_>.Adapt folder

    let mutable state = state
    for i = resizeArray.Count - 1 downto 0 do
        state <- folder.Invoke (i, resizeArray.[i], state)
    state

/// <summary>
/// Apply a function to pairs of elements drawn from the two collections, right-to-left, 
/// threading an accumulator argument through the computation.  The two input
/// arrays must have the same lengths, otherwise an <c>ArgumentException</c> is raised.
/// </summary>
/// <param name="folder"></param>
/// <param name="resizeArray1"></param>
/// <param name="resizeArray2"></param>
/// <param name="state"></param>
/// <returns></returns>
[<CompiledName("FoldBack2")>]
let foldBack2 folder (resizeArray1 : ResizeArray<'T1>) (resizeArray2 : ResizeArray<'T2>) (state : 'State) : 'State =
    // Preconditions
    checkNonNull "resizeArray1" resizeArray1
    checkNonNull "resizeArray2" resizeArray2

    let len = length resizeArray1
    if len <> length resizeArray2 then
        invalidArg "resizeArray2" "The arrays have different lengths."

    let folder = FSharpFunc<_,_,_,_>.Adapt folder
    let mutable state = state
    for i = len - 1 downto 0 do
        state <- folder.Invoke (resizeArray1.[i], resizeArray2.[i], state)
    state

/// <summary>
/// Apply a function to each element of the array, threading an accumulator argument
/// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> 
/// then computes <c>f (... (f i0 i1)...) iN</c>.
/// Raises <c>ArgumentException</c> if the array has size zero.
/// <summary>
/// <param name="reduction"></param>
/// <param name="resizeArray"></param>
/// <returns></returns>
[<CompiledName("Reduce")>]
let reduce (reduction : 'T -> 'T -> 'T) (resizeArray : ResizeArray<'T>) =
    // Preconditions
    checkNonNull "resizeArray" resizeArray
    if isEmpty resizeArray then
        invalidArg "resizeArray" "The ResizeArray is empty."

    let reduction = FSharpFunc<_,_,_>.Adapt reduction

    let mutable state = resizeArray.[0]
    let count = resizeArray.Count
    for i = 1 to count - 1 do
        state <- reduction.Invoke (state, resizeArray.[i])
    state

/// <summary>
/// Apply a function to each element of the array, threading an accumulator argument
/// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then 
/// computes <c>f i0 (...(f iN-1 iN))</c>.
/// Raises <c>ArgumentException</c> if the array has size zero.
/// </summary>
/// <param name="reduction"></param>
/// <param name="resizeArray"></param>
/// <returns></returns>
[<CompiledName("ReduceBack")>]
let reduceBack (reduction : 'T -> 'T -> 'T) (resizeArray : ResizeArray<'T>) : 'T =
    // Preconditions
    checkNonNull "resizeArray" resizeArray
    if isEmpty resizeArray then
        invalidArg "resizeArray" "The ResizeArray is empty."

    let reduction = FSharpFunc<_,_,_>.Adapt reduction

    let count = resizeArray.Count
    let mutable state = resizeArray.[count - 1]

    for i = count - 2 downto 0 do
        state <- reduction.Invoke (resizeArray.[i], state)
    state

/// <summary></summary>
/// <param name="folder"></param>
/// <param name="state"></param>
/// <param name="resizeArray"></param>
/// <param name="startIndex"></param>
/// <param name="endIndex"></param>
/// <returns></returns>
[<CompiledName("ScanSub")>]
let scanSub folder (state : 'State) (resizeArray : ResizeArray<'T>) startIndex endIndex : ResizeArray<'State> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray
    if startIndex < 0 then
        argOutOfRange "startIndex" "The starting index cannot be negative."
    elif endIndex < 0 then
        argOutOfRange "endIndex" "The ending index cannot be negative."
    
    let len = length resizeArray
    if startIndex >= len then
        argOutOfRange "startIndex" "The starting index is outside the bounds of the ResizeArray."
    elif endIndex >= len then
        argOutOfRange "endIndex" "The ending index is outside the bounds of the ResizeArray."

    let folder = FSharpFunc<_,_,_>.Adapt folder

    // Holds the initial and intermediate state values.
    let results = ResizeArray (endIndex - startIndex + 2)
    results.Add state

    // Fold over the specified range of items.
    let mutable state = state
    for i = startIndex to endIndex do
        state <- folder.Invoke (state, resizeArray.[i])
        results.Add state
    results

/// <summary>
/// Like <c>fold</c>, but return the intermediary and final results.
/// </summary>
/// <param name="folder"></param>
/// <param name="state"></param>
/// <param name="resizeArray"></param>
/// <returns></returns>
[<CompiledName("Scan")>]
let scan folder (state : 'State) (resizeArray : ResizeArray<'T>) : ResizeArray<'State> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    scanSub folder state resizeArray 0 (length resizeArray - 1)

/// <summary></summary>
/// <param name="folder"></param>
/// <param name="resizeArray"></param>
/// <param name="startIndex"></param>
/// <param name="endIndex"></param>
/// <param name="state"></param>
/// <returns></returns>
[<CompiledName("ScanBackSub")>]
let scanBackSub folder (resizeArray : ResizeArray<'T>) startIndex endIndex (state : 'State) : ResizeArray<'State> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray
    if startIndex < 0 then
        argOutOfRange "startIndex" "The starting index cannot be negative."
    elif endIndex < 0 then
        argOutOfRange "endIndex" "The ending index cannot be negative."
    
    let len = length resizeArray
    if startIndex >= len then
        argOutOfRange "startIndex" "The starting index is outside the bounds of the ResizeArray."
    elif endIndex >= len then
        argOutOfRange "endIndex" "The ending index is outside the bounds of the ResizeArray."

    let folder = FSharpFunc<_,_,_>.Adapt folder

    // Holds the initial and intermediate state values.
    let results = ResizeArray (endIndex - startIndex + 2)
    results.Add state

    // Fold over the specified range of items.
    let mutable state = state
    for i = endIndex downto startIndex do
        state <- folder.Invoke (resizeArray.[i], state)
        results.Insert (0, state)
    results

/// <summary>
/// Like <c>foldBack</c>, but return both the intermediary and final results.
/// </summary>
/// <param name="folder"></param>
/// <param name="resizeArray"></param>
/// <param name="state"></param>
/// <returns></returns>
[<CompiledName("ScanBack")>]
let scanBack folder (resizeArray : ResizeArray<'T>) (state : 'State) : ResizeArray<'State> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    scanBackSub folder resizeArray 0 (length resizeArray - 1) state

/// <summary>
/// Split the collection into two collections, containing the elements for which
/// the given predicate returns <c>true</c> and <c>false</c> respectively.
/// </summary>
/// <param name="predicate"></param>
/// <param name="resizeArray"></param>
/// <returns></returns>
[<CompiledName("Partition")>]
let partition predicate (resizeArray : ResizeArray<'T>) : ResizeArray<'T> * ResizeArray<'T> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let trueResults = ResizeArray ()
    let falseResults = ResizeArray ()

    let len = length resizeArray
    for i = 0 to len - 1 do
        let el = resizeArray.[i]
        if predicate el then
            trueResults.Add el
        else
            falseResults.Add el

    trueResults, falseResults

/// <summary>
/// Splits the collection into two (2) collections, containing the elements for which the
/// given function returns <c>Choice1Of2</c> or <c>Choice2Of2</c>, respectively. This function is similar to
/// <c>ResizeArray.partition</c>, but it allows the returned collections to have different element types.
/// </summary>
/// <param name="partitioner"></param>
/// <param name="resizeArray"></param>
/// <returns></returns>
[<CompiledName("MapPartition")>]
let mapPartition partitioner (resizeArray : ResizeArray<'T>) : ResizeArray<'U1> * ResizeArray<'U2> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let results1 = ResizeArray ()
    let results2 = ResizeArray ()

    let len = length resizeArray
    for i = 0 to len - 1 do
        match partitioner resizeArray.[i] with
        | Choice1Of2 value ->
            results1.Add value
        | Choice2Of2 value ->
            results2.Add value

    results1, results2

/// <summary>Returns the sum of the elements in the ResizeArray.</summary>
/// <param name="resizeArray">The input ResizeArray.</param>
/// <returns>The resulting sum.</returns>
[<CompiledName("Sum")>]
let inline sum (resizeArray : ResizeArray< ^T>) : ^T = 
    checkNonNull "resizeArray" resizeArray

    let mutable acc = LanguagePrimitives.GenericZero< (^T) >
//    for x in resizeArray do
//        acc <- Checked.(+) acc x
    for i = 0 to resizeArray.Count - 1 do
        acc <- Checked.(+) acc resizeArray.[i]
    acc

/// <summary>Returns the sum of the results generated by applying the function to each element of the ResizeArray.</summary>
/// <param name="projection">The function to transform the ResizeArray elements into the type to be summed.</param>
/// <param name="resizeArray">The input ResizeArray.</param>
/// <returns>The resulting sum.</returns>
[<CompiledName("SumBy")>]
let inline sumBy (projection : 'T -> ^U) (resizeArray : ResizeArray<'T>) : ^U = 
    checkNonNull "resizeArray" resizeArray

    let mutable acc = LanguagePrimitives.GenericZero< (^U) >
//    for x in resizeArray do
//        acc <- Checked.(+) acc (projection x)
    for i = 0 to resizeArray.Count - 1 do
        acc <- Checked.(+) acc (projection resizeArray.[i])
    acc

/// <summary>Returns the lowest of all elements of the ResizeArray, compared via Operators.min.</summary>
/// <param name="resizeArray">The input ResizeArray.</param>
/// <returns>The minimum element.</returns>
/// <exception cref="System.ArgumentException">Thrown when <paramref name="resizeArray"/> is empty.</exception>
[<CompiledName("Min")>]
let inline min (resizeArray : ResizeArray<'T>) =
    // Preconditions
    checkNonNull "resizeArray" resizeArray
    if resizeArray.Count = 0 then
        invalidArg "resizeArray" "The input collection is empty."

    let mutable acc = resizeArray.[0]
    for i = 1 to resizeArray.Count - 1 do
        let curr = resizeArray.[i]
        if curr < acc then
            acc <- curr
    acc

/// <summary>Returns the lowest of all elements of the ResizeArray, compared via Operators.min on the function result.</summary>
/// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
/// <param name="resizeArray">The input ResizeArray.</param>
/// <returns>The minimum element.</returns>
/// <exception cref="System.ArgumentException">Thrown when <paramref name="resizeArray"/> is empty.</exception>
[<CompiledName("MinBy")>]
let inline minBy (projection : 'T -> 'U) (resizeArray : ResizeArray<'T>) =
    // Preconditions
    checkNonNull "resizeArray" resizeArray
    if resizeArray.Count = 0 then
        invalidArg "resizeArray" "The input collection is empty."

    let mutable accv = resizeArray.[0]
    let mutable acc = projection accv
    for i = 1 to resizeArray.Count - 1 do
        let currv = resizeArray.[i]
        let curr = projection currv
        if curr < acc then
            acc <- curr
            accv <- currv
    accv

/// <summary>Returns the greatest of all elements of the ResizeArray, compared via Operators.max on the function result.</summary>
/// <param name="resizeArray">The input ResizeArray.</param>
/// <returns>The maximum element.</returns>
/// <exception cref="System.ArgumentException">Thrown when <paramref name="resizeArray"/> is empty.</exception>
[<CompiledName("Max")>]
let inline max (resizeArray : ResizeArray<'T>) =
    // Preconditions
    checkNonNull "resizeArray" resizeArray
    if resizeArray.Count = 0 then
        invalidArg "resizeArray" "The input collection is empty."

    let mutable acc = resizeArray.[0]
    for i = 1 to resizeArray.Count - 1 do
        let curr = resizeArray.[i]
        if curr > acc then
            acc <- curr
    acc

/// <summary>Returns the greatest of all elements of the ResizeArray, compared via Operators.max on the function result.</summary>
/// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
/// <param name="resizeArray">The input ResizeArray.</param>
/// <returns>The maximum element.</returns>
/// <exception cref="System.ArgumentException">Thrown when <paramref name="resizeArray"/> is empty.</exception>
[<CompiledName("MaxBy")>]
let inline maxBy (projection : 'T -> 'U) (resizeArray : ResizeArray<'T>) =
    // Preconditions
    checkNonNull "resizeArray" resizeArray
    if resizeArray.Count = 0 then
        invalidArg "resizeArray" "The input collection is empty."

    let mutable accv = resizeArray.[0]
    let mutable acc = projection accv
    for i = 1 to resizeArray.Count - 1 do
        let currv = resizeArray.[i]
        let curr = projection currv
        if curr > acc then
            acc <- curr
            accv <- currv
    accv

/// <summary>Returns the average of the elements in the ResizeArray.</summary>
/// <param name="resizeArray">The input ResizeArray.</param>
/// <returns>The average of the elements in the ResizeArray.</returns>
/// <exception cref="System.ArgumentException">Thrown when <paramref name="resizeArray"/> is empty.</exception>
[<CompiledName("Average")>]
let inline average (resizeArray : ResizeArray<'T>) : ^T =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    Seq.average resizeArray

/// <summary>Returns the average of the elements generated by applying the function to each element of the ResizeArray.</summary>
/// <param name="projection">The function to transform the ResizeArray elements before averaging.</param>
/// <param name="resizeArray">The input ResizeArray.</param>
/// <returns>The computed average.</returns>
/// <exception cref="System.ArgumentException">Thrown when <paramref name="resizeArray"/> is empty.</exception>
[<CompiledName("AverageBy")>]
let inline averageBy (projection : 'T -> ^U) (resizeArray : ResizeArray<'T>) : ^U = 
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    Seq.averageBy projection resizeArray
