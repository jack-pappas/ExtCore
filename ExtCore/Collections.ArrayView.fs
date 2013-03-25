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

/// Functional operators on ArrayViews.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExtCore.Collections.ArrayView

open LanguagePrimitives
open OptimizedClosures
open ExtCore


//
[<CompiledName("Array")>]
let inline array (view : ArrayView<'T>) =
    view.Array

//
[<CompiledName("Count")>]
let inline count (view : ArrayView<'T>) =
    view.Count

//
[<CompiledName("Offset")>]
let inline offset (view : ArrayView<'T>) =
    view.Offset

//
[<CompiledName("IsEmpty")>]
let inline isEmpty (view : ArrayView<'T>) =
    view.Count = 0

/// Creates an ArrayView spanning the entire length of an array.
[<CompiledName("OfArray")>]
let inline ofArray (array : 'T[]) : ArrayView<'T> =
    ArrayView (array)

/// Creates an ArrayView on an array, starting at the specified index.
let inline create (array : 'T[]) offset count : ArrayView<'T> =
    ArrayView (array, offset, count)

/// Gets an element of an ArrayView<'T>.
[<CompiledName("Get")>]
let inline get (view : ArrayView<'T>) index =
    view.[index]

/// Sets an element of an ArrayView<'T>.
[<CompiledName("Set")>]
let inline set (view : ArrayView<'T>) index value =
    view.[index] <- value

/// Gets the first element in an ArrayView<'T>.
[<CompiledName("First")>]
let inline first (view : ArrayView<'T>) =
    if isEmpty view then
        invalidOp "Cannot retrieve the first element of an empty ArrayView<'T>."
    else view.Array.[view.Offset]

/// Gets the index of the last element in an ArrayView<'T>, within the original array.
/// NOTE : This implemention is meant for internal use only, and does NOT perform bounds checking.
[<CompiledName("LastIndexUnsafe")>]
let inline private lastIndexUnsafe (view : ArrayView<'T>) =
    view.Offset + (view.Count - 1)

/// Gets the index of the last element in an ArrayView<'T>, within the original array.
[<CompiledName("LastIndex")>]
let inline lastIndex (view : ArrayView<'T>) =
    if isEmpty view then
        invalidOp "The ArrayView<'T> is empty."
    else lastIndexUnsafe view

/// Gets the last element in an ArrayView<'T>.
[<CompiledName("Last")>]
let inline last (view : ArrayView<'T>) =
    if isEmpty view then
        invalidOp "Cannot retrieve the last element of an empty ArrayView<'T>."
    else view.Array.[lastIndexUnsafe view]

/// Builds a new array from the elements within the ArrayView<'T>.
[<CompiledName("ToArray")>]
let toArray (view : ArrayView<'T>) =
    if isEmpty view then
        Array.empty
    else
        view.Array.[view.Offset .. (lastIndexUnsafe view)]

//
[<CompiledName("MapToArray")>]
let mapToArray (mapping : 'T -> 'U) (view : ArrayView<'T>) =
    if isEmpty view then
        Array.empty
    else
        //
        let lastIndex = lastIndexUnsafe view
        //
        let results = Array.zeroCreate (count view)

        //
        let arr = array view
        for i = (offset view) to lastIndex do
            results.[i] <- mapping arr.[i]

        // Return the mapped results.
        results

//
[<CompiledName("TryPick")>]
let tryPick picker (view : ArrayView<'T>) : 'U option =
    // OPTIMIZATION : Use imperative/mutable style for maximum performance.
    let array = view.Array
    /// The last index (inclusive) in the underlying array which belongs to this ArrayView.
    let endIndex = lastIndexUnsafe view

    let mutable matchResult = None
    let mutable index = view.Offset

    while Option.isNone matchResult && index <= endIndex do
        match picker array.[index] with
        | Some result ->
            matchResult <- result
        | None ->
            index <- index + 1

    // Return the result (if a match was found) or None.
    matchResult

//
[<CompiledName("Pick")>]
let pick picker (view : ArrayView<'T>) : 'U =
    // Call tryPick to find the value; if no match is found, raise an exception.
    match tryPick picker view with
    | Some result ->
        result
    | None ->
        // TODO : Provide a better error message
        //keyNotFound ""
        raise <| System.Collections.Generic.KeyNotFoundException ()

//
[<CompiledName("TryFind")>]
let tryFind predicate (view : ArrayView<'T>) =
    // OPTIMIZATION : Use imperative/mutable style for maximum performance.
    let array = view.Array
    /// The last index (inclusive) in the underlying array which belongs to this ArrayView.
    let endIndex = lastIndexUnsafe view

    let mutable matchedElement = None
    let mutable index = view.Offset

    while Option.isNone matchedElement && index <= endIndex do
        let el = array.[index]
        if predicate el then
            matchedElement <- Some el
        else
            index <- index + 1

    // Return the result (if a match was found) or None.
    matchedElement

//
[<CompiledName("Find")>]
let find predicate (view : ArrayView<'T>) =
    // Call tryFind to find the value; if no match is found, raise an exception.
    match tryFind predicate view with
    | Some element ->
        element
    | None ->
        // TODO : Provide a better error message
        //keyNotFound ""
        raise <| System.Collections.Generic.KeyNotFoundException ()

//
[<CompiledName("TryFindIndex")>]
let tryFindIndex predicate (view : ArrayView<'T>) =
    // OPTIMIZATION : Use imperative/mutable style for maximum performance.
    let array = view.Array
    /// The last index (inclusive) in the underlying array which belongs to this ArrayView.
    let endIndex = lastIndexUnsafe view

    let mutable matchedIndex = None
    let mutable index = view.Offset

    while Option.isNone matchedIndex && index <= endIndex do
        if predicate array.[index] then
            matchedIndex <- Some index
        else
            index <- index + 1

    // Return the result (if a match was found) or None.
    matchedIndex

//
[<CompiledName("FindIndex")>]
let findIndex predicate (view : ArrayView<'T>) =
    // Call tryFindIndex to find the value; if no match is found, raise an exception.
    match tryFindIndex predicate view with
    | Some index ->
        index
    | None ->
        // TODO : Provide a better error message
        //keyNotFound ""
        raise <| System.Collections.Generic.KeyNotFoundException ()

//
[<CompiledName("Iterate")>]
let iter action (view : ArrayView<'T>) =
    // OPTIMIZATION : Use imperative/mutable style for maximum performance.
    let array = view.Array
    /// The last index (inclusive) in the underlying array which belongs to this ArrayView.
    let endIndex = lastIndexUnsafe view

    for i = view.Offset to endIndex do
        action array.[i]

//
[<CompiledName("Exists")>]
let exists predicate (view : ArrayView<'T>) =
    // OPTIMIZATION : Use imperative/mutable style for maximum performance.
    let array = view.Array
    /// The last index (inclusive) in the underlying array which belongs to this ArrayView.
    let endIndex = lastIndexUnsafe view

    let mutable foundMatchingElement = false
    let mutable index = view.Offset

    while not foundMatchingElement && index <= endIndex do
        foundMatchingElement <- predicate array.[index]
        index <- index + 1

    // Return the value indicating if any element matched the predicate.
    foundMatchingElement

//
[<CompiledName("Forall")>]
let forall predicate (view : ArrayView<'T>) =
    // OPTIMIZATION : Use imperative/mutable style for maximum performance.
    let array = view.Array
    /// The last index (inclusive) in the underlying array which belongs to this ArrayView.
    let endIndex = lastIndexUnsafe view

    let mutable allElementsMatched = true
    let mutable index = view.Offset

    while allElementsMatched && index <= endIndex do
        allElementsMatched <- predicate array.[index]
        index <- index + 1

    // Return the value indicating if all elements matched the predicate.
    allElementsMatched

//
[<CompiledName("Fold")>]
let fold folder (state : 'State) (view : ArrayView<'T>) =
    let folder = FSharpFunc<_,_,_>.Adapt folder

    // OPTIMIZATION : Use imperative/mutable style for maximum performance.
    let array = view.Array
    /// The last index (inclusive) in the underlying array which belongs to this ArrayView.
    let endIndex = lastIndexUnsafe view

    let mutable state = state
    for i = view.Offset to endIndex do
        state <- folder.Invoke (state, array.[i])

    // Return the final state value.
    state

//
[<CompiledName("FoldBack")>]
let foldBack folder (view : ArrayView<'T>) (state : 'State) =
    let folder = FSharpFunc<_,_,_>.Adapt folder

    // OPTIMIZATION : Use imperative/mutable style for maximum performance.
    let array = view.Array
    /// The last index (inclusive) in the underlying array which belongs to this ArrayView.
    let endIndex = lastIndexUnsafe view

    let mutable state = state
    for i = endIndex downto view.Offset do
        state <- folder.Invoke (array.[i], state)

    // Return the final state value.
    state

//
[<CompiledName("Reduce")>]
let reduce (reduction : 'T -> 'T -> 'T) (view : ArrayView<'T>) =
    // Preconditions
    if isEmpty view then
        invalidArg "view" "Cannot reduce an empty ArrayView<'T>."

    // Create a new array segment which excludes the first element
    // of the input segment, then call 'fold' with it.
    let segment' = ArrayView (view.Array, view.Offset + 1, view.Count - 1)
    fold reduction view.[0] segment'

//
[<CompiledName("ReduceBack")>]
let reduceBack (reduction : 'T -> 'T -> 'T) (view : ArrayView<'T>) =
    // Preconditions
    if isEmpty view then
        invalidArg "view" "Cannot reduce an empty ArrayView<'T>."

    // Create a new array segment which excludes the last element
    // of the input segment, then call 'foldBack' with it.
    let segment' = ArrayView (view.Array, view.Offset, view.Count - 1)
    foldBack reduction segment' view.[view.Count - 1]

//
[<CompiledName("ToList")>]
let toList (segment : ArrayView<'T>) =
    // OPTIMIZATION : If the segment is empty return immediately.
    if isEmpty segment then []
    else
        // Fold backwards so we don't need to reverse the list
        // we create -- it'll already be in the correct order.
        (segment, [])
        ||> foldBack (fun el list ->
            el :: list)

//
[<CompiledName("Minimum")>]
let min<'T when 'T : comparison> (segment : ArrayView<'T>) =
    // Preconditions
    if isEmpty segment then
        invalidArg "segment" "Cannot compute the minimum element of an empty ArrayView<'T>."

    reduce min segment

//
[<CompiledName("Maximum")>]
let max<'T when 'T : comparison> (segment : ArrayView<'T>) =
    // Preconditions
    if isEmpty segment then
        invalidArg "segment" "Cannot compute the maximum element of an empty ArrayView<'T>."

    reduce max segment

//
[<CompiledName("Sum")>]
let inline sum (segment : ArrayView<'T>) =
    // Preconditions
    if isEmpty segment then
        invalidArg "segment" "Cannot compute the sum of an empty ArrayView<'T>."

    reduce (+) segment
