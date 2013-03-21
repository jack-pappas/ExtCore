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


//
type IMapReduction<'Key, 'T> =
    //
    abstract Map : 'Key -> 'T
    //
    abstract Reduce : 'T -> 'T -> 'T    // or 'T * 'T -> 'T

// TODO : Implement a MapReduction module:
// MapReduction.fromFunctions


/// Functional operators over a range of values.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Range =
    open LanguagePrimitives

    (* NOTE :   In the functions below, 'start' and 'finish'
                are *inclusive*, just like the F# 'for' loop. *)

    //
    [<CompiledName("Iterate")>]
    let inline iter (action : ^T -> unit) start finish : unit =
        let mutable index = start
        while index <= finish do
            action index
            index <- index + GenericOne

    //
    [<CompiledName("Fold")>]
    let inline fold (folder : ^State -> ^T -> ^State) start finish state : ^State =
        let mutable state = state
        let mutable index = start
        while index <= finish do
            state <- folder state index
            index <- index + GenericOne
        state

    //
    [<CompiledName("FoldBack")>]
    let inline foldBack (folder : ^T -> ^State -> ^State) start finish state : ^State =
        let mutable state = state
        let mutable index = finish
        while index >= start do
            state <- folder index state
            index <- index - GenericOne
        state

    //
    [<CompiledName("Exists")>]
    let inline exists (predicate : ^T -> bool) start finish : bool =
        let mutable foundMatch = false
        let mutable index = start
        while index <= finish && not foundMatch do
            foundMatch <- predicate index
            index <- index + GenericOne
        foundMatch

    //
    [<CompiledName("Forall")>]
    let inline forall (predicate : ^T -> bool) start finish : bool =
        let mutable allMatch = true
        let mutable index = start
        while index <= finish && allMatch do
            allMatch <- predicate index
            index <- index + GenericOne
        allMatch

    // TODO
    // mapReduce


/// Functional programming operators related to the System.Collections.Generic.KeyValuePair<_,_> type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module KeyValuePair =
    open System.Collections.Generic

    /// Gets the key in the key/value pair.
    [<CompiledName("Key")>]
    let inline key (kvp : KeyValuePair<'Key, 'T>) =
        kvp.Key

    /// Gets the value in the key/value pair.
    [<CompiledName("Value")>]
    let inline value (kvp : KeyValuePair<'Key, 'T>) =
        kvp.Value


/// Additional functional operators on sequences.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Seq =
    //
    [<CompiledName("AppendSingleton")>]
    let inline appendSingleton sequence (value : 'T) =
        Seq.append sequence (Seq.singleton value)

    //
    [<CompiledName("ProjectValues")>]
    let projectValues (mapping : 'Key -> 'T) (source : seq<'Key>) =
        // Preconditions
        checkNonNull "source" source

        source
        |> Seq.map (fun x ->
            x, mapping x)

    //
    [<CompiledName("ProjectKeys")>]
    let projectKeys (mapping : 'T -> 'Key) (source : seq<'T>) =
        // Preconditions
        checkNonNull "source" source

        source
        |> Seq.map (fun x ->
            mapping x, x)

    (* TODO

    Seq.choosei

    Seq.segment
        Groups elements of a sequence together "longitudinally" -- i.e., it works
        in a streaming fashion, rather than Seq.groupBy which needs to see the
        entire stream before returning. Alternatively, this can be thought of
        as a generalized form of Seq.windowed.

    Seq.sample
        Takes a positive integer and a sequence.
        Returns a sequence containing every n-th element of the input sequence.

    *)


/// Additional functional operators on immutable lists.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module List =
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

    //
    [<CompiledName("TryHead")>]
    let inline tryHead (list : 'T list) =
        match list with
        | [] -> None
        | hd :: _ ->
            Some hd

    //
    [<CompiledName("OfOption")>]
    let inline ofOption (value : 'T option) =
        match value with
        | None -> []
        | Some x -> [x]

    //
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

    //
    [<CompiledName("Indexed")>]
    let indexed (list : 'T list) =
        // Preconditions
        checkNonNull "list" list

        list |> List.mapi (fun i x -> i, x)

    /// Converts a list into an array (similar to List.toArray) but copies the elements into
    /// the array from right-to-left, so there's no need to call List.rev before List.toArray.
    [<CompiledName("ReverseIntoArray")>]
    let revIntoArray (list : 'T list) =
        // Preconditions
        checkNonNull "list" list

        let len = List.length list
        let results = Array.zeroCreate len

        let rec loop (idx, lst) =
            match lst with
            | [] -> results
            | hd :: tl ->
                results.[idx] <- hd
                loop (idx - 1, tl)

        loop (len - 1, list)

    /// <summary>Applies the given function to each element of a list,
    /// and copies the results into an array from right-to-left so the
    /// produced array represents the mapped original list in reverse order.</summary>
    /// <remarks><para>This represents an optimized version of:
    /// <c>fun mapping -> (List.map mapping) >> List.rev >> List.toArray</c>.</para></remarks>
    [<CompiledName("MapAndReverseIntoArray")>]
    let mapAndRevIntoArray (mapping : 'T -> 'U) (list : 'T list) =
        // Preconditions
        checkNonNull "list" list

        let len = List.length list
        let results = Array.zeroCreate len

        let rec loop (idx, lst) =
            match lst with
            | [] -> results
            | hd :: tl ->
                results.[idx] <- mapping hd
                loop (idx - 1, tl)

        loop (len - 1, list)

    //
    [<CompiledName("ProjectValues")>]
    let projectValues (projection : 'Key -> 'T) (list : 'Key list) =
        // Preconditions
        checkNonNull "list" list

        list |> List.map (fun x -> x, projection x)

    //
    [<CompiledName("ProjectKeys")>]
    let projectKeys (projection : 'T -> 'Key) (list : 'T list) =
        // Preconditions
        checkNonNull "list" list

        list |> List.map (fun x -> projection x, x)

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

    //
    [<CompiledName("FoldPairs")>]
    let foldPairs (folder : 'State -> 'T -> 'T -> 'State) state list =
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

    //
    [<CompiledName("FoldPairsBack")>]
    let foldPairsBack (folder : 'T -> 'T -> 'State -> 'State) state list =
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

    //
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

    //
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

    //
    [<CompiledName("Choose2")>]
    let choose2 (chooser : 'T1 -> 'T2 -> 'U option) list1 list2 : 'U list =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2
        // OPTIMIZE : Instead of checking List.length on both lists (which is O(n)),
        // just detect mismatched lengths on-the-fly.
        if List.length list1 <> List.length list2 then
            invalidArg "list2" "The lists have different lengths."

        let chooser = FSharpFunc<_,_,_>.Adapt chooser

        let mutable list1 = list1
        let mutable list2 = list2
        let mutable resultList = []

        while not <| List.isEmpty list1 do
            match chooser.Invoke (List.head list1, List.head list2) with
            | None -> ()
            | Some result ->
                resultList <- result :: resultList

            list1 <- List.tail list1
            list2 <- List.tail list2

        // Reverse the result list before returning.
        List.rev resultList

    //
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

    //
    [<CompiledName("ZipWith")>]
    let zipWith (mapping : 'T1 -> 'T2 -> 'U) list1 list2 : 'U list =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2
        // OPTIMIZE : Instead of checking List.length on both lists (which is O(n)),
        // just detect mismatched lengths on-the-fly.
        if List.length list1 <> List.length list2 then
            invalidArg "list2" "The lists have different lengths."

        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        
        let mutable list1 = list1
        let mutable list2 = list2
        let mutable resultList = []

        while not <| List.isEmpty list1 do
            resultList <-
                mapping.Invoke (List.head list1, List.head list2) :: resultList
            list1 <- List.tail list1
            list2 <- List.tail list2

        // Reverse the result list before returning.
        List.rev resultList

    //
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


    (* TODO

    List.choosei

    List.unzipMap
        Similar to List.map2 (and List.zipMap, below). Given a list of tuples (a, b)
        applies the elements of each tuple to a function 'f' (f a b).
    List.zipMap
        Similar to List.map2, but combines the elements into a tuple before
        applying them to the mapping function; this optimizes for the case where
        we have List.zip immediately followed by List.map, or to simplify code
        which needs to create the tuple and "manually" apply it to a function.

    *)


/// Additional functional operators on arrays.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array =
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


#if PROTO_COMPILER
// TODO : Re-implement this module using the F# proto compiler, as in the TagMap and TagSet modules.
// This will avoid the need to create "concrete" implementations of these functions, as we'll be able
// to simply re-use existing implementations for the standard array type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TaggedArray =
    /// Similar to Array.mapi, but 'tags' the index values with a unit-of-measure
    /// type before applying them to the mapping function.
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int<'Tag> -> 'T -> 'U) (array : 'T[]) =
        // Preconditions
        checkNonNull "array" array
        
        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        let len = array.Length
        let results = Array.zeroCreate len
        for i = 0 to len - 1 do
            results.[i] <- mapping.Invoke (Tag.ofInt<'Tag> i, array.[i])
        results

    /// Similar to Array.mapi, but 'tags' the index values with a unit-of-measure
    /// type before applying them to the mapping function.
    [<CompiledName("MapIndexed")>]
    let mapi2 (mapping : int<'Tag> -> 'T1 -> 'T2 -> 'U) (array1 : 'T1[]) (array2 : 'T2[]) =
        // Preconditions
        checkNonNull "array1" array1
        checkNonNull "array2" array2
        
        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
        let len1 = array1.Length 
        if len1 <> array2.Length then
            invalidArg "array2" "The arrays have differing lengths."

        let results = Array.zeroCreate len1
        for i = 0 to len1 - 1 do
            results.[i] <- mapping.Invoke (Tag.ofInt<'Tag> i, array1.[i], array2.[i])
        results

    /// Applies a function to each element of the collection, threading an accumulator argument through the computation.
    /// The integer index passed to the function indicates the array index of the element being transformed.
    /// The index values are tagged with a unit-of-measure type before applying them to the folder function.
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : int<'Tag> -> 'State -> 'T -> 'State) (state : 'State) (array : 'T[]) =
        // Preconditions
        checkNonNull "array" array
        
        let folder = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt folder
        let mutable state = state
        let len = Array.length array
        for i = 0 to len - 1 do
            state <- folder.Invoke (Tag.ofInt<'Tag> i, state, array.[i])
        state

    /// Applies a function to each element of the collection, threading an accumulator argument through the computation.
    /// The integer index passed to the function indicates the array index of the element being transformed.
    /// The index values are tagged with a unit-of-measure type before applying them to the folder function.
    [<CompiledName("FoldBackIndexed")>]
    let foldiBack (folder : int<'Tag> -> 'T -> 'State -> 'State) (array : 'T[]) (state : 'State) : 'State =
        // Preconditions
        checkNonNull "array" array

        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        
        let mutable state = state
        for i = Array.length array - 1 downto 0 do
            state <- folder.Invoke (Tag.ofInt<'Tag> i, array.[i], state)
        state
#endif


/// Functional operators on ArrayViews.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ArrayView =
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

    // TODO
    // minBy
    // maxBy    
    // sumBy   


/// Additional functional operators on sets.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Set =
    //
    [<CompiledName("OfArrayView")>]
    let ofArrayView (view : ArrayView<'T>) : Set<'T> =
        (Set.empty, view)
        ||> ArrayView.fold (fun set el ->
            Set.add el set)

    //
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : int -> 'State -> 'T -> 'State) (state : 'State) (set : Set<'T>) =
        // Preconditions
        checkNonNull "set" set

        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        let mutable state = state
        let mutable idx = 0
        for el in set do
            state <- folder.Invoke (idx, state, el)
            idx <- idx + 1
        state

    //
    [<CompiledName("MapToArray")>]
    let mapToArray (mapping : 'T -> 'U) (set : Set<'T>) =
        // Preconditions
        checkNonNull "set" set

        let results = Array.zeroCreate <| Set.count set
        let mutable idx = 0
        for el in set do
            results.[idx] <- mapping el
            idx <- idx + 1
        results

    /// Creates a set given a number of items in the set and a generator function.
    [<CompiledName("Initialize")>]
    let init count (initializer : int -> 'T) =
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

    //
    [<CompiledName("ExtractMinimum")>]
    let extractMin (set : Set<'T>) =
        // Preconditions
        checkNonNull "set" set
        if Set.isEmpty set then
            invalidArg "set" "The set is empty."

        let minElement = Set.minElement set
        let set = Set.remove minElement set
        minElement, set

    //
    [<CompiledName("ExtractMaximum")>]
    let extractMax (set : Set<'T>) =
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
    let reduce (reduction : 'T -> 'T -> 'T) (set : Set<'T>) =
        // Preconditions
        checkNonNull "set" set
        if Set.isEmpty set then
            invalidArg "set" "The set is empty."

        let minElement, set = extractMin set
        Set.fold reduction minElement set

    /// Reduces elements in a set in order from the greatest (maximum) element
    /// to the least (minimum) element.
    [<CompiledName("ReduceBack")>]
    let reduceBack (reduction : 'T -> 'T -> 'T) (set : Set<'T>) =
        // Preconditions
        checkNonNull "set" set
        if Set.isEmpty set then
            invalidArg "set" "The set is empty."

        let maxElement, set = extractMax set
        Set.foldBack reduction set maxElement

    //
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

    //
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

    //
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
            raise <| System.Collections.Generic.KeyNotFoundException ()

    //
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

    //
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
            raise <| System.Collections.Generic.KeyNotFoundException ()

    //
    [<CompiledName("MapPartition")>]
    let mapPartition (partitioner : 'T -> Choice<'U, 'V>) set =
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

    /// <summary>Computes the exclusive disjunction ("exclusive-or") of two sets.</summary>
    /// <remarks>
    /// The exclusive disjunction ("exclusive-or") operation creates
    /// a new set whose elements belong to exactly one (1) of the input sets.
    /// </remarks>
    [<CompiledName("ExclusiveOr")>]
    let xor (set1 : Set<'T>) (set2 : Set<'T>) : Set<'T> =
        // Preconditions
        checkNonNull "set1" set1
        checkNonNull "set2" set2

        // OPTIMIZATION : If either set is empty return immediately.
        if Set.isEmpty set1 then
            set2
        elif Set.isEmpty set2 then
            set1
        else
            // OPTIMIZE : Check to see which set is larger and take
            // an optimized code path depending on the result.
            // For now, we assume 'set1' is the larger of the two sets.

            // Remove the elements in set2 from set1.
            let set1' = Set.difference set1 set2

            // Remove the elements in set1 from set2.
            // We use set1' instead of set1' because it is a subset of set1
            // and having fewer elements makes this operation faster.
            let set2' = Set.difference set2 set1'

            // The XOR is the union of the two results.
            Set.union set1' set2'

    /// The Cartesian product of two sets.
    [<CompiledName("Cartesian")>]
    let cartesian (set1 : Set<'T>) (set2 : Set<'U>) =
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

    //
    [<CompiledName("MapIntersect")>]
    let mapIntersect (mapping : 'T -> Set<'U>) set =
        // Preconditions
        checkNonNull "set" set

        // OPTIMIZATION : If the set is empty return immediately.
        if Set.isEmpty set then
            Set.empty
        else
            // To compute the intersection, we must start with the mapped set
            // for the first element -- if we used a "standard" fold and started
            // with Set.empty, the result would always be Set.empty.

            let minElement, set = extractMin set
            let mappedMinElement = mapping minElement

            (mappedMinElement, set)
            ||> Set.fold (fun intersection el ->
                mapping el
                |> Set.intersect intersection)

    //
    [<CompiledName("MapUnion")>]
    let mapUnion (mapping : 'T -> Set<'U>) set =
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


    //
    [<RequireQualifiedAccess>]
    module Cartesian =
        //
        [<CompiledName("Fold")>]
        let fold (folder : 'State -> 'T -> 'U -> 'State) state set1 set2 =
            // Preconditions
            checkNonNull "set1" set1
            checkNonNull "set2" set2

            let folder = FSharpFunc<_,_,_,_>.Adapt folder
            
            (state, set1)
            ||> Set.fold (fun state x ->
                (state, set2)
                ||> Set.fold (fun state y ->
                    folder.Invoke (state, x, y)))

        //
        [<CompiledName("FoldBack")>]
        let foldBack (folder : 'T -> 'U -> 'State -> 'State) set1 set2 state =
            // Preconditions
            checkNonNull "set1" set1
            checkNonNull "set2" set2

            let folder = FSharpFunc<_,_,_,_>.Adapt folder

            (set1, state)
            ||> Set.foldBack (fun x state ->
                (set2, state)
                ||> Set.foldBack (fun y state ->
                    folder.Invoke (x, y, state)))

        //
        [<CompiledName("Iterate")>]
        let iter (action : 'T -> 'U -> unit) set1 set2 =
            // Preconditions
            checkNonNull "set1" set1
            checkNonNull "set2" set2

            let action = FSharpFunc<_,_,_>.Adapt action

            set1
            |> Set.iter (fun x ->
                set2
                |> Set.iter (fun y ->
                    action.Invoke (x, y)))

        //
        [<CompiledName("Map")>]
        let map (mapping : 'T -> 'U -> 'V) set1 set2 =
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

        //
        [<CompiledName("Choose")>]
        let choose (chooser : 'T -> 'U -> 'V option) set1 set2 =
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


/// Additional functional operators on maps.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Map =
    open System.Collections.Generic

    /// Determines the number of items in the Map.
    [<CompiledName("Count")>]
    let inline count (map : Map<'Key, 'Value>) =
        map.Count

    //
    [<CompiledName("FindOrDefault")>]
    let inline findOrDefault defaultValue key (map : Map<'Key, 'T>) =
        defaultArg (Map.tryFind key map) defaultValue

    //
    [<CompiledName("Singleton")>]
    let inline singleton key value : Map<'Key, 'T> =
        Map.empty
        |> Map.add key value

    //
    [<CompiledName("Keys")>]
    let keys (map : Map<'Key, 'T>) =
        // Preconditions
        checkNonNull "map" map

        (Set.empty, map)
        ||> Map.fold (fun keys key _ ->
            Set.add key keys)

    //
    [<CompiledName("Values")>]
    let values (map : Map<'Key, 'T>) =
        // Preconditions
        checkNonNull "map" map

        (Set.empty, map)
        ||> Map.fold (fun values _ value ->
            Set.add value values)

    //
    [<CompiledName("AddKvp")>]
    let inline addKvp (kvp : KeyValuePair<'Key, 'T>) map =
        // Preconditions
        checkNonNull "map" map

        Map.add kvp.Key kvp.Value map

    //
    [<CompiledName("FromKvpSequence")>]
    let ofKvpSeq (sequence : seq<KeyValuePair<'Key, 'T>>) =
        // Preconditions
        checkNonNull "sequence" sequence

        (Map.empty, sequence)
        ||> Seq.fold (fun map kvp ->
            addKvp kvp map)

    //
    [<CompiledName("FromKeys")>]
    let ofKeys (mapping : 'Key -> 'T) (set : Set<'Key>) : Map<'Key, 'T> =
        // Preconditions
        checkNonNull "set" set

        // OPTIMIZATION : If the set is empty return immediately.
        if Set.isEmpty set then
            Map.empty
        else
            (Map.empty, set)
            ||> Set.fold (fun map key ->
                Map.add key (mapping key) map)

    //
    [<CompiledName("FromValues")>]
    let ofValues (mapping : 'T -> 'Key) (set : Set<'T>) : Map<'Key, 'T> =
        // Preconditions
        checkNonNull "set" set

        // OPTIMIZATION : If the set is empty return immediately.
        if Set.isEmpty set then
            Map.empty
        else
            (Map.empty, set)
            ||> Set.fold (fun map value ->
                Map.add (mapping value) value map)

    //
    [<CompiledName("RemoveKeys")>]
    let removeKeys keys (map : Map<'Key, 'T>) =
        // Preconditions
        checkNonNull "keys" keys
        checkNonNull "map" map

        // OPTIMIZATION : If either the key set or input map is empty return immediately.
        if Set.isEmpty keys then
            map
        elif Map.isEmpty map then
            Map.empty
        else
            (map, keys)
            ||> Set.fold (fun map key ->
                Map.remove key map)

    //
    [<CompiledName("SelectKeys")>]
    let selectKeys keys (map : Map<'Key, 'T>) =
        // Preconditions
        checkNonNull "keys" keys
        checkNonNull "map" map

        // OPTIMIZATION : If either the key set or input map is empty return immediately.
        if Set.isEmpty keys || Map.isEmpty map then
            Map.empty
        else
            map
            |> Map.filter (fun key _ ->
                Set.contains key keys)

    //
    [<CompiledName("Choose")>]
    let choose (chooser : 'Key -> 'T -> 'U option) map =
        // Preconditions
        checkNonNull "map" map

        // OPTIMIZATION : If the input map is empty return immediately.
        if Map.isEmpty map then
            Map.empty
        else
            let chooser = FSharpFunc<_,_,_>.Adapt chooser

            (Map.empty, map)
            ||> Map.fold (fun chosenMap key value ->
                match chooser.Invoke (key, value) with
                | None ->
                    chosenMap
                | Some newValue ->
                    Map.add key newValue chosenMap)

    //
    [<CompiledName("MapPartition")>]
    let mapPartition (partitioner : 'Key -> 'T -> Choice<'U, 'V>) map =
        // Preconditions
        checkNonNull "map" map

        // OPTIMIZATION : If the map is empty return immediately.
        if Map.isEmpty map then
            Map.empty, Map.empty
        else
            let partitioner = FSharpFunc<_,_,_>.Adapt partitioner

            let mutable resultMap1 = Map.empty
            let mutable resultMap2 = Map.empty

            for kvp in map do
                match partitioner.Invoke (kvp.Key, kvp.Value) with
                | Choice1Of2 value ->
                    resultMap1 <- Map.add kvp.Key value resultMap1
                | Choice2Of2 value ->
                    resultMap2 <- Map.add kvp.Key value resultMap2

            resultMap1, resultMap2

    /// Combines two maps into a single map.
    /// Whenever a key exists in both maps, the first map's entry will be added to the result map.
    [<CompiledName("Union")>]
    let union (map1 : Map<'Key, 'T>) (map2 : Map<'Key, 'T>) : Map<'Key, 'T> =
        // Preconditions
        checkNonNull "map1" map1
        checkNonNull "map2" map2

        match map1.Count, map2.Count with
        // Optimize for empty inputs
        | 0, 0 ->
            Map.empty
        | 0, _ ->
            map2
        | _, 0 ->
            map1
        | _, _ ->
            // Start with the second map.
            // Fold over the first map, adding it's entries to the second
            // and overwriting any existing entries.
            (map2, map1)
            ||> Map.fold (fun combinedMap key value ->
                Map.add key value combinedMap)

    /// Combines two maps into a single map.
    /// Whenever a key exists in both maps, the specified function is used to determine the
    /// value to be used for that key in the combined map.
    [<CompiledName("Join")>]
    let join (joiner : 'Key -> 'T -> 'T -> 'T) (map1 : Map<'Key, 'T>) (map2 : Map<'Key, 'T>) : Map<'Key, 'T> =
        // Preconditions
        checkNonNull "map1" map1
        checkNonNull "map2" map2

        match map1.Count, map2.Count with
        // Optimize for empty inputs
        | 0, 0 ->
            Map.empty
        | 0, _ ->
            map2
        | _, 0 ->
            map1
        | _, _ ->
            let joiner = FSharpFunc<_,_,_,_>.Adapt joiner

            // Partition the second map into two maps -- one containing the entries which conflict
            // with the first map, and another whose entries don't exist in the first map.
            let conflicting, unique =
                map2
                |> Map.partition (fun key _ ->
                    Map.containsKey key map1)

            // Add the unique entries from the second map to the first map.
            let joined =
                (map1, unique)
                ||> Map.fold (fun joined key value ->
                    Map.add key value joined)

            // Now, add the conflicting entries into the joined map, using the joiner function
            // to determine which value should be used for the conflicting key.
            (joined, conflicting)
            ||> Map.fold (fun joined key value2 ->
                /// The first map's value for this conflicting key.
                let value1 = Map.find key map1

                /// The joined value for this key.
                let joinedValue = joiner.Invoke (key, value1, value2)

                // Add the joined value to the map.
                Map.add key joinedValue joined)

    //
    [<CompiledName("Pivot")>]
    let pivot (map : Map<'Key, 'T>) : Map<'T, Set<'Key>> =
        // Preconditions
        checkNonNull "map" map

        // OPTIMIZATION : If the input map is empty return immediately.
        if Map.isEmpty map then
            Map.empty
        else
            (Map.empty, map)
            ||> Map.fold (fun pivotMap key value ->
                /// The key set for this value.
                let keySet =
                    match Map.tryFind value pivotMap with
                    | Some keySet ->
                        Set.add key keySet
                    | None ->
                        Set.singleton key

                // Add/update the pivot map entry for this value.
                Map.add value keySet pivotMap)

    //
    // Combines Map.ofKeys and Map.pivot to avoid creating intermediate data structures.
    [<CompiledName("PivotKeySet")>]
    let pivotKeySet (mapping : 'Key -> 'T) (set : Set<'Key>) : Map<'T, Set<'Key>> =
        // Preconditions
        checkNonNull "set" set

        // OPTIMIZATION : If the set is empty return immediately.
        if Set.isEmpty set then
            Map.empty
        else
            (Map.empty, set)
            ||> Set.fold (fun pivotMap key ->
                /// The value for this key.
                let value = mapping key

                /// The key set for this value.
                let keySet =
                    match Map.tryFind value pivotMap with
                    | Some keySet ->
                        Set.add key keySet
                    | None ->
                        Set.singleton key

                // Add/update the key-set for this value in the pivot map.
                Map.add value keySet pivotMap)

    // Like Map.add, but doesn't overwrite an existing entry.
    [<CompiledName("TryAdd")>]
    let tryAdd key value (map : Map<'Key, 'T>) : Map<'Key, 'T> =
        // Preconditions
        checkNonNull "map" map

        if Map.containsKey key map then
            // Return the original map.
            map
        else
            // Add the new entry.
            Map.add key value map

    // Like Map.add, but only overwrites the value of an existing entry
    // (i.e., it won't create a new entry in the map).
    [<CompiledName("TryUpdate")>]
    let tryUpdate key value (map : Map<'Key, 'T>) : Map<'Key, 'T> =
        // Preconditions
        checkNonNull "map" map

        if Map.containsKey key map then
            // Overwrite the value of the existing entry.
            Map.add key value map
        else
            // Return the original map.
            map

    // TODO
    // mapi, mapiBack
    // foldi, foldiBack
    // reduce, reduceBack
    // update       // Similar to 'tryUpdate' but raises an exception if there's no existing entry.


//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ResizeArray =
    open System.Collections.Generic

    //
    [<CompiledName("Length")>]
    let inline length (resizeArray : ResizeArray<'T>) =
        resizeArray.Count

    //
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (resizeArray : ResizeArray<'T>) =
        resizeArray.Count = 0

    //
    [<CompiledName("Add")>]
    let inline add item (resizeArray : ResizeArray<'T>) =
        resizeArray.Add item

    //
    [<CompiledName("ToArray")>]
    let inline toArray (resizeArray : ResizeArray<'T>) =
        resizeArray.ToArray ()

    //
    [<CompiledName("OfArray")>]
    let inline ofArray (arr : 'T[]) : ResizeArray<'T> =
        ResizeArray (arr)

    //
    [<CompiledName("OfSeq")>]
    let inline ofSeq (sequence : seq<'T>) : ResizeArray<'T> =
        ResizeArray (sequence)

    //
    [<CompiledName("Get")>]
    let inline get (resizeArray : ResizeArray<'T>) index =
        resizeArray.[index]

    //
    [<CompiledName("Set")>]
    let inline set (resizeArray : ResizeArray<'T>) index value =
        resizeArray.[index] <- value

    //
    [<CompiledName("SortInPlace")>]
    let inline sortInPlace<'T when 'T : comparison> (resizeArray : ResizeArray<'T>) =
        resizeArray.Sort ()
        
    //
    [<CompiledName("SortInPlaceBy")>]
    let inline sortInPlaceBy<'T, 'Key when 'Key : comparison>
            (projection : 'T -> 'Key) (resizeArray : ResizeArray<'T>) =
        resizeArray.Sort (fun x y ->
            compare (projection x) (projection y))

    //
    [<CompiledName("SortInPlaceWith")>]
    let inline sortInPlaceWith (comparer : 'T -> 'T -> int) (resizeArray : ResizeArray<'T>) =
        resizeArray.Sort (comparer)

    //
    [<CompiledName("Contains")>]
    let inline contains (value : 'T) (resizeArray : ResizeArray<'T>) =
        // Preconditions
        checkNonNull "resizeArray" resizeArray

        resizeArray.Contains value

    //
    [<CompiledName("Filter")>]
    let inline filter (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : ResizeArray<'T> =
        // Preconditions
        checkNonNull "resizeArray" resizeArray

        resizeArray.FindAll (System.Predicate predicate)

    //
    [<CompiledName("Exists")>]
    let inline exists (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : bool =
        // Preconditions
        checkNonNull "resizeArray" resizeArray

        resizeArray.Exists (System.Predicate predicate)

    //
    [<CompiledName("Forall")>]
    let inline forall (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : bool =
        // Preconditions
        checkNonNull "resizeArray" resizeArray

        resizeArray.TrueForAll (System.Predicate predicate)

    //
    [<CompiledName("TryFindIndex")>]
    let tryFindIndex (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : int option =
        // Preconditions
        checkNonNull "resizeArray" resizeArray

        match resizeArray.FindIndex (System.Predicate predicate) with
        | -1 ->
            None
        | index ->
            Some index

    //
    [<CompiledName("TryFind")>]
    let tryFind (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : 'T option =
        // Preconditions
        checkNonNull "resizeArray" resizeArray

        match resizeArray.FindIndex (System.Predicate predicate) with
        | -1 ->
            None
        | index ->
            Some resizeArray.[index]
        
    //
    [<CompiledName("FindIndex")>]
    let findIndex (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : int =
        // Preconditions
        checkNonNull "resizeArray" resizeArray

        match resizeArray.FindIndex (System.Predicate predicate) with
        | -1 ->
            // TODO : Add a better error message.
            // keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()
        | index ->
            index

    //
    [<CompiledName("Find")>]
    let find (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : 'T =
        // Preconditions
        checkNonNull "resizeArray" resizeArray

        match resizeArray.FindIndex (System.Predicate predicate) with
        | -1 ->
            // TODO : Add a better error message.
            // keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()
        | index ->
            resizeArray.[index]

    //
    [<CompiledName("Map")>]
    let inline map (mapping : 'T -> 'U) (resizeArray : ResizeArray<'T>) =
        // Preconditions
        checkNonNull "resizeArray" resizeArray

        resizeArray.ConvertAll (System.Converter mapping)

    //
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> 'U) (resizeArray : ResizeArray<'T>) =
        // Preconditions
        checkNonNull "resizeArray" resizeArray

        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        let count = resizeArray.Count
        let result = ResizeArray (count)

        for i = 0 to count - 1 do
            result.Add <| mapping.Invoke (i, resizeArray.[i])
        result

    //
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> unit) (resizeArray : ResizeArray<'T>) =
        // Preconditions
        checkNonNull "resizeArray" resizeArray

        let count = resizeArray.Count
        for i = 0 to count - 1 do
            action resizeArray.[i]

    //
    [<CompiledName("IterateIndexed")>]
    let iteri (action : int -> 'T -> unit) (resizeArray : ResizeArray<'T>) =
        // Preconditions
        checkNonNull "resizeArray" resizeArray

        let action = FSharpFunc<_,_,_>.Adapt action

        let count = resizeArray.Count
        for i = 0 to count - 1 do
            action.Invoke (i, resizeArray.[i])

    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> 'State) state (resizeArray : ResizeArray<'T>) =
        // Preconditions
        checkNonNull "resizeArray" resizeArray

        let folder = FSharpFunc<_,_,_>.Adapt folder

        let mutable state = state
        let count = resizeArray.Count
        for i = 0 to count - 1 do
            state <- folder.Invoke (state, resizeArray.[i])
        state

    //
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : int -> 'State -> 'T -> 'State) state (resizeArray : ResizeArray<'T>) =
        // Preconditions
        checkNonNull "resizeArray" resizeArray

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        let mutable state = state
        let count = resizeArray.Count
        for i = 0 to count - 1 do
            state <- folder.Invoke (i, state, resizeArray.[i])
        state

    //
    [<CompiledName("FoldBack")>]
    let foldBack (folder : 'T -> 'State -> 'State) (resizeArray : ResizeArray<'T>) state =
        // Preconditions
        checkNonNull "resizeArray" resizeArray

        let folder = FSharpFunc<_,_,_>.Adapt folder

        let mutable state = state
        for i = resizeArray.Count - 1 downto 0 do
            state <- folder.Invoke (resizeArray.[i], state)
        state

    //
    [<CompiledName("FoldBackIndexed")>]
    let foldiBack (folder : int -> 'T -> 'State -> 'State) (resizeArray : ResizeArray<'T>) state =
        // Preconditions
        checkNonNull "resizeArray" resizeArray

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        let mutable state = state
        for i = resizeArray.Count - 1 downto 0 do
            state <- folder.Invoke (i, resizeArray.[i], state)
        state

    //
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

    //
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

    //
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

    //
    [<CompiledName("TryPick")>]
    let tryPick (picker : 'T -> 'U option) (resizeArray : ResizeArray<'T>) : 'U option =
        // Preconditions
        checkNonNull "resizeArray" resizeArray

        let count = resizeArray.Count
        let mutable result = None
        let mutable index = 0

        while index < count && Option.isNone result do
            result <- picker resizeArray.[index]
        result

    //
    [<CompiledName("Pick")>]
    let pick (picker : 'T -> 'U option) (resizeArray : ResizeArray<'T>) : 'U =
        // Preconditions
        checkNonNull "resizeArray" resizeArray

        let count = resizeArray.Count
        let mutable result = None
        let mutable index = 0

        while index < count && Option.isNone result do
            result <- picker resizeArray.[index]

        match result with
        | Some result ->
            result
        | None ->
            // TODO : Return a better error message
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()


/// Functional programming operators related to the System.Collections.Generic.IDictionary type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Dict =
    open System.Collections.Generic

    /// Thread-safe functional programming operators for mutable instances of System.Collections.Generic.IDictionary.
    module Safe =
        /// Attempts to retrieve the value associated with the specified key.
        [<CompiledName("TryFind")>]
        let tryFind key (dict : IDictionary<'Key, 'T>) =
            // Preconditions
            checkNonNull "dict" dict

            match lock dict <| fun () -> dict.TryGetValue key with
            | false, _ ->
                None
            | true, value ->
                Some value
        
        /// Lookup an element in the Dictionary, raising KeyNotFoundException if
        /// the Dictionary does not contain an element with the specified key.
        [<CompiledName("Find")>]
        let inline find key (dict : IDictionary<'Key, 'T>) =
            // Preconditions
            checkNonNull "dict" dict

            lock dict <| fun () -> dict.[key]

        /// Adds a new entry to the Dictionary.
        [<CompiledName("Add")>]
        let add key value (dict : IDictionary<'Key, 'T>) =
            // Preconditions
            checkNonNull "dict" dict

            if dict.IsReadOnly then
                invalidOp "Cannot add an entry to a read-only dictionary."
            lock dict <| fun () ->
                dict.Add (key, value)

        /// Updates an existing entry in the dictionary with a new value,
        /// raising KeyNotFoundException if the Dictionary does not
        /// contain an element with the specified key.
        [<CompiledName("Update")>]
        let update key value (dict : IDictionary<'Key, 'T>) =
            // Preconditions
            checkNonNull "dict" dict

            if dict.IsReadOnly then
                invalidOp "Cannot update an entry in a read-only dictionary."
            lock dict <| fun () ->
                if dict.ContainsKey key then
                    dict.[key] <- value
                else
                    // TODO : Return a better error message
                    //keyNotFound ""
                    raise <| System.Collections.Generic.KeyNotFoundException ()

        /// Removes the entry with the specified key from the Dictionary,
        /// returning a value indicating the success of the operation.
        [<CompiledName("Remove")>]
        let remove (key : 'Key) (dict : IDictionary<'Key, 'T>) =
            // Preconditions
            checkNonNull "dict" dict

            if dict.IsReadOnly then
                invalidOp "Cannot remove an entry from a read-only dictionary."
            lock dict <| fun () ->
                dict.Remove key

        /// Updates the value of an entry (which has the specified key)
        /// in the Dictionary, or creates a new entry if one doesn't exist.
        /// If the Dictionary is read-only, an InvalidOperationException is raised.
        [<CompiledName("UpdateOrAdd")>]
        let updateOrAdd key value (dict : IDictionary<'Key, 'T>) =
            // Preconditions
            checkNonNull "dict" dict
            
            if dict.IsReadOnly then
                invalidOp "Cannot update or add an entry to a read-only dictionary."
            lock dict <| fun () ->
                dict.[key] <- value

        /// Creates an immutable copy of a Dictionary.
        /// The entries are shallow-copied to the created Dictionary; that is,
        /// reference-typed keys and values will reference the same instances as
        /// in the mutable Dictionary, so care should be taken when using mutable keys and values.
        [<CompiledName("Immutable")>]
        let immutable (dictionary : IDictionary<'Key, 'T>) =
            // Preconditions
            checkNonNull "dictionary" dictionary

            lock dictionary <| fun () ->
                dictionary
                |> Seq.map (fun kvp ->
                    kvp.Key, kvp.Value)
                |> dict


    /// Views the keys of the Dictionary as a sequence.
    [<CompiledName("Keys")>]
    let inline keys (dictionary : IDictionary<'Key, 'T>) =
        dictionary.Keys :> IEnumerable<'Key>

    /// Views the values of the Dictionary as a sequence.
    [<CompiledName("Values")>]
    let inline values (dictionary : IDictionary<'Key, 'T>) =
        dictionary.Values :> IEnumerable<'T>

    /// Determines whether the Dictionary is empty.
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (dictionary : IDictionary<'Key,'T>) =
        dictionary.Count = 0

    /// Gets the number of entries in the Dictionary.
    [<CompiledName("Count")>]
    let inline count (dictionary : IDictionary<'Key, 'T>) =
        dictionary.Count

    /// Creates a mutable dictionary with the specified capacity.
    [<CompiledName("CreateMutable")>]
    let inline createMutable<'Key, 'T when 'Key : equality> (capacity : int) =
        System.Collections.Generic.Dictionary<'Key, 'T> (capacity)

    /// Determines whether the Dictionary contains an element with the specified key.
    [<CompiledName("ContainsKey")>]
    let inline containsKey k (dictionary : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dictionary" dictionary

        dictionary.ContainsKey k

    /// Adds a new entry to the dictionary.
    [<CompiledName("Add")>]
    let inline add k v (dictionary : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dictionary" dictionary

        dictionary.Add (k, v)
        dictionary

    /// Removes the entry with the specified key from the Dictionary.
    /// An exception is raised if the entry cannot be removed.
    [<CompiledName("Remove")>]
    let inline remove (k : 'Key) (dictionary : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dictionary" dictionary

        if dictionary.Remove k then
            dictionary
        else
            let msg = sprintf "Unable to remove the entry with the key '%O' from the dictionary." k
            raise <| exn msg

    /// Lookup an element in the Dictionary, raising KeyNotFoundException if
    /// the dictionary does not contain an element with the specified key.
    [<CompiledName("Find")>]
    let inline find k (dictionary : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dictionary" dictionary

        dictionary.[k]
            
    /// Attempts to retrieve the value associated with the specified key.
    [<CompiledName("TryFind")>]
    let inline tryFind k (dictionary : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dictionary" dictionary

        match dictionary.TryGetValue k with
        | false, _ -> None
        | true, v -> Some v

    /// Updates the value of an entry (which has the specified key) in the Dictionary.
    /// Raises a KeyNotFoundException if the Dictionary does not contain an entry with the specified key.
    [<CompiledName("Update")>]
    let update k v (dictionary : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dictionary" dictionary

        if dictionary.ContainsKey k then
            dictionary.[k] <- v
            dictionary
        else
            // TODO : Add an error message which includes the key.
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    /// Updates the value of an entry (which has the specified key) in
    /// the Dictionary, or creates a new entry if one doesn't exist.
    [<CompiledName("UpdateOrAdd")>]
    let inline updateOrAdd k v (dictionary : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dictionary" dictionary
        
        dictionary.[k] <- v
        dictionary    

    /// Applies the given function to successive entries, returning the
    /// first result where the function returns "Some(x)".
    [<CompiledName("TryPick")>]
    let tryPick picker (dictionary : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dictionary" dictionary

        let picker = FSharpFunc<_,_,_>.Adapt picker
        
        dictionary
        |> Seq.tryPick (fun kvp ->
            picker.Invoke (kvp.Key, kvp.Value))

    /// Applies the given function to sucecssive entries, returning the
    /// first x where the function returns "Some(x)".
    [<CompiledName("Pick")>]
    let pick picker (dictionary : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dictionary" dictionary

        match tryPick picker dictionary with
        | Some res -> res
        | None ->
            // TODO : Add an error message which includes the key.
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    /// Views the Dictionary as a sequence of tuples.
    [<CompiledName("ToSeq")>]
    let toSeq (dictionary : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dictionary" dictionary

        dictionary
        |> Seq.map (fun kvp ->
            kvp.Key, kvp.Value)

    /// Applies the given function to each entry in the Dictionary.
    [<CompiledName("Iterate")>]
    let iter (action : 'Key -> 'T -> unit) (dictionary : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dictionary" dictionary

        let action = FSharpFunc<_,_,_>.Adapt action

        dictionary
        |> Seq.iter (fun kvp ->
            action.Invoke (kvp.Key, kvp.Value))

    /// Returns a new Dictionary containing only the entries of the
    /// Dictionary for which the predicate returns 'true'.
    [<CompiledName("Filter")>]
    let filter (predicate : 'Key -> 'T -> bool) (dictionary : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dictionary" dictionary

        let predicate = FSharpFunc<_,_,_>.Adapt predicate

        dictionary
        |> Seq.choose (fun kvp ->
            if predicate.Invoke (kvp.Key, kvp.Value) then
                Some (kvp.Key, kvp.Value)
            else None)
        |> dict

    /// Builds a new Dictionary whose entries are the results of applying
    /// the given function to each element of the Dictionary.
    [<CompiledName("Map")>]
    let map (mapping : 'Key -> 'T -> 'U) (dictionary : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dictionary" dictionary

        let mapping = FSharpFunc<_,_,_>.Adapt mapping

        dictionary
        |> Seq.map (fun kvp ->
            kvp.Key, mapping.Invoke (kvp.Key, kvp.Value))
        |> dict

    /// Applies the given function to each element of the Dictionary.
    /// Returns a Dictionary comprised of the results "x,y" for each
    /// entry where the function returns Some(y).
    [<CompiledName("Choose")>]
    let choose (chooser : 'Key -> 'T -> 'U option) (dictionary : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dictionary" dictionary

        let chooser = FSharpFunc<_,_,_>.Adapt chooser        
        
        dictionary
        |> Seq.choose (fun kvp ->
            chooser.Invoke (kvp.Key, kvp.Value)
            |> Option.map (fun x -> kvp.Key, x))
        |> dict

    /// Applies a function to each entry of the Dictionary,
    /// threading an accumulator argument through the computation.
    [<CompiledName("Fold")>]
    let fold folder (state : 'State) (dict : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dict" dict

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        (state, dict)
        ||> Seq.fold (fun state kvp ->
            folder.Invoke (state, kvp.Key, kvp.Value))

    /// Splits the Dictionary into two Dictionaries, containing the entries
    /// for which the given predicate evaluates to "true" and "false".
    [<CompiledName("Partition")>]
    let partition partitioner (dictionary : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dictionary" dictionary

        let partitioner = FSharpFunc<_,_,_>.Adapt partitioner

        let t, f =
            ((Seq.empty, Seq.empty), dictionary)
            ||> fold (fun (trueSeq, falseSeq) k v ->
                if partitioner.Invoke (k, v) then
                    (Seq.append trueSeq (Seq.singleton (k, v)), falseSeq)
                else
                    (trueSeq, Seq.append falseSeq (Seq.singleton (k, v))))

        dict t, dict f

    //
    [<CompiledName("Readonly")>]
    let readonly (dictionary : IDictionary<'Key, 'T>) =
        dict <| toSeq dictionary

