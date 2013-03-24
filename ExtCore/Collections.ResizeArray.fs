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

//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExtCore.Collections.ResizeArray

open System.Collections.Generic
open LanguagePrimitives
open OptimizedClosures
open ExtCore


//
[<CompiledName("Length")>]
let inline length (resizeArray : ResizeArray<'T>) =
    resizeArray.Count

//
[<CompiledName("IsEmpty")>]
let inline isEmpty (resizeArray : ResizeArray<'T>) =
    resizeArray.Count = 0

//
[<CompiledName("Get")>]
let inline get (resizeArray : ResizeArray<'T>) index =
    resizeArray.[index]

//
[<CompiledName("Set")>]
let inline set (resizeArray : ResizeArray<'T>) index value =
    resizeArray.[index] <- value

//
[<CompiledName("Create")>]
let create count value : ResizeArray<'T> =
    // Preconditions
    if count < 0 then
        invalidArg "count" "The number of elements may not be negative."

    let resizeArray = ResizeArray (count)
    for i = 0 to count - 1 do
        resizeArray.Add value
    resizeArray

//
[<CompiledName("Init")>]
let init count initializer : ResizeArray<'T> =
    // Preconditions
    if count < 0 then
        invalidArg "count" "The number of elements may not be negative."

    let resizeArray = ResizeArray (count)
    for i = 0 to count - 1 do
        resizeArray.Add <| initializer count
    resizeArray

//
[<CompiledName("Add")>]
let inline add item (resizeArray : ResizeArray<'T>) =
    resizeArray.Add item

//
[<CompiledName("Contains")>]
let inline contains (value : 'T) (resizeArray : ResizeArray<'T>) =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    resizeArray.Contains value

//
[<CompiledName("OfSeq")>]
let inline ofSeq (sequence : seq<'T>) : ResizeArray<'T> =
    ResizeArray (sequence)

//
[<CompiledName("ToSeq")>]
let toSeq (resizeArray : ResizeArray<'T>) : seq<'T> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    Seq.readonly resizeArray

//
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

//
[<CompiledName("ToList")>]
let toList (resizeArray : ResizeArray<'T>) : 'T list =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let mutable res = []
    for i = length resizeArray - 1 downto 0 do
        res <- resizeArray.[i] :: res
    res

//
[<CompiledName("OfArray")>]
let inline ofArray (arr : 'T[]) : ResizeArray<'T> =
    ResizeArray (arr)

//
[<CompiledName("ToArray")>]
let inline toArray (resizeArray : ResizeArray<'T>) =
    resizeArray.ToArray ()

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
[<CompiledName("Iter")>]
let iter (action : 'T -> unit) (resizeArray : ResizeArray<'T>) =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let count = resizeArray.Count
    for i = 0 to count - 1 do
        action resizeArray.[i]

//
[<CompiledName("IterIndexed")>]
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

//
[<CompiledName("Concat")>]
let concat (resizeArrays : seq<ResizeArray<'T>>) : ResizeArray<'T> =
    // Preconditions
    checkNonNull "resizeArrays" resizeArrays

    let flattened = ResizeArray ()
    for resizeArray in resizeArrays do
        flattened.AddRange resizeArray
    flattened
    
//
[<CompiledName("Append")>]
let append (resizeArray1 : ResizeArray<'T>) (resizeArray2 : ResizeArray<'T>) : ResizeArray<'T> =
    // Preconditions
    checkNonNull "resizeArray1" resizeArray1
    checkNonNull "resizeArray2" resizeArray2

    let combined = ResizeArray (resizeArray1.Count + resizeArray2.Count)
    combined.AddRange resizeArray1
    combined.AddRange resizeArray2
    combined

//
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

//
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

//
[<CompiledName("Copy")>]
let inline copy (resizeArray : ResizeArray<'T>) : ResizeArray<'T> =
    ResizeArray (resizeArray)

//
[<CompiledName("Singleton")>]
let singleton value : ResizeArray<'T> =
    let resizeArray = ResizeArray ()
    resizeArray.Add value
    resizeArray

//
[<CompiledName("Iter2")>]
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

//
[<CompiledName("Map2")>]
let map2 mapping (resizeArray1 : ResizeArray<'T1>) (resizeArray2 : ResizeArray<'T2>)
    : ResizeArray<'U> =
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

//
[<CompiledName("Rev")>]
let rev (resizeArray : ResizeArray<'T>) : ResizeArray<'T> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let len = length resizeArray
    let result = ResizeArray (len)
    for i = len - 1 downto 0 do
        result.Add resizeArray.[i]
    resizeArray

//
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

//
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
    let mutable allMatch = false
    while index < len && allMatch do
        allMatch <- predicate.Invoke (resizeArray1.[index], resizeArray2.[index])
        index <- index + 1
    allMatch

//
[<CompiledName("IterIndexed2")>]
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

//
[<CompiledName("MapIndexed2")>]
let mapi2 mapping (resizeArray1 : ResizeArray<'T1>) (resizeArray2 : ResizeArray<'T2>)
    : ResizeArray<'U> =
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

//
[<CompiledName("TryFindIndexIndexed")>]
let tryFindIndexi predicate (resizeArray : ResizeArray<'T>) : int option =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let predicate = FSharpFunc<_,_,_>.Adapt predicate

    let len = length resizeArray
    let mutable index = -1
    let mutable foundMatch = false
    while index < len && not foundMatch do
        index <- index + 1
        foundMatch <- predicate.Invoke (index, resizeArray.[index])

    if foundMatch then
        Some index
    else None

//
[<CompiledName("FindIndexIndexed")>]
let findIndexi predicate (resizeArray : ResizeArray<'T>) : int =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    match tryFindIndexi predicate resizeArray with
    | Some index ->
        index
    | None ->
        keyNotFound "An element satisfying the predicate was not found in the collection."

//
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

//
[<CompiledName("Zip")>]
let zip (resizeArray1 : ResizeArray<'T1>) (resizeArray2 : ResizeArray<'T2>)
    : ResizeArray<'T1 * 'T2> =
    // Preconditions
    checkNonNull "resizeArray1" resizeArray1
    checkNonNull "resizeArray2" resizeArray2

    let len = length resizeArray1
    if len = length resizeArray2 then
        invalidArg "resizeArray2" "The ResizeArrays have different lengths."

    let results = ResizeArray (len)
    for i = 0 to len - 1 do
        results.Add (resizeArray1.[i], resizeArray2.[i])
    results

//
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

//
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

(* Functions below this point still need to be cleaned up. *)

//
[<CompiledName("FoldSub")>]
let foldSub f acc (arr: ResizeArray<_>) start fin = 
    let mutable res = acc
    for i = start to fin do
        res <- f res arr.[i] 
    res

//
[<CompiledName("FoldBackSub")>]
let foldBackSub f (arr: ResizeArray<_>) start fin acc = 
    let mutable res = acc 
    for i = fin downto start do
        res <- f arr.[i] res
    res

//
[<CompiledName("Fold2")>]
let fold2 f (acc: 'T) (arr1: ResizeArray<'T1>) (arr2: ResizeArray<'T2>) =
    let f = FSharpFunc<_,_,_,_>.Adapt(f)
    let mutable res = acc 
    let len = length arr1
    if len <> length arr2 then invalidArg "arr2" "the arrays have different lengths"
    for i = 0 to len - 1 do
        res <- f.Invoke(res,arr1.[i],arr2.[i])
    res

//
[<CompiledName("FoldBack2")>]
let foldBack2 f (arr1: ResizeArray<'T1>) (arr2: ResizeArray<'T2>) (acc: 'b) =
    let f = FSharpFunc<_,_,_,_>.Adapt(f)
    let mutable res = acc 
    let len = length arr1
    if len <> length arr2 then invalidArg "arr2" "the arrays have different lengths"
    for i = len - 1 downto 0 do 
        res <- f.Invoke(arr1.[i],arr2.[i],res)
    res

//
[<CompiledName("Scan")>]
let scan f acc (arr : ResizeArray<'T>) = 
    let arrn = length arr
    scanSub f acc arr 0 (arrn - 1)

//
[<CompiledName("ScanBack")>]
let scanBack f (arr : ResizeArray<'T>) acc = 
    let arrn = length arr
    scanBackSub f arr 0 (arrn - 1) acc

//
[<CompiledName("ScanSub")>]
let scanSub f  acc (arr : ResizeArray<'T>) start fin = 
    let f = FSharpFunc<_,_,_>.Adapt(f)
    let mutable state = acc
    let res = create (fin-start+2) acc
    for i = start to fin do
        state <- f.Invoke(state, arr.[i])
        res.[i - start+1] <- state
    res
    
//
[<CompiledName("ScanBackSub")>]
let scanBackSub f (arr: ResizeArray<'T>) start fin acc = 
    let f = FSharpFunc<_,_,_>.Adapt(f)
    let mutable state = acc
    let res = create (2+fin-start) acc
    for i = fin downto start do
        state <- f.Invoke(arr.[i], state)
        res.[i - start] <- state
    res

