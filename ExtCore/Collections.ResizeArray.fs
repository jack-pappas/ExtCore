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


(* Functions below this point still need to be cleaned up. *)

//
[<CompiledName("Concat")>]
let concat (arrs: ResizeArray<'T> list) = new ResizeArray<_> (seq { for arr in arrs do for x in arr do yield x })
    
//
[<CompiledName("Append")>]
let append (arr1: ResizeArray<'T>) (arr2: ResizeArray<'T>) = concat [arr1; arr2]

//
[<CompiledName("Unzip")>]
let sub (arr: ResizeArray<'T>) start len =
    if start < 0 then invalidArg "start" "index must be positive"
    if len < 0 then invalidArg "len" "length must be positive"
    if start + len > length arr then invalidArg "len" "length must be positive"
    new ResizeArray<_> (seq { for i in start .. start+len-1 -> arr.[i] })

//
[<CompiledName("Fill")>]
let fill (arr: ResizeArray<'T>) (start: int) (len: int) (x:'T) =
    if start < 0 then invalidArg "start" "index must be positive"
    if len < 0 then invalidArg "len" "length must be positive"
    if start + len > length arr then invalidArg "len" "length must be positive"
    for i = start to start + len - 1 do 
        arr.[i] <- x

//
[<CompiledName("Copy")>]
let inline copy (arr: ResizeArray<'T>) = ResizeArray<_>(arr)

let inline private indexNotFound () =
    keyNotFound "An index satisfying the predicate was not found in the collection"

//
[<CompiledName("Iter2")>]
let iter2 f (arr1: ResizeArray<'T>) (arr2: ResizeArray<'b>) = 
    let f = FSharpFunc<_,_,_>.Adapt(f)
    let len1 = length arr1
    if len1 <> length arr2 then invalidArg "arr2" "the arrays have different lengths"
    for i = 0 to len1 - 1 do 
        f.Invoke(arr1.[i], arr2.[i])

//
[<CompiledName("Map2")>]
let map2 f (arr1: ResizeArray<'T>) (arr2: ResizeArray<'b>) = 
    let f = FSharpFunc<_,_,_>.Adapt(f)
    let len1 = length arr1
    if len1 <> length arr2 then invalidArg "arr2" "the arrays have different lengths"
    let res = new ResizeArray<_>(len1)
    for i = 0 to len1 - 1 do
        res.Add(f.Invoke(arr1.[i], arr2.[i]))
    res

//
[<CompiledName("Rev")>]
let rev (arr: ResizeArray<_>) = 
    let len = length arr 
    let res = new ResizeArray<_>(len)
    for i = len - 1 downto 0 do 
        res.Add(arr.[i])
    res

//
[<CompiledName("Exists2")>]
let exists2 f (arr1: ResizeArray<_>) (arr2: ResizeArray<_>) =
    let len1 = length arr1
    if len1 <> length arr2 then invalidArg "arr2" "the arrays have different lengths"
    let rec loop i = i < len1 && (f arr1.[i] arr2.[i] || loop (i+1))
    loop 0

//
[<CompiledName("FindIndexIndexed")>]
let findIndexi f (arr: ResizeArray<_>) =
    let rec go n = if n >= length arr then indexNotFound() elif f n arr.[n] then n else go (n+1)
    go 0

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
[<CompiledName("Forall2")>]
let forall2 f (arr1: ResizeArray<_>) (arr2: ResizeArray<_>) = 
    let len1 = length arr1
    if len1 <> length arr2 then invalidArg "arr2" "the arrays have different lengths"
    let rec loop i = i >= len1 || (f arr1.[i] arr2.[i] && loop (i+1))
    loop 0
    
//
[<CompiledName("IterateIndexed2")>]
let iteri2 f (arr1: ResizeArray<'T>) (arr2: ResizeArray<'b>) =
    let f = FSharpFunc<_,_,_,_>.Adapt(f)
    let len1 = length arr1
    if len1 <> length arr2 then invalidArg "arr2" "the arrays have different lengths"
    for i = 0 to len1 - 1 do 
        f.Invoke(i,arr1.[i], arr2.[i])

//
[<CompiledName("MapIndexed2")>]
let mapi2 (f: int -> 'T -> 'b -> 'c) (arr1: ResizeArray<'T>) (arr2: ResizeArray<'b>) = 
    let f = FSharpFunc<_,_,_,_>.Adapt(f)
    let len1 = length arr1
    if len1 <> length arr2 then invalidArg "arr2" "the arrays have different lengths"
    init len1 (fun i -> f.Invoke(i, arr1.[i], arr2.[i]))

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
[<CompiledName("Singleton")>]
let singleton x =
    let res = new ResizeArray<_>(1)
    res.Add(x)
    res
        
//
[<CompiledName("TryFindIndexIndexed")>]
let tryFindIndexi f (arr: ResizeArray<'T>) = 
    let rec go n = if n >= length arr then None elif f n arr.[n] then Some n else go (n+1)
    go 0
    
//
[<CompiledName("Zip")>]
let zip (arr1: ResizeArray<_>) (arr2: ResizeArray<_>) = 
    let len1 = length arr1 
    if len1 <> length arr2 then invalidArg "arr2" "the arrays have different lengths"
    init len1 (fun i -> arr1.[i], arr2.[i])

//
[<CompiledName("Unzip")>]
let unzip (arr: ResizeArray<_>) = 
    let len = length arr
    let res1 = new ResizeArray<_>(len)
    let res2 = new ResizeArray<_>(len)
    for i = 0 to len - 1 do 
        let x,y = arr.[i] 
        res1.Add(x)
        res2.Add(y)
    res1,res2

//
[<CompiledName("Blit")>]
let blit (arr1: ResizeArray<'T>) start1 (arr2: ResizeArray<'T>) start2 len =
    if start1 < 0 then invalidArg "start1" "index must be positive"
    if start2 < 0 then invalidArg "start2" "index must be positive"
    if len < 0 then invalidArg "len" "length must be positive"
    if start1 + len > length arr1 then invalidArg "start1" "(start1+len) out of range"
    if start2 + len > length arr2 then invalidArg "start2" "(start2+len) out of range"
    for i = 0 to len - 1 do 
        arr2.[start2+i] <- arr1.[start1 + i]

//
[<CompiledName("Partition")>]
let partition f (arr: ResizeArray<_>) = 
    let res1 = new ResizeArray<_>()
    let res2 = new ResizeArray<_>()
    for i = 0 to length arr - 1 do 
        let x = arr.[i] 
        if f x then res1.Add(x) else res2.Add(x)
    res1, res2