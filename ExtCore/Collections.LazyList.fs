(*

Copyright 2005-2009 Microsoft Corporation
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

open System
open System.Collections.Generic
open LanguagePrimitives
open OptimizedClosures
open ExtCore

#nowarn "21" // recursive initialization
#nowarn "40" // recursive initialization


//
exception UndefinedException

//
type
    [<NoEquality; NoComparison>]
    internal LazyCellStatus<'T> =
        | Delayed of (unit -> LazyListCell<'T>)
        | Value of LazyListCell<'T>
        | Exception of exn

// OPTIMIZE : Would there be any benefit to applying [<UseNullAsTrueValue>] here
// so CellEmpty would be represented as null?
and [<NoEquality; NoComparison>]
    internal LazyListCell<'T> =
        | Empty
        | Cons of 'T * LazyList<'T>

/// LazyLists are possibly-infinite, cached sequences.  See also IEnumerable/Seq for
/// uncached sequences. LazyLists normally involve delayed computations without 
/// side-effects.  The results of these computations are cached and evaluations will be 
/// performed only once for each element of the lazy list.  In contrast, for sequences 
/// (IEnumerable) recomputation happens each time an enumerator is created and the sequence 
/// traversed.
///
/// LazyLists can represent cached, potentially-infinite computations.  Because they are 
/// cached they may cause memory leaks if some active code or data structure maintains a 
/// live reference to the head of an infinite or very large lazy list while iterating it, 
/// or if a reference is maintained after the list is no longer required.
///
/// Lazy lists may be matched using the LazyList.Cons and LazyList.Nil active patterns. 
/// These may force the computation of elements of the list.
and [<NoEquality; NoComparison>]
    LazyList<'T> internal (initialStatus) =
    //
    let mutable status : LazyCellStatus<'T> = initialStatus
    
    member internal this.Value =
        match status with
        | Value value ->
            value
        | _ ->
            lock this <| fun () ->
                match status with
                | Delayed f ->
                    status <- Exception UndefinedException
                    try
                        let res = f ()
                        status <- Value res
                        res
                    with ex ->
                        status <- Exception ex
                        reraise ()
                | Value value ->
                    value
                | Exception ex ->
                    raise ex

    /// Return the first element of the list.
    /// Forces the evaluation of the first cell of the list if it is not already evaluated.
    member this.Head
        with get () =
            match this.Value with
            | Cons (hd, _) -> hd
            | Empty ->
                invalidArg "s" "The list is empty."

    /// Return the list corresponding to the remaining items in the sequence.
    /// Forces the evaluation of the first cell of the list if it is not already evaluated.
    member this.Tail
        with get () =
            match this.Value with
            | Cons (_, tl) -> tl
            | Empty ->
                invalidArg "s" "The list is empty."

    /// Test if a list is empty.
    /// Forces the evaluation of the first element of the stream if it is not already evaluated.
    member this.IsEmpty
        with get () =
            match this.Value with
            | Empty -> true
            | Cons _ -> false

    /// Creates a sequence which enumerates the values in the LazyList.
    member this.ToSeq () =
        this
        |> Seq.unfold (fun list ->
            match list.Value with
            | Empty ->
                None
            | Cons (hd, tl) ->
                Some (hd, tl))
            
    interface IEnumerable<'T> with
        member this.GetEnumerator () =
            this.ToSeq().GetEnumerator ()

    interface System.Collections.IEnumerable with
        override this.GetEnumerator () =
            this.ToSeq().GetEnumerator ()
            :> System.Collections.IEnumerator

//
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LazyList =
    /// Curried form of the Cons constructor.
    let inline private consc value (list : LazyList<'T>) =
        assert (not <| isNull list)
        Cons (value, list)

    //
    let inline private lzy cellCreator : LazyList<'T> =
        LazyList (Delayed cellCreator)

    //
    let inline private getCell (list : LazyList<'T>) =
        list.Value

    /// Evaluates to the list that contains no items.
    [<GeneralizableValue>]
    [<CompiledName("Empty")>]
    let empty<'T> : LazyList<'T> =
        LazyList (Value Empty)
    
    /// Get the first cell of the list.
    [<CompiledName("TryGet")>]
    let tryGet (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        match getCell list with
        | Empty ->
            None
        | Cons (hd, tl) ->
            Some (hd, tl)

    /// Return the first element of the list.
    /// Forces the evaluation of the first cell of the list if it is not already evaluated.
    [<CompiledName("Head")>]
    let inline head (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list
        
        list.Head

    /// Return the list corresponding to the remaining items in the sequence.
    /// Forces the evaluation of the first cell of the list if it is not already evaluated.
    [<CompiledName("Tail")>]
    let inline tail (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list
        
        list.Tail

    /// Test if a list is empty.
    /// Forces the evaluation of the first element of the stream if it is not already evaluated.
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        list.IsEmpty
    
    /// Return a new list which contains the given item followed by the given list.
    [<CompiledName("Cons")>]
    let cons value (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        lzy <| fun () ->
            consc value list
    
    /// Return a new list which on consumption contains the given item 
    /// followed by the list returned by the given computation.
    [<CompiledName("ConsDelayed")>]
    let consDelayed (value : 'T) creator =
        lzy <| fun () ->
            consc value (lzy <| fun () ->
                getCell <| creator ())

    /// Return a list that contains the elements returned by the given computation.
    /// The given computation is not executed until the first element on the list is
    /// consumed.  The given argument is passed to the computation.  Subsequent elements
    /// in the list are generated by again applying the residual 'b to the computation.
    [<CompiledName("Unfold")>]
    let rec unfold (generator : 'State -> ('T * 'State) option) state =
        lzy <| fun () ->
            match generator state with
            | None ->
                Empty
            | Some (value, state) ->
                Cons (value, unfold generator state)

    /// Return the list which contains on demand the elements of the
    /// first list followed by the elements of the second list.
    [<CompiledName("Append")>]
    let rec append (list1 : LazyList<'T>) (list2 : LazyList<'T>) =
//        // Preconditions
//        checkNonNull "list1" list1
//        checkNonNull "list2" list2

        lzy <| fun () ->
            appendc list1 list2

    and private appendc l1 l2 =
        match getCell l1 with
        | Empty ->
            getCell l2
        | Cons (hd, tl) ->
            consc hd (append tl l2)

    /// Return a list that is -- in effect -- the list returned by the given computation.
    /// The given computation is not executed until the first element on the list is consumed.
    [<CompiledName("Delayed")>]
    let delayed creator : LazyList<'T> =
        lzy <| fun () ->
            creator ()
            |> getCell

    /// Return the list which on consumption will consist of an
    /// infinite sequence of the given item.
    [<CompiledName("Repeat")>]
    let repeat value : LazyList<'T> =
        let rec s = cons value (delayed (fun () -> s))
        s

    /// Build a new collection whose elements are the results of applying
    /// the given function to each of the elements of the collection.
    [<CompiledName("Map")>]
    let rec map (mapping : 'T -> 'U) (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        lzy <| fun () ->
            match getCell list with
            | Empty ->
                Empty
            | Cons (hd, tl) ->
                consc (mapping hd) (map mapping tl)

    /// Build a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise.
    [<CompiledName("Map2")>]
    let rec map2 (mapping : 'T1 -> 'T2 -> 'U) (list1 : LazyList<'T1>) (list2 : LazyList<'T2>) =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        lzy <| fun () ->
            match getCell list1, getCell list2 with
            | Cons (hd1, tl1), Cons (hd2, tl2) ->
                consc (mapping hd1 hd2) (map2 mapping tl1 tl2)
            | _ -> Empty

    /// Return the list which contains on demand the pair of elements of the first and second list.
    [<CompiledName("Zip")>]
    let rec zip (list1 : LazyList<'T>) (list2 : LazyList<'T>) =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        lzy <| fun () ->
            match getCell list1, getCell list2 with
            | Cons (hd1, tl1), Cons (hd2, tl2) ->
                consc (hd1, hd2) (zip tl1 tl2)
            | _ -> Empty

    /// Return the list which contains on demand the list of elements of the list of lazy lists.
    [<CompiledName("Concat")>]
    let rec concat (lists : LazyList<LazyList<'T>>) =
        // Preconditions
        checkNonNull "lists" lists

        lzy <| fun () ->
            match getCell lists with
            | Empty ->
                Empty
            | Cons (hd, tl) ->
                appendc hd (concat tl)

    /// Return a new collection which on consumption will consist of only the elements of the collection
    /// for which the given predicate returns "true".
    [<CompiledName("Filter")>]
    let rec filter predicate (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        lzy <| fun () ->
            filterc predicate list

    and private filterc predicate list =
        match getCell list with
        | Empty ->
            Empty
        | Cons (hd, tl) ->
            if predicate hd then
                consc hd (filter predicate tl)
            else
                filterc predicate tl

    /// Apply the given function to successive elements of the list, returning the first
    /// result where function returns <c>Some(x)</c> for some x.
    /// If the function never returns true, 'None' is returned.
    [<CompiledName("TryFind")>]
    let rec tryFind predicate (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        match getCell list with
        | Empty ->
            None
        | Cons (hd, tl) ->
            if predicate hd then Some hd
            else tryFind predicate tl

    /// Return the first element for which the given function returns <c>true</c>.
    /// Raise <c>KeyNotFoundException</c> if no such element exists.
    [<CompiledName("Find")>]
    let find predicate (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        match tryFind predicate list with
        | Some value ->
            value
        | None ->
            raise <| KeyNotFoundException "An index satisfying the predicate was not found in the collection"

    /// Return a new list consisting of the results of applying the
    /// given accumulating function to successive elements of the list.
    [<CompiledName("Scan")>]
    let rec scan (folder : 'State -> 'T -> 'State) (state : 'State) (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        lzy <| fun () ->
            match getCell list with
            | Empty ->
                consc state empty
            | Cons (hd, tl) ->
                let state' = folder state hd
                consc state (scan folder state' tl)

    /// Return the list which on consumption will consist of
    /// at most 'count' elements of the input list.
    [<CompiledName("Take")>]
    let rec take count (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list
        if count < 0 then
            argOutOfRange "count" "Cannot take a negative number of elements."

        lzy <| fun () ->
            if count = 0 then
                Empty
            else
                match getCell list with
                | Cons (hd, tl) ->
                    consc hd (take (count - 1) tl)
                | Empty ->
                    invalidArg "count" "not enough items in the list"

    let rec private skipc n (list : LazyList<'T>) =
        if n = 0 then
            getCell list
        else
            match getCell list with
            | Cons (_, tl) ->
                skipc (n - 1) tl
            | Empty ->
                invalidArg "n" "not enough items in the list"

    /// Return the list which on consumption will skip the first 'count' elements of the input list.
    [<CompiledName("Skip")>]
    let rec skip count (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list
        if count < 0 then
            argOutOfRange "count" "Cannot skip a negative number of elements."

        lzy <| fun () ->
            skipc count list

    /// Build a collection from the given list. This function will eagerly
    /// evaluate the entire list (and thus may not terminate).
    [<CompiledName("OfList")>]
    let rec ofList (list : 'T list) =
        // Preconditions
        checkNonNull "list" list

        lzy <| fun () ->
            match list with
            | [] ->
                Empty
            | hd :: tl ->
                consc hd (ofList tl)

    /// Build a non-lazy list from the given collection. This function will eagerly
    /// evaluate the entire list (and thus may not terminate).
    [<CompiledName("ToList")>]
    let toList (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        let rec loop acc (list : LazyList<'T>) =
            match getCell list with
            | Empty ->
                List.rev acc
            | Cons (hd, tl) ->
                loop (hd :: acc) tl
        loop [] list

    /// Apply the given function to each element of the collection.
    [<CompiledName("Iterate")>]
    let rec iter (action : 'T -> unit) (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        match getCell list with
        | Empty -> ()
        | Cons (hd, tl) ->
            action hd
            iter action tl

    let rec private copyFrom index (array : 'T[]) =
        lzy <| fun () ->
            if index >= Array.length array then
                Empty
            else
                copyFrom (index + 1) array
                |> consc array.[index]

    /// Build a collection from the given array. This function will eagerly
    /// evaluate the entire list (and thus may not terminate).
    [<CompiledName("OfArray")>]
    let ofArray (array : 'T[]) =
        // Preconditions
        checkNonNull "array" array

        copyFrom 0 array

//    let rec private copyTo (array : 'T[]) (list : LazyList<'T>) index =
//        match getCell list with
//        | Empty -> ()
//        | Cons (hd, tl) ->
//            array.[index] <- hd
//            copyTo array tl (index + 1)

    /// Build an array from the given collection.
    [<CompiledName("ToArray")>]
    let toArray (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        // Iterate over the LazyList and copy its elements into a ResizeArray.
        let elements = ResizeArray ()
        iter elements.Add list
        ResizeArray.toArray elements

    /// Return the length of the list.
    [<CompiledName("Length")>]
    let length (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        let rec lengthAux acc (list : LazyList<'T>) =
            match getCell list with
            | Empty -> acc
            | Cons (_, tl) ->
                lengthAux (acc + 1) tl

        lengthAux 0 list

    /// Return a view of the collection as an enumerable object.
    [<CompiledName("ToSeq")>]
    let inline toSeq (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        list.ToSeq ()

    // Note: this doesn't dispose of the IEnumerator if the iteration is not run to the end
    let rec private ofFreshIEnumerator (e : IEnumerator<'T>) =
        lzy <| fun () ->
            if e.MoveNext () then
                consc e.Current (ofFreshIEnumerator e)
            else
               e.Dispose ()
               Empty

    /// Build a new collection from the given enumerable object.
    [<CompiledName("OfSeq")>]
    let ofSeq (sequence : seq<'T>) =
        // Preconditions
        checkNonNull "sequence" sequence

        sequence.GetEnumerator ()
        |> ofFreshIEnumerator


//
[<AutoOpen>]
module LazyListPatterns =
    // Active pattern for deconstructing lazy lists.
    let (|Cons|Nil|) (list : LazyList<'T>) =
        match list.Value with
        | Cons (hd, tl) ->
            Cons (hd, tl)
        | Empty ->
            Nil

