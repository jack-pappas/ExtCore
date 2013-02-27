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
[<NoEquality; NoComparison>]
type LazyList<'T> = {
    //
    mutable status : LazyCellStatus< 'T >;
} with
    
    member x.Value =
        match x.status with
        | LazyCellStatus.Value value ->
            value
        | _ ->
            lock x <| fun () ->
                match x.status with
                | LazyCellStatus.Delayed f ->
                    x.status <- Exception UndefinedException
                    try
                        let res = f ()
                        x.status <- LazyCellStatus.Value res
                        res
                    with ex ->
                        x.status <- LazyCellStatus.Exception ex
                        reraise ()
                | LazyCellStatus.Value value ->
                    value
                | LazyCellStatus.Exception ex ->
                    raise ex
    
    member s.GetEnumeratorImpl() =
        let getCell (x : LazyList<'T>) = x.Value
        let toSeq s = s |> Seq.unfold (fun ll -> match getCell ll with CellEmpty -> None | CellCons(a,b) -> Some(a,b))
        (toSeq s).GetEnumerator()
            
    interface IEnumerable<'T> with
        member s.GetEnumerator() =
            s.GetEnumeratorImpl()

    interface System.Collections.IEnumerable with
        override s.GetEnumerator() =
            s.GetEnumeratorImpl() :> System.Collections.IEnumerator


and 
    [<NoEquality; NoComparison>]
    LazyCellStatus<'T> =
    | Delayed of (unit -> LazyListCell<'T>)
    | Value of LazyListCell<'T> 
    | Exception of System.Exception


and 
    [<NoEquality; NoComparison>]
    LazyListCell<'T> = 
    | CellCons of 'T * LazyList<'T> 
    | CellEmpty

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LazyList = 

    let private lzy f = { status = Delayed f }
    let private force (x: LazyList<'T>) = x.Value

    let private notlazy v = { status = Value v }
    
    type private EmptyValue<'T>() = 
        static let value : LazyList<'T> =
            notlazy CellEmpty
        static member Value : LazyList<'T> =
            value
        
    [<NoEquality; NoComparison>]
    type private LazyItem<'T> =
        | Cons of 'T * LazyList<'T>
        | Empty

    type private 'T item = 'T LazyItem
    
    /// Get the first cell of the list.
    let get (list : LazyList<'T>) =
        match force list with
        | CellCons (a, b) ->
            Some (a, b)
        | CellEmpty ->
            None
    
    let private getCell (list : LazyList<'T>) =
        force list
    
    /// Evaluates to the list that contains no items.
    [<GeneralizableValue>]
    let empty<'T> : LazyList<'T> =
        EmptyValue<'T>.Value
    
    let inline private consc x (list : LazyList<'T>) =
        CellCons (x, list)
    
    /// Return a new list which contains the given item followed by the given list.
    let cons value (list : LazyList<'T>) =
        lzy(fun () -> consc value list)
    
    /// Return a new list which on consumption contains the given item 
    /// followed by the list returned by the given computation.
    let consDelayed (value : 'T) creator =
        lzy(fun () -> (consc value (lzy(fun () -> (force (creator ()))))))

    /// Return a list that contains the elements returned by the given computation.
    /// The given computation is not executed until the first element on the list is
    /// consumed.  The given argument is passed to the computation.  Subsequent elements
    /// in the list are generated by again applying the residual 'b to the computation.
    let rec unfold (generator : 'State -> ('T * 'State) option) state =
        lzy(fun () ->
            match generator state with
            | None ->
                CellEmpty
            | Some (x, z) ->
                CellCons (x, unfold generator state))

    /// Return the list which contains on demand the elements of the
    /// first list followed by the elements of the second list.
    let rec append (list1 : LazyList<'T>) (list2 : LazyList<'T>) =
        lzy(fun () -> (appendc list1 list2))

    and private appendc l1 l2 =
        match getCell l1 with
        | CellEmpty ->
            force l2
        | CellCons (a, b) ->
            consc a (append b l2)

    /// Return a list that is -- in effect -- the list returned by the given computation.
    /// The given computation is not executed until the first element on the list is consumed.
    let delayed creator : LazyList<'T> =
        lzy(fun () -> (getCell (creator ())))

    /// Return the list which on consumption will consist of an
    /// infinite sequence of the given item.
    let repeat value : LazyList<'T> =
        let rec s = cons value (delayed (fun () -> s))
        s

    /// Build a new collection whose elements are the results of applying
    /// the given function to each of the elements of the collection.
    let rec map (mapping : 'T -> 'U) (list : LazyList<'T>) =
        lzy(fun () ->
            match getCell list with
            | CellEmpty ->
                CellEmpty
            | CellCons (a, b) ->
                consc (mapping a) (map mapping b))

    /// Build a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise.
    let rec map2 (mapping : 'T1 -> 'T2 -> 'U) (list1 : LazyList<'T1>) (list2 : LazyList<'T2>) =
        lzy(fun () ->
            match getCell list1, getCell list2 with
            | CellCons (a1, b1), CellCons (a2, b2) ->
                consc (mapping a1 a2) (map2 mapping b1 b2)
            | _ -> CellEmpty)

    /// Return the list which contains on demand the pair of elements of the first and second list.
    let rec zip (list1 : LazyList<'T>) (list2 : LazyList<'T>) =
        lzy(fun () ->
            match getCell list1, getCell list2 with
            | CellCons (a1, b1), CellCons (a2, b2) ->
                consc (a1, a2) (zip b1 b2)
            | _ -> CellEmpty)

    /// Return the list which contains on demand the list of elements of the list of lazy lists.
    let rec concat (lists : LazyList<LazyList<'T>>) =
        lzy(fun () ->
            match getCell lists with
            | CellCons (a, b) ->
                appendc a (concat b)
            | CellEmpty -> CellEmpty)

    /// Return a new collection which on consumption will consist of only the elements of the collection
    /// for which the given predicate returns "true".
    let rec filter predicate (list : LazyList<'T>) =
        lzy(fun () -> filterc predicate list)

    and private filterc predicate list =
        match getCell list with
        | CellEmpty ->
            CellEmpty
        | CellCons (a, b) ->
            if predicate a then
                consc a (filter predicate b)
            else
                filterc predicate b

    /// Apply the given function to successive elements of the list, returning the first
    /// result where function returns <c>Some(x)</c> for some x.
    /// If the function never returns true, 'None' is returned.
    let rec tryFind predicate (list : LazyList<'T>) =
        match getCell list with
        | CellEmpty ->
            None
        | CellCons (a, b) ->
            if predicate a then Some a
            else tryFind predicate b

    /// Return the first element for which the given function returns <c>true</c>.
    /// Raise <c>KeyNotFoundException</c> if no such element exists.
    let find predicate (list : LazyList<'T>) =
        match tryFind predicate list with
        | Some a -> a
        | None ->
            raise <| KeyNotFoundException "An index satisfying the predicate was not found in the collection"

    /// Return a new list consisting of the results of applying the
    /// given accumulating function to successive elements of the list.
    let rec scan (folder : 'State -> 'T -> 'State) (state : 'State) (list : LazyList<'T>) =
        lzy(fun () ->
            match getCell list with
            | CellEmpty ->
                consc state empty
            | CellCons (a, b) ->
                let state' = folder state a
                consc state (scan folder state' b))

    /// Return the first element of the list.
    /// Forces the evaluation of the first cell of the list if it is not already evaluated.
    let head (list : LazyList<'T>) =
        match getCell list with
        | CellCons (a, _) -> a
        | CellEmpty ->
            invalidArg "s" "The list is empty."

    /// Return the list corresponding to the remaining items in the sequence.
    /// Forces the evaluation of the first cell of the list if it is not already evaluated.
    let tail (list : LazyList<'T>) =
        match getCell list with
        | CellCons (_, b) -> b
        | CellEmpty ->
            invalidArg "s" "The list is empty."

    /// Test if a list is empty.
    /// Forces the evaluation of the first element of the stream if it is not already evaluated.
    let isEmpty (list : LazyList<'T>) =
        match getCell list with
        | CellCons _ -> false
        | CellEmpty -> true

    /// Return the list which on consumption will consist of
    /// at most 'count' elements of the input list.
    let rec take count (list : LazyList<'T>) =
        lzy(fun () ->
            if count < 0 then
                invalidArg "count" "Cannot take a negative number of elements."
            elif count = 0 then
                CellEmpty
            else
                match getCell list with
                | CellCons (a, s) ->
                    consc a (take (count - 1) s)
                | CellEmpty ->
                    invalidArg "count" "not enough items in the list")

    let rec private skipc n (list : LazyList<'T>) =
        if n = 0 then
            force list
        else
            match getCell list with
            | CellCons (_, s) ->
                skipc (n - 1) s
            | CellEmpty ->
                invalidArg "n" "not enough items in the list"

    /// Return the list which on consumption will skip the first 'count' elements of the input list.
    let rec skip count (list : LazyList<'T>) =
        lzy(fun () ->
            if count < 0 then
                invalidArg "count" "Cannot skip a negative number of elements."
            else skipc count list)

    /// Build a collection from the given list. This function will eagerly
    /// evaluate the entire list (and thus may not terminate).
    let rec ofList (list : 'T list) =
        lzy(fun () ->
            match list with
            | [] ->
                CellEmpty
            | h :: t ->
                consc h (ofList t))

    /// Build a non-lazy list from the given collection. This function will eagerly
    /// evaluate the entire list (and thus may not terminate).
    let toList (list : LazyList<'T>) =
        let rec loop acc (list : LazyList<'T>) =
            match getCell list with
            | CellEmpty ->
                List.rev acc
            | CellCons (hd, tl) ->
                loop (hd :: acc) tl
        loop [] list

    /// Apply the given function to each element of the collection.
    let rec iter (action : 'T -> unit) (list : LazyList<'T>) =
        match getCell list with
        | CellEmpty -> ()
        | CellCons (hd, tl) ->
            action hd
            iter action tl

    let rec private copyFrom i a =
        lzy(fun () ->
            if i >= Array.length a then CellEmpty
            else consc a.[i] (copyFrom (i + 1) a))

    let rec private copyTo (array : 'T[]) (list : LazyList<'T>) i =
        match getCell list with
        | CellEmpty -> ()
        | CellCons (a, b) ->
            array.[i] <- a
            copyTo array b (i + 1)

    /// Build a collection from the given array. This function will eagerly
    /// evaluate the entire list (and thus may not terminate).
    let ofArray (array : 'T[]) =
        copyFrom 0 array

    /// Build an array from the given collection.
    let toArray (list : LazyList<'T>) =
        // OPTIMIZE : Re-implement this so it doesn't create an additional list
        // here -- copy the elements from the LazyList into a ResizeArray instead.
        Array.ofList (toList list)

    let rec private lengthAux n (list : LazyList<'T>) =
        match getCell list with
        | CellEmpty -> n
        | CellCons (_, b) ->
            lengthAux (n + 1) b

    /// Return the length of the list.
    let length (list : LazyList<'T>) =
        lengthAux 0 list

    /// Return a view of the collection as an enumerable object.
    let toSeq (list : LazyList<'T>) =
        list :> IEnumerable<_>

    // Note: this doesn't dispose of the IEnumerator if the iteration is not run to the end
    let rec private ofFreshIEnumerator (e : IEnumerator<'T>) =
        lzy(fun () ->
            if e.MoveNext () then
                consc e.Current (ofFreshIEnumerator e)
            else
               e.Dispose ()
               CellEmpty)

    /// Build a new collection from the given enumerable object.
    let ofSeq (sequence : seq<'T>) =
        sequence.GetEnumerator ()
        |> ofFreshIEnumerator

    // Active pattern for deconstructing lazy lists.
    let (|Cons|Nil|) (list : LazyList<'T>) =
        match getCell list with
        | CellCons (a, b) ->
            Cons (a, b)
        | CellEmpty ->
            Nil
