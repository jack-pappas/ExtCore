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

open System.Collections.Generic
open ExtCore

#nowarn "21" // recursive initialization
#nowarn "40" // recursive initialization

//
[<NoEquality; NoComparison>]
type internal LazyCellStatus<'T> =
    //
    | Delayed of (unit -> LazyListCell<'T>)
    //
    | Value of LazyListCell<'T>
    //
    | Exception of exn

//
and [<NoEquality; NoComparison>]
    [<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
    internal LazyListCell<'T> =
    //
    | Empty
    //
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
and [<NoEquality; NoComparison; Sealed>]
//[<StructuredFormatDisplay("")>]
    LazyList<'T> internal (initialStatus) =
    //
    static let emptyList = LazyList (Value Empty)
    
    /// The status for undefined values.
    static let undefinedValue =
        System.InvalidOperationException "The value of the LazyList cell is undefined."
        :> exn
        |> Exception

    //
    let mutable status : LazyCellStatus<'T> = initialStatus

    /// <summary>The empty LazyList.</summary>
    static member internal Empty
        with get () = emptyList
    
    //
    member internal this.Value =
        match status with
        | Value value ->
            value
        | _ ->
            lock this <| fun () ->
                match status with
                | Delayed f ->
                    status <- undefinedValue
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

    /// <summary>Gets a value that indicates whether the <see cref="LazyList`1"/> is empty.</summary>
    /// <remarks>Forces the evaluation of the first element of the stream if it is not already evaluated.</remarks>
    member this.IsEmpty
        with get () =
            match this.Value with
            | Empty -> true
            | Cons _ -> false

    /// <summary>Determines the length of the list.</summary>
    /// <remarks>
    /// Forces the evaluation of the entire list if it is not already evaluated, so calls to this property may not terminate.
    /// </remarks>
    member this.Length
        with get () =
            this.LengthImpl 0u

    /// <summary>Determines the length of the list.</summary>
    /// <remarks>
    /// Forces the evaluation of the entire list if it is not already evaluated, so calls to this property may not terminate.
    /// </remarks>
    member this.LongLength
        with get () =
            this.LongLengthImpl 0UL

    /// <summary>Determines the evaluated length of the list.</summary>
    /// <remarks>This does not force any evaluation of the list.</remarks>
    member this.ForcedLength
        with get () =
            this.ForcedLengthImpl 0u

    /// <summary>Return the first element of the list.</summary>
    /// <remarks>Forces the evaluation of the first cell of the list if it is not already evaluated.</remarks>
    member this.Head () =
        match this.Value with
        | Cons (hd, _) -> hd
        | Empty ->
            invalidOp "The list is empty."

    /// <summary>Return the list corresponding to the remaining items in the sequence.</summary>
    /// <remarks>Forces the evaluation of the first cell of the list if it is not already evaluated.</remarks>
    member this.Tail () =
        match this.Value with
        | Cons (_, tl) -> tl
        | Empty ->
            invalidOp "The list is empty."

    /// <summary>Get the first cell of the list.</summary>
    member this.TryHeadTail () =
        match this.Value with
        | Empty ->
            None
        | Cons (hd, tl) ->
            Some (hd, tl)

    /// <summary>Creates a sequence which enumerates the values in the <see cref="LazyList`1"/>.</summary>
    member this.ToSeq () =
        this
        |> Seq.unfold (fun list ->
            match list.Value with
            | Empty ->
                None
            | Cons (hd, tl) ->
                Some (hd, tl))

    /// <summary></summary>
    /// <param name="cellCreator"></param>
    /// <returns></returns>
    static member internal CreateLazy cellCreator : LazyList<'T> =
        LazyList (Delayed cellCreator)

    /// <summary>Return a new list which contains the given item followed by the given list.</summary>
    /// <param name="value"></param>
    /// <param name="list"></param>
    /// <returns></returns>
    static member internal Cons (value, list) : LazyList<'T> =
        // Preconditions
        checkNonNull "list" list

        LazyList<_>.CreateLazy <| fun () ->
            Cons (value, list)

    /// <summary>
    /// Return a new list which on consumption contains the given item followed by the list returned by the given computation.
    /// </summary>
    /// <param name="value"></param>
    /// <param name="creator"></param>
    /// <returns></returns>
    static member internal ConsDelayed (value, creator : unit -> LazyList<'T>) : LazyList<'T> =
        LazyList<_>.CreateLazy <| fun () ->
            Cons (value, LazyList<_>.CreateLazy <| fun () ->
                (creator ()).Value)

    /// <summary>
    /// Return a list that is -- in effect -- the list returned by the given computation.
    /// The given computation is not executed until the first element on the list is consumed.
    /// </summary>
    /// <param name="creator"></param>
    /// <returns></returns>
    static member internal Delayed (creator : unit -> LazyList<'T>) : LazyList<'T> =
        LazyList (Delayed <| fun () ->
            (creator ()).Value)

    //
    member private this.LengthImpl (acc : uint32) =
        match this.Value with
        | Empty ->
            int acc
        | Cons (_, tl) ->
            // Check to see if we've reached Int32.MaxValue to avoid silently overflowing.
            if acc = uint32 System.Int32.MaxValue then
                invalidOp "The LazyList contains more than System.Int32.MaxValue elements."
            else
                tl.LengthImpl (acc + 1u)

    //
    member private this.LongLengthImpl (acc : uint64) =
        match this.Value with
        | Empty ->
            int64 acc
        | Cons (_, tl) ->
            // Check to see if we've reached Int32.MaxValue to avoid silently overflowing.
            if acc = uint64 System.Int64.MaxValue then
                invalidOp "The LazyList contains more than System.Int64.MaxValue elements."
            else
                tl.LongLengthImpl (acc + 1UL)

    //
    member private this.ForcedLengthImpl (acc : uint32) =
        let statusLocal = status
        match statusLocal with
        | Delayed _
        | Exception _ ->
            // Don't evaluate Delayed cases, and don't raise exceptions.
            int acc
        | Value cell ->
            match cell with
            | Empty ->
                int acc
            | Cons (_, tl) ->
                tl.ForcedLengthImpl (acc + 1u)
            
    interface IEnumerable<'T> with
        member this.GetEnumerator () =
            this.ToSeq().GetEnumerator ()

    interface System.Collections.IEnumerable with
        override this.GetEnumerator () =
            this.ToSeq().GetEnumerator ()
            :> System.Collections.IEnumerator


/// Functional operators on LazyLists.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LazyList =
    //open OptimizedClosures

    /// <summary>The empty LazyList.</summary>
    [<CompiledName("Empty")>]
    let empty<'T> : LazyList<'T> =
        LazyList<'T>.Empty
    
    /// <summary>Get the first cell of the list.</summary>
    /// <param name="list"></param>
    /// <returns></returns>
    [<CompiledName("TryGet")>]
    let inline tryGet (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        list.TryHeadTail ()

    /// <summary>Return the first element of the list.</summary>
    /// <param name="list"></param>
    /// <returns></returns>
    /// <remarks>Forces the evaluation of the first cell of the list if it is not already evaluated.</remarks>
    [<CompiledName("Head")>]
    let inline head (list : LazyList<'T>) : 'T =
        // Preconditions
        checkNonNull "list" list
        
        list.Head ()

    /// <summary>Return the list corresponding to the remaining items in the sequence.</summary>
    /// <param name="list"></param>
    /// <returns></returns>
    /// <remarks>Forces the evaluation of the first cell of the list if it is not already evaluated.</remarks>
    [<CompiledName("Tail")>]
    let inline tail (list : LazyList<'T>) : LazyList<'T> =
        // Preconditions
        checkNonNull "list" list
        
        list.Tail ()

    /// <summary>Test if a list is empty.</summary>
    /// <param name="list"></param>
    /// <returns></returns>
    /// <remarks>Forces the evaluation of the first element of the stream if it is not already evaluated.</remarks>
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (list : LazyList<'T>) : bool =
        // Preconditions
        checkNonNull "list" list

        list.IsEmpty

    /// <summary>Return a new list which contains the given item followed by the given list.</summary>
    /// <param name="value"></param>
    /// <param name="list"></param>
    /// <returns></returns>
    [<CompiledName("Cons")>]
    let cons value (list : LazyList<'T>) =
        // Preconditions checked by the implementation.
        LazyList<_>.Cons (value, list)
    
    /// <summary>
    /// Return a new list which on consumption contains the given item followed by the list returned by the given computation.
    /// </summary>
    /// <param name="value"></param>
    /// <param name="creator"></param>
    /// <returns></returns>
    [<CompiledName("ConsDelayed")>]
    let consDelayed (value : 'T) creator =
        LazyList<_>.ConsDelayed (value, creator)

    /// <summary>
    /// Return a list that is -- in effect -- the list returned by the given computation.
    /// The given computation is not executed until the first element on the list is consumed.
    /// </summary>
    /// <param name="creator"></param>
    /// <returns></returns>
    [<CompiledName("Delayed")>]
    let delayed creator : LazyList<'T> =
        LazyList<_>.Delayed creator

    /// <summary>Creates a LazyList containing the given value.</summary>
    /// <param name="value"></param>
    /// <returns></returns>
    [<CompiledName("Singleton")>]
    let singleton value : LazyList<'T> =
        LazyList<_>.Cons (value, LazyList.Empty)

    /// Returns a new LazyListCell created by adding a value to the end of the given LazyList.
    /// This is simply a curried form of the Cons constructor.
    let inline private consCell value (list : LazyList<'T>) =
        assert (not <| isNull list)
        Cons (value, list)

    /// <summary>Return the length of the list.</summary>
    /// <param name="list"></param>
    /// <returns></returns>
    /// <remarks>This forces evaluation of the entire list and may not terminate.</remarks>
    [<CompiledName("Length")>]
    let length (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        list.Length

    /// <summary>Return the length of the list.</summary>
    /// <param name="list"></param>
    /// <returns></returns>
    /// <remarks>This forces evaluation of the entire list and may not terminate.</remarks>
    [<CompiledName("Length")>]
    let longLength (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        list.LongLength

    /// <summary>
    /// Returns the evaluated ("forced") length of the list -- i.e., the number
    /// of elements which have already been evaluated. Unlike <see cref="LazyList.length"/>,
    /// this does not force evaluation of any cells and always terminates.
    /// </summary>
    /// <param name="list"></param>
    /// <returns></returns>
    [<CompiledName("ForcedLength")>]
    let forcedLength (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        list.ForcedLength

    /// <summary>Return the list which on consumption will consist of an infinite sequence of the given item.</summary>
    /// <param name="list"></param>
    /// <returns></returns>
    [<CompiledName("Repeat")>]
    let repeat value : LazyList<'T> =
        let rec s = cons value (delayed (fun () -> s))
        s

    /// <summary>
    /// Return the list which contains on demand the elements of the first list followed by the elements of the second list.
    /// </summary>
    /// <param name="list1"></param>
    /// <param name="list2"></param>
    /// <returns></returns>
    [<CompiledName("Append")>]
    let rec append (list1 : LazyList<'T>) (list2 : LazyList<'T>) =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        LazyList<_>.CreateLazy <| fun () ->
            appendCell list1 list2

    and private appendCell list1 list2 =
        match list1.Value with
        | Empty ->
            list2.Value
        | Cons (hd, tl) ->
            consCell hd (append tl list2)

    /// <summary>Return the list which contains on demand the list of elements of the list of lazy lists.</summary>
    /// <param name="lists"></param>
    /// <returns></returns>
    [<CompiledName("Concat")>]
    let rec concat (lists : LazyList<LazyList<'T>>) =
        // Preconditions
        checkNonNull "lists" lists

        LazyList<_>.CreateLazy <| fun () ->
            match lists.Value with
            | Empty ->
                Empty
            | Cons (hd, tl) ->
                appendCell hd (concat tl)

    /// <summary>
    /// Apply the given function to successive elements of the list, returning the first result where function returns
    /// <c>Some(x)</c> for some x. If the function never returns <c>true</c>, <c>None</c> is returned.
    /// </summary>
    /// <param name="predicate"></param>
    /// <param name="list"></param>
    /// <returns></returns>
    [<CompiledName("TryFind")>]
    let rec tryFind predicate (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        match list.Value with
        | Empty ->
            None
        | Cons (hd, tl) ->
            if predicate hd then Some hd
            else tryFind predicate tl

    /// <summary>
    /// Return the first element for which the given function returns <c>true</c>.
    /// Raise <see cref="KeyNotFoundException"/> if no such element exists.
    /// </summary>
    /// <param name="predicate"></param>
    /// <param name="list"></param>
    /// <returns></returns>
    [<CompiledName("Find")>]
    let find predicate (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        match tryFind predicate list with
        | Some value ->
            value
        | None ->
            raise <| KeyNotFoundException "An element satisfying the predicate was not found in the collection."

    /// <summary>Return a list that contains the elements returned by the given computation.</summary>
    /// <param name="generator"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    /// <remarks>
    /// The given computation is not executed until the first element on the list is consumed. The given argument is passed to the
    /// computation. Subsequent elements in the list are generated by again applying the residual state to the computation.
    /// </remarks>
    [<CompiledName("Unfold")>]
    let rec unfold (generator : 'State -> ('T * 'State) option) state =
        LazyList<_>.CreateLazy <| fun () ->
            match generator state with
            | None ->
                Empty
            | Some (value, state) ->
                Cons (value, unfold generator state)

    /// <summary>
    /// Build a new collection whose elements are the results of applying
    /// the given function to each of the elements of the collection.
    /// </summary>
    /// <param name="mapping"></param>
    /// <param name="list"></param>
    /// <returns></returns>
    [<CompiledName("Map")>]
    let rec map (mapping : 'T -> 'U) (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        LazyList<_>.CreateLazy <| fun () ->
            match list.Value with
            | Empty ->
                Empty
            | Cons (hd, tl) ->
                consCell (mapping hd) (map mapping tl)

    /// <summary>
    /// Build a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise.
    /// </summary>
    /// <param name="mapping"></param>
    /// <param name="list1"></param>
    /// <param name="list2"></param>
    /// <returns></returns>
    [<CompiledName("Map2")>]
    let rec map2 (mapping : 'T1 -> 'T2 -> 'U) (list1 : LazyList<'T1>) (list2 : LazyList<'T2>) =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        LazyList<_>.CreateLazy <| fun () ->
            match list1.Value, list2.Value with
            | Cons (hd1, tl1), Cons (hd2, tl2) ->
                consCell (mapping hd1 hd2) (map2 mapping tl1 tl2)
            | _ -> Empty

    /// <summary>Applies the given function to each element of the lazy list and concatenates all of the results.</summary>
    /// <param name="mapping"></param>
    /// <param name="list"></param>
    /// <returns></returns>
    [<CompiledName("Collect")>]
    let rec collect (mapping : 'T -> LazyList<'U>) (list : LazyList<'T>) : LazyList<'U> =
        // Preconditions
        checkNonNull "list" list

        LazyList.Delayed <| fun () ->
            match list.TryHeadTail () with
            | None ->
                LazyList<_>.Empty
            | Some (x, xs) ->
                append (mapping x) (collect mapping xs)

    /// <summary>Return the list which contains on demand the pair of elements of the first and second list.</summary>
    /// <param name="list1"></param>
    /// <param name="list2"></param>
    /// <returns></returns>
    [<CompiledName("Zip")>]
    let rec zip (list1 : LazyList<'T1>) (list2 : LazyList<'T2>) =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        LazyList<_>.CreateLazy <| fun () ->
            match list1.Value, list2.Value with
            | Cons (hd1, tl1), Cons (hd2, tl2) ->
                consCell (hd1, hd2) (zip tl1 tl2)
            | _ -> Empty

    /// <summary>Splits a lazy list of pairs into a pair of lazy lists.</summary>
    /// <param name="list"></param>
    /// <returns></returns>
    [<CompiledName("Unzip")>]
    let unzip (list : LazyList<'T1 * 'T2>) : LazyList<'T1> * LazyList<'T2> =
        // Preconditions
        checkNonNull "list" list

        map fst list,
        map snd list

    /// <summary>
    /// Return a new collection which on consumption will consist of only the
    /// elements of the collection for which the given predicate returns <c>true</c>.
    /// </summary>
    /// <param name="predicate"></param>
    /// <param name="list"></param>
    /// <returns></returns>
    [<CompiledName("Filter")>]
    let rec filter predicate (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        LazyList<_>.CreateLazy <| fun () ->
            filterCell predicate list

    and private filterCell predicate list =
        match list.Value with
        | Empty ->
            Empty
        | Cons (hd, tl) ->
            if predicate hd then
                consCell hd (filter predicate tl)
            else
                filterCell predicate tl

    /// <summary>
    /// Return a new list consisting of the results of applying the
    /// given accumulating function to successive elements of the list.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="list"></param>
    /// <returns></returns>
    [<CompiledName("Scan")>]
    let rec scan (folder : 'State -> 'T -> 'State) (state : 'State) (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        LazyList<_>.CreateLazy <| fun () ->
            match list.Value with
            | Empty ->
                consCell state empty
            | Cons (hd, tl) ->
                let state' = folder state hd
                consCell state (scan folder state' tl)

    /// <summary>
    /// Return the list which on consumption will consist of at most '<paramref name="count"/>' elements of the input list.
    /// </summary>
    /// <param name="count"></param>
    /// <param name="list"></param>
    /// <returns></returns>
    [<CompiledName("Take")>]
    let rec take count (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list
        if count < 0 then
            argOutOfRange "count" "Cannot take a negative number of elements."

        LazyList<_>.CreateLazy <| fun () ->
            if count = 0 then
                Empty
            else
                match list.Value with
                | Cons (hd, tl) ->
                    consCell hd (take (count - 1) tl)
                | Empty ->
                    let msg = sprintf "There are not enough items in the list. (Count = %i)" count
                    invalidArg "count" msg

    let rec private skipCell count (list : LazyList<'T>) =
        if count = 0 then
            list.Value
        else
            match list.Value with
            | Cons (_, tl) ->
                skipCell (count - 1) tl
            | Empty ->
                let msg = sprintf "There are not enough items in the list. (Count = %i)" count
                invalidArg "count" msg

    /// <summary>
    /// Return the list which on consumption will skip the first '<paramref name="count"/>' elements of the input list.
    /// </summary>
    /// <param name="count"></param>
    /// <param name="list"></param>
    /// <returns></returns>
    [<CompiledName("Skip")>]
    let skip count (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list
        if count < 0 then
            argOutOfRange "count" "Cannot skip a negative number of elements."

        LazyList<_>.CreateLazy <| fun () ->
            skipCell count list

    /// <summary>Apply the given function to each element of the collection.</summary>
    /// <param name="action"></param>
    /// <param name="list"></param>
    /// <returns></returns>
    [<CompiledName("Iterate")>]
    let rec iter (action : 'T -> unit) (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        match list.Value with
        | Empty -> ()
        | Cons (hd, tl) ->
            action hd
            iter action tl

    // NOTE : This function doesn't dispose the IEnumerator until it reaches
    // the end of the enumeration; this function can therefore cause memory leaks
    // if not used carefully.
    let rec private ofFreshIEnumerator (e : IEnumerator<'T>) =
        LazyList<_>.CreateLazy <| fun () ->
            if e.MoveNext () then
                consCell e.Current (ofFreshIEnumerator e)
            else
               e.Dispose ()
               Empty

    /// <summary>Build a <see cref="LazyList`1"/> from the given sequence of elements.</summary>
    /// <param name="sequence"></param>
    /// <returns></returns>
    [<CompiledName("OfSeq")>]
    let ofSeq (sequence : seq<'T>) =
        // Preconditions
        checkNonNull "sequence" sequence

        sequence.GetEnumerator ()
        |> ofFreshIEnumerator

    /// <summary>Create a <see cref="LazyList`1"/> containing the elements of the given list.</summary>
    /// <param name="list"></param>
    /// <returns></returns>
    [<CompiledName("OfList")>]
    let rec ofList (list : 'T list) =
        // Preconditions
        checkNonNull "list" list

        LazyList<_>.CreateLazy <| fun () ->
            match list with
            | [] ->
                Empty
            | hd :: tl ->
                consCell hd (ofList tl)

    /// Creates a LazyList from an array by copying elements from the array into the LazyList.
    let rec private copyFrom index (array : 'T[]) =
        LazyList<_>.CreateLazy <| fun () ->
            if index >= Array.length array then
                Empty
            else
                copyFrom (index + 1) array
                |> consCell array.[index]

    /// <summary>Create a <see cref="LazyList`1"/> containing the elements of the given array.</summary>
    /// <param name="array"></param>
    /// <returns></returns>
    [<CompiledName("OfArray")>]
    let ofArray (array : 'T[]) =
        // Preconditions
        checkNonNull "array" array

        copyFrom 0 array

    /// <summary>Create a <see cref="LazyList`1"/> containing the elements of the given vector.</summary>
    /// <param name="vector"></param>
    /// <returns></returns>
    [<CompiledName("OfVector")>]
    let ofVector (vector : vector<'T>) =
        // Preconditions
        // TODO

        copyFrom 0 vector.Elements

    /// <summary>Return a view of the collection as an enumerable object.</summary>
    /// <param name="list"></param>
    /// <returns></returns>
    [<CompiledName("ToSeq")>]
    let inline toSeq (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        list.ToSeq ()

    /// <summary>Build a non-lazy list from the given collection.</summary>
    /// <param name="list"></param>
    /// <returns></returns>
    /// <remarks>This function will eagerly evaluate the entire list (and thus may not terminate).</remarks>
    [<CompiledName("ToList")>]
    let toList (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        let rec loop acc (list : LazyList<'T>) =
            match list.Value with
            | Empty ->
                List.rev acc
            | Cons (hd, tl) ->
                loop (hd :: acc) tl
        loop [] list

    /// <summary>Build an array from the given <see cref="LazyList`1"/>.</summary>
    /// <param name="list"></param>
    /// <returns></returns>
    /// <remarks>This function will eagerly evaluate the entire list (and thus may not terminate).</remarks>
    [<CompiledName("ToArray")>]
    let toArray (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        // Iterate over the LazyList and copy its elements into a ResizeArray.
        let elements = ResizeArray ()
        iter elements.Add list
        ResizeArray.toArray elements

    /// <summary>Build a vector from the given <see cref="LazyList`1"/>.</summary>
    /// <param name="list"></param>
    /// <returns></returns>
    /// <remarks>This function will eagerly evaluate the entire list (and thus may not terminate).</remarks>
    [<CompiledName("ToVector")>]
    let toVector (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list

        // Iterate over the LazyList and copy its elements into a ResizeArray.
        let elements = ResizeArray ()
        iter elements.Add list
        ResizeArray.toArray elements
        |> vector.UnsafeCreate

    /// <summary>Returns a LazyList that when enumerated returns at most N elements.</summary>
    /// <param name="count">The maximum number of items to enumerate.</param>
    /// <returns>The resulting <see cref="LazyList`1"/>.</returns>
    [<CompiledName("Truncate")>]
    let truncate count (list : LazyList<'T>) =
        // Preconditions
        checkNonNull "list" list
        if count < 0 then
            argOutOfRange "count" "Cannot take a negative number of elements."

        LazyList<_>.CreateLazy <| fun () ->
            if count = 0 then
                Empty
            else
                match list.Value with
                | Empty ->
                    Empty
                | Cons (hd, tl) ->
                    consCell hd (take (count - 1) tl)


type LazyList<'T> with
    /// <summary>Appends the second <see cref="LazyList`1"/> to the end of the first.</summary>
    /// <param name="list1"></param>
    /// <param name="list2"></param>
    /// <returns></returns>
    static member op_PlusPlus (list1 : LazyList<'T>, list2 : LazyList<'T>) : LazyList<'T> =
        // Preconditions
        checkNonNull "list1" list1
        checkNonNull "list2" list2

        LazyList.append list1 list2

/// Computation expression ("workflow") builder for creating lazy lists.
[<Sealed>]
type LazyListBuilder () =
    member __.Zero () : LazyList<'T> =
        LazyList<_>.Empty
    member __.Delay creator : LazyList<'T> =
        LazyList.Delayed creator
    member __.Combine (list1 : LazyList<'T>, list2 : LazyList<'T>) =
        LazyList.append list1 list2
    member __.Yield (value : 'T) =
        LazyList.singleton value
    member __.YieldFrom (list : LazyList<'T>) =
        list
    member __.For (lazyList : LazyList<'T>, binding) =
        LazyList.collect binding lazyList
    member __.For (sequence, f) =
        // TODO :   Right now, we're evaluating the entire sequence, then wrapping the results in a LazyList.
        //          It would be more efficient to evaluate the sequence on-demand, though we need to decide if it's safe enough.
        //          Or, we could use type tests on 'sequence' so if it's an array, list, etc., we can use a more-efficient conversion to LazyList.
        // LazyList.collect f (LazyList.ofSeq sequence)
        LazyList.collect f (LazyList.ofList (List.ofSeq sequence))
    member __.Bind (lazyValue : Lazy<'T>, binding : 'T -> LazyList<'U>) =
        LazyList.Delayed (fun () ->
            lazyValue.Force ()
            |> LazyList.singleton)
        |> LazyList.collect binding

/// Active patterns for deconstructing lazy lists.
[<AutoOpen>]
module LazyListPatterns =
    // Active pattern for deconstructing lazy lists.
    let inline (|Nil|Cons|) (list : LazyList<'T>) =
        match list.TryHeadTail () with
        | None ->
            Nil
        | Some hd_tl ->
            Cons hd_tl
