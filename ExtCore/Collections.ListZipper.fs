(*

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

open ExtCore


/// An implementation of a 'zipper' for F# lists.
[<Struct; NoComparison>]
type ListZipper<'T> private (path : 'T list, list : 'T list) =
    /// The empty ListZipper value.
    static let empty : ListZipper<'T> =
        ListZipper ([], [])

    /// The empty ListZipper value.
    static member Empty
        with get () = empty

    /// Creates a ListZipper whose initial position is prior to
    /// the head element of the given list.
    new (list : 'T list) =
        ListZipper ([], list)

    /// Is the zipper positioned prior to the start of the list?
    member __.AtStart
        with get () =
            List.isEmpty path

    /// Is the zipper positioned after the end of the list?
    member __.AtEnd
        with get () =
            List.isEmpty list

    /// Is the zipper empty?
    member __.IsEmpty
        with get () =
            List.isEmpty path && List.isEmpty list

    /// The previous element (if any) and current element (if any),
    /// based on the zipper's current position.
    member __.Context
        with get () =
            match path, list with
            | [], [] ->
                None, None
            | prev :: _, [] ->
                Some prev, None
            | [], current :: _ ->
                None, Some current
            | prev :: _, current :: _ ->
                Some prev, Some current

    /// The current element (if any).
    member __.Current
        with get () =
            match list with
            | [] -> None
            | current :: _ ->
                Some current

    /// The previous element (if any).
    member __.Previous
        with get () =
            match path with
            | [] -> None
            | prev :: _ ->
                Some prev

    /// Moves the zipper backwards by one step.
    /// No exception is raised if the zipper is already at the start of the list.
    member this.MoveBack () =
        match path with
        | [] -> this
        | prev :: path ->
            ListZipper (path, prev :: list)

    /// Moves the zipper forwards by one step.
    /// No exception is raised if the zipper is already at the end of the list.
    member this.MoveNext () =
        match list with
        | [] -> this
        | current :: list ->
            ListZipper (current :: path, list)

    /// Moves the zipper to the start of the list.
    /// No exception is raised if the zipper is already at the start of the list.
    member this.MoveStart () =
        match path with
        | [] -> this
        | prev :: path ->
            (ListZipper (path, prev :: list)).MoveStart ()

    /// Moves the zipper to the end of the list.
    /// No exception is raised if the zipper is already at the end of the list.
    member this.MoveEnd () =
        match list with
        | [] -> this
        | current :: list ->
            (ListZipper (current :: path, list)).MoveEnd ()

    /// Creates a new zipper by removing the current element from the list.
    /// No exception is raised if the zipper is empty.
    member this.Remove () =
        match list with
        | [] -> this
        | _ :: list ->
            ListZipper (path, list)

    /// Creates a new zipper by inserting the given value prior to the current list element.
    /// If the zipper is empty, the new zipper's list is created from the given value.
    member this.Insert (value : 'T) =
        ListZipper (path, value :: list)

    /// <summary>
    /// Creates a new zipper by replacing the current list element with the given value.
    /// This is similar to <c>zipper.Remove().Insert(value)</c> except that when the zipper
    /// is empty, <c>zipper.Update(value)</c> returns an empty zipper.
    /// </summary>
    member this.Update (value : 'T) =
        match list with
        | [] -> this
        | _ :: list ->
            ListZipper (path, value :: list)

    /// Returns a new list whose elements are the same as those in the zipper,
    /// and also in the same order as those in the zipper.
    member __.ToList () : 'T list =
        // Same as ListZipper.MoveStart(), except here we return the list
        // once we've reached the start of the list.
        match path with
        | [] -> list
        | prev :: path ->
            (ListZipper (path, prev :: list)).ToList ()


/// Functional operators related to the ListZipper<_> type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ListZipper =
    /// The empty zipper value.
    [<CompiledName("Empty"); GeneralizableValue>]
    let empty<'T> : ListZipper<'T> =
        ListZipper.Empty

    /// Is the zipper empty?
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (zipper : ListZipper<'T>) : bool =
        zipper.IsEmpty

    /// Is the zipper positioned prior to the start of the list?
    [<CompiledName("AtStart")>]
    let inline atStart (zipper : ListZipper<'T>) : bool =
        zipper.AtStart

    /// Is the zipper positioned after the end of the list?
    [<CompiledName("AtEnd")>]
    let inline atEnd (zipper : ListZipper<'T>) : bool =
        zipper.AtEnd

    /// The previous element (if any) and current element (if any),
    /// based on the zipper's current position.
    [<CompiledName("Context")>]
    let inline context (zipper : ListZipper<'T>) : 'T option * 'T option =
        zipper.Context

    /// The current element (if any).
    [<CompiledName("Current")>]
    let inline current (zipper : ListZipper<'T>) : 'T option =
        zipper.Current

    /// The previous element (if any).
    [<CompiledName("Previous")>]
    let inline prev (zipper : ListZipper<'T>) : 'T option =
        zipper.Previous

    /// Moves the zipper backwards by one step.
    /// No exception is raised if the zipper is already at the start of the list.
    [<CompiledName("MoveBack")>]
    let inline moveBack (zipper : ListZipper<'T>) : ListZipper<'T> =
        zipper.MoveBack ()

(* NOTE : This function is temporarily disabled due to a bug in the F# 3.0 compiler. *)
(*
    /// Moves the zipper forwards by one step.
    /// No exception is raised if the zipper is already at the end of the list.
    [<CompiledName("MoveNext")>]
    let inline moveNext (zipper : ListZipper<'T>) : ListZipper<'T> =
        zipper.MoveNext ()
*)

    /// Moves the zipper to the start of the list.
    /// No exception is raised if the zipper is already at the start of the list.
    [<CompiledName("MoveStart")>]
    let inline moveStart (zipper : ListZipper<'T>) : ListZipper<'T> =
        zipper.MoveStart ()

    /// Moves the zipper to the end of the list.
    /// No exception is raised if the zipper is already at the end of the list.
    [<CompiledName("MoveEnd")>]
    let inline moveEnd (zipper : ListZipper<'T>) : ListZipper<'T> =
        zipper.MoveEnd ()

    /// Creates a new zipper by removing the current element from the list.
    /// No exception is raised if the zipper is empty.
    [<CompiledName("Remove")>]
    let inline remove (zipper : ListZipper<'T>) : ListZipper<'T> =
        zipper.Remove ()

    /// Creates a new zipper by inserting the given value prior to the current list element.
    /// If the zipper is empty, the new zipper's list is created from the given value.
    [<CompiledName("Insert")>]
    let inline insert (value : 'T) (zipper : ListZipper<'T>) : ListZipper<'T> =
        zipper.Insert value

    /// <summary>
    /// Creates a new zipper by replacing the current list element with the given value.
    /// This is similar to <c>zipper.Remove().Insert(value)</c> except that when the zipper
    /// is empty, <c>zipper.Update(value)</c> returns an empty zipper.
    /// </summary>
    [<CompiledName("Update")>]
    let inline update (value : 'T) (zipper : ListZipper<'T>) : ListZipper<'T> =
        zipper.Update value

    /// Creates a new zipper on the given list.
    /// The created zipper will be positioned at the start of the list.
    [<CompiledName("OfList")>]
    let inline ofList (list : 'T list) : ListZipper<'T> =
        ListZipper (list)

    /// Returns a new list whose elements are the same as those in the zipper,
    /// and also in the same order as those in the zipper.
    [<CompiledName("ToList")>]
    let inline toList (zipper : ListZipper<'T>) : 'T list =
        zipper.ToList ()
