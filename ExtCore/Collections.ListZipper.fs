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


/// <summary>An implementation of a 'zipper' for F# lists.</summary>
/// <typeparam name="T"></typeparam>
[<Struct; NoComparison>]
type ListZipper<'T> private (path : 'T list, list : 'T list) =
    /// <summary>The empty <see cref="ListZipper`1"/> value.</summary>
    static let empty : ListZipper<'T> =
        ListZipper ([], [])

    /// <summary>The empty <see cref="ListZipper`1"/> value.</summary>
    static member Empty
        with get () = empty

    /// <summary>Creates a <see cref="ListZipper`1"/> whose initial position is prior to the head element of the given list.</summary>
    /// <param name="list"></param>
    new (list : 'T list) =
        ListZipper ([], list)

    /// <summary>Is the zipper positioned prior to the start of the list?</summary>
    member __.AtStart
        with get () =
            List.isEmpty path

    /// <summary>Is the zipper positioned after the end of the list?</summary>
    member __.AtEnd
        with get () =
            List.isEmpty list

    /// <summary>Is the zipper empty?</summary>
    member __.IsEmpty
        with get () =
            List.isEmpty path && List.isEmpty list

    /// <summary>The previous element (if any) and current element (if any), based on the zipper's current position.</summary>
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

    /// <summary>The current element (if any).</summary>
    member __.Current
        with get () =
            match list with
            | [] -> None
            | current :: _ ->
                Some current

    /// <summary>The previous element (if any).</summary>
    member __.Previous
        with get () =
            match path with
            | [] -> None
            | prev :: _ ->
                Some prev

    /// <summary>Moves the zipper backwards by one step.</summary>
    /// <returns></returns>
    /// <remarks>No exception is raised if the zipper is already at the start of the list.</remarks>
    member this.MoveBack () =
        match path with
        | [] -> this
        | prev :: path ->
            ListZipper (path, prev :: list)

    /// <summary>Moves the zipper forwards by one step.</summary>
    /// <returns></returns>
    /// <remarks>No exception is raised if the zipper is already at the end of the list.</remarks>
    member this.MoveNext () =
        match list with
        | [] -> this
        | current :: list ->
            ListZipper (current :: path, list)

    /// <summary>Moves the zipper to the start of the list.</summary>
    /// <returns></returns>
    /// <remarks>No exception is raised if the zipper is already at the start of the list.</remarks>
    member this.MoveStart () =
        match path with
        | [] -> this
        | prev :: path ->
            (ListZipper (path, prev :: list)).MoveStart ()

    /// <summary>Moves the zipper to the end of the list.</summary>
    /// <returns></returns>
    /// <remarks>No exception is raised if the zipper is already at the end of the list.</remarks>
    member this.MoveEnd () =
        match list with
        | [] -> this
        | current :: list ->
            (ListZipper (current :: path, list)).MoveEnd ()

    /// <summary>Creates a new zipper by removing the current element from the list.</summary>
    /// <returns></returns>
    /// <remarks>No exception is raised if the zipper is empty.</remarks>
    member this.Remove () =
        match list with
        | [] -> this
        | _ :: list ->
            ListZipper (path, list)

    /// <summary>
    /// Creates a new zipper by inserting the given value prior to the current list element.
    /// If the zipper is empty, the new zipper's list is created from the given value.
    /// </summary>
    /// <param name="value"></param>
    /// <returns></returns>
    member this.Insert (value : 'T) =
        ListZipper (path, value :: list)

    /// <summary>Creates a new zipper by replacing the current list element with the given value.</summary>
    /// <param name="value"></param>
    /// <returns></returns>
    /// <remarks>
    /// This is similar to <c>zipper.Remove().Insert(value)</c> except that when the zipper
    /// is empty, <c>zipper.Update(value)</c> returns an empty zipper.
    /// </remarks>
    member this.Update (value : 'T) =
        match list with
        | [] -> this
        | _ :: list ->
            ListZipper (path, value :: list)

    /// <summary>
    /// Returns a new list whose elements are the same as those in the zipper, and also in the same order as those in the zipper.
    /// </summary>
    /// <returns></returns>
    member __.ToList () : 'T list =
        // Same as ListZipper.MoveStart(), except here we return the list
        // once we've reached the start of the list.
        match path with
        | [] -> list
        | prev :: path ->
            (ListZipper (path, prev :: list)).ToList ()


/// <summary>Functional operators related to the <see cref="ListZipper`1"/> type.</summary>
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

    /// The previous element (if any) and current element (if any), based on the zipper's current position.
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

    /// Moves the zipper forwards by one step.
    /// No exception is raised if the zipper is already at the end of the list.
    [<CompiledName("MoveNext")>]
    let inline moveNext (zipper : ListZipper<'T>) : ListZipper<'T> =
        // WORKAROUND : For some reason, the F# 3.0 compiler emits an error message here
        // unless we first assign the value to a mutable variable.
        //zipper.MoveNext ()
        let mutable zipper' = zipper
        zipper'.MoveNext ()

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
