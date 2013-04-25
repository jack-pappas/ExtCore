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

namespace ExtCore.Collections

open LanguagePrimitives
open OptimizedClosures
open ExtCore


/// <summary>Immutable array with constant-time access to elements.</summary>
[<Struct; CompiledName("FSharpVector`1")>]
type vector<'T> private (elements : 'T[]) =
    /// The empty vector instance.
    static let empty : vector<'T> = vector (Array.empty)

    /// The empty vector.
    static member Empty
        with get () = empty

    /// Gets the array containing the vector's elements.
    member internal __.Elements
        with get () = elements

    /// Is the vector empty?
    member __.IsEmpty
        with get () =
            Array.isEmpty elements

    /// Is the vector 'null' (uninitialized)?
    member __.IsNull
        with get () =
            isNull elements

    /// Gets a 32-bit integer that represents the total number of elements in the Vector.
    member __.Length
        with get () =
            elements.Length

    /// Gets a 64-bit integer that represents the total number of elements in the Vector.
    member __.LongLength
        with get () =
            elements.LongLength

    /// Returns the vector element at the specified index.
    member __.Item
        with get index =
            // Preconditions
            // None -- The CLR inserts it's own bounds check automatically so adding one here
            // would impact performance without gaining any additional safety.
            elements.[index]

    /// Creates a new Vector from the given array.
    static member Create (source : 'T[]) : vector<'T> =
        // Preconditions
        checkNonNull "source" source

        // Create a shallow copy of the source array, then pass it to
        // the private Vector constructor and return the new Vector value.
        vector (Array.copy source)

    /// Creates a new Vector from the given array.
    /// This method is considered "unsafe" (and restricted to internal callers only)
    /// because it uses the given array directly instead of making a copy of it.
    static member internal UnsafeCreate (source : 'T[]) : vector<'T> =
        // Preconditions
        checkNonNull "source" source

        // Create a new vector directly from the source array.
        vector (source)


/// Functional operators related to vectors (immutable arrays).
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector =
    /// Checks if the vector has been initialized; if not, an InvalidArgumentException is raised.
    [<CompiledName("CheckInitialized")>]
    let inline private checkInitialized paramName (vec : vector<'T>) =
        if vec.IsNull then
            invalidArg paramName "The vector has not been initialized; it is equal to the \
                                  Vector type's default value (the equivalent of 'null')."

    /// Builds a vector from the given array.
    [<CompiledName("OfArray")>]
    let ofArray (array : 'T[]) : vector<'T> =
        // Preconditions
        checkNonNull "array" array

        vector.Create array

    /// Returns the length of a vector.
    /// You can also use the property vec.Length.
    [<CompiledName("Length")>]
    let inline length (vector : vector<'T>) : int =
        // Preconditions
        checkInitialized "vector" vector
        
        vector.Length

    /// Is the vector empty?
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (vector : vector<'T>) : bool =
        // Preconditions
        checkInitialized "vector" vector
    
        vector.IsEmpty

    /// Given an element, creates an array containing just that element.
    [<CompiledName("Singleton")>]
    let singleton (value : 'T) : vector<'T> =
        vector.UnsafeCreate [| value |]

    /// Creates a vector that contains the elements of one vector followed by the elements of another vector.
    [<CompiledName("Append")>]
    let append (vector1 : vector<'T>) (vector2 : vector<'T>) : vector<'T> =
        // Preconditions
        checkInitialized "vector1" vector1
        checkInitialized "vector2" vector2

        // OPTIMIZATION : Only perform the append operation if both inputs are non-empty.
        if isEmpty vector1 then vector2
        elif isEmpty vector2 then vector1
        else
            Array.append vector1.Elements vector2.Elements
            |> vector.UnsafeCreate

    /// Returns the average of the elements in a vector.
    [<CompiledName("Average")>]
    let inline average (vec : vector<'T>) : ^T =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.average"

    //
    [<CompiledName("AverageBy")>]
    let inline averageBy (projection : 'T -> ^U) (vec : vector<'T>) : ^U =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.averageBy"

    //
    [<CompiledName("Blit")>]
    let blit (source : vector<'T>) sourceIndex (target : vector<'T>) targetIndex count : unit =
        // Preconditions
        checkInitialized "source" source
        checkInitialized "target" target
        // TODO : sourceIndex, targetIndex, count in-bounds
        
        notImpl "Vector.blit"

    //
    [<CompiledName("Collect")>]
    let collect (mapping : 'T -> vector<'U>) (vec : vector<'T>) : vector<'U> =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.collect"

    //
    [<CompiledName("Concat")>]
    let concat (vectors : seq<vector<'T>>) : vector<'T> =
        // Preconditions
        checkNonNull "vectors" vectors
        
        notImpl "Vector.concat"

    //
    [<CompiledName("Copy")>]
    let copy (vec : vector<'T>) : vector<'T> =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.copy"

    //
    [<CompiledName("Create")>]
    let create count (value : 'T) : vector<'T> =
        // Preconditions
        // TODO : count must be >= 0
        
        notImpl "Vector.create"

    //
    [<CompiledName("TryPick")>]
    let tryPick (chooser : 'T -> 'U option) (vec : vector<'T>) : 'U option =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.tryPick"

    //
    [<CompiledName("Pick")>]
    let pick (chooser : 'T -> 'U option) (vec : vector<'T>) : 'U =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.pick"

    //
    [<CompiledName("Choose")>]
    let choose (chooser : 'T -> 'U option) (vec : vector<'T>) : vector<'U> =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.choose"

    /// Returns an empty vector of the given type.
    [<CompiledName("Empty")>]
    let empty<'T> : vector<'T> =
        vector.Empty

    //
    [<CompiledName("Exists")>]
    let exists (predicate : 'T -> bool) (vec : vector<'T>) : bool =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.exists"

    //
    [<CompiledName("Exists2")>]
    let exists2 (predicate : 'T1 -> 'T2 -> bool) (vector1 : vector<'T1>) (vector2 : vector<'T2>) : bool =
        // Preconditions
        checkInitialized "vector1" vector1
        checkInitialized "vector2" vector2
        // TODO : Check vectors have the same length
        
        notImpl "Vector.exists2"

    //
    [<CompiledName("Filter")>]
    let filter (predicate : 'T -> bool) (vec : vector<'T>) : vector<'T> =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.filter"

    //
    [<CompiledName("Find")>]
    let find (predicate : 'T -> bool) (vec : vector<'T>) : 'T =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.find"

    //
    [<CompiledName("FindIndex")>]
    let findIndex (predicate : 'T -> bool) (vec : vector<'T>) : int =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.findIndex"

    //
    [<CompiledName("Forall")>]
    let forall (predicate : 'T -> bool) (vec : vector<'T>) : bool =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.forall"

    //
    [<CompiledName("Forall2")>]
    let forall2 (predicate : 'T1 -> 'T2 -> bool) (vector1 : vector<'T1>) (vector2 : vector<'T2>) : bool =
        // Preconditions
        checkInitialized "vector1" vector1
        checkInitialized "vector2" vector2
        // TODO : Check vectors have the same length
        
        notImpl "Vector.forall2"

    //
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> 'State) (state : 'State) (vec : vector<'T>) : 'State =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.fold"

    //
    [<CompiledName("FoldBack")>]
    let foldBack (folder : 'T -> 'State -> 'State) (vec : vector<'T>) (state : 'State) : 'State =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.foldBack"

    //
    [<CompiledName("Get")>]
    let inline get (vec : vector<'T>) (index : int) : 'T =
        // Preconditions
        checkInitialized "vec" vec
        // TODO : Index must be in-bounds
        
        notImpl "Vector.get"

    //
    [<CompiledName("Init")>]
    let init (count : int) (initializer : int -> 'T) : vector<'T> =
        // Preconditions
        // TODO : count must be >= 0
        
        notImpl "Vector.init"

    //
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> unit) (vec : vector<'T>) : unit =
        // Preconditions
        checkInitialized "vec" vec

        // If the vector is empty return immediately.
        if not <| isEmpty vec then
            Array.iter action vec.Elements

    //
    [<CompiledName("Iterate2")>]
    let iter2 (action : 'T1 -> 'T2 -> unit) (vector1 : vector<'T1>) (vector2 : vector<'T2>) : unit =
        // Preconditions
        checkInitialized "vector1" vector1
        checkInitialized "vector2" vector2
        
        notImpl "Vector.iter2"

    //
    [<CompiledName("IterateIndexed")>]
    let iteri (action : int -> 'T -> unit) (vec : vector<'T>) : unit =
        // Preconditions
        checkInitialized "vec" vec
        
        // If the vector is empty return immediately.
        if not <| isEmpty vec then
            Array.iteri action vec.Elements

    //
    [<CompiledName("IterateIndexed2")>]
    let iteri2 (action : int -> 'T1 -> 'T2 -> unit) (vector1 : vector<'T1>) (vector2 : vector<'T2>) : unit =
        // Preconditions
        checkInitialized "vector1" vector1
        checkInitialized "vector2" vector2
        
        notImpl "Vector.iteri2"

    //
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'U) (vec : vector<'T>) : vector<'U> =
        // Preconditions
        checkInitialized "vec" vec

        // If the vector is empty, return immediately.
        if isEmpty vec then empty
        else
            Array.map mapping vec.Elements
            |> vector.UnsafeCreate

    //
    [<CompiledName("Map2")>]
    let map2 (mapping : 'T1 -> 'T2 -> 'U) (vector1 : vector<'T1>) (vector2 : vector<'T2>) : vector<'U> =
        // Preconditions
        checkInitialized "vector1" vector1
        checkInitialized "vector2" vector2
        
        notImpl "Vector.map2"

    //
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> 'U) (vec : vector<'T>) : vector<'U> =
        // Preconditions
        checkInitialized "vec" vec
        
        // If the vector is empty, return immediately.
        if isEmpty vec then empty
        else
            Array.mapi mapping vec.Elements
            |> vector.UnsafeCreate

    //
    [<CompiledName("MapIndexed2")>]
    let mapi2 (mapping : int -> 'T1 -> 'T2 -> 'U) (vector1 : vector<'T1>) (vector2 : vector<'T2>) : vector<'U> =
        // Preconditions
        checkInitialized "vector1" vector1
        checkInitialized "vector2" vector2
        
        notImpl "Vector.mapi2"

    //
    [<CompiledName("Max")>]
    let max (vec : vector<'T>) : 'T =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.max"

    //
    [<CompiledName("MaxBy")>]
    let maxBy (projection : 'T -> 'U) (vec : vector<'T>) : 'T =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.maxBy"

    //
    [<CompiledName("Min")>]
    let min (vec : vector<'T>) : 'T =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.min"

    //
    [<CompiledName("MinBy")>]
    let minBy (projection : 'T -> 'U) (vec : vector<'T>) : 'T =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.minBy"

    //
    [<CompiledName("OfList")>]
    let ofList (list : 'T list) : vector<'T> =
        // Preconditions
        checkNonNull "list" list

        match list with
        | [] -> empty
        | _ ->
            Array.ofList list
            |> vector.UnsafeCreate

    //
    [<CompiledName("OfSeq")>]
    let ofSeq (source : seq<'T>) : vector<'T> =
        // Preconditions
        checkNonNull "source" source
        
        notImpl "Vector.ofSeq"

    //
    [<CompiledName("Partition")>]
    let partition (predicate : 'T -> bool) (vec : vector<'T>) : vector<'T> * vector<'T> =
        // Preconditions
        checkInitialized "vec" vec

        // If the vector is empty return immediately.
        if isEmpty vec then empty, empty
        else
            let trueVec, falseVec = Array.partition predicate vec.Elements
            vector.UnsafeCreate trueVec,
            vector.UnsafeCreate falseVec

    //
    [<CompiledName("Permute")>]
    let permute (indexMap : int -> int) (vec : vector<'T>) : vector<'T> =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.permute"

    //
    [<CompiledName("Reduce")>]
    let reduce (reduction : 'T -> 'T -> 'T) (vec : vector<'T>) : 'T =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.reduce"

    //
    [<CompiledName("ReduceBack")>]
    let reduceBack (reduction : 'T -> 'T -> 'T) (vec : vector<'T>) : 'T =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.reduceBack"

    //
    [<CompiledName("Rev")>]
    let rev (vec : vector<'T>) : vector<'T> =
        // Preconditions
        checkInitialized "vec" vec

        // If the input vector is empty return immediately.
        if isEmpty vec then empty
        else
            Array.rev vec.Elements
            |> vector.UnsafeCreate

    //
    [<CompiledName("Scan")>]
    let scan (folder : 'State -> 'T -> 'State) (state : 'State) (vec : vector<'T>) : vector<'State> =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.scan"

    //
    [<CompiledName("ScanBack")>]
    let scanBack (folder : 'T -> 'State -> 'State) (vec : vector<'T>) (state : 'State) : vector<'State> =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.scanBack"

    //
    [<CompiledName("Sub")>]
    let sub (vec : vector<'T>) (startIndex : int) (count : int) =
        // Preconditions
        checkInitialized "vec" vec
        // TODO : startIndex in bounds, count >= 0
        
        notImpl "Vector.sub"

    //
    [<CompiledName("Sort")>]
    let sort (vec : vector<'T>) : vector<'T> =
        // Preconditions
        checkInitialized "vec" vec

        // If the vector is empty return immediately.
        if isEmpty vec then empty
        else
            Array.sort vec.Elements
            |> vector.UnsafeCreate

    //
    [<CompiledName("SortBy")>]
    let sortBy (projection : 'T -> 'Key) (vec : vector<'T>) : vector<'T> =
        // Preconditions
        checkInitialized "vec" vec
        
        // If the vector is empty return immediately.
        if isEmpty vec then empty
        else
            Array.sortBy projection vec.Elements
            |> vector.UnsafeCreate

    //
    [<CompiledName("SortWith")>]
    let sortWith (comparer : 'T -> 'T -> int) (vec : vector<'T>) : vector<'T> =
        // Preconditions
        checkInitialized "vec" vec
        
        // If the vector is empty return immediately.
        if isEmpty vec then empty
        else
            Array.sortWith comparer vec.Elements
            |> vector.UnsafeCreate

    //
    [<CompiledName("Sum")>]
    let inline sum (vec : vector<'T>) : ^T =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.sum"

    //
    [<CompiledName("SumBy")>]
    let inline sumBy (projection : 'T -> ^U) (vec : vector<'T>) : ^U =
        // Preconditions
        checkInitialized "vec" vec
        
        notImpl "Vector.sumBy"

    //
    [<CompiledName("ToList")>]
    let toList (vec : vector<'T>) : 'T list =
        // Preconditions
        checkInitialized "vec" vec

        // If the vector is empty return immediately.
        if isEmpty vec then List.empty
        else
            Array.toList vec.Elements

    //
    [<CompiledName("ToSeq")>]
    let toSeq (vec : vector<'T>) : seq<'T> =
        // Preconditions
        checkInitialized "vec" vec
        
        // If the vector is empty return immediately.
        if isEmpty vec then Seq.empty
        else
            Seq.readonly vec.Elements

    //
    [<CompiledName("TryFind")>]
    let tryFind (predicate : 'T -> bool) (vec : vector<'T>) : 'T option =
        // Preconditions
        checkInitialized "vec" vec

        // If the vector is empty return immediately.
        if isEmpty vec then None
        else
            Array.tryFind predicate vec.Elements

    //
    [<CompiledName("TryFindIndex")>]
    let tryFindIndex (predicate : 'T -> bool) (vec : vector<'T>) : int option =
        // Preconditions
        checkInitialized "vec" vec
        
        // If the vector is empty return immediately.
        if isEmpty vec then None
        else
            Array.tryFindIndex predicate vec.Elements

    //
    [<CompiledName("Unzip")>]
    let unzip (vec : vector<'T1 * 'T2>) : vector<'T1> * vector<'T2> =
        // Preconditions
        checkInitialized "vec" vec

        // If the vector is empty return immediately.
        if isEmpty vec then
            empty, empty
        else
            let vec1, vec2 = Array.unzip vec.Elements
            vector.UnsafeCreate vec1,
            vector.UnsafeCreate vec2

    //
    [<CompiledName("Unzip3")>]
    let unzip3 (vec : vector<'T1 * 'T2 * 'T3>) : vector<'T1> * vector<'T2> * vector<'T3> =
        // Preconditions
        checkInitialized "vec" vec
        
        // If the vector is empty return immediately.
        if isEmpty vec then
            empty, empty, empty
        else
            let vec1, vec2, vec3 = Array.unzip3 vec.Elements
            vector.UnsafeCreate vec1,
            vector.UnsafeCreate vec2,
            vector.UnsafeCreate vec3

    //
    [<CompiledName("Zip")>]
    let zip (vector1 : vector<'T1>) (vector2 : vector<'T2>) : vector<'T1 * 'T2> =
        // Preconditions
        checkInitialized "vector1" vector1
        checkInitialized "vector2" vector2
        
        notImpl "Vector.zip"

    //
    [<CompiledName("Zip3")>]
    let zip3 (vector1 : vector<'T1>) (vector2 : vector<'T2>) (vector3 : vector<'T3>)
        : vector<'T1 * 'T2 * 'T3> =
        // Preconditions
        checkInitialized "vector1" vector1
        checkInitialized "vector2" vector2
        checkInitialized "vector3" vector3
        
        notImpl "Vector.zip3"

    /// Builds a vector that contains the elements of the set in order.
    [<CompiledName("OfSet")>]
    let inline ofSet (set : Set<'T>) : vector<'T> =
        // Preconditions
        checkNonNull "set" set

        vector.Create (Set.toArray set)

    /// Builds a set that contains the same elements as the given vector.
    [<CompiledName("ToSet")>]
    let toSet (vec : vector<'T>) : Set<'T> =
        // Preconditions
        checkInitialized "vec" vec

        Set.ofArray vec.Elements

    /// Applies a function to each element of the array, returning a new array whose elements are
    /// tuples of the original element and the function result for that element.
    [<CompiledName("ProjectValues")>]
    let projectValues (projection : 'Key -> 'T) (vec : vector<'Key>) : vector<'Key * 'T> =
        // Preconditions
        checkInitialized "vec" vec

        vec |> map (fun x -> x, projection x)

    /// Applies a function to each element of the array, returning a new array whose elements are
    /// tuples of the original element and the function result for that element.
    [<CompiledName("ProjectKeys")>]
    let projectKeys (projection : 'T -> 'Key) (vec : vector<'T>) : vector<'Key * 'T> =
        // Preconditions
        checkInitialized "vec" vec

        vec |> map (fun x -> projection x, x)

    /// Returns the first element in the array.
    [<CompiledName("First")>]
    let inline first (vec : vector<'T>) : 'T =
        if isEmpty vec then
            invalidOp "Cannot retrieve the first element of an empty vector."
        else vec.[0]

    /// Returns the index of the last element in the array.
    [<CompiledName("LastIndex")>]
    let inline lastIndex (vec : vector<'T>) : int =
        if isEmpty vec then
            invalidOp "The array is empty."
        else vec.Length - 1

    /// Returns the last element in the array.
    [<CompiledName("Last")>]
    let inline last (vec : vector<'T>) : 'T =
        if isEmpty vec then
            invalidOp "Cannot retrieve the last element of an empty vector."
        else vec.[vec.Length - 1]

    (*
    /// Determines if an array contains a specified value.
    [<CompiledName("Contains")>]
    let contains value (vector : vector<'T>) : bool =
        // Preconditions
        checkInitialized "vector" vector

        vector.IndexOf value <> -1

    /// <summary>
    /// Returns a new collection containing the indices of the elements for which
    /// the given predicate returns &quot;true&quot;.
    /// </summary>
    [<CompiledName("FindIndices")>]
    let findIndices (predicate : 'T -> bool) (vector : vector<'T>) : int[] =
        // Preconditions
        checkNonNull "vector" vector

        let indices = ResizeArray ()
        array |> Array.iteri (fun idx el ->
            if predicate el then indices.Add idx)
        indices.ToArray ()

    /// <summary>
    /// Applies the given function to each element of the array.
    /// Returns the array comprised of the results <c>x</c> for each element where the function
    /// returns <c>Some(x)</c>. The integer index passed to the function indicates the index
    /// of the element being transformed.
    /// </summary>
    [<CompiledName("ChooseIndexed")>]
    let choosei (chooser : int -> 'T -> 'U option) (vector : vector<'T>) : vector<'U> =
        // Preconditions
        checkNonNull "vector" vector

        let chooser = FSharpFunc<_,_,_>.Adapt chooser

        let chosen = ResizeArray ()
        let len = Array.length array

        for i = 0 to len - 1 do
            match chooser.Invoke (i, array.[i]) with
            | None -> ()
            | Some value ->
                chosen.Add value

        chosen.ToArray ()

    /// <summary>
    /// Applies the given function pairwise to the two arrays.
    /// Returns the array comprised of the results <c>x</c> for each element where the function
    /// returns <c>Some(x)</c>.
    /// </summary>
    [<CompiledName("Choose2")>]
    let choose2 (chooser : 'T1 -> 'T2 -> 'U option) array1 array2 : vector<'U> =
        // Preconditions
        checkNonNull "array1" array1
        checkNonNull "array2" array2
        if Array.length array1 <> Array.length array2 then
            invalidArg "array2" "The arrays have different lengths."

        let chooser = FSharpFunc<_,_,_>.Adapt chooser

        let chosen = ResizeArray ()
        let len = Array.length array1

        for i = 0 to len - 1 do
            match chooser.Invoke (array1.[i], array2.[i]) with
            | None -> ()
            | Some value ->
                chosen.Add value

        chosen.ToArray ()

    /// Applies a function to each element of the collection, threading an accumulator argument through the computation.
    /// The integer index passed to the function indicates the array index of the element being transformed.
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : 'State -> int -> 'T -> 'State) (state : 'State) (vector : vector<'T>) =
        // Preconditions
        checkNonNull "vector" vector

        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        let mutable state = state
        let len = array.Length
        for i = 0 to len - 1 do
            state <- folder.Invoke (state, i, array.[i])
        state

    /// Applies a function to each element of the collection, threading an accumulator argument through the computation.
    /// The integer index passed to the function indicates the array index of the element being transformed.
    [<CompiledName("FoldBackIndexed")>]
    let foldiBack (folder : int -> 'T -> 'State -> 'State) (vector : vector<'T>) (state : 'State) : 'State =
        // Preconditions
        checkNonNull "vector" vector

        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        
        let mutable state = state
        for i = Array.length array - 1 downto 0 do
            state <- folder.Invoke (i, array.[i], state)
        state

    /// Splits an array into one or more arrays; the specified predicate is applied
    /// to each element in the array, and whenever it returns true, that element will
    /// be the first element in one of the "subarrays".
    [<CompiledName("Split")>]
    let split (predicate : 'T -> bool) (vector : vector<'T>) =
        // Preconditions
        checkNonNull "vector" vector

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
    let segment (predicate : 'T -> bool) (vector : vector<'T>) : ArrayView<'T>[] =
        // Preconditions
        checkNonNull "vector" vector
        
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
    let segment2 (predicate : 'T -> 'U -> bool) (array1 : vector<'T>) (array2 : vector<'U>)
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

    /// Splits the collection into two (2) collections, containing the elements for which the given
    /// function returns Choice1Of2 or Choice2Of2, respectively. This function is similar to
    /// Array.partition, but it allows the returned collections to have different types.
    [<CompiledName("MapPartition")>]
    let mapPartition (partitioner : 'T -> Choice<'U1, 'U2>) vector : 'U1[] * 'U2[] =
        // Preconditions
        checkNonNull "vector" vector
    
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

    /// Splits the collection into two (3) collections, containing the elements for which the given
    /// function returns Choice1Of3, Choice2Of3, or Choice3Of3, respectively. This function is similar
    /// to Array.partition, but it allows the returned collections to have different types.
    [<CompiledName("MapPartition")>]
    let mapPartition3 (partitioner : 'T -> Choice<'U1, 'U2, 'U3>) vector : 'U1[] * 'U2[] * 'U3[] =
        // Preconditions
        checkNonNull "vector" vector

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

    /// Applies a mapping function to each element of the array, then repeatedly applies
    /// a reduction function to each pair of results until one (1) result value remains.
    [<CompiledName("MapReduce")>]
    let mapReduce (mapReduction : IMapReduction<'Key, 'T>) (vector : 'Key[]) : 'T =
        // Preconditions
        checkNonNull "mapReduction" mapReduction
        checkNonNull "vector" vector
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
    *)
