(*

Copyright 2002-2012 Microsoft Corporation
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

namespace ExtCore

open System
open System.Collections
open System.Collections.Generic

/// <summary>Immutable array with constant-time access to elements.</summary>
[<Struct; CompiledName("FSharpVector`1")>]
type vector<'T> private (elements : 'T[]) =
    /// The empty vector instance.
    static let empty : vector<'T> = vector (Array.empty)

    /// The value representing the equivalent of 'null' for a vector.
    static member Null
        with get () : vector<'T> =
            Unchecked.defaultof<vector<'T>>

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
    member this.Item
        with get index =
            // Preconditions
            if this.IsNull then
                raise <| System.NullReferenceException "Cannot retrieve an item from a null-equivalent vector."

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
    /// This method is considered "unsafe" and should be used with caution because
    /// the given array is used directly instead of being copied; if the array is
    /// modified by some other code, the vector will also be modified (which violates
    /// the semantics of the type).
    static member UnsafeCreate (source : 'T[]) : vector<'T> =
        // Preconditions
        checkNonNull "source" source

        // Create a new vector directly from the source array.
        vector (source)

    interface IEnumerable with
        /// <inherit />
        member this.GetEnumerator () : IEnumerator =
            // Preconditions
            if this.IsNull then
                invalidOp "Cannot get an enumerator for a null-equivalent vector."

            elements.GetEnumerator ()

    interface IEnumerable<'T> with
        /// <inherit />
        member this.GetEnumerator () : IEnumerator<'T> =
            // Preconditions
            if this.IsNull then
                invalidOp "Cannot get an enumerator for a null-equivalent vector."

            elements.GetEnumerator () :?> IEnumerator<'T>


namespace ExtCore.Collections

open LanguagePrimitives
open OptimizedClosures
open ExtCore


/// Functional operators related to vectors (immutable arrays).
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector =
    /// Checks if the vector has been initialized; if not, an ArgumentNullException is raised.
    [<CompiledName("CheckInitialized")>]
    let inline private checkInitialized paramName (vec : vector<'T>) =
        if vec.IsNull then
            System.ArgumentNullException (
                paramName,
                "The vector has not been initialized; it is equal to the \
                 Vector type's default value (the equivalent of 'null').")
            |> raise

    /// Returns the underlying array for a vector.
    let internal elements (vec : vector<'T>) : 'T[] =
        vec.Elements

    /// Returns an empty vector of the given type.
    [<GeneralizableValue>]
    [<CompiledName("Empty")>]
    let empty<'T> : vector<'T> =
        ExtCore.vector.Empty

    /// Builds a vector from the given array.
    [<CompiledName("OfArray")>]
    let ofArray (array : 'T[]) : vector<'T> =
        // Preconditions
        checkNonNull "array" array

        ExtCore.vector.Create array

    /// <summary>Returns the length of a vector. You can also use property vec.Length.</summary>
    /// <param name="vec">The input vector.</param>
    /// <returns>The length of the vector.</returns>
    [<CompiledName("Length")>]
    let inline length (vec : vector<'T>) : int =
        // Preconditions
        checkInitialized "vec" vec
        
        vec.Length

    /// <summary>Returns true if the given vector is empty, otherwise false.</summary>
    /// <param name="vec">The input vector.</param>
    /// <returns>True if the vector is empty.</returns>
    [<CompiledName("IsEmpty")>]
    let inline isEmpty (vec : vector<'T>) : bool =
        // Preconditions
        checkInitialized "vec" vec
    
        vec.IsEmpty

    /// Given an element, creates a vector containing just that element.
    [<CompiledName("Singleton")>]
    let singleton (value : 'T) : vector<'T> =
        ExtCore.vector.UnsafeCreate [| value |]

    /// <summary>Builds a new vector that contains the elements of the first vector followed by the elements of the second vector.</summary>
    /// <param name="vector1">The first input vector.</param>
    /// <param name="vector2">The second input vector.</param>
    /// <returns>The resulting vector.</returns>
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
            |> ExtCore.vector.UnsafeCreate

    /// <summary>Returns the average of the elements in the vector.</summary>
    /// <param name="vec">The input vector.</param>
    /// <exception cref="System.ArgumentException">Thrown when <paramref name="vec"/> is empty.</exception>
    /// <returns>The average of the elements in the vector.</returns>
    [<CompiledName("Average")>]
    let inline average (vec : vector<'T>) : ^T =
        // Preconditions
        checkInitialized "vec" vec

        let len = vec.Length
        if len = 0 then
            invalidArg "vec" "The vector is empty."

        let mutable acc : ^T = GenericZero
        for i = 0 to len - 1 do
            acc <- Checked.(+) acc vec.[i]
        DivideByInt acc len

    /// <summary>Returns the average of the elements generated by applying the function to each element of the vector.</summary>
    /// <param name="projection">The function to transform the vector elements before averaging.</param>
    /// <param name="vec">The input vector.</param>
    /// <exception cref="System.ArgumentException">Thrown when <c>vector</c> is empty.</exception>
    /// <returns>The computed average.</returns>
    [<CompiledName("AverageBy")>]
    let inline averageBy (projection : 'T -> ^U) (vec : vector<'T>) : ^U =
        // Preconditions
        checkInitialized "vec" vec
        
        let len = vec.Length
        if len = 0 then
            invalidArg "vec" "The vector is empty."

        let mutable acc : ^U = GenericZero
        for i = 0 to len - 1 do
            acc <- Checked.(+) acc (projection vec.[i])
        DivideByInt acc len

    /// <summary>Reads a range of elements from the first vector and write them into the second.</summary>
    /// <param name="source">The source vector.</param>
    /// <param name="sourceIndex">The starting index of the source vector.</param>
    /// <param name="target">The target vector.</param>
    /// <param name="targetIndex">The starting index of the target vector.</param>
    /// <param name="count">The number of elements to copy.</param>
    /// <returns>A new vector created by copying the target vector, then overwriting the specified
    /// range of elements with the elements from the source vector.</returns>
    [<CompiledName("CopyTo")>]
    let blit (source : vector<'T>) sourceIndex (target : vector<'T>) targetIndex count : vector<'T> =
        // Preconditions
        checkInitialized "source" source
        checkInitialized "target" target
        if sourceIndex < 0 then
            invalidArg "sourceIndex" "The source index cannot be negative."
        elif count < 0 then
            invalidArg "count" "The number of elements to copy cannot be negative."
        elif targetIndex < 0 then
            invalidArg "targetIndex" "The target index cannot be negative."
        elif sourceIndex + count > source.Length
            then invalidArg "count" "The number of elements to copy is too large given the source index and source vector length."
        elif targetIndex + count > target.Length then
            invalidArg "count" "The number of elements to copy is too large given the target index and target vector length."
        
        // First, create a copy of the target vector.
        let resultArray = Array.copy target.Elements
        
        // Blit the elements from the source vector into the result.
        Array.blit source.Elements sourceIndex resultArray targetIndex count

        // Return a vector which wraps the result array.
        ExtCore.vector.UnsafeCreate resultArray

    /// <summary>Applies the given function to each element of the vector. Returns
    /// the vector comprised of the results "x" for each element where
    /// the function returns Some(x)</summary>
    /// <param name="chooser">The function to generate options from the elements.</param>
    /// <param name="vec">The input vector.</param>
    /// <returns>The vector of results.</returns>
    [<CompiledName("Choose")>]
    let choose (chooser : 'T -> 'U option) (vec : vector<'T>) : vector<'U> =
        // Preconditions
        checkInitialized "vec" vec

        // If the vector is empty, return immediately.
        if isEmpty vec then
            ExtCore.vector.Empty
        else
            Array.choose chooser vec.Elements
            |> ExtCore.vector.UnsafeCreate

    /// <summary>For each element of the vector, applies the given function. Concatenates all the results and return the combined vector.</summary>
    /// <param name="mapping">The function to create sub-vectors from the input vector elements.</param>
    /// <param name="vec">The input vector.</param>
    /// <returns>The concatenation of the sub-vectors.</returns>
    [<CompiledName("Collect")>]
    let collect (mapping : 'T -> vector<'U>) (vec : vector<'T>) : vector<'U> =
        // Preconditions
        checkInitialized "vec" vec

        // OPTIMIZATION : If the vector is empty return the empty instance.
        if isEmpty vec then
            ExtCore.vector.Empty
        else
            Array.collect (mapping >> elements) vec.Elements
            |> ExtCore.vector.UnsafeCreate

    /// <summary>Builds a new vector that contains the elements of each of the given sequence of vectors.</summary>
    /// <param name="vectors">The input sequence of vectors.</param>
    /// <returns>The concatenation of the sequence of input vectors.</returns>
    [<CompiledName("Concat")>]
    let concat (vectors : seq<vector<'T>>) : vector<'T> =
        // Preconditions
        checkNonNull "vectors" vectors

        // OPTIMIZATION : If the sequence is empty, return the empty vector instance.
        if Seq.isEmpty vectors then
            ExtCore.vector.Empty
        else
            // Check a couple of specific cases so we can use an
            // optimized codepath when possible.
            match vectors with
//            | :? (vector<'T>[]) as vecArray ->
//                // NOTE : Array.map and Array.concat must be used here; if Array.collect is used
//                // some unit tests fail because Array.collect doesn't raise an exception if an
//                // vector returned by the mapping function is null.
//                Array.map elements vecArray
//                |> Array.concat
//                |> ExtCore.vector.UnsafeCreate

//            | :? (vector<vector<'T>>) as vecVector ->
//                notImpl ""

            | _ ->
                vectors
                |> Seq.map elements
                |> Array.concat
                |> ExtCore.vector.UnsafeCreate

    /// <summary>Builds a new vector that contains the elements of the given vector.</summary>
    /// <param name="vec">The input vector.</param>
    /// <returns>A copy of the input vector.</returns>
    [<CompiledName("Copy")>]
    let copy (vec : vector<'T>) : vector<'T> =
        // Preconditions
        checkInitialized "vec" vec

        ExtCore.vector.Create vec.Elements

    /// <summary>Creates a vector whose elements are all initially the given value.</summary>
    /// <param name="count">The length of the vector to create.</param>
    /// <param name="value">The value for the elements.</param>
    /// <returns>The created vector.</returns>
    [<CompiledName("Create")>]
    let create count (value : 'T) : vector<'T> =
        // Preconditions
        if count < 0 then
            invalidArg "count" "The number of elements must be greater than or equal to zero (0)."

        Array.create count value
        |> ExtCore.vector.UnsafeCreate

    /// <summary>Tests if any element of the vector satisfies the given predicate.</summary>
    ///
    /// <remarks>The predicate is applied to the elements of the input vector. If any application 
    /// returns true then the overall result is true and no further elements are tested. 
    /// Otherwise, false is returned.</remarks>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="vec">The input vector.</param>
    /// <returns>True if any result from <c>predicate</c> is true.</returns>
    [<CompiledName("Exists")>]
    let exists (predicate : 'T -> bool) (vec : vector<'T>) : bool =
        // Preconditions
        checkInitialized "vec" vec

        Array.exists predicate vec.Elements

    /// <summary>Tests if any pair of corresponding elements of the vectors satisfies the given predicate.</summary>
    ///
    /// <remarks>The predicate is applied to matching elements in the two collections up to the lesser of the 
    /// two lengths of the collections. If any application returns true then the overall result is 
    /// true and no further elements are tested. Otherwise, if one collections is longer 
    /// than the other then the <c>ArgumentException</c> exception is raised. 
    /// Otherwise, false is returned.</remarks>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="vector1">The first input vector.</param>
    /// <param name="vector2">The second input vector.</param>
    /// <returns>True if any result from <c>predicate</c> is true.</returns>
    [<CompiledName("Exists2")>]
    let exists2 (predicate : 'T1 -> 'T2 -> bool) (vector1 : vector<'T1>) (vector2 : vector<'T2>) : bool =
        // Preconditions
        checkInitialized "vector1" vector1
        checkInitialized "vector2" vector2
        if vector1.Length <> vector2.Length then
            invalidArg "vector2" "The vectors have different lengths."
        
        Array.exists2 predicate vector1.Elements vector2.Elements

    /// <summary>Fills a range of elements of the vector with the given value.</summary>
    /// <param name="target">The target vector.</param>
    /// <param name="targetIndex">The index of the first element to set.</param>
    /// <param name="count">The number of elements to set.</param>
    /// <param name="value">The value to set.</param>
    /// <returns>A new vector created by copying the target vector, then overwriting the specified
    /// range of values with <paramref name="value"/>.</returns>
    [<CompiledName("Fill")>]
    let fill (target : vector<'T>) (targetIndex : int) (count : int) (value : 'T) : vector<'T> =
        // Preconditions
        checkInitialized "target" target
        if targetIndex < 0 then
            invalidArg "targetIndex" "The target index cannot be negative."
        elif count < 0 then
            invalidArg "count" "The number of elements to fill is too great given the target index and target vector length."

        // Create a copy of the target vector.
        let resultArray = Array.copy target.Elements
        
        // Fill the elements in the copied array.
        Array.fill resultArray targetIndex count value

        // Return a vector which wraps the result array.
        ExtCore.vector.UnsafeCreate resultArray

    /// <summary>Returns a new collection containing only the elements of the collection
    /// for which the given predicate returns "true".</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="vec">The input vector.</param>
    /// <returns>An vector containing the elements for which the given predicate returns true.</returns>
    [<CompiledName("Filter")>]
    let filter (predicate : 'T -> bool) (vec : vector<'T>) : vector<'T> =
        // Preconditions
        checkInitialized "vec" vec

        // If the vector is empty, return the empty vector instance.
        if isEmpty vec then
            ExtCore.vector.Empty
        else
            Array.filter predicate vec.Elements
            |> ExtCore.vector.UnsafeCreate

    /// <summary>Returns the first element for which the given function returns 'true'.
    /// Raise <c>KeyNotFoundException</c> if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="vec">The input vector.</param>
    /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown if <c>predicate</c>
    /// never returns true.</exception>
    /// <returns>The first element for which <c>predicate</c> returns true.</returns>
    [<CompiledName("Find")>]
    let find (predicate : 'T -> bool) (vec : vector<'T>) : 'T =
        // Preconditions
        checkInitialized "vec" vec

        Array.find predicate vec.Elements

    /// <summary>Returns the index of the first element in the vector
    /// that satisfies the given predicate. Raise <c>KeyNotFoundException</c> if 
    /// none of the elements satisy the predicate.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="vec">The input vector.</param>
    /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown if <c>predicate</c>
    /// never returns true.</exception>
    /// <returns>The index of the first element in the vector that satisfies the given predicate.</returns>
    [<CompiledName("FindIndex")>]
    let findIndex (predicate : 'T -> bool) (vec : vector<'T>) : int =
        // Preconditions
        checkInitialized "vec" vec

        Array.findIndex predicate vec.Elements

    /// <summary>Tests if all elements of the vector satisfy the given predicate.</summary>
    ///
    /// <remarks>The predicate is applied to the elements of the input collection. If any application 
    /// returns false then the overall result is false and no further elements are tested. 
    /// Otherwise, true is returned.</remarks>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="vec">The input vector.</param>
    /// <returns>True if all of the vector elements satisfy the predicate.</returns>
    [<CompiledName("Forall")>]
    let forall (predicate : 'T -> bool) (vec : vector<'T>) : bool =
        // Preconditions
        checkInitialized "vec" vec

        Array.forall predicate vec.Elements

    /// <summary>Tests if all corresponding elements of the vector satisfy the given predicate pairwise.</summary>
    ///
    /// <remarks>The predicate is applied to matching elements in the two collections up to the lesser of the 
    /// two lengths of the collections. If any application returns false then the overall result is 
    /// false and no further elements are tested. Otherwise, if one collection is longer 
    /// than the other then the <c>ArgumentException</c> exception is raised. 
    /// Otherwise, true is returned.</remarks>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="vector1">The first input vector.</param>
    /// <param name="vector2">The second input vector.</param>
    /// <exception cref="System.ArgumentException">Thrown when the input vectors differ in length.</exception>
    /// <returns>True if all of the vector elements satisfy the predicate.</returns>
    [<CompiledName("Forall2")>]
    let forall2 (predicate : 'T1 -> 'T2 -> bool) (vector1 : vector<'T1>) (vector2 : vector<'T2>) : bool =
        // Preconditions
        checkInitialized "vector1" vector1
        checkInitialized "vector2" vector2
        if vector1.Length <> vector2.Length then
            invalidArg "vector2" "The vectors have different lengths."

        Array.forall2 predicate vector1.Elements vector2.Elements

    /// <summary>Applies a function to each element of the collection, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes 
    /// <c>f (... (f s i0)...) iN</c></summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="vec">The input vector.</param>
    /// <returns>The final state.</returns>
    [<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> 'State) (state : 'State) (vec : vector<'T>) : 'State =
        // Preconditions
        checkInitialized "vec" vec

        Array.fold folder state vec.Elements

    /// <summary>Applies a function to pairs of elements drawn from the two collections, 
    /// left-to-right, threading an accumulator argument
    /// through the computation. The two input
    /// vectors must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="vector1">The first input vector.</param>
    /// <param name="vector2">The second input vector.</param>
    /// <exception cref="System.ArgumentException">Thrown when the input vectors differ in length.</exception>
    /// <returns>The final state.</returns>
    [<CompiledName("Fold2")>]
    let fold2 (folder : 'State -> 'T1 -> 'T2 -> 'State) (state : 'State) (vector1 : vector<'T1>) (vector2 : vector<'T2>) : 'State =
        // Preconditions
        checkInitialized "vector1" vector1
        checkInitialized "vector2" vector2
        if vector1.Length <> vector2.Length then
            invalidArg "vector2" "The vectors have different lengths."
        
        Array.fold2 folder state vector1.Elements vector2.Elements

    /// <summary>Applies a function to each element of the vector, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes 
    /// <c>f i0 (...(f iN s))</c></summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="vec">The input vector.</param>
    /// <param name="state">The initial state.</param>
    /// <returns>The final state.</returns>
    [<CompiledName("FoldBack")>]
    let foldBack (folder : 'T -> 'State -> 'State) (vec : vector<'T>) (state : 'State) : 'State =
        // Preconditions
        checkInitialized "vec" vec

        Array.foldBack folder vec.Elements state

    /// <summary>Apply a function to pairs of elements drawn from the two collections, right-to-left, 
    /// threading an accumulator argument through the computation. The two input
    /// vectors must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="vector1">The first input vector.</param>
    /// <param name="vector2">The second input vector.</param>
    /// <param name="state">The initial state.</param>
    /// <exception cref="System.ArgumentException">Thrown when the input vectors differ in length.</exception>
    /// <returns>The final state.</returns>
    [<CompiledName("FoldBack2")>]
    let foldBack2 (folder : 'T1 -> 'T2 -> 'State -> 'State) (vector1 : vector<'T1>) (vector2 : vector<'T2>) (state : 'State) : 'State =
        // Preconditions
        checkInitialized "vector1" vector1
        checkInitialized "vector2" vector2
        if vector1.Length <> vector2.Length then
            invalidArg "vector2" "The vectors have different lengths."

        Array.foldBack2 folder vector1.Elements vector2.Elements state

    /// <summary>Gets an element from a vector.</summary>
    /// <param name="vec">The input vector.</param>
    /// <param name="index">The input index.</param>
    /// <returns>The value of the vector at the given index.</returns>
    [<CompiledName("Get")>]
    let inline get (vec : vector<'T>) (index : int) : 'T =
        // Preconditions are handled by the vector.Item property.
        vec.[index]

    /// <summary>Creates a vector given the dimension and a generator function to compute the elements.</summary>
    /// <param name="count">The number of elements to initialize.</param>
    /// <param name="initializer">The function to generate the initial values for each index.</param>
    /// <returns>The created vector.</returns>
    [<CompiledName("Init")>]
    let init (count : int) (initializer : int -> 'T) : vector<'T> =
        // Preconditions
        if count < 0 then
            invalidArg "count" "Cannot initialize a vector with a negative number of elements."
        
        // OPTIMIZATION : If the count is zero, return the empty vector instance.
        if count = 0 then
            ExtCore.vector.Empty
        else
            Array.init count initializer
            |> ExtCore.vector.UnsafeCreate

    /// <summary>Applies the given function to each element of the vector.</summary>
    /// <param name="action">The function to apply.</param>
    /// <param name="vec">The input vector.</param>
    [<CompiledName("Iterate")>]
    let iter (action : 'T -> unit) (vec : vector<'T>) : unit =
        // Preconditions
        checkInitialized "vec" vec

        Array.iter action vec.Elements

    /// <summary>Applies the given function to pair of elements drawn from matching indices in two vectors. The
    /// two vectors must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.</summary>
    /// <param name="action">The function to apply.</param>
    /// <param name="vector1">The first input vector.</param>
    /// <param name="vector2">The second input vector.</param>
    /// <exception cref="System.ArgumentException">Thrown when the input vectors differ in length.</exception>
    [<CompiledName("Iterate2")>]
    let iter2 (action : 'T1 -> 'T2 -> unit) (vector1 : vector<'T1>) (vector2 : vector<'T2>) : unit =
        // Preconditions
        checkInitialized "vector1" vector1
        checkInitialized "vector2" vector2
        if vector1.Length <> vector2.Length then
            invalidArg "vector2" "The vectors have different lengths."

        Array.iter2 action vector1.Elements vector2.Elements

    /// <summary>Applies the given function to each element of the vector. The integer passed to the
    /// function indicates the index of element.</summary>
    /// <param name="action">The function to apply to each index and element.</param>
    /// <param name="vec">The input vector.</param>
    [<CompiledName("IterateIndexed")>]
    let iteri (action : int -> 'T -> unit) (vec : vector<'T>) : unit =
        // Preconditions
        checkInitialized "vec" vec
        
        Array.iteri action vec.Elements

    /// <summary>Applies the given function to pair of elements drawn from matching indices in two vectors,
    /// also passing the index of the elements. The two vectors must have the same lengths, 
    /// otherwise an <c>ArgumentException</c> is raised.</summary>
    /// <param name="action">The function to apply to each index and pair of elements.</param>
    /// <param name="vector1">The first input vector.</param>
    /// <param name="vector2">The second input vector.</param>
    /// <exception cref="System.ArgumentException">Thrown when the input vectors differ in length.</exception>
    [<CompiledName("IterateIndexed2")>]
    let iteri2 (action : int -> 'T1 -> 'T2 -> unit) (vector1 : vector<'T1>) (vector2 : vector<'T2>) : unit =
        // Preconditions
        checkInitialized "vector1" vector1
        checkInitialized "vector2" vector2
        if vector1.Length <> vector2.Length then
            invalidArg "vector2" "The vectors have different lengths."
        
        Array.iteri2 action vector1.Elements vector2.Elements

    /// <summary>Builds a new vector whose elements are the results of applying the given function
    /// to each of the elements of the vector.</summary>
    /// <param name="mapping">The function to transform elements of the vector.</param>
    /// <param name="vec">The input vector.</param>
    /// <returns>The vector of transformed elements.</returns>
    [<CompiledName("Map")>]
    let map (mapping : 'T -> 'U) (vec : vector<'T>) : vector<'U> =
        // Preconditions
        checkInitialized "vec" vec

        // If the vector is empty, return immediately.
        if isEmpty vec then empty
        else
            Array.map mapping vec.Elements
            |> ExtCore.vector.UnsafeCreate

    /// <summary>Builds a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise. The two input
    /// vectors must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.</summary>
    /// <param name="mapping">The function to transform the pairs of the input elements.</param>
    /// <param name="vector1">The first input vector.</param>
    /// <param name="vector2">The second input vector.</param>
    /// <exception cref="System.ArgumentException">Thrown when the input vectors differ in length.</exception>
    /// <returns>The vector of transformed elements.</returns>
    [<CompiledName("Map2")>]
    let map2 (mapping : 'T1 -> 'T2 -> 'U) (vector1 : vector<'T1>) (vector2 : vector<'T2>) : vector<'U> =
        // Preconditions
        checkInitialized "vector1" vector1
        checkInitialized "vector2" vector2
        if vector1.Length <> vector2.Length then
            invalidArg "vector2" "The vectors have different lengths."
        
        // If the vectors are empty, return immediately.
        if isEmpty vector1 then
            ExtCore.vector.Empty
        else
            Array.map2 mapping vector1.Elements vector2.Elements
            |> ExtCore.vector.UnsafeCreate

    /// <summary>Builds a new vector whose elements are the results of applying the given function
    /// to each of the elements of the vector. The integer index passed to the
    /// function indicates the index of element being transformed.</summary>
    /// <param name="mapping">The function to transform elements and their indices.</param>
    /// <param name="vec">The input vector.</param>
    /// <returns>The vector of transformed elements.</returns>
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> 'U) (vec : vector<'T>) : vector<'U> =
        // Preconditions
        checkInitialized "vec" vec
        
        // If the vector is empty, return immediately.
        if isEmpty vec then empty
        else
            Array.mapi mapping vec.Elements
            |> ExtCore.vector.UnsafeCreate

    /// <summary>Builds a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise, also passing the index of 
    /// the elements. The two input vectors must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.</summary>
    /// <param name="mapping">The function to transform pairs of input elements and their indices.</param>
    /// <param name="vector1">The first input vector.</param>
    /// <param name="vector2">The second input vector.</param>
    /// <exception cref="System.ArgumentException">Thrown when the input vectors differ in length.</exception>
    /// <returns>The vector of transformed elements.</returns>
    [<CompiledName("MapIndexed2")>]
    let mapi2 (mapping : int -> 'T1 -> 'T2 -> 'U) (vector1 : vector<'T1>) (vector2 : vector<'T2>) : vector<'U> =
        // Preconditions
        checkInitialized "vector1" vector1
        checkInitialized "vector2" vector2
        if vector1.Length <> vector2.Length then
            invalidArg "vector2" "The vectors have different lengths."

        // If the vectors are empty, return immediately.
        if isEmpty vector1 then
            ExtCore.vector.Empty
        else
            Array.mapi2 mapping vector1.Elements vector2.Elements
            |> ExtCore.vector.UnsafeCreate

    /// <summary>Returns the greatest of all elements of the vector, compared via Operators.max on the function result.</summary>
    ///
    /// <remarks>Throws ArgumentException for empty vectors.</remarks>
    /// <param name="vec">The input vector.</param>
    /// <exception cref="System.ArgumentException">Thrown when the input vector is empty.</exception>
    /// <returns>The maximum element.</returns>
    [<CompiledName("Max")>]
    let inline max (vec : vector<'T>) : 'T =
        // Preconditions
        checkInitialized "vec" vec
        
        let len = vec.Length
        if len = 0 then
            invalidArg "vec" "The vector is empty."

        let mutable acc = vec.[0]
        for i = 1 to len - 1 do
            let curr = vec.[i]
            if curr > acc then 
                acc <- curr
        acc

    /// <summary>Returns the greatest of all elements of the vector, compared via Operators.max on the function result.</summary>
    ///
    /// <remarks>Throws ArgumentException for empty vectors.</remarks>
    /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
    /// <param name="vec">The input vector.</param>
    /// <exception cref="System.ArgumentException">Thrown when the input vector is empty.</exception>
    /// <returns>The maximum element.</returns>
    [<CompiledName("MaxBy")>]
    let inline maxBy (projection : 'T -> 'U) (vec : vector<'T>) : 'T =
        // Preconditions
        checkInitialized "vec" vec
        
        let len = vec.Length
        if len = 0 then
            invalidArg "vec" "The vector is empty."

        let mutable accv = vec.[0]
        let mutable acc = projection accv
        for i = 1 to len - 1 do
            let currv = vec.[i]
            let curr = projection currv
            if curr > acc then
                acc <- curr
                accv <- currv
        accv

    /// <summary>Returns the lowest of all elements of the vector, compared via Operators.min.</summary>
    ///
    /// <remarks>Throws ArgumentException for empty vectors</remarks>
    /// <param name="vec">The input vector.</param>
    /// <exception cref="System.ArgumentException">Thrown when the input vector is empty.</exception>
    /// <returns>The minimum element.</returns>
    [<CompiledName("Min")>]
    let inline min (vec : vector<'T>) : 'T =
        // Preconditions
        checkInitialized "vec" vec

        let len = vec.Length
        if len = 0 then
            invalidArg "vec" "The vector is empty."

        let mutable acc = vec.[0]
        for i = 1 to len - 1 do
            let curr = vec.[i]
            if curr < acc then 
                acc <- curr
        acc

    /// <summary>Returns the lowest of all elements of the vector, compared via Operators.min on the function result.</summary>
    ///
    /// <remarks>Throws ArgumentException for empty vectors.</remarks>
    /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
    /// <param name="vec">The input vector.</param>
    /// <exception cref="System.ArgumentException">Thrown when the input vector is empty.</exception>
    /// <returns>The minimum element.</returns>
    [<CompiledName("MinBy")>]
    let inline minBy (projection : 'T -> 'U) (vec : vector<'T>) : 'T =
        // Preconditions
        checkInitialized "vec" vec
        
        let len = vec.Length
        if len = 0 then
            invalidArg "vec" "The vector is empty."

        let mutable accv = vec.[0]
        let mutable acc = projection accv
        for i = 1 to len - 1 do
            let currv = vec.[i]
            let curr = projection currv
            if curr < acc then
                acc <- curr
                accv <- currv
        accv

    /// <summary>Builds a vector from the given list.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>The vector of elements from the list.</returns>
    [<CompiledName("OfList")>]
    let ofList (list : 'T list) : vector<'T> =
        // Preconditions
        checkNonNull "list" list

        match list with
        | [] -> empty
        | _ ->
            Array.ofList list
            |> ExtCore.vector.UnsafeCreate

    /// <summary>Builds a new vector from the given enumerable object.</summary>
    /// <param name="source">The input sequence.</param>
    /// <returns>The vector of elements from the sequence.</returns>
    [<CompiledName("OfSeq")>]
    let ofSeq (source : seq<'T>) : vector<'T> =
        // Preconditions
        checkNonNull "source" source

        // If the source is empty, return the empty vector instance.
        if Seq.isEmpty source then
            ExtCore.vector.Empty
        else
            // Examine the type of the sequence, so we can use
            // an optimized code path if possible.
            match source with
            | :? ('T[]) as arr ->
                ExtCore.vector.Create arr
            | _ ->
                Seq.toArray source
                |> ExtCore.vector.UnsafeCreate

    /// <summary>Splits the collection into two collections, containing the 
    /// elements for which the given predicate returns "true" and "false"
    /// respectively.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="vec">The input vector.</param>
    /// <returns>A pair of vectors. The first containing the elements the predicate evaluated to true,
    /// and the second containing those evaluated to false.</returns>
    [<CompiledName("Partition")>]
    let partition (predicate : 'T -> bool) (vec : vector<'T>) : vector<'T> * vector<'T> =
        // Preconditions
        checkInitialized "vec" vec

        // If the vector is empty return immediately.
        if isEmpty vec then empty, empty
        else
            let trueVec, falseVec = Array.partition predicate vec.Elements
            ExtCore.vector.UnsafeCreate trueVec,
            ExtCore.vector.UnsafeCreate falseVec

    /// <summary>Returns a vector with all elements permuted according to the
    /// specified permutation.</summary>
    /// <param name="indexMap">The function that maps input indices to output indices.</param>
    /// <param name="vec">The input vector.</param>
    /// <returns>The output vector.</returns>
    [<CompiledName("Permute")>]
    let permute (indexMap : int -> int) (vec : vector<'T>) : vector<'T> =
        // Preconditions
        checkInitialized "vec" vec

        // If the input vector is empty, return the empty instance.
        if isEmpty vec then
            ExtCore.vector.Empty
        else
            Array.permute indexMap vec.Elements
            |> ExtCore.vector.UnsafeCreate

    /// <summary>Applies the given function to successive elements, returning the first
    /// result where function returns <c>Some(x)</c> for some <c>x</c>. If the function 
    /// never returns <c>Some(x)</c> then <c>KeyNotFoundException</c> is raised.</summary>
    /// <param name="chooser">The function to generate options from the elements.</param>
    /// <param name="vec">The input vector.</param>
    /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown if every result from
    /// <c>chooser</c> is <c>None</c>.</exception>
    /// <returns>The first result.</returns>
    [<CompiledName("Pick")>]
    let pick (chooser : 'T -> 'U option) (vec : vector<'T>) : 'U =
        // Preconditions
        checkInitialized "vec" vec

        Array.pick chooser vec.Elements

    /// <summary>Applies a function to each element of the vector, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> 
    /// then computes <c>f (... (f i0 i1)...) iN</c>.
    /// Raises ArgumentException if the vector has size zero.</summary>
    /// <param name="reduction">The function to reduce a pair of elements to a single element.</param>
    /// <param name="vec">The input vector.</param>
    /// <exception cref="System.ArgumentException">Thrown when the input vector is empty.</exception>
    /// <returns>The final result of the redcutions.</returns>
    [<CompiledName("Reduce")>]
    let reduce (reduction : 'T -> 'T -> 'T) (vec : vector<'T>) : 'T =
        // Preconditions
        checkInitialized "vec" vec
        
        Array.reduce reduction vec.Elements

    /// <summary>Applies a function to each element of the vector, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> 
    /// then computes <c>f i0 (...(f iN-1 iN))</c>.
    /// Raises ArgumentException if the vector has size zero.</summary>
    /// <param name="reduction">The function to reduce a pair of elements to a single element.</param>
    /// <param name="vec">The input vector.</param>
    /// <exception cref="System.ArgumentException">Thrown when the input vector is empty.</exception>
    /// <returns>The final result of the reductions.</returns>
    [<CompiledName("ReduceBack")>]
    let reduceBack (reduction : 'T -> 'T -> 'T) (vec : vector<'T>) : 'T =
        // Preconditions
        checkInitialized "vec" vec

        Array.reduceBack reduction vec.Elements

    /// <summary>Returns a new vector with the elements in reverse order.</summary>
    /// <param name="vec">The input vector.</param>
    /// <returns>The reversed vector.</returns>
    [<CompiledName("Reverse")>]
    let rev (vec : vector<'T>) : vector<'T> =
        // Preconditions
        checkInitialized "vec" vec

        // If the input vector is empty return immediately.
        if isEmpty vec then empty
        else
            Array.rev vec.Elements
            |> ExtCore.vector.UnsafeCreate

    /// <summary>Like <c>fold</c>, but return the intermediary and final results.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="vec">The input vector.</param>
    /// <returns>The vector of state values.</returns>
    [<CompiledName("Scan")>]
    let scan (folder : 'State -> 'T -> 'State) (state : 'State) (vec : vector<'T>) : vector<'State> =
        // Preconditions
        checkInitialized "vec" vec

        // If the vector is empty, return the empty vector instance.
        if isEmpty vec then
            ExtCore.vector.Empty
        else
            Array.scan folder state vec.Elements
            |> ExtCore.vector.UnsafeCreate

    /// <summary>Like <c>foldBack</c>, but return both the intermediary and final results.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="vec">The input vector.</param>
    /// <param name="state">The initial state.</param>
    /// <returns>The vector of state values.</returns>
    [<CompiledName("ScanBack")>]
    let scanBack (folder : 'T -> 'State -> 'State) (vec : vector<'T>) (state : 'State) : vector<'State> =
        // Preconditions
        checkInitialized "vec" vec
        
        // If the vector is empty, return the empty vector instance.
        if isEmpty vec then
            ExtCore.vector.Empty
        else
            Array.scanBack folder vec.Elements state
            |> ExtCore.vector.UnsafeCreate

    /// <summary>Builds a new vector that contains the given subrange specified by
    /// starting index and length.</summary>
    /// <param name="vec">The input vector.</param>
    /// <param name="startIndex">The index of the first element of the sub vector.</param>
    /// <param name="count">The length of the sub vector.</param>
    /// <returns>The created sub vector.</returns>
    [<CompiledName("GetSubVector")>]
    let sub (vec : vector<'T>) (startIndex : int) (count : int) =
        // Preconditions
        checkInitialized "vec" vec
        if startIndex < 0 then
            invalidArg "startIndex" "The start index cannot be negative."
        elif count < 0 then
            invalidArg "count" "The number of elements cannot be negative."
        elif startIndex + count > length vec then
            invalidArg "count" "The number of elements is too great given the start index and vector length."

        Array.sub vec.Elements startIndex count
        |> vector.UnsafeCreate

    /// <summary>Sorts the elements of a vector, returning a new vector. Elements are compared using Operators.compare. </summary>
    ///
    /// <remarks>This is not a stable sort, i.e. the original order of equal elements is not necessarily preserved. 
    /// For a stable sort, consider using Seq.sort.</remarks>
    /// <param name="vec">The input vector.</param>
    /// <returns>The sorted vector.</returns>
    [<CompiledName("Sort")>]
    let sort (vec : vector<'T>) : vector<'T> =
        // Preconditions
        checkInitialized "vec" vec

        // If the vector is empty return immediately.
        if isEmpty vec then empty
        else
            Array.sort vec.Elements
            |> ExtCore.vector.UnsafeCreate

    /// <summary>Sorts the elements of a vector, using the given projection for the keys and returning a new vector. 
    /// Elements are compared using Operators.compare.</summary>
    ///
    /// <remarks>This is not a stable sort, i.e. the original order of equal elements is not necessarily preserved. 
    /// For a stable sort, consider using Seq.sort.</remarks>
    /// <param name="projection">The function to transform vector elements into the type that is compared.</param>
    /// <param name="vec">The input vector.</param>
    /// <returns>The sorted vector.</returns>
    [<CompiledName("SortBy")>]
    let sortBy (projection : 'T -> 'Key) (vec : vector<'T>) : vector<'T> =
        // Preconditions
        checkInitialized "vec" vec
        
        // If the vector is empty return immediately.
        if isEmpty vec then empty
        else
            Array.sortBy projection vec.Elements
            |> ExtCore.vector.UnsafeCreate

    /// <summary>Sorts the elements of a vector, using the given comparison function as the order, returning a new vector.</summary>
    ///
    /// <remarks>This is not a stable sort, i.e. the original order of equal elements is not necessarily preserved. 
    /// For a stable sort, consider using Seq.sort.</remarks>
    /// <param name="comparer">The function to compare pairs of vector elements.</param>
    /// <param name="vec">The input vector.</param>
    /// <returns>The sorted vector.</returns>
    [<CompiledName("SortWith")>]
    let sortWith (comparer : 'T -> 'T -> int) (vec : vector<'T>) : vector<'T> =
        // Preconditions
        checkInitialized "vec" vec
        
        // If the vector is empty return immediately.
        if isEmpty vec then empty
        else
            Array.sortWith comparer vec.Elements
            |> ExtCore.vector.UnsafeCreate

    /// <summary>Returns the sum of the elements in the vector.</summary>
    /// <param name="vec">The input vector.</param>
    /// <returns>The resulting sum.</returns>
    [<CompiledName("Sum")>]
    let inline sum (vec : vector<'T>) : ^T =
        // Preconditions
        checkInitialized "vec" vec

        let mutable acc : ^T = GenericZero
        let len = vec.Length
        for i = 0 to len - 1 do
            acc <- Checked.(+) acc vec.[i]
        acc

    /// <summary>Returns the sum of the results generated by applying the function to each element of the vector.</summary>
    /// <param name="projection">The function to transform the vector elements into the type to be summed.</param>
    /// <param name="vec">The input vector.</param>
    /// <returns>The resulting sum.</returns>
    [<CompiledName("SumBy")>]
    let inline sumBy (projection : 'T -> ^U) (vec : vector<'T>) : ^U =
        // Preconditions
        checkInitialized "vec" vec

        let mutable acc : ^U = GenericZero
        let len = vec.Length
        for i = 0 to len - 1 do
            acc <- Checked.(+) acc (projection vec.[i])
        acc

    /// <summary>Builds a list from the given vector.</summary>
    /// <param name="vec">The input vector.</param>
    /// <returns>The list of vector elements.</returns>
    [<CompiledName("ToList")>]
    let toList (vec : vector<'T>) : 'T list =
        // Preconditions
        checkInitialized "vec" vec

        // If the vector is empty return immediately.
        if isEmpty vec then List.empty
        else
            Array.toList vec.Elements

    /// <summary>Views the given vector as a sequence.</summary>
    /// <param name="vec">The input vector.</param>
    /// <returns>The sequence of vector elements.</returns>
    [<CompiledName("ToSeq")>]
    let toSeq (vec : vector<'T>) : seq<'T> =
        // Preconditions
        checkInitialized "vec" vec
        
        // If the vector is empty return immediately.
        if isEmpty vec then Seq.empty
        else
            Seq.readonly vec.Elements

    /// <summary>Returns the first element for which the given function returns <c>true</c>.
    /// Return <c>None</c> if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="vec">The input vector.</param>
    /// <returns>The first element that satisfies the predicate, or None.</returns>
    [<CompiledName("TryFind")>]
    let tryFind (predicate : 'T -> bool) (vec : vector<'T>) : 'T option =
        // Preconditions
        checkInitialized "vec" vec

        // If the vector is empty return immediately.
        if isEmpty vec then None
        else
            Array.tryFind predicate vec.Elements

    /// <summary>Returns the index of the first element in the vector
    /// that satisfies the given predicate.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="vec">The input vector.</param>
    /// <returns>The index of the first element that satisfies the predicate, or None.</returns>
    [<CompiledName("TryFindIndex")>]
    let tryFindIndex (predicate : 'T -> bool) (vec : vector<'T>) : int option =
        // Preconditions
        checkInitialized "vec" vec
        
        // If the vector is empty return immediately.
        if isEmpty vec then None
        else
            Array.tryFindIndex predicate vec.Elements

    /// <summary>Applies the given function to successive elements, returning the first
    /// result where function returns <c>Some(x)</c> for some <c>x</c>. If the function 
    /// never returns <c>Some(x)</c> then <c>None</c> is returned.</summary>
    /// <param name="chooser">The function to transform the vector elements into options.</param>
    /// <param name="vec">The input vector.</param>
    /// <returns>The first transformed element that is <c>Some(x)</c>.</returns>
    [<CompiledName("TryPick")>]
    let tryPick (chooser : 'T -> 'U option) (vec : vector<'T>) : 'U option =
        // Preconditions
        checkInitialized "vec" vec

        Array.tryPick chooser vec.Elements

    /// <summary>Splits a vector of pairs into two vectors.</summary>
    /// <param name="vec">The input vector.</param>
    /// <returns>The two vectors.</returns>
    [<CompiledName("Unzip")>]
    let unzip (vec : vector<'T1 * 'T2>) : vector<'T1> * vector<'T2> =
        // Preconditions
        checkInitialized "vec" vec

        // If the vector is empty return immediately.
        if isEmpty vec then
            empty, empty
        else
            let vec1, vec2 = Array.unzip vec.Elements
            ExtCore.vector.UnsafeCreate vec1,
            ExtCore.vector.UnsafeCreate vec2

    /// <summary>Splits a vector of triples into three vectors.</summary>
    /// <param name="vec">The input vector.</param>
    /// <returns>The tuple of three vectors.</returns>
    [<CompiledName("Unzip3")>]
    let unzip3 (vec : vector<'T1 * 'T2 * 'T3>) : vector<'T1> * vector<'T2> * vector<'T3> =
        // Preconditions
        checkInitialized "vec" vec
        
        // If the vector is empty return immediately.
        if isEmpty vec then
            empty, empty, empty
        else
            let vec1, vec2, vec3 = Array.unzip3 vec.Elements
            ExtCore.vector.UnsafeCreate vec1,
            ExtCore.vector.UnsafeCreate vec2,
            ExtCore.vector.UnsafeCreate vec3

    /// <summary>Combines two vectors into an vector of pairs. The two vectors must have equal lengths,
    /// otherwise an <c>ArgumentException</c> is raised.</summary>
    /// <param name="vector1">The first input vector.</param>
    /// <param name="vector2">The second input vector.</param>
    /// <exception cref="System.ArgumentException">Thrown when the input vectors differ in length.</exception>
    /// <returns>The vector of tupled elements.</returns>
    [<CompiledName("Zip")>]
    let zip (vector1 : vector<'T1>) (vector2 : vector<'T2>) : vector<'T1 * 'T2> =
        // Preconditions
        checkInitialized "vector1" vector1
        checkInitialized "vector2" vector2
        if vector1.Length <> vector2.Length then
            invalidArg "vector2" "The vectors have different lengths."

        // If the vectors are empty, return immediately.
        if isEmpty vector1 then
            vector.Empty
        else
            Array.zip vector1.Elements vector2.Elements
            |> vector.UnsafeCreate

    /// <summary>Combines three vectors into an vector of triples. The three vectors must have equal lengths,
    /// otherwise an <c>ArgumentException</c> is raised.</summary>
    /// <param name="vector1">The first input vector.</param>
    /// <param name="vector2">The second input vector.</param>
    /// <param name="vector3">The third input vector.</param>
    /// <exception cref="System.ArgumentException">Thrown when the input vectors differ in length.</exception>
    /// <returns>The vector of tupled elements.</returns>
    [<CompiledName("Zip3")>]
    let zip3 (vector1 : vector<'T1>) (vector2 : vector<'T2>) (vector3 : vector<'T3>)
        : vector<'T1 * 'T2 * 'T3> =
        // Preconditions
        checkInitialized "vector1" vector1
        checkInitialized "vector2" vector2
        checkInitialized "vector3" vector3
        if vector1.Length <> vector2.Length then
            invalidArg "vector2" "The vectors have different lengths."
        elif vector1.Length <> vector3.Length then
            invalidArg "vector3" "The vectors have different lengths."

        // If the vectors are empty, return immediately.
        if isEmpty vector1 then
            vector.Empty
        else
            Array.zip3 vector1.Elements vector2.Elements vector3.Elements
            |> vector.UnsafeCreate

    /// <summary>Creates a vector where the entries are initially the default value Unchecked.defaultof&lt;'T&gt;.</summary>
    /// <param name="count">The length of the vector to create.</param>
    /// <returns>The created vector.</returns>
    [<CompiledName("ZeroCreate")>]
    let zeroCreate count : vector<'T> =
        // Preconditions
        if count < 0 then
            invalidArg "count" "The number of elements cannot be negative."

        // OPTIMIZATION : If the count is zero return the empty vector instance.
        if count = 0 then
            ExtCore.vector.Empty
        else
            Array.zeroCreate count
            |> ExtCore.vector.UnsafeCreate

    /// Builds a vector that contains the elements of the set in order.
    [<CompiledName("OfSet")>]
    let inline ofSet (set : Set<'T>) : vector<'T> =
        // Preconditions
        checkNonNull "set" set

        ExtCore.vector.Create (Set.toArray set)

    /// Builds a set that contains the same elements as the given vector.
    [<CompiledName("ToSet")>]
    let toSet (vec : vector<'T>) : Set<'T> =
        // Preconditions
        checkInitialized "vec" vec

        Set.ofArray vec.Elements

    /// Applies a function to each element of the vector, returning a new vector whose elements are
    /// tuples of the original element and the function result for that element.
    [<CompiledName("ProjectValues")>]
    let projectValues (projection : 'Key -> 'T) (vec : vector<'Key>) : vector<'Key * 'T> =
        // Preconditions
        checkInitialized "vec" vec

        vec |> map (fun x -> x, projection x)

    /// Applies a function to each element of the vector, returning a new vector whose elements are
    /// tuples of the original element and the function result for that element.
    [<CompiledName("ProjectKeys")>]
    let projectKeys (projection : 'T -> 'Key) (vec : vector<'T>) : vector<'Key * 'T> =
        // Preconditions
        checkInitialized "vec" vec

        vec |> map (fun x -> projection x, x)

    /// Returns the first element in the vector.
    [<CompiledName("First")>]
    let inline first (vec : vector<'T>) : 'T =
        if isEmpty vec then
            invalidOp "Cannot retrieve the first element of an empty vector."
        else vec.[0]

    /// Returns the index of the last element in the vector.
    [<CompiledName("LastIndex")>]
    let inline lastIndex (vec : vector<'T>) : int =
        if isEmpty vec then
            invalidOp "The vector is empty."
        else vec.Length - 1

    /// Returns the last element in the vector.
    [<CompiledName("Last")>]
    let inline last (vec : vector<'T>) : 'T =
        if isEmpty vec then
            invalidOp "Cannot retrieve the last element of an empty vector."
        else vec.[vec.Length - 1]

    /// Determines if a vector contains a specified value.
    [<CompiledName("Contains")>]
    let contains value (vec : vector<'T>) : bool =
        // Preconditions
        checkInitialized "vec" vec

        System.Array.IndexOf (vec.Elements, value) <> -1

    /// <summary>
    /// Returns a new collection containing the indices of the elements for which
    /// the given predicate returns &quot;true&quot;.
    /// </summary>
    [<CompiledName("FindIndices")>]
    let findIndices (predicate : 'T -> bool) (vec : vector<'T>) : int[] =
        // Preconditions
        checkInitialized "vec" vec

        let indices = ResizeArray ()
        vec.Elements
        |> Array.iteri (fun idx el ->
            if predicate el then indices.Add idx)
        indices.ToArray ()

    /// <summary>
    /// Applies the given function to each element of the vector.
    /// Returns the vector comprised of the results <c>x</c> for each element where the function
    /// returns <c>Some(x)</c>. The integer index passed to the function indicates the index
    /// of the element being transformed.
    /// </summary>
    [<CompiledName("ChooseIndexed")>]
    let choosei (chooser : int -> 'T -> 'U option) (vec : vector<'T>) : vector<'U> =
        // Preconditions
        checkInitialized "vec" vec

        let chooser = FSharpFunc<_,_,_>.Adapt chooser

        let chosen = ResizeArray ()
        let len = length vec

        for i = 0 to len - 1 do
            match chooser.Invoke (i, vec.[i]) with
            | None -> ()
            | Some value ->
                chosen.Add value

        chosen.ToArray ()
        |> vector.UnsafeCreate

    /// <summary>
    /// Applies the given function pairwise to the two vectors.
    /// Returns the vector comprised of the results <c>x</c> for each element where the function
    /// returns <c>Some(x)</c>.
    /// </summary>
    [<CompiledName("Choose2")>]
    let choose2 (chooser : 'T1 -> 'T2 -> 'U option) (vector1 : vector<'T1>) (vector2 : vector<'T2>) : vector<'U> =
        // Preconditions
        checkInitialized "vector1" vector1
        checkInitialized "vector2" vector2
        if length vector1 <> length vector2 then
            invalidArg "vector2" "The vectors have different lengths."

        let chooser = FSharpFunc<_,_,_>.Adapt chooser

        let chosen = ResizeArray ()
        let len = length vector1

        for i = 0 to len - 1 do
            match chooser.Invoke (vector1.[i], vector2.[i]) with
            | None -> ()
            | Some value ->
                chosen.Add value

        chosen.ToArray ()
        |> vector.UnsafeCreate

    /// Applies a function to each element of the collection, threading an accumulator argument through the computation.
    /// The integer index passed to the function indicates the vector index of the element being transformed.
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : 'State -> int -> 'T -> 'State) (state : 'State) (vec : vector<'T>) =
        // Preconditions
        checkInitialized "vec" vec

        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        let mutable state = state
        let len = length vec
        for i = 0 to len - 1 do
            state <- folder.Invoke (state, i, vec.[i])
        state

    /// Applies a function to each element of the collection, threading an accumulator argument through the computation.
    /// The integer index passed to the function indicates the vector index of the element being transformed.
    [<CompiledName("FoldBackIndexed")>]
    let foldiBack (folder : int -> 'T -> 'State -> 'State) (vec : vector<'T>) (state : 'State) : 'State =
        // Preconditions
        checkInitialized "vec" vec

        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        
        let mutable state = state
        for i = length vec - 1 downto 0 do
            state <- folder.Invoke (i, vec.[i], state)
        state

    /// Splits a vector into one or more vectors; the specified predicate is applied
    /// to each element in the vector, and whenever it returns true, that element will
    /// be the first element in one of the "subvectors".
    [<CompiledName("Split")>]
    let split (predicate : 'T -> bool) (vec : vector<'T>) : vector<vector<'T>> =
        // Preconditions
        checkInitialized "vec" vec

        let segments = ResizeArray<_> ()
        let mutable currentSegment = ResizeArray<_> ()

        let len = length vec
        for i = 0 to len - 1 do
            let el = vec.[i]
            if currentSegment.Count > 0 && predicate el then
                currentSegment.ToArray ()
                |> vector.UnsafeCreate
                |> segments.Add

                currentSegment <- ResizeArray<_> ()

            currentSegment.Add el

        // Append the last segment to the segment list,
        // then return the segment list as a vector.
        currentSegment.ToArray ()
        |> vector.UnsafeCreate
        |> segments.Add
        
        segments.ToArray ()
        |> vector.UnsafeCreate
(*
    /// Splits a vector into one or more segments by applying the specified predicate
    /// to each element of the vector and starting a new view at each element where
    /// the predicate returns true.
    [<CompiledName("Segment")>]
    let segment (predicate : 'T -> bool) (vec : vector<'T>) : ArrayView<'T>[] =
        // Preconditions
        checkInitialized "vec" vec
        
        let segments = ResizeArray<_> ()

        let len = length vec
        let mutable segmentLength = 0
        for i = 0 to len - 1 do
            //
            if segmentLength > 0 && predicate vec.[i] then
                // NOTE : The current element is the first element in the *new* segment!
                let offset = i - segmentLength

                ArrayView<_> (vector, offset, segmentLength)
                |> segments.Add

                segmentLength <- 1
            else
                // The existing segment is empty, or the predicate returned false --
                // so "append" the element to the existing vector segment.
                segmentLength <- segmentLength + 1

        // Finish the last/current segment, then return the list of segments as a vector.
        let offset = len - segmentLength

        ArrayView<_> (vector, offset, segmentLength)
        |> segments.Add

        segments.ToArray()

    /// Splits two vectors into one or more segments by applying the specified predicate
    /// to the each pair of vector elements and starting a new view whenever the
    /// predicate returns true.
    [<CompiledName("Segment2")>]
    let segment2 (predicate : 'T -> 'U -> bool) (vector1 : vector<'T>) (vector2 : vector<'U>)
        : ArrayView<'T>[] * ArrayView<'U>[] =
        // Preconditions
        checkInitialized "vector1" vector1
        checkInitialized "vector2" vector2

        let predicate = FSharpFunc<_,_,_>.Adapt predicate
        let len1 = vector1.Length 
        if len1 <> vector2.Length then
            invalidArg "vector2" "The vectors have differing lengths."

        let segments1 = ResizeArray<_> ()
        let segments2 = ResizeArray<_> ()

        let mutable segmentLength = 0
        for i = 0 to len1 - 1 do
            //
            if segmentLength > 0 && predicate.Invoke (vector1.[i], vector2.[i]) then
                // NOTE : The current element is the first element in the *new* segment!
                let offset = i - segmentLength

                ArrayView<_> (vector1, offset, segmentLength)
                |> segments1.Add
                ArrayView<_> (vector2, offset, segmentLength)
                |> segments2.Add

                segmentLength <- 1
            else
                // The existing segment is empty, or the predicate returned false --
                // so "append" the element to the existing vector segment.
                segmentLength <- segmentLength + 1

        // Finish the last/current segment, then return the list of segments as a vector.
        let offset = len1 - segmentLength

        ArrayView<_> (vector1, offset, segmentLength)
        |> segments1.Add
        ArrayView<_> (vector2, offset, segmentLength)
        |> segments2.Add

        segments1.ToArray(), segments2.ToArray()
*)
    /// Splits the collection into two (2) collections, containing the elements for which the given
    /// function returns Choice1Of2 or Choice2Of2, respectively. This function is similar to
    /// Array.partition, but it allows the returned collections to have different types.
    [<CompiledName("MapPartition")>]
    let mapPartition (partitioner : 'T -> Choice<'U1, 'U2>) (vec : vector<'T>) : vector<'U1> * vector<'U2> =
        // Preconditions
        checkInitialized "vec" vec
    
        // OPTIMIZATION : If the vector is empty, return immediately.
        if isEmpty vec then
            vector.Empty, vector.Empty
        else
            // Use ResizeArrays to hold the mapped values.
            let resultList1 = ResizeArray ()
            let resultList2 = ResizeArray ()

            // Partition the vector, adding each element to the ResizeArray
            // specific by the partition function.
            vec.Elements
            |> Array.iter (fun el ->
                match partitioner el with
                | Choice1Of2 value ->
                    resultList1.Add value
                | Choice2Of2 value ->
                    resultList2.Add value)

            // Convert the ResizeArrays to vectors and return them.
            resultList1.ToArray () |> vector.UnsafeCreate,
            resultList2.ToArray () |> vector.UnsafeCreate

    /// Splits the collection into two (3) collections, containing the elements for which the given
    /// function returns Choice1Of3, Choice2Of3, or Choice3Of3, respectively. This function is similar
    /// to Array.partition, but it allows the returned collections to have different types.
    [<CompiledName("MapPartition")>]
    let mapPartition3 (partitioner : 'T -> Choice<'U1, 'U2, 'U3>) (vec : vector<_>)
        : vector<'U1> * vector<'U2> * vector<'U3> =
        // Preconditions
        checkInitialized "vec" vec

        // OPTIMIZATION : If the input vector is empty, immediately return empty results.
        if isEmpty vec then
            vector.Empty, vector.Empty, vector.Empty
        else
            // Use ResizeArrays to hold the mapped values.
            let resultList1 = ResizeArray ()
            let resultList2 = ResizeArray ()
            let resultList3 = ResizeArray ()

            // Partition the vector, adding each element to the ResizeArray
            // specific by the partition function.
            vec.Elements
            |> Array.iter (fun el ->
                match partitioner el with
                | Choice1Of3 value ->
                    resultList1.Add value
                | Choice2Of3 value ->
                    resultList2.Add value
                | Choice3Of3 value ->
                    resultList3.Add value)

            // Convert the ResizeArrays to vectors and return them.
            resultList1.ToArray () |> vector.UnsafeCreate,
            resultList2.ToArray () |> vector.UnsafeCreate,
            resultList3.ToArray () |> vector.UnsafeCreate

    /// Applies a mapping function to each element of the vector, then repeatedly applies
    /// a reduction function to each pair of results until one (1) result value remains.
    [<CompiledName("MapReduce")>]
    let mapReduce (mapReduction : IMapReduction<'Key, 'T>) (vec : vector<'Key>) : 'T =
        // Preconditions
        checkNonNull "mapReduction" mapReduction
        checkInitialized "vec" vec
        if isEmpty vec then
            invalidArg "vec" "The vector is empty."

        // Map the first element of the vector so it can be
        // used as the seed for the fold.
        let mutable state = mapReduction.Map vec.[0]

        // Implement an imperative-style fold, mapping each element
        // then reducing it with the current state to get the new state.
        let len = length vec
        for i = 1 to len - 1 do
            state <- mapReduction.Reduce state (mapReduction.Map vec.[i])

        // Return the final state.
        state

    /// Returns the number of vector elements matching a given predicate.
    [<CompiledName("CountWith")>]
    let countWith (predicate : 'T -> bool) (vec : vector<'T>) : int =
        // Preconditions
        checkInitialized "vec" vec

        let mutable matches = 0
        let len = length vec
        for i = 0 to len - 1 do
            if predicate vec.[i] then
                matches <- matches + 1
    
        // Return the number of matching vector elements.
        matches

#if FX_NO_TPL_PARALLEL
#else
    /// Provides parallel operations on vectors.
    module Parallel =
        open System.Threading.Tasks

        /// <summary>Apply the given function to each element of the vector. Return
        /// the vector comprised of the results "x" for each element where
        /// the function returns Some(x).</summary>
        ///
        /// <remarks>Performs the operation in parallel using System.Threading.Parallel.For.
        /// The order in which the given function is applied to elements of the input vector is not specified.</remarks>
        /// <param name="chooser">The function to generate options from the elements.</param>
        /// <param name="vec">The input vector.</param>
        /// <returns>The vector of results.</returns>
        [<CompiledName("Choose")>]
        let choose (chooser : 'T -> 'U option) (vec : vector<'T>) : vector<'U> =
            // Preconditions
            checkInitialized "vec" vec

            Array.Parallel.choose chooser vec.Elements
            |> vector.UnsafeCreate

        /// <summary>For each element of the vector, apply the given function. Concatenate all the results and return the combined vector.</summary>
        ///
        /// <remarks>Performs the operation in parallel using System.Threading.Parallel.For.
        /// The order in which the given function is applied to elements of the input vector is not specified.</remarks>
        /// <param name="mapping"></param>
        /// <param name="vec">The input vector.</param>
        /// <returns>The concatenation of the sub-vectors.</returns>
        [<CompiledName("Collect")>]
        let collect (mapping : 'T -> vector<'U>) (vec : vector<'T>) : vector<'U> =
            // Preconditions
            checkInitialized "vec" vec

            Array.Parallel.collect (mapping >> elements) vec.Elements
            |> vector.UnsafeCreate

        /// <summary>Apply the given function to each element of the vector. </summary>
        ///
        /// <remarks>Performs the operation in parallel using System.Threading.Parallel.For.
        /// The order in which the given function is applied to elements of the input vector is not specified.</remarks>
        /// <param name="action"></param>
        /// <param name="vec">The input vector.</param>
        [<CompiledName("Iterate")>]
        let iter (action : 'T -> unit) (vec : vector<'T>) : unit =
            // Preconditions
            checkInitialized "vec" vec

            vec.Elements
            |> Array.Parallel.iter action

        /// <summary>Apply the given function to each element of the vector. The integer passed to the
        /// function indicates the index of element.</summary>
        ///
        /// <remarks>Performs the operation in parallel using System.Threading.Parallel.For.
        /// The order in which the given function is applied to elements of the input vector is not specified.</remarks>
        /// <param name="action"></param>
        /// <param name="vec">The input vector.</param>
        [<CompiledName("IterateIndexed")>]
        let iteri (action : int -> 'T -> unit) (vec : vector<'T>) : unit =
            // Preconditions
            checkInitialized "vec" vec

            vec.Elements
            |> Array.Parallel.iteri action

        /// <summary>Create a vector given the dimension and a generator function to compute the elements.</summary>
        /// <remarks>Performs the operation in parallel using System.Threading.Parallel.For.
        /// The order in which the given function is applied to indicies is not specified.</remarks>
        /// <param name="count"></param>
        /// <param name="initializer"></param>
        /// <returns>The created vector.</returns>
        [<CompiledName("Initialize")>]
        let init (count : int) (initializer : int -> 'T) : vector<'T> =
            // Preconditions
            if count < 0 then
                invalidArg "count" "The number of elements to initialize cannot be negative."

            Array.Parallel.init count initializer
            |> vector.UnsafeCreate

        /// <summary>Build a new vector whose elements are the results of applying the given function
        /// to each of the elements of the vector.</summary>
        ///
        /// <remarks>Performs the operation in parallel using System.Threading.Parallel.For.
        /// The order in which the given function is applied to elements of the input vector is not specified.</remarks>
        /// <param name="mapping"></param>
        /// <param name="vec">The input vector.</param>
        /// <returns>The vector of transformed elements.</returns>
        [<CompiledName("Map")>]
        let map (mapping : 'T -> 'U) (vec : vector<'T>) : vector<'U> =
            // Preconditions
            checkInitialized "vec" vec

            vec.Elements
            |> Array.Parallel.map mapping
            |> vector.UnsafeCreate

        /// <summary>Build a new vector whose elements are the results of applying the given function
        /// to each of the elements of the vector. The integer index passed to the
        /// function indicates the index of element being transformed.</summary>
        ///
        /// <remarks>Performs the operation in parallel using System.Threading.Parallel.For.
        /// The order in which the given function is applied to elements of the input vector is not specified.</remarks>
        /// <param name="mapping"></param>
        /// <param name="vec">The input vector.</param>
        /// <returns>The vector of transformed elements.</returns>
        [<CompiledName("MapIndexed")>]
        let mapi (mapping : int -> 'T -> 'U) (vec : vector<'T>) : vector<'U> =
            // Preconditions
            checkInitialized "vec" vec

            vec.Elements
            |> Array.Parallel.mapi mapping
            |> vector.UnsafeCreate

        /// <summary>Split the collection into two collections, containing the 
        /// elements for which the given predicate returns "true" and "false"
        /// respectively.</summary>
        /// <remarks>Performs the operation in parallel using System.Threading.Parallel.For.
        /// The order in which the given function is applied to indicies is not specified.</remarks>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="vec">The input vector.</param>
        /// <returns>A pair of vectors. The first containing the elements the predicate evaluated
        /// to true, and the second containing those evaluated to false.</returns>
        [<CompiledName("Partition")>]
        let partition (predicate : 'T -> bool) (vec : vector<'T>) : vector<'T> * vector<'T> =
            // Preconditions
            checkInitialized "vec" vec

            let trueArray, falseArray =
                Array.Parallel.partition predicate vec.Elements

            vector.UnsafeCreate trueArray,
            vector.UnsafeCreate falseArray

#endif
