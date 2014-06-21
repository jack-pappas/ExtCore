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

/// Additional functional operators on sequences.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExtCore.Collections.Seq

open System.Collections.Generic
open LanguagePrimitives
open OptimizedClosures
open ExtCore


/// <summary>Appends an element to a sequence of elements.</summary>
/// <param name="value"></param>
/// <param name="source"></param>
/// <returns></returns>
[<CompiledName("AppendSingleton")>]
let inline appendSingleton (value : 'T) (source : seq<'T>) =
    Seq.append source (Seq.singleton value)

/// <summary>
/// Applies a function to each element of the sequence, returning a new sequence whose elements are
/// tuples of the original element and the function result for that element.
/// </summary>
/// <param name="mapping"></param>
/// <param name="source"></param>
/// <returns></returns>
[<CompiledName("ProjectValues")>]
let projectValues (mapping : 'Key -> 'T) (source : seq<'Key>) =
    // Preconditions
    checkNonNull "source" source

    source
    |> Seq.map (fun x ->
        x, mapping x)

/// <summary>
/// Applies a function to each element of the sequence, returning a new sequence whose elements are
/// tuples of the original element and the function result for that element.
/// </summary>
/// <param name="mapping"></param>
/// <param name="source"></param>
/// <returns></returns>
[<CompiledName("ProjectKeys")>]
let projectKeys (mapping : 'T -> 'Key) (source : seq<'T>) =
    // Preconditions
    checkNonNull "source" source

    source
    |> Seq.map (fun x ->
        mapping x, x)

/// <summary>Generates a new sequence which repeats the given sequence a specified number of times.</summary>
/// <param name="count"></param>
/// <param name="source"></param>
/// <returns></returns>
[<CompiledName("Replicate")>]
let replicate count source : seq<'T> =
    // Preconditions
    if count < 0 then
        invalidArg "count" "The count cannot be negative."
    // HACK : The F# compiler gives a warning about the type parameter being
    // constrained if 'checkNonNull' is used here, even though it shouldn't.
    // Instead, we'll use Object.ReferenceEquals directly.
    elif null == source then
        nullArg "source"

    // Cache the input sequence so it's only evaluated once.
    let cachedSeq = Seq.cache source

    seq {
    for _ in 0 .. count - 1 do
        yield! cachedSeq
    }

/// <summary>Generates a new sequence which returns the given value an infinite number of times.</summary>
/// <param name="value"></param>
/// <returns></returns>
[<CompiledName("Repeat")>]
let rec repeat value : seq<'T> =
    seq {
    while true do
        yield value }

/// <summary>
/// Creates a cyclical sequence with the specified number of elements using the given generator function.
/// The integer index passed to the function indicates the index of the element being generated.
/// </summary>
/// <param name="count"></param>
/// <param name="generator"></param>
/// <returns></returns>
[<CompiledName("Cycle")>]
let cycle count (generator : int -> 'T) : seq<'T> =
    // Preconditions
    if count < 0 then
        invalidArg "count" "The count cannot be negative."
    
    // If the count is zero, return an empty sequence.
    if count = 0 then
        Seq.empty
    else
        // Create the sequence to be cycled.
        // The sequence is cached so the generator function is only called once per element.
        let cycledSeq =
            Seq.init count generator
            |> Seq.cache

        // Return a sequence which loops forever and repeats the sequence to be cycled.
        seq {
        while true do
            yield! cycledSeq }

/// <summary>Returns the number of elements in the sequence matching a given predicate.</summary>
/// <param name="predicate"></param>
/// <param name="source"></param>
/// <returns></returns>
/// <remarks>
/// <c>Seq.countWith predicate source = (Seq.filter predicate source |> Seq.length)</c>
/// </remarks>
[<CompiledName("CountWith")>]
let countWith (predicate : 'T -> bool) (source : seq<'T>) : int64 =
    // Preconditions
    checkNonNull "source" source

    // Fold over the sequence, counting the number of elements which match the given predicate.
    (0L, source)
    ||> Seq.fold (fun matchCount el ->
        if predicate el then
            matchCount + 1L
        else
            matchCount)

/// <summary>Creates a new sequence by sampling a sequence at a given interval.</summary>
/// <param name="interval"></param>
/// <param name="source"></param>
/// <returns></returns>
[<CompiledName("Sample")>]
let sample interval (source : seq<'T>) : seq<'T> =
    // Preconditions
    checkNonNull "source" source
    if interval < 1 then
        argOutOfRange "interval" "The sampling interval is less than one (1)."

    // OPTIMIZATION : If the sampling interval is 1, there's no need to create a new sequence, just return the original.
    if interval = 1 then source
    else
        let enumerator = source.GetEnumerator ()

        //
        let rec sampleRec currentWidth =
            seq {
            if enumerator.MoveNext () then
                if currentWidth = 0 then
                    yield enumerator.Current

                // Recurse, incrementing the interval width, or resetting it if we've hit the end of this interval.
                let currentWidth = currentWidth + 1
                yield! sampleRec (if currentWidth = interval then 0 else currentWidth)
                }

        sampleRec 0

/// <summary>
/// Applies a function to pairs of elements drawn from two sequences, threading an accumulator argument through the computation.
/// If one sequence is shorter than the other then the remaining elements of the longer sequence are ignored.
/// </summary>
/// <param name="folder"></param>
/// <param name="state"></param>
/// <param name="source1"></param>
/// <param name="source2"></param>
/// <returns></returns>
[<CompiledName("Fold2")>]
let fold2 (folder : 'State -> 'T1 -> 'T2 -> 'State) (state : 'State) (source1 : seq<'T1>) (source2 : seq<'T2>) =
    // Preconditions
    checkNonNull "source1" source1
    checkNonNull "source2" source2

    // Get enumerators for the input sequences.
    let enumerator1 = source1.GetEnumerator ()
    let enumerator2 = source2.GetEnumerator ()

    let folder = FSharpFunc<_,_,_,_>.Adapt folder
    let mutable state = state

    // Fold until one (or both) of the input sequences are empty.
    while enumerator1.MoveNext () && enumerator2.MoveNext () do
        state <- folder.Invoke (state, enumerator1.Current, enumerator2.Current)

    // Return the final state value.
    state

/// <summary>
/// Wraps an <see cref="T:System.Collections.Generic.IEnumerator`1{T}"/> to provide an additional 'peek' operation.
/// </summary>
/// <typeparam name="T">The type of objects to enumerate.</typeparam>
[<Sealed>]
type internal PeekableEnumerator<'T> (enumerator : IEnumerator<'T>) =
    /// The current element in the enumerator.
    let mutable current = Unchecked.defaultof<_>
    
    /// Holds the next element from the enumerator, i.e., the element following 'current'.
    /// This is necessary to avoid skipping an element when peeking.
    let mutable nextElement = None
    
    //
    member __.Current
        with get () = current

    //
    member __.Peek () =
        // If we've already seen the next element, we can return the cached value.
        match nextElement with
        | Some _ ->
            nextElement
        | None ->
            // Try to read the next element from the enumerator.
            // If we can read an element, cache it so if Peek() is called multiple times between
            // calls to MoveNext(), we don't consume multiple elements from the underlying enumerator.
            if enumerator.MoveNext () then
                nextElement <- Some enumerator.Current
                nextElement
            else None

    //
    member __.MoveNext () =
        // If we've already read the next element, move that into 'current' and return it
        // without advancing the underlying enumerator.
        match nextElement with
        | Some x ->
            current <- x
            nextElement <- None
            true
        | None ->
            if enumerator.MoveNext () then
                current <- enumerator.Current
                true
            else false

    //
    member __.Reset () =
        enumerator.Reset ()
        current <- Unchecked.defaultof<_>
        nextElement <- None

    interface System.IDisposable with
        member __.Dispose () =
            enumerator.Dispose ()

    interface System.Collections.IEnumerator with
        member this.Current
            with get () = upcast this.Current
        member this.MoveNext () =
            this.MoveNext ()
        member this.Reset () =
            this.Reset ()

    interface System.Collections.Generic.IEnumerator<'T> with
        member this.Current
            with get () = this.Current


/// Helper function used to implement the 'segmentWith' function.
/// Given a peekable enumerator, produces one "segment" subsequence.
let rec private segmentWithImpl (predicate : FSharpFunc<'T, 'T, bool>) (enumerator : PeekableEnumerator<'T>) =
    seq {
    // Yield the first element in the segment.
    yield enumerator.Current

    /// Are there any more elements in this segment?
    match enumerator.Peek () with
    | Some nextElement
        when predicate.Invoke (enumerator.Current, nextElement) ->
        // Move to the next element, then recurse to continue emitting this segment.
        if enumerator.MoveNext () then
            yield! segmentWithImpl predicate enumerator
        else
            // This should never happen -- when a 'Peek' operation returns Some, the following
            // 'MoveNext' operation should always succeed.
            raise <| exn "Peek() returned Some but the following call to MoveNext() returned false."
    | _ -> ()
    }

/// <summary>
/// Groups consecutive elements from a sequence together into "segments".
/// The specified predicate is applied to each adjacent pair of elements in the sequence;
/// a new segment is started whenever the predicate returns 'false'.
/// </summary>
/// <param name="predicate"></param>
/// <param name="source"></param>
/// <returns></returns>
/// <remarks>This function can be thought of as a generalized form of 'Seq.windowed'.</remarks>
[<CompiledName("SegmentWith")>]
let segmentWith (predicate : 'T -> 'T -> bool) (source : seq<'T>) : seq<seq<'T>> =
    // Preconditions
    checkNonNull "source" source

    let predicate = FSharpFunc<_,_,_>.Adapt predicate

    /// Get an enumerator for the input sequence, then create a "peekable" wrapper for it.
    use enumerator = new PeekableEnumerator<_> (source.GetEnumerator ())

    // Create segments using the enumerator until the sequence is empty.
    seq {
    while enumerator.MoveNext () do
        yield segmentWithImpl predicate enumerator
    }
    
/// <summary>
/// Groups consecutive elements from a sequence together into "segments".
/// The specified projection function is applied to each element to produce a key;
/// a new segment is started whenever an element's key is different from the previous element's key.
/// </summary>
/// <param name="projection"></param>
/// <param name="source"></param>
/// <returns></returns>
[<CompiledName("SegmentBy")>]
let segmentBy (projection : 'T -> 'Key) (source : seq<'T>) : seq<seq<'T>> =
    // Preconditions
    checkNonNull "source" source

    // Implement this function in terms of 'segmentWith'.
    source
    |> segmentWith (fun x y ->
        // Consecutive elements belong to the same sequence if the 'projection' function
        // returns the same key for both of them.
        projection x = projection y)
