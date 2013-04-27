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

open LanguagePrimitives
open OptimizedClosures
open ExtCore


/// Appends an element to a sequence of elements.
[<CompiledName("AppendSingleton")>]
let inline appendSingleton (value : 'T) sequence =
    Seq.append sequence (Seq.singleton value)

/// Applies a function to each element of the sequence, returning a new sequence whose elements are
/// tuples of the original element and the function result for that element.
[<CompiledName("ProjectValues")>]
let projectValues (mapping : 'Key -> 'T) (source : seq<'Key>) =
    // Preconditions
    checkNonNull "source" source

    source
    |> Seq.map (fun x ->
        x, mapping x)

/// Applies a function to each element of the sequence, returning a new sequence whose elements are
/// tuples of the original element and the function result for that element.
[<CompiledName("ProjectKeys")>]
let projectKeys (mapping : 'T -> 'Key) (source : seq<'T>) =
    // Preconditions
    checkNonNull "source" source

    source
    |> Seq.map (fun x ->
        mapping x, x)

/// Generates a new sequence which repeats the given sequence a specified number of times.
[<CompiledName("Replicate")>]
let replicate count sequence : seq<'T> =
    // Preconditions
    if count < 0 then
        invalidArg "count" "The count cannot be negative."
    // HACK : The F# compiler gives a warning about the type parameter being
    // constrained if 'checkNonNull' is used here, even though it shouldn't.
    // Instead, we'll use Object.ReferenceEquals directly.
    elif System.Object.ReferenceEquals (null, sequence) then
        nullArg "sequence"

    // Cache the input sequence so it's only evaluated once.
    let cachedSeq = Seq.cache sequence

    seq {
    for i = 0 to count - 1 do
        yield! cachedSeq
    }

/// Generates a new sequence which returns the given value
/// an infinite number of times.
[<CompiledName("Repeat")>]
let rec repeat value : seq<'T> =
    seq {
    while true do
        yield value }

/// Creates a cyclical sequence with the specified number of elements using the given
/// generator function. The integer index passed to the function indicates the index
/// of the element being generated.
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
        // The sequence is cached so the generator function is
        // only called once per element.
        let cycledSeq =
            Seq.init count generator
            |> Seq.cache

        // Return a sequence which loops forever and repeats the
        // sequence to be cycled.
        seq {
        while true do
            yield! cycledSeq }

/// Returns the number of elements in the sequence matching a given predicate.
// Seq.countWith predicate sequence = (Seq.filter predicate sequence |> Seq.length)
[<CompiledName("CountWith")>]
let countWith (predicate : 'T -> bool) (sequence : seq<'T>) : int64 =
    // Preconditions
    checkNonNull "sequence" sequence

    // Fold over the sequence, counting the number of elements which
    // match the given predicate.
    (0L, sequence)
    ||> Seq.fold (fun matchCount el ->
        if predicate el then
            matchCount + 1L
        else
            matchCount)
