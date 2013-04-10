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


#if PROTO_COMPILER
// TODO : Re-implement this module using the F# proto compiler, as in the TagMap and TagSet modules.
// This will avoid the need to create "concrete" implementations of these functions, as we'll be able
// to simply re-use existing implementations for the standard array type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TaggedArray =
    /// Similar to Array.mapi, but 'tags' the index values with a unit-of-measure
    /// type before applying them to the mapping function.
    [<CompiledName("MapIndexed")>]
    let mapi (mapping : int<'Tag> -> 'T -> 'U) (array : 'T[]) : 'U[] =
        // Preconditions
        checkNonNull "array" array
        
        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        let len = array.Length
        let results = Array.zeroCreate len
        for i = 0 to len - 1 do
            results.[i] <- mapping.Invoke (Tag.ofInt<'Tag> i, array.[i])
        results

    /// Similar to Array.mapi, but 'tags' the index values with a unit-of-measure
    /// type before applying them to the mapping function.
    [<CompiledName("MapIndexed")>]
    let mapi2 (mapping : int<'Tag> -> 'T1 -> 'T2 -> 'U) (array1 : 'T1[]) (array2 : 'T2[]) : 'U[] =
        // Preconditions
        checkNonNull "array1" array1
        checkNonNull "array2" array2
        
        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
        let len1 = array1.Length 
        if len1 <> array2.Length then
            invalidArg "array2" "The arrays have differing lengths."

        let results = Array.zeroCreate len1
        for i = 0 to len1 - 1 do
            results.[i] <- mapping.Invoke (Tag.ofInt<'Tag> i, array1.[i], array2.[i])
        results

    /// Applies a function to each element of the collection, threading an accumulator argument through the computation.
    /// The integer index passed to the function indicates the array index of the element being transformed.
    /// The index values are tagged with a unit-of-measure type before applying them to the folder function.
    [<CompiledName("FoldIndexed")>]
    let foldi (folder : int<'Tag> -> 'State -> 'T -> 'State) (state : 'State) (array : 'T[]) : 'State =
        // Preconditions
        checkNonNull "array" array
        
        let folder = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt folder
        let mutable state = state
        let len = Array.length array
        for i = 0 to len - 1 do
            state <- folder.Invoke (Tag.ofInt<'Tag> i, state, array.[i])
        state

    /// Applies a function to each element of the collection, threading an accumulator argument through the computation.
    /// The integer index passed to the function indicates the array index of the element being transformed.
    /// The index values are tagged with a unit-of-measure type before applying them to the folder function.
    [<CompiledName("FoldBackIndexed")>]
    let foldiBack (folder : int<'Tag> -> 'T -> 'State -> 'State) (array : 'T[]) (state : 'State) : 'State =
        // Preconditions
        checkNonNull "array" array

        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        
        let mutable state = state
        for i = Array.length array - 1 downto 0 do
            state <- folder.Invoke (Tag.ofInt<'Tag> i, array.[i], state)
        state

#endif
