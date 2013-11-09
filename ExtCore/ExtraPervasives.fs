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

namespace ExtCore


/// Additional F# operators and types that are available without opening a module or namespace.
[<AutoOpen>]
module ExtraTopLevelOperators =
    open LanguagePrimitives
    open ExtCore.Collections

    /// Creates a tagged integer from an integer value.
    [<CompiledName("Tag")>]
    let inline tag<[<Measure>] 'Tag> (value : int) : int<'Tag> =
        Int32WithMeasure value

    /// Removes the tag from a tagged integer, returning the integer value.
    [<CompiledName("Untag")>]
    let inline untag<[<Measure>] 'Tag> (tag : int<'Tag>) : int =
        int tag

    /// Creates a tagged 64-bit integer from a 64-bit integer value.
    [<CompiledName("TagLong")>]
    let inline tagl<[<Measure>] 'Tag> (value : int64) : int64<'Tag> =
        Int64WithMeasure value

    /// Removes the tag from a tagged 64-bit integer, returning the 64-bit integer value.
    [<CompiledName("UntagLong")>]
    let inline untagl<[<Measure>] 'Tag> (tag : int64<'Tag>) : int64 =
        int64 tag

    /// Builds a map from a sequence of key-value pairs.
    [<CompiledName("CreateMap")>]
    let inline map (elements : seq<_>) : Map<'Key, 'T> =
        Map (elements)

    /// Creates an immutable vector from an array.
    [<CompiledName("CreateVector")>]
    let inline vector (array : 'T[]) : vector<'T> =
        vector.Create array

    /// Builds a map from a sequence of key-value pairs.
    [<CompiledName("CreateIntMap")>]
    let inline intMap (elements : seq<_>) : IntMap<'T> =
        IntMap (elements)

    /// Builds a set from a sequence of elements.
    [<CompiledName("CreateIntSet")>]
    let inline intSet (elements : seq<_>) : IntSet =
        IntSet (elements)

    /// Builds a map from a sequence of key-value pairs.
    [<CompiledName("CreateLongMap")>]
    let inline longMap (elements : seq<_>) : LongMap<'T> =
        LongMap (elements)

    /// Builds a set from a sequence of elements.
    [<CompiledName("CreateLongSet")>]
    let inline longSet (elements : seq<_>) : LongSet =
        LongSet (elements)

    /// Builds a map from a sequence of key-value pairs.
    [<CompiledName("CreateHashMap")>]
    let inline hashMap (elements : seq<_>) : HashMap<'Key, 'T> =
        HashMap (elements)

    /// Builds a set from a sequence of elements.
    [<CompiledName("CreateHashSet")>]
    let inline hashSet (elements : seq<_>) : HashSet<'T> =
        HashSet (elements)
