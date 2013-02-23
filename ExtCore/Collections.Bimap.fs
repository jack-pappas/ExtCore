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

//
namespace ExtCore.Collections

open LanguagePrimitives
open OptimizedClosures
open ExtCore


/// A bi-directional map.
type Bimap<'T1, 'T2
    when 'T1 : comparison
    and 'T2 : comparison> = {
    //
    Left : Map<'T1, 'T2>;
    //
    Right : Map<'T2, 'T1>;
}

/// <summary>A bi-directional TagMap.</summary>
/// <typeparam name="Tag1">The tag (measure) type for the first set of values.</typeparam>
/// <typeparam name="Tag2">The tag (measure) type for the second set of values.</typeparam>
type TagBimap< [<Measure>] 'Tag1, [<Measure>] 'Tag2 > = {
    //
    Left : Map<int<'Tag1>, int<'Tag2>>;
    //
    Right : Map<int<'Tag2>, int<'Tag1>>;
}





