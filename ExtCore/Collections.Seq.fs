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


//
[<CompiledName("AppendSingleton")>]
let inline appendSingleton sequence (value : 'T) =
    Seq.append sequence (Seq.singleton value)

//
[<CompiledName("ProjectValues")>]
let projectValues (mapping : 'Key -> 'T) (source : seq<'Key>) =
    // Preconditions
    checkNonNull "source" source

    source
    |> Seq.map (fun x ->
        x, mapping x)

//
[<CompiledName("ProjectKeys")>]
let projectKeys (mapping : 'T -> 'Key) (source : seq<'T>) =
    // Preconditions
    checkNonNull "source" source

    source
    |> Seq.map (fun x ->
        mapping x, x)

(* TODO

Seq.choosei

Seq.segment
    Groups elements of a sequence together "longitudinally" -- i.e., it works
    in a streaming fashion, rather than Seq.groupBy which needs to see the
    entire stream before returning. Alternatively, this can be thought of
    as a generalized form of Seq.windowed.

Seq.sample
    Takes a positive integer and a sequence.
    Returns a sequence containing every n-th element of the input sequence.

*)
