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


(* TODO : Implement the VectorView type. *)

(*
/// Functional operators related to the VectorView type (views of immutable arrays).
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module VectorView =
    //
    let dummy () = ()
*)
    (*

    //
    [<CompiledName("Average")>]
    let average (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.average"

    //
    [<CompiledName("AverageBy")>]
    let averageBy (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.averageBy"

    //
    [<CompiledName("Blit")>]
    let blit (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.blit"

    //
    [<CompiledName("Collect")>]
    let collect (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.collect"

    //
    [<CompiledName("Concat")>]
    let concat (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.concat"

    //
    [<CompiledName("Copy")>]
    let copy (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.copy"

    //
    [<CompiledName("Create")>]
    let create (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.create"

    //
    [<CompiledName("TryPick")>]
    let tryPick (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.tryPick"

    //
    [<CompiledName("Pick")>]
    let pick (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.pick"

    //
    [<CompiledName("Choose")>]
    let choose (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.choose"

    //
    [<CompiledName("Empty")>]
    let empty<'T> : Vector<'T> =
        Vector.Empty

    //
    [<CompiledName("Exists")>]
    let exists (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.exists"

    //
    [<CompiledName("Exists2")>]
    let exists2 (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.exists2"

    //
    [<CompiledName("Filter")>]
    let filter (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.filter"

    //
    [<CompiledName("Find")>]
    let find (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.find"

    //
    [<CompiledName("FindIndex")>]
    let findIndex (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.findIndex"

    //
    [<CompiledName("Forall")>]
    let forall (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.forall"

    //
    [<CompiledName("Forall2")>]
    let forall2 (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.forall2"

    //
    [<CompiledName("Fold")>]
    let fold (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.fold"

    //
    [<CompiledName("FoldBack")>]
    let foldBack (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.foldBack"

    //
    [<CompiledName("Get")>]
    let get (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.get"

    //
    [<CompiledName("Init")>]
    let init (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.init"

    //
    [<CompiledName("Iterate")>]
    let iter (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.iter"

    //
    [<CompiledName("Iterate2")>]
    let iter2 (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.iter2"

    //
    [<CompiledName("IterateIndexed")>]
    let iteri (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.iteri"

    //
    [<CompiledName("IterateIndexed2")>]
    let iteri2 (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.iteri2"

    //
    [<CompiledName("Map")>]
    let map (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.map"

    //
    [<CompiledName("Map2")>]
    let map2 (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.map2"

    //
    [<CompiledName("MapIndexed")>]
    let mapi (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.mapi"

    //
    [<CompiledName("MapIndexed2")>]
    let mapi2 (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.mapi2"

    //
    [<CompiledName("Max")>]
    let max (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.max"

    //
    [<CompiledName("MaxBy")>]
    let maxBy (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.maxBy"

    //
    [<CompiledName("Min")>]
    let min (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.min"

    //
    [<CompiledName("MinBy")>]
    let minBy (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.minBy"

    //
    [<CompiledName("OfList")>]
    let ofList (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.ofList"

    //
    [<CompiledName("OfSeq")>]
    let ofSeq (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.ofSeq"

    //
    [<CompiledName("Partition")>]
    let partition (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.partition"

    //
    [<CompiledName("Permute")>]
    let permute (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.permute"

    //
    [<CompiledName("Reduce")>]
    let reduce (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.reduce"

    //
    [<CompiledName("ReduceBack")>]
    let reduceBack (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.reduceBack"

    //
    [<CompiledName("Scan")>]
    let scan (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.scan"

    //
    [<CompiledName("ScanBack")>]
    let scanBack (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.scanBack"

    //
    [<CompiledName("Sub")>]
    let sub (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.sub"

    //
    [<CompiledName("Sort")>]
    let sort (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.sort"

    //
    [<CompiledName("SortBy")>]
    let sortBy (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.sortBy"

    //
    [<CompiledName("SortWith")>]
    let sortWith (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.sortWith"

    //
    [<CompiledName("Sum")>]
    let sum (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.sum"

    //
    [<CompiledName("SumBy")>]
    let sumBy (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.sumBy"

    //
    [<CompiledName("ToList")>]
    let toList (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.toList"

    //
    [<CompiledName("ToSeq")>]
    let toSeq (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.toSeq"

    //
    [<CompiledName("TryFind")>]
    let tryFind (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.tryFind"

    //
    [<CompiledName("TryFindIndex")>]
    let tryFindIndex (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.tryFindIndex"

    //
    [<CompiledName("Unzip")>]
    let unzip (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.unzip"

    //
    [<CompiledName("Unzip3")>]
    let unzip3 (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.unzip3"

    //
    [<CompiledName("Zip")>]
    let zip (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.zip"

    //
    [<CompiledName("Zip3")>]
    let zip3 (vector : vector<'T>) =
        // Preconditions
        checkInitialized "vector" vector
        
        notImpl "Vector.zip3"


    *)

