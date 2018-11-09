(*

Copyright 2018 Jack Pappas

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

namespace Benchmarks.ExtCore.Collections.IntMap

open System
open BenchmarkDotNet.Attributes

//
module MapData =
    open System.Collections.Generic

    //
    let createSequential size =
        Array.init size <| fun i ->
            KeyValuePair<_,_> (i, string i)

    //
    let createRandom size =
        // Create a hash set. We'll use this to check each randomly-created data point
        // to determine if we've already seen it; if not, we add the value (and it's string representation)
        // to the list of data to use for the map test.
        let seen = HashSet<_> ()
        let mutable data = ResizeArray<_> ()
        let rng = Random ()

        while seen.Count < size do
            let currentElement = rng.Next ()

            if seen.Add currentElement then
                data.Add (KeyValuePair<_,_> (currentElement, string currentElement))

        data.ToArray ()


/// Benchmark of lookup (find) performance of maps / dictionaries
/// using integer keys.
//[<ClrJob; CoreJob>]
type MapFindIntKey () =
    let mutable mapData = Array.empty
    let mutable fsharpMap = Map.empty
    let mutable intMap = IntMap.empty
    let mutable dictionary = dict Array.empty

    [<Params(true, false)>]
    member val public Randomized = false with get, set

    [<Params (100, 500, 1000, 2000)>] 
    member val public DictSize = 0 with get, set

    [<GlobalSetup>]
    member self.Setup () : unit =
        // Create data for the test.
        mapData <-
            if self.Randomized then
                MapData.createRandom self.DictSize
            else
                MapData.createSequential self.DictSize

        // Create the data structures using the generated data.
        fsharpMap <-
            (Map.empty, mapData)
            ||> Array.fold (fun map (KeyValue (k, v)) ->
                Map.add k v map)

        intMap <-
            (IntMap.empty, mapData)
            ||> Array.fold (fun map (KeyValue (k, v)) ->
                IntMap.add k v map)

        dictionary <-
            mapData
            |> Seq.map (fun (KeyValue (k, v)) -> k, v)
            |> dict

    [<Benchmark(Baseline = true)>]
    member __.FSharpMap () : unit =
        // Iterate through all keys in the map data, looking each one up
        // in the map (to fetch the associated value).
        for kvp in mapData do
            Map.find kvp.Key fsharpMap |> ignore

    [<Benchmark>]
    member __.IntMap () : unit =
        // Iterate through all keys in the map data, looking each one up
        // in the map (to fetch the associated value).
        for kvp in mapData do
            IntMap.find kvp.Key intMap |> ignore

    [<Benchmark>]
    member __.Dictionary () : unit =
        // Iterate through all keys in the map data, looking each one up
        // in the dictionary (to fetch the associated value).
        for kvp in mapData do
            dictionary.[kvp.Key] |> ignore


/// Benchmark comparing creation time of maps / dictionaries
/// using integer keys.
//[<ClrJob; CoreJob>]
type MapCreateIntKey () =
    let mutable mapData = Array.empty

    [<Params(true, false)>]
    member val public Randomized = false with get, set

    [<Params (100, 500, 1000, 2000)>] 
    member val public DictSize = 0 with get, set

    [<GlobalSetup>]
    member self.Setup () : unit =
        // Create data for the test.
        mapData <-
            if self.Randomized then
                MapData.createRandom self.DictSize
            else
                MapData.createSequential self.DictSize

    [<Benchmark(Baseline = true)>]
    member __.FSharpMap () : unit =
        // Iterate through all keys in the map data, adding each one to the map.
        let mutable map = Map.empty
        for kvp in mapData do
            map <- Map.add kvp.Key kvp.Value map

    [<Benchmark>]
    member __.IntMap () : unit =
        // Iterate through all keys in the map data, adding each one to the map.
        let mutable map = IntMap.empty
        for kvp in mapData do
            map <- IntMap.add kvp.Key kvp.Value map

    [<Benchmark>]
    member __.Dictionary () : unit =
        // Iterate through all keys in the map data, adding each one to the map.
        let dict = System.Collections.Generic.Dictionary<_,_>()
        for kvp in mapData do
            dict.Add(kvp.Key, kvp.Value)
