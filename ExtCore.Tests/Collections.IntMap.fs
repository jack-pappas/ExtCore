(*

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

/// Tests for the IntMap data structure.
module ExtCore.Collections.Tests.IntMap

open ExtCore.Collections
open NUnit.Framework
open FsCheck

open System.Collections.Generic

type internal Generators =
    /// Generates a random IntMap.
    static member IntMap () =
        let genIntMap =
            gen {
            let! keys =
                Gen.arrayOf Arb.generate
                |> Gen.map (Seq.distinct >> Seq.toArray)
            let! values = Gen.arrayOfLength (Array.length keys) Arb.generate
            return
                (IntMap.empty, keys, values)
                |||> Array.fold2 (fun map key value ->
                    IntMap.add key value map)
            }

        Arb.fromGen genIntMap


let qq = Arb.register<Generators> ()




let private elementCount<'T> (map : IntMap<'T>) =
    IntMap.count map = (Array.length <| IntMap.toArray map)


[<Test>]
let ``element count`` () =
    Check.QuickThrowOnFailure elementCount<int>


