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


let genIntMap<'T> =
    let genIntMap count =
        // Preconditions
        if count < 0 then
            invalidArg "count" "The number of elements in the generated map cannot be negative."

        let keySet = ref Set.empty
        let map = ref IntMap.Empty

        gen {
        while Set.count !keySet < count do
            let! key = Arb.generate<int>
        
            // Only generate a new value and add the binding
            // to the map if this is a key we haven't seen yet.
            if not <| Set.contains key !keySet then
                // Generate a random value to go with the key.
                let! value = Arb.generate<'T>

                // Add the key to the key set.
                keySet := Set.add key !keySet

                // Add the binding to the map.
                map := IntMap.add key value !map

        // Return the generated IntMap.
        return !map
        }

    Gen.sized genIntMap

// Register the Arbitrary instance for IntMap.
let intMapArb<'T> : Arbitrary<IntMap<'T>> =
    Arb.fromGen genIntMap


let private elementCount<'T> (map : IntMap<'T>) =
    IntMap.count map = (Array.length <| IntMap.toArray map)


[<Test>]
let ``element count`` () =
    Check.QuickThrowOnFailure elementCount<int>


