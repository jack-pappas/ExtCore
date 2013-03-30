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
(*

Some of the tests below were adapted from the unit tests for
the Haskell 'containers' package:
    https://github.com/haskell/containers/blob/master/tests/intmap-properties.hs

*)

/// Unit tests for the ExtCore.Collections.IntMap type and module.
module ExtCore.Collections.IntMap.Tests

open NUnit.Framework
open FsUnit


[<TestCase>]
let index () : unit =
    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.find 5
    |> should equal 'a'

[<TestCase>]
let isEmpty () : unit =
    IntMap.empty
    |> IntMap.isEmpty
    |> should be True

    IntMap.singleton 1 'a'
    |> IntMap.isEmpty
    |> should be False

[<TestCase>]
let count () : unit =
    IntMap.empty
    |> IntMap.count
    |> should equal 0

    IntMap.singleton 1 'a'
    |> IntMap.count
    |> should equal 1

    [(1, 'a'); (2, 'c'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.count
    |> should equal 3

[<TestCase>]
let contains () : unit =
    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.containsKey 5
    |> should be True

    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.containsKey 1
    |> should be False

[<TestCase>]
let singleton () : unit =
    IntMap.singleton 1 'a'
    |> should equal <| IntMap.ofList [(1, 'a')]

    IntMap.singleton 1 'a'
    |> IntMap.count
    |> should equal 1

[<TestCase>]
let add () : unit =
    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.add 5 'x'
    |> should equal (
        IntMap.ofList [(3, 'b'); (5, 'x')])

    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.add 7 'x'
    |> should equal (
        IntMap.ofList [(3, 'b'); (5, 'a'); (7, 'x')])

    IntMap.empty
    |> IntMap.add 5 'x'
    |> should equal (
        IntMap.singleton 5 'x')

[<TestCase>]
let remove () : unit =
    [(5, "a"); (3, "b")]
    |> IntMap.ofList
    |> IntMap.remove 5
    |> should equal (
        IntMap.singleton 3 "b")

    [(5, "a"); (3, "b")]
    |> IntMap.ofList
    |> IntMap.remove 7
    |> should equal (
        IntMap.ofList [(5, "a"); (3, "b")])

    (IntMap.empty : IntMap<string>)
    |> IntMap.remove 5
    |> should equal (IntMap.empty : IntMap<string>)

[<TestCase>]
let map () : unit =
    [(5, "a"); (3, "b")]
    |> IntMap.ofList
    |> IntMap.map (sprintf "%i:%s")
    |> should equal (
        IntMap.ofList [(5, "5:a"); (3, "3:b")])










(* FsCheck Tests *)

open FsCheck

//
let arbIntMap<'T> : Arbitrary<IntMap<'T>> =
    gen {
    let! ks = Arb.generate<int list>
    let! xs = Arb.generate<'T list>

    return IntMap.ofList <| List.zip ks xs
    
    } |> Arb.fromGen

type UMap = IntMap<unit>
type IMap = IntMap<int>
type SMap = IntMap<string>
