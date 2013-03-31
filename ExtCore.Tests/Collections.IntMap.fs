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

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.count
    |> should equal 8

[<TestCase>]
let singleton () : unit =
    IntMap.singleton 1 'a'
    |> should equal (
        IntMap.add 1 'a' IntMap.empty)

    IntMap.singleton 1 'a'
    |> IntMap.count
    |> should equal 1

[<TestCase>]
let containsKey () : unit =
    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.containsKey 5
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.containsKey 5
    |> should be True

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.containsKey 1
    |> should be False

[<TestCase>]
let tryFind () : unit =
    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.tryFind 5
    |> should equal (Some 'a')

    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.tryFind 7
    |> should equal None

[<TestCase>]
let find () : unit =
    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.find 5
    |> should equal 'a'

[<TestCase>]
let findOrDefault () : unit =
    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.findOrDefault 'z' 5
    |> should equal 'a'

    [(5, 'a'); (3, 'b')]
    |> IntMap.ofList
    |> IntMap.findOrDefault 'z' 7
    |> should equal 'z'

[<TestCase>]
let tryFindKey () : unit =
    (IntMap.empty : IntMap<string>)
    |> IntMap.tryFindKey (fun k v ->
        k % 2 = 0)
    |> should equal None

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.tryFindKey (fun k v ->
        (k + int v) % 11 = 0)
    |> should equal (Some 12)

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.tryFindKey (fun k v ->
        (k + int v) % 289 = 0)
    |> should equal None

[<TestCase>]
let add () : unit =
    let foo =
        [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
            (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
        |> IntMap.ofArray
        |> IntMap.add 5 'x'
    foo
    |> should equal (IntMap.ofArray
       [| (5, 'x'); (3, 'b'); (11, 'f'); (2, 'd');
          (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |])

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.add 13 'x'
    |> should equal (IntMap.ofArray
       [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
          (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); (13, 'x'); |])

    IntMap.empty
    |> IntMap.add 5 'x'
    |> should equal (
        IntMap.singleton 5 'x')

[<TestCase>]
let remove () : unit =
    (IntMap.empty : IntMap<string>)
    |> IntMap.remove 5
    |> should equal (IntMap.empty : IntMap<string>)

    [(5, "a"); (3, "b")]
    |> IntMap.ofList
    |> IntMap.remove 5
    |> should equal (
        IntMap.singleton 3 "b")

    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.remove 4
    |> should equal (IntMap.ofArray
       [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
          (17, 'a'); (12, 'b'); (14, 'c'); |])

[<TestCase>]
let union () : unit =
    IntMap.union
        (IntMap.ofArray [| (3, 'b'); (11, 'F'); (2, 'd'); (4, 'G'); (12, 'b'); |])
        (IntMap.ofArray [| (5, 'a'); (11, 'f'); (17, 'a'); (4, 'g'); (14, 'c'); |])
    |> should equal (
        IntMap.ofArray [| (5, 'a'); (3, 'b'); (11, 'F'); (2, 'd'); (17, 'a'); (4, 'G'); (12, 'b'); (14, 'c'); |])

[<TestCase>]
let ofSeq () : unit =
    (Seq.empty : seq<int * string>)
    |> IntMap.ofSeq
    |> should equal IntMap.empty

//    seq {
//        yield (

//[<TestCase>]
//let ofList () : unit =
//    Assert.Fail ()
//
//[<TestCase>]
//let ofArray () : unit =
//    Assert.Fail ()
//
//[<TestCase>]
//let ofMap () : unit =
//    Assert.Fail ()
//
//[<TestCase>]
//let toSeq () : unit =
//    Assert.Fail ()
//
//[<TestCase>]
//let toList () : unit =
//    Assert.Fail ()
//
//[<TestCase>]
//let toArray () : unit =
//    Assert.Fail ()
//
//[<TestCase>]
//let toMap () : unit =
//    Assert.Fail ()
//
//[<TestCase>]
//let tryPick () : unit =
//    Assert.Fail ()
//
//[<TestCase>]
//let pick () : unit =
//    Assert.Fail ()

[<TestCase>]
let map () : unit =
    [| (5, 'a'); (3, 'b'); (11, 'f'); (2, 'd');
        (17, 'a'); (4, 'g'); (12, 'b'); (14, 'c'); |]
    |> IntMap.ofArray
    |> IntMap.map (sprintf "%i:%c")
    |> should equal (IntMap.ofArray
        [| (5, "5:a"); (3, "3:b"); (11, "11:f"); (2, "2:d");
            (17, "17:a"); (4, "4:g"); (12, "12:b"); (14, "14:c"); |])

//[<TestCase>]
//let filter () : unit =
//    Assert.Fail ()
//
//[<TestCase>]
//let choose () : unit =
//    Assert.Fail ()
//
//[<TestCase>]
//let iter () : unit =
//    Assert.Fail ()
//
//[<TestCase>]
//let iterBack () : unit =
//    Assert.Fail ()
//
//[<TestCase>]
//let fold () : unit =
//    Assert.Fail ()
//
//[<TestCase>]
//let foldBack () : unit =
//    Assert.Fail ()
//
//[<TestCase>]
//let exists () : unit =
//    Assert.Fail ()
//
//[<TestCase>]
//let forall () : unit =
//    Assert.Fail ()
//
//[<TestCase>]
//let partition () : unit =
//    Assert.Fail ()
//
//[<TestCase>]
//let mapPartition () : unit =
//    Assert.Fail ()



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
