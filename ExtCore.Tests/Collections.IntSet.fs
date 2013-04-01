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

/// Unit tests for the ExtCore.Collections.IntSet type and module.
module ExtCore.Collections.IntSet.Tests

open NUnit.Framework
open FsUnit


[<TestCase>]
let isEmpty () : unit =
    IntSet.empty
    |> IntSet.isEmpty
    |> should be True
    
    IntSet.singleton 5
    |> IntSet.isEmpty
    |> should be False

[<TestCase>]
let count () : unit =
    IntSet.empty
    |> IntSet.count
    |> should equal 0

    IntSet.singleton 4
    |> IntSet.count
    |> should equal 1

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.count
    |> should equal 8

[<TestCase>]
let singleton () : unit =
    IntSet.singleton 6
    |> should equal (
        IntSet.empty
        |> IntSet.add 6)

[<TestCase>]
let contains () : unit =
    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.contains 11
    |> should be True

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.contains 6
    |> should be False

[<TestCase>]
let add () : unit =
    IntSet.empty
    |> IntSet.add 5
    |> should equal (
        IntSet.singleton 5)

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.add 5
    |> should equal (IntSet.ofArray
        [| 5; 3; 11; 2; 17; 4; 12; 14 |])

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.add 8
    |> should equal (IntSet.ofArray
        [| 5; 3; 11; 2; 17; 4; 12; 14; 8 |])

[<TestCase>]
let remove () : unit =
    IntSet.singleton 6
    |> IntSet.remove 6
    |> should equal IntSet.empty

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.remove 5
    |> should equal (IntSet.ofArray
        [| 3; 11; 2; 17; 4; 12; 14 |])

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> IntSet.remove 8
    |> should equal (IntSet.ofArray
        [| 5; 3; 11; 2; 17; 4; 12; 14 |])

[<TestCase>]
let union () : unit =
    IntSet.union
        (IntSet.ofArray [| 3; 11; 2; 4; 12 |])
        (IntSet.ofArray [| 5; 11; 17; 4; 14 |])
    |> should equal (
        IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])

[<TestCase>]
let ofSeq () : unit =
    Seq.empty
    |> IntSet.ofSeq
    |> should equal IntSet.empty
    
    seq {
        yield! seq { 2 .. 5 }
        yield 11
        yield 12
        yield 14
        yield 17 }
    |> IntSet.ofSeq
    |> should equal (
        IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])

[<TestCase>]
let ofList () : unit =
    List.empty
    |> IntSet.ofList
    |> should equal IntSet.empty

    [5; 3; 11; 2; 17; 4; 12; 14]
    |> IntSet.ofList
    |> should equal (
        IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])

[<TestCase>]
let ofArray () : unit =
    Array.empty
    |> IntSet.ofArray
    |> should equal IntSet.empty

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> IntSet.ofArray
    |> should equal (
        IntSet.empty
        |> IntSet.add 2
        |> IntSet.add 3
        |> IntSet.add 4
        |> IntSet.add 5
        |> IntSet.add 11
        |> IntSet.add 12
        |> IntSet.add 14
        |> IntSet.add 17)

[<TestCase>]
let ofSet () : unit =
    Set.empty
    |> IntSet.ofSet
    |> should equal IntSet.empty

    [| 5; 3; 11; 2; 17; 4; 12; 14 |]
    |> Set.ofArray
    |> IntSet.ofSet
    |> should equal (
        IntSet.ofArray [| 5; 3; 11; 2; 17; 4; 12; 14 |])

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
//let toSet () : unit =
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
//let choose () : unit =
//    Assert.Fail ()
//
//[<TestCase>]
//let filter () : unit =
//    Assert.Fail ()
//
//[<TestCase>]
//let map () : unit =
//    Assert.Fail ()
//
//[<TestCase>]
//let partition () : unit =
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
//let tryPick () : unit =
//    Assert.Fail ()
//
//[<TestCase>]
//let pick () : unit =
//    Assert.Fail ()


open FsCheck

(* TODO : Implement FsCheck tests. *)
