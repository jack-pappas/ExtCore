(*

Copyright 2005-2009 Microsoft Corporation
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

(* NOTE : This file has been modified from it's original form in the F# PowerPack. *)

/// Unit tests for the ExtCore.Collections.LazyList type and module.
module Tests.ExtCore.Collections.LazyList

#nowarn "40"

open NUnit.Framework
//open FsCheck


// TODO : Remove this ASAP, replacing any uses with direct calls to Assert.IsTrue.
let private test msg condition =
    Assert.IsTrue (condition, sprintf "MiniTest '%s'" msg)

let private nats =
    0 |> LazyList.unfold (fun z -> Some (z, z + 1))

[<Test>]
let ``Basic Test #1`` () : unit =
    let l = LazyList.ofList [1;2;3]
    
    let res = ref 2
    for i in LazyList.toSeq l do
        res := !res + i
    Assert.AreEqual (8, !res, "test2398984: LazyList.toSeq")

    // TODO : Does this test anything? It's exactly the same as the code above it...
    let res = ref 2
    for i in LazyList.toSeq l do
        res := !res + i
    Assert.AreEqual (8, !res, "test2398984: LazyList.toSeq")
  
[<Test>]
let ``Basic Test #2`` () : unit =
    let res = ref 2

    LazyList.ofList [1;2;3]
    |> LazyList.toSeq
    |> Seq.iter (fun i -> res := !res + i)

    Assert.AreEqual (8, !res, "test2398994: foreach, LazyList.toSeq")

[<Test>]
let se () : unit =
    test "se1" (LazyList.isEmpty LazyList.empty)
    test "se2" (not (LazyList.isEmpty (LazyList.cons 1 LazyList.empty)))
    test "se3" (not (LazyList.isEmpty (LazyList.repeat 1)))
    test "se4" (not (LazyList.isEmpty (LazyList.unfold (fun z -> Some (z,z + 1)) 0)))

[<Test>]
let seq () : unit =
    test "seq1" (LazyList.head (LazyList.cons 1 LazyList.empty) = 1)
    test "seq2" (LazyList.head (LazyList.cons 1 (LazyList.cons 2 LazyList.empty)) = 1)
    test "seq3" (LazyList.head (LazyList.tail (LazyList.cons 1 (LazyList.cons 2 LazyList.empty))) = 2)

[<Test>]
let take () : unit =
    test "take1" (LazyList.toList (LazyList.take 4 nats) = [0;1;2;3])

[<Test>]
let drop () : unit =
    test "drop1" (LazyList.head (LazyList.skip 4 nats) = 4)
    test "drop1" (LazyList.head (LazyList.skip 0 nats) = 0)

[<Test>]
let repeat () : unit =
    test "repeat" (LazyList.toList (LazyList.take 4 (LazyList.repeat 1)) = [1;1;1;1])

[<Test>]
let append () : unit =
    test "append" (LazyList.toList (LazyList.take 4 (LazyList.append (LazyList.cons 77 (LazyList.empty)) nats)) = [77;0;1;2])

    // Test the append (++) operator.
    let lzylst = LazyList.cons 77 (LazyList.empty)
    test "append" (LazyList.toList (LazyList.take 4 (lzylst ++ nats)) = [77;0;1;2])

[<Test>]
let zip () : unit =
    test "zip"  (LazyList.toList (LazyList.take 3 (LazyList.zip nats (LazyList.skip 6 nats))) = [0,6;1,7; 2,8])

[<Test>]
let first () : unit =
    test "firstS" (LazyList.tryFind (fun x -> x>=8) nats = Some 8)
    test "firstN" (LazyList.tryFind (fun x -> x>=8) (LazyList.take 5 nats) = None)

[<Test>]
let find () : unit =
    test "find S" (LazyList.find (fun x -> x>=8) nats = 8)
    test "find N" (let res =
                        try
                            LazyList.find (fun x -> x>=8) (LazyList.take 5 nats)
                        with
                            :? System.Collections.Generic.KeyNotFoundException -> 9999
                   res = 9999) (* testing for exception *)

let rec private diverge () = diverge ()

[<Test>]
let consf () : unit =
    test "consfA"       (LazyList.head (LazyList.consDelayed 1 diverge) = 1)
    test "consfB"       (let ss = LazyList.tail (LazyList.consDelayed 1 diverge) in true) (* testing for lazy divergence *)

[<Test>]
let dropDiverge () : unit =
    test "dropDiverge1" (let ss = LazyList.skip 1 (LazyList.consDelayed 1 diverge) in true) (* testing for lazy divergence *)
    test "dropDiverge0" (let ss = LazyList.skip 0 (LazyList.delayed (fun () -> failwith "errors")) in true) (* testing for lazy divergence *)

[<Test>]
let takedrop () : unit =
    test "takedrop" (LazyList.toList (LazyList.take 3 (LazyList.skip 4 nats)) = [4;5;6])

[<Test>]
let filter () : unit =
    test "filter" (LazyList.toList (LazyList.take 4 (LazyList.filter (fun x -> x % 2 = 0) nats)) = [0;2;4;6])

[<Test>]
let map () : unit =
    test "map"    (LazyList.toList (LazyList.take 4 (LazyList.map (fun x -> x+1) nats)) = [1;2;3;4])

[<Test>]
let map2 () : unit =
    test "map2"   (LazyList.toList (LazyList.take 4 (LazyList.map2 (fun x y -> x*y) nats (LazyList.tail nats))) = [0*1;1*2;2*3;3*4])

[<Test>]
let array () : unit =
    test "array"  (Array.toList (LazyList.toArray (LazyList.take 6 nats)) = LazyList.toList (LazyList.take 6 nats))
    test "array"  (LazyList.toList (LazyList.ofArray [|1;2;3;4|]) = LazyList.toList (LazyList.ofList [1;2;3;4]))

// TODO : Remove this -- replace all calls with Assert.AreEqual.
let inline private check msg (v1 : 'T) (v2 : 'T) =
    Assert.AreEqual (v1, v2, msg)

[<Test>]
let ``check for tail recursion`` () : unit =
    // This checks that LazyList.map, LazyList.length etc. are tail recursive
    check "LazyList.length" (LazyList.ofSeq (Seq.init 100 (fun c -> c)) |> LazyList.length) 100
    check "LazyList.length" (LazyList.ofSeq (Seq.init 1000000 (fun c -> c)) |> LazyList.length) 1000000
    check "LazyList.length" (LazyList.ofSeq (Seq.init 0 (fun c -> c)) |> LazyList.length) 0
    check "LazyList.map" (LazyList.map (fun x -> x + 1) (LazyList.ofSeq (Seq.init 1000000 (fun c -> c))) |> Seq.length) 1000000
    check "LazyList.filter" (LazyList.filter (fun x -> x % 2 = 0) (LazyList.ofSeq (Seq.init 1000000 (fun c -> c))) |> Seq.length) 500000
    check "LazyList.iter" (let count = ref 0 in LazyList.iter (fun x -> incr count) (LazyList.ofSeq (Seq.init 0 (fun c -> c))); !count) 0
    check "LazyList.iter" (let count = ref 0 in LazyList.iter (fun x -> incr count) (LazyList.ofSeq (Seq.init 1000000 (fun c -> c))); !count) 1000000
    check "LazyList.toList" (LazyList.toList (LazyList.ofSeq (Seq.init 200000 (fun c -> c))) |> Seq.length) 200000
    check "LazyList.toArray" (LazyList.toArray (LazyList.ofSeq (Seq.init 200000 (fun c -> c))) |> Seq.length) 200000

[<Test>]
let ``check for termination`` () : unit =
    // check exists on an infinite stream terminates
    check "IEnumerableTest.exists" (Seq.exists ((=) "a") (LazyList.repeat "a" |> LazyList.toSeq)) true
    // check a succeeding 'exists' on a concat of an infinite number of finite streams terminates
    check "IEnumerableTest.exists" (Seq.exists ((=) "a") (Seq.concat (LazyList.repeat [| "a"; "b"|] |> LazyList.toSeq))) true
    // check a succeeding 'exists' on a concat of an infinite number of infinite streams terminates
    check "IEnumerableTest.exists" (Seq.exists ((=) "a") (Seq.concat (LazyList.repeat (LazyList.repeat "a" |> LazyList.toSeq) |> LazyList.toSeq))) true
    // check a failing for_all on an infinite stream terminates
    check "IEnumerableTest.exists" (Seq.forall ((=) "a" >> not) (LazyList.repeat "a" |> LazyList.toSeq)) false
    // check a failing for_all on a concat of an infinite number of finite streams terminates
    check "IEnumerableTest.exists" (Seq.forall ((=) "a" >> not) (Seq.concat (LazyList.repeat [| "a"; "b"|] |> LazyList.toSeq))) false
    check "IEnumerableTest.append, infinite, infinite, then take" (Seq.take 2 (Seq.append (LazyList.repeat "a" |> LazyList.toSeq) (LazyList.repeat "b" |> LazyList.toSeq)) |> Seq.toList) [ "a"; "a" ]
    // check exists on an infinite stream terminates
    check "IEnumerableTest.exists" (Seq.exists ((=) "a") (LazyList.repeat "a" |> LazyList.toSeq |> countEnumeratorsAndCheckedDisposedAtMostOnce)) true
    check "<dispoal>" !numActiveEnumerators 0
    // check a succeeding 'exists' on a concat of an infinite number of finite streams terminates
    check "IEnumerableTest.exists" (Seq.exists ((=) "a") (Seq.concat (LazyList.repeat [| "a"; "b"|] |> LazyList.toSeq |> countEnumeratorsAndCheckedDisposedAtMostOnce))) true
    check "<dispoal>" !numActiveEnumerators 0
    // check a succeeding 'exists' on a concat of an infinite number of infinite streams terminates
    check "IEnumerableTest.exists" (Seq.exists ((=) "a") (Seq.concat (LazyList.repeat (LazyList.repeat "a" |> LazyList.toSeq) |> LazyList.toSeq |> countEnumeratorsAndCheckedDisposedAtMostOnce))) true
    check "<dispoal>" !numActiveEnumerators 0
    // check a failing for_all on an infinite stream terminates
    check "IEnumerableTest.exists" (Seq.forall ((=) "a" >> not) (LazyList.repeat "a" |> LazyList.toSeq |> countEnumeratorsAndCheckedDisposedAtMostOnce)) false
    check "<dispoal>" !numActiveEnumerators 0

    // check a failing for_all on a concat of an infinite number of finite streams terminates
    check "<dispoal>" !numActiveEnumerators 0
    check "IEnumerableTest.exists" (Seq.forall ((=) "a" >> not) (Seq.concat (LazyList.repeat [| "a"; "b"|] |> LazyList.toSeq |> countEnumeratorsAndCheckedDisposedAtMostOnce))) false
    check "<dispoal>" !numActiveEnumerators 0
    check "IEnumerableTest.append, infinite, infinite, then take" (Seq.take 2 (Seq.append (LazyList.repeat "a" |> LazyList.toSeq) (LazyList.repeat "b" |> LazyList.toSeq)) |> countEnumeratorsAndCheckedDisposedAtMostOnceAtEnd |> Seq.toList) [ "a"; "a" ]
    check "<dispoal>" !numActiveEnumerators 0
    

[<Test>]
let ``Active Patterns`` () : unit =
    let matchTwo ll =
        match ll with
        | Cons (h1, Cons (h2, t)) ->
            printf "%O,%O\n" h1 h2
        | Cons (h1, t) ->
            printf "%O\n" h1
        | Nil () ->
            printf "empty!\n"

    let rec pairReduce xs =
        match xs with
        | Cons (x, Cons (y, ys)) ->
            LazyList.consDelayed (x + y) (fun () -> pairReduce ys)
        | Cons (x, Nil) ->
            LazyList.cons x LazyList.empty
        | Nil -> LazyList.empty

    let rec inf =
        LazyList.consDelayed 0 <| fun () ->
            LazyList.map (fun x -> x + 1) inf

    //let ll = LazyList.ofList [1;2;3;4]
    Assert.AreEqual (
        "[1; 5; 9; 13; 17; 21; 25; 29; 33; 37]",
        sprintf "%A" (LazyList.toList (LazyList.take 10 (pairReduce inf))),
        "we09wek")

    Assert.AreEqual (
        [0;1;3],
        LazyList.scan (+) 0 (LazyList.ofList [1;2]) |> LazyList.toList,
        "we09wek")

    Assert.AreEqual (
        [0],
        LazyList.scan (+) 0 (LazyList.ofList []) |> LazyList.toList,
        "we09wek")

[<Test>]
let ``Basic Test #4`` () : unit =
    let lazyList =
        LazyList.empty
        |> LazyList.cons 4
        |> LazyList.cons 3
        |> LazyList.cons 2
        |> LazyList.cons 1

    lazyList
    |> LazyList.head
    |> assertEqual 1

    lazyList
    |> LazyList.tail
    |> LazyList.head
    |> assertEqual 2

    lazyList
    |> LazyList.tail
    |> LazyList.tail
    |> LazyList.head
    |> assertEqual 3

[<Test>]
let ``Basic Test #5`` () : unit =
    let lmn =
        LazyList.empty
        |> LazyList.cons 'N'
        |> LazyList.cons 'M'
        |> LazyList.cons 'L'

    let almnz =
        LazyList.empty
        |> LazyList.cons 'Z'
        |> LazyList.append lmn
        |> LazyList.cons 'A'

    almnz
    |> LazyList.tail
    |> LazyList.tail
    |> LazyList.head
    |> assertEqual 'M'


[<Test>]
let singleton () : unit =
    Assert.Ignore "Test not yet implemented."
