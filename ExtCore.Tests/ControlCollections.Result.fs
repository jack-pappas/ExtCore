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

/// Unit tests for the ExtCore.Control.Collections.Choice module.
module Tests.ExtCore.Control.Collections.Result

open ExtCore.Control
open ExtCore.Control.Collections
open NUnit.Framework


/// Tests for the ExtCore.Control.Collections.Result.Array module.
module Array =
    [<Test>]
    let fold () : unit =
        // Test case for an empty array.
        ("", Array.empty)
        ||> Result.Array.fold (fun _ _ -> Error "Error!")
        |> assertEqual (Ok "")

        // Sample usage test cases.
        ("", [| 0..4 |])
        ||> Result.Array.fold (fun str x ->
            if x <> 0 && x % 5 = 0 then Error x
            else Ok (str + ((char (int 'a' + x)).ToString ())))
        |> assertEqual (Ok "abcde")

        ("", [| 0..5 |])
        ||> Result.Array.fold (fun str x ->
            if x <> 0 && x % 5 = 0 then Error x
            else Ok (str + ((char (int 'a' + x)).ToString ())))
        |> assertEqual (Error 5)

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            ("", [| 0..5 |])
            ||> Result.Array.fold (fun str x ->
                incr iterationCount
                if x <> 0 && x % 2 = 0 then Error x
                else Ok (str + ((char (int 'a' + x)).ToString ())))
            |> assertEqual (Error 2)

            !iterationCount |> assertEqual 3

    [<Test>]
    let foldi () : unit =
        // Test case for an empty array.
        ("", Array.empty)
        ||> Result.Array.foldi (fun _ _ _ -> Error "Error!")
        |> assertEqual (Ok "")

        // Sample usage test cases.
        ("", [| 0..4 |])
        ||> Result.Array.foldi (fun idx str x ->
            let y = x * idx
            if y <> 0 && y % 5 = 0 then Error y
            else Ok (str + ((char (int 'a' + x)).ToString ())))
        |> assertEqual (Ok "abcde")

        ("", [| 0..5 |])
        ||> Result.Array.foldi (fun idx str x ->
            let y = x * idx
            if y <> 0 && y % 5 = 0 then Error y
            else Ok (str + ((char (int 'a' + x)).ToString ())))
        |> assertEqual (Error 25)

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            ("", [| 0..5 |])
            ||> Result.Array.foldi (fun idx str x ->
                incr iterationCount
                let y = x * idx
                if y <> 0 && y % 2 = 0 then Error y
                else Ok (str + ((char (int 'a' + x)).ToString ())))
            |> assertEqual (Error 4)

            !iterationCount |> assertEqual 3

    [<Test>]
    let init () : unit =
        // Test case for an empty array.
        Result.Array.init 0 <| fun _ ->
            Error "Error!"
        |> assertEqual (Ok Array.empty)

        // Sample usage test cases.
        Result.Array.init 5 <| fun x ->
            Ok (x * 7)
        |> assertEqual
            <| Ok [| 0; 7; 14; 21; 28; |]

        Result.Array.init 5 <| fun x ->
            if x > 0 && x % 5 = 0 then Error x
            else Ok (x * 2)
        |> assertEqual
            <| Ok [| 0; 2; 4; 6; 8; |]

        Result.Array.init 6 <| fun x ->
            if x > 0 && x % 5 = 0 then Error x
            else Ok (x * 2)
        |> assertEqual (Error 5)

    [<Test>]
    let iter () : unit =
        // Test case for an empty array.
        do
            let iterationCount = ref 0

            Array.empty
            |> Result.Array.iter (fun _ ->
                incr iterationCount
                Error "Error!")
            |> assertEqual (Ok ())

            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let iterationCount = ref 0

            [| 0..4 |]
            |> Result.Array.iter (fun x ->
                incr iterationCount
                if x > 0 && x % 5 = 0 then Error x
                else Ok ())
            |> assertEqual (Ok ())

            !iterationCount |> assertEqual 5

        do
            let iterationCount = ref 0

            [| 0..5 |]
            |> Result.Array.iter (fun x ->
                incr iterationCount
                if x > 0 && x % 5 = 0 then Error x
                else Ok ())
            |> assertEqual (Error 5)

            !iterationCount |> assertEqual 6

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            [| 0..5 |]
            |> Result.Array.iter (fun x ->
                incr iterationCount
                if x > 0 && x % 2 = 0 then Error x
                else Ok ())
            |> assertEqual (Error 2)

            !iterationCount |> assertEqual 3

    [<Test>]
    let iteri () : unit =
        // Test case for an empty array.
        do
            let iterationCount = ref 0

            Array.empty
            |> Result.Array.iteri (fun _ _ ->
                incr iterationCount
                Error "Error!")
            |> assertEqual (Ok ())

            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let iterationCount = ref 0

            [| 0..4 |]
            |> Result.Array.iteri (fun idx x ->
                incr iterationCount
                let y = x * idx
                if y > 0 && y % 5 = 0 then Error y
                else Ok ())
            |> assertEqual (Ok ())

            !iterationCount |> assertEqual 5

        do
            let iterationCount = ref 0

            [| 0..5 |]
            |> Result.Array.iteri (fun idx x ->
                incr iterationCount
                let y = x * idx
                if y > 0 && y % 5 = 0 then Error y
                else Ok ())
            |> assertEqual (Error 25)

            !iterationCount |> assertEqual 6

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            [| 0..4 |]
            |> Result.Array.iteri (fun idx x ->
                incr iterationCount
                let y = x * idx
                if y > 0 && y % 2 = 0 then Error y
                else Ok ())
            |> assertEqual (Error 4)

            !iterationCount |> assertEqual 3

    [<Test>]
    let map () : unit =
        // Test case for an empty array.
        Array.empty
        |> Result.Array.map (fun _ -> Error "Error!")
        |> assertEqual (Ok Array.empty)

        // Sample usage test cases.
        [| 0..4 |]
        |> Result.Array.map (fun x ->
            if x > 0 && x % 5 = 0 then Error x
            else Ok (x * 3))
        |> assertEqual (Ok [| 0; 3; 6; 9; 12; |])

        [| 0..5 |]
        |> Result.Array.map (fun x ->
            if x > 0 && x % 5 = 0 then Error x
            else Ok (x * 3))
        |> assertEqual (Error 5)

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            [| 0..4 |]
            |> Result.Array.map (fun x ->
                incr iterationCount
                if x > 0 && x % 2 = 0 then Error x
                else Ok (x * 3))
            |> assertEqual (Error 2)

            !iterationCount |> assertEqual 3

    [<Test>]
    let mapi () : unit =
        // Test case for an empty array.
        Array.empty
        |> Result.Array.mapi (fun _ _ -> Error "Error!")
        |> assertEqual (Ok Array.empty)

        // Sample usage test cases.
        [| 0..4 |]
        |> Result.Array.mapi (fun idx x ->
            let y = x * idx
            if y > 0 && y % 5 = 0 then Error y
            else Ok (y * 3))
        |> assertEqual (Ok [| 0; 3; 12; 27; 48; |])

        [| 0..5 |]
        |> Result.Array.mapi (fun idx x ->
            let y = x * idx
            if y > 0 && y % 5 = 0 then Error y
            else Ok (y * 3))
        |> assertEqual (Error 25)

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            [| 0..4 |]
            |> Result.Array.mapi (fun idx x ->
                incr iterationCount
                let y = x * idx
                if y > 0 && y % 2 = 0 then Error y
                else Ok (y * 3))
            |> assertEqual (Error 4)

            !iterationCount |> assertEqual 3

    [<Test>]
    let map2 () : unit =
        // Test case for an empty array.
        (Array.empty, Array.empty)
        ||> Result.Array.map2 (fun _ _ -> Error "Error!")
        |> assertEqual (Ok Array.empty)

        // Sample usage test cases.
        ([| 0..4 |], [| 1; 1; 2; 3; 5; |])
        ||> Result.Array.map2 (fun nat fibo ->
            let x = nat + fibo
            if x >= 10 then Error nat
            else Ok x)
        |> assertEqual (Ok [| 1; 2; 4; 6; 9; |])

        ([| 0..5 |], [| 1; 1; 2; 3; 5; 8; |])
        ||> Result.Array.map2 (fun nat fibo ->
            let x = nat + fibo
            if x >= 10 then Error nat
            else Ok x)
        |> assertEqual (Error 5)

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            ([| 0..5 |], [| 1; 1; 2; 3; 5; 8; |])
            ||> Result.Array.map2 (fun nat fibo ->
                incr iterationCount
                let x = nat + fibo
                if x >= 2 then Error nat
                else Ok x)
            |> assertEqual (Error 1)

            !iterationCount |> assertEqual 2

    [<Test>]
    let ``map2 raises exn when arrays have different lengths`` () : unit =
        Assert.Throws<System.ArgumentException>(fun () ->
            ([| 0..4 |], [| 0..7|])
            ||> Result.Array.map2 (fun _ _ -> Error "Error!")
            |> ignore) |> ignore

    [<Test>]
    let reduce () : unit =
        // Sample usage test cases.
        [| 0..4 |]
        |> Result.Array.reduce (fun x y ->
            let z = x + y
            if z > 10 then Error x
            else Ok z)
        |> assertEqual (Ok 10)

        [| 0..5 |]
        |> Result.Array.reduce (fun x y ->
            let z = x + y
            if z > 10 then Error x
            else Ok z)
        |> assertEqual (Error 10)

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            [| 0..5 |]
            |> Result.Array.reduce (fun x y ->
                incr iterationCount
                let z = x + y
                if z > 5 then Error x
                else Ok z)
            |> assertEqual (Error 3)

            !iterationCount |> assertEqual 3

    [<Test>]
    let ``reduce raises exn for empty array`` () : unit =
        Assert.Throws<System.ArgumentException>(fun () ->
            Array.empty
            |> Result.Array.reduce (fun _ _ -> Error "Error!")
            |> ignore) |> ignore


/// Tests for the ExtCore.Control.Collections.Result.List module.
module List =
    [<Test>]
    let iter () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let iteri () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let iter2 () : unit =
        // Test case for an empty list.
        do
            let iterationCount = ref 0

            (List.empty, List.empty)
            ||> Result.List.iter2 (fun _ _ ->
                incr iterationCount
                Error "Error")
            |> assertEqual (Ok ())

            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let iterationCount = ref 0

            ([0..4], [1; 1; 2; 3; 5])
            ||> Result.List.iter2 (fun nat fibo ->
                incr iterationCount
                let x = nat + fibo
                if x >= 10 then Error x
                else Ok ())
            |> assertEqual (Ok ())

            !iterationCount |> assertEqual 5

        do
            let iterationCount = ref 0

            ([0..5], [1; 1; 2; 3; 5; 8])
            ||> Result.List.iter2 (fun nat fibo ->
                incr iterationCount
                let x = nat + fibo
                if x >= 10 then Error x
                else Ok ())
            |> assertEqual (Error 13)

            !iterationCount |> assertEqual 6

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            ([0..5], [1; 1; 2; 3; 5; 8])
            ||> Result.List.iter2 (fun nat fibo ->
                incr iterationCount
                let x = nat + fibo
                if x >= 2 then Error x
                else Ok ())
            |> assertEqual (Error 2)

            !iterationCount |> assertEqual 2

    [<Test>]
    let ``iter2 raises exn when lists have different lengths and function always returns result`` () : unit =
        // NOTE : Result.List.iter2 only raises the exception iff the lists have different lengths
        //        and the action function does not return an error (Error) before the length
        //        imbalance is discovered (because the lists are traversed as needed, not up-front).
        assertRaises<System.ArgumentException> <| fun () ->
            ([0..4], [0..7])
            ||> Result.List.iter2 (fun _ _ -> Ok ())
            |> ignore

    [<Test>]
    let ``iter2 doesn't raise exn when lists have different lengths and function returns error`` () : unit =
        // This should _not_ raise an exception, because an error value (Error) is returned
        // before the difference in the list lengths is discovered.
        ([0..4], [0..7])
        ||> Result.List.iter2 (fun _ _ -> Error "Error!")
        |> ignore
        Assert.Pass ()

    [<Test>]
    let iteri2 () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let map () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let mapi () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let map2 () : unit =
        // Test case for an empty list.
        (List.empty, List.empty)
        ||> Result.List.map2 (fun _ _ -> Error "Error!")
        |> assertEqual (Ok List.empty)

        // Sample usage test cases.
        ([0..4], [1; 1; 2; 3; 5])
        ||> Result.List.map2 (fun nat fibo ->
            let x = nat + fibo
            if x >= 10 then Error x
            else Ok x)
        |> assertEqual (Ok [1; 2; 4; 6; 9])

        ([0..5], [1; 1; 2; 3; 5; 8])
        ||> Result.List.map2 (fun nat fibo ->
            let x = nat + fibo
            if x >= 10 then Error x
            else Ok x)
        |> assertEqual (Error 13)

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            ([0..5], [1; 1; 2; 3; 5; 8])
            ||> Result.List.map2 (fun nat fibo ->
                incr iterationCount
                let x = nat + fibo
                if x >= 2 then Error x
                else Ok x)
            |> assertEqual (Error 2)

            !iterationCount |> assertEqual 2

    [<Test>]
    let ``map2 raises exn when lists have different lengths and function always returns result`` () : unit =
        // NOTE : Result.List.map2 only raises the exception iff the lists have different lengths
        //        and the mapping function does not return an error (Error) before the length
        //        imbalance is discovered (because the lists are traversed as needed, not up-front).
        assertRaises<System.ArgumentException> <| fun () ->
            ([0..4], [0..7])
            ||> Result.List.map2 (fun _ _ -> Ok "Hello world!")
            |> ignore

    [<Test>]
    let ``map2 doesn't raise exn when lists have different lengths and function returns error`` () : unit =
        // This should _not_ raise an exception, because an error value (Error) is returned
        // before the difference in the list lengths is discovered.
        ([0..4], [0..7])
        ||> Result.List.map2 (fun _ _ -> Error "Error!")
        |> ignore
        Assert.Pass ()

    [<Test>]
    let mapi2 () : unit =
        // Test case for an empty list.
        (List.empty, List.empty)
        ||> Result.List.mapi2 (fun _ _ _ -> Error "Error!")
        |> assertEqual (Ok List.empty)

        // Sample usage test cases.
        ([1..5], [1; 1; 2; 3; 5])
        ||> Result.List.mapi2 (fun idx nat fibo ->
            let x = idx + (max nat fibo)
            if x >= 10 then Error x
            else Ok x)
        |> assertEqual (Ok [1; 3; 5; 7; 9])

        ([1..6], [1; 1; 2; 3; 5; 8])
        ||> Result.List.mapi2 (fun idx nat fibo ->
            let x = idx + (max nat fibo)
            if x >= 10 then Error x
            else Ok x)
        |> assertEqual (Error 13)

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            ([1..5], [1; 1; 2; 3; 5])
            ||> Result.List.mapi2 (fun idx nat fibo ->
                incr iterationCount
                let x = idx + (max nat fibo)
                if x >= 2 then Error x
                else Ok x)
            |> assertEqual (Error 3)

            !iterationCount |> assertEqual 2

    [<Test>]
    let ``mapi2 raises exn when lists have different lengths and function always returns result`` () : unit =
        // NOTE : Result.List.mapi2 only raises the exception iff the lists have different lengths
        //        and the mapping function does not return an error (Error) before the length
        //        imbalance is discovered (because the lists are traversed as needed, not up-front).
        assertRaises<System.ArgumentException> <| fun () ->
            ([0..4], [0..7])
            ||> Result.List.mapi2 (fun _ _ _ -> Ok "Hello world!")
            |> ignore

    [<Test>]
    let ``mapi2 doesn't raise exn when lists have different lengths and function returns error`` () : unit =
        // This should _not_ raise an exception, because an error value (Error) is returned
        // before the difference in the list lengths is discovered.
        ([0..4], [0..7])
        ||> Result.List.mapi2 (fun _ _ _ -> Error "Error!")
        |> ignore
        Assert.Pass ()

    [<Test>]
    let fold () : unit =
        // Test case for an empty list.
        ("", List.empty)
        ||> Result.List.fold (fun _ _ -> Error "Error!")
        |> assertEqual (Ok "")

        // Sample usage test cases.
        ("", [0..4])
        ||> Result.List.fold (fun str x ->
            if x <> 0 && x % 5 = 0 then Error x
            else Ok (str + ((char (int 'a' + x)).ToString ())))
        |> assertEqual (Ok "abcde")

        ("", [0..5])
        ||> Result.List.fold (fun str x ->
            if x <> 0 && x % 5 = 0 then Error x
            else Ok (str + ((char (int 'a' + x)).ToString ())))
        |> assertEqual (Error 5)

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            ("", [0..5])
            ||> Result.List.fold (fun str x ->
                incr iterationCount
                if x <> 0 && x % 2 = 0 then Error x
                else Ok (str + ((char (int 'a' + x)).ToString ())))
            |> assertEqual (Error 2)

            !iterationCount |> assertEqual 3

    [<Test>]
    let foldBack () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let fold2 () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let foldBack2 () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let reduce () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let reduceBack () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let exists () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let exists2 () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let forall () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let forall2 () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let filter () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let choose () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let tryFind () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let find () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let tryPick () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let pick () : unit =
        Assert.Ignore "Test not yet implemented."

    [<Test>]
    let partition () : unit =
        Assert.Ignore "Test not yet implemented."


/// Tests for the ExtCore.Control.Collections.Result.Seq module.
module Seq =
    [<Test>]
    let iter () : unit =
        // Test case for an empty sequence.
        do
            let iterationCount = ref 0

            Seq.empty
            |> Result.Seq.iter (fun _ ->
                incr iterationCount
                Error "Error!")
            |> assertEqual (Ok ())

            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let iterationCount = ref 0

            [| 0..4 |]
            |> Seq.ofArray
            |> Result.Seq.iter (fun x ->
                incr iterationCount
                if x > 0 && x % 5 = 0 then Error x
                else Ok ())
            |> assertEqual (Ok ())

            !iterationCount |> assertEqual 5

        do
            let iterationCount = ref 0

            [| 0..5 |]
            |> Seq.ofArray
            |> Result.Seq.iter (fun x ->
                incr iterationCount
                if x > 0 && x % 5 = 0 then Error x
                else Ok ())
            |> assertEqual (Error 5)

            !iterationCount |> assertEqual 6

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            [| 0..5 |]
            |> Seq.ofArray
            |> Result.Seq.iter (fun x ->
                incr iterationCount
                if x > 0 && x % 2 = 0 then Error x
                else Ok ())
            |> assertEqual (Error 2)

            !iterationCount |> assertEqual 3

    [<Test>]
    let exists () =
        let getTestSequence failAtTheEnd =
            seq {
                yield! [ 1..10 ]
                if failAtTheEnd then failwith "should not be reached"
            }

        let createPredicate errorAt positiveAt =
            function
            | x when x = errorAt -> Error ()
            | x when x = positiveAt -> Ok true
            | _ -> Ok false

        do
            getTestSequence true
            |> Result.Seq.exists (createPredicate 100 3)
            |> assertEqual (Ok true)

        do
            getTestSequence true
            |> Result.Seq.exists (createPredicate 2 5)
            |> assertEqual (Error ())

        do
            getTestSequence false
            |> Result.Seq.exists (createPredicate 100 100)
            |> assertEqual (Ok false)


/// Tests for the ExtCore.Control.Collections.Result.Set module.
module Set =
    [<Test>]
    let fold () : unit =
        // Test case for an empty set.
        ("", Set.empty)
        ||> Result.Set.fold (fun _ _ -> Error "Error!")
        |> assertEqual (Ok "")

        // Sample usage test cases.
        ("", set [| 0..4 |])
        ||> Result.Set.fold (fun str x ->
            if x <> 0 && x % 5 = 0 then Error str
            else Ok (str + ((char (int 'a' + x)).ToString ())))
        |> assertEqual (Ok "abcde")

        ("", set [| 0..5 |])
        ||> Result.Set.fold (fun str x ->
            if x <> 0 && x % 5 = 0 then Error str
            else Ok (str + ((char (int 'a' + x)).ToString ())))
        |> assertEqual (Error "abcde")

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            ("", set [| 0..5 |])
            ||> Result.Set.fold (fun str x ->
                incr iterationCount
                if x <> 0 && x % 2 = 0 then Error str
                else Ok (str + ((char (int 'a' + x)).ToString ())))
            |> assertEqual (Error "ab")

            !iterationCount |> assertEqual 3

    [<Test>]
    let mapToArray () : unit =
        // Test case for an empty set.
        Set.empty
        |> Result.Set.mapToArray (fun _ -> Error "Error!")
        |> assertEqual (Ok Array.empty)

        // Sample usage test cases.
        set [| 0..4 |]
        |> Result.Set.mapToArray (fun x ->
            if x <> 0 && x % 5 = 0 then Error x
            else Ok ((char (int 'a' + x)).ToString ()))
        |> assertEqual (Ok [| "a"; "b"; "c"; "d"; "e"; |])

        set [| 0..5 |]
        |> Result.Set.mapToArray (fun x ->
            if x <> 0 && x % 5 = 0 then Error x
            else Ok ((char (int 'a' + x)).ToString ()))
        |> assertEqual (Error 5)

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            set [| 0..5 |]
            |> Result.Set.mapToArray (fun x ->
                incr iterationCount
                if x <> 0 && x % 2 = 0 then Error x
                else Ok ((char (int 'a' + x)).ToString ()))
            |> assertEqual (Error 2)

            !iterationCount |> assertEqual 3


/// Tests for the ExtCore.Control.Collections.Result.ArrayView module.
module ArrayView =
    [<Test>]
    let fold () : unit =
        // Test case for an empty ArrayView.
        ("", ArrayView.create [| 0..4 |] 0 0)
        ||> Result.ArrayView.fold (fun _ _ -> Error "Error!")
        |> assertEqual (Ok "")

        // Sample usage test cases.
        ("", ArrayView.create [| -2..8 |] 2 5)
        ||> Result.ArrayView.fold (fun str x ->
            if x <> 0 && x % 5 = 0 then Error str
            else Ok (str + ((char (int 'a' + x)).ToString ())))
        |> assertEqual (Ok "abcde")

        ("", ArrayView.create [| -2..8 |] 2 6)
        ||> Result.ArrayView.fold (fun str x ->
            if x <> 0 && x % 5 = 0 then Error str
            else Ok (str + ((char (int 'a' + x)).ToString ())))
        |> assertEqual (Error "abcde")

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            ("", ArrayView.create [| -2..8 |] 2 5)
            ||> Result.ArrayView.fold (fun str x ->
                incr iterationCount
                if x <> 0 && x % 2 = 0 then Error str
                else Ok (str + ((char (int 'a' + x)).ToString ())))
            |> assertEqual (Error "ab")

            !iterationCount |> assertEqual 3

