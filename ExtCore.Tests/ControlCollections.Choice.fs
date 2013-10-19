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
module Tests.ExtCore.Control.Collections.Choice

open ExtCore.Control
open ExtCore.Control.Collections
open NUnit.Framework


/// Tests for the ExtCore.Control.Collections.Choice.Array module.
module Array =
    [<Test>]
    let fold () : unit =
        // Test case for an empty array.
        ("", Array.empty)
        ||> Choice.Array.fold (fun _ _ -> Choice2Of2 "Error!")
        |> assertEqual (Choice1Of2 "")

        // Sample usage test cases.
        ("", [| 0..4 |])
        ||> Choice.Array.fold (fun str x ->
            if x <> 0 && x % 5 = 0 then Choice2Of2 x
            else Choice1Of2 (str + ((char (int 'a' + x)).ToString ())))
        |> assertEqual (Choice1Of2 "abcde")

        ("", [| 0..5 |])
        ||> Choice.Array.fold (fun str x ->
            if x <> 0 && x % 5 = 0 then Choice2Of2 x
            else Choice1Of2 (str + ((char (int 'a' + x)).ToString ())))
        |> assertEqual (Choice2Of2 5)

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            ("", [| 0..5 |])
            ||> Choice.Array.fold (fun str x ->
                incr iterationCount
                if x <> 0 && x % 2 = 0 then Choice2Of2 x
                else Choice1Of2 (str + ((char (int 'a' + x)).ToString ())))
            |> assertEqual (Choice2Of2 2)

            !iterationCount |> assertEqual 3

    [<Test>]
    let foldi () : unit =
        // Test case for an empty array.
        ("", Array.empty)
        ||> Choice.Array.foldi (fun _ _ _ -> Choice2Of2 "Error!")
        |> assertEqual (Choice1Of2 "")

        // Sample usage test cases.
        ("", [| 0..4 |])
        ||> Choice.Array.foldi (fun idx str x ->
            let y = x * idx
            if y <> 0 && y % 5 = 0 then Choice2Of2 y
            else Choice1Of2 (str + ((char (int 'a' + x)).ToString ())))
        |> assertEqual (Choice1Of2 "abcde")

        ("", [| 0..5 |])
        ||> Choice.Array.foldi (fun idx str x ->
            let y = x * idx
            if y <> 0 && y % 5 = 0 then Choice2Of2 y
            else Choice1Of2 (str + ((char (int 'a' + x)).ToString ())))
        |> assertEqual (Choice2Of2 25)

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            ("", [| 0..5 |])
            ||> Choice.Array.foldi (fun idx str x ->
                incr iterationCount
                let y = x * idx
                if y <> 0 && y % 2 = 0 then Choice2Of2 y
                else Choice1Of2 (str + ((char (int 'a' + x)).ToString ())))
            |> assertEqual (Choice2Of2 4)

            !iterationCount |> assertEqual 3

    [<Test>]
    let init () : unit =
        // Test case for an empty array.
        Choice.Array.init 0 <| fun _ ->
            Choice2Of2 "Error!"
        |> assertEqual (Choice1Of2 Array.empty)

        // Sample usage test cases.
        Choice.Array.init 5 <| fun x ->
            Choice1Of2 (x * 7)
        |> assertEqual
            <| Choice1Of2 [| 0; 7; 14; 21; 28; |]

        Choice.Array.init 5 <| fun x ->
            if x > 0 && x % 5 = 0 then Choice2Of2 x
            else Choice1Of2 (x * 2)
        |> assertEqual
            <| Choice1Of2 [| 0; 2; 4; 6; 8; |]

        Choice.Array.init 6 <| fun x ->
            if x > 0 && x % 5 = 0 then Choice2Of2 x
            else Choice1Of2 (x * 2)
        |> assertEqual (Choice2Of2 5)

    [<Test>]
    let iter () : unit =
        // Test case for an empty array.
        do
            let iterationCount = ref 0

            Array.empty
            |> Choice.Array.iter (fun _ ->
                incr iterationCount
                Choice2Of2 "Error!")
            |> assertEqual (Choice1Of2 ())

            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let iterationCount = ref 0

            [| 0..4 |]
            |> Choice.Array.iter (fun x ->
                incr iterationCount
                if x > 0 && x % 5 = 0 then Choice2Of2 x
                else Choice1Of2 ())
            |> assertEqual (Choice1Of2 ())

            !iterationCount |> assertEqual 5

        do
            let iterationCount = ref 0

            [| 0..5 |]
            |> Choice.Array.iter (fun x ->
                incr iterationCount
                if x > 0 && x % 5 = 0 then Choice2Of2 x
                else Choice1Of2 ())
            |> assertEqual (Choice2Of2 5)

            !iterationCount |> assertEqual 6

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            [| 0..5 |]
            |> Choice.Array.iter (fun x ->
                incr iterationCount
                if x > 0 && x % 2 = 0 then Choice2Of2 x
                else Choice1Of2 ())
            |> assertEqual (Choice2Of2 2)

            !iterationCount |> assertEqual 3

    [<Test>]
    let iteri () : unit =
        // Test case for an empty array.
        do
            let iterationCount = ref 0

            Array.empty
            |> Choice.Array.iteri (fun _ _ ->
                incr iterationCount
                Choice2Of2 "Error!")
            |> assertEqual (Choice1Of2 ())

            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let iterationCount = ref 0

            [| 0..4 |]
            |> Choice.Array.iteri (fun idx x ->
                incr iterationCount
                let y = x * idx
                if y > 0 && y % 5 = 0 then Choice2Of2 y
                else Choice1Of2 ())
            |> assertEqual (Choice1Of2 ())

            !iterationCount |> assertEqual 5

        do
            let iterationCount = ref 0

            [| 0..5 |]
            |> Choice.Array.iteri (fun idx x ->
                incr iterationCount
                let y = x * idx
                if y > 0 && y % 5 = 0 then Choice2Of2 y
                else Choice1Of2 ())
            |> assertEqual (Choice2Of2 25)

            !iterationCount |> assertEqual 6

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            [| 0..4 |]
            |> Choice.Array.iteri (fun idx x ->
                incr iterationCount
                let y = x * idx
                if y > 0 && y % 2 = 0 then Choice2Of2 y
                else Choice1Of2 ())
            |> assertEqual (Choice2Of2 4)

            !iterationCount |> assertEqual 3

    [<Test>]
    let map () : unit =
        // Test case for an empty array.
        Array.empty
        |> Choice.Array.map (fun _ -> Choice2Of2 "Error!")
        |> assertEqual (Choice1Of2 Array.empty)

        // Sample usage test cases.
        [| 0..4 |]
        |> Choice.Array.map (fun x ->
            if x > 0 && x % 5 = 0 then Choice2Of2 x
            else Choice1Of2 (x * 3))
        |> assertEqual (Choice1Of2 [| 0; 3; 6; 9; 12; |])

        [| 0..5 |]
        |> Choice.Array.map (fun x ->
            if x > 0 && x % 5 = 0 then Choice2Of2 x
            else Choice1Of2 (x * 3))
        |> assertEqual (Choice2Of2 5)

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            [| 0..4 |]
            |> Choice.Array.map (fun x ->
                incr iterationCount
                if x > 0 && x % 2 = 0 then Choice2Of2 x
                else Choice1Of2 (x * 3))
            |> assertEqual (Choice2Of2 2)

            !iterationCount |> assertEqual 3

    [<Test>]
    let mapi () : unit =
        // Test case for an empty array.
        Array.empty
        |> Choice.Array.mapi (fun _ _ -> Choice2Of2 "Error!")
        |> assertEqual (Choice1Of2 Array.empty)

        // Sample usage test cases.
        [| 0..4 |]
        |> Choice.Array.mapi (fun idx x ->
            let y = x * idx
            if y > 0 && y % 5 = 0 then Choice2Of2 y
            else Choice1Of2 (y * 3))
        |> assertEqual (Choice1Of2 [| 0; 3; 12; 27; 48; |])

        [| 0..5 |]
        |> Choice.Array.mapi (fun idx x ->
            let y = x * idx
            if y > 0 && y % 5 = 0 then Choice2Of2 y
            else Choice1Of2 (y * 3))
        |> assertEqual (Choice2Of2 25)

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            [| 0..4 |]
            |> Choice.Array.mapi (fun idx x ->
                incr iterationCount
                let y = x * idx
                if y > 0 && y % 2 = 0 then Choice2Of2 y
                else Choice1Of2 (y * 3))
            |> assertEqual (Choice2Of2 4)

            !iterationCount |> assertEqual 3

    [<Test>]
    let map2 () : unit =
        // Test case for an empty array.
        (Array.empty, Array.empty)
        ||> Choice.Array.map2 (fun _ _ -> Choice2Of2 "Error!")
        |> assertEqual (Choice1Of2 Array.empty)

        // Sample usage test cases.
        ([| 0..4 |], [| 1; 1; 2; 3; 5; |])
        ||> Choice.Array.map2 (fun nat fibo ->
            let x = nat + fibo
            if x >= 10 then Choice2Of2 nat
            else Choice1Of2 x)
        |> assertEqual (Choice1Of2 [| 1; 2; 4; 6; 9; |])

        ([| 0..5 |], [| 1; 1; 2; 3; 5; 8; |])
        ||> Choice.Array.map2 (fun nat fibo ->
            let x = nat + fibo
            if x >= 10 then Choice2Of2 nat
            else Choice1Of2 x)
        |> assertEqual (Choice2Of2 5)

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            ([| 0..5 |], [| 1; 1; 2; 3; 5; 8; |])
            ||> Choice.Array.map2 (fun nat fibo ->
                incr iterationCount
                let x = nat + fibo
                if x >= 2 then Choice2Of2 nat
                else Choice1Of2 x)
            |> assertEqual (Choice2Of2 1)

            !iterationCount |> assertEqual 2

    [<Test; ExpectedException(typeof<System.ArgumentException>)>]
    let ``map2 raises exn when arrays have different lengths`` () : unit =
        ([| 0..4 |], [| 0..7|])
        ||> Choice.Array.map2 (fun _ _ -> Choice2Of2 "Error!")
        |> ignore

    [<Test>]
    let reduce () : unit =
        // Sample usage test cases.
        [| 0..4 |]
        |> Choice.Array.reduce (fun x y ->
            let z = x + y
            if z > 10 then Choice2Of2 x
            else Choice1Of2 z)
        |> assertEqual (Choice1Of2 10)

        [| 0..5 |]
        |> Choice.Array.reduce (fun x y ->
            let z = x + y
            if z > 10 then Choice2Of2 x
            else Choice1Of2 z)
        |> assertEqual (Choice2Of2 10)

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            [| 0..5 |]
            |> Choice.Array.reduce (fun x y ->
                incr iterationCount
                let z = x + y
                if z > 5 then Choice2Of2 x
                else Choice1Of2 z)
            |> assertEqual (Choice2Of2 3)

            !iterationCount |> assertEqual 3

    [<Test; ExpectedException(typeof<System.ArgumentException>)>]
    let ``reduce raises exn for empty array`` () : unit =
        Array.empty
        |> Choice.Array.reduce (fun _ _ -> Choice2Of2 "Error!")
        |> ignore


/// Tests for the ExtCore.Control.Collections.Choice.List module.
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
            ||> Choice.List.iter2 (fun _ _ ->
                incr iterationCount
                Choice2Of2 "Error")
            |> assertEqual (Choice1Of2 ())

            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let iterationCount = ref 0

            ([0..4], [1; 1; 2; 3; 5])
            ||> Choice.List.iter2 (fun nat fibo ->
                incr iterationCount
                let x = nat + fibo
                if x >= 10 then Choice2Of2 x
                else Choice1Of2 ())
            |> assertEqual (Choice1Of2 ())

            !iterationCount |> assertEqual 5

        do
            let iterationCount = ref 0

            ([0..5], [1; 1; 2; 3; 5; 8])
            ||> Choice.List.iter2 (fun nat fibo ->
                incr iterationCount
                let x = nat + fibo
                if x >= 10 then Choice2Of2 x
                else Choice1Of2 ())
            |> assertEqual (Choice2Of2 13)

            !iterationCount |> assertEqual 6

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            ([0..5], [1; 1; 2; 3; 5; 8])
            ||> Choice.List.iter2 (fun nat fibo ->
                incr iterationCount
                let x = nat + fibo
                if x >= 2 then Choice2Of2 x
                else Choice1Of2 ())
            |> assertEqual (Choice2Of2 2)

            !iterationCount |> assertEqual 2

    [<Test; ExpectedException(typeof<System.ArgumentException>)>]
    let ``iter2 raises exn when lists have different lengths`` () : unit =
        ([0..4], [0..7])
        ||> Choice.List.iter2 (fun _ _ -> Choice2Of2 "Error")
        |> ignore

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
        ||> Choice.List.map2 (fun _ _ -> Choice2Of2 "Error!")
        |> assertEqual (Choice1Of2 List.empty)

        // Sample usage test cases.
        ([0..4], [1; 1; 2; 3; 5])
        ||> Choice.List.map2 (fun nat fibo ->
            let x = nat + fibo
            if x >= 10 then Choice2Of2 x
            else Choice1Of2 x)
        |> assertEqual (Choice1Of2 [1; 2; 4; 6; 9])

        ([0..5], [1; 1; 2; 3; 5; 8])
        ||> Choice.List.map2 (fun nat fibo ->
            let x = nat + fibo
            if x >= 10 then Choice2Of2 x
            else Choice1Of2 x)
        |> assertEqual (Choice2Of2 13)

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            ([0..5], [1; 1; 2; 3; 5; 8])
            ||> Choice.List.map2 (fun nat fibo ->
                incr iterationCount
                let x = nat + fibo
                if x >= 2 then Choice2Of2 x
                else Choice1Of2 x)
            |> assertEqual (Choice2Of2 2)

            !iterationCount |> assertEqual 2

    [<Test; ExpectedException(typeof<System.ArgumentException>)>]
    let ``map2 raises exn when lists have different lengths`` () : unit =
        ([0..4], [0..7])
        ||> Choice.List.map2 (fun _ _ -> Choice2Of2 "Error!")
        |> ignore

    [<Test>]
    let mapi2 () : unit =
        // Test case for an empty list.
        (List.empty, List.empty)
        ||> Choice.List.mapi2 (fun _ _ _ -> Choice2Of2 "Error!")
        |> assertEqual (Choice1Of2 List.empty)

        // Sample usage test cases.
        ([1..5], [1; 1; 2; 3; 5])
        ||> Choice.List.mapi2 (fun idx nat fibo ->
            let x = idx + (max nat fibo)
            if x >= 10 then Choice2Of2 x
            else Choice1Of2 x)
        |> assertEqual (Choice1Of2 [1; 3; 5; 7; 9])

        ([1..6], [1; 1; 2; 3; 5; 8])
        ||> Choice.List.mapi2 (fun idx nat fibo ->
            let x = idx + (max nat fibo)
            if x >= 10 then Choice2Of2 x
            else Choice1Of2 x)
        |> assertEqual (Choice2Of2 13)

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            ([1..5], [1; 1; 2; 3; 5])
            ||> Choice.List.mapi2 (fun idx nat fibo ->
                incr iterationCount
                let x = idx + (max nat fibo)
                if x >= 2 then Choice2Of2 x
                else Choice1Of2 x)
            |> assertEqual (Choice2Of2 3)

            !iterationCount |> assertEqual 2

    [<Test; ExpectedException(typeof<System.ArgumentException>)>]
    let ``mapi2 raises exn when lists have different lengths`` () : unit =
        ([0..4], [0..7])
        ||> Choice.List.mapi2 (fun _ _ _ -> Choice2Of2 "Error!")
        |> ignore

    [<Test>]
    let fold () : unit =
        // Test case for an empty list.
        ("", List.empty)
        ||> Choice.List.fold (fun _ _ -> Choice2Of2 "Error!")
        |> assertEqual (Choice1Of2 "")

        // Sample usage test cases.
        ("", [0..4])
        ||> Choice.List.fold (fun str x ->
            if x <> 0 && x % 5 = 0 then Choice2Of2 x
            else Choice1Of2 (str + ((char (int 'a' + x)).ToString ())))
        |> assertEqual (Choice1Of2 "abcde")

        ("", [0..5])
        ||> Choice.List.fold (fun str x ->
            if x <> 0 && x % 5 = 0 then Choice2Of2 x
            else Choice1Of2 (str + ((char (int 'a' + x)).ToString ())))
        |> assertEqual (Choice2Of2 5)

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            ("", [0..5])
            ||> Choice.List.fold (fun str x ->
                incr iterationCount
                if x <> 0 && x % 2 = 0 then Choice2Of2 x
                else Choice1Of2 (str + ((char (int 'a' + x)).ToString ())))
            |> assertEqual (Choice2Of2 2)

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


/// Tests for the ExtCore.Control.Collections.Choice.Seq module.
module Seq =
    [<Test>]
    let iter () : unit =
        // Test case for an empty sequence.
        do
            let iterationCount = ref 0

            Seq.empty
            |> Choice.Seq.iter (fun _ ->
                incr iterationCount
                Choice2Of2 "Error!")
            |> assertEqual (Choice1Of2 ())

            !iterationCount |> assertEqual 0

        // Sample usage test cases.
        do
            let iterationCount = ref 0

            [| 0..4 |]
            |> Seq.ofArray
            |> Choice.Seq.iter (fun x ->
                incr iterationCount
                if x > 0 && x % 5 = 0 then Choice2Of2 x
                else Choice1Of2 ())
            |> assertEqual (Choice1Of2 ())

            !iterationCount |> assertEqual 5

        do
            let iterationCount = ref 0

            [| 0..5 |]
            |> Seq.ofArray
            |> Choice.Seq.iter (fun x ->
                incr iterationCount
                if x > 0 && x % 5 = 0 then Choice2Of2 x
                else Choice1Of2 ())
            |> assertEqual (Choice2Of2 5)

            !iterationCount |> assertEqual 6

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            [| 0..5 |]
            |> Seq.ofArray
            |> Choice.Seq.iter (fun x ->
                incr iterationCount
                if x > 0 && x % 2 = 0 then Choice2Of2 x
                else Choice1Of2 ())
            |> assertEqual (Choice2Of2 2)

            !iterationCount |> assertEqual 3


/// Tests for the ExtCore.Control.Collections.Choice.Set module.
module Set =
    [<Test>]
    let fold () : unit =
        // Test case for an empty set.
        ("", Set.empty)
        ||> Choice.Set.fold (fun _ _ -> Choice2Of2 "Error!")
        |> assertEqual (Choice1Of2 "")

        // Sample usage test cases.
        ("", set [| 0..4 |])
        ||> Choice.Set.fold (fun str x ->
            if x <> 0 && x % 5 = 0 then Choice2Of2 str
            else Choice1Of2 (str + ((char (int 'a' + x)).ToString ())))
        |> assertEqual (Choice1Of2 "abcde")

        ("", set [| 0..5 |])
        ||> Choice.Set.fold (fun str x ->
            if x <> 0 && x % 5 = 0 then Choice2Of2 str
            else Choice1Of2 (str + ((char (int 'a' + x)).ToString ())))
        |> assertEqual (Choice2Of2 "abcde")

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            ("", set [| 0..5 |])
            ||> Choice.Set.fold (fun str x ->
                incr iterationCount
                if x <> 0 && x % 2 = 0 then Choice2Of2 str
                else Choice1Of2 (str + ((char (int 'a' + x)).ToString ())))
            |> assertEqual (Choice2Of2 "ab")

            !iterationCount |> assertEqual 3

    [<Test>]
    let mapToArray () : unit =
        // Test case for an empty set.
        Set.empty
        |> Choice.Set.mapToArray (fun _ -> Choice2Of2 "Error!")
        |> assertEqual (Choice1Of2 Array.empty)

        // Sample usage test cases.
        set [| 0..4 |]
        |> Choice.Set.mapToArray (fun x ->
            if x <> 0 && x % 5 = 0 then Choice2Of2 x
            else Choice1Of2 ((char (int 'a' + x)).ToString ()))
        |> assertEqual (Choice1Of2 [| "a"; "b"; "c"; "d"; "e"; |])

        set [| 0..5 |]
        |> Choice.Set.mapToArray (fun x ->
            if x <> 0 && x % 5 = 0 then Choice2Of2 x
            else Choice1Of2 ((char (int 'a' + x)).ToString ()))
        |> assertEqual (Choice2Of2 5)

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            set [| 0..5 |]
            |> Choice.Set.mapToArray (fun x ->
                incr iterationCount
                if x <> 0 && x % 2 = 0 then Choice2Of2 x
                else Choice1Of2 ((char (int 'a' + x)).ToString ()))
            |> assertEqual (Choice2Of2 2)

            !iterationCount |> assertEqual 3


/// Tests for the ExtCore.Control.Collections.Choice.ArrayView module.
module ArrayView =
    [<Test>]
    let fold () : unit =
        // Test case for an empty ArrayView.
        ("", ArrayView.create [| 0..4 |] 0 0)
        ||> Choice.ArrayView.fold (fun _ _ -> Choice2Of2 "Error!")
        |> assertEqual (Choice1Of2 "")

        // Sample usage test cases.
        ("", ArrayView.create [| -2..8 |] 2 5)
        ||> Choice.ArrayView.fold (fun str x ->
            if x <> 0 && x % 5 = 0 then Choice2Of2 str
            else Choice1Of2 (str + ((char (int 'a' + x)).ToString ())))
        |> assertEqual (Choice1Of2 "abcde")

        ("", ArrayView.create [| -2..8 |] 2 6)
        ||> Choice.ArrayView.fold (fun str x ->
            if x <> 0 && x % 5 = 0 then Choice2Of2 str
            else Choice1Of2 (str + ((char (int 'a' + x)).ToString ())))
        |> assertEqual (Choice2Of2 "abcde")

        // Test case for short-circuiting.
        do
            let iterationCount = ref 0

            ("", ArrayView.create [| -2..8 |] 2 5)
            ||> Choice.ArrayView.fold (fun str x ->
                incr iterationCount
                if x <> 0 && x % 2 = 0 then Choice2Of2 str
                else Choice1Of2 (str + ((char (int 'a' + x)).ToString ())))
            |> assertEqual (Choice2Of2 "ab")

            !iterationCount |> assertEqual 3

