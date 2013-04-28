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

/// Unit tests for the ExtCore.Control.Collections.Async module.
module Tests.ExtCore.Control.Collections.Async

open ExtCore.Control
open ExtCore.Control.Collections
open NUnit.Framework
open FsUnit
//open FsCheck


/// Tests for the ExtCore.Control.Collections.Async.Array module.
module Array =
    [<TestCase>]
    let lift () : unit =
        // Sample usage test cases.
        [| 1; 1; 2; 3; 5; 8; 13; 21; |]
        |> Async.Array.lift
        |> Array.map Async.RunSynchronously
        |> should equal
            [| 1; 1; 2; 3; 5; 8; 13; 21; |]

    [<TestCase>]
    let init () : unit =
        // Test case for creating an empty array.
        Async.Array.init 0 <| fun x ->
            async {
            return x * 17
            }
        |> Async.RunSynchronously
        |> Array.isEmpty
        |> should be True

        // Sample usage test cases.
        Async.Array.init 4 <| fun x ->
            async {
            return x * 17
            }
        |> Async.RunSynchronously
        |> should equal
            [| 0; 17; 34; 51; |]

    [<TestCase>]
    let map () : unit =
        // Sample usage test cases.
        [| 1; 1; 2; 3; 5; 8; 13; 21; |]
        |> Async.Array.map (fun el ->
            async {
            return el * 3
            })
        |> Async.RunSynchronously
        |> should equal
            [| 3; 3; 6; 9; 15; 24; 39; 63; |]

    [<TestCase>]
    let mapi () : unit =
        // Sample usage test cases.
        [| 1; 1; 2; 3; 5; 8; 13; 21; |]
        |> Async.Array.mapi (fun idx el ->
            async {
            return idx + el
            })
        |> Async.RunSynchronously
        |> should equal
            [1; 2; 4; 6; 9; 13; 19; 28]

    [<TestCase>]
    let map2 () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let mapi2 () : unit =
        Assert.Ignore "Test not yet implemented."

    [<TestCase>]
    let mapPartition () : unit =
        // Sample usage test cases.
        do
            let left, right =
                [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]
                |> Async.Array.mapPartition (fun colorName ->
                    async {
                    let len = String.length colorName
                    if len % 2 = 0 then
                        return Choice1Of2 len
                    else
                        return Choice2Of2 <| colorName.ToLower ()
                    })
                |> Async.RunSynchronously

            left
            |> should equal [| 6; 6; 4; 6; |]

            right
            |> should equal [| "red"; "green" |]

    [<TestCase>]
    let fold () : unit =
        // Sample usage test cases.
        (String.empty, [| 2; 17; 4; 12; |])
        ||> Async.Array.fold (fun state x ->
            async {
            return state + x.ToString()
            })
        |> Async.RunSynchronously
        |> should equal "217412"

    [<TestCase>]
    let foldBack () : unit =
        // Sample usage test cases.
        ([| 2; 17; 4; 12; |], String.empty)
        ||> Async.Array.foldBack (fun x state ->
            async {
            return state + x.ToString()
            })
        |> Async.RunSynchronously
        |> should equal "124172"

    [<TestCase>]
    let foldi () : unit =
        // Sample usage test cases.
        (String.empty, [| 2; 17; 4; 12; |])
        ||> Async.Array.foldi (fun state idx x ->
            async {
            return state + (x + idx).ToString()
            })
        |> Async.RunSynchronously
        |> should equal "218615"

    [<TestCase>]
    let foldiBack () : unit =
        // Sample usage test cases.
        ([| 2; 17; 4; 12; |], String.empty)
        ||> Async.Array.foldiBack (fun idx x state ->
            async {
            return state + (x + idx).ToString()
            })
        |> Async.RunSynchronously
        |> should equal "156182"

    [<TestCase>]
    let iter () : unit =
        do
            // Test case for an empty array.
            let elements = ResizeArray ()

            Array.empty
            |> Async.Array.iter (fun x ->
                async {
                do elements.Add (x * 3)
                })
            |> Async.RunSynchronously

            ResizeArray.isEmpty elements
            |> should be True

        do
            // Sample usage test case.
            let elements = ResizeArray ()

            [| 2; 17; 4; 12; |]
            |> Async.Array.iter (fun x ->
                async {
                do elements.Add (x * 3)
                })
            |> Async.RunSynchronously

            ResizeArray.toArray elements
            |> should equal
                [| 6; 51; 12; 36; |]

        do
            // Sample usage test case.
            let elements = ResizeArray ()

            [| 11; 23; 47; |]
            |> Async.Array.iter (fun x ->
                async {
                do elements.Add (x * 3)
                })
            |> Async.RunSynchronously

            ResizeArray.toArray elements
            |> should equal
                [| 33; 69; 141; |]

        do
            // Sample usage test case.
            let elements = ResizeArray ()

            [| 7; 11; 18; |]
            |> Async.Array.iter (fun x ->
                async {
                do elements.Add (x * 3)
                })
            |> Async.RunSynchronously

            ResizeArray.toArray elements
            |> should equal
                [| 21; 33; 54; |]

    [<TestCase>]
    let iteri () : unit =
        do
            // Test case for an empty ArrayView.
            let elements = ResizeArray ()

            Array.empty
            |> Async.Array.iteri (fun idx x ->
                async {
                do elements.Add ((x + idx) * 3)
                })
            |> Async.RunSynchronously

            ResizeArray.isEmpty elements
            |> should be True

        do
            // Sample usage test case.
            let elements = ResizeArray ()

            [| 2; 17; 4; 12; |]
            |> Async.Array.iteri (fun idx x ->
                async {
                do elements.Add ((x + idx) * 3)
                })
            |> Async.RunSynchronously

            ResizeArray.toArray elements
            |> should equal
                [| 6; 54; 18; 45; |]

        do
            // Sample usage test case.
            let elements = ResizeArray ()

            [| 11; 23; 47; |]
            |> Async.Array.iteri (fun idx x ->
                async {
                do elements.Add ((x + idx) * 3)
                })
            |> Async.RunSynchronously

            ResizeArray.toArray elements
            |> should equal
                [| 33; 72; 147; |]

        do
            // Sample usage test case.
            let elements = ResizeArray ()

            [| 7; 11; 18; |]
            |> Async.Array.iteri (fun idx x ->
                async {
                do elements.Add ((x + idx) * 3)
                })
            |> Async.RunSynchronously

            ResizeArray.toArray elements
            |> should equal
                [| 21; 36; 60; |]

    [<TestCase>]
    let exists () : unit =
        // Test case for an empty array.
        Array.empty
        |> Async.Array.exists (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should be False

        // Sample usage test cases.
        [| 2; 17; 4; 12; |]
        |> Async.Array.exists (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should be True

        [| 11; 23; 47; |]
        |> Async.Array.exists (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should be False

        // Test case for multiple matching values.
        [| 7; 11; 18; 29; 48; |]
        |> Async.Array.exists (fun x ->
            async {
            return x % 2 = 0 && x % 3 = 0
            })
        |> Async.RunSynchronously
        |> should be True

    [<TestCase>]
    let find () : unit =
        // Sample usage test cases.
        [| 2; 17; 4; 12; |]
        |> Async.Array.find (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should equal 12

        // Test case for multiple matching values.
        [| 7; 11; 18; 29; 48; |]
        |> Async.Array.find (fun x ->
            async {
            return x % 2 = 0 && x % 3 = 0
            })
        |> Async.RunSynchronously
        |> should equal 18

    [<TestCase>]
    let findIndex () : unit =
        // Sample usage test cases.
        [| 2; 17; 4; 12; |]
        |> Async.Array.findIndex (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should equal 3

        // Test case for multiple matching values.
        [| 7; 11; 18; 29; 48; |]
        |> Async.Array.findIndex (fun x ->
            async {
            return x % 2 = 0 && x % 3 = 0
            })
        |> Async.RunSynchronously
        |> should equal 2

    [<TestCase>]
    let forall () : unit =
        // Test case for an empty array.
        Array.empty
        |> Async.Array.forall (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should be True

        // Test case for multiple matching values.
        [| 2; 16; 4; 12; |]
        |> Async.Array.forall (fun x ->
            async {
            return x % 2 = 0
            })
        |> Async.RunSynchronously
        |> should be True

        // Sample usage test cases.
        [| 11; 23; 47; |]
        |> Async.Array.forall (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should be False
    
        [| 7; 11; 18; 29; 48; |]
        |> Async.Array.forall (fun x ->
            async {
            return x % 2 = 0 && x % 3 = 0
            })
        |> Async.RunSynchronously
        |> should be False

    [<TestCase>]
    let pick () : unit =
        // Sample usage test cases.
        [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]
        |> Async.Array.pick (fun colorName ->
            async {
            if colorName.StartsWith "G" then
                return Some (String.length colorName)
            else return None
            })
        |> Async.RunSynchronously
        |> should equal 5

    [<TestCase>]
    let reduce () : unit =
        // Sample usage test cases.
        [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]
        |> Async.Array.reduce (fun x y ->
            async {
            if String.length x % 2 = 0 then
                return x + y
            else
                return y + x
            })
        |> Async.RunSynchronously
        |> should equal "GreenYellowOrangeRedBlueViolet"

    [<TestCase>]
    let reduceBack () : unit =
        // Sample usage test cases.
        [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]
        |> Async.Array.reduceBack (fun x y ->
            async {
            if String.length x % 2 = 0 then
                return x + y
            else
                return y + x
            })
        |> Async.RunSynchronously
        |> should equal "OrangeYellowBlueVioletGreenRed"

    [<TestCase>]
    let tryFind () : unit =
        // Test case for an empty array.
        Array.empty
        |> Async.Array.tryFind (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should equal None

        // Sample usage test cases.
        [| 2; 17; 4; 12; |]
        |> Async.Array.tryFind (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should equal (Some 12)

        [| 11; 23; 47; |]
        |> Async.Array.tryFind (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should equal None

        // Test case for multiple matching values.
        [| 7; 11; 18; 29; 48; |]
        |> Async.Array.tryFind (fun x ->
            async {
            return x % 2 = 0 && x % 3 = 0
            })
        |> Async.RunSynchronously
        |> should equal (Some 18)

    [<TestCase>]
    let tryFindIndex () : unit =
        // Test case for an empty array.
        Array.empty
        |> Async.Array.tryFindIndex (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should equal None

        // Sample usage test cases.
        [| 2; 17; 4; 12; |]
        |> Async.Array.tryFindIndex (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should equal (Some 3)

        [| 23; 47; 106; |]
        |> Async.Array.tryFindIndex (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should equal None

        // Test case for multiple matching values.
        [| 7; 11; 18; 29; 48; |]
        |> Async.Array.tryFindIndex (fun x ->
            async {
            return x % 2 = 0 && x % 3 = 0
            })
        |> Async.RunSynchronously
        |> should equal (Some 2)

    [<TestCase>]
    let tryPick () : unit =
        [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]
        |> Async.Array.tryPick (fun colorName ->
            async {
            if colorName.StartsWith "T" then
                return Some (String.length colorName)
            else return None
            })
        |> Async.RunSynchronously
        |> should equal None

        [| "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet" |]
        |> Async.Array.tryPick (fun colorName ->
            async {
            if colorName.StartsWith "G" then
                return Some (String.length colorName)
            else return None
            })
        |> Async.RunSynchronously
        |> should equal (Some 5)


/// Tests for the ExtCore.Control.Collections.Async.List module.
module List =
    [<TestCase>]
    let lift () : unit =
        // Sample usage test cases.
        [1; 1; 2; 3; 5; 8; 13; 21]
        |> Async.List.lift
        |> List.map Async.RunSynchronously
        |> should equal
            [1; 1; 2; 3; 5; 8; 13; 21]

    [<TestCase>]
    let map () : unit =
        // Sample usage test cases.
        [1; 1; 2; 3; 5; 8; 13; 21]
        |> Async.List.map (fun el ->
            async {
            return el * 3 })
        |> Async.RunSynchronously
        |> should equal
            [3; 3; 6; 9; 15; 24; 39; 63]

    [<TestCase>]
    let mapi () : unit =
        // Sample usage test cases.
        [1; 1; 2; 3; 5; 8; 13; 21]
        |> Async.List.mapi (fun idx el ->
            async {
            return idx + el })
        |> Async.RunSynchronously
        |> should equal
            [1; 2; 4; 6; 9; 13; 19; 28]

    [<TestCase>]
    let fold () : unit =
        // Sample usage test cases.
        (String.empty, [2; 17; 4; 12])
        ||> Async.List.fold (fun state x ->
            async {
            return state + x.ToString() })
        |> Async.RunSynchronously
        |> should equal "217412"

    [<TestCase>]
    let foldBack () : unit =
        // Sample usage test cases.
        ([2; 17; 4; 12], String.empty)
        ||> Async.List.foldBack (fun x state ->
            async {
            return state + x.ToString() })
        |> Async.RunSynchronously
        |> should equal "124172"

    [<TestCase>]
    let choose () : unit =
        // Test case for an empty list.
        List.empty
        |> Async.List.choose (fun x ->
            async {
            if x % 3 = 0 && x % 4 = 0 then
                return Some <| x.ToString ()
            else
                return None
            })
        |> Async.RunSynchronously
        |> List.isEmpty
        |> should be True

        // Sample usage test cases.
        [2; 17; 4; 12]
        |> Async.List.choose (fun x ->
            async {
            if x % 3 = 0 && x % 4 = 0 then
                return Some <| x.ToString ()
            else
                return None
            })
        |> Async.RunSynchronously
        |> should equal ["12"]

        [11; 23; 47]
        |> Async.List.choose (fun x ->
            async {
            if x % 3 = 0 && x % 4 = 0 then
                return Some <| x.ToString ()
            else
                return None
            })
        |> Async.RunSynchronously
        |> List.isEmpty
        |> should be True

        // Test case for multiple matching values.
        [7; 11; 18; 29; 48]
        |> Async.List.choose (fun x ->
            async {
            if x % 2 = 0 && x % 3 = 0 then
                return Some <| x.ToString ()
            else
                return None
            })
        |> Async.RunSynchronously
        |> should equal ["18"; "48"]

    [<TestCase>]
    let collect () : unit =
        // Sample usage test cases.
        [0; 1; 1; 2; 3; 5; 8]
        |> Async.List.collect (fun el ->
            async {
            return [el; el + 1; el * 2]
            })
        |> Async.RunSynchronously
        |> should equal
            [0; 1; 0; 1; 2; 2; 1; 2; 2; 2; 3; 4; 3; 4; 6; 5; 6; 10; 8; 9; 16]

    [<TestCase>]
    let exists () : unit =
        // Test case for an empty list.
        List.empty
        |> Async.List.exists (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should be False

        // Sample usage test cases.
        [2; 17; 4; 12]
        |> Async.List.exists (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should be True

        [11; 23; 47]
        |> Async.List.exists (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should be False

        // Test case for multiple matching values.
        [7; 11; 18; 29; 48]
        |> Async.List.exists (fun x ->
            async {
            return x % 2 = 0 && x % 3 = 0
            })
        |> Async.RunSynchronously
        |> should be True

    [<TestCase>]
    let forall () : unit =
        // Test case for an empty list.
        List.empty
        |> Async.List.forall (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should be True

        // Test case for multiple matching values.
        [2; 16; 4; 12]
        |> Async.List.forall (fun x ->
            async {
            return x % 2 = 0
            })
        |> Async.RunSynchronously
        |> should be True

        // Sample usage test cases.
        [11; 23; 47]
        |> Async.List.forall (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should be False
    
        [7; 11; 18; 29; 48]
        |> Async.List.forall (fun x ->
            async {
            return x % 2 = 0 && x % 3 = 0
            })
        |> Async.RunSynchronously
        |> should be False

    [<TestCase>]
    let filter () : unit =
        // Test case for an empty list.
        List.empty
        |> Async.List.filter (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> List.isEmpty
        |> should be True

        // Sample usage test cases.
        [2; 17; 4; 12]
        |> Async.List.filter (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should equal [12]

        [11; 23; 47]
        |> Async.List.filter (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> List.isEmpty
        |> should be True

        // Test case for multiple matching values.
        [7; 11; 18; 29; 48]
        |> Async.List.filter (fun x ->
            async {
            return x % 2 = 0 && x % 3 = 0
            })
        |> Async.RunSynchronously
        |> should equal [18; 48]

    [<TestCase>]
    let tryFind () : unit =
        // Test case for an empty list.
        List.empty
        |> Async.List.tryFind (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should equal None

        // Sample usage test cases.
        [2; 17; 4; 12]
        |> Async.List.tryFind (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should equal (Some 12)

        [11; 23; 47]
        |> Async.List.tryFind (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should equal None

        // Test case for multiple matching values.
        [7; 11; 18; 29; 48]
        |> Async.List.tryFind (fun x ->
            async {
            return x % 2 = 0 && x % 3 = 0
            })
        |> Async.RunSynchronously
        |> should equal (Some 18)

    [<TestCase>]
    let find () : unit =
        // Sample usage test cases.
        [2; 17; 4; 12]
        |> Async.List.find (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should equal 12

        // Test case for multiple matching values.
        [7; 11; 18; 29; 48]
        |> Async.List.find (fun x ->
            async {
            return x % 2 = 0 && x % 3 = 0
            })
        |> Async.RunSynchronously
        |> should equal 18

    [<TestCase>]
    let tryFindIndex () : unit =
        // Test case for an empty ArrayView.
        List.empty
        |> Async.List.tryFindIndex (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should equal None

        // Sample usage test cases.
        [2; 17; 4; 12]
        |> Async.List.tryFindIndex (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should equal (Some 3)

        [23; 47; 106]
        |> Async.List.tryFindIndex (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should equal None

        // Test case for multiple matching values.
        [7; 11; 18; 29; 48]
        |> Async.List.tryFindIndex (fun x ->
            async {
            return x % 2 = 0 && x % 3 = 0
            })
        |> Async.RunSynchronously
        |> should equal (Some 2)

    [<TestCase>]
    let findIndex () : unit =
        // Sample usage test cases.
        [2; 17; 4; 12]
        |> Async.List.findIndex (fun x ->
            async {
            return x % 3 = 0 && x % 4 = 0
            })
        |> Async.RunSynchronously
        |> should equal 3

        // Test case for multiple matching values.
        [7; 11; 18; 29; 48]
        |> Async.List.findIndex (fun x ->
            async {
            return x % 2 = 0 && x % 3 = 0
            })
        |> Async.RunSynchronously
        |> should equal 2

    [<TestCase>]
    let init () : unit =
        // Test case for creating an empty list.
        Async.List.init 0 <| fun x ->
            async {
            return x * 17
            }
        |> Async.RunSynchronously
        |> List.isEmpty
        |> should be True

        // Sample usage test cases.
        Async.List.init 4 <| fun x ->
            async {
            return x * 17
            }
        |> Async.RunSynchronously
        |> should equal
            [0; 17; 34; 51]

    [<TestCase>]
    let iter () : unit =
        do
            // Test case for an empty ArrayView.
            let elements = ResizeArray ()

            List.empty
            |> Async.List.iter (fun x ->
                async {
                do elements.Add (x * 3)
                })
            |> Async.RunSynchronously

            ResizeArray.isEmpty elements
            |> should be True

        do
            // Sample usage test case.
            let elements = ResizeArray ()

            [2; 17; 4; 12]
            |> Async.List.iter (fun x ->
                async {
                do elements.Add (x * 3)
                })
            |> Async.RunSynchronously

            ResizeArray.toArray elements
            |> should equal
                [| 6; 51; 12; 36; |]

        do
            // Sample usage test case.
            let elements = ResizeArray ()

            [11; 23; 47]
            |> Async.List.iter (fun x ->
                async {
                do elements.Add (x * 3)
                })
            |> Async.RunSynchronously

            ResizeArray.toArray elements
            |> should equal
                [| 33; 69; 141; |]

        do
            // Sample usage test case.
            let elements = ResizeArray ()

            [7; 11; 18]
            |> Async.List.iter (fun x ->
                async {
                do elements.Add (x * 3)
                })
            |> Async.RunSynchronously

            ResizeArray.toArray elements
            |> should equal
                [| 21; 33; 54; |]

    [<TestCase>]
    let iteri () : unit =
        do
            // Test case for an empty ArrayView.
            let elements = ResizeArray ()

            List.empty
            |> Async.List.iteri (fun idx x ->
                async {
                do elements.Add ((x + idx) * 3)
                })
            |> Async.RunSynchronously

            ResizeArray.isEmpty elements
            |> should be True

        do
            // Sample usage test case.
            let elements = ResizeArray ()

            [2; 17; 4; 12]
            |> Async.List.iteri (fun idx x ->
                async {
                do elements.Add ((x + idx) * 3)
                })
            |> Async.RunSynchronously

            ResizeArray.toArray elements
            |> should equal
                [| 6; 54; 18; 45; |]

        do
            // Sample usage test case.
            let elements = ResizeArray ()

            [11; 23; 47]
            |> Async.List.iteri (fun idx x ->
                async {
                do elements.Add ((x + idx) * 3)
                })
            |> Async.RunSynchronously

            ResizeArray.toArray elements
            |> should equal
                [| 33; 72; 147; |]

        do
            // Sample usage test case.
            let elements = ResizeArray ()

            [7; 11; 18]
            |> Async.List.iteri (fun idx x ->
                async {
                do elements.Add ((x + idx) * 3)
                })
            |> Async.RunSynchronously

            ResizeArray.toArray elements
            |> should equal
                [| 21; 36; 60; |]


/// Tests for the ExtCore.Control.Collections.Async.Seq module.
module Seq =
    [<TestCase>]
    let lift () : unit =
        Assert.Ignore "Test not yet implemented."


    /// Tests for the ExtCore.Control.Collections.Async.Seq.Parallel module.
    module Parallel =
        [<TestCase>]
        let batch () : unit =
            Assert.Ignore "Test not yet implemented."

