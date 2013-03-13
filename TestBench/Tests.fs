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

module Tests

open System
open System.Diagnostics
open ExtCore
open ExtCore.Collections


(* Test IntSet *)

let [<Literal>] elementCount = 1000000

printf "Creating %i random integers..." elementCount
let randValues =
    let rand = Random ()
    Array.init elementCount <| fun _ ->
        rand.Next ()
printfn "done."
printfn ""

let stopwatch = Stopwatch ()
printfn "Creating sets from the random values to measure performance..."
printfn "--------------------------------------------------------------"

stopwatch.Restart ()
let fsharpSet = Set.ofArray randValues
stopwatch.Stop ()
printfn "Created F# Set<int> in: %4f ms" stopwatch.Elapsed.TotalMilliseconds

GC.Collect ()

stopwatch.Restart ()
let intSet = IntSet.ofArray randValues
stopwatch.Stop ()
printfn "Created IntSet in: %4f ms" stopwatch.Elapsed.TotalMilliseconds

GC.Collect ()

