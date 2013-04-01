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

/// Unit tests for the ExtCore.Collections.BitOps module (used by IntSet and IntMap).
namespace ExtCore.Collections.BitOps.Tests

open NUnit.Framework
open FsUnit


(* TODO : Implement exhaustive and randomized tests for the other functions in BitOps. *)
// zeroBit
// mask
// maskPrefix

//
///// Little-endian operations.
//module LE =
//    [<TestCase(Category="Exhaustive",
//        Description="Tests the 'leastSignificantBit' function against all possible input values.")>]
//    let ``Least Significant Set Bit (Exhaustive)`` () : unit =
//        Assert.Fail ()
//
//    [<TestCase(Category="Randomized",
//        Description="Tests the 'leastSignificantBit' function against some randomly-generated input values.")>]
//    let ``Least Significant Set Bit (Randomized)`` () : unit =
//        Assert.Fail ()
//
//    [<TestCase(Category="Exhaustive",
//        Description="Tests the 'branchingBit' function against all possible input values.")>]
//    let ``Branching Bit (Exhaustive)`` () : unit =
//        Assert.Fail ()
//
//    [<TestCase(Category="Randomized",
//        Description="Tests the 'branchingBit' function against some randomly-generated input values.")>]
//    let ``Branching Bit (Randomized)`` () : unit =
//        Assert.Fail ()
//
//
///// Big-endian operations.
//module BE =
//    [<TestCase(Category="Exhaustive",
//        Description="Tests the 'mostSignificantBit' function against all possible input values.")>]
//    let ``Most Significant Set Bit (Exhaustive)`` () : unit =
//        Assert.Fail ()
//
//    [<TestCase(Category="Randomized",
//        Description="Tests the 'mostSignificantBit' function against some randomly-generated input values.")>]
//    let ``Most Significant Set Bit (Randomized)`` () : unit =
//        Assert.Fail ()
//
//    [<TestCase(Category="Exhaustive",
//        Description="Tests the 'branchingBit' function against all possible input values.")>]
//    let ``Branching Bit (Exhaustive)`` () : unit =
//        Assert.Fail ()
//
//    [<TestCase(Category="Randomized",
//        Description="Tests the 'branchingBit' function against some randomly-generated input values.")>]
//    let ``Branching Bit (Randomized)`` () : unit =
//        Assert.Fail ()

