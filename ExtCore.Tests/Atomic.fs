(*

Copyright 2019 Bartosz Sypytkowski

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

namespace Tests.ExtCore

module Atomic =

    open ExtCore
    open ExtCore.Atomic.Operators
    open NUnit.Framework

    (* AtomicRef<'a> tests *)

    [<Test>]
    let ``Atomic ref returns init value`` () : unit =
        let atom = Atomic.ref "hello"
        !atom |> assertEqual "hello"
        
    [<Test>]
    let ``Atomic ref swap returns old value`` () : unit =
        let atom = Atomic.ref "hello"
        let old = atom := "world"
        old   |> assertEqual "hello"
        !atom |> assertEqual "world"
        
    [<Test>]
    let ``Atomic ref cas swaps value is comparand is equal`` () : unit =
        let value = "hello"
        let atom = Atomic.ref value
        let success = atom |> Atomic.cas value "world"
        success |> assertTrue
        !atom   |> assertEqual "world"
        
    [<Test>]
    let ``Atomic ref cas doesn't swap value is comparand is not equal`` () : unit =
        let atom = Atomic.ref "hello"
        let success = atom |> Atomic.cas "hi" "world"
        success |> assertFalse
        !atom   |> assertEqual "hello"
        
    [<Test>]
    let ``Atomic ref update replaces old value with modified one`` () : unit =
        let atom = Atomic.ref "hello"
        let old = atom |> Atomic.update (fun o -> o + o)
        old   |> assertEqual "hello"
        !atom |> assertEqual "hellohello"
        
    (* AtomicInt tests *)

    [<Test>]
    let ``Atomic int returns init value`` () : unit =
        let atom = Atomic.int 1
        !atom |> assertEqual 1
        
    [<Test>]
    let ``Atomic int swap returns old value`` () : unit =
        let atom = Atomic.int 1
        let old = atom := 2
        old   |> assertEqual 1
        !atom |> assertEqual 2
        
    [<Test>]
    let ``Atomic int cas swaps value is comparand is equal`` () : unit =
        let atom = Atomic.int 1
        let success = atom |> Atomic.cas 1 2
        success |> assertTrue
        !atom   |> assertEqual 2
        
    [<Test>]
    let ``Atomic int cas doesn't swap value is comparand is not equal`` () : unit =
        let atom = Atomic.int 1
        let success = atom |> Atomic.cas 3 2
        success |> assertFalse
        !atom   |> assertEqual 1
        
    [<Test>]
    let ``Atomic int update replaces old value with modified one`` () : unit =
        let atom = Atomic.int 1
        let old = atom |> Atomic.update (fun o -> o + o)
        old   |> assertEqual 1
        !atom |> assertEqual 2
        
    [<Test>]
    let ``Atomic int inc increments the counter value`` () : unit =
        let atom = Atomic.int 2
        let current = Atomic.inc atom
        current |> assertEqual 3
        !atom   |> assertEqual 3
        
    [<Test>]
    let ``Atomic int dec decrements the counter value`` () : unit =
        let atom = Atomic.int 2
        let current = Atomic.dec atom
        current |> assertEqual 1
        !atom   |> assertEqual 1
        
    (* AtomicInt64 tests *)

    [<Test>]
    let ``Atomic int64 returns init value`` () : unit =
        let atom = Atomic.int64 1L
        !atom |> assertEqual 1L
        
    [<Test>]
    let ``Atomic int64 swap returns old value`` () : unit =
        let atom = Atomic.int64 1L
        let old = atom := 2L
        old   |> assertEqual 1L
        !atom |> assertEqual 2L
        
    [<Test>]
    let ``Atomic int64 cas swaps value is comparand is equal`` () : unit =
        let atom = Atomic.int64 1L
        let success = atom |> Atomic.cas 1L 2L
        success |> assertTrue
        !atom   |> assertEqual 2L
        
    [<Test>]
    let ``Atomic int64 cas doesn't swap value is comparand is not equal`` () : unit =
        let atom = Atomic.int64 1L
        let success = atom |> Atomic.cas 3L 2L
        success |> assertFalse
        !atom   |> assertEqual 1L
        
    [<Test>]
    let ``Atomic int64 update replaces old value with modified one`` () : unit =
        let atom = Atomic.int64 1L
        let old = atom |> Atomic.update (fun o -> o + o)
        old   |> assertEqual 1L
        !atom |> assertEqual 2L
        
    [<Test>]
    let ``Atomic int64 inc increments the counter value`` () : unit =
        let atom = Atomic.int64 2L
        let current = Atomic.inc atom
        current |> assertEqual 3L
        !atom   |> assertEqual 3L
        
    [<Test>]
    let ``Atomic int64 dec decrements the counter value`` () : unit =
        let atom = Atomic.int64 2L
        let current = Atomic.dec atom
        current |> assertEqual 1L
        !atom   |> assertEqual 1L
        
    (* AtomicFloat tests *)

    [<Test>]
    let ``Atomic float returns init value`` () : unit =
        let atom = Atomic.float 1.0
        !atom |> assertEqual 1.0
        
    [<Test>]
    let ``Atomic float swap returns old value`` () : unit =
        let atom = Atomic.float 1.0
        let old = atom := 2.0
        old   |> assertEqual 1.0
        !atom |> assertEqual 2.0
        
    [<Test>]
    let ``Atomic float cas swaps value is comparand is equal`` () : unit =
        let atom = Atomic.float 1.0
        let success = atom |> Atomic.cas 1.0 2.0
        success |> assertTrue
        !atom   |> assertEqual 2.0
        
    [<Test>]
    let ``Atomic float cas doesn't swap value is comparand is not equal`` () : unit =
        let atom = Atomic.float 1.0
        let success = atom |> Atomic.cas 3.0 2.0
        success |> assertFalse
        !atom   |> assertEqual 1.0
        
    [<Test>]
    let ``Atomic float update replaces old value with modified one`` () : unit =
        let atom = Atomic.float 1.0
        let old = atom |> Atomic.update (fun o -> o + o)
        old   |> assertEqual 1.0
        !atom |> assertEqual 2.0
        
    (* AtomicFloat32 tests *)

    [<Test>]
    let ``Atomic float32 returns init value`` () : unit =
        let atom = Atomic.float32 1.0f
        !atom |> assertEqual 1.0f
        
    [<Test>]
    let ``Atomic float32 swap returns old value`` () : unit =
        let atom = Atomic.float32 1.0f
        let old = atom := 2.0f
        old   |> assertEqual 1.0f
        !atom |> assertEqual 2.0f
        
    [<Test>]
    let ``Atomic float32 cas swaps value is comparand is equal`` () : unit =
        let atom = Atomic.float32 1.0f
        let success = atom |> Atomic.cas 1.0f 2.0f
        success |> assertTrue
        !atom   |> assertEqual 2.0f
        
    [<Test>]
    let ``Atomic float32 cas doesn't swap value is comparand is not equal`` () : unit =
        let atom = Atomic.float32 1.0f
        let success = atom |> Atomic.cas 3.0f 2.0f
        success |> assertFalse
        !atom   |> assertEqual 1.0f
        
    [<Test>]
    let ``Atomic float32 update replaces old value with modified one`` () : unit =
        let atom = Atomic.float32 1.0f
        let old = atom |> Atomic.update (fun o -> o + o)
        old   |> assertEqual 1.0f
        !atom |> assertEqual 2.0f
        
    (* AtomicBool tests *)

    [<Test>]
    let ``Atomic bool returns init value`` () : unit =
        let atom = Atomic.bool true
        !atom |> assertEqual true
        
    [<Test>]
    let ``Atomic bool swap returns old value`` () : unit =
        let atom = Atomic.bool true
        let old = atom := false
        old   |> assertEqual true
        !atom |> assertEqual false
        
    [<Test>]
    let ``Atomic bool cas swaps value is comparand is equal`` () : unit =
        let atom = Atomic.bool true
        let success = atom |> Atomic.cas true false
        success |> assertTrue
        !atom   |> assertEqual false
        
    [<Test>]
    let ``Atomic bool cas doesn't swap value is comparand is not equal`` () : unit =
        let atom = Atomic.bool true
        let success = atom |> Atomic.cas false true
        success |> assertFalse
        !atom   |> assertEqual true
        
    [<Test>]
    let ``Atomic bool update replaces old value with modified one`` () : unit =
        let atom = Atomic.bool true
        let old = atom |> Atomic.update (not)
        old   |> assertEqual true
        !atom |> assertEqual false
