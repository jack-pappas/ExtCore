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

open ExtCore
open ExtCore.Collections


let testMap1 =
    IntMap.empty
    |> IntMap.add 2 "uuu"
    |> IntMap.add 3 "abc"
    |> IntMap.add 5 "xyz"
    |> IntMap.add 7 "yyy"

let testMap1Count =
    IntMap.count testMap1

let testMap2 =
    testMap1
    |> IntMap.remove 2

let testMap2Count =
    IntMap.count testMap2


let sosoksfd = "wfokwoef".Length + 123
()

