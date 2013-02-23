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

let testMap2 =
    testMap1
    |> IntMap.remove 2

let testMap3 =
    testMap1
    |> IntMap.add 2 "ggg"
    |> IntMap.add 1 "wefowo"
    |> IntMap.add 10 "wefook"
    |> IntMap.add 13 "wofeff"
    |> IntMap.add 39 "weokw"
    |> IntMap.add 31 "wmmcf"
    |> IntMap.add 94 "mkmss"
    |> IntMap.add 44 "emff"
    |> IntMap.add 55 "fmlmf"
    |> IntMap.add 42 "efmmfr"
    |> IntMap.add 40 "wefmmk"
    |> IntMap.add 34 "fokwoe"


let sosoksfd = "wfokwoef".Length + 123
()

