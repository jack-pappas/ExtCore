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

namespace ExtCore.IO

open System
open System.IO
open ExtCore


(* TODO :   Implement PathZipper type and module. *)
(* TODO :   Implement a Stream module? Or perhaps just some type extensions? *)

//
[<RequireQualifiedAccess>]
module File =
    //
    [<RequireQualifiedAccess>]
    module Lines =
        //
        [<CompiledName("Iterate")>]
        let iter (action : string -> unit) (path : string) : unit =
            // Preconditions
            checkNonNull "path" path

            notImpl "File.Lines.iter"

        //
        [<CompiledName("Fold")>]
        let fold (folder : 'State -> string -> 'State) (state : 'State) (path : string) : 'State =
            // Preconditions
            checkNonNull "path" path

            notImpl "File.Lines.fold"

        // iteri
        // foldi
        // choose
        // choosei
        // exists
        // forall

