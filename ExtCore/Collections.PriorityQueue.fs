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

//
namespace ExtCore.Collections

open LanguagePrimitives
open OptimizedClosures
open ExtCore

#nowarn "25"

(* TODO :   Implement modifications for global root, data bootstrapping, and other optimizations. *)
(* TODO :   Extract code for Brodal-Okasaki meldable heaps from the Coq theories here:
            https://code.google.com/p/priority-queues/ *)

// TODO : Tag this with a unit-of-measure.
type Rank = int

type Tree<'T when 'T : comparison> =
    Node of 'T * Rank * Tree<'T> list

//type Tree<'T when 'T : comparison> = {
//    //
//    Value : 'T;
//    //
//    Children : Tree<'T> list;
//    //
//    Rank : Rank;
//}

(* auxiliary functions *)

//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SkewBinomialQueue =
    type T<'T when 'T : comparison> = Tree<'T> list

    let root (Node (x,r,c)) = x
    let rank (Node (x,r,c)) = r

    let link (t1, t2) =
        match t1, t2 with
        | Node (x1, r1, c1), Node (x2, r2, c2)
            when r1 = r2 ->
            if x1 <= x2 then
                Node (x1, r1 + 1, t2 :: c1)
            else
                Node (x2, r2 + 1, t1 :: c2)
        | _ ->
            raise <| System.ArgumentException "The rank of both nodes must be the same."

    let skewLink ((Node (x0, r0, c0) as t0), (Node (x1, r1, c1) as t1), (Node (x2, r2, c2) as t2)) =
        if x1 <= x0 && x1 <= x2 then
            Node (x1, r1 + 1, t0 :: t2 :: c1)
        elif x2 <= x0 && x2 <= x1 then
            Node (x2, r2 + 1, t0 :: t1 :: c2)
        else
            Node (x0, r1 + 1, [t1; t2])

    let rec ins = function
        | t, [] -> [t]
        | t, t' :: ts ->
            // rank t <= rank t'
            if rank t < rank t' then
                t :: t' :: ts
            else
                ins (link (t, t'), ts)

    let uniqify = function
        | [] -> []
        | t :: ts ->
            // eliminate initial duplicate
            ins (t, ts)

    let rec meldUniq = function
        | [], ts
        | ts, [] ->
            ts
        | t1 :: ts1, t2 :: ts2 ->
            if rank t1 < rank t2 then
                t1 :: meldUniq (ts1, t2 :: ts2)
            elif rank t2 < rank t1 then
                t2 :: meldUniq (t1 :: ts1, ts2)
            else
                ins (link (t1, t2), meldUniq (ts1, ts2))

    let empty = []

    let isEmpty ts = List.isEmpty ts

    let insert x = function
        | t1 :: t2 :: rest as ts ->
            if rank t1 = rank t2 then
                skewLink (Node (x, 0, []), t1, t2) :: rest
            else
                Node (x, 0, []) :: ts
        | ts ->
            Node (x, 0, []) :: ts

    let meld (ts, ts') =
        meldUniq (uniqify ts, uniqify ts')

    exception EMPTY

    let rec findMin = function
        | [] ->
            raise EMPTY
        | [t] ->
            root t
        | t :: ts ->
            let x = findMin ts
            let rt = root t
            if rt <= x then rt else x

    let deleteMin = function
        | [] ->
            raise EMPTY
        | ts ->
            let rec getMin = function
                | [] ->
                    invalidArg "ts" "The tree list is empty."
                | [t] ->
                    t, []
                | t :: ts ->
                    let t', ts' = getMin ts
                    if root t <= root t' then
                        t, ts
                    else
                        t', t :: ts'

            let rec split = function
                | (ts, xs, []) ->
                    ts, xs
                | (ts, xs, t :: c) ->
                    if rank t = 0 then
                        split (ts, root t :: xs, c)
                    else
                        split (t :: ts, xs, c)

            let (Node (x,r,c), ts) = getMin ts
            let ts', xs' = split ([], [], c)
            List.foldBack insert xs' (meld (ts, ts'))


//
type RootedPriorityQueue<'T when 'T : comparison> =
    //
    | Empty
    //
    | Root of 'T * SkewBinomialQueue.T<'T>


//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RootedPriorityQueue =
    module Q = SkewBinomialQueue

    //
    [<CompiledName("Empty")>]
    [<GeneralizableValue>]
    let empty<'T when 'T : comparison> : RootedPriorityQueue<'T> =
        Empty

    //
    let isEmpty = function
        | Empty -> true
        | _ -> false

    //
    let insert y = function
        | Empty ->
            Root (y, Q.empty)
        | Root (x, q) ->
            if y <= x then
                Root (y, Q.insert x q)
            else
                Root (x, Q.insert y q)

    //
    let meld = function
        | Empty, rq
        | rq, Empty ->
            rq
        | Root (x1, q1), Root (x2, q2) ->
            if x1 <= x2 then
                Root (x1, Q.insert x2 <| Q.meld (q1, q2))
            else
                Root (x2, Q.insert x1 <| Q.meld (q1, q2))

    exception EMPTY

    let findMin = function
        | Root (x, _) -> x
        | Empty ->
            raise EMPTY

    let deleteMin = function
        | Empty ->
            raise EMPTY
        | Root (x, q) ->
            if Q.isEmpty q then Empty
            else
                Root (Q.findMin q, Q.deleteMin q)


