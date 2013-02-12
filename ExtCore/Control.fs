(*

Copyright 2010-2012 TidePowerd Ltd.
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
namespace ExtCore.Control

//
// TODO : Import workflows
// TODO : Implement modules for each of the other workflows.
//


//
type State<'State, 'T> =
    'State -> 'T * 'State

//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State =
    //
    let ``return`` (value : 'T) : State<'State, 'T> =
        fun state ->
        value, state


//
type StateBuilder () = class end

//
type ReaderBuilder () = class end

//
type WriterBuilder () = class end

//
type MaybeBuilder () = class end

//
type ChoiceBuilder () = class end

//
type TransactionBuilder () = class end

//
type ContinuationBuilder () = class end

// Equivalent to the following Haskell:
// ErrorT State s a e
// The monoid of this monad is : 'State -> Choice<'T * 'State, 'Error>
type ProtectedStateBuilder () = class end

// Equivalent to the following Haskell:
// StateT s (Error e)
// The monoid of this monad is : 'State -> Choice<'T, 'Error> * 'State
type StatefulChoiceBuilder () = class end

//
type StateContinuationBuilder () = class end

//
type ProtectedStateContinuationBuilder () = class end


//
[<AutoOpen>]
module WorkflowBuilders =
    //
    let state = StateBuilder ()
    //
    let reader = ReaderBuilder ()
    //
    let writer = WriterBuilder ()
    //
    let maybe = MaybeBuilder ()
    //
    let choice = ChoiceBuilder ()
    //
    let transaction = TransactionBuilder ()
    //
    let cps = ContinuationBuilder ()
    //
    let protectedState = ProtectedStateBuilder ()
    //
    let statefulChoice = StatefulChoiceBuilder ()
    //
    let stateCps = StateContinuationBuilder ()
    //
    let protectedStateCps = ProtectedStateContinuationBuilder ()

