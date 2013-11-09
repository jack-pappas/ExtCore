(*

Copyright 2011 Tomas Petricek
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

namespace ExtCore.Control

open ExtCore
open System
open System.Threading

#nowarn "40"


/// Union type that represents different messages that can be sent to the
/// IObserver interface. The IObserver type is equivalent to a type that has
/// just OnNext method that gets 'ObservableUpdate' as an argument.
type ObservableUpdate<'T> =
    //
    | Completed
    //
    | Next of 'T
    //
    | Error of exn
    
//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Observable =
    open ExtCore.Control.Agents

    /// Returns an observable that yields sliding windows of 
    /// containing elements drawn from the input observable. 
    /// Each window is returned as a fresh array.
    [<CompiledName("Windowed")>]
    let windowed size (input : IObservable<'T>) =
        // Preconditions
        if size < 1 then
            argOutOfRange "size" "The window size must be greater than zero (0)."
        checkNonNull "input" input

        { new IObservable<'T[]> with
            member x.Subscribe observer =
                // Create sliding window agent for every call
                // and redirect batches to the observer
                let cts = new CancellationTokenSource ()
                let agent = SlidingWindowAgent<_> (size, cts.Token)
                agent.WindowProduced.Add observer.OnNext

                // Subscribe to the input and send values to the agent
                let subscription =
                    { new IObserver<'T> with
                        member __.OnNext v =
                            agent.Enqueue v
                        member __.OnCompleted () = 
                            cts.Cancel ()
                            observer.OnCompleted ()
                        member __.OnError e =
                            cts.Cancel ()
                            observer.OnError e }
                    |> input.Subscribe

                // Cancel subscription & cancel the agent
                { new IDisposable with
                    member x.Dispose () =
                        subscription.Dispose ()
                        cts.Cancel () } }

    /// Creates an observable that calls the specified function (each time)
    /// after an observer is attached to the observable. This is useful to
    /// make sure that events triggered by the function are handled.
    [<CompiledName("Guard")>]
    let guard action (input : IObservable<'Args>) =
        // Preconditions
        checkNonNull "input" input

        { new IObservable<'Args> with
            member x.Subscribe observer =
                let rm = input.Subscribe observer
                action ()
                rm }

    /// Turns observable into an observable that only calls OnNext method of the
    /// observer, but gives it a discriminated union that represents different
    /// kinds of events (error, next, completed)
    [<CompiledName("AsUpdates")>]
    let asUpdates (input : IObservable<'T>) =
        // Preconditions
        checkNonNull "input" input

        { new IObservable<_> with
            member x.Subscribe observer =
                { new IObserver<_> with
                    member x.OnNext v =
                        observer.OnNext (Next v)
                    member x.OnCompleted () =
                        observer.OnNext(Completed) 
                    member x.OnError e =
                        observer.OnNext (Error e) }
                |> input.Subscribe }


//
[<AutoOpen>]
module ObservableExtensions =
    /// Helper that can be used for writing CPS-style code that resumes
    /// on the same thread where the operation was started.
    let internal synchronize f =
        let ctx = System.Threading.SynchronizationContext.Current
        f <| fun g ->
            let nctx = System.Threading.SynchronizationContext.Current
            if ctx <> null && ctx <> nctx then
                ctx.Post ((fun _ -> g ()), null)
            else g ()

    type Microsoft.FSharp.Control.Async with
        /// Behaves like AwaitObservable, but calls the specified guarding function
        /// after a subscriber is registered with the observable.
        static member GuardedAwaitObservable (ev1 : IObservable<'T1>) guardFunction =
            // Preconditions
            checkNonNull "ev1" ev1

            synchronize <| fun f ->
                Async.FromContinuations <| fun (cont, econt, ccont) ->
                    let rec finish cont value =
                        remover.Dispose ()
                        f (fun () -> cont value)

                    and remover : IDisposable =
                        { new IObserver<_> with
                            member x.OnNext v =
                                finish cont v
                            member x.OnError e =
                                finish econt e
                            member x.OnCompleted () =
                                let msg = "Cancelling the workflow, because the Observable awaited using AwaitObservable has completed."
                                finish ccont (System.OperationCanceledException msg) }
                        |> ev1.Subscribe

                    guardFunction ()

        /// Creates an asynchronous workflow that will be resumed when the specified observables produces a value.
        /// The workflow will return the value produced by the observable.
        static member AwaitObservable (ev1 : IObservable<'T1>) =
            // Preconditions
            checkNonNull "ev1" ev1

            synchronize <| fun f ->
                Async.FromContinuations <| fun (cont, econt, ccont) ->
                    let rec finish cont value =
                        remover.Dispose ()
                        f (fun () -> cont value)

                    and remover : IDisposable =
                        { new IObserver<_> with
                            member x.OnNext v =
                                finish cont v
                            member x.OnError e =
                                finish econt e
                            member x.OnCompleted () =
                                let msg = "Cancelling the workflow, because the Observable awaited using AwaitObservable has completed."
                                finish ccont (System.OperationCanceledException msg) }
                        |> ev1.Subscribe

                    // Explicitly return the unit value here to make this an expression and satisfy the compiler.
                    ()
  
        /// Creates an asynchronous workflow that will be resumed when the first of the specified
        /// two observables produces a value. The workflow will return a Choice value that can be
        /// used to identify the observable that produced the value.
        static member AwaitObservable (ev1 : IObservable<'T1>, ev2 : IObservable<'T2>) =
            // Preconditions
            checkNonNull "ev1" ev1
            checkNonNull "ev2" ev2

            [ ev1 |> Observable.map Choice1Of2;
              ev2 |> Observable.map Choice2Of2; ]
            |> List.reduce Observable.merge
            |> Async.AwaitObservable

        /// Creates an asynchronous workflow that will be resumed when the first of the specified
        /// three observables produces a value. The workflow will return a Choice value that can be
        /// used to identify the observable that produced the value.
        static member AwaitObservable (ev1 : IObservable<'T1>, ev2 : IObservable<'T2>, ev3 : IObservable<'T3>) =
            // Preconditions
            checkNonNull "ev1" ev1
            checkNonNull "ev2" ev2
            checkNonNull "ev3" ev3

            [ ev1 |> Observable.map Choice1Of3;
              ev2 |> Observable.map Choice2Of3;
              ev3 |> Observable.map Choice3Of3; ]
            |> List.reduce Observable.merge
            |> Async.AwaitObservable

        /// Creates an asynchronous workflow that will be resumed when the first of the specified
        /// four observables produces a value. The workflow will return a Choice value that can be
        /// used to identify the observable that produced the value.
        static member AwaitObservable (ev1 : IObservable<'T1>, ev2 : IObservable<'T2>, ev3 : IObservable<'T3>, ev4 : IObservable<'T4>) =
            // Preconditions
            checkNonNull "ev1" ev1
            checkNonNull "ev2" ev2
            checkNonNull "ev3" ev3
            checkNonNull "ev4" ev4

            [ ev1 |> Observable.map Choice1Of4;
              ev2 |> Observable.map Choice2Of4;
              ev3 |> Observable.map Choice3Of4;
              ev4 |> Observable.map Choice4Of4; ]
            |> List.reduce Observable.merge
            |> Async.AwaitObservable

