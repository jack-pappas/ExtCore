(*

Copyright 2014 Jack Pappas

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

namespace ExtCore.Control.Tasks

open System.Threading.Tasks
open ExtCore


/// <summary>
/// </summary>
[<Sealed>]
type TaskBuilder () =
    // 'T -> M<'T>
    member inline __.Return value
        : Task<'T> =
#if FX_ATLEAST_45
        Task.FromResult<'T> (value)
#else
        new Task<'T> (fun () -> value)
#endif

    // M<'T> -> M<'T>
    member inline __.ReturnFrom task
        : Task<'T> =
        task

    // unit -> M<'T>
    member inline __.Zero ()
        : Task<unit> =
        new Task<unit> (fun () -> ())

    // (unit -> M<'T>) -> M<'T>
    member __.Delay (taskThunk : unit -> Task<'T>)
        : Task<'T> =
        new Task<'T>(fun () ->
            // Execute the task thunk to create the task.
            let task = taskThunk ()

            // Run the delayed task synchronously.
            task.RunSynchronously ()
            task.Wait ()

            // Return the result of the task.
            task.Result)

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (task : Task<'T>, binder : 'T -> Task<'U>)
        : Task<'U> =
        task.ContinueWith<'U>(fun (task : Task<'T>) ->
            // Apply the result from 'task' to the binder function to create a new task.
            let childTask = binder task.Result

            // Run the child task synchronously, then return it's result.
            childTask.RunSynchronously ()
            childTask.Result)

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline __.Combine (task1 : Task<unit>, task2 : Task<'T>)
        : Task<'T> =
        task1.ContinueWith<'T> (fun (_ : Task<unit>) ->
            // Run 'task2' synchronously, then return it's result.
            task2.RunSynchronously ()
            task2.Result)

    // M<'T> * (exn -> M<'T>) -> M<'T>
    member inline __.TryWith (body : Task<'T>, handler : System.AggregateException -> Task<'T>)
        : Task<'T> =
        new Task<'T>(fun () ->
            try
                // Run the body task.
                body.RunSynchronously ()
                body.Wait ()
                    
                // Return the result from the body task.
                body.Result
            with :? System.AggregateException as ae ->
                // Create the handler task from the handler function.
                let handlerTask = handler ae

                // Run the handler task.
                handlerTask.RunSynchronously ()
                handlerTask.Wait ()

                // Return the result from the handler task.
                handlerTask.Result)

    // M<'T> * (unit -> unit) -> M<'T>
    member inline __.TryFinally (body : Task<'T>, handler : unit -> unit)
        : Task<'T> =
        new Task<'T>(fun () ->
            try
                // Run the body task.
                body.RunSynchronously ()
                body.Wait ()

                // Return the result from the body task.
                body.Result
            finally
                // Run the handler function.
                handler ())

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : 'T -> Task<_>)
        : Task<'U> =
        this.TryFinally (body resource, (fun () ->
            if not <| isNull (box resource) then
                resource.Dispose ()))

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : Task<_>)
        : Task<unit> =
        if guard () then
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> Task<_>)
        : Task<unit> =
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))

/// <summary>
/// </summary>
[<AutoOpen>]
module WorkflowBuilders =
    //
    [<CompiledName("Tasks")>]
    let tasks = TaskBuilder ()

