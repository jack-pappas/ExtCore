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

namespace ExtCore.Control.Agents

open System
open System.Collections.Generic
open System.Threading
open ExtCore


/// Type alias for F# mailbox processor type
type Agent<'T> = MailboxProcessor<'T>

/// Wrapper for the standard F# agent (MailboxProcessor) that
/// supports stopping of the agent's body using the IDisposable
/// interface (the type automatically creates a cancellation token)
[<Sealed>]
type AutoCancelAgent<'T> private (mbox : Agent<'T>, cts : CancellationTokenSource) = 
    /// Start a new disposable agent using the specified body function
    /// (the method creates a new cancellation token for the agent)
    static member Start body =
        let cts = new CancellationTokenSource ()
        new AutoCancelAgent<'T>(Agent<'T>.Start(body, cancellationToken = cts.Token), cts)
  
    /// Returns the number of unprocessed messages in the message queue of the agent.
    member __.CurrentQueueLength =
        mbox.CurrentQueueLength

    /// Occurs when the execution of the agent results in an exception.
    [<CLIEvent>]
    member __.Error =
        mbox.Error

    /// Waits for a message. This will consume the first message in arrival order.
    member __.Receive (?timeout) =
        mbox.Receive (?timeout = timeout)

    /// Scans for a message by looking through messages in arrival order until <c>scanner</c> 
    /// returns a Some value. Other messages remain in the queue.
    member __.Scan (scanner, ?timeout) =
        mbox.Scan (scanner, ?timeout = timeout)

    /// Like PostAndReply, but returns None if no reply within the timeout period.
    member __.TryPostAndReply (buildMessage, ?timeout) =
        mbox.TryPostAndReply (buildMessage, ?timeout = timeout)

    /// Waits for a message. This will consume the first message in arrival order.
    member __.TryReceive (?timeout) =
        mbox.TryReceive (?timeout = timeout)

    /// Scans for a message by looking through messages in arrival order until <c>scanner</c> 
    /// returns a Some value. Other messages remain in the queue.
    member __.TryScan (scanner, ?timeout) =
        mbox.TryScan (scanner, ?timeout = timeout)

    /// Posts a message to the message queue of the MailboxProcessor, asynchronously.
    member __.Post (message) =
        mbox.Post message

    /// Posts a message to an agent and await a reply on the channel, synchronously.
    member __.PostAndReply (buildMessage, ?timeout) =
        mbox.PostAndReply (buildMessage, ?timeout = timeout)

    /// Like PostAndAsyncReply, but returns None if no reply within the timeout period.
    member __.PostAndTryAsyncReply (buildMessage, ?timeout) =
        mbox.PostAndTryAsyncReply (buildMessage, ?timeout = timeout)

    /// Posts a message to an agent and await a reply on the channel, asynchronously.
    member __.PostAndAsyncReply (buildMessage, ?timeout) =
        mbox.PostAndAsyncReply (buildMessage, ?timeout=timeout)

    interface IDisposable with
        member __.Dispose() =
          (mbox :> IDisposable).Dispose()
          cts.Cancel()


/// Agent that implements a simple concurrent set. The agent exposes a 
/// member that adds value to the set and returns whether the value
/// was already present.
[<Sealed>]
type ConcurrentSetAgent<'T> () =
    let agent = Agent.Start <| fun agent ->
        async {
        let hashSet = System.Collections.Generic.HashSet<_> (HashIdentity.Structural)
        while true do
            let! value, (repl : AsyncReplyChannel<_>) = agent.Receive ()
            repl.Reply (hashSet.Add value)
        }

    /// Adds the specified element to the set and returns 
    /// 'false' when it was already present in the set
    member __.AsyncAdd value =
        agent.PostAndAsyncReply(fun repl -> value, repl)


/// Agent that can be used to implement batch processing. It creates groups
/// of messages (added using the Enqueue method) and emits them using the 
/// BatchProduced event. A group is produced when it reaches the maximal 
/// size or after the timeout elapses.
[<Sealed>]
type BatchProcessingAgent<'T> (bulkSize, timeout) =
    //
    let bulkEvent = Event<'T[]>()

    //
    let agent : Agent<'T> = Agent.Start <| fun agent ->
        let rec loop remainingTime messages =
            async {
            let start = DateTime.Now
            let! msg = agent.TryReceive(timeout = max 0 remainingTime)
            let elapsed = int (DateTime.Now - start).TotalMilliseconds

            match msg with
            | Some msg when List.length messages = bulkSize - 1 ->
                // OPTIMIZE : Use Array.ofList and System.Array.Reverse.
                bulkEvent.Trigger (msg :: messages |> List.rev |> Array.ofList)
                return! loop timeout []
            | Some msg ->
                return! loop (remainingTime - elapsed) (msg::messages)
            | None when List.length messages <> 0 ->
                // OPTIMIZE : Use Array.ofList and System.Array.Reverse.
                bulkEvent.Trigger (messages |> List.rev |> Array.ofList)
                return! loop timeout []
            | None ->
                return! loop timeout []
            }

        loop timeout []

    /// The event is triggered when a group of messages is collected. The
    /// group is not empty, but may not be of the specified maximal size
    /// (when the timeout elapses before enough messages is collected)
    [<CLIEvent>]
    member __.BatchProduced =
        bulkEvent.Publish

    /// Sends new message to the agent
    member __.Enqueue message =
        agent.Post message

//
type private BlockingAgentMessage<'T> =
    //
    | Add of 'T * AsyncReplyChannel<unit>
    //
    | Get of AsyncReplyChannel<'T>

/// Agent that implements an asynchronous queue with blocking put
/// and blocking get operation (this implements the producer-consumer 
/// concurrent programming pattern). The constructor takes the maximal
/// size of the buffer.
[<Sealed>]
type BlockingQueueAgent<'T> (maxLength) =
    [<VolatileField>]
    let mutable count = 0

    let agent = Agent.Start <| fun agent ->
        //
        let queue = Queue<_> ()

        let rec emptyQueue () =
            agent.Scan <| fun msg ->
                match msg with
                | Get _ -> None
                | Add (value, reply) ->
                    Some <| enqueueAndContinue (value, reply)
                

        and fullQueue () =
            agent.Scan <| fun msg ->
                match msg with
                | Add (_,_) -> None
                | Get reply ->
                    Some <| dequeueAndContinue reply

        and runningQueue () =
            async {
            let! msg = agent.Receive ()
            match msg with
            | Add (value, reply) ->
                return! enqueueAndContinue (value, reply)
            | Get reply ->
                return! dequeueAndContinue reply
            }

        and enqueueAndContinue (value, reply) =
            async {
            reply.Reply ()
            queue.Enqueue value
            count <- queue.Count
            return! chooseState ()
            }

        and dequeueAndContinue reply =
            async {
            reply.Reply <| queue.Dequeue ()
            count <- queue.Count
            return! chooseState()
            }

        and chooseState () =
            if queue.Count = 0 then
                emptyQueue ()
            elif queue.Count < maxLength then
                runningQueue ()
            else fullQueue ()

        // Start with an empty queue
        emptyQueue ()

    /// Asynchronously adds item to the queue. The operation ends when
    /// there is a place for the item. If the queue is full, the operation
    /// will block until some items are removed.
    member x.AsyncAdd (value : 'T, ?timeout) =
        agent.PostAndAsyncReply((fun ch -> Add (value, ch)), ?timeout = timeout)

    /// Asynchronously gets item from the queue. If there are no items
    /// in the queue, the operation will block unitl items are added.
    member x.AsyncGet (?timeout) =
        agent.PostAndAsyncReply (Get, ?timeout = timeout)

    /// Synchronously gets item from the queue. If there are no items
    /// in the queue, the operation will block unitl items are added.
    /// This method blocks until value is available!
    member x.Get (?timeout) =
        agent.PostAndReply (Get, ?timeout = timeout)

    /// Gets the number of elements currently waiting in the queue.
    member x.Count = count

/// Agent that implements the "sliding window" functionality. It collects
/// messages added using the Enqueue method and emits them in overlapping 
/// groups of the specified size. For example, given [1,2,3,4,5...] and a 
/// size 3, the produced groups will be [1,2,3], [2,3,4], [3,4,5], ...
[<Sealed>]
type SlidingWindowAgent<'T> (windowSize, ?cancelToken) =
    // Event used to report groups.
    let windowEvent = Event<_> ()

    // Start an agent that remembers partial windows of length 
    // smaller than the count (new agent for every observer)
    let agent = Agent<'T>.Start((fun agent ->
        // The parameter 'lists' contains partial lists and their lengths
        let rec loop lists =
            async { 
            // Receive the next value
            let! value = agent.Receive()

            // Add new empty list and then the new element to all lists.
            // Then split the lists into 'full' that should be sent
            // to the observer and 'partial' which need more elements.
            let full, partial =
                ((0, []) :: lists)
                |> List.map (fun (length, l) -> length + 1, value::l)
                |> List.partition (fun (length, l) -> length = windowSize)
              
            // Send all full lists to the observer (as arrays)
            for (_, l) in full do
                // OPTIMIZE : Use Array.ofSeq and System.Array.Reverse.
                windowEvent.Trigger (l |> Array.ofSeq |> Array.rev)

            // Continue looping with incomplete lists
            return! loop partial
            }

        // Start with an empty list of partial lists
        loop []), ?cancellationToken = cancelToken)

    /// The event is triggered when a group of messages is collected. 
    /// The size of the group is exactly 'windowSize' and the values are
    /// returned in a fresh array.
    [<CLIEvent>]
    member x.WindowProduced =
        windowEvent.Publish

    /// Sends new message to the agent.
    member x.Enqueue message =
        agent.Post message

