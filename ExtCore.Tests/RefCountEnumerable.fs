(*

Copyright 2015 Jack Pappas

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

open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Threading


/// <summary>
/// An <see cref="IEnumerator&lt;T&gt;"/> instance which wraps another enumerator
/// and decrements a reference counter when disposed.
/// This class is used primarily in unit tests to track whether enumerators are disposed properly after use.
/// </summary>
[<Sealed>]
type RefCountEnumerator<'T> (enumeratorRefCount : StrongBox<int>, enumerator : IEnumerator<'T>) =
    /// If zero, indicates this instance has not been disposed;
    /// otherwise, this instance has been disposed.
    let mutable isDisposed = 0

    do
        // Increment the enumerator reference count upon creation.
        Interlocked.Increment &enumeratorRefCount.Value |> ignore

    interface System.IDisposable with
        member __.Dispose () =
            // Only perform disposal operations once, even if Dispose() is called multiple times.
            if Interlocked.CompareExchange (&isDisposed, 1, 0) = 0 then
                // Decrement the enumerator reference count.
                Interlocked.Decrement &enumeratorRefCount.Value |> ignore

                // Dispose the wrapped enumerator.
                enumerator.Dispose ()

    interface IEnumerator<'T> with
        member __.Current =
            enumerator.Current

    interface System.Collections.IEnumerator with
        member __.Current =
            upcast enumerator.Current

        member __.MoveNext () =
            enumerator.MoveNext ()

        member __.Reset () =
            enumerator.Reset ()

/// <summary>
/// Wraps another <see cref="IEnumerable&lt;T&gt;"/> to track when enumerators
/// created for the enumerable are disposed.
/// </summary>
[<Sealed>]
type RefCountEnumerable<'T> (enumerable : seq<'T>) =
    /// The number of enumerators which have been created for the enumerable
    /// but not yet disposed.
    let enumeratorRefCount = StrongBox<int> 0

    /// Returns the number of enumerator instances created for the underlying
    /// enumeration which have not yet been disposed.
    member __.EnumeratorReferenceCount =
        Volatile.Read &enumeratorRefCount.Value

    member private __.GetRefCountEnumerator () =
        /// An enumerator from the underlying enumerable.
        let underlyingEnumerator = enumerable.GetEnumerator ()

        // Wrap the enumerator in a reference-counting wrapper and return it.
        new RefCountEnumerator<'T> (enumeratorRefCount, underlyingEnumerator)

    interface IEnumerable<'T> with
        member this.GetEnumerator () =
            this.GetRefCountEnumerator () :> IEnumerator<'T>

    interface System.Collections.IEnumerable with
        member this.GetEnumerator () =
            this.GetRefCountEnumerator () :> System.Collections.IEnumerator

