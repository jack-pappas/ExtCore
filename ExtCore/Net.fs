(*

Copyright 2011 Tomas Petricek

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

namespace ExtCore.Net

open System
open System.IO
open System.Net
open System.Text
open System.Threading
open ExtCore
open ExtCore.Collections
open ExtCore.IO

#if FX_ATLEAST_PORTABLE
#else

/// Extensions that simplify working with HttpListener and related types.
[<AutoOpen>]
module HttpExtensions =
    type System.Net.HttpListener with
        /// Asynchronously waits for an incoming request and returns it.
        member this.AsyncGetContext () =
            Async.FromBeginEnd (this.BeginGetContext, this.EndGetContext)

        /// Starts HttpListener on the specified URL. The 'handler' function is
        /// called (in a new thread pool thread) each time an HTTP request is received.
        static member Start (url, handler, ?cancellationToken) =
            let server =
                async {
                use listener = new HttpListener ()
                listener.Prefixes.Add url
                listener.Start ()
                while true do
                  let! context = listener.AsyncGetContext ()
                  let asyncHandler = handler (context.Request, context.Response)
                  Async.Start (asyncHandler, ?cancellationToken = cancellationToken)
                }

            Async.Start (server, ?cancellationToken = cancellationToken)

    type System.Net.HttpListenerRequest with
        /// Asynchronously reads the 'InputStream' of the request and converts it to a string
        member this.AsyncInputString =
            async {
            use tmp = new MemoryStream ()
            for data in this.InputStream.AsyncReadSeq (16 * 1024) do
                tmp.Write (data, 0, data.Length)
            tmp.Seek (0L, SeekOrigin.Begin) |> ignore
            use sr = new StreamReader (tmp)
            return sr.ReadToEnd ()
            }

    type System.Net.HttpListenerResponse with
        /// Sends the specified string as a reply in UTF-8 encoding
        member this.AsyncReply (str : string) =
            // Preconditions
            checkNonNull "str" str

            async {
            let buffer = Encoding.UTF8.GetBytes str
            this.ContentLength64 <- int64 buffer.Length
            let output = this.OutputStream
            do! output.AsyncWrite (buffer, 0, buffer.Length)
            output.Close ()
            }

        /// Sends the specified data as a reply with the specified content type
        member this.AsyncReply (contentType, buffer : byte[]) =
            // Preconditions
            //checkNonNull "contentType" contentType
            checkNonNull "buffer" buffer

            async {
            this.ContentLength64 <- int64 buffer.Length
            let output = this.OutputStream
            this.ContentType <- contentType
            do! output.AsyncWrite (buffer, 0, buffer.Length)
            output.Close ()
            }

#endif
