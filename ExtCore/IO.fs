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

namespace ExtCore.IO

open System
open System.IO
open OptimizedClosures
open ExtCore
open ExtCore.Collections


/// Functions operating on or over files.
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
            // TODO : Check file exists

            // Create a FileStream and wrap it in a StreamReader so we can read the file line-by-line.
            // FileStream is used instead of creating the StreamReader directly from the path because
            // it allows us to relax the FileAccess and FileShare settings.
            use fileStream = new FileStream (path, FileMode.Open, FileAccess.Read, FileShare.Read)
            use streamReader = new StreamReader (fileStream)

            // Iterate over the lines of the file, applying the function to each of them.
            while not streamReader.EndOfStream do
                action <| streamReader.ReadLine ()

        //
        [<CompiledName("IterateIndexed")>]
        let iteri (action : int -> string -> unit) (path : string) : unit =
            // Preconditions
            checkNonNull "path" path
            // TODO : Check file exists

            // Create a FileStream and wrap it in a StreamReader so we can read the file line-by-line.
            // FileStream is used instead of creating the StreamReader directly from the path because
            // it allows us to relax the FileAccess and FileShare settings.
            use fileStream = new FileStream (path, FileMode.Open, FileAccess.Read, FileShare.Read)
            use streamReader = new StreamReader (fileStream)

            let action = FSharpFunc<_,_,_>.Adapt action

            /// The current line index.
            let mutable lineIndex = 0

            // Iterate over the lines of the file, applying the function to each of them.
            while not streamReader.EndOfStream do
                action.Invoke (lineIndex, streamReader.ReadLine ())
                lineIndex <- lineIndex + 1

        //
        [<CompiledName("Fold")>]
        let fold (folder : 'State -> string -> 'State) (state : 'State) (path : string) : 'State =
            // Preconditions
            checkNonNull "path" path
            // TODO : Check file exists

            // Preconditions
            checkNonNull "path" path
            // TODO : Check file exists

            // Create a FileStream and wrap it in a StreamReader so we can read the file line-by-line.
            // FileStream is used instead of creating the StreamReader directly from the path because
            // it allows us to relax the FileAccess and FileShare settings.
            use fileStream = new FileStream (path, FileMode.Open, FileAccess.Read, FileShare.Read)
            use streamReader = new StreamReader (fileStream)

            let folder = FSharpFunc<_,_,_>.Adapt folder
            let mutable state = state

            // Iterate over the lines of the file, applying the function to each of them.
            while not streamReader.EndOfStream do
                state <- folder.Invoke (state, streamReader.ReadLine ())

            // Return the final state
            state

        //
        [<CompiledName("FoldIndexed")>]
        let foldi (folder : 'State -> int -> string -> 'State) (state : 'State) (path : string) : 'State =
            // Preconditions
            checkNonNull "path" path
            // TODO : Check file exists

            // Create a FileStream and wrap it in a StreamReader so we can read the file line-by-line.
            // FileStream is used instead of creating the StreamReader directly from the path because
            // it allows us to relax the FileAccess and FileShare settings.
            use fileStream = new FileStream (path, FileMode.Open, FileAccess.Read, FileShare.Read)
            use streamReader = new StreamReader (fileStream)

            let folder = FSharpFunc<_,_,_,_>.Adapt folder
            let mutable state = state
            
            /// The current line index.
            let mutable lineIndex = 0

            // Iterate over the lines of the file, applying the function to each of them.
            while not streamReader.EndOfStream do
                state <- folder.Invoke (state, lineIndex, streamReader.ReadLine ())
                lineIndex <- lineIndex + 1

            // Return the final state
            state

        //
        [<CompiledName("Filter")>]
        let filter (predicate : string -> bool) (path : string) : string[] =
            // Preconditions
            checkNonNull "path" path
            // TODO : Check file exists

            // Create a FileStream and wrap it in a StreamReader so we can read the file line-by-line.
            // FileStream is used instead of creating the StreamReader directly from the path because
            // it allows us to relax the FileAccess and FileShare settings.
            use fileStream = new FileStream (path, FileMode.Open, FileAccess.Read, FileShare.Read)
            use streamReader = new StreamReader (fileStream)

            /// The filtered lines.
            let filteredLines = ResizeArray ()

            // Iterate over the lines of the file, applying the function to each of them.
            while not streamReader.EndOfStream do
                let currentLine = streamReader.ReadLine ()
                if predicate currentLine then
                    ResizeArray.add currentLine filteredLines

            // Return the filtered lines.
            ResizeArray.toArray filteredLines

        //
        [<CompiledName("Choose")>]
        let choose (chooser : string -> 'T option) (path : string) : 'T[] =
            // Preconditions
            checkNonNull "path" path
            // TODO : Check file exists

            // Create a FileStream and wrap it in a StreamReader so we can read the file line-by-line.
            // FileStream is used instead of creating the StreamReader directly from the path because
            // it allows us to relax the FileAccess and FileShare settings.
            use fileStream = new FileStream (path, FileMode.Open, FileAccess.Read, FileShare.Read)
            use streamReader = new StreamReader (fileStream)

            /// The chosen values.
            let chosenValues = ResizeArray ()

            // Iterate over the lines of the file, applying the function to each of them.
            while not streamReader.EndOfStream do
                match chooser <| streamReader.ReadLine () with
                | None -> ()
                | Some result ->
                    ResizeArray.add result chosenValues

            // Return the chosen values.
            ResizeArray.toArray chosenValues

        //
        [<CompiledName("ChooseIndexed")>]
        let choosei (chooser : int -> string -> 'T option) (path : string) : 'T[] =
            // Preconditions
            checkNonNull "path" path
            // TODO : Check file exists

            // Create a FileStream and wrap it in a StreamReader so we can read the file line-by-line.
            // FileStream is used instead of creating the StreamReader directly from the path because
            // it allows us to relax the FileAccess and FileShare settings.
            use fileStream = new FileStream (path, FileMode.Open, FileAccess.Read, FileShare.Read)
            use streamReader = new StreamReader (fileStream)

            let chooser = FSharpFunc<_,_,_>.Adapt chooser

            /// The chosen values.
            let chosenValues = ResizeArray ()

            /// The current line index.
            let mutable lineIndex = 0

            // Iterate over the lines of the file, applying the function to each of them.
            while not streamReader.EndOfStream do
                match chooser.Invoke (lineIndex, streamReader.ReadLine ()) with
                | None -> ()
                | Some result ->
                    ResizeArray.add result chosenValues

                lineIndex <- lineIndex + 1

            // Return the chosen values.
            ResizeArray.toArray chosenValues

        //
        [<CompiledName("Exists")>]
        let exists (predicate : string -> bool) (path : string) : bool =
            // Preconditions
            checkNonNull "path" path
            // TODO : Check file exists

            // Create a FileStream and wrap it in a StreamReader so we can read the file line-by-line.
            // FileStream is used instead of creating the StreamReader directly from the path because
            // it allows us to relax the FileAccess and FileShare settings.
            use fileStream = new FileStream (path, FileMode.Open, FileAccess.Read, FileShare.Read)
            use streamReader = new StreamReader (fileStream)

            let mutable foundMatch = false

            // Iterate over the lines of the file until we find one that matches the predicate.
            while not foundMatch && not streamReader.EndOfStream do
                if predicate <| streamReader.ReadLine () then
                    foundMatch <- true

            // Return the value indicating whether a match was found.
            foundMatch

        //
        [<CompiledName("Forall")>]
        let forall (predicate : string -> bool) (path : string) : bool =
            // Preconditions
            checkNonNull "path" path
            // TODO : Check file exists

            // Create a FileStream and wrap it in a StreamReader so we can read the file line-by-line.
            // FileStream is used instead of creating the StreamReader directly from the path because
            // it allows us to relax the FileAccess and FileShare settings.
            use fileStream = new FileStream (path, FileMode.Open, FileAccess.Read, FileShare.Read)
            use streamReader = new StreamReader (fileStream)

            let mutable allMatches = true

            // Iterate over the lines of the file until we find one that does not match the predicate.
            while allMatches && not streamReader.EndOfStream do
                if not (predicate <| streamReader.ReadLine ()) then
                    allMatches <- false

            // Return the value indicating whether all lines matched the predicate.
            allMatches

        //
        [<CompiledName("TryFind")>]
        let tryFind (predicate : string -> bool) (path : string) : string option =
            // Preconditions
            checkNonNull "path" path
            // TODO : Check file exists

            // Create a FileStream and wrap it in a StreamReader so we can read the file line-by-line.
            // FileStream is used instead of creating the StreamReader directly from the path because
            // it allows us to relax the FileAccess and FileShare settings.
            use fileStream = new FileStream (path, FileMode.Open, FileAccess.Read, FileShare.Read)
            use streamReader = new StreamReader (fileStream)

            let mutable result = None

            // Iterate over the lines of the file until we find one that matches the predicate.
            while Option.isNone result && not streamReader.EndOfStream do
                let currentLine = streamReader.ReadLine ()
                if predicate currentLine then
                    result <- Some currentLine

            // Return the result.
            result

        //
        [<CompiledName("Find")>]
        let find (predicate : string -> bool) (path : string) : string =
            // Preconditions
            checkNonNull "path" path
            // TODO : Check file exists

            // Call tryFind; raise an exception if no matching line was found.
            match tryFind predicate path with
            | Some str -> str
            | None ->
                // TODO : Provide a better exception message.
                //keyNotFound ""
                raise <| System.Collections.Generic.KeyNotFoundException ()

        //
        [<CompiledName("TryPick")>]
        let tryPick (picker : string -> 'T option) (path : string) : 'T option =
            // Preconditions
            checkNonNull "path" path
            // TODO : Check file exists

            // Create a FileStream and wrap it in a StreamReader so we can read the file line-by-line.
            // FileStream is used instead of creating the StreamReader directly from the path because
            // it allows us to relax the FileAccess and FileShare settings.
            use fileStream = new FileStream (path, FileMode.Open, FileAccess.Read, FileShare.Read)
            use streamReader = new StreamReader (fileStream)

            let mutable result = None

            // Iterate over the lines of the file until we find one that matches the predicate.
            while Option.isNone result && not streamReader.EndOfStream do
                match picker <| streamReader.ReadLine () with
                | None -> ()
                | Some _ as res ->
                    result <- res

            // Return the result.
            result

        //
        [<CompiledName("Pick")>]
        let pick (picker : string -> 'T option) (path : string) : 'T =
            // Preconditions
            checkNonNull "path" path
            // TODO : Check file exists

            // Call tryPick; raise an exception if no matching line was found.
            match tryPick picker path with
            | Some x -> x
            | None ->
                // TODO : Provide a better exception message.
                //keyNotFound ""
                raise <| System.Collections.Generic.KeyNotFoundException ()

        //
        [<CompiledName("Map")>]
        let map (mapping : string -> 'T) (path : string) : 'T[] =
            // Preconditions
            checkNonNull "path" path
            // TODO : Check file exists

            // Create a FileStream and wrap it in a StreamReader so we can read the file line-by-line.
            // FileStream is used instead of creating the StreamReader directly from the path because
            // it allows us to relax the FileAccess and FileShare settings.
            use fileStream = new FileStream (path, FileMode.Open, FileAccess.Read, FileShare.Read)
            use streamReader = new StreamReader (fileStream)

            /// The mapped values.
            let mappedValues = ResizeArray ()

            // Iterate over the lines of the file, applying the function to each of them.
            while not streamReader.EndOfStream do
                let mappedValue = mapping <| streamReader.ReadLine ()
                ResizeArray.add mappedValue mappedValues

            // Return the chosen values.
            ResizeArray.toArray mappedValues

//        //
//        [<CompiledName("Partition")>]
//        let partition (predicate : string -> bool) (path : string) : string[] * string[] =
//            // Preconditions
//            checkNonNull "path" path
//            // TODO : Check file exists
//
//            notImpl "IO.File.Lines.partition"
//
//        //
//        [<CompiledName("MapPartition")>]
//        let mapPartition (partitioner : string -> Choice<'T, 'U>) (path : string) : 'T[] * 'U[] =
//            // Preconditions
//            checkNonNull "path" path
//            // TODO : Check file exists
//
//            notImpl "IO.File.Lines.mapPartition"
//
//        //
//        [<CompiledName("MapReduce")>]
//        let mapReduce (mapReducer : IMapReduction<string, 'T>) (path : string) : 'T =
//            // Preconditions
//            checkNonNull "path" path
//            // TODO : Check file exists
//
//            notImpl "IO.File.Lines.mapPartition"


/// Extensions that simplify working with Stream using async sequences.
[<AutoOpen>]
module IOExtensions = 
  type System.IO.Stream with
    /// Asynchronously reads the stream in chunks of a specified size
    /// and returns the result as an asynchronous sequence.
    member x.AsyncReadSeq(?bufferSize) = 
      let bufferSize = defaultArg bufferSize 1024
      let buffer = Array.zeroCreate bufferSize
      let rec loop () = asyncSeq {
        let! count = x.AsyncRead(buffer, 0, bufferSize)
        if count > 0 then 
          yield Array.sub buffer 0 count
          yield! loop() }
      loop ()

    /// Asynchronously writes all data specified by the 
    /// given asynchronous sequence to the stream.
    member x.AsyncWriteSeq(input : AsyncSeq<byte[]>) = async {
      for data in input do
        do! x.AsyncWrite(data) }

