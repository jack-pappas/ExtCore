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

/// Functional programming operators related to the System.Collections.Generic.IDictionary type.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExtCore.Collections.Dict

open System.Collections.Generic
open LanguagePrimitives
open OptimizedClosures
open ExtCore


/// Thread-safe functional programming operators for mutable instances of System.Collections.Generic.IDictionary.
module Safe =
    /// Attempts to retrieve the value associated with the specified key.
    [<CompiledName("TryFind")>]
    let tryFind key (dict : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dict" dict

        match lock dict <| fun () -> dict.TryGetValue key with
        | false, _ ->
            None
        | true, value ->
            Some value
        
    /// Lookup an element in the Dictionary, raising KeyNotFoundException if
    /// the Dictionary does not contain an element with the specified key.
    [<CompiledName("Find")>]
    let inline find key (dict : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dict" dict

        lock dict <| fun () -> dict.[key]

    /// Adds a new entry to the Dictionary.
    [<CompiledName("Add")>]
    let add key value (dict : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dict" dict

        if dict.IsReadOnly then
            invalidOp "Cannot add an entry to a read-only dictionary."
        lock dict <| fun () ->
            dict.Add (key, value)

    /// Updates an existing entry in the dictionary with a new value,
    /// raising KeyNotFoundException if the Dictionary does not
    /// contain an element with the specified key.
    [<CompiledName("Update")>]
    let update key value (dict : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dict" dict

        if dict.IsReadOnly then
            invalidOp "Cannot update an entry in a read-only dictionary."
        lock dict <| fun () ->
            if dict.ContainsKey key then
                dict.[key] <- value
            else
                // TODO : Return a better error message
                //keyNotFound ""
                raise <| System.Collections.Generic.KeyNotFoundException ()

    /// Removes the entry with the specified key from the Dictionary,
    /// returning a value indicating the success of the operation.
    [<CompiledName("Remove")>]
    let remove (key : 'Key) (dict : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dict" dict

        if dict.IsReadOnly then
            invalidOp "Cannot remove an entry from a read-only dictionary."
        lock dict <| fun () ->
            dict.Remove key

    /// Updates the value of an entry (which has the specified key)
    /// in the Dictionary, or creates a new entry if one doesn't exist.
    /// If the Dictionary is read-only, an InvalidOperationException is raised.
    [<CompiledName("UpdateOrAdd")>]
    let updateOrAdd key value (dict : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dict" dict
            
        if dict.IsReadOnly then
            invalidOp "Cannot update or add an entry to a read-only dictionary."
        lock dict <| fun () ->
            dict.[key] <- value

    /// Creates an immutable copy of a Dictionary.
    /// The entries are shallow-copied to the created Dictionary; that is,
    /// reference-typed keys and values will reference the same instances as
    /// in the mutable Dictionary, so care should be taken when using mutable keys and values.
    [<CompiledName("Immutable")>]
    let immutable (dictionary : IDictionary<'Key, 'T>) =
        // Preconditions
        checkNonNull "dictionary" dictionary

        lock dictionary <| fun () ->
            dictionary
            |> Seq.map (fun kvp ->
                kvp.Key, kvp.Value)
            |> dict


/// Views the keys of the Dictionary as a sequence.
[<CompiledName("Keys")>]
let inline keys (dictionary : IDictionary<'Key, 'T>) =
    dictionary.Keys :> IEnumerable<'Key>

/// Views the values of the Dictionary as a sequence.
[<CompiledName("Values")>]
let inline values (dictionary : IDictionary<'Key, 'T>) =
    dictionary.Values :> IEnumerable<'T>

/// Determines whether the Dictionary is empty.
[<CompiledName("IsEmpty")>]
let inline isEmpty (dictionary : IDictionary<'Key,'T>) =
    dictionary.Count = 0

/// Gets the number of entries in the Dictionary.
[<CompiledName("Count")>]
let inline count (dictionary : IDictionary<'Key, 'T>) =
    dictionary.Count

/// Creates a mutable dictionary with the specified capacity.
[<CompiledName("CreateMutable")>]
let inline createMutable<'Key, 'T when 'Key : equality> (capacity : int) =
    System.Collections.Generic.Dictionary<'Key, 'T> (capacity)

/// Determines whether the Dictionary contains an element with the specified key.
[<CompiledName("ContainsKey")>]
let inline containsKey k (dictionary : IDictionary<'Key, 'T>) =
    // Preconditions
    checkNonNull "dictionary" dictionary

    dictionary.ContainsKey k

/// Adds a new entry to the dictionary.
[<CompiledName("Add")>]
let inline add k v (dictionary : IDictionary<'Key, 'T>) =
    // Preconditions
    checkNonNull "dictionary" dictionary

    dictionary.Add (k, v)
    dictionary

/// Removes the entry with the specified key from the Dictionary.
/// An exception is raised if the entry cannot be removed.
[<CompiledName("Remove")>]
let inline remove (k : 'Key) (dictionary : IDictionary<'Key, 'T>) =
    // Preconditions
    checkNonNull "dictionary" dictionary

    if dictionary.Remove k then
        dictionary
    else
        let msg = sprintf "Unable to remove the entry with the key '%O' from the dictionary." k
        raise <| exn msg

/// Lookup an element in the Dictionary, raising KeyNotFoundException if
/// the dictionary does not contain an element with the specified key.
[<CompiledName("Find")>]
let inline find k (dictionary : IDictionary<'Key, 'T>) =
    // Preconditions
    checkNonNull "dictionary" dictionary

    dictionary.[k]
            
/// Attempts to retrieve the value associated with the specified key.
[<CompiledName("TryFind")>]
let inline tryFind k (dictionary : IDictionary<'Key, 'T>) =
    // Preconditions
    checkNonNull "dictionary" dictionary

    match dictionary.TryGetValue k with
    | false, _ -> None
    | true, v -> Some v

/// Updates the value of an entry (which has the specified key) in the Dictionary.
/// Raises a KeyNotFoundException if the Dictionary does not contain an entry with the specified key.
[<CompiledName("Update")>]
let update k v (dictionary : IDictionary<'Key, 'T>) =
    // Preconditions
    checkNonNull "dictionary" dictionary

    if dictionary.ContainsKey k then
        dictionary.[k] <- v
        dictionary
    else
        // TODO : Add an error message which includes the key.
        //keyNotFound ""
        raise <| System.Collections.Generic.KeyNotFoundException ()

/// Updates the value of an entry (which has the specified key) in
/// the Dictionary, or creates a new entry if one doesn't exist.
[<CompiledName("UpdateOrAdd")>]
let inline updateOrAdd k v (dictionary : IDictionary<'Key, 'T>) =
    // Preconditions
    checkNonNull "dictionary" dictionary
        
    dictionary.[k] <- v
    dictionary    

/// Applies the given function to successive entries, returning the
/// first result where the function returns "Some(x)".
[<CompiledName("TryPick")>]
let tryPick picker (dictionary : IDictionary<'Key, 'T>) =
    // Preconditions
    checkNonNull "dictionary" dictionary

    let picker = FSharpFunc<_,_,_>.Adapt picker
        
    dictionary
    |> Seq.tryPick (fun kvp ->
        picker.Invoke (kvp.Key, kvp.Value))

/// Applies the given function to sucecssive entries, returning the
/// first x where the function returns "Some(x)".
[<CompiledName("Pick")>]
let pick picker (dictionary : IDictionary<'Key, 'T>) =
    // Preconditions
    checkNonNull "dictionary" dictionary

    match tryPick picker dictionary with
    | Some res -> res
    | None ->
        // TODO : Add an error message which includes the key.
        //keyNotFound ""
        raise <| System.Collections.Generic.KeyNotFoundException ()

/// Views the Dictionary as a sequence of tuples.
[<CompiledName("ToSeq")>]
let toSeq (dictionary : IDictionary<'Key, 'T>) =
    // Preconditions
    checkNonNull "dictionary" dictionary

    dictionary
    |> Seq.map (fun kvp ->
        kvp.Key, kvp.Value)

/// Applies the given function to each entry in the Dictionary.
[<CompiledName("Iter")>]
let iter (action : 'Key -> 'T -> unit) (dictionary : IDictionary<'Key, 'T>) =
    // Preconditions
    checkNonNull "dictionary" dictionary

    let action = FSharpFunc<_,_,_>.Adapt action

    dictionary
    |> Seq.iter (fun kvp ->
        action.Invoke (kvp.Key, kvp.Value))

/// Returns a new Dictionary containing only the entries of the
/// Dictionary for which the predicate returns 'true'.
[<CompiledName("Filter")>]
let filter (predicate : 'Key -> 'T -> bool) (dictionary : IDictionary<'Key, 'T>) =
    // Preconditions
    checkNonNull "dictionary" dictionary

    let predicate = FSharpFunc<_,_,_>.Adapt predicate

    dictionary
    |> Seq.choose (fun kvp ->
        if predicate.Invoke (kvp.Key, kvp.Value) then
            Some (kvp.Key, kvp.Value)
        else None)
    |> dict

/// Builds a new Dictionary whose entries are the results of applying
/// the given function to each element of the Dictionary.
[<CompiledName("Map")>]
let map (mapping : 'Key -> 'T -> 'U) (dictionary : IDictionary<'Key, 'T>) =
    // Preconditions
    checkNonNull "dictionary" dictionary

    let mapping = FSharpFunc<_,_,_>.Adapt mapping

    dictionary
    |> Seq.map (fun kvp ->
        kvp.Key, mapping.Invoke (kvp.Key, kvp.Value))
    |> dict

/// Applies the given function to each element of the Dictionary.
/// Returns a Dictionary comprised of the results "x,y" for each
/// entry where the function returns Some(y).
[<CompiledName("Choose")>]
let choose (chooser : 'Key -> 'T -> 'U option) (dictionary : IDictionary<'Key, 'T>) =
    // Preconditions
    checkNonNull "dictionary" dictionary

    let chooser = FSharpFunc<_,_,_>.Adapt chooser        
        
    dictionary
    |> Seq.choose (fun kvp ->
        chooser.Invoke (kvp.Key, kvp.Value)
        |> Option.map (fun x -> kvp.Key, x))
    |> dict

/// Applies a function to each entry of the Dictionary,
/// threading an accumulator argument through the computation.
[<CompiledName("Fold")>]
let fold folder (state : 'State) (dict : IDictionary<'Key, 'T>) =
    // Preconditions
    checkNonNull "dict" dict

    let folder = FSharpFunc<_,_,_,_>.Adapt folder

    (state, dict)
    ||> Seq.fold (fun state kvp ->
        folder.Invoke (state, kvp.Key, kvp.Value))

/// Splits the Dictionary into two Dictionaries, containing the entries
/// for which the given predicate evaluates to "true" and "false".
[<CompiledName("Partition")>]
let partition partitioner (dictionary : IDictionary<'Key, 'T>) =
    // Preconditions
    checkNonNull "dictionary" dictionary

    let partitioner = FSharpFunc<_,_,_>.Adapt partitioner

    let t, f =
        ((Seq.empty, Seq.empty), dictionary)
        ||> fold (fun (trueSeq, falseSeq) k v ->
            if partitioner.Invoke (k, v) then
                (Seq.append trueSeq (Seq.singleton (k, v)), falseSeq)
            else
                (trueSeq, Seq.append falseSeq (Seq.singleton (k, v))))

    dict t, dict f

/// Returns a read-only view of a dictionary.
[<CompiledName("Readonly")>]
let readonly (dictionary : IDictionary<'Key, 'T>) =
    dict <| toSeq dictionary

