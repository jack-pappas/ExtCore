(*

Copyright 2005-2009 Microsoft Corporation
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

namespace ExtCore

(* TODO :   Replace the code from the F# PowerPack with a new implementation
            which doesn't require the use of mutable state (e.g., ref cells)
            and which does allow the use of arbitrary parser functions for
            parsing flag values in a type-safe way. For example, this would
            allow DateTime values to be parsed directly from the string then
            returned to the user, instead of parsing the value as a string then
            needing to convert/validate it manually. *)

////
//type TypedArgInfo<'T> = {
//    //
//    ShortName : char option;
//    //
//    ShortAliases : Set<char>;
//    //
//    Name : string;
//    //
//    Aliases : Set<string>;
//    //
//    Description : string option;
//    //
//    DefaultValue : 'T option;
//}
//
//module Example =
//    /// Help options.
//    let helpArgInfo : TypedArgInfo<unit> = {
//        ShortName = Some 'h';
//        ShortAliases = Set.singleton '?';
//        Name = "help";
//        Aliases = Set.empty;
//        Description = Some "Display this list of options.";
//        DefaultValue = None;
//    }


(*  The code below is from the F# PowerPack and is only temporary --
    eventually it will be replaced with a new, functional-style API. *)

#nowarn "44"    // Disable warnings from [<Obsolete>]

/// The spec value describes the action of the argument,
/// and whether it expects a following parameter.
type ArgType =
    //
    | Clear of bool ref
    //
    | Float of (float -> unit)
    //
    | Int of (int -> unit)
    //
    | Rest of (string -> unit)
    //
    | Set of bool ref
    //
    | String of (string -> unit)
    //
    | Unit of (unit -> unit)

//
type ArgInfo = {
    /// The name of the argument.
    Name : string;
    /// The argument type and action of the argument.
    Type : ArgType;
    /// The usage help associated with the argument.
    HelpText : string;
} with
    /// Create an ArgInfo instance.
    static member Create (name, argType, helpText) =
        { Name = name;
          Type = argType;
          HelpText = helpText; }
  
/// When thrown, the string will be displayed to the user (usually printed to the console).
exception private ShowText of string

//
[<Sealed>]
type ArgParser () =
    static let getUsage specs (usageText : string) =
        let sb = System.Text.StringBuilder 100

        sb.AppendLine usageText |> ignore

        specs
        |> List.iter (fun (arg : ArgInfo) ->
            let argDisplayType =
                match arg.Type with
                | Unit _
                | Set _
                | Clear _ ->
                    None
                | String _ ->
                    Some "<string>"
                | Int _ ->
                    Some "<int>"
                | Float _ ->
                    Some "<float>"
                | Rest _ ->
                    Some "..."

            match argDisplayType with
            | None ->
                Printf.bprintfn sb "\t%s: %s" arg.Name arg.HelpText
            | Some displayType ->
                Printf.bprintfn sb "\t%s %s: %s" arg.Name displayType arg.HelpText)

        // Append the help options.
        sb.AppendLine "\t--help: display this list of options" |> ignore
        sb.AppendLine "\t-help: display this list of options" |> ignore

        // Return the usage text.
        sb.ToString ()

    /// Parse some of the arguments given by 'argv', starting at the given position
    [<System.Obsolete("This method should not be used directly as it will be removed in a future revision of this library")>]
    static member private ParsePartial (cursor, argv, argSpecs : ArgInfo list, ?other, ?usageText) =
        let other = defaultArg other ignore
        let usageText = defaultArg usageText ""
        let nargs = Array.length argv
        incr cursor
        let specs =
            argSpecs
            |> List.map (fun (arg : ArgInfo) ->
                arg.Name, arg.Type)
                
        while !cursor < nargs do
            let arg = argv.[!cursor]
            let rec findMatchingArg args =
                match args with
                | [] ->
                    // If any of the supplied arguments is a help switch, immediately print the help text and exit.
                    if arg = "-help" || arg = "--help" || arg = "/help" || arg = "/help" || arg = "/?" then
                        raise <| ShowText (getUsage argSpecs usageText)

                    // Note: for '/abc/def' does not count as an argument
                    // Note: '/abc' does
                    elif arg.Length > 0 && (arg.[0] = '-' || (arg.[0] = '/' && not (arg.Length > 1 && arg.[1..].Contains "/"))) then
                        let msg =
                            sprintf "Unrecognized argument: %s" arg
                            + System.Environment.NewLine
                            + getUsage argSpecs usageText
                        raise <| ShowText msg

                    else
                       other arg
                       incr cursor

                | ((s, action) :: _) when s = arg ->
                    let getSecondArg () =
                        if !cursor + 1 >= nargs then
                            let msg =
                                sprintf "option %s needs an argument." s
                                + System.Environment.NewLine
                                + getUsage argSpecs usageText
                            raise <| ShowText msg
                        argv.[!cursor + 1]
                 
                    match action with
                    | Unit f ->
                         f ()
                         incr cursor
                    | Set f ->
                         f := true
                         incr cursor
                    | Clear f ->
                         f := false
                         incr cursor
                    | String f ->
                         let arg2 = getSecondArg ()
                         f arg2
                         cursor := !cursor + 2
                    | Int f ->
                         let arg2 =
                            let arg2 = getSecondArg ()
                            try int32 arg2
                            with _ ->
                                raise <| ShowText (getUsage argSpecs usageText)
                         f arg2
                         cursor := !cursor + 2
                    | Float f ->
                         let arg2 =
                            let arg2 = getSecondArg ()
                            try float arg2
                            with _ ->
                                raise <| ShowText (getUsage argSpecs usageText)
                         f arg2
                         cursor := !cursor + 2
                    | Rest f ->
                        incr cursor
                        while !cursor < nargs do
                             f argv.[!cursor]
                             incr cursor

                | (_ :: more) ->
                    findMatchingArg more

            findMatchingArg specs

    /// Prints the help for each argument.
    static member Usage (specs, ?usage) =
        defaultArg usage ""
        |> getUsage (Array.toList specs)
        |> System.Console.Error.WriteLine

    #if FX_NO_COMMAND_LINE_ARGS
    #else
    /// Parse the arguments given by System.Environment.GetEnvironmentVariables()
    /// according to the argument processing specifications "specs".
    /// Args begin with "-". Non-arguments are passed to "f" in
    /// order.  "use" is printed as part of the usage line if an error occurs.
    static member Parse (specs, ?other, ?usageText) =
        let current = ref 0
        let argv = System.Environment.GetCommandLineArgs () 
        try
            ArgParser.ParsePartial (
                current, argv, Array.toList specs, ?other = other, ?usageText = usageText)
        with
        | ShowText text ->
            System.Console.Error.WriteLine text
            System.Console.Error.Flush ()
            exit 1
        | _ ->
            reraise ()
    #endif
