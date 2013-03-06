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

module internal AssemblyInfo

open System
open System.Reflection
open System.Resources
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Security
open System.Security.Permissions
    

/// <summary>A subset of the conditional compilation symbols
/// specified when this assembly was compiled.</summary>
/// <remarks>Used for diagnostics purposes, e.g., to mark traced
/// and debug builds.</remarks>
let [<Literal>] private assemblyConfig =
    #if DEBUG
    #if TRACE
    "DEBUG;TRACE"
    #else
    "DEBUG"
    #endif
    #else
    #if TRACE
    "TRACE"
    #else
    ""
    #endif
    #endif

// Version information
[<assembly: AssemblyVersion("0.8.1")>]
[<assembly: AssemblyFileVersion("0.8.1")>]
[<assembly: AssemblyInformationalVersion("0.8.1")>]

// Assembly information
[<assembly: AssemblyTitle("ExtCore")>]
[<assembly: AssemblyDescription("An extended core library for F#.")>]
[<assembly: NeutralResourcesLanguage("en-US")>]
[<assembly: Guid("ecbe6801-9675-413e-849b-c3359721cf06")>]

// General Information about an assembly is controlled through the following
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
[<assembly: AssemblyConfiguration(assemblyConfig)>]
[<assembly: AssemblyCopyright("Copyright © Jack Pappas 2013")>]
//[<assembly: AssemblyTrademark("")>]
//[<assembly: AssemblyCulture("")>]

// Setting ComVisible to false makes the types in this assembly not visible
// to COM components.  If you need to access a type in this assembly from
// COM, set the ComVisible attribute to true on that type.
[<assembly: ComVisible(false)>]

// Only allow types derived from System.Exception to be thrown --
// any other types should be automatically wrapped.
[<assembly: RuntimeCompatibility(WrapNonExceptionThrows = true)>]

// Security-related stuff.
//[<assembly: AllowPartiallyTrustedCallers>]
//[<assembly: SecurityTransparent>]

// This assembly is CLS-compliant.
// TODO : Enable this once we can verify that the assembly is actually CLS-compliant.
//[<assembly: CLSCompliant(true)>]

(*  Makes internal modules, types, and functions visible
    to the test project so they can be unit-tested. *)
#if DEBUG
[<assembly: InternalsVisibleTo("ExtCore.Tests")>]
#endif

(* Dependency hints for Ngen *)
[<assembly: DependencyAttribute("FSharp.Core", LoadHint.Always)>]
[<assembly: DependencyAttribute("System", LoadHint.Always)>]
[<assembly: DependencyAttribute("System.Core", LoadHint.Always)>]
[<assembly: DefaultDependency(LoadHint.Always)>]


(*  F# considers modules which only contain attributes to be empty;
    so, we appease the compiler by adding an empty function. *)
do ()







