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


// Version information
[<assembly: AssemblyVersion("0.8.16")>]
[<assembly: AssemblyFileVersion("0.8.16")>]
[<assembly: AssemblyInformationalVersion("0.8.16")>]

// Assembly information
[<assembly: AssemblyTitle("ExtCore")>]
[<assembly: AssemblyDescription("An extended core library for F#.")>]
[<assembly: NeutralResourcesLanguage("en-US")>]
[<assembly: Guid("ecbe6801-9675-413e-849b-c3359721cf06")>]
[<assembly: AssemblyCopyright("Copyright © Jack Pappas 2013")>]
[<assembly: ComVisible(false)>]

// This assembly is CLS-compliant.
// TODO : Enable this once we can verify that the assembly is actually CLS-compliant.
//[<assembly: CLSCompliant(true)>]

// Only allow types derived from System.Exception to be thrown --
// any other types should be automatically wrapped.
[<assembly: RuntimeCompatibility(WrapNonExceptionThrows = true)>]

// Security-related stuff.
//[<assembly: AllowPartiallyTrustedCallers>]
//[<assembly: SecurityTransparent>]

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

(* Automatically open the basic namespaces, as in FSharp.Core. *)
[<assembly: AutoOpen("ExtCore")>]
[<assembly: AutoOpen("ExtCore.Collections")>]
// These are disabled, for now, to avoid cluttering up everyone's default contexts.
// If it seems reasonable to do so, enable these prior to releasing ExtCore 1.0.
//[<assembly: AutoOpen("ExtCore.Control")>]
//[<assembly: AutoOpen("ExtCore.Control.Collections")>]

(* Create an AssemblyConfiguration attribute with relevant conditional compilation symbols
   defined when this assembly was compiled. This can be useful for diagnostic purposes. *)
#if PROTO_COMPILER
#if DEBUG
#if TRACE
[<assembly: AssemblyConfiguration("DEBUG;TRACE;PROTO_COMPILER")>]
#else
[<assembly: AssemblyConfiguration("DEBUG;PROTO_COMPILER")>]
#endif
#else
#if TRACE
[<assembly: AssemblyConfiguration("TRACE;PROTO_COMPILER")>]
#else
[<assembly: AssemblyConfiguration("PROTO_COMPILER")>]
#endif
#endif

#else

#if DEBUG
#if TRACE
[<assembly: AssemblyConfiguration("DEBUG;TRACE")>]
#else
[<assembly: AssemblyConfiguration("DEBUG")>]
#endif
#else
#if TRACE
[<assembly: AssemblyConfiguration("TRACE")>]
#else
[<assembly: AssemblyConfiguration("")>]
#endif
#endif

#endif

(*  F# considers modules which only contain attributes to be empty;
    so, we appease the compiler by adding an empty function. *)
do ()







