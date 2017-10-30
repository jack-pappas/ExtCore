//
namespace ExtCore.Control.Compability.Indexed

open ExtCore
open ExtCore.Control.Indexed
/// Indexed-state workflows.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Indexed =
    //
    [<CompiledName("ProtectedState")>]
    let protectedState = ProtectedIndexedStateBuilder ()

    //
    [<CompiledName("ReaderProtectedState")>]
    let readerProtectedState = ReaderProtectedIndexedStateBuilder ()

    //
    [<CompiledName("StatefulChoice")>]
    let statefulChoice = IndexedStatefulChoiceBuilder ()
