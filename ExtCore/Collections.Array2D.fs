(*

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

(* TODO :   Implement a struct, ArraySegment2D, which is like ArraySegment but for 2D arrays.
            It should support structural equality comparison and hashing. *)
(* TODO :   Implement structs, RowSegment and ColumnSegment, similar to the above, but which
            expose a segment of a single row or column in a 2D array. These will be used to avoid
            unnecessary data copies in the implementations of the Array2D.Rows and Array2D.Columns modules. *)
(* TODO :   Implement an 'array2D<'T>' type alias for 'T[,], just as 'array<'T>' is an alias for 'T[]. *)
(* TODO :   Implement a 'vector2D<'T>' struct and a Vector2D module based on the modules below,
            just as 'vector<'T>' and the Vector module are based on the code for the Array module. *)


/// Additional functional operators on 2-dimensional arrays.
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExtCore.Collections.Array2D

open LanguagePrimitives
open OptimizedClosures
open ExtCore


/// Returns the number of rows in the array.
[<CompiledName("RowCount")>]
let rowCount (array : 'T[,]) : int =
    // Preconditions
    checkNonNull "array" array

    // Return the number of rows in the array.
    array.GetLength 0

/// Returns the number of columns in the array.
[<CompiledName("ColumnCount")>]
let columnCount (array : 'T[,]) : int =
    // Preconditions
    checkNonNull "array" array

    // Return the number of columns in the array.
    array.GetLength 1

/// Returns the number of elements in the array.
[<CompiledName("Count")>]
let count (array : 'T[,]) : int =
    // Preconditions
    checkNonNull "array" array

    // Get the lengths of the array dimensions.
    let dim0_length = array.GetLength 0
    let dim1_length = array.GetLength 1

    Checked.(*) dim0_length dim1_length



(* TODO :   Implement nested modules, Array2D.Rows and Array2D.Columns (or Array2D.Cols), which provide
            row- and column-based operations on 2D arrays. These functions will be extremely useful in
            implementing simple data analyses, or for "massaging" data being used with other libraries like .NET Numerics.

            For example, we might have a function that performs a column-wise reduction, producing a row vector
            whose length is equal to the number of columns in the input array2D.
                Array2D.Cols.reduce (reduction : 'T -> 'T -> 'T) (array : 'T[,]) : 'T[]

            We could implement two different kinds of folds: one which takes an input row/column vector, so that
            different initial state values can be used for folding over each column/row, and another which takes
            a single initial state value (similar to Array.fold), which will be used as the initial state value
            for the folds over each column/row.
*)

