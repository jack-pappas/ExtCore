TODO for ExtCore 1.0
####################

Library_
    General, library-wide TODO items.

General_
    General-purpose modules, such as ``Operators`` and ``ExtraTopLevelOperators``.

Strings_
    String manipulation.
    
`Collections and Data Structures`_
    General-purpose and specialized collections and data structures.

Caching_
    Map-like data structures implementing various caching protocols.

`Workflow Collections`_
    Collections modules from the F# Core Library (e.g., ``Array``) and ExtCore,
    adapted ("lifted") for compatibility with various F# workflows.

Parallel_
    Functions implementing parallelized operations over collections.


Library
=======

Documentation
-------------
- Fill out missing documentation comments throughout the library. This is not limited to summaries --
  we also need to fill in descriptions for parameters (``<param>``), return values (``<returns>``),
  type parameters (``<typeparam>``) and so on.

- Generate some nice-looking documentation from the XML documentation file (``ExtCore.XML``) generated
  when the library is compiled. This needs to be done in a way that can be automated so it can be
  added to the build process.


Build
-----
- Publish debugging symbols (e.g., on SymbolSource) and add ``ExtCore.pdb`` to the NuGet package.
- Should we publish separate NuGet packages for F# 2.0 and F# 3.0 versions of the library?

  - If not, we should still include some logic in the NuGet package which adds a binding redirect
    when the package is added to an F# 3.0 project.

- Write some better build scripts using MSBuild or FAKE so we can easily compile multiple
  versions of the library targeting different CLR versions for distribution in the NuGet package.


Tests
-----
- Fill out the unit tests and implement more tests with FsCheck.

  - Currently, there are approximately 1200 unit tests in ``ExtCore.Tests``, around 300 of which
    are just stubs (they only call ``Assert.Ignore()``). There are a small number (~10) failing
    tests which need to be investigated and fixed.

- Clean up the unit tests imported from the F# distribution and the F# PowerPack. This includes:

  - The Map and Set type and module tests which were imported and adapted for our custom
    map and set types (e.g., ``IntSet``).
  - The Array module tests which were imported and adapted for the ``vector`` type.
  - The tests for ``ResizeArray`` and ``LazyList`` which were imported from the F# PowerPack.


General
=======

Nullable
--------
- Implement a ``Nullable`` module similar to the ``Option`` module in the F# Core Library.
  This'll make it easier to write functional-style code for nullable values.


TextWriter
----------
- Add extension methods / overloads of Write and WriteLine which accept a ``substring`` value.
- Add extension methods / overloads of Write and WriteLine which accept a ``vector<char>`` value.


Strings
=======

String
------
- exactlyOne
- foldi
- foldiBack
- foldi2
- foldiBack2


String.Split
------------
- get

  Given an index, gets the substring at that index in the array of substrings created by the split operation.


Substring
---------
- toList
- trim
- trimStart
- trimEnd
- trimWith
- trimStartWith
- trimEndWith

  These should work like the functions in the String module, except on ``substring``.
  This makes it so trimming a string doesn't need to create an additional string object,
  it simply returns a ``substring`` which is equal to or smaller than the input substring.

- Interfaces

  - IEquatable
  - IEquatable<'T>
  - IComparable
  - IComparable<'T>

    Implement comparison which works just like the built-in string comparison.


Collections and Data Structures
===============================

Array
-----
- exactlyOne
- scan2
- scanBack2
- unfold


Bimap
-----
- ofMap
- toMap
- tryOfMap
- projectKeys
- projectValues


IntMap/IntSet/HashMap/HashSet
-----------------------------
- Implement serialization/deserialization code for parity with Map and Set.


IntBimap / LongBimap
--------------------
- map
- ofIntMap
- tryOfIntMap
- projectKeys
- projectValues


IntMap / LongMap
----------------
- exactlyOne
- ofKeys
- ofValues
- ofIntKeys

  This should work like 'ofKeys' but with IntSet instead of Set.

- keys
- values

  Should this return a set or a multiset?

- extractMin
- extractMax
- tryExtractMin
- tryExtractMax

- Interfaces

  - IReadOnlyDictionary<TKey, TValue> (.NET 4.5)
  - IReadOnlyCollection<KeyValuePair<TKey, TValue>> (.NET 4.5)

- Implement IntMap-based versions of our custom Map functions.


IntSet / LongSet
----------------
- exactlyOne
- extractMin
- extractMax
- reduce
- reduceBack
- tryExtractMin
- tryExtractMax
- scan
- scanBack
- ``allSubsets : IntSet -> seq<IntSet>``

  Returns a sequence of all subsets of the given set.

- ``subsets : IntSet -> int -> seq<IntSet>``

  Returns a sequence which produces all subsets of the given set, which have the given size.

- lessThan
- greaterThan

  Given an IntSet and a value, returns the subset containing the values less than (or greater than) the value.

- Interfaces

  - ISet<'T> (.NET 4.0)
  - IReadOnlyCollection<'T> (.NET 4.5)

- Implement IntSet- and TagSet-based versions of our custom Set functions.


LazyList
--------
- exactlyOne
- ``force : LazyList<'T> -> unit``

  Traverses the LazyList and forces evaluation of all cells. May not terminate.

- ``forcePartial : int -> LazyList<'T> -> unit``

  Traverses the given number of cells in the LazyList (or to the end), forcing evaluation
  of the traversed cells. May not terminate.

- ``lazyLength : LazyList<'T> -> int``

  Computes the "lazy" length of the LazyList<'T> -- that is, the number of cells which have
  already been evaluated. Unlike LazyList.length, this does not force evaluation of any cells
  and always terminates.

- ``ofSeqEager : seq<'T> -> LazyList<'T>``

  Similar to 'ofSeq', but eagerly enumerates the sequence to build a LazyList.
  This allows us to detect certain sequence types (like 'T[] and 'T list) and use optimized
  implementations, avoids the possibility of memory leaks, and avoids lazily-evaluating
  list elements when they don't really need it.

- Interfaces

  - ICollection
  - ICollection<'T>
  - IList
  - IList<'T>
  - IReadOnlyList<'T> (.NET 4.5)
  - IReadOnlyCollection<'T> (.NET 4.5)

- Implement a DebuggerTypeProxy? If so, we need to figure out how to do this in a safe way.


List
----
- exactlyOne
- ``insert : (index : int) -> (value : 'T) -> (list : 'T list) : 'T list``

  Creates a new list by inserting the value at a given index in a list.

- ``update : (index : int) -> (value : 'T) -> (list : 'T list) : 'T list``

  Creates a new list by setting the element at the specified index to a given value.

- ``splice : (index : int) -> (list1 : 'T list) -> (list2 : 'T list) : 'T list``

  Creates a new list by "splicing" the second list into the first at the given index.

- ``distinct : (list : 'T list) : 'T list (where 'T : equality)``

  Returns a new list created by keeping only the first (earliest) instance of each element.


Map
---
- exactlyOne
- mapi
- mapiBack
- foldi
- foldiBack
- ``scan (folder : 'State -> 'T -> 'State) (state : 'State) (map : Map<'Key, 'T>) : Map<'Key, 'State>``
- ``scanBack``

  Like Map.fold/Map.foldBack, but returns a new map which holds the intermediate result after processing each key/value pair.

- ``findOrAdd (generator : 'Key -> 'T) (key : 'Key) (map : Map<'Key, 'T>) : 'T * Map<'Key, 'T>``

  Retrieves the value associated with the specified key in the map; if the key does not exist in the map,
  the key is applied to the generator function to create a value, which is then stored in the map.
  The retrieved/created value is returned along with the (possibly) updated map.

- ``tryFindOrAdd (generator : 'Key -> 'T option) (key : 'Key) (map : Map<'Key, 'T>) : 'T option * Map<'Key, 'T>``
- ``extract (key : 'Key) (map : Map<'Key, 'T>) : 'T * Map<'Key, 'T>``
- ``tryExtract (key : 'Key) (map : Map<'Key, 'T>) : 'T option * Map<'Key, 'T>``
- ``findAndUpdate (generator : 'Key -> 'T -> 'T) (key : 'Key) (map : Map<'Key, 'T>) : 'T * Map<'Key, 'T>``

  Retrieves the value associated with the specified key in the map; if the key does not exist in the map,
  KeyNotFoundException is raised. The key and original value are applied to the generator function to
  produce a new value which is stored in the map. (OPTIMIZATION: Only update the map if the generated value
  is different than the original value.)
  The retrieved value is returned along with the (possibly) updated map.

- ``addOrUpdate (generator : 'Key -> 'T option -> 'T) (key : 'Key) (map : Map<'Key, 'T>) : Map<'Key, 'T>``
- ``maxKey : (map : Map<'Key, 'T>) : 'Key``
- ``minKey : (map : Map<'Key, 'T>) : 'Key``

  The minimum/maximum key value in the map.

- ``maxKeyBy (projection : 'Key -> 'T -> 'U) (map : Map<'Key, 'T>) : 'Key (where 'U : comparison)``
- ``minKeyBy (projection : 'Key -> 'T -> 'U) (map : Map<'Key, 'T>) : 'Key (where 'U : comparison)``

  The minimum/maximum key value in the map, compared using the given function.

- findKeyBack
- pickBack
- tryPickBack
- tryFindKeyBack

  Just like the built-in functions (e.g., findKey, pick) except they traverse "backwards" over the map,
  i.e., from greatest to least key value. This is useful when the map could contain multiple matching
  key/value pairs and we want to choose the one with the greatest key value.


Queue
-----
- exactlyOne
- ofList
- ofArray
- ofSeq
- toSeq
- peek

- Interfaces

  - IEnumerable
  - IEnumerable<'T>
  - ICollection
  - ICollection<'T>
  - IList
  - IList<'T>
  - IReadOnlyList<'T> (.NET 4.5)

- Implement a DebuggerTypeProxy


ResizeArray
-----------
- exactlyOne
- ofVector
- toVector


Seq
---
- fold2
- Seq.choosei
- Seq.segment

  Groups elements of a sequence together "longitudinally" -- i.e., it works
  in a streaming fashion, rather than Seq.groupBy which needs to see the
  entire stream before returning. Alternatively, this can be thought of
  as a generalized form of Seq.windowed.

- Seq.sample

  Takes a positive integer and a sequence.
  Returns a sequence containing every n-th element of the input sequence.


Set
---
- ``exactlyOne``
- ``scan : folder:('State -> 'T -> 'State) -> state:'State -> set:Set<'T> -> Set<'State>``
- ``scanBack``
- ``allSubsets : Set<'T> -> seq<Set<'T>>``

  Returns a sequence of all subsets of the given set.

- ``subsets : Set<'T> -> int -> seq<Set<'T>>``

  Returns a sequence which produces all subsets of the given set, which have the given size.

- ``lessThan``
- ``greaterThan``

  Given a Set and a value, returns the subset containing the values less than (or greater than) the value.

- ``findBack``
- ``pickBack``
- ``tryFindBack``
- ``tryPickBack``

  Just like the built-in functions (e.g., findKey, pick) except they traverse "backwards" over the set,
  i.e., from greatest to least value. This is useful when the set could contain multiple matching
  values and we want to choose the greatest one.

- Define a type extension for ``Set<'T>`` which provides the xor ``(^^^)`` operator,
  via the ``Set.symmetricDifference`` function.


TagBimap
--------
- map
- ofTagMap
- tryOfTagMap
- projectKeys
- projectValues


Vector
------
- exactlyOne
- findBack
- pickBack
- tryFindBack
- tryPickBack

  Just like the built-in functions (e.g., findKey, pick) except they traverse "backwards" over the vector,
  i.e., from highest to lowest index. This is useful when the vector could contain multiple matching
  values and we want to choose the one with the greatest index.

- Interfaces

  - IEquatable
  - IEquatable<'T>
  - IComparable
  - IComparable<'T>
  - ICollection
  - ICollection<'T>
  - IList
  - IList<'T>
  - ICloneable
  - IStructuralComparable
  - IStructuralEquatable


Caching
=======

LruCache
--------
- findKey
- tryFindKey

  These should work like the functions in the Map module.

- findKeyBack
- pickBack
- tryPickBack
- tryFindKeyBack

  Just like the built-in functions (e.g., findKey, pick) except they traverse "backwards" over the cache,
  i.e., from newest (most-recently-used) to oldest (least-recently-used) key value. This is useful when the
  cache could contain multiple matching key/value pairs and we want to choose the one with the newest key value.

- Import the MapType and MapModule tests from the F# distribution and adapt them for LruCache.
- Implement a comparison method similar to how LruCache.Equals is implemented.


Workflow Collections
====================

State.Array
-----------
- mapReduce


State.List
----------
- foldi
- foldBack
- foldiBack


Parallel
========
Implement parallel versions of functions similar to those in the "top-level" module
(e.g., ``Array``, ``List``), based on the TPL and/or PLINQ. These will complement the
modules which ship within the F# Core Library, providing additional functionality.

Array.Parallel
--------------
*TODO*


IntMap.Parallel
---------------
*TODO*


IntSet.Parallel
---------------
*TODO*


List.Parallel
-------------
*TODO*


Map.Parallel
------------
*TODO*


Seq.Parallel
------------
*TODO*


Set.Parallel
------------
*TODO*


Vector.Parallel
---------------
*TODO*
