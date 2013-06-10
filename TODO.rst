TODO for ExtCore 1.0
====================

General
-------

  - Fill-in any missing documentation comments.
  - Publish debugging symbols (e.g., on SymbolSource) and add ExtCore.pdb to the NuGet package.


Array
-----

  - scan2
  - scanBack2
  - unfold
  - findBack
  - pickBack
  - tryFindBack
  - tryPickBack
    Just like the built-in functions (e.g., findKey, pick) except they traverse "backwards" over the array,
    i.e., from highest to lowest index. This is useful when the array could contain multiple matching
    values and we want to choose the one with the greatest index.


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


IntBimap
--------

  - ofMap
  - toMap
  - tryOfMap
  - projectKeys
  - projectValues


IntMap
------

  - ofKeys
  - ofValues
  - ofIntKeys
    This should work like 'ofKeys' but with IntSet instead of Set.
  - keys
  - values
    Should this return a multiset?
  - extractMin
  - extractMax
  - tryExtractMin
  - tryExtractMax
  - IReadOnlyDictionary<TKey, TValue> (.NET 4.5)
  - IReadOnlyCollection<KeyValuePair<TKey, TValue>> (.NET 4.5)
  - Implement IntMap-based versions of our custom Map functions.


IntSet
------

  - extractMin
  - extractMax
  - reduce
  - reduceBack
  - tryExtractMin
  - tryExtractMax
  - tryFind
  - find
  - scan
  - scanBack
  - allSubsets : IntSet -> seq<IntSet>
    Returns a sequence of all subsets of the given set.
  - subsets : IntSet -> int -> seq<IntSet>
    Returns a sequence which produces all subsets of the given set, which have the given size.
  - lessThan
  - greaterThan
    Given an IntSet and a value, returns the subset containing the values less than (or greater than) the value.
  - ISet<'T> (.NET 4.0)
  - IReadOnlyCollection<'T> (.NET 4.5)
  - Implement IntSet- and TagSet-based versions of our custom Set functions.


LazyList
--------

  - force : LazyList<'T> -> unit
    Traverses the LazyList and forces evaluation of all cells. May not terminate.

  - forcePartial : int -> LazyList<'T> -> unit
    Traverses the given number of cells in the LazyList (or to the end), forcing evaluation
    of the traversed cells. May not terminate.

  - lazyLength : LazyList<'T> -> int
    Computes the "lazy" length of the LazyList<'T> -- that is, the number of cells which have
    already been evaluated. Unlike LazyList.length, this does not force evaluation of any cells
    and always terminates.

  - ofSeqEager : seq<'T> -> LazyList<'T>
    Similar to 'ofSeq', but eagerly enumerates the sequence to build a LazyList.
    This allows us to detect certain sequence types (like 'T[] and 'T list) and use optimized
    implementations, avoids the possibility of memory leaks, and avoids lazily-evaluating
    list elements when they don't really need it.

  - ICollection
  - ICollection<'T>
  - IList
  - IList<'T>
  - IReadOnlyList<'T> (.NET 4.5)
  - IReadOnlyCollection<'T> (.NET 4.5)

  - Implement a DebuggerTypeProxy? If so, we need to figure out how to do this in a safe way.


List
----

  - insert : (index : int) -> (value : 'T) -> (list : 'T list) : 'T list
    Creates a new list by inserting the value at a given index in a list.
  - update : (index : int) -> (value : 'T) -> (list : 'T list) : 'T list
    Creates a new list by setting the element at the specified index to a given value.
  - splice : (index : int) -> (list1 : 'T list) -> (list2 : 'T list) : 'T list
    Creates a new list by "splicing" the second list into the first at the given index.
  - distinct : (list : 'T list) : 'T list (where 'T : equality)
    Returns a new list created by keeping only the first (earliest) instance of each element.


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


Map
---

  - mapi
  - mapiBack
  - foldi
  - foldiBack
  - scan (folder : 'State -> 'T -> 'State) (state : 'State) (map : Map<'Key, 'T>) : Map<'Key, 'State>
  - scanBack
    Like Map.fold/Map.foldBack, but returns a new map which holds the intermediate result after processing each key/value pair.
  - findOrAdd (generator : 'Key -> 'T) (key : 'Key) (map : Map<'Key, 'T>) : 'T * Map<'Key, 'T>
    Retrieves the value associated with the specified key in the map; if the key does not exist in the map,
    the key is applied to the generator function to create a value, which is then stored in the map.
    The retrieved/created value is returned along with the (possibly) updated map.
  - tryFindOrAdd (generator : 'Key -> 'T option) (key : 'Key) (map : Map<'Key, 'T>) : 'T option * Map<'Key, 'T>
  - extract (key : 'Key) (map : Map<'Key, 'T>) : 'T * Map<'Key, 'T>
  - tryExtract (key : 'Key) (map : Map<'Key, 'T>) : 'T option * Map<'Key, 'T>
  - findAndUpdate (generator : 'Key -> 'T -> 'T) (key : 'Key) (map : Map<'Key, 'T>) : 'T * Map<'Key, 'T>
    Retrieves the value associated with the specified key in the map; if the key does not exist in the map,
    KeyNotFoundException is raised. The key and original value are applied to the generator function to
    produce a new value which is stored in the map. (OPTIMIZATION: Only update the map if the generated value
    is different than the original value.)
    The retrieved value is returned along with the (possibly) updated map.
  - addOrUpdate (generator : 'Key -> 'T option -> 'T) (key : 'Key) (map : Map<'Key, 'T>) : Map<'Key, 'T>
  - maxKey : (map : Map<'Key, 'T>) : 'Key
  - minKey
    The minimum/maximum key value in the map.
  - maxKeyBy (projection : 'Key -> 'T -> 'U) (map : Map<'Key, 'T>) : 'Key (where 'U : comparison)
  - minKeyBy
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

  - ofList
  - ofArray
  - ofSeq
  - toSeq
  - peek
  
  - IEnumerable
  - IEnumerable<'T>
  - ICollection
  - ICollection<'T>
  - IList
  - IList<'T>
  - IReadOnlyList<'T> (.NET 4.5)

  - Implement a DebuggerTypeProxy


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
  - scan : folder:('State -> 'T -> 'State) -> state:'State -> set:Set<'T> -> Set<'State>
  - scanBack
  - allSubsets : Set<'T> -> seq<Set<'T>>
    Returns a sequence of all subsets of the given set.
  - subsets : Set<'T> -> int -> seq<Set<'T>>
    Returns a sequence which produces all subsets of the given set, which have the given size.
  - lessThan
  - greaterThan
    Given a Set and a value, returns the subset containing the values less than (or greater than) the value.
  - findBack
  - pickBack
  - tryFindBack
  - tryPickBack
    Just like the built-in functions (e.g., findKey, pick) except they traverse "backwards" over the set,
    i.e., from greatest to least value. This is useful when the set could contain multiple matching
    values and we want to choose the greatest one.


String
------

  - foldi
  - foldiBack
  - foldi2
  - foldiBack2
  - Split
    - get
      Given an index, gets the substring at that index in the array of substrings created by the split operation.


Substring
---------

  - IEquatable
  - IEquatable<'T>
  - IComparable
  - IComparable<'T>
    Implement comparison which works just like the built-in string comparison.
  - toList
  - trim
  - trimStart
  - trimEnd
  - trimWith
  - trimStartWith
  - trimEndWith
    These should work just like the functions in the String module, except on Substrings.
    This makes it so trimming a string doesn't need to create an additional string object,
    it simply returns a substring which is equal to or smaller than the input substring.


TagBimap
--------

  - ofMap
  - toMap
  - tryOfMap
  - projectKeys
  - projectValues
  - forall
  - exists


Vector
------

  - findBack
  - pickBack
  - tryFindBack
  - tryPickBack
    Just like the built-in functions (e.g., findKey, pick) except they traverse "backwards" over the vector,
    i.e., from highest to lowest index. This is useful when the vector could contain multiple matching
    values and we want to choose the one with the greatest index.

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


Workflow Collections -- State.Array
-----------------------------------

  - mapReduce


TextWriter
----------

  - Add extension methods / overloads of Write and WriteLine which accept a substring value.
  - Add extension methods / overloads of Write and WriteLine which accept a vector<char> value.


Parallel (TPL) functions
------------------------

  - Array.Parallel, Vector.Parallel, Map.Parallel, Set.Parallel, IntMap.Parallel, IntSet.Parallel
    For these sub-modules, implement some relevant functions which are similar to those
    in the main module (e.g., Array) but which use the TPL and/or PLINQ under the hood.

