=======
ExtCore
=======

.. image:: https://travis-ci.org/jack-pappas/ExtCore.png  
    :target: https://travis-ci.org/jack-pappas/ExtCore

.. image:: http://img.shields.io/nuget/v/ExtCore.svg
    :target: https://nuget.org/packages/ExtCore/

*ExtCore* is an extended core library for F#. *ExtCore* is designed to the same coding guidelines and quality standards as the F# core library (*FSharp.Core*), so the API is simple and intuitive to use.

*ExtCore* *does not* aim to be a "batteries-included" library; instead, *ExtCore* functionality is concentrated into a few key areas:

- *Pervasives*

  Top-level operators, functions, and modules providing common functionality. For example, the ``notImpl`` operator can be used to raise a ``System.NotImplementedException`` instead of the less-specific ``failwith``, and the ``Option`` module provides additional functions for working with the ``'T option`` type to complement those available in *FSharp.Core*'s ``Option`` module. 

- *Collections*

  Immutable data structures. Some of these are drop-in (almost) replacements for the ``Map`` and ``Set`` types in *FSharp.Core* which provide improved performance in specific scenarios (e.g., ``HashMap``). Others provide unique functionality to help tackle specific coding tasks (e.g., ``LazyList`` and ``LruCache``).

- *Control*

  Computation expressions (a.k.a. "workflows" or "monads") aimed at simplifying the implementation of common functional programming patterns. For example, *ExtCore* provides an ``asyncMaybe`` workflow which is similar to ``async`` in *FSharp.Core* but includes additional logic to handle cases where a computation encounters an error or fails to return a valid result.

- *Control.Collections*

  Modules which mirror the built-in collections modules in *FSharp.Core* like ``Array`` or ``Map``, but whose functions are designed to work with the computation expressions included in *ExtCore*. For example, you might have some code that uses the ``Array.map`` function; if your mapping function can sometimes fail, you can use the ``Choice.Array.map`` function instead to gracefully handle the error instead of raising an exception.


Known Bugs/Issues
=================

Known bugs or other issues are listed on the `ExtCore issues`_ page.

.. _`ExtCore issues`: https://github.com/jack-pappas/ExtCore/issues


Licensing
=========
ExtCore is licensed under the terms of the `Apache 2.0 license`_.

.. _`Apache 2.0 license`: https://www.apache.org/licenses/LICENSE-2.0.html
