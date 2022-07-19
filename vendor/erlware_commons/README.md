Erlware Commons
===============

Current Status
--------------

![Tests](https://github.com/erlware/erlware_commons/workflows/EUnit/badge.svg)

Introduction
------------

Erlware commons can best be described as an extension to the stdlib
application that is distributed with Erlang. These are things that we
at Erlware have found useful for production applications but are not
included with the distribution. We hope that as things in this library
prove themselves useful, they will make their way into the main Erlang
distribution. However, whether they do or not, we hope that this
application will prove generally useful.

Goals for the project
---------------------

* Generally Useful Code
* High Quality
* Well Documented
* Well Tested

Licenses
--------

This project contains elements licensed with Apache License, Version 2.0,
as well as elements licensed with The MIT License.

You'll find license-related information in the header of specific files,
where warranted.

In cases where no such information is present refer to
[COPYING](COPYING).

Currently Available Modules/Systems
------------------------------------

### [ec_date](https://github.com/erlware/erlware_commons/blob/master/src/ec_date.erl)

This module formats erlang dates in the form {{Year, Month, Day},
{Hour, Minute, Second}} to printable strings, using (almost)
equivalent formatting rules as http://uk.php.net/date, US vs European
dates are disambiguated in the same way as
http://uk.php.net/manual/en/function.strtotime.php That is, Dates in
the m/d/y or d-m-y formats are disambiguated by looking at the
separator between the various components: if the separator is a slash
(/), then the American m/d/y is assumed; whereas if the separator is a
dash (-) or a dot (.), then the European d-m-y format is assumed. To
avoid potential ambiguity, it's best to use ISO 8601 (YYYY-MM-DD)
dates.

erlang has no concept of timezone so the following formats are not
implemented: B e I O P T Z formats c and r will also differ slightly

### [ec_file](https://github.com/erlware/erlware_commons/blob/master/src/ec_file.erl)

A set of commonly defined helper functions for files that are not
included in stdlib.

### [ec_plists](https://github.com/erlware/erlware_commons/blob/master/src/ec_plists.erl)

plists is a drop-in replacement for module <a
href="http://www.erlang.org/doc/man/lists.html">lists</a>, making most
list operations parallel. It can operate on each element in parallel,
for IO-bound operations, on sublists in parallel, for taking advantage
of multi-core machines with CPU-bound operations, and across erlang
nodes, for parallizing inside a cluster. It handles errors and node
failures. It can be configured, tuned, and tweaked to get optimal
performance while minimizing overhead.

Almost all the functions are identical to equivalent functions in
lists, returning exactly the same result, and having both a form with
an identical syntax that operates on each element in parallel and a
form which takes an optional "malt", a specification for how to
parallize the operation.

fold is the one exception, parallel fold is different from linear
fold.  This module also include a simple mapreduce implementation, and
the function runmany. All the other functions are implemented with
runmany, which is as a generalization of parallel list operations.

### [ec_semver](https://github.com/erlware/erlware_commons/blob/master/src/ec_semver.erl)

A complete parser for the [semver](http://semver.org/)
standard. Including a complete set of conforming comparison functions.

### [ec_lists](https://github.com/erlware/erlware_commons/blob/master/src/ec_lists.erl)

A set of additional list manipulation functions designed to supliment
the `lists` module in stdlib.

### [ec_talk](https://github.com/erlware/erlware_commons/blob/master/src/ec_talk.erl)

A set of simple utility functions to facilitate command line
communication with a user.

Signatures
-----------

Other languages, have built in support for **Interface** or
**signature** functionality. Java has Interfaces, SML has
Signatures. Erlang, though, doesn't currently support this model, at
least not directly. There are a few ways you can approximate it. We
have defined a mechnism called *signatures* and several modules that
to serve as examples and provide a good set of *dictionary*
signatures. More information about signatures can be found at
[signature](https://github.com/erlware/erlware_commons/blob/master/doc/signatures.md).


### [ec_dictionary](https://github.com/erlware/erlware_commons/blob/master/src/ec_dictionary.erl)

A signature that supports association of keys to values. A map cannot
contain duplicate keys; each key can map to at most one value.

### [ec_dict](https://github.com/erlware/erlware_commons/blob/master/src/ec_dict.erl)

This provides an implementation of the ec_dictionary signature using
erlang's dicts as a base. The function documentation for ec_dictionary
applies here as well.

### [ec_gb_trees](https://github.com/erlware/erlware_commons/blob/master/src/ec_gb_trees.erl)

This provides an implementation of the ec_dictionary signature using
erlang's gb_trees as a base. The function documentation for
ec_dictionary applies here as well.

### [ec_orddict](https://github.com/erlware/erlware_commons/blob/master/src/ec_orddict.erl)

This provides an implementation of the ec_dictionary signature using
erlang's orddict as a base. The function documentation for
ec_dictionary applies here as well.

### [ec_rbdict](https://github.com/erlware/erlware_commons/blob/master/src/ec_rbdict.erl)

This provides an implementation of the ec_dictionary signature using
Robert Virding's rbdict module as a base. The function documentation
for ec_dictionary applies here as well.
