ppx_const
=========

This is an OCaml language extension implementing an `if%const` statement. The `if%const` is evaluated at compile time, and the appropriate clause substituted without the ignored clause being fully compiled. This allows you to avoid consequences such as module inclusion or type inference changes which would otherwise have resulted from the ignored clause. 

In other words, ppx_const works like `#if` in the C preprocessor, but is implemented entirely within the OCaml language using the ppx mechanism. In conjunction with [ppx_getenv](https://github.com/whitequark/ppx_getenv), it provides a lightweight alternative to cppo.

This software was written by Andi McClure <andi.m.mcclure@gmail.com> based on whitequark's ppx_getenv sample.

Usage
-----

ppx_const may be invoked with either of the following:

    if%const COND then A else B
    if%const COND then A

COND must be one of the following:

* `true`
* `false`
* An expression consisting of two literals and either the `<>` or `=` operator.

COND may also contain extension nodes (including `if%const`s) as long as they evaluate to a constant expression by the time ppx_const sees them.

A and B are not required to be of the same type. Like with normal `if`, the return type of `if%const false then X` is unit.

License
-------

[Creative Commons Zero](LICENSE.txt) ("public domain or equivalent")
