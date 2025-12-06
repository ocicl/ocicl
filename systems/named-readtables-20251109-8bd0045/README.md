<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-MANUAL-20MGL-PAX-3ASECTION-29"></a>

# Named Readtables Manual

## Table of Contents

- [1 Introduction][480f]
    - [1.1 Links][a61b]
    - [1.2 Acknowledgements][ebdc]
- [2 Overview][c1e9]
    - [2.1 Notes on the API][f800]
    - [2.2 Important API idiosyncrasies][398b]
    - [2.3 Preregistered Readtables][c5dc]
    - [2.4 Examples][aae8]
- [3 Reference][4d56]

###### \[in package EDITOR-HINTS.NAMED-READTABLES with nicknames NAMED-READTABLES\]
<a id="x-28-22named-readtables-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>

- [system] **"named-readtables"**
    - _Version:_ 0.9
    - _Description:_ Library that creates a namespace for readtables akin
        to the namespace of packages.
    - _Licence:_ BSD, see LICENSE
    - _Author:_ Tobias C. Rittweiler <trittweiler@common-lisp.net>
    - _Maintainer:_ Gábor Melis <mega@retes.hu>
    - _Mailto:_ [mega@retes.hu](mailto:mega@retes.hu)
    - _Homepage:_ [http://melisgl.github.io/named-readtables](http://melisgl.github.io/named-readtables)
    - _Bug tracker:_ [https://github.com/melisgl/named-readtables/issues](https://github.com/melisgl/named-readtables/issues)
    - _Source control:_ [GIT](https://github.com/melisgl/named-readtables.git)
    - *Depends on:* mgl-pax-bootstrap

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-INTRODUCTION-20MGL-PAX-3ASECTION-29"></a>

## 1 Introduction

Named-Readtables is a library that provides a namespace for
readtables akin to the already-existing namespace of packages. In
particular:

- you can associate readtables with names, and retrieve
  readtables by names;

- you can associate source files with readtable names, and be
  sure that the right readtable is active when compiling/loading
  the file;

- similiarly, your development environment now has a chance to
  automatically determine what readtable should be active while
  processing source forms on interactive commands. (E.g. think of
  `C-c C-c` in Slime (yet to be done))

It follows that Named-Readtables is a facility for using readtables in
a localized way.

Additionally, it also attempts to become a facility for using
readtables in a *modular* way. In particular:

- it provides a macro to specify the content of a readtable at a
 glance;

- it makes it possible to use multiple inheritance between readtables.


<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-LINKS-20MGL-PAX-3ASECTION-29"></a>

### 1.1 Links

Here is the [official repository][named-readtables-repo] and the
[HTML documentation][named-readtables-doc] for the latest version.

[named-readtables-repo]: https://github.com/melisgl/named-readtables

[named-readtables-doc]: http://melisgl.github.io/mgl-pax-world/named-readtables-manual.html


<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-ACKNOWLEDGEMENTS-20MGL-PAX-3ASECTION-29"></a>

### 1.2 Acknowledgements

Thanks to Robert Goldman for making me want to write this library.

Thanks to Stephen Compall, Ariel Badichi, David Lichteblau, Bart
Botta, David Crawford, and Pascal Costanza for being early adopters,
providing comments and bugfixes.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-OVERVIEW-20MGL-PAX-3ASECTION-29"></a>

## 2 Overview

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-API-NOTES-20MGL-PAX-3ASECTION-29"></a>

### 2.1 Notes on the API

The API heavily imitates the API of packages. This has the nice
property that any experienced Common Lisper will take it up without
effort.

    DEFREADTABLE              -   DEFPACKAGE
    
    IN-READTABLE              -   IN-PACKAGE
    
    MERGE-READTABLES-INTO     -   USE-PACKAGE
    
    MAKE-READTABLE            -   MAKE-PACKAGE
    
    UNREGISTER-READTABLE      -   DELETE-PACKAGE
    
    RENAME-READTABLE          -   RENAME-PACKAGE
    
    FIND-READTABLE            -   FIND-PACKAGE
    
    READTABLE-NAME            -   PACKAGE-NAME
    
    LIST-ALL-NAMED-READTABLES -   LIST-ALL-PACKAGES


<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-API-IDIOSYNCRASIES-20MGL-PAX-3ASECTION-29"></a>

### 2.2 Important API idiosyncrasies

There are three major differences between the API of Named-Readtables,
and the API of packages.

1. Readtable names are symbols not strings.

    Time has shown that the fact that packages are named by strings
    causes severe headache because of the potential of package names
    colliding with each other.

    Hence, readtables are named by symbols lest to make the
    situation worse than it already is. Consequently, readtables
    named `CL-ORACLE:SQL-SYNTAX` and `CL-MYSQL:SQL-SYNTAX` can
    happily coexist next to each other. Or, taken to an extreme,
    `SCHEME:SYNTAX` and `ELISP:SYNTAX`.

    If, for example to duly signify the importance of your cool
    readtable hack, you really think it deserves a global name, you
    can always resort to keywords.

2. The inheritance is resolved statically, not dynamically.

    A package that uses another package will have access to all the
    other package's exported symbols, even to those that will be
    added after its definition. I.e. the inheritance is resolved at
    run-time, that is dynamically.

    Unfortunately, we cannot do the same for readtables in a
    portable manner.

    Therefore, we do not talk about "using" another readtable but
    about "merging" the other readtable's definition into the
    readtable we are going to define. I.e. the inheritance is
    resolved once at definition time, that is statically.

    (Such merging can more or less be implemented portably albeit at
    a certain cost. Most of the time, this cost manifests itself at
    the time a readtable is defined, i.e. once at compile-time, so
    it may not bother you. Nonetheless, we provide extra support for
    Sbcl, ClozureCL, and AllegroCL at the moment. Patches for your
    implementation of choice are welcome, of course.)

3. [`DEFREADTABLE`][6a02] does not have compile-time effects.

    If you define a package via [`DEFPACKAGE`][9b43], you can make that
    package the currently active package for the subsequent
    compilation of the same file via [`IN-PACKAGE`][125e]. The same is,
    however, not true for `DEFREADTABLE` and [`IN-READTABLE`][ee2d] for the
    following reason:

    It's unlikely that the need for special reader-macros arises for
    a problem which can be solved in just one file. Most often,
    you're going to define the reader macro functions, and set up
    the corresponding readtable in an extra file.

    If `DEFREADTABLE` had compile-time effects, you'd have to wrap
    each definition of a reader-macro function in an [`EVAL-WHEN`][9c9c] to
    make its definition available at compile-time. Because that's
    simply not the common case, `DEFREADTABLE` does not have a
    compile-time effect.

    If you want to use a readtable within the same file as its
    definition, wrap the `DEFREADTABLE` and the reader-macro function
    definitions in an explicit `EVAL-WHEN`.


<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-PREREGISTERED-20MGL-PAX-3ASECTION-29"></a>

### 2.3 Preregistered Readtables

- `NIL`, `:STANDARD`, and `:COMMON-LISP` designate the
*standard readtable*.

- `:MODERN` designates a *case-preserving* *standard-readtable*.

- `:CURRENT` designates the *current readtable*.


<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-EXAMPLES-20MGL-PAX-3ASECTION-29"></a>

### 2.4 Examples

```commonlisp
(defreadtable elisp:syntax
   (:merge :standard)
   (:macro-char #\? #'elisp::read-character-literal t)
   (:macro-char #\[ #'elisp::read-vector-literal t)
   ...
   (:case :preserve))

(defreadtable scheme:syntax
   (:merge :standard)
   (:macro-char #\[ #'(lambda (stream char)
                         (read-delimited-list #\] stream)))
   (:macro-char #\# :dispatch)
   (:dispatch-macro-char #\# #\t #'scheme::read-#t)
   (:dispatch-macro-char #\# #\f #'scheme::read-#f)
   ...
   (:case :preserve))

(in-readtable elisp:syntax)

...

(in-readtable scheme:syntax)

...
```


<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-REFERENCE-20MGL-PAX-3ASECTION-29"></a>

## 3 Reference

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3ADEFREADTABLE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFREADTABLE** *NAME &BODY OPTIONS*

    Define a new named readtable, whose name is given by the symbol `NAME`.
    Or, if a readtable is already registered under that name, redefine
    that one.
    
    The readtable can be populated using the following `OPTIONS`:
    
    - If the first element of `OPTIONS` is a string then it is associated
      with the readtable as in `(SETF (DOCUMENTATION NAME 'READTABLE)
      DOCSTRING)`.
    
    - `(:MERGE READTABLE-DESIGNATORS+)`
    
        Merge the macro character definitions from the readtables
        designated into the new readtable being defined as per
        [`MERGE-READTABLES-INTO`][1625]. The copied options are
        `:DISPATCH-MACRO-CHAR`, `:MACRO-CHAR` and `:SYNTAX-FROM`, but not
        [`READTABLE-CASE`][48f1].
    
        If no `:MERGE` clause is given, an empty readtable is used. See
        [`MAKE-READTABLE`][fd4c].
    
    - `(:FUSE READTABLE-DESIGNATORS+)`
    
        Like `:MERGE` except:
    
        Error conditions of type [`READER-MACRO-CONFLICT`][eab7] that are signaled
        during the merge operation will be silently *continued*. It
        follows that reader macros in earlier entries will be
        overwritten by later ones. For backward compatibility, `:FUZE` is
        accepted as an alias of `:FUSE`.
    
    - `(:DISPATCH-MACRO-CHAR MACRO-CHAR SUB-CHAR FUNCTION)`
    
        Define a new sub character `SUB-CHAR` for the dispatching macro
        character `MACRO-CHAR`, per [`SET-DISPATCH-MACRO-CHARACTER`][5b1b]. You
        probably have to define `MACRO-CHAR` as a dispatching macro
        character by the following option first.
    
    - `(:MACRO-CHAR MACRO-CHAR FUNCTION [NON-TERMINATING-P])`
    
        Define a new macro character in the readtable, per
        [`SET-MACRO-CHARACTER`][a8c1]. If `FUNCTION` is the keyword
        `:DISPATCH`, `MACRO-CHAR` is made a dispatching macro character,
        per [`MAKE-DISPATCH-MACRO-CHARACTER`][1ee4].
    
    - `(:SYNTAX-FROM FROM-READTABLE-DESIGNATOR FROM-CHAR TO-CHAR)`
    
        Set the character syntax of `TO-CHAR` in the readtable being
        defined to the same syntax as `FROM-CHAR` as per
        [`SET-SYNTAX-FROM-CHAR`][3867].
    
    - `(:CASE CASE-MODE)`
    
        Defines the *case sensitivity mode* of the resulting readtable.
    
    Any number of option clauses may appear. The options are grouped by
    their type, but in each group the order the options appeared
    textually is preserved. The following groups exist and are executed
    in the following order: `:MERGE` and `:FUSE` (one group), `:CASE`,
    `:MACRO-CHAR` and `:DISPATCH-MACRO-CHAR` (one group), finally
    `:SYNTAX-FROM`.
    
    Notes:
    
    The readtable is defined at load-time. If you want to have it
    available at compilation time -- say to use its reader-macros in the
    same file as its definition -- you have to wrap the `DEFREADTABLE`
    form in an explicit [`EVAL-WHEN`][9c9c].
    
    On redefinition, the target readtable is made empty first before
    it's refilled according to the clauses.
    
    `NIL`, `:STANDARD`, `:COMMON-LISP`, `:MODERN`, and `:CURRENT` are
    preregistered readtable names.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AIN-READTABLE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **IN-READTABLE** *NAME*

    Set [`*READTABLE*`][b79a] to the readtable referred to by the symbol `NAME`.
    Return the readtable.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AMAKE-READTABLE-20FUNCTION-29"></a>

- [function] **MAKE-READTABLE** *&OPTIONAL (NAME NIL NAME-SUPPLIED-P) &KEY MERGE*

    Creates and returns a new readtable under the specified
    `NAME`.
    
    `MERGE` takes a list of [`NAMED-READTABLE-DESIGNATOR`][4e61]s and specifies the
    readtables the new readtable is created from. (See the `:MERGE` clause
    of [`DEFREADTABLE`][6a02] for details.)
    
    If `MERGE` is `NIL`, an empty readtable is used instead.
    
    If `NAME` is not given, an anonymous empty readtable is returned.
    
    Notes:
    
    An empty readtable is a readtable where each character's syntax is
    the same as in the *standard readtable* except that each macro
    character has been made a constituent. Basically: whitespace stays
    whitespace, everything else is constituent.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AMERGE-READTABLES-INTO-20FUNCTION-29"></a>

- [function] **MERGE-READTABLES-INTO** *RESULT-READTABLE &REST NAMED-READTABLES*

    Copy macro character definitions of each readtable in
    `NAMED-READTABLES` into `RESULT-READTABLE`.
    
    If a macro character appears in more than one of the readtables,
    i.e. if a conflict is discovered during the merge, an error of type
    [`READER-MACRO-CONFLICT`][eab7] is signaled.
    
    The copied options are `:DISPATCH-MACRO-CHAR`, `:MACRO-CHAR` and
    `:SYNTAX-FROM`, but not [`READTABLE-CASE`][48f1].

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AFIND-READTABLE-20FUNCTION-29"></a>

- [function] **FIND-READTABLE** *NAME*

    Looks for the readtable specified by `NAME` and returns it if it is
    found. Returns `NIL` otherwise.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AENSURE-READTABLE-20FUNCTION-29"></a>

- [function] **ENSURE-READTABLE** *NAME &OPTIONAL (DEFAULT NIL DEFAULT-P)*

    Looks up the readtable specified by `NAME` and returns it if it's found.
    If it is not found, it registers the readtable designated by `DEFAULT`
    under the name represented by `NAME`; or if no default argument is
    given, it signals an error of type [`READTABLE-DOES-NOT-EXIST`][02bf]
    instead.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3ARENAME-READTABLE-20FUNCTION-29"></a>

- [function] **RENAME-READTABLE** *OLD-NAME NEW-NAME*

    Replaces the associated name of the readtable designated by
    `OLD-NAME` with `NEW-NAME`. If a readtable is already registered under
    `NEW-NAME`, an error of type [`READTABLE-DOES-ALREADY-EXIST`][78ad] is
    signaled.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREADTABLE-NAME-20FUNCTION-29"></a>

- [function] **READTABLE-NAME** *NAMED-READTABLE*

    Returns the name of the readtable designated by `NAMED-READTABLE`,
    or `NIL`.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREGISTER-READTABLE-20FUNCTION-29"></a>

- [function] **REGISTER-READTABLE** *NAME READTABLE*

    Associate `READTABLE` with `NAME`. Returns the readtable.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AUNREGISTER-READTABLE-20FUNCTION-29"></a>

- [function] **UNREGISTER-READTABLE** *NAMED-READTABLE*

    Remove the association of `NAMED-READTABLE`. Returns `T` if successfull,
    `NIL` otherwise.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3ACOPY-NAMED-READTABLE-20FUNCTION-29"></a>

- [function] **COPY-NAMED-READTABLE** *NAMED-READTABLE*

    Like [`COPY-READTABLE`][6d9f] but takes a [`NAMED-READTABLE-DESIGNATOR`][4e61] as argument.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3ALIST-ALL-NAMED-READTABLES-20FUNCTION-29"></a>

- [function] **LIST-ALL-NAMED-READTABLES**

    Returns a list of all registered readtables. The returned list is
    guaranteed to be fresh, but may contain duplicates.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3ANAMED-READTABLE-DESIGNATOR-20TYPE-29"></a>

- [type] **NAMED-READTABLE-DESIGNATOR**

    Either a symbol or a readtable itself.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREADTABLE-ERROR-20CONDITION-29"></a>

- [condition] **READTABLE-ERROR** *[ERROR][d162]*

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREADER-MACRO-CONFLICT-20CONDITION-29"></a>

- [condition] **READER-MACRO-CONFLICT** *[READTABLE-ERROR][371c]*

    Continuable.
    
    This condition is signaled during the merge process if a reader
    macro (be it a macro character or the sub character of a dispatch
    macro character) is present in the both source and the target
    readtable and the two respective reader macro functions differ.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREADTABLE-DOES-ALREADY-EXIST-20CONDITION-29"></a>

- [condition] **READTABLE-DOES-ALREADY-EXIST** *[READTABLE-ERROR][371c]*

    Continuable.

<a id="x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREADTABLE-DOES-NOT-EXIST-20CONDITION-29"></a>

- [condition] **READTABLE-DOES-NOT-EXIST** *[READTABLE-ERROR][371c]*

  [02bf]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREADTABLE-DOES-NOT-EXIST-20CONDITION-29 "EDITOR-HINTS.NAMED-READTABLES:READTABLE-DOES-NOT-EXIST CONDITION"
  [125e]: http://www.lispworks.com/documentation/HyperSpec/Body/m_in_pkg.htm "IN-PACKAGE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [1625]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3AMERGE-READTABLES-INTO-20FUNCTION-29 "EDITOR-HINTS.NAMED-READTABLES:MERGE-READTABLES-INTO FUNCTION"
  [1ee4]: http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_dis.htm "MAKE-DISPATCH-MACRO-CHARACTER (MGL-PAX:CLHS FUNCTION)"
  [371c]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREADTABLE-ERROR-20CONDITION-29 "EDITOR-HINTS.NAMED-READTABLES:READTABLE-ERROR CONDITION"
  [3867]: http://www.lispworks.com/documentation/HyperSpec/Body/f_set_sy.htm "SET-SYNTAX-FROM-CHAR (MGL-PAX:CLHS FUNCTION)"
  [398b]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-API-IDIOSYNCRASIES-20MGL-PAX-3ASECTION-29 "Important API idiosyncrasies"
  [480f]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-INTRODUCTION-20MGL-PAX-3ASECTION-29 "Introduction"
  [48f1]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rdtabl.htm "READTABLE-CASE (MGL-PAX:CLHS FUNCTION)"
  [4d56]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-REFERENCE-20MGL-PAX-3ASECTION-29 "Reference"
  [4e61]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3ANAMED-READTABLE-DESIGNATOR-20TYPE-29 "EDITOR-HINTS.NAMED-READTABLES:NAMED-READTABLE-DESIGNATOR TYPE"
  [5b1b]: http://www.lispworks.com/documentation/HyperSpec/Body/f_set__1.htm "SET-DISPATCH-MACRO-CHARACTER (MGL-PAX:CLHS FUNCTION)"
  [6a02]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3ADEFREADTABLE-20MGL-PAX-3AMACRO-29 "EDITOR-HINTS.NAMED-READTABLES:DEFREADTABLE MGL-PAX:MACRO"
  [6d9f]: http://www.lispworks.com/documentation/HyperSpec/Body/f_cp_rdt.htm "COPY-READTABLE (MGL-PAX:CLHS FUNCTION)"
  [78ad]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREADTABLE-DOES-ALREADY-EXIST-20CONDITION-29 "EDITOR-HINTS.NAMED-READTABLES:READTABLE-DOES-ALREADY-EXIST CONDITION"
  [9b43]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defpkg.htm "DEFPACKAGE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [9c9c]: http://www.lispworks.com/documentation/HyperSpec/Body/s_eval_w.htm "EVAL-WHEN (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [a61b]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-LINKS-20MGL-PAX-3ASECTION-29 "Links"
  [a8c1]: http://www.lispworks.com/documentation/HyperSpec/Body/f_set_ma.htm "SET-MACRO-CHARACTER (MGL-PAX:CLHS FUNCTION)"
  [aae8]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-EXAMPLES-20MGL-PAX-3ASECTION-29 "Examples"
  [b79a]: http://www.lispworks.com/documentation/HyperSpec/Body/v_rdtabl.htm "*READTABLE* (MGL-PAX:CLHS VARIABLE)"
  [c1e9]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-OVERVIEW-20MGL-PAX-3ASECTION-29 "Overview"
  [c5dc]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-PREREGISTERED-20MGL-PAX-3ASECTION-29 "Preregistered Readtables"
  [d162]: http://www.lispworks.com/documentation/HyperSpec/Body/e_error.htm "ERROR (MGL-PAX:CLHS CONDITION)"
  [eab7]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREADER-MACRO-CONFLICT-20CONDITION-29 "EDITOR-HINTS.NAMED-READTABLES:READER-MACRO-CONFLICT CONDITION"
  [ebdc]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-ACKNOWLEDGEMENTS-20MGL-PAX-3ASECTION-29 "Acknowledgements"
  [ee2d]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3AIN-READTABLE-20MGL-PAX-3AMACRO-29 "EDITOR-HINTS.NAMED-READTABLES:IN-READTABLE MGL-PAX:MACRO"
  [f800]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-API-NOTES-20MGL-PAX-3ASECTION-29 "Notes on the API"
  [fd4c]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3AMAKE-READTABLE-20FUNCTION-29 "EDITOR-HINTS.NAMED-READTABLES:MAKE-READTABLE FUNCTION"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
