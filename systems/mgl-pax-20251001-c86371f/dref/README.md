<a id="x-28DREF-3A-40DREF-MANUAL-20MGL-PAX-3ASECTION-29"></a>

# DRef Manual

## Table of Contents

- [1 Links and Systems][a584]
- [2 Introduction][ad80]
- [3 References][14ea]
    - [3.1 Dissecting References][6f15]
    - [3.2 References Glossary][852d]
- [4 `DTYPE`s][a459]
- [5 Listing Definitions][e1d4]
- [6 Basic Operations][662d]
- [7 Basic Locative Types][1d1d]
    - [7.1 Locatives for Variables][462c]
    - [7.2 Locatives for Macros][d45d]
    - [7.3 Locatives for Functions and Methods][1d59]
    - [7.4 Locatives for Types and Declarations][7a04]
    - [7.5 Locatives for the Condition System][408d]
    - [7.6 Locatives for Packages and Readtables][c339]
    - [7.7 Locatives for Unknown Definitions][58f1]
    - [7.8 Locatives for DRef Constructs][da93]
- [8 Extending DRef][68fb]
    - [8.1 Extension Tutorial][9d60]
    - [8.2 Locative Type Hierarchy][c9ab]
    - [8.3 Defining Locative Types][f494]
        - [8.3.1 Symbol Locatives][59c9]
    - [8.4 Extending `LOCATE`][4c16]
        - [8.4.1 Initial Definition][87fc]
        - [8.4.2 Canonicalization][9383]
        - [8.4.3 Defining Lookups, Locators and Casts][adc7]
    - [8.5 Extending Everything Else][793d]
        - [8.5.1 Definition Properties][c9de]
    - [8.6 `DREF-CLASS`es][6354]
    - [8.7 Source Locations][a078]

###### \[in package DREF\]
<a id="x-28DREF-3A-40LINKS-AND-SYSTEMS-20MGL-PAX-3ASECTION-29"></a>

## 1 Links and Systems

Here is the [official
repository](https://github.com/melisgl/mgl-pax/dref) and the [HTML
documentation](http://melisgl.github.io/mgl-pax-world/dref-manual.html)
for the latest version.

DRef is bundled in the same repository with [PAX][2415],
the documentation system.

<a id="x-28-22dref-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>

- [system] **"dref"**

    - _Version:_ 0.4.4
    - _Description:_ Reify definitions, provide portable access to
        docstrings and source locations in an extensible framework.
    - _Long Description:_ [`DEFUN`][f472] defines a first-class object: a `FUNCTION`([`0`][119e] [`1`][81f7]).
        [`DEFVAR`][7334] does not. This library provides a way to refer to all
        definitions and smooths over the differences between
        implementations. This system has minimal dependencies. It autoloads
        the [`dref/full`][0c7e] `ASDF:SYSTEM`, which depends Alexandria and Swank.
    - _Licence:_ MIT, see COPYING.
    - _Author:_ Gábor Melis
    - _Mailto:_ [mega@retes.hu](mailto:mega@retes.hu)
    - _Homepage:_ [http://github.com/melisgl/mgl-pax/tree/master/dref](http://github.com/melisgl/mgl-pax/tree/master/dref)
    - _Bug tracker:_ [https://github.com/melisgl/mgl-pax/issues](https://github.com/melisgl/mgl-pax/issues)
    - _Source control:_ [GIT](https://github.com/melisgl/mgl-pax.git)
    - *Depends on:* autoload, mgl-pax-bootstrap, named-readtables, pythonic-string-reader
    - *Defsystem depends on:* mgl-pax.asdf

<a id="x-28-22dref-2Ffull-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>

- [system] **"dref/full"**

    - _Description:_ `DREF`([`0`][d930] [`1`][7e92]) with everything loaded. There should be no need
        to explicitly load this system (or depend on it) as it is autoloaded
        as necessary by all publicly accessible functionality in `DREF`.
        
        However, to get the dependencies, install this system.
    - *Depends on:* alexandria, [dref][021a], swank(?)
    - *Defsystem depends on:* mgl-pax.asdf

<a id="x-28DREF-3A-40INTRODUCTION-20MGL-PAX-3ASECTION-29"></a>

## 2 Introduction

*What if definitions were first-class objects?*

Some [defining forms][23a8] do not create first-class
[objects][3c8a]. For example, [`DEFUN`][f472] creates
[`FUNCTION`][119e] objects, but [`DEFVAR`][7334] does not create variable
objects as no such thing exists. The main purpose of this library is
to fill this gap with the introduction of [`DREF`][d930] objects:

```common-lisp
(defvar *my-var* nil
  "This is my var.")
(dref '*my-var* 'variable)
==> #<DREF *MY-VAR* VARIABLE>
```

`DREF`s just package up a [name][5fc4] (`*MY-VAR*`) and a
[locative][7ac8] ([`VARIABLE`][6c83]) then check that the definition
actually exists:

```common-lisp
(dref 'junk 'variable)
.. debugger invoked on LOCATE-ERROR:
..   Could not locate JUNK VARIABLE.
```

The [Basic Operations][662d] on definitions in DRef are [`ARGLIST`][e6bd], [`DOCSTRING`][affc]
and [`SOURCE-LOCATION`][32da].

```common-lisp
(docstring (dref '*my-var* 'variable))
=> "This is my var."
```

For definitions associated with objects, the definition can be
[`LOCATE`][8f19]d from the object:

```common-lisp
(locate #'print)
==> #<DREF PRINT FUNCTION>
```

These objects designate their definitions, so `(DOCSTRING #'PRINT)`
works. Extending DRef and these operations is possible through
[Defining Locative Types][f494]. It is also possible to define
new operations. For example, [PAX][2415] makes
[`PAX:DOCUMENT`][432c] extensible through [`PAX:DOCUMENT-OBJECT*`][8269].

Finally, existing definitions can be queried with [`DEFINITIONS`][e196] and
[`DREF-APROPOS`][65b4]:

```common-lisp
(definitions 'dref-ext:arglist*)
==> (#<DREF ARGLIST* GENERIC-FUNCTION>
-->  #<DREF ARGLIST* (METHOD (MGL-PAX::GO-DREF))>
-->  #<DREF ARGLIST* (METHOD (LAMBDA-DREF))>
-->  #<DREF ARGLIST* (METHOD (TYPE-DREF))>
-->  #<DREF ARGLIST* (METHOD (METHOD-DREF))>
-->  #<DREF ARGLIST* (METHOD (FUNCTION-DREF))>
-->  #<DREF ARGLIST* (METHOD (COMPILER-MACRO-DREF))>
-->  #<DREF ARGLIST* (METHOD (MACRO-DREF))>
-->  #<DREF ARGLIST* (METHOD (SETF-DREF))> #<DREF ARGLIST* (METHOD (T))>
-->  #<DREF ARGLIST* (METHOD (DREF))>
-->  #<DREF ARGLIST* (UNKNOWN
-->                   (DECLAIM ARGLIST*
-->                            FTYPE))>)
```

```common-lisp
(dref-apropos 'locate-error :package :dref)
==> (#<DREF LOCATE-ERROR CONDITION> #<DREF LOCATE-ERROR FUNCTION>)

(dref-apropos "ate-err" :package :dref :external-only t)
==> (#<DREF LOCATE-ERROR CONDITION> #<DREF LOCATE-ERROR FUNCTION>)
```


<a id="x-28DREF-3A-40REFERENCES-20MGL-PAX-3ASECTION-29"></a>

## 3 References

After the [Introduction][ad80], here we get into the details. Of special
interest are:

- The [`XREF`][cda7] function to construct an arbitrary [reference][43bd] without any
  checking of validity.

- [`LOCATE`][8f19] and [`DREF`][7e92] to look up the [definition][2143] of an
  object (e.g `#'PRINT`) or a [reference][43bd] (e.g. `(XREF 'PRINT
  'FUNCTION)`).

- [`RESOLVE`][63b4] to find the first-class (non-[`XREF`][1538]) object the
  definition refers to, if any.

The [Basic Operations][662d] ([`ARGLIST`][e6bd], [`DOCSTRING`][affc], [`SOURCE-LOCATION`][32da]) know how to
deal with references (discussed in the [Extending DRef][68fb]).

<a id="x-28DREF-3AXREF-20CLASS-29"></a>

- [class] **XREF**

    An `XREF` (cross-reference) is a [reference][43bd]. It may
    represent some kind of [definition][2143] of its [name][5fc4] in the context given
    by its [locative][7ac8]. The definition may not exist and the locative may
    even be [invalid][7ac8]. The subclass [`DREF`][d930] represents
    definitions that exist.

<a id="x-28DREF-3AXREF-20FUNCTION-29"></a>

- [function] **XREF** *NAME LOCATIVE*

    A shorthand for `(MAKE-INSTANCE 'XREF :NAME NAME :LOCATIVE LOCATIVE)`
    to create [`XREF`][1538] objects. It does no error checking: the
    [`LOCATIVE-TYPE`][97ba] of `LOCATIVE-TYPE` need not be defined, and the
    [`LOCATIVE-ARGS`][2444] need not be valid. Use [`LOCATE`][8f19] or the [`DREF`][7e92] function to
    create [`DREF`][d930] objects.

<a id="x-28DREF-3AXREF-3D-20FUNCTION-29"></a>

- [function] **XREF=** *XREF1 XREF2*

    See if `XREF1` and `XREF2` have the same [`XREF-NAME`][5447] and [`XREF-LOCATIVE`][a70d]
    under [`EQUAL`][3fb5]. Comparing like this makes most sense for
    [`DREF`][d930]s. However, two [`XREF`][1538]s different under `XREF=`
    may denote the same [`DREF`][d930]s.

<a id="x-28DREF-3ADREF-20CLASS-29"></a>

- [class] **DREF** *[XREF][1538]*

    `DREF`s can be thought of as [definition][2143]s that
    actually exist, although changes in the system can invalidate
    them (for example, a `DREF` to a function definition can be
    invalidated by [`FMAKUNBOUND`][609c]). `DREF`s must be created with [`LOCATE`][8f19] or
    the [`DREF`][7e92] function.
    
    Two `DREF`s created in the same [dynamic environment][62e7] denote the
    same thing if and only if they are [`XREF=`][0617].

<a id="x-28DREF-3ALOCATE-20FUNCTION-29"></a>

- [function] **LOCATE** *OBJECT &OPTIONAL (ERRORP T)*

    Return a [`DREF`][d930] representing the [definition][2143] of `OBJECT`.
    
    `OBJECT` must be a supported first-class object, a `DREF`, or an
    [`XREF`][1538]:
    
    ```common-lisp
    (locate #'print)
    ==> #<DREF PRINT FUNCTION>
    ```
    
    ```common-lisp
    (locate (locate #'print))
    ==> #<DREF PRINT FUNCTION>
    ```
    
    ```common-lisp
    (locate (xref 'print 'function))
    ==> #<DREF PRINT FUNCTION>
    ```
    
    When `OBJECT` is a `DREF`, it is simply returned.
    
    Else, a `LOCATE-ERROR`([`0`][6334] [`1`][6932]) is signalled if `OBJECT` is an `XREF` with an
    invalid [locative][7ac8], or if no corresponding definition is found. If
    `ERRORP` is `NIL`, then `NIL` is returned instead.
    
    ```common-lisp
    (locate (xref 'no-such-function 'function))
    .. debugger invoked on LOCATE-ERROR:
    ..   Could not locate NO-SUCH-FUNCTION FUNCTION.
    ..   NO-SUCH-FUNCTION does not name a function.
    ```
    
    ```common-lisp
    (locate (xref 'print '(function xxx)))
    .. debugger invoked on LOCATE-ERROR:
    ..   Could not locate PRINT #'XXX.
    ..   Bad arguments (XXX) for locative FUNCTION with lambda list NIL.
    ```
    
    ```common-lisp
    (locate "xxx")
    .. debugger invoked on LOCATE-ERROR:
    ..   Could not locate "xxx".
    ```
    
    Use the [`XREF`][cda7] function to construct an `XREF` without error checking.
    
    See [Extending `LOCATE`][4c16].

<a id="x-28DREF-3ADREF-20FUNCTION-29"></a>

- [function] **DREF** *NAME LOCATIVE &OPTIONAL (ERRORP T)*

    Shorthand for `(LOCATE (XREF NAME LOCATIVE) ERRORP)`.

<a id="x-28DREF-3ARESOLVE-20FUNCTION-29"></a>

- [function] **RESOLVE** *OBJECT &OPTIONAL (ERRORP T)*

    If `OBJECT` is an [`XREF`][1538], then return the first-class object
    associated with its definition if any. Return `OBJECT` if it's not an
    `XREF`. Thus, the value returned is never an `XREF`. The second return
    value is whether resolving succeeded.
    
    ```common-lisp
    (resolve (dref 'print 'function))
    ==> #<FUNCTION PRINT>
    => T
    ```
    
    ```common-lisp
    (resolve #'print)
    ==> #<FUNCTION PRINT>
    => T
    ```
    
    If `OBJECT` is an `XREF`, and the definition for it cannot be [`LOCATE`][8f19]d,
    then `LOCATE-ERROR`([`0`][6334] [`1`][6932]) is signalled.
    
    ```common-lisp
    (resolve (xref 'undefined 'variable))
    .. debugger invoked on LOCATE-ERROR:
    ..   Could not locate UNDEFINED VARIABLE.
    ```
    
    If there is a definition, but there is no first-class object
    corresponding to it, then `RESOLVE-ERROR`([`0`][58ba] [`1`][7825]) is signalled or `NIL` is
    returned depending on `ERRORP`:
    
    ```common-lisp
    (resolve (dref '*print-length* 'variable))
    .. debugger invoked on RESOLVE-ERROR:
    ..   Could not resolve *PRINT-LENGTH* VARIABLE.
    ```
    
    ```common-lisp
    (resolve (dref '*print-length* 'variable) nil)
    => NIL
    => NIL
    ```
    
    `RESOLVE` is a partial inverse of `LOCATE`: if a [`DREF`][d930] is
    `RESOLVE`able, then `LOCATE`ing the object it resolves to recovers the
    `DREF` equivalent to the original ([`XREF=`][0617] and of the same type but not
    [`EQ`][5a82]).
    
    Can be extended via [`RESOLVE*`][d3b3].

<a id="x-28DREF-EXT-3ALOCATE-ERROR-20CONDITION-29"></a>

- [condition] **LOCATE-ERROR** *[ERROR][d162]*

    Signalled by [`LOCATE`][8f19] when the definition cannot be
    found, and `ERRORP` is true.

<a id="x-28DREF-EXT-3ARESOLVE-ERROR-20CONDITION-29"></a>

- [condition] **RESOLVE-ERROR** *[ERROR][d162]*

    Signalled by [`RESOLVE`][63b4] when the object defined cannot
    be returned, and `ERRORP` is true.

<a id="x-28DREF-3A-40DISSECTING-REFERENCES-20MGL-PAX-3ASECTION-29"></a>

### 3.1 Dissecting References

<a id="x-28DREF-3AXREF-NAME-20-28MGL-PAX-3AREADER-20DREF-3AXREF-29-29"></a>

- [reader] **XREF-NAME** *[XREF][1538] (:NAME)*

    The [name][5fc4] of the reference.

<a id="x-28DREF-3AXREF-LOCATIVE-20-28MGL-PAX-3AREADER-20DREF-3AXREF-29-29"></a>

- [reader] **XREF-LOCATIVE** *[XREF][1538] (:LOCATIVE)*

    The [locative][7ac8] of the reference.
    
    The locative is normalized by replacing single-element lists with
     their only element:
    
    ```common-lisp
    (xref 'print 'function)
    ==> #<XREF PRINT FUNCTION>
    ```
    
    ```common-lisp
    (xref 'print '(function))
    ==> #<XREF PRINT FUNCTION>
    ```

<a id="x-28DREF-3ADREF-NAME-20-28MGL-PAX-3AREADER-20DREF-3ADREF-29-29"></a>

- [reader] **DREF-NAME** *[DREF][d930]*

    The same as [`XREF-NAME`][5447], but only works on
    [`DREF`][d930]s. Use it as a statement of intent.

<a id="x-28DREF-3ADREF-LOCATIVE-20-28MGL-PAX-3AREADER-20DREF-3ADREF-29-29"></a>

- [reader] **DREF-LOCATIVE** *[DREF][d930]*

    The same as [`XREF-LOCATIVE`][a70d], but only works on
    [`DREF`][d930]s. Use it as a statement of intent.

<a id="x-28DREF-3ADREF-ORIGIN-20-28MGL-PAX-3AREADER-20DREF-3ADREF-29-29"></a>

- [reader] **DREF-ORIGIN** *[DREF][d930]*

    The object from which [`LOCATE`][8f19] constructed this
    [`DREF`][d930]. `DREF-ORIGIN` may have [presentation][ade6] arguments, which
    are not included in [`LOCATIVE-ARGS`][2444] as is the case with the `INITFORM`
    argument of the [`VARIABLE`][6c83] locative:
    
    ```common-lisp
    (dref '*standard-output* '(variable "see-below"))
    ==> #<DREF *STANDARD-OUTPUT* VARIABLE>
    ```
    
    ```common-lisp
    (dref-origin (dref '*standard-output* '(variable "see-below")))
    ==> #<XREF *STANDARD-OUTPUT* (VARIABLE "see-below")>
    ```
    
    The `INITFORM` argument overrides the global binding of
    [`*STANDARD-OUTPUT*`][e7ee] when it's [`PAX:DOCUMENT`][432c]ed:
    
    ```common-lisp
    (first-line
     (pax:document (dref '*standard-output* '(variable "see-below"))
                   :stream nil))
    => "- [variable] *STANDARD-OUTPUT* \"see-below\""
    ```

<a id="x-28DREF-EXT-3ALOCATIVE-TYPE-20FUNCTION-29"></a>

- [function] **LOCATIVE-TYPE** *LOCATIVE*

    Return [locative type][a11d] of the [locative][7ac8] `LOCATIVE`. This is the first
    element of `LOCATIVE` if it's a list. If it's a symbol, it's that
    symbol itself.

<a id="x-28DREF-EXT-3ALOCATIVE-ARGS-20FUNCTION-29"></a>

- [function] **LOCATIVE-ARGS** *LOCATIVE*

    Return the [`REST`][fe9f] of [locative][7ac8] `LOCATIVE` if it's a list. If it's a symbol,
    then return `NIL`. See [locative][7ac8].

The following convenience functions are compositions of
{[`LOCATIVE-TYPE`][97ba], [`LOCATIVE-ARGS`][2444]} and {[`XREF-LOCATIVE`][a70d],
[`DREF-LOCATIVE`][cc049]}.

<a id="x-28DREF-3AXREF-LOCATIVE-TYPE-20FUNCTION-29"></a>

- [function] **XREF-LOCATIVE-TYPE** *XREF*

<a id="x-28DREF-3AXREF-LOCATIVE-ARGS-20FUNCTION-29"></a>

- [function] **XREF-LOCATIVE-ARGS** *XREF*

<a id="x-28DREF-3ADREF-LOCATIVE-TYPE-20FUNCTION-29"></a>

- [function] **DREF-LOCATIVE-TYPE** *DREF*

<a id="x-28DREF-3ADREF-LOCATIVE-ARGS-20FUNCTION-29"></a>

- [function] **DREF-LOCATIVE-ARGS** *DREF*

<a id="x-28DREF-3A-40REFERENCES-GLOSSARY-20MGL-PAX-3ASECTION-29"></a>

### 3.2 References Glossary

<a id="x-28DREF-3A-40REFERENCE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **reference**

    A reference is a [name][5fc4] plus a [locative][7ac8], and it identifies a
    possible definition. References are of class [`XREF`][1538]. When a reference
    is a [`DREF`][d930], it may also be called a definition.

<a id="x-28DREF-3A-40DEFINITION-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **definition**

    A definition is a [reference][43bd] that identifies a concrete definition.
    Definitions are of class [`DREF`][d930]. A definition [`RESOLVE`][63b4]s to the
    first-class object associated with the definition if such a thing
    exists, and [`LOCATE`][8f19] on this object returns the canonical `DREF` object
    that's unique under [`XREF=`][0617].
    
    The kind of a definition is given by its [locative type][a11d]. There is at
    most one definition for any given [name][5fc4] and locative type.
    Equivalently, there can be no two definitions of the same [`DREF-NAME`][1e36]
    and [`DREF-LOCATIVE-TYPE`][a22e] but different [`DREF-LOCATIVE-ARGS`][0976].

<a id="x-28DREF-3A-40NAME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **name**

    Names are symbols, strings and nested lists of the previous, which
    name [functions][ba62], [types][926d],
    [packages][4dd7], etc. Together with [locative][7ac8]s, they
    form [reference][43bd]s.
    
    See [`XREF-NAME`][5447] and [`DREF-NAME`][1e36].

<a id="x-28DREF-3A-40LOCATIVE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **locative**

    Locatives specify a *type* of definition such as
    [`FUNCTION`][ba62] or [`VARIABLE`][6c83]. Together with [name][5fc4]s,
    they form [reference][43bd]s.
    
    In their compound form, locatives may have arguments (see
    [`LOCATIVE-ARGS`][2444]) as in `(METHOD (NUMBER))`. In fact, their atomic form
    is shorthand for the common no-argument case: that is, `FUNCTION` is
    equivalent to `(FUNCTION)`.
    
    A locative is valid if it names an existing [locative type][a11d] and its
    `LOCATIVE-ARGS` match that type's lambda-list (see
    [`DEFINE-LOCATIVE-TYPE`][b6c4]).
    
    ```common-lisp
    (arglist (dref 'method 'locative))
    => (&REST QUALIFIERS-AND-SPECIALIZERS)
    => :DESTRUCTURING
    ```
    
    See [`XREF-LOCATIVE`][a70d] and [`DREF-LOCATIVE`][cc049].

<a id="x-28DREF-3A-40LOCATIVE-TYPE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **locative type**

    The locative type is the part of a [locative][7ac8] that identifies
    what kind definition is being referred to. This is always a symbol.
    
    Locative types are defined with [`DEFINE-LOCATIVE-TYPE`][b6c4] or
    [`DEFINE-PSEUDO-LOCATIVE-TYPE`][68b4]. See [Basic Locative Types][1d1d] for the list
    locative types built into DRef, and [PAX Locatives][292a] for
    those in PAX.
    
    Also, see [`LOCATIVE-TYPE`][97ba], [`XREF-LOCATIVE-TYPE`][840e], [`DREF-LOCATIVE-TYPE`][a22e],
    [Defining Locative Types][f494].

<a id="x-28DREF-3A-40PRESENTATION-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **presentation**

    [reference][43bd]s may have arguments (see
    [Defining Locative Types][f494]) that do not affect the behaviour
    of [`LOCATE`][8f19] and the [Basic Operations][662d], but which may be used for
    other, "presentation" purposes. For example, the [`VARIABLE`][6c83]
    locative's `INITFORM` argument is used for presentation by
    [`PAX:DOCUMENT`][432c]. Presentation arguments are available via
    [`DREF:DREF-ORIGIN`][e742] but do not feature in [`DREF-LOCATIVE`][cc049] to ensure the
    uniqueness of the definition under [`XREF=`][0617].

<a id="x-28DREF-3A-40DTYPES-20MGL-PAX-3ASECTION-29"></a>

## 4 `DTYPE`s

`DTYPE`s are to Lisp types what [locative type][a11d]s are to [`CLASS`][1f37]es.
A `DTYPE` is either

- a [locative type][a11d] such as [`FUNCTION`][ba62], [`TYPE`][926d]
  and [`CLHS`][ed5f], or

- a full [locative][7ac8] such as `(METHOD (NUMBER))` and `(CLHS SECTION)`,
  or

- `NIL` (the empty `DTYPE`) and `T` (that encompasses all
  [`LISP-LOCATIVE-TYPES`][30ad]), or

- named with [`DEFINE-DTYPE`][c635] (such as [`PSEUDO`][943a] and [`TOP`][3301]), or

- a combination of the above with [`AND`][dd55], [`OR`][e2d1] and
  [`NOT`][954a], or

- a `MEMBER`([`0`][82ae] [`1`][a79d]) form with [`LOCATE`][8f19]able definitions, or

- a [`SATISFIES`][2b8b] form with the name of a function that takes a single
  [definition][2143] as its argument.

`DTYPE`s are used in [`DEFINITIONS`][e196] and [`DREF-APROPOS`][65b4] to filter the set of
definitions as in

```common-lisp
(definitions 'print :dtype '(not unknown))
==> (#<DREF PRINT (CLHS FUNCTION)> #<DREF PRINT FUNCTION>)
```

```common-lisp
(dref-apropos "type specifier" :dtype 'pseudo)
==> (#<DREF "1.4.4.6" #1=(CLHS SECTION)> #<DREF "1.4.4.6.1" #1#>
-->  #<DREF "1.4.4.6.2" #1#> #<DREF "1.4.4.6.3" #1#>
-->  #<DREF "1.4.4.6.4" #1#> #<DREF "4.2.3" #1#>
-->  #<DREF "atomic type specifier" #2=(CLHS GLOSSARY-TERM)>
-->  #<DREF "compound type specifier" #2#>
-->  #<DREF "derived type specifier" #2#> #<DREF "type specifier" #2#>)
```


<a id="x-28DREF-3ADEFINE-DTYPE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-DTYPE** *NAME LAMBDA-LIST &BODY BODY*

    Like [`DEFTYPE`][7f9a], but it may expand into other `DTYPE`s.
    
    The following example defines `METHOD*` as the locative [`METHOD`][172e]
    without its direct locative subtypes.
    
    ```common-lisp
    (define-dtype method* () '(and method (not reader) (not writer)))
    ```
    
    See [`DTYPEP`][963f] for the semantics and also the locative [`DTYPE`][85ba].

<a id="x-28DREF-3ATOP-20DREF-3ADTYPE-29"></a>

- [dtype] **TOP**

    This is the top of the `DTYPE` hierarchy, much like `T` for Lisp types.
    It expands to ([`OR`][e2d1] `T` [`PSEUDO`][943a]). While `T` matches every normal
    Lisp object and objectless definitions present in the running
    Lisp (see [`LISP-LOCATIVE-TYPES`][30ad]), `TOP` matches even pseudo
    definitions (see [`PSEUDO-LOCATIVE-TYPES`][c340]).

<a id="x-28DREF-3APSEUDO-20DREF-3ADTYPE-29"></a>

- [dtype] **PSEUDO**

    This is the union of all [`PSEUDO-LOCATIVE-TYPES`][c340]. It expands
    to `(OR ,@(PSEUDO-LOCATIVE-TYPES))`.

<a id="x-28DREF-3ADTYPEP-20FUNCTION-29"></a>

- [function] **DTYPEP** *DREF DTYPE*

    See if `DREF` is of `DTYPE`.
    
    - *[Atomic locatives][7ac8]:* If `DTYPE` is a [locative type][a11d],
      then it matches definitions with that locative type and its
      locative subtypes.
    
        Because [`CONSTANT`][c819] is defined with `VARIABLE` among its
        [ `LOCATIVE-SUPERTYPES`][b6c4]:
    
        ```common-lisp
        (dtypep (dref 'pi 'constant) 'variable)
        => T
        ```
    
        ```common-lisp
        (dtypep (dref 'number 'class) 'type)
        => T
        ```
    
        It is an error if `DTYPE` is an `ATOM`([`0`][5152] [`1`][a657]) but is not a [locative type][a11d],
        but (the empty) argument list of bare locative types are not
        checked even if having no arguments makes them [invalid
        locatives][7ac8].
    
    - *[Compound locatives][7ac8]:* Locatives in their compound
      form are validated and must match exactly (under [`EQUAL`][3fb5], as in
      [`XREF=`][0617]).
    
        ```common-lisp
        (defparameter *d* (dref 'dref* '(method (t t t))))
        (defparameter *d2* (dref 'dref* '(method :around (t t t))))
        (dtypep *d* 'method)
        => T
        (dtypep *d* '(accessor))
        .. debugger invoked on SIMPLE-ERROR:
        ..   Bad arguments NIL for locative ACCESSOR with lambda list (CLASS-NAME).
        (dtypep *d* '(method (t t t)))
        => T
        (dtypep *d2* '(method (t t t)))
        => NIL
        ```
    
    - `DTYPE` may be constructed with [`AND`][dd55], [`OR`][e2d1] and
      [`NOT`][954a] from Lisp types, locative types, full locatives and
      named `DTYPE`s:
    
        ```common-lisp
        (dtypep (dref 'locate-error 'condition) '(or condition class))
        => T
        (dtypep (dref nil 'type) '(and type (not class)))
        => T
        ```
    
    - For `(MEMBER &REST OBJS)`, each of `OBJ`s is [`LOCATE`][8f19]d and `DREF` is
      matched against them with `XREF=`:
    
        ```common-lisp
        (dtypep (locate #'print) `(member ,#'print))
        => T
        ```
    
    - For `(SATISFIES PRED)`, the predicate `PRED` is funcalled with `DREF`.
    
    - `DTYPE` may be named by [`DEFINE-DTYPE`][c635]:
    
        ```common-lisp
        (dtypep (locate #'car) 'top)
        => T
        ```

<a id="x-28DREF-3A-40LISTING-DEFINITIONS-20MGL-PAX-3ASECTION-29"></a>

## 5 Listing Definitions

<a id="x-28DREF-3ADEFINITIONS-20FUNCTION-29"></a>

- [function] **DEFINITIONS** *NAME &KEY (DTYPE T)*

    List all definitions of `NAME` that are of `DTYPE` as [`DREF`s][d930].
    
    Just as `(DREF NAME LOCATIVE)` returns the canonical definition, the
    [`DREF-NAME`][1e36]s of returned by `DEFINITIONS` may be different from `NAME`:
    
    ```common-lisp
    (definitions "PAX")
    ==> (#<DREF "MGL-PAX" PACKAGE>)
    ```
    
    ```common-lisp
    (definitions 'mgl-pax)
    ==> (#<DREF "mgl-pax" ASDF/SYSTEM:SYSTEM> #<DREF "MGL-PAX" PACKAGE>)
    ```
    
    Similarly, [`DREF-LOCATIVE-TYPE`][a22e] may be more made more specific:
    
    ```common-lisp
    (definitions 'dref:locate-error :dtype 'type)
    ==> (#<DREF LOCATE-ERROR CONDITION>)
    ```
    
    Can be extended via [`MAP-DEFINITIONS-OF-NAME`][97b4].

<a id="x-28DREF-3ADREF-APROPOS-20FUNCTION-29"></a>

- [function] **DREF-APROPOS** *NAME &KEY PACKAGE EXTERNAL-ONLY CASE-SENSITIVE (DTYPE T)*

    Return a list of [`DREF`][d930]s corresponding to existing
    definitions that match the various arguments. First, `(DREF-APROPOS
    NIL)` lists all definitions in the running Lisp and maybe more (e.g.
    [`MGL-PAX:CLHS`][ed5f]). Arguments specify how the list of
    definitions is filtered.
    
    `DREF-APROPOS` itself is similar to [`CL:APROPOS-LIST`][7328], but
    
    - it finds [definition][2143]s not [`SYMBOL`][e5af]s,
    
    - it supports an extensible definition types, and
    
    - filtering based on them.
    
    PAX has a live browsing [frontend][b7fc].
    
    Roughly speaking, when `NAME` or `PACKAGE` is a `SYMBOL`, they must match
    the whole [name][5fc4] of the definition:
    
    ```common-lisp
    (dref-apropos 'method :package :dref :external-only t)
    ==> (#<DREF METHOD CLASS> #<DREF METHOD LOCATIVE>)
    ```
    
    On the other hand, when `NAME` or `PACKAGE` is a `STRING`([`0`][b93c] [`1`][dae6]), they are
    matched as substrings to the definition's name [`PRINC-TO-STRING`][a541]ed:
    
    ```common-lisp
    (dref-apropos "method" :package :dref :external-only t)
    ==> (#<DREF SETF-METHOD LOCATIVE> #<DREF METHOD CLASS>
    -->  #<DREF METHOD LOCATIVE> #<DREF METHOD-COMBINATION CLASS>
    -->  #<DREF METHOD-COMBINATION LOCATIVE>)
    ```
    
    Definitions that are not of `DTYPE` (see [`DTYPEP`][963f]) are filtered out:
    
    ```common-lisp
    (dref-apropos "method" :package :dref :external-only t :dtype 'class)
    ==> (#<DREF METHOD CLASS> #<DREF METHOD-COMBINATION CLASS>)
    ```
    
    When `PACKAGE` is `:NONE`, only non-symbol [name][5fc4]s are matched:
    
    ```
    (dref-apropos "dref" :package :none)
    ==> (#<DREF "DREF" PACKAGE> #<DREF "DREF-EXT" PACKAGE>
    -->  #<DREF "DREF-TEST" PACKAGE> #<DREF "dref" ASDF/SYSTEM:SYSTEM>
    -->  #<DREF "dref/full" ASDF/SYSTEM:SYSTEM>
    -->  #<DREF "dref/test" ASDF/SYSTEM:SYSTEM>
    -->  #<DREF "dref/test-autoload" ASDF/SYSTEM:SYSTEM>)
    ```
    
    The exact rules of filtering are as follows. Let `C` be the [name][5fc4] of
    the candidate definition from the list of all definitions that we
    are matching against the arguments and denote its string
    representation `(PRINC-TO-STRING C)` with `P`. Note that
    `PRINC-TO-STRING` does not print the package of symbols. We say that
    two strings *match* if `CASE-SENSITIVE` is `NIL` and they are [`EQUALP`][2ff3], or
    `CASE-SENSITIVE` is true and they are [`EQUAL`][3fb5]. `CASE-SENSITIVE` affects
    *substring* comparisons too.
    
    - If `NAME` is a `SYMBOL`, then its [`SYMBOL-NAME`][0d07] must *match* `P`.
    
    - If `NAME` is a `STRING`, then it must be a *substring* of `P`.
    
    - If `PACKAGE` is `:ANY`, then `C` must be a `SYMBOL`.
    
    - If `PACKAGE` is `:NONE`, then `C` must *not* be a `SYMBOL`.
    
    - If `PACKAGE` is not `NIL`, `:ANY` or `:NONE`, then `C` must be a symbol.
    
    - If `PACKAGE` is a [`PACKAGE`][1d5a], it must be [`EQ`][5a82] to the
      [`SYMBOL-PACKAGE`][e5ab] of `C`.
    
    - If `PACKAGE` is a `SYMBOL` other than `NIL`, `:ANY` and `:NONE`, then its
      `SYMBOL-NAME` must *match* the [`PACKAGE-NAME`][db68] or one of the
      [`PACKAGE-NICKNAMES`][4b93] of `SYMBOL-PACKAGE` of `C`.
    
    - If `PACKAGE` is a `STRING`, then it must be a *substring* of the
      `PACKAGE-NAME` of `SYMBOL-PACKAGE` of `C`.
    
    - If `EXTERNAL-ONLY` and `C` is a symbol, then `C` must be external in
      a matching package.
    
    - `DTYPE` matches candidate definition `D` if `(DTYPEP D DTYPE)`.
    
    Can be extended via MAP-REFERENCES-OF-TYPE and
    [`MAP-DEFINITIONS-OF-NAME`][97b4].

<a id="x-28DREF-3A-40REVERSE-DEFINITION-ORDER-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **reverse definition order**

    Lists of [locative type][a11d]s and aliases are sometimes in reverse order
    of the time of their definition. This order is not affected by
    redefinition, regardless of whether it's by [`DEFINE-LOCATIVE-TYPE`][b6c4],
    [`DEFINE-PSEUDO-LOCATIVE-TYPE`][68b4], [`DEFINE-SYMBOL-LOCATIVE-TYPE`][ee9b] or
    [`DEFINE-LOCATIVE-ALIAS`][548e].

<a id="x-28DREF-3ALOCATIVE-TYPES-20FUNCTION-29"></a>

- [function] **LOCATIVE-TYPES**

    Return a list of non-[alias][94d1] locative types.
    This is the [`UNION`][d6c7] of [`LISP-LOCATIVE-TYPES`][30ad] and [`PSEUDO-LOCATIVE-TYPES`][c340],
    which is the set of constituents of the `DTYPE` [`TOP`][3301].
    
    This list is in [reverse definition order][9bf9].

<a id="x-28DREF-3ALISP-LOCATIVE-TYPES-20FUNCTION-29"></a>

- [function] **LISP-LOCATIVE-TYPES**

    Return the locative types that correspond to Lisp definitions,
    which typically have [`SOURCE-LOCATION`][32da]. These are defined with
    [`DEFINE-LOCATIVE-TYPE`][b6c4] and [`DEFINE-SYMBOL-LOCATIVE-TYPE`][ee9b] and are the
    constituents of `DTYPE` `T`.
    
    This list is in [reverse definition order][9bf9].

<a id="x-28DREF-3APSEUDO-LOCATIVE-TYPES-20FUNCTION-29"></a>

- [function] **PSEUDO-LOCATIVE-TYPES**

    Return the locative types that correspond to non-Lisp definitions.
    These are the ones defined with [`DEFINE-PSEUDO-LOCATIVE-TYPE`][68b4] and are
    the constituents of `DTYPE` [`PSEUDO`][943a].
    
    This list is in [reverse definition order][9bf9].

<a id="x-28DREF-3ALOCATIVE-ALIASES-20FUNCTION-29"></a>

- [function] **LOCATIVE-ALIASES**

    Return the list of locatives aliases, defined with [`DEFINE-LOCATIVE-ALIAS`][548e].
    
    This list is in [reverse definition order][9bf9].

<a id="x-28DREF-3A-40BASIC-OPERATIONS-20MGL-PAX-3ASECTION-29"></a>

## 6 Basic Operations

The following functions take a single argument, which may be a
[`DREF`][d930], or an object denoting its own definition (see
[`LOCATE`][8f19]).

<a id="x-28DREF-3AARGLIST-20FUNCTION-29"></a>

- [function] **ARGLIST** *OBJECT*

    Return the arglist of the definition of `OBJECT` or `NIL` if the
    arglist cannot be determined.
    
    The second return value indicates whether the arglist has been
    found. As the second return value, `:ORDINARY` indicates an [ordinary
    lambda list][059c], `:MACRO` a [macro lambda list][cc32], `:DEFTYPE` a
    [deftype lambda list][817d], and `:DESTRUCTURING` a [destructuring
    lambda list][6067]. Other non-`NIL` values are also allowed.
    
    ```common-lisp
    (arglist #'arglist)
    => (OBJECT)
    => :ORDINARY
    ```
    
    ```common-lisp
    (arglist (dref 'define-locative-type 'macro))
    => (LOCATIVE-TYPE-AND-LAMBDA-LIST LOCATIVE-SUPERTYPES &OPTIONAL
        DOCSTRING DREF-DEFCLASS-FORM)
    => :MACRO
    ```
    
    ```common-lisp
    (arglist (dref 'method 'locative))
    => (&REST QUALIFIERS-AND-SPECIALIZERS)
    => :DESTRUCTURING
    ```
    
    This function supports [`MACRO`s][f3cc],
    [`COMPILER-MACRO`s][41fd], [`SETF`][d83a] functions,
    [`FUNCTION`s][ba62], [`GENERIC-FUNCTION`s][5875],
    [`METHOD`s][172e], [`TYPE`s][926d], [`LOCATIVE`s][0b3a]. Note
    that `ARGLIST` depends on the quality of `SWANK-BACKEND:ARGLIST`. With
    the exception of SBCL, which has perfect support, all Lisp
    implementations have minor omissions:
    
    - [`DEFTYPE`][7f9a] lambda lists on ABCL, AllegroCL, CLISP, CCL, CMUCL, ECL;
    
    - default values in `MACRO` lambda lists on AllegroCL;
    
    - various edge cases involving traced functions.
    
    Can be extended via [`ARGLIST*`][0a96]

<a id="x-28MGL-PAX-3ADOCSTRING-20FUNCTION-29"></a>

- [function] **DOCSTRING** *OBJECT*

    Return the docstring from the definition of `OBJECT`.
    As the second value, return the [`*PACKAGE*`][5ed1] that was in effect when
    the docstring was installed or `NIL` if it cannot be determined (this
    is used by [`PAX:DOCUMENT`][432c] when [Parsing][378f] the docstring). This
    function is similar in purpose to [`CL:DOCUMENTATION`][c5ae].
    
    Note that some locative types such as [`ASDF:SYSTEM`s][c097] and
    [`DECLARATION`s][6e04] have no docstrings, and some Lisp
    implementations do not record all docstrings. The following are
    known to be missing:
    
    - [`COMPILER-MACRO`][41fd] docstrings on ABCL, AllegroCL, CCL, ECL;
    
    - [`METHOD-COMBINATION`][82e0] docstrings on ABCL, AllegroCL.
    
    Can be extended via [`DOCSTRING*`][9fd4].

<a id="x-28DREF-3ASOURCE-LOCATION-20FUNCTION-29"></a>

- [function] **SOURCE-LOCATION** *OBJECT &KEY ERROR*

    Return the Swank source location for the [defining form][23a8]
    of `OBJECT`.
    
    The returned Swank location object is to be accessed only through
    the [Source Locations][a078] API or to be passed to e.g Slime's
    `slime-goto-source-location`.
    
    If no source location was found,
    
    - if `ERROR` is `NIL`, then return `NIL`;
    
    - if `ERROR` is `:ERROR`, then return a list of the form `(:ERROR
      <ERROR-MESSAGE>)` suitable for `slime-goto-source-location`;
    
    - if `ERROR` is `T`, then signal an [`ERROR`][d162] condition with the same error
      message as in the previous case.
    
    Note that the availability of source location information varies
    greatly across Lisp implementations.
    
    Can be extended via [`SOURCE-LOCATION*`][444d].

<a id="x-28DREF-3A-40BASIC-LOCATIVE-TYPES-20MGL-PAX-3ASECTION-29"></a>

## 7 Basic Locative Types

The following are the [locative type][a11d]s supported out of the
box. As all locative types, they are named by symbols. When there is
a CL type corresponding to the reference's locative type, the
references can be [`RESOLVE`][63b4]d to a unique object as is the case in

```common-lisp
(resolve (dref 'print 'function))
==> #<FUNCTION PRINT>
=> T
```

Even if there is no such CL type, the [`ARGLIST`][e6bd], the [`DOCSTRING`][affc], and
the [`SOURCE-LOCATION`][32da] of the defining form is usually recorded unless
otherwise noted.

The basic locative types and their inheritance structure is loosely
based on the `DOC-TYPE` argument of [`CL:DOCUMENTATION`][c5ae].

<a id="x-28DREF-3A-40VARIABLELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

### 7.1 Locatives for Variables

<a id="x-28VARIABLE-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **VARIABLE** *&OPTIONAL INITFORM*

    - Direct locative subtypes: [`GLOSSARY-TERM`][5119], [`SECTION`][672f], [`CONSTANT`][c819]

    Refers to a global special variable.
    `INITFORM`, or if not specified, the global value of the variable is
    to be used for [presentation][ade6].
    
    ```common-lisp
    (dref '*print-length* 'variable)
    ==> #<DREF *PRINT-LENGTH* VARIABLE>
    ```
    
    `VARIABLE` references do not [`RESOLVE`][63b4].

<a id="x-28MGL-PAX-3ACONSTANT-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **CONSTANT** *&OPTIONAL INITFORM*

    - Direct locative supertypes: [`VARIABLE`][6c83]

    Refers to a constant variable defined with [`DEFCONSTANT`][8934]. `INITFORM`,
    or if not specified, the value of the constant is included in the
    documentation. The [`CONSTANT`][c819] locative is like the [`VARIABLE`][6c83] locative,
    but it also checks that its object is [`CONSTANTP`][a26f].
    
    `CONSTANT` references do not [`RESOLVE`][63b4].

<a id="x-28DREF-3A-40MACROLIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

### 7.2 Locatives for Macros

<a id="x-28SETF-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **SETF**

    - Direct locative subtypes: [`SETF-METHOD`][1a03], [`SETF-FUNCTION`][19f6]

    Refers to a [setf expander][35a2] (see [`DEFSETF`][66dc] and [`DEFINE-SETF-EXPANDER`][d2cb]).
    
    [Setf functions][99b05] (e.g. `(DEFUN (SETF NAME) ...)` or the same
    with [`DEFGENERIC`][c7f7]) are handled by the [`SETF-FUNCTION`][19f6],
    [`SETF-GENERIC-FUNCTION`][ab5e], and `SETF-METHOD` locatives.
    
    `SETF` expander references do not [`RESOLVE`][63b4].

<a id="x-28MGL-PAX-3AMACRO-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **MACRO**

    Refers to a global macro, typically defined with [`DEFMACRO`][14cb], or to a
    [special operator][9a71].
    
    `MACRO` references resolve to the [`MACRO-FUNCTION`][e924] of their `NAME` or
    signal `RESOLVE-ERROR`([`0`][58ba] [`1`][7825]) if that's `NIL`.

<a id="x-28MGL-PAX-3ASYMBOL-MACRO-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **SYMBOL-MACRO**

    Refers to a global symbol macro, defined with [`DEFINE-SYMBOL-MACRO`][46c0].
    Note that since `DEFINE-SYMBOL-MACRO` does not support docstrings, PAX
    defines methods on the [`DOCUMENTATION`][c5ae] generic function specialized on
    `(DOC-TYPE (EQL 'SYMBOL-MACRO))`.
    
    ```
    (define-symbol-macro my-mac 42)
    (setf (documentation 'my-mac 'symbol-macro)
          "This is MY-MAC.")
    (documentation 'my-mac 'symbol-macro)
    => "This is MY-MAC."
    ```
    
    `SYMBOL-MACRO` references do not [`RESOLVE`][63b4].

<a id="x-28COMPILER-MACRO-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **COMPILER-MACRO**

    - Direct locative subtypes: [`SETF-COMPILER-MACRO`][5df4]

    Refers to a [`COMPILER-MACRO-FUNCTION`][c575], typically defined with
    [`DEFINE-COMPILER-MACRO`][23d5].

<a id="x-28DREF-3ASETF-COMPILER-MACRO-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **SETF-COMPILER-MACRO**

    - Direct locative supertypes: [`COMPILER-MACRO`][41fd]

    Refers to a compiler macro with a [setf function name][867c].
    
    `SETF-COMPILER-MACRO` references do not [`RESOLVE`][63b4].

<a id="x-28DREF-3A-40FUNCTIONLIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

### 7.3 Locatives for Functions and Methods

<a id="x-28FUNCTION-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **FUNCTION**

    - Direct locative subtypes: [`STRUCTURE-ACCESSOR`][090c], [`SETF-FUNCTION`][19f6], [`GENERIC-FUNCTION`][5875]

    Refers to a global function, typically defined with [`DEFUN`][f472]. The
    [name][5fc4] must be a [function name][5191]. It is also allowed to
    reference [`GENERIC-FUNCTION`][efe2]s as `FUNCTION`s:
    
    ```common-lisp
    (dref 'docstring 'function)
    ==> #<DREF DOCSTRING FUNCTION>
    ```

<a id="x-28DREF-3ASETF-FUNCTION-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **SETF-FUNCTION**

    - Direct locative supertypes: [`FUNCTION`][ba62], [`SETF`][d83a]
    
    - Direct locative subtypes: [`STRUCTURE-ACCESSOR`][090c], [`SETF-GENERIC-FUNCTION`][ab5e]

    Refers to a global `FUNCTION`([`0`][119e] [`1`][81f7]) with a [setf function name][867c].
    
    ```common-lisp
    (defun (setf ooh) ())
    (locate #'(setf ooh))
    ==> #<DREF OOH SETF-FUNCTION>
    (dref 'ooh 'setf-function)
    ==> #<DREF OOH SETF-FUNCTION>
    (dref '(setf ooh) 'function)
    ==> #<DREF OOH SETF-FUNCTION>
    ```

<a id="x-28GENERIC-FUNCTION-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **GENERIC-FUNCTION**

    - Direct locative supertypes: [`FUNCTION`][ba62]
    
    - Direct locative subtypes: [`SETF-GENERIC-FUNCTION`][ab5e]

    Refers to a [`GENERIC-FUNCTION`][efe2], typically defined with
    [`DEFGENERIC`][c7f7]. The [name][5fc4] must be a [function name][5191].

<a id="x-28DREF-3ASETF-GENERIC-FUNCTION-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **SETF-GENERIC-FUNCTION**

    - Direct locative supertypes: [`GENERIC-FUNCTION`][5875], [`SETF-FUNCTION`][19f6]

    Refers to a global [`GENERIC-FUNCTION`][efe2] with a [setf function name][867c].
    
    ```common-lisp
    (defgeneric (setf oog) ())
    (locate #'(setf oog))
    ==> #<DREF OOG SETF-GENERIC-FUNCTION>
    (dref 'oog 'setf-function)
    ==> #<DREF OOG SETF-GENERIC-FUNCTION>
    (dref '(setf oog) 'function)
    ==> #<DREF OOG SETF-GENERIC-FUNCTION>
    ```

<a id="x-28METHOD-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **METHOD** *&REST QUALIFIERS-AND-SPECIALIZERS*

    - Direct locative subtypes: [`WRITER`][e548], [`READER`][cc04], [`SETF-METHOD`][1a03]

    Refers to a `METHOD`. [name][5fc4] must be a [function name][5191].
    `METHOD-QUALIFIERS-AND-SPECIALIZERS` has the form
    
        (<QUALIFIER>* <SPECIALIZERS>)
    
    For example, the method
    
    ```common-lisp
    (defgeneric foo-gf (x y z)
      (:method :around (x (y (eql 'xxx)) (z string))
        (values x y z)))
    ```
    
    can be referred to as
    
    ```common-lisp
    (dref 'foo-gf '(method :around (t (eql xxx) string)))
    ==> #<DREF FOO-GF (METHOD :AROUND (T (EQL XXX) STRING))>
    ```
    
    `METHOD` is not [`EXPORTABLE-LOCATIVE-TYPE-P`][c930].

<a id="x-28DREF-3ASETF-METHOD-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **SETF-METHOD** *&REST METHOD-QUALIFIERS-AND-SPECIALIZERS*

    - Direct locative supertypes: [`METHOD`][172e], [`SETF`][d83a]
    
    - Direct locative subtypes: [`ACCESSOR`][00d4]

    Refers to a [`METHOD`][51c3] of a `SETF-GENERIC-FUNCTION`.
    
    ```common-lisp
    (defgeneric (setf oog) (v)
      (:method ((v string))))
    (locate (find-method #'(setf oog) () (list (find-class 'string))))
    ==> #<DREF OOG (SETF-METHOD (STRING))>
    (dref 'oog '(setf-method (string)))
    ==> #<DREF OOG (SETF-METHOD (STRING))>
    (dref '(setf oog) '(method (string)))
    ==> #<DREF OOG (SETF-METHOD (STRING))>
    ```

<a id="x-28METHOD-COMBINATION-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **METHOD-COMBINATION**

    Refers to a [`METHOD-COMBINATION`][9b70], defined with
    [`DEFINE-METHOD-COMBINATION`][006c].
    
    `METHOD-COMBINATION` references do not [`RESOLVE`][63b4].

<a id="x-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **READER** *CLASS-NAME*

    - Direct locative supertypes: [`METHOD`][172e]
    
    - Direct locative subtypes: [`ACCESSOR`][00d4]

    Refers to a `:READER` method in a [`DEFCLASS`][ead6]:
    
    ```common-lisp
    (defclass foo ()
      ((xxx :reader foo-xxx)))
    
    (dref 'foo-xxx '(reader foo))
    ==> #<DREF FOO-XXX (READER FOO)>
    ```

<a id="x-28MGL-PAX-3AWRITER-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **WRITER** *CLASS-NAME*

    - Direct locative supertypes: [`METHOD`][172e]
    
    - Direct locative subtypes: [`ACCESSOR`][00d4]

    Like [`ACCESSOR`][00d4], but refers to a `:WRITER` method in a [`DEFCLASS`][ead6].

<a id="x-28MGL-PAX-3AACCESSOR-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **ACCESSOR** *CLASS-NAME*

    - Direct locative supertypes: [`READER`][cc04], [`WRITER`][e548], [`SETF-METHOD`][1a03]

    Refers to an `:ACCESSOR` in a [`DEFCLASS`][ead6].
    
    An `:ACCESSOR` in `DEFCLASS` creates a reader and a writer method.
    Somewhat arbitrarily, `ACCESSOR` references [`RESOLVE`][63b4] to the writer
    method but can be [`LOCATE`][8f19]d with either.

<a id="x-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **STRUCTURE-ACCESSOR** *&OPTIONAL STRUCTURE-CLASS-NAME*

    - Direct locative supertypes: [`SETF-FUNCTION`][19f6], [`FUNCTION`][ba62]

    Refers to an accessor function generated by [`DEFSTRUCT`][eac1] or [`DEFSTRUCT*`][12814].
    A [`LOCATE-ERROR`][6334] condition is signalled if the wrong
    `STRUCTURE-CLASS-NAME` is provided.
    
    Note that there is no portable way to detect structure accessors,
    and on some platforms, `(LOCATE #'MY-ACCESSOR)`, [`DEFINITIONS`][e196] and
    [`DREF-APROPOS`][65b4] will return `FUNCTION`([`0`][119e] [`1`][81f7]) references instead. On such
    platforms, `STRUCTURE-ACCESSOR` references do not [`RESOLVE`][63b4].

<a id="x-28DREF-3ADEFSTRUCT-2A-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFSTRUCT\*** *NAME-AND-OPTIONS &REST SLOT-DESCRIPTIONS*

    Like [`DEFSTRUCT`][eac1], but support `:DOCUMENTATION` among slot options.
    The documentation is attached to the slot's `STRUCTURE-ACCESSOR`.
    Example:
    
        (defstruct* my-struct
          (my-slot nil :documentation "docstring"))
    
    In addition normal `DEFSTRUCT` processing, the above also does the
    moral equivalent of
    
        (setf (documentation 'my-struct-my-slot 'function) "docstring")

<a id="x-28DREF-3A-40TYPELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

### 7.4 Locatives for Types and Declarations

<a id="x-28TYPE-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **TYPE**

    - Direct locative subtypes: [`CLASS`][2060]

    This locative can refer to [types and classes][0ff7] and
    [conditions][e237], simply put, to things defined by [`DEFTYPE`][7f9a],
    [`DEFCLASS`][ead6] and [`DEFINE-CONDITION`][f7e4].
    
    ```common-lisp
    (deftype my-type () t)
    (dref 'my-type 'type)
    ==> #<DREF MY-TYPE TYPE>
    ```
    
    ```common-lisp
    (dref 'xref 'type)
    ==> #<DREF XREF CLASS>
    ```
    
    ```common-lisp
    (dref 'locate-error 'type)
    ==> #<DREF LOCATE-ERROR CONDITION>
    ```
    
    `TYPE` references do not [`RESOLVE`][63b4].

<a id="x-28CLASS-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **CLASS**

    - Direct locative supertypes: [`TYPE`][926d]
    
    - Direct locative subtypes: [`CONDITION`][c479], [`STRUCTURE`][da65]

    Naturally, `CLASS` is the locative type for [`CLASS`][1f37]es.
    
    Also, see the related [`CONDITION`][c479] locative.

<a id="x-28STRUCTURE-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **STRUCTURE**

    - Direct locative supertypes: [`CLASS`][2060]

    Refers to a [`STRUCTURE-CLASS`][e608], typically defined with [`DEFSTRUCT`][eac1].
    
    Also, see [`DEFSTRUCT*`][12814].

<a id="x-28DECLARATION-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **DECLARATION**

    Refers to a declaration, used in [`DECLARE`][1574], [`DECLAIM`][ebea] and [`PROCLAIM`][d3e1].
    
    User code may also define new declarations with CLTL2 functionality,
    but there is currently no way to provide a docstring, and their
    [`ARGLIST`][e6bd] is always `NIL`.
    
    ```
    (cl-environments:define-declaration my-decl (&rest things)
      (values :declare (cons 'foo things)))
    ```
    
    `DECLARATION` references do not [`RESOLVE`][63b4].
    
    Also, [`SOURCE-LOCATION`][32da] on declarations currently only works on SBCL.

<a id="x-28DREF-3A-40CONDITION-SYSTEM-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

### 7.5 Locatives for the Condition System

<a id="x-28CONDITION-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **CONDITION**

    - Direct locative supertypes: [`CLASS`][2060]

    Although `CONDITION` is not [`SUBTYPEP`][daac] of [`CLASS`][1f37], actual condition
    objects are commonly instances of a condition class that is a CLOS
    class. HyperSpec [ISSUE:CLOS-CONDITIONS][023a] and
    [ISSUE:CLOS-CONDITIONS-AGAIN][da2e] provide the relevant history.
    
    Whenever a `CLASS` denotes a `CONDITION`, its [`DREF-LOCATIVE-TYPE`][a22e] will be
    `CONDITION`:
    
    ```common-lisp
    (dref 'locate-error 'class)
    ==> #<DREF LOCATE-ERROR CONDITION>
    ```

<a id="x-28RESTART-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **RESTART**

    A locative to refer to the definition of a restart defined by
    [`DEFINE-RESTART`][bb23].

<a id="x-28DREF-3ADEFINE-RESTART-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-RESTART** *SYMBOL LAMBDA-LIST &BODY DOCSTRING*

    Associate a definition with the name of a restart, which must be a symbol.
    `LAMBDA-LIST` should be what calls like `(INVOKE-RESTART '<SYMBOL>
    ...)` must conform to, but this not enforced.
    
    PAX "defines" standard CL restarts such as `USE-VALUE`([`0`][5406] [`1`][cf08]) with
    `DEFINE-RESTART`:
    
    ```common-lisp
    (first-line (source-location-snippet
                 (source-location (dref 'use-value 'restart))))
    => "(define-restart use-value (value)"
    ```
    
    Note that while there is a [`CL:RESTART`][38e4] class, its instances have no
    docstring or source location.

<a id="x-28DREF-3A-40PACKAGELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

### 7.6 Locatives for Packages and Readtables

<a id="x-28ASDF-2FSYSTEM-3ASYSTEM-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **ASDF/SYSTEM:SYSTEM**

    Refers to an already loaded `ASDF:SYSTEM` (those in `ASDF:REGISTERED-SYSTEMS`).
    The [name][5fc4] may be anything `ASDF:FIND-SYSTEM` supports.
    
    `ASDF:SYSTEM` is not [`EXPORTABLE-LOCATIVE-TYPE-P`][c930].

<a id="x-28PACKAGE-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **PACKAGE**

    Refers to a [`PACKAGE`][1d5a], defined by [`DEFPACKAGE`][9b43] or [`MAKE-PACKAGE`][6e6e].
    The [name][5fc4] may be anything [`FIND-PACKAGE`][4dc9] supports.
    
    `PACKAGE` is not [`EXPORTABLE-LOCATIVE-TYPE-P`][c930].

<a id="x-28READTABLE-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **READTABLE**

    Refers to a named [`READTABLE`][d646] defined with
    `NAMED-READTABLES:DEFREADTABLE`, which associates a global name and a
    docstring with the readtable object. The [name][5fc4] may be anything
    `FIND-READTABLE` supports.
    
    `READTABLE` references [`RESOLVE`][63b4] to `FIND-READTABLE` on their [name][5fc4].

<a id="x-28DREF-3A-40UNKNOWN-DEFINITIONS-20MGL-PAX-3ASECTION-29"></a>

### 7.7 Locatives for Unknown Definitions

<a id="x-28MGL-PAX-3AUNKNOWN-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **UNKNOWN** *DSPEC*

    This locative type allows PAX to work in a limited way with
    definition types it doesn't know. `UNKNOWN` definitions come from
    [`DEFINITIONS`][e196], which uses `SWANK/BACKEND:FIND-DEFINITIONS`. The
    following examples show PAX stuffing the Swank
    dspec `(:DEFINE-ALIEN-TYPE DOUBLE-FLOAT)` into an [`UNKNOWN`][a951] locative
    on SBCL.
    
    ```common-lisp
    (definitions 'double-float)
    ==> (#<DREF DOUBLE-FLOAT CLASS>
    -->  #<DREF DOUBLE-FLOAT (UNKNOWN (:DEFINE-ALIEN-TYPE DOUBLE-FLOAT))>)
    ```
    
    ```common-lisp
    (dref 'double-float '(unknown (:define-alien-type double-float)))
    ==> #<DREF DOUBLE-FLOAT (UNKNOWN (:DEFINE-ALIEN-TYPE DOUBLE-FLOAT))>
    ```
    
    [`ARGLIST`][e6bd] and [`DOCSTRING`][affc] return `NIL` for `UNKNOWN`s, but [`SOURCE-LOCATION`][32da]
    works.

<a id="x-28DREF-3A-40DREF-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

### 7.8 Locatives for DRef Constructs

<a id="x-28DREF-3ADTYPE-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **DTYPE**

    - Direct locative subtypes: [`LOCATIVE`][0b3a]

    Locative for [`DTYPE`s][a459] defined with [`DEFINE-DTYPE`][c635] and `LOCATIVE` types.
    `DTYPE` is to `LOCATIVE` as [`TYPE`][7c9f] is to [`CLASS`][1f37].
    
    The [`TOP`][3301] of the `DTYPE` hierarchy:
    
    ```common-lisp
    (dref 'top 'dtype)
    ==> #<DREF TOP DTYPE>
    ```
    
    This very definition:
    
    ```common-lisp
    (dref 'dtype 'locative)
    ==> #<DREF DTYPE LOCATIVE>
    ```

<a id="x-28MGL-PAX-3ALOCATIVE-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **LOCATIVE**

    - Direct locative supertypes: [`DTYPE`][85ba]

    This is the locative for [locative type][a11d]s defined with
    [`DEFINE-LOCATIVE-TYPE`][b6c4], [`DEFINE-PSEUDO-LOCATIVE-TYPE`][68b4] and
    [`DEFINE-LOCATIVE-ALIAS`][548e].
    
    ```
    (first-line (source-location-snippet
                 (source-location (dref 'macro 'locative))))
    => "(define-locative-type macro ()"
    ```

<a id="x-28LAMBDA-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **LAMBDA** *&KEY ARGLIST ARGLIST-TYPE DOCSTRING DOCSTRING-PACKAGE FILE FILE-POSITION SNIPPET &ALLOW-OTHER-KEYS*

    A [pseudo locative type][c340] that carries its
    [`ARGLIST`][e6bd], [`DOCSTRING`][affc] and [`SOURCE-LOCATION`][32da] in the locative itself. See
    [`MAKE-SOURCE-LOCATION`][3bdc] for the description of `FILE`, [`FILE-POSITION`][6f95], and
    `SNIPPET`. `LAMBDA` references do not [`RESOLVE`][63b4]. The [name][5fc4] must be `NIL`.
    
    ```common-lisp
    (arglist (dref nil '(lambda :arglist ((x y) z)
                                :arglist-type :macro)))
    => ((X Y) Z)
    => :MACRO
    ```
    
    ```common-lisp
    (docstring (dref nil '(lambda :docstring "xxx"
                                  :docstring-package :dref)))
    => "xxx"
    ==> #<PACKAGE "DREF">
    ```
    
    ```common-lisp
    (source-location-file
     (source-location (dref nil '(lambda :file "xxx.el"))))
    => "xxx.el"
    ```
    
    Also, see the [`PAX:INCLUDE`][5cd7] locative.

<a id="x-28DREF-EXT-3A-40EXTENDING-DREF-20MGL-PAX-3ASECTION-29"></a>

## 8 Extending DRef

<a id="x-28DREF-EXT-3A-40EXTENSION-TUTORIAL-20MGL-PAX-3ASECTION-29"></a>

### 8.1 Extension Tutorial

Let's see how to tell DRef about new kinds of definitions through
the example of the implementation of the [`CLASS`][2060] locative. Note that
this is a verbatim [`PAX:INCLUDE`][5cd7] of the sources. Please
ignore any internal machinery. The first step is to define the
[locative type][a11d]:

```
(define-locative-type class (type)
  "Naturally, CLASS is the locative type for [CLASS][class]es.

  Also, see the related CONDITION locative.")

```

Then, we make it possible to look up [`CLASS`][1f37] definitions:

```
(define-locator class ((class class))
  (make-instance 'class-dref :name (class-name class) :locative 'class))

(define-lookup class (symbol locative-args)
  (unless (and (symbolp symbol)
               (find-class symbol nil))
    (locate-error "~S does not name a class." symbol))
  (make-instance 'class-dref :name symbol :locative 'class))

```

[`DEFINE-LOCATOR`][16b6] makes `(LOCATE (FIND-CLASS 'DREF))` work, while
[`DEFINE-LOOKUP`][49b5] is for `(DREF 'DREF 'CLASS)`. Naturally, for locative
types that do not define first-class objects, the first method
cannot be defined.

Finally, we define a [`RESOLVE*`][d3b3] method to recover the [`CLASS`][1f37]
object from a [`CLASS-DREF`][b3a7]. We also specialize [`DOCSTRING*`][9fd4] and
[`SOURCE-LOCATION*`][444d]:

```
(defmethod resolve* ((dref class-dref))
  (find-class (dref-name dref)))

(defmethod docstring* ((class class))
  (documentation* class t))

(defmethod source-location* ((dref class-dref))
  (swank-source-location* (resolve dref) (dref-name dref) 'class))

```

We took advantage of having just made the class locative type being
[`RESOLVE`][63b4]able, by specializing [`DOCSTRING*`][9fd4] on the [`CLASS`][1f37] class.
[`SOURCE-LOCATION*`][444d] was specialized on [`CLASS-DREF`][b3a7] to demonstrate how
this can be done for non-`RESOLVE`able locative types.

Classes have no arglist, so no [`ARGLIST*`][0a96] method is needed. In the
following, we describe the pieces in detail.

<a id="x-28DREF-EXT-3A-40LOCATIVE-TYPE-HIERARCHY-20MGL-PAX-3ASECTION-29"></a>

### 8.2 Locative Type Hierarchy

[Locative types][a11d] form their own hierarchy, that
is only superficially similar to the Lisp [`CLASS`][1f37] hierarchy.
The hierarchies of [`LISP-LOCATIVE-TYPES`][30ad] and [`PSEUDO-LOCATIVE-TYPES`][c340]
are distinct. That is, the [`DREF-CLASS`][25be] of a Lisp locative type must
not be a subclass of a [`PSEUDO`][943a] one, and vice versa. This is enforced
by [`DEFINE-LOCATIVE-TYPE`][b6c4] and [`DEFINE-PSEUDO-LOCATIVE-TYPE`][68b4].

<a id="x-28DREF-EXT-3ADREF-CLASS-20FUNCTION-29"></a>

- [function] **DREF-CLASS** *LOCATIVE-TYPE*

    Return the name of the [`CLASS`][1f37] used to represent [definition][2143]s with
    `LOCATIVE-TYPE`. This is always a subclass of [`DREF`][d930]. Returns
    `NIL` if `LOCATIVE-TYPE` is not a valid locative type.
    
    Note that the actual [`TYPE-OF`][9caa] a `DREF` is mostly intended for
    [Extending DRef][68fb]. Hence, it is hidden when a `DREF` is printed:
    
    ```common-lisp
    (dref 'print 'function)
    ==> #<DREF PRINT FUNCTION>
    (type-of *)
    => FUNCTION-DREF
    ```
    
    Due to [Canonicalization][9383], the actual type may be a proper subtype of
    `DREF-CLASS`:
    
    ```common-lisp
    (dref 'documentation 'function)
    ==> #<DREF DOCUMENTATION GENERIC-FUNCTION>
    (type-of *)
    => GENERIC-FUNCTION-DREF
    (subtypep 'generic-function-dref 'function-dref)
    => T
    => T
    ```

<a id="x-28DREF-EXT-3ALOCATIVE-TYPE-DIRECT-SUPERS-20FUNCTION-29"></a>

- [function] **LOCATIVE-TYPE-DIRECT-SUPERS** *LOCATIVE-TYPE*

    List the [locative type][a11d]s whose [`DREF-CLASS`][25be]es are direct superclasses
    of the `DREF-CLASS` of `LOCATIVE-TYPE`. These can be considered
    supertypes of `LOCATIVE-TYPE` in the sense of [`DTYPEP`][963f].
    
    This is ordered as in the corresponding definition.

<a id="x-28DREF-EXT-3ALOCATIVE-TYPE-DIRECT-SUBS-20FUNCTION-29"></a>

- [function] **LOCATIVE-TYPE-DIRECT-SUBS** *LOCATIVE-TYPE*

    List the [locative type][a11d]s whose [`DREF-CLASS`][25be]es are direct subclasses
    of the `DREF-CLASS` of `LOCATIVE-TYPE`. These can be considered subtypes
    of `LOCATIVE-TYPE` in the sense of [`DTYPEP`][963f].
    
    This list is in [reverse definition order][9bf9].

<a id="x-28DREF-EXT-3A-40DEFINING-LOCATIVE-TYPES-20MGL-PAX-3ASECTION-29"></a>

### 8.3 Defining Locative Types

<a id="x-28DREF-EXT-3ADEFINE-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-LOCATIVE-TYPE** *LOCATIVE-TYPE-AND-LAMBDA-LIST LOCATIVE-SUPERTYPES &OPTIONAL DOCSTRING DREF-DEFCLASS-FORM*

    Declare `LOCATIVE-TYPE` as a [`LOCATIVE`][0b3a],
    which is the first step in [Extending DRef][68fb].
    
    - *Simple example*
    
        To define a locative type called `DUMMY` that takes no arguments
        and is not a locative subtype of any other locative type:
    
        ```
        (define-locative-type dummy ()
          "Dummy docstring.")
        ```
    
        With this definition, only the locatives `DUMMY` and its
        equivalent form `(DUMMY)` are valid. The above defines a `DREF`([`0`][d930] [`1`][7e92])
        subclass called `DUMMY-DREF` in the current package. All
        definitions with locative type `DUMMY` and its locatives
        subtypes must be instances of `DUMMY-DREF`.
    
        `(LOCATE 'DUMMY 'LOCATIVE)` refers to this definition. That is,
        [`ARGLIST`][e6bd], [`DOCSTRING`][affc] and [`SOURCE-LOCATION`][32da] all work on
        it.
    
    - *Complex example*
    
        `DUMMY` may have arguments `X` and `Y` and inherit from locative
        types `L1` and `L2`:
    
        ```
        (define-locative-type (dummy x &key y) (l1 l2)
          "Dummy docstring."
          (defclass dummy-dref ()
            ((xxx :initform nil :accessor dummy-xxx))))
        ```
    
        One may change name of `DUMMY-DREF`, specify superclasses and
        add slots as with [`DEFCLASS`][ead6]. Behind the scenes, the `DREF` classes
        of `L1` and `L2` are added automatically to the list of
        superclasses.
    
    Arguments:
    
    - The general form of `LOCATIVE-TYPE-AND-LAMBDA-LIST`
      is (`LOCATIVE-TYPE` [`&REST`][4336] `LAMBDA-LIST`), where `LOCATIVE-TYPE` is a
      [`SYMBOL`][e5af], and `LAMBDA-LIST` is a [destructuring lambda list][6067].
      The [`LOCATIVE-ARGS`][2444] of [`DREF`][d930]s with [locative type][a11d]
      `LOCATIVE-TYPE` (the argument given to this macro) always conform to
      this lambda list. See [`CHECK-LOCATIVE-ARGS`][10a7].
    
        If `LOCATIVE-TYPE-AND-LAMBDA-LIST` is a single symbol, then that's
        interpreted as `LOCATIVE-TYPE`, and `LAMBDA-LIST` is `NIL`.
    
    - `LOCATIVE-SUPERTYPES` is a list of [locative type][a11d]s whose
      [`DREF-CLASS`][25be]es are added to prepended to the list of superclasses
      this definition.
    
    Locative types defined with `DEFINE-LOCATIVE-TYPE` can be listed with
    [`LISP-LOCATIVE-TYPES`][30ad].

<a id="x-28DREF-EXT-3ADEFINE-PSEUDO-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-PSEUDO-LOCATIVE-TYPE** *LOCATIVE-TYPE-AND-LAMBDA-LIST LOCATIVE-SUPERTYPES &OPTIONAL DOCSTRING DREF-DEFCLASS-FORM*

    Like [`DEFINE-LOCATIVE-TYPE`][b6c4], but declare that
    `LOCATIVE-TYPE` does not correspond to definitions in the
    running Lisp. Definitions with pseudo locatives are of `DTYPE` [`PSEUDO`][943a]
    and are not listed by default by [`DEFINITIONS`][e196].
    
    Locative types defined with `DEFINE-PSEUDO-LOCATIVE-TYPE` can be
    listed with [`PSEUDO-LOCATIVE-TYPES`][c340].

<a id="x-28DREF-EXT-3ADEFINE-LOCATIVE-ALIAS-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-LOCATIVE-ALIAS** *ALIAS LOCATIVE-TYPE &BODY DOCSTRING*

    Define `ALIAS` that can be substituted for `LOCATIVE-TYPE` (both
    [`SYMBOL`][e5af]s) for the purposes of [`LOCATE`][8f19]ing. `LOCATIVE-TYPE` must
    exist (i.e. be among [`LOCATIVE-TYPES`][99b0]). For example, let's define
    `OBJECT` as an alias of the [`CLASS`][2060] locative:
    
    ```
    (define-locative-alias object class)
    ```
    
    Then, `LOCATE`ing with `OBJECT` will find the `CLASS`:
    
    ```
    (dref 'xref 'object)
    ==> #<DREF XREF CLASS>
    ```
    
    The [`LOCATIVE-ARGS`][2444] of `OBJECT` (none in the above) are passed on to
    `CLASS`.
    
    ```
    (arglist (dref 'object 'locative))
    => (&REST ARGS)
    => :DESTRUCTURING
    ```
    
    Note that [`LOCATIVE-ALIASES`][94d1] are not `LOCATIVE-TYPES` and are not valid
    `DTYPE`s.
    
    Also, see [Locative Aliases][0fa3] in PAX.

<a id="x-28DREF-EXT-3A-40SYMBOL-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

#### 8.3.1 Symbol Locatives

Let's see how the opaque [`DEFINE-SYMBOL-LOCATIVE-TYPE`][ee9b] and the
obscure [`DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE`][3b96] macros work together
to simplify the common task of associating definition with a symbol
in a certain context.

<a id="x-28DREF-EXT-3ADEFINE-SYMBOL-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-SYMBOL-LOCATIVE-TYPE** *LOCATIVE-TYPE-AND-LAMBDA-LIST LOCATIVE-SUPERTYPES &OPTIONAL DOCSTRING DREF-CLASS-DEF*

    Similar to [`DEFINE-LOCATIVE-TYPE`][b6c4], but it assumes that all things
    [`LOCATE`][8f19]able with [`LOCATIVE-TYPE`][97ba] are going to be symbols defined with a
    definer defined with [`DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE`][3b96]. Symbol
    locatives are for attaching a definition (along with arglist,
    documentation and source location) to a symbol in a particular
    context. An example will make everything clear:
    
    ```
    (define-symbol-locative-type direction ()
      "A direction is a symbol.")
    
    (define-definer-for-symbol-locative-type define-direction direction
      "With DEFINE-DIRECTION, one can document what a symbol means when
      interpreted as a DIRECTION.")
    
    (define-direction up ()
      "UP is equivalent to a coordinate delta of (0, -1).")
    ```
    
    After all this, `(DREF 'UP 'DIRECTION)` refers to the
    `DEFINE-DIRECTION` form above.
    
    The [`DREF-CLASS`][25be] of the defined locative type inherits from
    [`SYMBOL-LOCATIVE-DREF`][34b9], which may be used for specializing when
    implementing new operations.

<a id="x-28DREF-EXT-3ADEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE** *NAME LOCATIVE-TYPE &BODY DOCSTRING*

    Define a macro with `NAME` that can be used to attach a lambda list,
    documentation, and source location to a symbol in the context of
    `LOCATIVE-TYPE`. The defined macro's arglist is `(SYMBOL LAMBDA-LIST
    &OPTIONAL DOCSTRING)`. `LOCATIVE-TYPE` is assumed to have been defined
    with [`DEFINE-SYMBOL-LOCATIVE-TYPE`][ee9b].

<a id="x-28DREF-EXT-3A-40EXTENDING-LOCATE-20MGL-PAX-3ASECTION-29"></a>

### 8.4 Extending `LOCATE`

Internally, [`LOCATE`][8f19] finds an initial [`DREF`][d930] of its `OBJECT`
argument with a [lookup][49b5] or with a
[locator][16b6]. This initial `DREF` is then canonicalized
with a series of [casts][2066]. In more detail, the process
is as follows.

- If the `OBJECT` argument of `LOCATE` is a `DREF`, then it is returned
  without processing.

Else, `LOCATE` first needs to finds the initial definition.

<a id="x-28DREF-EXT-3A-40INITIAL-DEFINITION-20MGL-PAX-3ASECTION-29"></a>

#### 8.4.1 Initial Definition

[`LOCATE`][8f19] can find the initial definition in one of two ways:

- *With direct
lookup*

    If `OBJECT` is an `XREF`([`0`][1538] [`1`][cda7]), then the [lookup][49b5]
    for ([`XREF-LOCATIVE-TYPE`][840e] `OBJECT`) is invoked. For an `XREF` with the
    locative `(METHOD (NUMBER))`, this would be the lookup
    defined as

    ```
    (define-lookup method (name locative-args) ...)
    ```

- *With locator
search*

    Else, `OBJECT` is a normal Lisp object, such as a [`METHOD`][51c3]
    object from [`FIND-METHOD`][6d46]. The first of [`LISP-LOCATIVE-TYPES`][30ad] whose
    [locator][16b6] succeeds provides the initial
    definition, which may be defined like this:

    ```
    (define-locator method ((obj method)) ...)
    ```

    This is a locator that returns definitions with the [`METHOD`][172e]
    locative type and takes an argument named `OBJ` of class
    [`METHOD`][51c3] (which is like a specializer in [`DEFMETHOD`][6832]).

    - `LISP-LOCATIVE-TYPES` are tried one by one in the order
      specified there.

    - For a given locative type, if there are multiple locators,
      standard CLOS method selection applies.


<a id="x-28DREF-EXT-3A-40CANONICALIZATION-20MGL-PAX-3ASECTION-29"></a>

#### 8.4.2 Canonicalization

The initial definition thus found is then canonicalized so that
there is a unique [definition][2143] under [`XREF=`][0617]:

```
(locate #'arglist*)
==> #<DREF ARGLIST* GENERIC-FUNCTION>
(dref 'arglist* 'function)
==> #<DREF ARGLIST* GENERIC-FUNCTION>
(dref 'arglist* 'generic-function)
==> #<DREF ARGLIST* GENERIC-FUNCTION>
```

Canonicalization is performed by recursively attempting to
[downcast][2066] the current definition to one of its
[`LOCATIVE-TYPE-DIRECT-SUBS`][130a] in a depth-first manner, backtracking if a
cast fails.

<a id="x-28DREF-EXT-3A-40DEFAULT-DOWNCAST-20MGL-PAX-3ASECTION-29"></a>

##### Default Downcast

Downcasting to [direct locative subtypes][130a] is performed by default by looking up
the definition where the locative type is replaced with its sub
while the name and the locative args remain the same.

<a id="x-28DREF-EXT-3A-40CAST-NAME-CHANGE-20MGL-PAX-3ASECTION-29"></a>

##### Cast Name Change

[Casts][2066] must be careful about changing [`DREF-NAME`][1e36].

Their `DREF` argument and the [`DREF`][d930] returned must have the
same `DREF-NAME` (under [`EQUAL`][3fb5], see [`XREF=`][0617]) or it must be possible to
upcast the returned value to the `DREF` argument's `DREF-LOCATIVE-TYPE`.

- *Implementation note*

    The purpose of this rule is to allow [`DTYPEP`][963f] answer this correctly:

    ```common-lisp
    (defclass foo ()
      ((a :accessor foo-a)))
    (dref '(setf foo-a) '(method (t foo)))
    ==> #<DREF FOO-A (ACCESSOR FOO)>
    (dtypep * '(method (t foo)))
    => T
    ;; Internally, DTYPEP upcast #<DREF FOO-A (ACCESSOR FOO)>
    ;; and checks that the locative args of the resulting
    ;; definition match those in (METHOD (T FOO)).
    (locate* ** 'method)
    ==> #<DREF (SETF FOO-A) (METHOD (T FOO))>
    ```

    For even more background, also note that if the name remains the
    same but locative args change, then `DTYPEP` can simply check with
    [`DREF`][7e92] if there is a definition of the name with the
    given locative:

    ```common-lisp
    (defclass foo ()
      ((r :reader foo-r)))
    (dref 'foo-r '(reader foo))
    ==> #<DREF FOO-R (READER FOO)>
    (dtypep * '(method (foo)))
    => T
    ;; Behind the scenes, DTYPEP does this:
    (xref= ** (dref 'foo-r '(method (foo))))
    => T
    ```


<a id="x-28DREF-EXT-3A-40DEFINING-LOOKUPS-LOCATORS-AND-CASTS-20MGL-PAX-3ASECTION-29"></a>

#### 8.4.3 Defining Lookups, Locators and Casts

As we have seen, the [Initial Definition][87fc] is provided either by a
lookup or a locator, then [Canonicalization][9383] works with
casts. Here, we look at how to define these.

*Implementation note:* All three are currently implemented as
methods of generic functions with [`EQL` specializers][29a1] for
the locative type, which may easily prove to be problematic down the
road. To make future changes easier, the generic function and the
methods are hidden behind e.g. the [`DEFINE-LOOKUP`][49b5] and [`CALL-LOOKUP`][2ab8]
macros.

<a id="x-28DREF-EXT-3A-2ACHECK-LOCATE-2A-20VARIABLE-29"></a>

- [variable] **\*CHECK-LOCATE\*** *NIL*

    Enable runtime verification of invariants during [`LOCATE`][8f19] calls.
    This carries a performance penalty and is intended for testing and
    debugging.
    
    In particular, enforce the rule of [Cast Name Change][c68e] and that [lookups][49b5], [locators][16b6] and
    [casts][2066] obey the following:
    
    - The value returned must be either `NIL` or a `DREF`([`0`][d930] [`1`][7e92]). Alternatively,
      `LOCATE-ERROR`([`0`][6334] [`1`][6932]) may be signalled.
    
    - If a `DREF` is returned, then its [`DREF-LOCATIVE-TYPE`][a22e] must be
      [`LOCATIVE-TYPE`][97ba], and its class must be the [`DREF-CLASS`][25be] of
      `LOCATIVE-TYPE`.
    
    - [`LOCATIVE-ARGS`][2444] must be congruent with the destructuring lambda list
      in the definition of `LOCATIVE-TYPE`.

<a id="x-28DREF-EXT-3ADEFINE-LOOKUP-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-LOOKUP** *LOCATIVE-TYPE (NAME LOCATIVE-ARGS) &BODY BODY*

    Define a method of looking up [definition][2143]s of `LOCATIVE-TYPE`
    with the given `LOCATIVE-ARGS`. Lookups are invoked by [`LOCATE`][8f19] when its
    `OBJECT` argument is an `XREF`([`0`][1538] [`1`][cda7]) with `LOCATIVE-TYPE` but it is not a `DREF`([`0`][d930] [`1`][7e92]),
    as in the case of `(DREF 'PRINT 'FUNCTION)`. When called, the
    variables `NAME` and `LOCATIVE-ARGS` are bound to [`XREF-NAME`][5447] and
    [`XREF-LOCATIVE-ARGS`][1490] of the `XREF`. `LOCATIVE-ARGS` is validated with
    [`CHECK-LOCATIVE-ARGS`][10a7] before `BODY` is evaluated.
    
    ```
    (define-lookup variable (name locative-args)
      (unless (special-variable-name-p name)
        (locate-error))
      (make-instance 'variable-dref :name name :locative 'variable))
    ```
    
    - `LOCATIVE-TYPE` is a valid [locative type][a11d].
    
    - `NAME` and `LOCATIVE-ARGS` are both [`SYMBOL`][e5af]s.
    
    The above are enforced at macro-expansion time.
    
    - `BODY` must follow the rules in [`*CHECK-LOCATE*`][b038].

<a id="x-28DREF-EXT-3ACALL-LOOKUP-20MGL-PAX-3AMACRO-29"></a>

- [macro] **CALL-LOOKUP** *NAME LOCATIVE-TYPE LOCATIVE-ARGS*

    Call the [lookup][49b5] for `LOCATIVE-TYPE` with `NAME`
    and `LOCATIVE-ARGS`.

<a id="x-28DREF-EXT-3ADEFINE-LOCATOR-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-LOCATOR** *LOCATIVE-TYPE ((OBJECT CLASS)) &BODY BODY*

    Define a method of finding the [definition][2143] with `LOCATIVE-TYPE` of
    instances of `CLASS`. When a locator's `BODY` is evaluated, `OBJECT` is
    bound to such an instance.
    
    ```
    (define-locator class ((class class))
      (make-instance 'class-dref :name (class-name class) :locative 'class))
    ```
    
    - `LOCATIVE-TYPE` is one of [`LISP-LOCATIVE-TYPES`][30ad]. This is because
      [`PSEUDO-LOCATIVE-TYPES`][c340] never [`RESOLVE`][63b4] to first-class objects.
    
    - `OBJECT` is a [`SYMBOL`][e5af].
    
    - `CLASS` names a [`CLASS`][1f37] that is not a subtype of
      [`XREF`][1538]. For how to convert definitions from one locative
      type to another, see [`DEFINE-CAST`][2066].
    
    The above are enforced at macro-expansion time.
    
    - `BODY` must follow the rules in [`*CHECK-LOCATE*`][b038].
    
    In contrast to when the [Initial Definition][87fc] is created from an
    `XREF` (see [`DEFINE-LOOKUP`][49b5]), here [`LOCATIVE-ARGS`][2444] are determined from
    `OBJECT`.

<a id="x-28DREF-EXT-3ACALL-LOCATOR-20MGL-PAX-3AMACRO-29"></a>

- [macro] **CALL-LOCATOR** *OBJECT LOCATIVE-TYPE*

    Call the [locator][16b6] for `LOCATIVE-TYPE` with `OBJECT`.

<a id="x-28DREF-EXT-3ADEFINE-CAST-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-CAST** *LOCATIVE-TYPE ((DREF DREF-CLASS)) &BODY BODY*

    Define a method of converting a [definition][2143] to another
    with `LOCATIVE-TYPE`. When a cast's `BODY` is evaluated, `DREF` is bound
    to an instance `DREF-CLASS`, which denotes a valid but potentially
    [non-canonical][9383] definition.
    
    Note the [Default Downcast][8529] often suffices, and defining a cast is
    only necessary if the [name][c68e] or the locative
    args change:
    
    ```
    (define-cast accessor ((dref reader-dref))
      (let ((name (dref-name dref))
            (class (second (dref-locative dref))))
        (when (ignore-errors (find-accessor-slot-definition name class))
          (make-instance 'accessor-dref :name name
                          :locative `(accessor ,class)))))
    ```
    
    - `LOCATIVE-TYPE` is a valid [locative type][a11d].
    
    - If `LOCATIVE-TYPE` is one of [`PSEUDO-LOCATIVE-TYPES`][c340], then `DREF-CLASS`
      must be of another pseudo locative type.
    
    - `DREF-CLASS` is either a direct *downcast* or an potentially
      non-direct *upcast*.
    
        - *Downcast:* In this case, `LOCATIVE-TYPE` is one of
          [`LOCATIVE-TYPE-DIRECT-SUBS`][130a] of (`DREF-CLASS-TO-LOCATIVE-TYPE`
          `DREF-CLASS`).
    
            Downcasting to non-direct subtypes is done in multiple
            steps. Consequently,the `BODY` of a downcast can rely on
            ([`CLASS-OF`][6a98] `DREF`) being [`CLASS`][1f37], not any subclass thereof.
    
        - *Upcast:* `LOCATIVE-TYPE` is different but reachable
          from (`DREF-CLASS-TO-LOCATIVE-TYPE` `DREF-CLASS`) by repeatedly
          choosing one of [`LOCATIVE-TYPE-DIRECT-SUPERS`][80a8]. Upcasting to
          non-direct supertypes is done in one step.
    
    The above are enforced at macro-expansion time.
    
    - `BODY` must follow the rules in [`*CHECK-LOCATE*`][b038], including those in
      [Cast Name Change][c68e].

<a id="x-28DREF-EXT-3ACALL-CAST-20MGL-PAX-3AMACRO-29"></a>

- [macro] **CALL-CAST** *LOCATIVE-TYPE DREF*

    Call the [cast][2066] to `LOCATIVE-TYPE` with `DREF`.

<a id="x-28DREF-EXT-3ALOCATE-ERROR-20FUNCTION-29"></a>

- [function] **LOCATE-ERROR** *&OPTIONAL FORMAT-CONTROL &REST FORMAT-ARGS*

    Call this function to signal a [`LOCATE-ERROR`][6334] condition from the
    [dynamic extent][36e9] of a [`LOCATE`][8f19] call, that is, from the `BODY`s
    of [`DEFINE-LOOKUP`][49b5], [`DEFINE-LOCATOR`][16b6] and [`DEFINE-CAST`][2066]. It is an error to
    call `LOCATE-ERROR` elsewhere.
    
    `FORMAT-CONTROL`, if non-`NIL`, is a [format control][b8d5] for which
    `FORMAT-ARGS` are suitable.

<a id="x-28DREF-EXT-3ACHECK-LOCATIVE-ARGS-20MGL-PAX-3AMACRO-29"></a>

- [macro] **CHECK-LOCATIVE-ARGS** *LOCATIVE-TYPE LOCATIVE-ARGS*

    Signal a [`LOCATE-ERROR`][6334] condition if `LOCATIVE-ARGS` do not match the
    `LAMBDA-LIST` argument of `LOCATIVE-TYPE` (not evaluated).

<a id="x-28DREF-EXT-3A-40EXTENDING-EVERYTHING-ELSE-20MGL-PAX-3ASECTION-29"></a>

### 8.5 Extending Everything Else

<a id="x-28DREF-EXT-3ARESOLVE-2A-20GENERIC-FUNCTION-29"></a>

- [generic-function] **RESOLVE\*** *DREF*

    Return the object defined by the definition `DREF`
    refers to. Signal a [`RESOLVE-ERROR`][58ba] condition by calling the
    [`RESOLVE-ERROR`][7825] function if the lookup fails.
    
    To keep [`RESOLVE`][63b4] a partial inverse of [`LOCATE`][8f19], [`DEFINE-LOCATOR`][16b6] may be
    necessary for `RESOLVE`able definitions. This function is for
    extending `RESOLVE`. Do not call it directly.
    
    It is an error for methods of this generic function to return an
    [`XREF`][1538].

<a id="x-28DREF-EXT-3ARESOLVE-ERROR-20FUNCTION-29"></a>

- [function] **RESOLVE-ERROR** *&REST FORMAT-AND-ARGS*

    Call this function to signal a [`RESOLVE-ERROR`][58ba] condition from the
    [dynamic extent][36e9] of a [`RESOLVE*`][d3b3] method. It is an error to call
    `RESOLVE-ERROR` elsewhere.
    
    `FORMAT-AND-ARGS`, if non-`NIL`, is a format string and arguments
    suitable for [`FORMAT`][ad78].

<a id="x-28DREF-EXT-3AMAP-DEFINITIONS-OF-NAME-20GENERIC-FUNCTION-29"></a>

- [generic-function] **MAP-DEFINITIONS-OF-NAME** *FN NAME LOCATIVE-TYPE*

    Call `FN` with [`DREF`][d930]s which can be [`LOCATE`][8f19]d
    with an `XREF`([`0`][1538] [`1`][cda7]) with `NAME`, `LOCATIVE-TYPE` and some [`LOCATIVE-ARGS`][2444]. The
    strange wording here is because there may be multiple ways (and thus
    `XREF`s) that refer to the same definition.
    
    For most locative types, there is at most one such definition, but
    for [`METHOD`][51c3], for example, there may be many. The default method
    simply does `(DREF NAME LOCATIVE-TYPE NIL)` and calls `FN` with result
    if [`DREF`][7e92] succeeds.
    
    `FN` must not be called with the same (under [`XREF=`][0617]) definition
    multiple times.
    
    This function is for extending [`DEFINITIONS`][e196] and [`DREF-APROPOS`][65b4]. Do not
    call it directly.

<a id="x-28DREF-EXT-3AMAP-DEFINITIONS-OF-TYPE-20GENERIC-FUNCTION-29"></a>

- [generic-function] **MAP-DEFINITIONS-OF-TYPE** *FN LOCATIVE-TYPE*

    Call `FN` with [`DREF`][d930]s which can be [`LOCATE`][8f19]d
    with an `XREF`([`0`][1538] [`1`][cda7]) with `LOCATIVE-TYPE` with some `NAME` and [`LOCATIVE-ARGS`][2444].
    
    The default method forms `XREF`s by combining each interned symbol as
    [name][5fc4]s with `LOCATIVE-TYPE` and no `LOCATIVE-ARGS` and calls `FN` if it
    `LOCATE`s a definition.
    
    `FN` may be called with `DREF`s that are [`XREF=`][0617] but differ in the `XREF` in
    their [`DREF-ORIGIN`][e742].
    
    This function is for extending [`DREF-APROPOS`][65b4]. Do not call it
    directly.

<a id="x-28DREF-EXT-3AARGLIST-2A-20GENERIC-FUNCTION-29"></a>

- [generic-function] **ARGLIST\*** *OBJECT*

    To extend [`ARGLIST`][e6bd], specialize `OBJECT` on a normal
    Lisp type or on a subclass of [`DREF`][d930].
    
    `ARGLIST` first calls `ARGLIST*` with its `OBJECT` argument. If that
    doesn't work (i.e. the second value returned is `NIL`), then it calls
    `ARGLIST*` with `OBJECT` either [`RESOLVE`][63b4]d (if it's a `DREF`) or [`LOCATE`][8f19]d (if
    it's not a `DREF`).
    
    - The default method returns `NIL`, `NIL`.
    
    - There is also a method specialized on [`DREF`s][d930], that looks
      up the [`DEFINITION-PROPERTY`][5f91] called `ARGLIST` and returns its value
      with [`VALUES-LIST`][dbd4]. Thus, an arglist and its kind can be specified
      with something like
    
        ```
        (setf (definition-property xref 'arglist)
              (list arglist :destructuring))
        ```
    
    This function is for extension only. Do not call it directly.

<a id="x-28DREF-EXT-3ADOCSTRING-2A-20GENERIC-FUNCTION-29"></a>

- [generic-function] **DOCSTRING\*** *OBJECT*

    To extend [`DOCSTRING`][affc], specialize `OBJECT` on a normal
    Lisp type or on a subclass of [`DREF`][d930].
    
    `DOCSTRING` first calls `DOCSTRING*` with its `OBJECT` argument. If that
    doesn't work (i.e. `NIL` is returned), then it calls `DOCSTRING*` with
    `OBJECT` either [`RESOLVE`][63b4]d (if it's a `DREF`) or [`LOCATE`][8f19]d (if it's not a
    `DREF`).
    
    - The default method returns `NIL`.
    
    - There is also a method specialized on [`DREF`s][d930], that looks
      up the [`DEFINITION-PROPERTY`][5f91] called `DOCSTRING` and returns its value
      with [`VALUES-LIST`][dbd4]. Thus, a docstring and a package can be specified
      with something like
    
        ```
        (setf (definition-property xref 'docstring)
              (list docstring *package*))
        ```
    
    This function is for extension only. Do not call it directly.

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-2A-20GENERIC-FUNCTION-29"></a>

- [generic-function] **SOURCE-LOCATION\*** *OBJECT*

    To extend [`SOURCE-LOCATION`][32da], specialize `OBJECT` on a
    normal Lisp type or on a subclass of [`DREF`][d930].
    
    `SOURCE-LOCATION` first calls `SOURCE-LOCATION*` with its `OBJECT`
    argument. If that doesn't work (i.e. `NIL` or `(:ERROR <MESSAGE>)` is
    returned), then it calls `SOURCE-LOCATION*` with `OBJECT` either
    [`RESOLVE`][63b4]d (if it's a `DREF`) or [`LOCATE`][8f19]d (if it's not a `DREF`).
    
    `SOURCE-LOCATION` returns the last of the `(:ERROR <MESSAGE>)`s
    encountered or a generic error message if only `NIL`s were returned.
    
    - The default method returns `NIL`.
    
    - There is also a method specialized on [`DREF`s][d930], that looks
      up the [`DEFINITION-PROPERTY`][5f91] called `SOURCE-LOCATION`. If present, it
      must be a function of no arguments that returns a source location
      or `NIL`. Typically, this is set up in the defining macro like this:
    
        ```
        (setf (definition-property xref 'source-location)
              (this-source-location))
        ```
    
    This function is for extension only. Do not call it directly.

<a id="x-28DREF-EXT-3A-40DEFINITION-PROPERTIES-20MGL-PAX-3ASECTION-29"></a>

#### 8.5.1 Definition Properties

Arbitrary data may be associated with definitions.
This mechanism is used by [`ARGLIST*`][0a96], [`DOCSTRING*`][9fd4] and
[`SOURCE-LOCATION*`][444d] for easy extension.

The following functions take an `XREF` argument and not a `DREF`([`0`][d930] [`1`][7e92]) to
allow working with [non-canonical][9383] or
non-existent definitions.

<a id="x-28DREF-EXT-3ADEFINITION-PROPERTY-20FUNCTION-29"></a>

- [function] **DEFINITION-PROPERTY** *XREF INDICATOR*

    Return the value of the property associated with `XREF` whose name
    is `EQL`([`0`][db03] [`1`][5fd4]) to `INDICATOR`. The second return value indicates whether the
    property was found. [`SETF`][a138]able.

<a id="x-28DREF-EXT-3ADELETE-DEFINITION-PROPERTY-20FUNCTION-29"></a>

- [function] **DELETE-DEFINITION-PROPERTY** *XREF INDICATOR*

    Delete the property associated with `XREF` whose name is `EQL`([`0`][db03] [`1`][5fd4]) to `INDICATOR`.
    Return true if the property was found.

<a id="x-28DREF-EXT-3ADEFINITION-PROPERTIES-20FUNCTION-29"></a>

- [function] **DEFINITION-PROPERTIES** *XREF*

    Return the properties of `XREF` as an association list.

<a id="x-28DREF-EXT-3ADELETE-DEFINITION-PROPERTIES-20FUNCTION-29"></a>

- [function] **DELETE-DEFINITION-PROPERTIES** *XREF*

    Delete all properties associated with `XREF`.

<a id="x-28DREF-EXT-3AMOVE-DEFINITION-PROPERTIES-20FUNCTION-29"></a>

- [function] **MOVE-DEFINITION-PROPERTIES** *FROM-XREF TO-XREF*

    Associate all properties of `FROM-XREF` with `TO-XREF`, as if readding
    them one-by-one with `(SETF DEFINITION-PROPERTY)`, and
    deleting them from `FROM-XREF` with [`DELETE-DEFINITION-PROPERTY`][09b7].

<a id="x-28DREF-EXT-3A-40DREF-CLASSES-20MGL-PAX-3ASECTION-29"></a>

### 8.6 `DREF-CLASS`es

These are the [`DREF-CLASS`][25be]es corresponding to [Basic Locative Types][1d1d].
They are exported to make it possible to go beyond the
[Basic Operations][662d] (e.g. [`PAX:DOCUMENT-OBJECT*`][8269]). For
[Defining Locative Types][f494], they are not necessary, as
[`DEFINE-LOCATIVE-TYPE`][b6c4] handles inheritance automatically based on its
`LOCATIVE-SUPERTYPES` argument.

**[for Variables][462c]**

<a id="x-28DREF-EXT-3AVARIABLE-DREF-20CLASS-29"></a>

- [class] **VARIABLE-DREF** *[DREF][d930]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`VARIABLE`][6c83].

<a id="x-28DREF-EXT-3ACONSTANT-DREF-20CLASS-29"></a>

- [class] **CONSTANT-DREF** *[VARIABLE-DREF][ad35]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`MGL-PAX:CONSTANT`][c819].

**[for Macros][d45d]**

<a id="x-28DREF-EXT-3AMACRO-DREF-20CLASS-29"></a>

- [class] **MACRO-DREF** *[DREF][d930]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`MGL-PAX:MACRO`][f3cc].

<a id="x-28DREF-EXT-3ASYMBOL-MACRO-DREF-20CLASS-29"></a>

- [class] **SYMBOL-MACRO-DREF** *[DREF][d930]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`MGL-PAX:SYMBOL-MACRO`][be85].

<a id="x-28DREF-EXT-3ACOMPILER-MACRO-DREF-20CLASS-29"></a>

- [class] **COMPILER-MACRO-DREF** *[DREF][d930]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`COMPILER-MACRO`][41fd].

<a id="x-28DREF-EXT-3ASETF-DREF-20CLASS-29"></a>

- [class] **SETF-DREF** *[DREF][d930]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`SETF`][d83a].

<a id="x-28DREF-EXT-3ASETF-COMPILER-MACRO-DREF-20CLASS-29"></a>

- [class] **SETF-COMPILER-MACRO-DREF** *[COMPILER-MACRO-DREF][59cf]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`DREF:SETF-COMPILER-MACRO`][5df4].

**[for Functions][1d59]**

<a id="x-28DREF-EXT-3AFUNCTION-DREF-20CLASS-29"></a>

- [class] **FUNCTION-DREF** *[DREF][d930]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`FUNCTION`][ba62].

<a id="x-28DREF-EXT-3ASETF-FUNCTION-DREF-20CLASS-29"></a>

- [class] **SETF-FUNCTION-DREF** *[FUNCTION-DREF][e576] [SETF-DREF][0db5]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`DREF:SETF-FUNCTION`][19f6].

<a id="x-28DREF-EXT-3AGENERIC-FUNCTION-DREF-20CLASS-29"></a>

- [class] **GENERIC-FUNCTION-DREF** *[FUNCTION-DREF][e576]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`GENERIC-FUNCTION`][5875].

<a id="x-28DREF-EXT-3ASETF-GENERIC-FUNCTION-DREF-20CLASS-29"></a>

- [class] **SETF-GENERIC-FUNCTION-DREF** *[GENERIC-FUNCTION-DREF][df33] [SETF-FUNCTION-DREF][798d]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`DREF:SETF-GENERIC-FUNCTION`][ab5e].

<a id="x-28DREF-EXT-3AMETHOD-DREF-20CLASS-29"></a>

- [class] **METHOD-DREF** *[DREF][d930]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`METHOD`][172e].

<a id="x-28DREF-EXT-3ASETF-METHOD-DREF-20CLASS-29"></a>

- [class] **SETF-METHOD-DREF** *[METHOD-DREF][2c45] [SETF-DREF][0db5]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`DREF:SETF-METHOD`][1a03].

<a id="x-28DREF-EXT-3AMETHOD-COMBINATION-DREF-20CLASS-29"></a>

- [class] **METHOD-COMBINATION-DREF** *[DREF][d930]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`METHOD-COMBINATION`][82e0].

<a id="x-28DREF-EXT-3AREADER-DREF-20CLASS-29"></a>

- [class] **READER-DREF** *[METHOD-DREF][2c45]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`MGL-PAX:READER`][cc04].

<a id="x-28DREF-EXT-3AWRITER-DREF-20CLASS-29"></a>

- [class] **WRITER-DREF** *[METHOD-DREF][2c45]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`MGL-PAX:WRITER`][e548].

<a id="x-28DREF-EXT-3AACCESSOR-DREF-20CLASS-29"></a>

- [class] **ACCESSOR-DREF** *[READER-DREF][ec6f] [WRITER-DREF][2638] [SETF-METHOD-DREF][5ab8]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`MGL-PAX:ACCESSOR`][00d4].

<a id="x-28DREF-EXT-3ASTRUCTURE-ACCESSOR-DREF-20CLASS-29"></a>

- [class] **STRUCTURE-ACCESSOR-DREF** *[SETF-FUNCTION-DREF][798d] [FUNCTION-DREF][e576]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`MGL-PAX:STRUCTURE-ACCESSOR`][090c].

**[for Types and Declarations][7a04]**

<a id="x-28DREF-EXT-3ATYPE-DREF-20CLASS-29"></a>

- [class] **TYPE-DREF** *[DREF][d930]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`TYPE`][926d].

<a id="x-28DREF-EXT-3ACLASS-DREF-20CLASS-29"></a>

- [class] **CLASS-DREF** *[TYPE-DREF][b4e9]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`CLASS`][2060].

<a id="x-28DREF-EXT-3ADECLARATION-DREF-20CLASS-29"></a>

- [class] **DECLARATION-DREF** *[DREF][d930]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`DECLARATION`][6e04].

**[for the Condition System][408d]**

<a id="x-28DREF-EXT-3ACONDITION-DREF-20CLASS-29"></a>

- [class] **CONDITION-DREF** *[CLASS-DREF][b3a7]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`CONDITION`][c479].

<a id="x-28DREF-EXT-3ARESTART-DREF-20CLASS-29"></a>

- [class] **RESTART-DREF** *[SYMBOL-LOCATIVE-DREF][34b9]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`RESTART`][e023].

**[for Packages and Readtables][c339]**

<a id="x-28DREF-EXT-3AASDF-SYSTEM-DREF-20CLASS-29"></a>

- [class] **ASDF-SYSTEM-DREF** *[DREF][d930]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`ASDF/SYSTEM:SYSTEM`][c097].

<a id="x-28DREF-EXT-3APACKAGE-DREF-20CLASS-29"></a>

- [class] **PACKAGE-DREF** *[DREF][d930]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`PACKAGE`][4dd7].

<a id="x-28DREF-EXT-3AREADTABLE-DREF-20CLASS-29"></a>

- [class] **READTABLE-DREF** *[DREF][d930]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`READTABLE`][7506].

**[for Unknown Definitions][58f1]**

<a id="x-28DREF-EXT-3AUNKNOWN-DREF-20CLASS-29"></a>

- [class] **UNKNOWN-DREF** *[DREF][d930]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`MGL-PAX:UNKNOWN`][a951].

**[for DRef Constructs][da93]**

<a id="x-28DREF-EXT-3ADTYPE-DREF-20CLASS-29"></a>

- [class] **DTYPE-DREF** *[DREF][d930]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`DREF:DTYPE`][85ba].

<a id="x-28DREF-EXT-3ALOCATIVE-DREF-20CLASS-29"></a>

- [class] **LOCATIVE-DREF** *[DTYPE-DREF][6aa6]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`MGL-PAX:LOCATIVE`][0b3a].

<a id="x-28DREF-EXT-3ASYMBOL-LOCATIVE-DREF-20CLASS-29"></a>

- [class] **SYMBOL-LOCATIVE-DREF** *[DREF][d930]*

    All [locative type][a11d]s defined with
    [`DEFINE-SYMBOL-LOCATIVE-TYPE`][ee9b] inherit from this class.

<a id="x-28DREF-EXT-3ALAMBDA-DREF-20CLASS-29"></a>

- [class] **LAMBDA-DREF** *[DREF][d930]*

    [`DREF-EXT:DREF-CLASS`][25be] of [`LAMBDA`][4796].

<a id="x-28DREF-EXT-3A-40SOURCE-LOCATIONS-20MGL-PAX-3ASECTION-29"></a>

### 8.7 Source Locations

These represent the file or buffer position of a [defining
form][23a8] and are returned by the [`SOURCE-LOCATION`][32da] function. For
the details, see the Elisp function `slime-goto-source-location`.

<a id="x-28DREF-EXT-3AMAKE-SOURCE-LOCATION-20FUNCTION-29"></a>

- [function] **MAKE-SOURCE-LOCATION** *&KEY FILE FILE-POSITION BUFFER BUFFER-POSITION SNIPPET*

    Make a Swank source location. The ultimate reference is `slime.el`.
    When `SNIPPET` is provided, the match nearest to `FILE-POSITION` is
    determined (see the Elisp `slime-isearch` and
    [`SOURCE-LOCATION-ADJUSTED-FILE-POSITION`][daacd]).

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-P-20FUNCTION-29"></a>

- [function] **SOURCE-LOCATION-P** *OBJECT*

    See if `OBJECT` is a source location object.

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-FILE-20FUNCTION-29"></a>

- [function] **SOURCE-LOCATION-FILE** *LOCATION*

    Return the name of the file of the [defining form][23a8].
    This may be `NIL`, for example, if `LOCATION` is of a [defining
    form][23a8] that was entered at the REPL, or compiled in the
    `*slime-scratch*` buffer.

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-FILE-POSITION-20FUNCTION-29"></a>

- [function] **SOURCE-LOCATION-FILE-POSITION** *LOCATION*

    Return the file position of the [defining form][23a8] or `NIL`
    if it's not available. The first position is 0.

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-BUFFER-20FUNCTION-29"></a>

- [function] **SOURCE-LOCATION-BUFFER** *LOCATION*

    Return the name of the Emacs buffer of the [defining form][23a8] or
    `NIL` if there is no such Emacs buffer.

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-BUFFER-POSITION-20FUNCTION-29"></a>

- [function] **SOURCE-LOCATION-BUFFER-POSITION** *LOCATION*

    Return the position of the [defining form][23a8] in
    [`SOURCE-LOCATION-BUFFER`][39c2] or `NIL` if it's not available. The first
    position is 1.

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-SNIPPET-20FUNCTION-29"></a>

- [function] **SOURCE-LOCATION-SNIPPET** *LOCATION*

    Return the [defining form][23a8] or a prefix of it as a string or `NIL`
    if it's not available.

<a id="x-28DREF-EXT-3ASOURCE-LOCATION-ADJUSTED-FILE-POSITION-20FUNCTION-29"></a>

- [function] **SOURCE-LOCATION-ADJUSTED-FILE-POSITION** *LOCATION*

    Return the actual file position `LOCATION` points to allowing for 
    some deviation from the raw [`SOURCE-LOCATION-FILE-POSITION`][be18], which is
    adjusted by searching for the nearest occurrence of
    [`SOURCE-LOCATION-SNIPPET`][6ec3] in the file. Needless to say, this can be a
    very expensive operation.
    
    If [`SOURCE-LOCATION-FILE`][ae5a] is `NIL`, `NIL` is returned. If there is no
    snippet, or it doesn't match, then `SOURCE-LOCATION-FILE-POSITION` (or
    0 if that's `NIL`) is returned.
    
    This is a non-interactive companion to the Elisp function
    `slime-location-offset`, supporting only file positions and
    non-partial matching of snippets.

<a id="x-28DREF-EXT-3ATHIS-SOURCE-LOCATION-20MGL-PAX-3AMACRO-29"></a>

- [macro] **THIS-SOURCE-LOCATION**

    The value of this macro form is a function of no arguments that
    returns its own [`SOURCE-LOCATION`][32da].

  [006c]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_4.htm "DEFINE-METHOD-COMBINATION (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [00d4]: #x-28MGL-PAX-3AACCESSOR-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:ACCESSOR MGL-PAX:LOCATIVE"
  [021a]: #x-28-22dref-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "\"dref\" ASDF/SYSTEM:SYSTEM"
  [023a]: http://www.lispworks.com/documentation/HyperSpec/Issues/iss049_w.htm "\"ISSUE:CLOS-CONDITIONS\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [059c]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_o.htm#ordinary_lambda_list "\"ordinary lambda list\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [0617]: #x-28DREF-3AXREF-3D-20FUNCTION-29 "DREF:XREF= FUNCTION"
  [090c]: #x-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:STRUCTURE-ACCESSOR MGL-PAX:LOCATIVE"
  [0976]: #x-28DREF-3ADREF-LOCATIVE-ARGS-20FUNCTION-29 "DREF:DREF-LOCATIVE-ARGS FUNCTION"
  [09b7]: #x-28DREF-EXT-3ADELETE-DEFINITION-PROPERTY-20FUNCTION-29 "DREF-EXT:DELETE-DEFINITION-PROPERTY FUNCTION"
  [0a96]: #x-28DREF-EXT-3AARGLIST-2A-20GENERIC-FUNCTION-29 "DREF-EXT:ARGLIST* GENERIC-FUNCTION"
  [0b3a]: #x-28MGL-PAX-3ALOCATIVE-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:LOCATIVE MGL-PAX:LOCATIVE"
  [0c7e]: #x-28-22dref-2Ffull-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "\"dref/full\" ASDF/SYSTEM:SYSTEM"
  [0d07]: http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_2.htm "SYMBOL-NAME (MGL-PAX:CLHS FUNCTION)"
  [0db5]: #x-28DREF-EXT-3ASETF-DREF-20CLASS-29 "DREF-EXT:SETF-DREF CLASS"
  [0fa3]: ../README.md#x-28MGL-PAX-3A-40LOCATIVE-ALIASES-20MGL-PAX-3ASECTION-29 "Locative Aliases"
  [0ff7]: http://www.lispworks.com/documentation/HyperSpec/Body/04_.htm "\"4\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [10a7]: #x-28DREF-EXT-3ACHECK-LOCATIVE-ARGS-20MGL-PAX-3AMACRO-29 "DREF-EXT:CHECK-LOCATIVE-ARGS MGL-PAX:MACRO"
  [119e]: http://www.lispworks.com/documentation/HyperSpec/Body/t_fn.htm "FUNCTION (MGL-PAX:CLHS CLASS)"
  [12814]: #x-28DREF-3ADEFSTRUCT-2A-20MGL-PAX-3AMACRO-29 "DREF:DEFSTRUCT* MGL-PAX:MACRO"
  [130a]: #x-28DREF-EXT-3ALOCATIVE-TYPE-DIRECT-SUBS-20FUNCTION-29 "DREF-EXT:LOCATIVE-TYPE-DIRECT-SUBS FUNCTION"
  [1490]: #x-28DREF-3AXREF-LOCATIVE-ARGS-20FUNCTION-29 "DREF:XREF-LOCATIVE-ARGS FUNCTION"
  [14cb]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defmac.htm "DEFMACRO (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [14ea]: #x-28DREF-3A-40REFERENCES-20MGL-PAX-3ASECTION-29 "References"
  [1538]: #x-28DREF-3AXREF-20CLASS-29 "DREF:XREF CLASS"
  [1574]: http://www.lispworks.com/documentation/HyperSpec/Body/s_declar.htm "DECLARE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [16b6]: #x-28DREF-EXT-3ADEFINE-LOCATOR-20MGL-PAX-3AMACRO-29 "DREF-EXT:DEFINE-LOCATOR MGL-PAX:MACRO"
  [172e]: #x-28METHOD-20MGL-PAX-3ALOCATIVE-29 "METHOD MGL-PAX:LOCATIVE"
  [19f6]: #x-28DREF-3ASETF-FUNCTION-20MGL-PAX-3ALOCATIVE-29 "DREF:SETF-FUNCTION MGL-PAX:LOCATIVE"
  [1a03]: #x-28DREF-3ASETF-METHOD-20MGL-PAX-3ALOCATIVE-29 "DREF:SETF-METHOD MGL-PAX:LOCATIVE"
  [1d1d]: #x-28DREF-3A-40BASIC-LOCATIVE-TYPES-20MGL-PAX-3ASECTION-29 "Basic Locative Types"
  [1d59]: #x-28DREF-3A-40FUNCTIONLIKE-LOCATIVES-20MGL-PAX-3ASECTION-29 "Locatives for Functions and Methods"
  [1d5a]: http://www.lispworks.com/documentation/HyperSpec/Body/t_pkg.htm "PACKAGE (MGL-PAX:CLHS CLASS)"
  [1e36]: #x-28DREF-3ADREF-NAME-20-28MGL-PAX-3AREADER-20DREF-3ADREF-29-29 "DREF:DREF-NAME (MGL-PAX:READER DREF:DREF)"
  [1f37]: http://www.lispworks.com/documentation/HyperSpec/Body/t_class.htm "CLASS (MGL-PAX:CLHS CLASS)"
  [2060]: #x-28CLASS-20MGL-PAX-3ALOCATIVE-29 "CLASS MGL-PAX:LOCATIVE"
  [2066]: #x-28DREF-EXT-3ADEFINE-CAST-20MGL-PAX-3AMACRO-29 "DREF-EXT:DEFINE-CAST MGL-PAX:MACRO"
  [2143]: #x-28DREF-3A-40DEFINITION-20MGL-PAX-3AGLOSSARY-TERM-29 "definition"
  [23a8]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#defining_form "\"defining form\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [23d5]: http://www.lispworks.com/documentation/HyperSpec/Body/m_define.htm "DEFINE-COMPILER-MACRO (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [2415]: ../README.md "PAX Manual"
  [2444]: #x-28DREF-EXT-3ALOCATIVE-ARGS-20FUNCTION-29 "DREF-EXT:LOCATIVE-ARGS FUNCTION"
  [25be]: #x-28DREF-EXT-3ADREF-CLASS-20FUNCTION-29 "DREF-EXT:DREF-CLASS FUNCTION"
  [2638]: #x-28DREF-EXT-3AWRITER-DREF-20CLASS-29 "DREF-EXT:WRITER-DREF CLASS"
  [292a]: ../README.md#x-28MGL-PAX-3A-40PAX-LOCATIVES-20MGL-PAX-3ASECTION-29 "PAX Locatives"
  [29a1]: http://www.lispworks.com/documentation/HyperSpec/Body/07_fb.htm "\"7.6.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [2ab8]: #x-28DREF-EXT-3ACALL-LOOKUP-20MGL-PAX-3AMACRO-29 "DREF-EXT:CALL-LOOKUP MGL-PAX:MACRO"
  [2b8b]: http://www.lispworks.com/documentation/HyperSpec/Body/t_satisf.htm "SATISFIES (MGL-PAX:CLHS TYPE)"
  [2c45]: #x-28DREF-EXT-3AMETHOD-DREF-20CLASS-29 "DREF-EXT:METHOD-DREF CLASS"
  [2ff3]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equalp.htm "EQUALP (MGL-PAX:CLHS FUNCTION)"
  [30ad]: #x-28DREF-3ALISP-LOCATIVE-TYPES-20FUNCTION-29 "DREF:LISP-LOCATIVE-TYPES FUNCTION"
  [32da]: #x-28DREF-3ASOURCE-LOCATION-20FUNCTION-29 "DREF:SOURCE-LOCATION FUNCTION"
  [3301]: #x-28DREF-3ATOP-20DREF-3ADTYPE-29 "DREF:TOP DREF:DTYPE"
  [34b9]: #x-28DREF-EXT-3ASYMBOL-LOCATIVE-DREF-20CLASS-29 "DREF-EXT:SYMBOL-LOCATIVE-DREF CLASS"
  [35a2]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#setf_expander "\"setf expander\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [36e9]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#dynamic_extent "\"dynamic extent\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [378f]: ../README.md#x-28MGL-PAX-3A-40PARSING-20MGL-PAX-3ASECTION-29 "Parsing"
  [38e4]: http://www.lispworks.com/documentation/HyperSpec/Body/t_rst.htm "RESTART (MGL-PAX:CLHS CLASS)"
  [39c2]: #x-28DREF-EXT-3ASOURCE-LOCATION-BUFFER-20FUNCTION-29 "DREF-EXT:SOURCE-LOCATION-BUFFER FUNCTION"
  [3b96]: #x-28DREF-EXT-3ADEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29 "DREF-EXT:DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE MGL-PAX:MACRO"
  [3bdc]: #x-28DREF-EXT-3AMAKE-SOURCE-LOCATION-20FUNCTION-29 "DREF-EXT:MAKE-SOURCE-LOCATION FUNCTION"
  [3c8a]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_o.htm#object "\"object\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [3fb5]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equal.htm "EQUAL (MGL-PAX:CLHS FUNCTION)"
  [408d]: #x-28DREF-3A-40CONDITION-SYSTEM-LOCATIVES-20MGL-PAX-3ASECTION-29 "Locatives for the Condition System"
  [41fd]: #x-28COMPILER-MACRO-20MGL-PAX-3ALOCATIVE-29 "COMPILER-MACRO MGL-PAX:LOCATIVE"
  [432c]: ../README.md#x-28MGL-PAX-3ADOCUMENT-20FUNCTION-29 "MGL-PAX:DOCUMENT FUNCTION"
  [4336]: http://www.lispworks.com/documentation/HyperSpec/Body/03_da.htm "\"3.4.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [43bd]: #x-28DREF-3A-40REFERENCE-20MGL-PAX-3AGLOSSARY-TERM-29 "reference"
  [444d]: #x-28DREF-EXT-3ASOURCE-LOCATION-2A-20GENERIC-FUNCTION-29 "DREF-EXT:SOURCE-LOCATION* GENERIC-FUNCTION"
  [462c]: #x-28DREF-3A-40VARIABLELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29 "Locatives for Variables"
  [46c0]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_1.htm "DEFINE-SYMBOL-MACRO (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [4796]: #x-28LAMBDA-20MGL-PAX-3ALOCATIVE-29 "LAMBDA MGL-PAX:LOCATIVE"
  [49b5]: #x-28DREF-EXT-3ADEFINE-LOOKUP-20MGL-PAX-3AMACRO-29 "DREF-EXT:DEFINE-LOOKUP MGL-PAX:MACRO"
  [4b93]: http://www.lispworks.com/documentation/HyperSpec/Body/f_pkg_ni.htm "PACKAGE-NICKNAMES (MGL-PAX:CLHS FUNCTION)"
  [4c16]: #x-28DREF-EXT-3A-40EXTENDING-LOCATE-20MGL-PAX-3ASECTION-29 "Extending `LOCATE`"
  [4dc9]: http://www.lispworks.com/documentation/HyperSpec/Body/f_find_p.htm "FIND-PACKAGE (MGL-PAX:CLHS FUNCTION)"
  [4dd7]: #x-28PACKAGE-20MGL-PAX-3ALOCATIVE-29 "PACKAGE MGL-PAX:LOCATIVE"
  [5119]: ../README.md#x-28MGL-PAX-3AGLOSSARY-TERM-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:GLOSSARY-TERM MGL-PAX:LOCATIVE"
  [5152]: http://www.lispworks.com/documentation/HyperSpec/Body/f_atom.htm "ATOM (MGL-PAX:CLHS FUNCTION)"
  [5191]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#function_name "\"function name\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [51c3]: http://www.lispworks.com/documentation/HyperSpec/Body/t_method.htm "METHOD (MGL-PAX:CLHS CLASS)"
  [5406]: http://www.lispworks.com/documentation/HyperSpec/Body/f_abortc.htm "USE-VALUE (MGL-PAX:CLHS FUNCTION)"
  [5447]: #x-28DREF-3AXREF-NAME-20-28MGL-PAX-3AREADER-20DREF-3AXREF-29-29 "DREF:XREF-NAME (MGL-PAX:READER DREF:XREF)"
  [548e]: #x-28DREF-EXT-3ADEFINE-LOCATIVE-ALIAS-20MGL-PAX-3AMACRO-29 "DREF-EXT:DEFINE-LOCATIVE-ALIAS MGL-PAX:MACRO"
  [5875]: #x-28GENERIC-FUNCTION-20MGL-PAX-3ALOCATIVE-29 "GENERIC-FUNCTION MGL-PAX:LOCATIVE"
  [58ba]: #x-28DREF-EXT-3ARESOLVE-ERROR-20CONDITION-29 "DREF-EXT:RESOLVE-ERROR CONDITION"
  [58f1]: #x-28DREF-3A-40UNKNOWN-DEFINITIONS-20MGL-PAX-3ASECTION-29 "Locatives for Unknown Definitions"
  [59c9]: #x-28DREF-EXT-3A-40SYMBOL-LOCATIVES-20MGL-PAX-3ASECTION-29 "Symbol Locatives"
  [59cf]: #x-28DREF-EXT-3ACOMPILER-MACRO-DREF-20CLASS-29 "DREF-EXT:COMPILER-MACRO-DREF CLASS"
  [5a82]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm "EQ (MGL-PAX:CLHS FUNCTION)"
  [5ab8]: #x-28DREF-EXT-3ASETF-METHOD-DREF-20CLASS-29 "DREF-EXT:SETF-METHOD-DREF CLASS"
  [5cd7]: ../README.md#x-28MGL-PAX-3AINCLUDE-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:INCLUDE MGL-PAX:LOCATIVE"
  [5df4]: #x-28DREF-3ASETF-COMPILER-MACRO-20MGL-PAX-3ALOCATIVE-29 "DREF:SETF-COMPILER-MACRO MGL-PAX:LOCATIVE"
  [5ed1]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pkg.htm "*PACKAGE* (MGL-PAX:CLHS VARIABLE)"
  [5f91]: #x-28DREF-EXT-3ADEFINITION-PROPERTY-20FUNCTION-29 "DREF-EXT:DEFINITION-PROPERTY FUNCTION"
  [5fc4]: #x-28DREF-3A-40NAME-20MGL-PAX-3AGLOSSARY-TERM-29 "name"
  [5fd4]: http://www.lispworks.com/documentation/HyperSpec/Body/t_eql.htm "EQL (MGL-PAX:CLHS TYPE)"
  [6067]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#destructuring_lambda_list "\"destructuring lambda list\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [609c]: http://www.lispworks.com/documentation/HyperSpec/Body/f_fmakun.htm "FMAKUNBOUND (MGL-PAX:CLHS FUNCTION)"
  [62e7]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#dynamic_environment "\"dynamic environment\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [6334]: #x-28DREF-EXT-3ALOCATE-ERROR-20CONDITION-29 "DREF-EXT:LOCATE-ERROR CONDITION"
  [6354]: #x-28DREF-EXT-3A-40DREF-CLASSES-20MGL-PAX-3ASECTION-29 "`DREF-CLASS`es"
  [63b4]: #x-28DREF-3ARESOLVE-20FUNCTION-29 "DREF:RESOLVE FUNCTION"
  [65b4]: #x-28DREF-3ADREF-APROPOS-20FUNCTION-29 "DREF:DREF-APROPOS FUNCTION"
  [662d]: #x-28DREF-3A-40BASIC-OPERATIONS-20MGL-PAX-3ASECTION-29 "Basic Operations"
  [66dc]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defset.htm "DEFSETF (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [672f]: ../README.md#x-28MGL-PAX-3ASECTION-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:SECTION MGL-PAX:LOCATIVE"
  [6832]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defmet.htm "DEFMETHOD (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [68b4]: #x-28DREF-EXT-3ADEFINE-PSEUDO-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29 "DREF-EXT:DEFINE-PSEUDO-LOCATIVE-TYPE MGL-PAX:MACRO"
  [68fb]: #x-28DREF-EXT-3A-40EXTENDING-DREF-20MGL-PAX-3ASECTION-29 "Extending DRef"
  [6932]: #x-28DREF-EXT-3ALOCATE-ERROR-20FUNCTION-29 "DREF-EXT:LOCATE-ERROR FUNCTION"
  [6a98]: http://www.lispworks.com/documentation/HyperSpec/Body/f_clas_1.htm "CLASS-OF (MGL-PAX:CLHS FUNCTION)"
  [6aa6]: #x-28DREF-EXT-3ADTYPE-DREF-20CLASS-29 "DREF-EXT:DTYPE-DREF CLASS"
  [6c83]: #x-28VARIABLE-20MGL-PAX-3ALOCATIVE-29 "VARIABLE MGL-PAX:LOCATIVE"
  [6d46]: http://www.lispworks.com/documentation/HyperSpec/Body/f_find_m.htm "FIND-METHOD (MGL-PAX:CLHS GENERIC-FUNCTION)"
  [6e04]: #x-28DECLARATION-20MGL-PAX-3ALOCATIVE-29 "DECLARATION MGL-PAX:LOCATIVE"
  [6e6e]: http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_pkg.htm "MAKE-PACKAGE (MGL-PAX:CLHS FUNCTION)"
  [6ec3]: #x-28DREF-EXT-3ASOURCE-LOCATION-SNIPPET-20FUNCTION-29 "DREF-EXT:SOURCE-LOCATION-SNIPPET FUNCTION"
  [6f15]: #x-28DREF-3A-40DISSECTING-REFERENCES-20MGL-PAX-3ASECTION-29 "Dissecting References"
  [6f95]: http://www.lispworks.com/documentation/HyperSpec/Body/f_file_p.htm "FILE-POSITION (MGL-PAX:CLHS FUNCTION)"
  [7328]: http://www.lispworks.com/documentation/HyperSpec/Body/f_apropo.htm "APROPOS-LIST (MGL-PAX:CLHS FUNCTION)"
  [7334]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defpar.htm "DEFVAR (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [7506]: #x-28READTABLE-20MGL-PAX-3ALOCATIVE-29 "READTABLE MGL-PAX:LOCATIVE"
  [7825]: #x-28DREF-EXT-3ARESOLVE-ERROR-20FUNCTION-29 "DREF-EXT:RESOLVE-ERROR FUNCTION"
  [793d]: #x-28DREF-EXT-3A-40EXTENDING-EVERYTHING-ELSE-20MGL-PAX-3ASECTION-29 "Extending Everything Else"
  [798d]: #x-28DREF-EXT-3ASETF-FUNCTION-DREF-20CLASS-29 "DREF-EXT:SETF-FUNCTION-DREF CLASS"
  [7a04]: #x-28DREF-3A-40TYPELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29 "Locatives for Types and Declarations"
  [7ac8]: #x-28DREF-3A-40LOCATIVE-20MGL-PAX-3AGLOSSARY-TERM-29 "locative"
  [7c9f]: http://www.lispworks.com/documentation/HyperSpec/Body/d_type.htm "TYPE (MGL-PAX:CLHS DECLARATION)"
  [7e92]: #x-28DREF-3ADREF-20FUNCTION-29 "DREF:DREF FUNCTION"
  [7f9a]: http://www.lispworks.com/documentation/HyperSpec/Body/m_deftp.htm "DEFTYPE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [80a8]: #x-28DREF-EXT-3ALOCATIVE-TYPE-DIRECT-SUPERS-20FUNCTION-29 "DREF-EXT:LOCATIVE-TYPE-DIRECT-SUPERS FUNCTION"
  [817d]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#deftype_lambda_list "\"deftype lambda list\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [81f7]: http://www.lispworks.com/documentation/HyperSpec/Body/s_fn.htm "FUNCTION (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [8269]: ../README.md#x-28MGL-PAX-3ADOCUMENT-OBJECT-2A-20GENERIC-FUNCTION-29 "MGL-PAX:DOCUMENT-OBJECT* GENERIC-FUNCTION"
  [82ae]: http://www.lispworks.com/documentation/HyperSpec/Body/t_mem_m.htm "MEMBER (MGL-PAX:CLHS FUNCTION)"
  [82e0]: #x-28METHOD-COMBINATION-20MGL-PAX-3ALOCATIVE-29 "METHOD-COMBINATION MGL-PAX:LOCATIVE"
  [840e]: #x-28DREF-3AXREF-LOCATIVE-TYPE-20FUNCTION-29 "DREF:XREF-LOCATIVE-TYPE FUNCTION"
  [8529]: #x-28DREF-EXT-3A-40DEFAULT-DOWNCAST-20MGL-PAX-3ASECTION-29 "Default Downcast"
  [852d]: #x-28DREF-3A-40REFERENCES-GLOSSARY-20MGL-PAX-3ASECTION-29 "References Glossary"
  [85ba]: #x-28DREF-3ADTYPE-20MGL-PAX-3ALOCATIVE-29 "DREF:DTYPE MGL-PAX:LOCATIVE"
  [867c]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#setf_function_name "\"setf function name\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [87fc]: #x-28DREF-EXT-3A-40INITIAL-DEFINITION-20MGL-PAX-3ASECTION-29 "Initial Definition"
  [8934]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defcon.htm "DEFCONSTANT (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [8f19]: #x-28DREF-3ALOCATE-20FUNCTION-29 "DREF:LOCATE FUNCTION"
  [926d]: #x-28TYPE-20MGL-PAX-3ALOCATIVE-29 "TYPE MGL-PAX:LOCATIVE"
  [9383]: #x-28DREF-EXT-3A-40CANONICALIZATION-20MGL-PAX-3ASECTION-29 "Canonicalization"
  [943a]: #x-28DREF-3APSEUDO-20DREF-3ADTYPE-29 "DREF:PSEUDO DREF:DTYPE"
  [94d1]: #x-28DREF-3ALOCATIVE-ALIASES-20FUNCTION-29 "DREF:LOCATIVE-ALIASES FUNCTION"
  [954a]: http://www.lispworks.com/documentation/HyperSpec/Body/t_not.htm "NOT (MGL-PAX:CLHS TYPE)"
  [963f]: #x-28DREF-3ADTYPEP-20FUNCTION-29 "DREF:DTYPEP FUNCTION"
  [97b4]: #x-28DREF-EXT-3AMAP-DEFINITIONS-OF-NAME-20GENERIC-FUNCTION-29 "DREF-EXT:MAP-DEFINITIONS-OF-NAME GENERIC-FUNCTION"
  [97ba]: #x-28DREF-EXT-3ALOCATIVE-TYPE-20FUNCTION-29 "DREF-EXT:LOCATIVE-TYPE FUNCTION"
  [99b0]: #x-28DREF-3ALOCATIVE-TYPES-20FUNCTION-29 "DREF:LOCATIVE-TYPES FUNCTION"
  [99b05]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#setf_function "\"setf function\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [9a71]: http://www.lispworks.com/documentation/HyperSpec/Body/f_specia.htm "SPECIAL-OPERATOR-P (MGL-PAX:CLHS FUNCTION)"
  [9b43]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defpkg.htm "DEFPACKAGE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [9b70]: http://www.lispworks.com/documentation/HyperSpec/Body/t_meth_1.htm "METHOD-COMBINATION (MGL-PAX:CLHS CLASS)"
  [9bf9]: #x-28DREF-3A-40REVERSE-DEFINITION-ORDER-20MGL-PAX-3AGLOSSARY-TERM-29 "reverse definition order"
  [9caa]: http://www.lispworks.com/documentation/HyperSpec/Body/f_tp_of.htm "TYPE-OF (MGL-PAX:CLHS FUNCTION)"
  [9d60]: #x-28DREF-EXT-3A-40EXTENSION-TUTORIAL-20MGL-PAX-3ASECTION-29 "Extension Tutorial"
  [9fd4]: #x-28DREF-EXT-3ADOCSTRING-2A-20GENERIC-FUNCTION-29 "DREF-EXT:DOCSTRING* GENERIC-FUNCTION"
  [a078]: #x-28DREF-EXT-3A-40SOURCE-LOCATIONS-20MGL-PAX-3ASECTION-29 "Source Locations"
  [a11d]: #x-28DREF-3A-40LOCATIVE-TYPE-20MGL-PAX-3AGLOSSARY-TERM-29 "locative type"
  [a138]: http://www.lispworks.com/documentation/HyperSpec/Body/m_setf_.htm "SETF (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [a22e]: #x-28DREF-3ADREF-LOCATIVE-TYPE-20FUNCTION-29 "DREF:DREF-LOCATIVE-TYPE FUNCTION"
  [a26f]: http://www.lispworks.com/documentation/HyperSpec/Body/f_consta.htm "CONSTANTP (MGL-PAX:CLHS FUNCTION)"
  [a459]: #x-28DREF-3A-40DTYPES-20MGL-PAX-3ASECTION-29 "`DTYPE`s"
  [a541]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_to_.htm "PRINC-TO-STRING (MGL-PAX:CLHS FUNCTION)"
  [a584]: #x-28DREF-3A-40LINKS-AND-SYSTEMS-20MGL-PAX-3ASECTION-29 "Links and Systems"
  [a657]: http://www.lispworks.com/documentation/HyperSpec/Body/t_atom.htm "ATOM (MGL-PAX:CLHS TYPE)"
  [a70d]: #x-28DREF-3AXREF-LOCATIVE-20-28MGL-PAX-3AREADER-20DREF-3AXREF-29-29 "DREF:XREF-LOCATIVE (MGL-PAX:READER DREF:XREF)"
  [a79d]: http://www.lispworks.com/documentation/HyperSpec/Body/t_member.htm "MEMBER (MGL-PAX:CLHS TYPE)"
  [a951]: #x-28MGL-PAX-3AUNKNOWN-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:UNKNOWN MGL-PAX:LOCATIVE"
  [ab5e]: #x-28DREF-3ASETF-GENERIC-FUNCTION-20MGL-PAX-3ALOCATIVE-29 "DREF:SETF-GENERIC-FUNCTION MGL-PAX:LOCATIVE"
  [ad35]: #x-28DREF-EXT-3AVARIABLE-DREF-20CLASS-29 "DREF-EXT:VARIABLE-DREF CLASS"
  [ad78]: http://www.lispworks.com/documentation/HyperSpec/Body/f_format.htm "FORMAT (MGL-PAX:CLHS FUNCTION)"
  [ad80]: #x-28DREF-3A-40INTRODUCTION-20MGL-PAX-3ASECTION-29 "Introduction"
  [adc7]: #x-28DREF-EXT-3A-40DEFINING-LOOKUPS-LOCATORS-AND-CASTS-20MGL-PAX-3ASECTION-29 "Defining Lookups, Locators and Casts"
  [ade6]: #x-28DREF-3A-40PRESENTATION-20MGL-PAX-3AGLOSSARY-TERM-29 "presentation"
  [ae5a]: #x-28DREF-EXT-3ASOURCE-LOCATION-FILE-20FUNCTION-29 "DREF-EXT:SOURCE-LOCATION-FILE FUNCTION"
  [affc]: #x-28MGL-PAX-3ADOCSTRING-20FUNCTION-29 "MGL-PAX:DOCSTRING FUNCTION"
  [b038]: #x-28DREF-EXT-3A-2ACHECK-LOCATE-2A-20VARIABLE-29 "DREF-EXT:*CHECK-LOCATE* VARIABLE"
  [b3a7]: #x-28DREF-EXT-3ACLASS-DREF-20CLASS-29 "DREF-EXT:CLASS-DREF CLASS"
  [b4e9]: #x-28DREF-EXT-3ATYPE-DREF-20CLASS-29 "DREF-EXT:TYPE-DREF CLASS"
  [b6c4]: #x-28DREF-EXT-3ADEFINE-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29 "DREF-EXT:DEFINE-LOCATIVE-TYPE MGL-PAX:MACRO"
  [b7fc]: ../README.md#x-28MGL-PAX-3A-40APROPOS-20MGL-PAX-3ASECTION-29 "Apropos"
  [b8d5]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#format_control "\"format control\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [b93c]: http://www.lispworks.com/documentation/HyperSpec/Body/t_string.htm "STRING (MGL-PAX:CLHS CLASS)"
  [ba62]: #x-28FUNCTION-20MGL-PAX-3ALOCATIVE-29 "FUNCTION MGL-PAX:LOCATIVE"
  [bb23]: #x-28DREF-3ADEFINE-RESTART-20MGL-PAX-3AMACRO-29 "DREF:DEFINE-RESTART MGL-PAX:MACRO"
  [be18]: #x-28DREF-EXT-3ASOURCE-LOCATION-FILE-POSITION-20FUNCTION-29 "DREF-EXT:SOURCE-LOCATION-FILE-POSITION FUNCTION"
  [be85]: #x-28MGL-PAX-3ASYMBOL-MACRO-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:SYMBOL-MACRO MGL-PAX:LOCATIVE"
  [c097]: #x-28ASDF-2FSYSTEM-3ASYSTEM-20MGL-PAX-3ALOCATIVE-29 "ASDF/SYSTEM:SYSTEM MGL-PAX:LOCATIVE"
  [c339]: #x-28DREF-3A-40PACKAGELIKE-LOCATIVES-20MGL-PAX-3ASECTION-29 "Locatives for Packages and Readtables"
  [c340]: #x-28DREF-3APSEUDO-LOCATIVE-TYPES-20FUNCTION-29 "DREF:PSEUDO-LOCATIVE-TYPES FUNCTION"
  [c479]: #x-28CONDITION-20MGL-PAX-3ALOCATIVE-29 "CONDITION MGL-PAX:LOCATIVE"
  [c575]: http://www.lispworks.com/documentation/HyperSpec/Body/f_cmp_ma.htm "COMPILER-MACRO-FUNCTION (MGL-PAX:CLHS FUNCTION)"
  [c5ae]: http://www.lispworks.com/documentation/HyperSpec/Body/f_docume.htm "DOCUMENTATION (MGL-PAX:CLHS GENERIC-FUNCTION)"
  [c635]: #x-28DREF-3ADEFINE-DTYPE-20MGL-PAX-3AMACRO-29 "DREF:DEFINE-DTYPE MGL-PAX:MACRO"
  [c68e]: #x-28DREF-EXT-3A-40CAST-NAME-CHANGE-20MGL-PAX-3ASECTION-29 "Cast Name Change"
  [c7f7]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defgen.htm "DEFGENERIC (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [c819]: #x-28MGL-PAX-3ACONSTANT-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:CONSTANT MGL-PAX:LOCATIVE"
  [c930]: ../README.md#x-28MGL-PAX-3AEXPORTABLE-LOCATIVE-TYPE-P-20GENERIC-FUNCTION-29 "MGL-PAX:EXPORTABLE-LOCATIVE-TYPE-P GENERIC-FUNCTION"
  [c9ab]: #x-28DREF-EXT-3A-40LOCATIVE-TYPE-HIERARCHY-20MGL-PAX-3ASECTION-29 "Locative Type Hierarchy"
  [c9de]: #x-28DREF-EXT-3A-40DEFINITION-PROPERTIES-20MGL-PAX-3ASECTION-29 "Definition Properties"
  [cc04]: #x-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:READER MGL-PAX:LOCATIVE"
  [cc049]: #x-28DREF-3ADREF-LOCATIVE-20-28MGL-PAX-3AREADER-20DREF-3ADREF-29-29 "DREF:DREF-LOCATIVE (MGL-PAX:READER DREF:DREF)"
  [cc32]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_m.htm#macro_lambda_list "\"macro lambda list\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [cda7]: #x-28DREF-3AXREF-20FUNCTION-29 "DREF:XREF FUNCTION"
  [cf08]: http://www.lispworks.com/documentation/HyperSpec/Body/r_use_va.htm "USE-VALUE (MGL-PAX:CLHS RESTART)"
  [d162]: http://www.lispworks.com/documentation/HyperSpec/Body/e_error.htm "ERROR (MGL-PAX:CLHS CONDITION)"
  [d2cb]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_3.htm "DEFINE-SETF-EXPANDER (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [d3b3]: #x-28DREF-EXT-3ARESOLVE-2A-20GENERIC-FUNCTION-29 "DREF-EXT:RESOLVE* GENERIC-FUNCTION"
  [d3e1]: http://www.lispworks.com/documentation/HyperSpec/Body/f_procla.htm "PROCLAIM (MGL-PAX:CLHS FUNCTION)"
  [d45d]: #x-28DREF-3A-40MACROLIKE-LOCATIVES-20MGL-PAX-3ASECTION-29 "Locatives for Macros"
  [d646]: http://www.lispworks.com/documentation/HyperSpec/Body/t_rdtabl.htm "READTABLE (MGL-PAX:CLHS CLASS)"
  [d6c7]: http://www.lispworks.com/documentation/HyperSpec/Body/f_unionc.htm "UNION (MGL-PAX:CLHS FUNCTION)"
  [d83a]: #x-28SETF-20MGL-PAX-3ALOCATIVE-29 "SETF MGL-PAX:LOCATIVE"
  [d930]: #x-28DREF-3ADREF-20CLASS-29 "DREF:DREF CLASS"
  [da2e]: http://www.lispworks.com/documentation/HyperSpec/Issues/iss048_w.htm "\"ISSUE:CLOS-CONDITIONS-AGAIN\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [da65]: #x-28STRUCTURE-20MGL-PAX-3ALOCATIVE-29 "STRUCTURE MGL-PAX:LOCATIVE"
  [da93]: #x-28DREF-3A-40DREF-LOCATIVES-20MGL-PAX-3ASECTION-29 "Locatives for DRef Constructs"
  [daac]: http://www.lispworks.com/documentation/HyperSpec/Body/f_subtpp.htm "SUBTYPEP (MGL-PAX:CLHS FUNCTION)"
  [daacd]: #x-28DREF-EXT-3ASOURCE-LOCATION-ADJUSTED-FILE-POSITION-20FUNCTION-29 "DREF-EXT:SOURCE-LOCATION-ADJUSTED-FILE-POSITION FUNCTION"
  [dae6]: http://www.lispworks.com/documentation/HyperSpec/Body/f_string.htm "STRING (MGL-PAX:CLHS FUNCTION)"
  [db03]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eql.htm "EQL (MGL-PAX:CLHS FUNCTION)"
  [db68]: http://www.lispworks.com/documentation/HyperSpec/Body/f_pkg_na.htm "PACKAGE-NAME (MGL-PAX:CLHS FUNCTION)"
  [dbd4]: http://www.lispworks.com/documentation/HyperSpec/Body/f_vals_l.htm "VALUES-LIST (MGL-PAX:CLHS FUNCTION)"
  [dd55]: http://www.lispworks.com/documentation/HyperSpec/Body/t_and.htm "AND (MGL-PAX:CLHS TYPE)"
  [df33]: #x-28DREF-EXT-3AGENERIC-FUNCTION-DREF-20CLASS-29 "DREF-EXT:GENERIC-FUNCTION-DREF CLASS"
  [e023]: #x-28RESTART-20MGL-PAX-3ALOCATIVE-29 "RESTART MGL-PAX:LOCATIVE"
  [e196]: #x-28DREF-3ADEFINITIONS-20FUNCTION-29 "DREF:DEFINITIONS FUNCTION"
  [e1d4]: #x-28DREF-3A-40LISTING-DEFINITIONS-20MGL-PAX-3ASECTION-29 "Listing Definitions"
  [e237]: http://www.lispworks.com/documentation/HyperSpec/Body/09_.htm "\"9\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [e2d1]: http://www.lispworks.com/documentation/HyperSpec/Body/t_or.htm "OR (MGL-PAX:CLHS TYPE)"
  [e548]: #x-28MGL-PAX-3AWRITER-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:WRITER MGL-PAX:LOCATIVE"
  [e576]: #x-28DREF-EXT-3AFUNCTION-DREF-20CLASS-29 "DREF-EXT:FUNCTION-DREF CLASS"
  [e5ab]: http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_3.htm "SYMBOL-PACKAGE (MGL-PAX:CLHS FUNCTION)"
  [e5af]: http://www.lispworks.com/documentation/HyperSpec/Body/t_symbol.htm "SYMBOL (MGL-PAX:CLHS CLASS)"
  [e608]: http://www.lispworks.com/documentation/HyperSpec/Body/t_stu_cl.htm "STRUCTURE-CLASS (MGL-PAX:CLHS CLASS)"
  [e6bd]: #x-28DREF-3AARGLIST-20FUNCTION-29 "DREF:ARGLIST FUNCTION"
  [e742]: #x-28DREF-3ADREF-ORIGIN-20-28MGL-PAX-3AREADER-20DREF-3ADREF-29-29 "DREF:DREF-ORIGIN (MGL-PAX:READER DREF:DREF)"
  [e7ee]: http://www.lispworks.com/documentation/HyperSpec/Body/v_debug_.htm "*STANDARD-OUTPUT* (MGL-PAX:CLHS VARIABLE)"
  [e924]: http://www.lispworks.com/documentation/HyperSpec/Body/f_macro_.htm "MACRO-FUNCTION (MGL-PAX:CLHS FUNCTION)"
  [eac1]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defstr.htm "DEFSTRUCT (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [ead6]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defcla.htm "DEFCLASS (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [ebea]: http://www.lispworks.com/documentation/HyperSpec/Body/m_declai.htm "DECLAIM (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [ec6f]: #x-28DREF-EXT-3AREADER-DREF-20CLASS-29 "DREF-EXT:READER-DREF CLASS"
  [ed5f]: ../README.md#x-28MGL-PAX-3ACLHS-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:CLHS MGL-PAX:LOCATIVE"
  [ee9b]: #x-28DREF-EXT-3ADEFINE-SYMBOL-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29 "DREF-EXT:DEFINE-SYMBOL-LOCATIVE-TYPE MGL-PAX:MACRO"
  [efe2]: http://www.lispworks.com/documentation/HyperSpec/Body/t_generi.htm "GENERIC-FUNCTION (MGL-PAX:CLHS CLASS)"
  [f3cc]: #x-28MGL-PAX-3AMACRO-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:MACRO MGL-PAX:LOCATIVE"
  [f472]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defun.htm "DEFUN (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [f494]: #x-28DREF-EXT-3A-40DEFINING-LOCATIVE-TYPES-20MGL-PAX-3ASECTION-29 "Defining Locative Types"
  [f7e4]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_5.htm "DEFINE-CONDITION (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [fe9f]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rest.htm "REST (MGL-PAX:CLHS FUNCTION)"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
