<a id="x-2840ANTS-DOC-FULL-2FDOC-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# 40ANTS-DOC Documentation Generator


<table class="badges">
<tr>
<td><a href="https://github.com/40ants/doc/actions/workflows/ci.yml"><img src="http://github-actions.40ants.com/40ants/doc/matrix.svg?only=ci.run-tests"/></a></td>

<td><a href="https://github.com/40ants/doc/actions/workflows/linter.yml"><img src="http://github-actions.40ants.com/40ants/doc/matrix.svg?only=linter.linter"/></a></td>

<td><a href="https://coveralls.io/github/40ants/doc?branch=master"><img src="https://coveralls.io/repos/github/40ants/doc/badge.svg?branch=master"/></a></td>

<td><img src="https://api.quickdocs.org/badge/doc.svg"/></td>
</tr>
</table>

<a id="x-2840ANTS-DOC-FULL-2FDOC-3A-3A-40ABOUT-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## About this fork

This system is a fork of [MGL-PAX][7927].

There are a few reasons, why I've created the fork.

The main goal is to extract a core features into the [`40ants-doc`][a2c7] system
with as little dependencies as possible. This is important, because with `MGL-PAX`'s
style, you define documentation sections in your library's code, which makes
it dependent on the documentation system. However, heavy weight dependencies
like `IRONCLAD`, `3BMD` or `SWANK` should not be required.

The seconds goal was to refactor a 3.5k lines of `pax.lisp` file into
a smaller modules to make navigation easier. This will help any person
who will decide to learn how the documentation builder works. Also,
granular design will make it possible loading subsystems like `SLIME` or `SLY`
integration.

The third goal was to make documentation processing more sequential and hackable.
To introduce hooks for adding new markup languages, and `HTML` themes.

<a id="x-2840ANTS-DOC-FULL-2FDOC-3A-3A-40DIFFERENCE-FROM-MGL-PAX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Why this fork is different

Here are features already implemented in this fork:

* Core system [`40ants-doc`][a2c7] now has only two dependencies on `NAMED-READTABLES`
  and `PYTHONIC-STRING-READER`. If you want to compile a documentation, load
  [`40ants-doc-full`][a689] system which will download such dependencies as markdown
  parser and more.
* Now you don't have to import any locative symbols into your package. Import
  only a [`defsection`][4e8b] macro and it will be enough to define documentation for
  your library!
* Added a warning mechanism, which will issue such warnings on words which looks
  like a symbol, but when real symbol or reference is absent:

`
  WARNING: Unable to find target for reference #<XREF "FIND-SOURCE" GENERIC-FUNCTION>
           mentioned at 40Ants Doc Manual / Extension API / Reference Based Extensions
`

* Documentation processing now uses CommonDoc as intermediate format, and markup languages
  other than Markdown can be supported.
* Added a `JS` search index which will work when you are hosting pages on a static website
  like GitHub pages.
* It is possible to render pages in multiple formats and having cross references between them.
  See [`Multiple Formats`][a367].

I'm planning to extend this fork even more. Read [`todo`][bc3a] section to learn about
proposed features or [start a new discussion][6f00]
on the GitHub to suggest a new feature.

See full list of changes in the [`ChangeLog`][e991] section.

<a id="x-2840ANTS-DOC-FULL-2FDOC-3A-3A-40FULL-DOC-LINK-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Full Documentation

Read full documentation at [site 40ants.com/doc/][778d].

<a id="x-2840ANTS-DOC-FULL-2FDOC-3A-3A-40TUTORIAL-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Tutorial

[`40ants-doc`][a2c7] provides an extremely poor man's Explorable Programming
environment. Narrative primarily lives in so called sections that
mix markdown docstrings with references to functions, variables,
etc, all of which should probably have their own docstrings.

The primary focus is on making code easily explorable by using
`SLIME`'s `M-.` (`slime-edit-definition`). See how to enable some
fanciness in [`Emacs Integration`][6f57]. Generating documentation
from sections and all the referenced items in Markdown or `HTML`
format is also implemented.

With the simplistic tools provided, one may accomplish similar
effects as with Literate Programming, but documentation is generated
from code, not vice versa and there is no support for chunking yet.
Code is first, code must look pretty, documentation is code.

When the code is loaded into the lisp, pressing `M-.` in `SLIME` on
the name of the section will take you there. Sections can also refer
to other sections, packages, functions, etc and you can keep exploring.

Here is an example of how it all works together:

```commonlisp
(uiop:define-package #:foo-random
  (:nicknames #:40ants-doc-full/tutorial)
  (:documentation "This package provides various utilities for
                   random. See @FOO-RANDOM-MANUAL.")
  (:use #:common-lisp
        #:40ants-doc)
  (:import-from #:40ants-doc/ignored-words
                #:ignore-words-in-package)
  (:export #:foo-random-state
           #:state
           #:*foo-state*
           #:gaussian-random
           #:uniform-random))

(in-package foo-random)

(defsection @foo-random-manual (:title "Foo Random manual"
                                :ignore-words ("FOO"))
  "Here you describe what's common to all the referenced (and
   exported) functions that follow. They work with *FOO-STATE*,
   and have a :RANDOM-STATE keyword arg. Also explain when to
   choose which."
  (foo-random-state class)
  (state (reader foo-random-state))
  
  "Hey we can also print states!"
  
  (print-object (method () (foo-random-state t)))
  (*foo-state* variable)
  (gaussian-random function)
  (uniform-random function)
  ;; this is a subsection
  (@foo-random-examples section))

(defclass foo-random-state ()
  ((state :reader state
          :documentation "Returns random foo's state.")))

(defmethod print-object ((object foo-random-state) stream)
  (print-unreadable-object (object stream :type t)))

(defvar *foo-state* (make-instance 'foo-random-state)
  "Much like *RANDOM-STATE* but uses the FOO algorithm.")

(defun uniform-random (limit &key (random-state *foo-state*))
  "Return a random number from the between 0 and LIMIT (exclusive)
   uniform distribution."
  (declare (ignore limit random-state))
  nil)

(defun gaussian-random (stddev &key (random-state *foo-state*))
  "Return not a random number from a zero mean normal distribution with
   STDDEV."
  (declare (ignore stddev random-state))
  nil)

(defsection @foo-random-examples (:title "Examples")
  "Let's see the transcript of a real session of someone working
   with FOO:

   ```cl-transcript
   (values (princ :hello) (list 1 2))
   .. HELLO
   => :HELLO
   => (1 2)

   (make-instance 'foo-random-state)
   ==> #<FOO-RANDOM-STATE >
   ```")

```
Generating documentation in a very stripped down markdown format is
easy:

```lisp
(40ants-doc-full/builder:render-to-string
  @foo-random-manual
  :format :markdown)
```
For this example, the generated markdown would look like this:

````markdown
<a id="x-28FOO-RANDOM-3A-3A-40FOO-RANDOM-MANUAL-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# Foo Random manual

Here you describe what's common to all the referenced (and
exported) functions that follow. They work with [`*foo-state*`][2133],
and have a `:RANDOM-STATE` keyword arg. Also explain when to
choose which.

<a id="x-28FOO-RANDOM-3AFOO-RANDOM-STATE-20CLASS-29"></a>

## [class](0d91) `foo-random:foo-random-state` ()

<a id="x-28FOO-RANDOM-3ASTATE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20FOO-RANDOM-3AFOO-RANDOM-STATE-29-29"></a>

## [reader](d52a) `foo-random:state` (foo-random-state) ()

Returns random foo's state.

Hey we can also print states!

<a id="x-28PRINT-OBJECT-20-28METHOD-20NIL-20-28FOO-RANDOM-3AFOO-RANDOM-STATE-20T-29-29-29"></a>

## [method](501f) `common-lisp:print-object` (object foo-random-state) stream

<a id="x-28FOO-RANDOM-3A-2AFOO-STATE-2A-20-28VARIABLE-29-29"></a>

## [variable](ab78) `foo-random:*foo-state*` #<foo-random-state >

Much like `*RANDOM-STATE*` but uses the `FOO` algorithm.

<a id="x-28FOO-RANDOM-3AGAUSSIAN-RANDOM-20FUNCTION-29"></a>

## [function](1bb1) `foo-random:gaussian-random` stddev &key (random-state \*foo-state\*)

Return not a random number from a zero mean normal distribution with
`STDDEV`.

<a id="x-28FOO-RANDOM-3AUNIFORM-RANDOM-20FUNCTION-29"></a>

## [function](3d30) `foo-random:uniform-random` limit &key (random-state \*foo-state\*)

Return a random number from the between 0 and `LIMIT` (exclusive)
uniform distribution.

<a id="x-28FOO-RANDOM-3A-3A-40FOO-RANDOM-EXAMPLES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Examples

Let's see the transcript of a real session of someone working
with `FOO`:

```cl-transcript
(values (princ :hello) (list 1 2))
.. HELLO
=> :HELLO
=> (1 2)

(make-instance 'foo-random-state)
==> #<FOO-RANDOM-STATE >
```

[2133]: #x-28FOO-RANDOM-3A-2AFOO-STATE-2A-20-28VARIABLE-29-29
[0d91]: https://github.com/40ants/doc/blob/165e8162ca653039e2eb3de1ffa9f28603690678/full/tutorial.lisp#L35
[d52a]: https://github.com/40ants/doc/blob/165e8162ca653039e2eb3de1ffa9f28603690678/full/tutorial.lisp#L36
[501f]: https://github.com/40ants/doc/blob/165e8162ca653039e2eb3de1ffa9f28603690678/full/tutorial.lisp#L39
[ab78]: https://github.com/40ants/doc/blob/165e8162ca653039e2eb3de1ffa9f28603690678/full/tutorial.lisp#L42
[3d30]: https://github.com/40ants/doc/blob/165e8162ca653039e2eb3de1ffa9f28603690678/full/tutorial.lisp#L45
[1bb1]: https://github.com/40ants/doc/blob/165e8162ca653039e2eb3de1ffa9f28603690678/full/tutorial.lisp#L51
````
`MGL-PAX` supported the plain text format which was more readble when viewed
from a simple text editor, but I've dropped support for plain text in this fork
because most time documentation are read in the browser these days.

To render into the files, use [`40ants-doc-full/builder:render-to-files`][6a41]
and [`40ants-doc-full/builder:update-asdf-system-docs`][8c6d] functions.

Last one can even generate documentation for different, but related
libraries at the same time with the output going to different files,
but with cross-page links being automatically added for symbols
mentioned in docstrings. See [`Generating Documentation`][eb87] for
some convenience functions to cover the most common cases.

Note how `(*FOO-STATE* VARIABLE)` in the [`defsection`][4e8b] form includes its documentation in
`@FOO-RANDOM-MANUAL`. The symbols [`variable`][8f5a] and [`function`][0bdf] are just two
instances of 'locatives' which are used in [`defsection`][4e8b] to refer to
definitions tied to symbols. See [`Locative Types`][69f3].

The transcript in the code block tagged with `cl-transcript` is
automatically checked for up-to-dateness. See
[`Transcripts`][b035].

<a id="x-2840ANTS-DOC-FULL-2FDOC-3A-3A-40TODO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## TODO

* <s>Refactor code and make a core package with only a few dependencies.</s>
* <s>Add warnings on `UPPERCASED` symbols in docstrings which aren't found in the package and can't be cross referenced.</s>
* <s>Support `SLY` and make both `SLIME` and `SLY` integrations optional.</s>
* <s>Add a search facility which will build an index for static file like Sphinx does.</s>
* <s>Separate markup parsing and result rendering code to support markups other than Markdown and `HTML`.</s>
* <s>Add a new section type to render ChangeLog.</s>
* <s>Support custom `HTML` themes.</s>
* <s>Generate `RSS` or Atom feed out of changelog items, defined with
  [`40ants-doc/changelog:defchangelog`][8c40] macro.</s>
* Make some warnings compile-time for defsection and show them in the Emacs, if possible.


[778d]: https://40ants.com/doc/
[a2c7]: https://40ants.com/doc/#x-28-23A-28-2810-29-20BASE-CHAR-20-2E-20-2240ants-doc-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29
[a689]: https://40ants.com/doc/#x-28-23A-28-2815-29-20BASE-CHAR-20-2E-20-2240ants-doc-full-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29
[8c40]: https://40ants.com/doc/#x-2840ANTS-DOC-2FCHANGELOG-3ADEFCHANGELOG-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[4e8b]: https://40ants.com/doc/#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[eb87]: https://40ants.com/doc/#x-2840ANTS-DOC-FULL-2FBUILDER-3A-3A-40GENERATING-DOCUMENTATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[a367]: https://40ants.com/doc/#x-2840ANTS-DOC-FULL-2FBUILDER-3A-3A-40RENDERING-MULTIPLE-FORMATS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[6a41]: https://40ants.com/doc/#x-2840ANTS-DOC-FULL-2FBUILDER-3ARENDER-TO-FILES-20FUNCTION-29
[8c6d]: https://40ants.com/doc/#x-2840ANTS-DOC-FULL-2FBUILDER-3AUPDATE-ASDF-SYSTEM-DOCS-20FUNCTION-29
[6f57]: https://40ants.com/doc/#x-2840ANTS-DOC-FULL-2FDOC-3A-3A-40EMACS-INTEGRATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[69f3]: https://40ants.com/doc/#x-2840ANTS-DOC-FULL-2FDOC-3A-3A-40LOCATIVE-TYPES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[bc3a]: https://40ants.com/doc/#x-2840ANTS-DOC-FULL-2FDOC-3A-3A-40TODO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[b035]: https://40ants.com/doc/#x-2840ANTS-DOC-FULL-2FTRANSCRIBE-3A-3A-40TRANSCRIPT-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[0bdf]: https://40ants.com/doc/#x-28FUNCTION-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29
[8f5a]: https://40ants.com/doc/#x-28VARIABLE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29
[e991]: https://40ants.com/doc/changelog/#x-2840ANTS-DOC-2FCHANGELOG-3A-40CHANGELOG-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[6f00]: https://github.com/40ants/doc/discussions
[7927]: https://github.com/melisgl/mgl-pax

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
