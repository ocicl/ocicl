(uiop:define-package #:40ants-doc-full/doc
  (:use #:cl
        #:40ants-doc/locatives)
  (:import-from #:40ants-doc
                #:exportable-locative-type-p
                #:defsection)
  (:import-from #:40ants-doc/restart)
  (:import-from #:40ants-doc/glossary)
  
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader)
  (:import-from #:40ants-doc-full/builder
                #:@generating-documentation)
  (:import-from #:40ants-doc-full/markdown
                #:@markdown-support)
  (:import-from #:40ants-doc-full/builder/printer)
  (:import-from #:40ants-doc-full/link)
  (:import-from #:40ants-doc-full/tutorial)
  (:import-from #:40ants-doc-full/builder/vars)
  (:import-from #:40ants-doc/locatives/base
                #:locate-and-find-source
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/source-api
                #:find-source)
  (:import-from #:40ants-doc/changelog
                #:@changelog)
  (:import-from #:40ants-doc-full/themes/docs)
  (:import-from #:docs-config
                #:docs-config)
  (:import-from #:40ants-doc/ignored-words)
  (:import-from #:40ants-doc-full/transcribe
                #:@transcript)
  (:import-from #:40ants-doc-full/commondoc/builder
                #:reference-to-commondoc
                #:to-commondoc)
  (:import-from #:40ants-doc-full/commondoc/bullet
                #:make-bullet)
  (:import-from #:40ants-doc-full/commondoc/mapper
                #:with-node-package
                #:node-supports-children
                #:map-nodes)
  (:import-from #:40ants-doc-full/locatives/asdf-system
                #:end-of-asdf-example)
  (:import-from #:40ants-doc-full/locatives/definers
                #:define-symbol-locative-type)
  (:import-from #:40ants-doc/locatives/define-definer
                #:define-definer-for-symbol-locative-type)
  (:import-from #:40ants-doc-full/locatives/variable
                #:end-of-variable-example)
  (:import-from #:40ants-doc-full/swank
                #:locate-definition-for-emacs)
  (:import-from #:40ants-doc/autodoc
                #:defautodoc)
  (:import-from #:40ants-doc/locatives/asdf-system
                #:asdf-system-documentation-title)
  (:import-from #:40ants-doc-full/plugins/mathjax
                #:mathjax)
  (:import-from #:40ants-doc-full/plugins/highlightjs
                #:highlightjs)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:40ants-doc-full/doc)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *ignore-words*
    '("HTML"
      "HTMLs"
      "API"
      "CL-TELEGRAM-BOT"
      "README"
      "JSON"
      "MGL-PAX"
      "SLIME"
      "SWANK"
      "SLY"
      "URI"
      "URL"
      "URLs"
      "LISP"
      "SBCL"
      "FOO"
      "FOO-SLOT"
      "FOO-EXAMPLE"
      "*FOO-STATE*"
      "BAZ"
      "BAR"
      "OTHER-PACKAGE:SOMETHING"
      "MIT"
      "GIT"
      "ASDF"
      "CSS"
      "3BMD"
      "PYTHONIC-STRING-READER"
      "DOCS-BUILDER"
      "COMMON-DOC:DOCUMENT-NODE"))


  (defparameter *badges*
    "
<table class=\"badges\">
<tr>
<td><a href=\"https://github.com/40ants/doc/actions/workflows/ci.yml\"><img src=\"http://github-actions.40ants.com/40ants/doc/matrix.svg?only=ci.run-tests\"/></a></td>

<td><a href=\"https://github.com/40ants/doc/actions/workflows/linter.yml\"><img src=\"http://github-actions.40ants.com/40ants/doc/matrix.svg?only=linter.linter\"/></a></td>

<td><a href=\"https://coveralls.io/github/40ants/doc?branch=master\"><img src=\"https://coveralls.io/repos/github/40ants/doc/badge.svg?branch=master\"/></a></td>

<td><img src=\"https://api.quickdocs.org/badge/doc.svg\"/></td>
</tr>
</table>
"))

(defsection @index (:title "40Ants Doc Manual"
                    :ignore-words *ignore-words*)
  *badges*
  (@about section)
  (40ants-doc system)
  (40ants-doc-full system)
  (@links section)
  (@background section)
  (@tutorial section)
  (@emacs-integration section)
  (@basics section)
  (@generating-documentation section)
  (@markdown-support section)
  (@documentation-printer-variables section)
  (@locative-types section)
  (@extension-api section)
  (@transcript section)
  (@todo section))


(defsection @readme (:title "40ANTS-DOC Documentation Generator"
                     :ignore-words (list* "DEFSECTION"
                                          *ignore-words*))
  *badges*
  (@about section)
  (@full-doc-link section)
  (@tutorial section)
  (@todo section))


(defsection @full-doc-link (:title "Full Documentation")
  "Read full documentation at [site 40ants.com/doc/](https://40ants.com/doc/).")


(defsection @about (:title "About this fork"
                    :ignore-words ("IRONCLAD"))
  "
This system is a fork of [MGL-PAX](https://github.com/melisgl/mgl-pax).

There are a few reasons, why I've created the fork.

The main goal is to extract a core features into the 40ANTS-DOC system
with as little dependencies as possible. This is important, because with MGL-PAX's
style, you define documentation sections in your library's code, which makes
it dependent on the documentation system. However, heavy weight dependencies
like IRONCLAD, 3BMD or SWANK should not be required.

The seconds goal was to refactor a 3.5k lines of `pax.lisp` file into
a smaller modules to make navigation easier. This will help any person
who will decide to learn how the documentation builder works. Also,
granular design will make it possible loading subsystems like SLIME or SLY
integration.

The third goal was to make documentation processing more sequential and hackable.
To introduce hooks for adding new markup languages, and HTML themes.
"
  (@difference-from-mgl-pax section))

(defsection @difference-from-mgl-pax (:title "Why this fork is different"
                                      :ignore-words ("NAMED-READTABLES"
                                                     "PYTHONIC-STRING-READER"
                                                     "API"
                                                     "@INDEX"
                                                     "SECTION"
                                                     "JS"
                                                     "XREF"
                                                     "40A"
                                                     "FIND-SOURCE"))
  "
Here are features already implemented in this fork:

* Core system `40ANTS-DOC` now has only two dependencies on `NAMED-READTABLES`
  and `PYTHONIC-STRING-READER`. If you want to compile a documentation, load
  `40ANTS-DOC-FULL` system which will download such dependencies as markdown
  parser and more.
* Now you don't have to import any locative symbols into your package. Import
  only a DEFSECTION macro and it will be enough to define documentation for
  your library!
* Added a warning mechanism, which will issue such warnings on words which looks
  like a symbol, but when real symbol or reference is absent:

  ```
  WARNING: Unable to find target for reference #<XREF \"FIND-SOURCE\" GENERIC-FUNCTION>
           mentioned at 40Ants Doc Manual / Extension API / Reference Based Extensions
  ```

* Documentation processing now uses CommonDoc as intermediate format, and markup languages
  other than Markdown can be supported.
* Added a JS search index which will work when you are hosting pages on a static website
  like GitHub pages.
* It is possible to render pages in multiple formats and having cross references between them.
  See 40ANTS-DOC-FULL/BUILDER::@RENDERING-MULTIPLE-FORMATS.

I'm planning to extend this fork even more. Read @TODO section to learn about
proposed features or [start a new discussion](https://github.com/40ants/doc/discussions)
on the GitHub to suggest a new feature.

See full list of changes in the 40ANTS-DOC/CHANGELOG::@CHANGELOG section.
")


(defsection @links (:title "Links")
  "
  Here is the [official repository](https://github.com/40ants/doc) and
  the [HTML documentation](https://40ants.com/doc) for the latest version.

  This system is a fork of the [MGL-PAX](https://github.com/melisgl/mgl-pax).
  Because of massive refactoring, it is incompatible with original repository.
")


(defsection @background (:export nil :title "Background"
                         :ignore-words ("OAOO"))
  "Here is the story behind the MGL-PAX, precursor of 40ANTS-DOC, written
   by Gábor Melis."
  
  "As a user, I frequently run into documentation that's incomplete
  and out of date, so I tend to stay in the editor and explore the
  code by jumping around with SLIME's [`M-.`][slime-M-.]. As a library
  author, I spend a great deal of time polishing code, but precious
  little writing documentation.

  [slime-M-.]: http://common-lisp.net/project/slime/doc/html/Finding-definitions.html#Finding-definitions

  In fact, I rarely write anything more comprehensive than docstrings
  for exported stuff. Writing docstrings feels easier than writing a
  separate user manual and they are always close at hand during
  development. The drawback of this style is that users of the library
  have to piece the big picture together themselves.

  That's easy to solve, I thought, let's just put all the narrative
  that holds docstrings together in the code and be a bit like a
  Literate Programming weenie turned inside out. The original
  prototype which did almost everything I wanted was this:

  ```
  (defmacro defsection (name docstring)
    `(defun ,name () ,docstring))
  ```

  Armed with DEFSECTION, I soon found myself organizing code following
  the flow of user level documentation and relegated comments to
  implementational details entirely. However, some portions of
  DEFSECTION docstrings were just listings of all the functions,
  macros and variables related to the narrative, and this list was
  effectively repeated in the DEFPACKAGE form complete with little
  comments that were like section names. A clear violation of
  [OAOO][oaoo], one of them had to go, so DEFSECTION got a list of
  symbols to export.

  [oaoo]: http://c2.com/cgi/wiki?OnceAndOnlyOnce

  That was great, but soon I found that the listing of symbols is
  ambiguous if, for example, a function, a compiler macro and a class
  are named by the same symbol. This did not concern exporting, of
  course, but it didn't help readability. Distractingly, on such
  symbols, `M-.` was popping up selection dialogs. There were two
  birds to kill, and the symbol got accompanied by a type which was
  later generalized into the concept of locatives:

  ```lisp
  (defsection @introduction ()
    \"A single line for one man ...\"
    (foo class)
    (bar function))
  ```

  After a bit of elisp hacking, `M-.` was smart enough to disambiguate
  based on the locative found in the vicinity of the symbol and
  everything was good for a while.

  Then I realized that sections could refer to other sections if there
  were a SECTION locative. Going down that path, I soon began to feel
  the urge to generate pretty documentation as all the necessary
  information was manifest in the DEFSECTION forms. The design
  constraint imposed on documentation generation was that following
  the typical style of upcasing symbols in docstrings there should be
  no need to explicitly mark up links: if `M-.` works, then the
  documentation generator shall also be able find out what's being
  referred to.

  I settled on [Markdown][markdown] as a reasonably non-intrusive
  format, and a few thousand lines later MGL-PAX was born.

  [markdown]: https://daringfireball.net/projects/markdown/")

(defsection @tutorial (:title "Tutorial"
                       :ignore-words ("UIOP:DEFINE-PACKAGE"
                                      "@FOO-RANDOM-MANUAL"
                                      "LIMIT"
                                      "STDDEV"
                                      "HELLO"
                                      "FOO-RANDOM-STATE"
                                      "FOO-RANDOM"
                                      "STATE"
                                      "OBJECT"
                                      "KEY"
                                      "GAUSSIAN-RANDOM"
                                      "UNIFORM-RANDOM"))
  """40ANTS-DOC provides an extremely poor man's Explorable Programming
  environment. Narrative primarily lives in so called sections that
  mix markdown docstrings with references to functions, variables,
  etc, all of which should probably have their own docstrings.

  The primary focus is on making code easily explorable by using
  SLIME's `M-.` (`slime-edit-definition`). See how to enable some
  fanciness in @EMACS-INTEGRATION. Generating documentation
  from sections and all the referenced items in Markdown or HTML
  format is also implemented.

  With the simplistic tools provided, one may accomplish similar
  effects as with Literate Programming, but documentation is generated
  from code, not vice versa and there is no support for chunking yet.
  Code is first, code must look pretty, documentation is code.

  When the code is loaded into the lisp, pressing `M-.` in SLIME on
  the name of the section will take you there. Sections can also refer
  to other sections, packages, functions, etc and you can keep exploring.

  Here is an example of how it all works together:

  """

  (tutorial-code (include (:start (foo-random package)
                           :end (foo-random::+end+ variable))
                          :lang "commonlisp"))

  """
  Generating documentation in a very stripped down markdown format is
  easy:

  ```lisp
  (40ants-doc-full/builder:render-to-string
    @foo-random-manual
    :format :markdown)
  ```

  For this example, the generated markdown would look like this:"""

  (render-to-stribg-output
   (stdout-of (format t (40ants-doc-full/builder:render-to-string
                         foo-random::@foo-random-manual
                         :format :markdown))
              :lang "markdown"))

  """
  MGL-PAX supported the plain text format which was more readble when viewed
  from a simple text editor, but I've dropped support for plain text in this fork
  because most time documentation are read in the browser these days.

  To render into the files, use 40ANTS-DOC-FULL/BUILDER:RENDER-TO-FILES
  and 40ANTS-DOC-FULL/BUILDER:UPDATE-ASDF-SYSTEM-DOCS functions.

  Last one can even generate documentation for different, but related
  libraries at the same time with the output going to different files,
  but with cross-page links being automatically added for symbols
  mentioned in docstrings. See `40ANTS-DOC-FULL/BUILDER::@GENERATING-DOCUMENTATION` for
  some convenience functions to cover the most common cases.

  Note how `(*FOO-STATE* VARIABLE)` in the DEFSECTION form includes its documentation in
  `@FOO-RANDOM-MANUAL`. The symbols VARIABLE and FUNCTION are just two
  instances of 'locatives' which are used in DEFSECTION to refer to
  definitions tied to symbols. See @LOCATIVE-TYPES.

  The transcript in the code block tagged with `cl-transcript` is
  automatically checked for up-to-dateness. See
  `40ANTS-DOC-FULL/TRANSCRIBE::@TRANSCRIPT`.
"""
  )

(defsection @emacs-integration (:title "Emacs Integration"
                                :ignore-words ("SWANK-BACKEND:FIND-SOURCE-LOCATION"
                                               "SWANK-BACKEND:FIND-DEFINITIONS"))
  "Integration into SLIME's `M-.` (`slime-edit-definition`) allows one
  to visit the source location of the thing that's identified by a
  symbol and the locative before or after the symbol in a buffer. With
  this extension, if a locative is the previous or the next expression
  around the symbol of interest, then `M-.` will go straight to the
  definition which corresponds to the locative. If that fails, `M-.`
  will try to find the definitions in the normal way which may involve
  popping up an xref buffer and letting the user interactively select
  one of possible definitions.

  *Note that the this feature is implemented in terms of
  SWANK-BACKEND:FIND-SOURCE-LOCATION and
  SWANK-BACKEND:FIND-DEFINITIONS whose support varies across the Lisp
  implementations. Sadly, but this integration does not with SLY because
  it does not support hooks on finding definition.*

  In the following examples, pressing `M-.` when the cursor is on one
  of the characters of `FOO` or just after `FOO`, will visit the
  definition of function `FOO`:

      function foo
      foo function
      (function foo)
      (foo function)

  In particular, references in a DEFSECTION form are in (SYMBOL
  LOCATIVE) format so `M-.` will work just fine there.

  Just like vanilla `M-.`, this works in comments and docstrings. In
  this example pressing `M-.` on `FOO` will visit `FOO`'s default
  method:

  ```lisp
  ;;;; See FOO `(method () (t t t))` for how this all works.
  ;;;; But if the locative has semicolons inside: FOO `(method
  ;;;; () (t t t))`, then it won't, so be wary of line breaks
  ;;;; in comments.
  ```

  With a prefix argument (`C-u M-.`), one can enter a symbol plus a
  locative separated by whitespace to preselect one of the
  possibilities.

  The `M-.` extensions can be enabled by adding this to your Emacs
  initialization file (or loading `src/pax.el`):"
  (edit-locative.el (include #.(asdf:system-relative-pathname :40ants-doc "elisp/edit-locative.el")
                             :lang "elisp"))
  (locate-definition-for-emacs function)

  "Note, there is also another part of Emacs code, related to
   transcription blocks. It is described in 40ANTS-DOC-FULL/TRANSCRIBE::@TRANSCRIPT section.")


(defsection @basics (:title "Basics"
                     :ignore-words ("@BAR"
                                    "UIOP:DEFINE-PACKAGE"
                                    "OBJECT"
                                    "LOCATIVE"
                                    "3BMD"
                                    "@MANUAL"
                                    "@MGL-PAX-EXTENSION-API"
                                    "SECTION"
                                    "FIND-SOURCE"))
  "Now let's examine the most important pieces in detail."
  (@defining-sections section)
  (@cross-referencing section)
  (@autodoc section))


(defsection @defining-sections (:title "Defining Sections")
  (40ants-doc:defsection macro)
  (40ants-doc:defsection-copy macro)
  (40ants-doc:*discard-documentation-p* variable)
  (40ants-doc:*symbols-with-ignored-missing-locations* variable)
  (40ants-doc/ignored-words:ignored-words generic-function)
  (40ants-doc/ignored-words:supports-ignored-words-p generic-function)
  (40ants-doc/ignored-words:ignore-words-in-package macro))


(defsection @cross-referencing (:title "Cross-referencing")
  "
  You can cross-reference entries from different documentation parts be it
  content of the DEFSECTION or a documentation string of some lisp entity.

  The simples form of cross reference is uppercased name of the entity, like:
  40ANTS-DOC/REFERENCE:MAKE-REFERENCE. But if there are more than one locative
  bound to the name, then all these links will be rendered in a parenthesis.
  For example, docstring:

  ```text
  See 40ANTS-DOC/SOURCE-API:FIND-SOURCE.
  ```

  will be rendered as \"See 40ANTS-DOC/SOURCE-API:FIND-SOURCE.\" because
  there is a generic-function and a method called FIND-SOURCE.

  But you can mention a locative type in a docstring before or after a symbol name:

  ```text
  See 40ANTS-DOC/SOURCE-API:FIND-SOURCE generic-function.
  ```

  and it will be rendered as: See 40ANTS-DOC/SOURCE-API:FIND-SOURCE generic-function.

  In case if you don't want locative type to appear in the resulting documentation
  or if locative type is complex, then you can use in a docstring markdown reference:

  ```text
  See [40ANTS-DOC/SOURCE-API:FIND-SOURCE][(method () (40ants-doc/reference:reference))].
  ```

  and link will lead to the specified method: See [40ANTS-DOC/SOURCE-API:FIND-SOURCE][(method () (40ants-doc/reference:reference))].")


(defsection @autodoc (:title "Autodocumentation")
  "40ANTS-DOC system provides an additional subsystem and package `40ANTS-DOC/AUTODOC`.
   This subsystem contains a macro DEFAUTODOC, which is similar to
   DEFSECTION, but generates a section filled with content of the given ASDF system.

   This subsystem is not loaded by default because it brings a multiple additional dependencies:

   - alexandria
   - cl-change-case
   - cl-ppcre
   - cl-unicode
   - closer-mop

   but I'm trying to keep dependencies of the core 40ANTS-DOC system is minimal.

   Use it if your don't care or your have docs in a separate ASDF sybsystem.
"
  (defautodoc macro))


(defsection @locatives-and-references
    (:title "Locatives and References"
     :ignore-words ("FOO"))
  "While Common Lisp has rather good introspective abilities, not
  everything is first class. For example, there is no object
  representing the variable defined with `(DEFVAR
  FOO)`. `(40ANTS-DOC/REFERENCE:MAKE-REFERENCE 'FOO 'VARIABLE)` constructs a 40ANTS-DOC/REFERENCE:REFERENCE that
  captures the path to take from an object (the symbol FOO) to an
  entity of interest (for example, the documentation of the variable).
  The path is called the locative. A locative can be applied to an
  object like this:

  ```
  (locate 'foo 'variable)
  ```

  which will return the same reference as `(40ANTS-DOC/REFERENCE:MAKE-REFERENCE 'FOO
  'VARIABLE)`. Operations need to know how to deal with references
  which we will see in 40ANTS-DOC/LOCATIVES/BASE:LOCATE-AND-FIND-SOURCE.

  Naturally, `(40ANTS-DOC/LOCATIVES/BASE:LOCATE 'FOO 'FUNCTION)` will simply return `#'FOO`, no
  need to muck with references when there is a perfectly good object."
  (40ants-doc/locatives/base:locate function)
  (40ants-doc/locatives/base:locate-error condition)
  (40ants-doc/locatives/base:locate-error-message (reader 40ants-doc/locatives/base:locate-error))
  (40ants-doc/locatives/base:locate-error-object (reader 40ants-doc/locatives/base:locate-error))
  (40ants-doc/locatives/base:locate-error-locative (reader 40ants-doc/locatives/base:locate-error))
  (40ants-doc/reference:resolve function)
  (40ants-doc/reference:reference class)
  (40ants-doc/reference:reference-object (reader 40ants-doc/reference:reference))
  (40ants-doc/reference:reference-locative (reader 40ants-doc/reference:reference))
  (40ants-doc/reference:make-reference function)
  (40ants-doc/locatives/base:locative-type generic-function)
  (40ants-doc/locatives/base:locative-args generic-function))


(defsection @documentation-printer-variables
    (:title "Documentation Printer Variables"
     :ignore-words ("@FOO-MANUAL"))
  "Docstrings are assumed to be in markdown format and they are pretty
  much copied verbatim to the documentation subject to a few knobs
  described below.

  **Note, some of these variables might be not supported yet in this fork.**
"
  (40ants-doc-full/builder/printer:*document-uppercase-is-code* variable)
  (40ants-doc-full/link:*document-link-code* variable)
  (40ants-doc-full/builder/vars:*document-max-numbering-level* variable))


(defsection @locative-types (:title "Locative Types"
                             :ignore-words ("SWANK-BACKEND:ARGLIST"
                                            "START"
                                            "END")
                             :package :40ants-doc/locatives)
  "These are the locatives type supported out of the box. As all
  locative types, they are symbols and their names should make it
  obvious what kind of things they refer to. Unless otherwise noted,
  locatives take no arguments."
  
  (40ants-doc/locatives package)
  (system locative)
  (asdf-system-documentation-title generic-function)
  
  (section locative)
  (variable locative)
  (constant locative)
  (macro locative)
  (compiler-macro locative)
  (symbol-macro locative)
  (function locative)
  (generic-function locative)
  (method locative)
  (accessor locative)
  (reader locative)
  (writer locative)
  (structure-accessor locative)
  (class locative)
  (condition locative)
  (type locative)
  (package locative)
  (dislocated locative)
  (argument locative)
  (locative locative)
  (include locative)
  (stdout-of locative)
  (40ants-doc/restart:define-restart macro)
  (restart locative)
  (40ants-doc/glossary:define-glossary-term macro)
  (glossary-term locative)

  "There is also a helper function to compare locatives:"
  (40ants-doc/locatives/base:locative-equal function))


(defsection @extension-api (:title "Extension API")
  (40ants-doc-full/themes/docs::@defining-a-theme section)
  (@locatives-and-references section)
  (@new-object-types section)
  (@reference-based-extensions section)
  (@sections section))


(defsection @new-object-types (:title "Adding New Object Types"
                               :ignore-words ("SWANK:FIND-DEFINITION-FOR-THING"
                                              "COMMON-DOC:CHILDREN"))
  "
  If you wish to make it possible to render documentation for a new
  object type, then you have to define a method for the
  40ANTS-DOC-FULL/COMMONDOC/BUILDER:TO-COMMONDOC generic function.
  And to make `M-.` navigation work with new object types, a methods of
  40ANTS-DOC/LOCATIVES/BASE:LOCATE-OBJECT generic-function and
  40ANTS-DOC/SOURCE-API:FIND-SOURCE generic-function are to be defined.
  Also, additional method for 40ANTS-DOC/REFERENCE-API:CANONICAL-REFERENCE generic-function
  need to be defined to make an opposite to [40ANTS-DOC/LOCATIVES/BASE:LOCATE-OBJECT][generic-function]'s action.

  Finally, 40ANTS-DOC:EXPORTABLE-LOCATIVE-TYPE-P generic-function
  may be overridden if exporting does not makes sense.
  Here is a stripped down example of how all this is done
  for ASDF:SYSTEM:"
  (asdf-example (include (:start (asdf:system locative)
                          :end (end-of-asdf-example variable))
                         :lang "commonlisp"))
  (define-locative-type macro)
  (exportable-locative-type-p generic-function)
  (locate-object generic-function)
  (locate-error function)
  (canonical-reference generic-function)
  (find-source generic-function)
  (to-commondoc generic-function)
  (make-bullet function)
  (map-nodes generic-function)
  (node-supports-children generic-function)
  (with-node-package macro))


(defsection @reference-based-extensions
    (:title "Reference Based Extensions"
     :ignore-words ("DEFINE-DIRECTION"
                    "UP"
                    "DIRECTION"))
  "Let's see how to extend 40ANTS-DOC-FULL/BUILDER:RENDER-TO-FILES and `M-.` navigation if there is
  no first class object to represent the thing of interest. Recall
  that 40ANTS-DOC/LOCATIVES/BASE:LOCATE returns a 40ANTS-DOC/REFERENCE:REFERENCE object in this case:

  ```cl-transcript
  (40ants-doc/locatives/base:locate
     '40ants-doc:*discard-documentation-p*
     'variable)
  ==> #<40ANTS-DOC/REFERENCE:REFERENCE 40ANTS-DOC:*DISCARD-DOCUMENTATION-P* (VARIABLE)>
  ```

  Some methods of 40ANTS-DOC/SOURCE-API:FIND-SOURCE generic-function defer to
  40ANTS-DOC/LOCATIVES/BASE:LOCATE-AND-FIND-SOURCE generic-function,
  which have [LOCATIVE-TYPE][argument] in their argument
  list for EQL specializing pleasure.

  Here is a stripped down example of how the VARIABLE locative is defined.
  Pay attention how it defines a method of
  40ANTS-DOC-FULL/COMMONDOC/BUILDER:REFERENCE-TO-COMMONDOC generic-function instead of
  [40ANTS-DOC-FULL/COMMONDOC/BUILDER:TO-COMMONDOC][generic-function]. This is because we have no
  a lisp object to represent a variable and have to specialize method on
  LOCATIVE-TYPE argument:"
  (variable-example (include (:start (variable locative)
                              :end (end-of-variable-example variable))
                             :lang "commonlisp"))
  (find-source (method () (40ants-doc/reference:reference)))
  (locate-and-find-source generic-function)
  (locate-and-find-source (method () (t t t)))
  (reference-to-commondoc generic-function)
  
  "We have covered the basic building blocks of reference based
  extensions. Now let's see how the obscure
  DEFINE-SYMBOL-LOCATIVE-TYPE and
  DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE macros work together to
  simplify the common task of associating definition and documentation
  with symbols in a certain context."
  (define-symbol-locative-type macro)
  (define-definer-for-symbol-locative-type macro))


(defsection @sections (:title "Sections")
  "40ANTS-DOC:SECTION objects rarely need to be dissected since
  40ANTS-DOC:DEFSECTION and 40ANTS-DOC-FULL/BUILDER:RENDER-TO-FILES cover most needs. However, it is plausible
  that one wants to subclass them and maybe redefine how they are
  presented."
  (40ants-doc:section class)
  (40ants-doc:section-name (reader 40ants-doc:section))
  (40ants-doc:section-package (reader 40ants-doc:section))
  (40ants-doc:section-readtable (reader 40ants-doc:section))
  (40ants-doc:section-title (reader 40ants-doc:section))
  (40ants-doc:section-link-title-to (reader 40ants-doc:section))
  (40ants-doc:section-entries (reader 40ants-doc:section))
  (40ants-doc:section-external-docs (reader 40ants-doc:section))
  (40ants-doc:section-ignore-words (reader 40ants-doc:section)))


(defsection @todo (:title "TODO"
                   :ignore-words ("SLIME"
                                  "SLY"
                                  "UPPERCASED"
                                  "RSS"
                                  "HTML"))
  "
- <s>Refactor code and make a core package with only a few dependencies.</s>
- <s>Add warnings on UPPERCASED symbols in docstrings which aren't found in the package and can't be cross referenced.</s>
- <s>Support SLY and make both SLIME and SLY integrations optional.</s>
- <s>Add a search facility which will build an index for static file like Sphinx does.</s>
- <s>Separate markup parsing and result rendering code to support markups other than Markdown and HTML.</s>
- <s>Add a new section type to render ChangeLog.</s>
- <s>Support custom HTML themes.</s>
- <s>Generate RSS or Atom feed out of changelog items, defined with
  40ANTS-DOC/CHANGELOG:DEFCHANGELOG macro.</s>
- Make some warnings compile-time for defsection and show them in the Emacs, if possible.
")


(defmethod docs-config ((system (eql (asdf:registered-system "40ants-doc-full"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  (uiop:symbol-call :ql :quickload
                    :40ants-doc-theme-40ants)
  (let ((theme (find-symbol "40ANTS-THEME"
                            (find-package "40ANTS-DOC-THEME-40ANTS"))))
    (unless theme
      (error "Unable to find 40ANTS-DOC-THEME-40ANTS::40ANTS-THEME symbol"))
    
    (list :theme
          (make-instance theme
                         :plugins (list (highlightjs)
                                        (mathjax))))))
