(uiop:define-package #:40ants-doc-full/link
  (:use #:cl)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader)
  (:export
   #:*document-link-code*))
(in-package #:40ants-doc-full/link)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(defvar *document-link-code* t
  """When true, during the process of generating documentation for a
  40ANTS-DOC:SECTION class, HTML anchors are added before the documentation of
  every reference that's not to a section. Also, markdown style
  reference links are added when a piece of inline code found in a
  docstring refers to a symbol that's referenced by one of the
  sections being documented. Assuming `BAR` is defined, the
  documentation for:

  ```lisp
  (defsection @foo
    (foo function)
    (bar function))

  (defun foo (x)
    "Calls `BAR` on `X`."
    (bar x))
  ```

  would look like this:

      - [function] FOO X

          Calls [`BAR`][1] on `X`.

  Instead of `BAR`, one can write `[bar][]` or ``[`bar`][]`` as well.
  Since symbol names are parsed according to READTABLE-CASE, character
  case rarely matters.

  Now, if `BAR` has references with different locatives:

  ```lisp
  (defsection @foo
    (foo function)
    (bar function)
    (bar type))

  (defun foo (x)
    "Calls `BAR` on `X`."
    (bar x))
  ```

  then documentation would link to all interpretations:

      - [function] FOO X

          Calls `BAR`([`1`][link-id-1] [`2`][link-id-2]) on `X`.

  This situation occurs with 40ANTS-DOC:SECTION which is both a class (see
  40ANTS-DOC:SECTION class) and a locative type denoted by a symbol (see
  40ANTS-DOC/LOCATIVES:SECTION locative). Back in the example above, clearly,
  there is no reason to link to type `BAR`, so one may wish to select
  the function locative. There are two ways to do that. One is to
  specify the locative explicitly as the id of a reference link:

      "Calls [BAR][function] on X."

  However, if in the text there is a locative immediately before or
  after the symbol, then that locative is used to narrow down the
  range of possibilities. This is similar to what the `M-.` extension
  does. In a nutshell, if `M-.` works without questions then the
  documentation will contain a single link. So this also works without
  any markup:

      "Calls function `BAR` on X."

  This last option needs backticks around the locative if it's not a
  single symbol.

  Note that [*DOCUMENT-LINK-CODE*][variable] can be combined with
  40ANTS-DOC-FULL/BUILDER/PRINTER:*DOCUMENT-UPPERCASE-IS-CODE* to have links generated for
  uppercase names with no quoting required.""")


