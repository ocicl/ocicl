(uiop:define-package #:40ants-doc-full/builder/printer
  (:use #:cl)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader)
  (:export #:*document-uppercase-is-code*))
(in-package #:40ants-doc-full/builder/printer)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(defvar *full-package-names* t
  "If true, symbols are printed with full package names.
   Otherwise they are printed relative to 40ANTS-DOC:SECTION-PACKAGE of the
   innermost containing section or with full package names if there is
   no containing section. Thus, unless true, some symbols might be printed
   without packages and some - with packages and this might be annoing
   especially for ASDF systems which use package inferred structure and
   have many packages.")


(defvar *document-uppercase-is-code* t
  """When true, words with at least three characters and no lowercase
  characters naming an interned symbol are assumed to be code as if
  they were marked up with backticks which is especially useful when
  combined with 40ANTS-DOC-FULL/LINK:*DOCUMENT-LINK-CODE*. For example, this docstring:

      "`FOO` and FOO."

  is equivalent to this:

      "`FOO` and `FOO`."

  if `FOO` is an interned symbol.""")




