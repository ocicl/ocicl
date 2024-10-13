(uiop:define-package #:40ants-doc-full/locatives/argument
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:40ants-doc/locatives
                #:argument)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader))
(in-package #:40ants-doc-full/locatives/argument)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)

;;;; ARGUMENT locative

(define-locative-type argument ()
  """An alias for 40ANTS-DOC/LOCATIVES:DISLOCATED, so the one can refer to an argument of a
  macro without accidentally linking to a class that has the same name
  as that argument. In the following example, FORMAT may link to
  CL:FORMAT (if we generated documentation for it):

  ```
  "See the FORMAT in DOCUMENT."
  ```

  Since ARGUMENT is a locative, we can prevent that linking by writing:

  ```
  "See the FORMAT argument of DOCUMENT."
  ```""")

(defmethod locate-object (symbol (locative-type (eql 'argument)) locative-args)
  (declare (ignore symbol locative-args))
  (locate-error))
