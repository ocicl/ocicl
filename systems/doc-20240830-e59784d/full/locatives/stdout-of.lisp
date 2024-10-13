(uiop:define-package #:40ants-doc-full/locatives/stdout-of
  (:use #:cl)
  (:import-from #:40ants-doc/locatives
                #:stdout-of)
  (:import-from #:named-readtables)
  (:import-from #:common-doc)
  (:import-from #:pythonic-string-reader)
  (:import-from #:40ants-doc/locatives/base
                #:locate-object
                #:define-locative-type)
  (:import-from #:40ants-doc-full/commondoc/builder)
  (:import-from #:40ants-doc/reference))
(in-package #:40ants-doc-full/locatives/stdout-of)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(define-locative-type stdout-of (form &key lang)
  """
  Creates a block containing output of a given form.
  Also, an optional :LANG argument may be specified.
  This could be useful when you want to show the results
  of some code's evaluation.

  Here is an example of the usage:

  ```lisp
  (defsection @example ()
   (describe-output (stdout-of (format t "Hello World!"))))
  ```

  Resulting block, rendered to Markdown format will look like:

      ```markdown
      Hello World!
      ```

  """)


(defmethod locate-object (symbol (locative-type (eql 'stdout-of))
                                                    locative-args)
  (destructuring-bind (form &key lang) locative-args
    (declare (ignore form lang))
    (40ants-doc/reference::make-reference symbol (cons locative-type locative-args))))


(defmethod 40ants-doc-full/commondoc/builder::reference-to-commondoc ((symbol symbol) (locative-type (eql 'stdout-of)) locative-args)
  (destructuring-bind (form &key 
                            lang)
      locative-args
    (common-doc:make-code-block lang
                                (common-doc:make-text
                                 (with-output-to-string (*standard-output*)
                                   (eval form))))))
