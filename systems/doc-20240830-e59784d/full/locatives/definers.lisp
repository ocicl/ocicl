(uiop:define-package #:40ants-doc-full/locatives/definers
  (:use #:cl)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader)
  (:import-from #:40ants-doc/locatives/base)
  (:import-from #:40ants-doc/source-api)
  (:import-from #:40ants-doc-full/commondoc/bullet)
  (:import-from #:40ants-doc-full/commondoc/builder)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/docstring)
  (:import-from #:40ants-doc-full/commondoc/markdown)
  (:export
   #:define-symbol-locative-type))
(in-package #:40ants-doc-full/locatives/definers)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun check-body-docstring (docstring)
    (assert (or (endp docstring)
                (and (= 1 (length docstring))
                     (string (first docstring)))))))


(defmacro define-symbol-locative-type (locative-type lambda-list
                                       &body docstring)
  """Similar to 40ANTS-DOC/LOCATIVES/BASE:DEFINE-LOCATIVE-TYPE but it assumes that all things
  locatable with LOCATIVE-TYPE are going to be just symbols defined
  with a definer defined with 40ANTS-DOC/LOCATIVES/DEFINE-DEFINER:DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE.
  It is useful to attach documentation and source location to symbols
  in a particular context. An example will make everything clear:

  ```lisp
  (define-symbol-locative-type direction ()
    "A direction is a symbol. (After this `M-.` on `DIRECTION LOCATIVE`
                                     works and it can also be included in DEFSECTION forms.)")

  (define-definer-for-symbol-locative-type define-direction direction
    "With DEFINE-DIRECTION one can document what a symbol means when
  interpreted as a direction.")

  (define-direction up ()
    "UP is equivalent to a coordinate delta of (0, -1).")
  ```

  After all this, `(UP DIRECTION)` refers to the `DEFINE-DIRECTION`
  form above."""
  (check-body-docstring docstring)
  ;; We need to create a symbol with the name of locative-type inside 40ANTS-DOC/LOCATIVES,
  ;; because when we refer this locative from the documentation section, the symbol denoting
  ;; a locative will be searched in that package
  (let ((locative-type (intern (symbol-name locative-type)
                               (find-package "40ANTS-DOC/LOCATIVES"))))
    `(progn
       (40ants-doc/locatives/base::define-locative-type ,locative-type ,lambda-list ,@docstring)
       (defmethod 40ants-doc/locatives/base::locate-object (symbol (locative-type (eql ',locative-type)) locative-args)
         (or (40ants-doc/locatives/base::symbol-lambda-list-method symbol ',locative-type)
             (40ants-doc/locatives/base::locate-error))
         (40ants-doc/reference::make-reference symbol (cons locative-type locative-args)))
      
      
       (defmethod 40ants-doc-full/commondoc/builder::reference-to-commondoc ((symbol symbol) (locative-type (eql ',locative-type)) locative-args)
         (let* ((method (40ants-doc/locatives/base::symbol-lambda-list-method symbol ',locative-type))
                (arglist (40ants-doc/locatives/base::symbol-lambda-list symbol ',locative-type))
                (reference (40ants-doc/reference::make-reference
                            symbol (cons locative-type locative-args)))
                (docstring (40ants-doc/docstring:get-docstring method t))
                (children (when docstring
                            (40ants-doc-full/commondoc/markdown:parse-markdown docstring))))

           (40ants-doc-full/commondoc/bullet:make-bullet reference
                                                         :arglist arglist
                                                         :children children
                                                         :ignore-words symbol)))
      
       (defmethod 40ants-doc/locatives/base:locate-and-find-source (symbol (locative-type (eql ',locative-type)) locative-args)
         (40ants-doc/source-api:find-source (40ants-doc/locatives/base::symbol-lambda-list-method symbol ',locative-type))))))
