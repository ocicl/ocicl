(uiop:define-package #:40ants-doc/locatives/define-definer
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base)
  (:export
   #:define-definer-for-symbol-locative-type))
(in-package #:40ants-doc/locatives/define-definer)


(defun expand-define-definer-for-symbol-as-locative-definer-body
    (symbol locative-type lambda-list docstring)
  `(defmethod 40ants-doc/locatives/base::symbol-lambda-list ((symbol (eql ',symbol))
                                                             (locative-type (eql ',locative-type)))
     ,@docstring
     ',lambda-list))


;; This definer is in a separate module, to make it available without
;; additional dependencies:
(defmacro define-definer-for-symbol-locative-type
    (name locative-type &body docstring)
  "Define a macro with NAME which can be used to attach documentation,
  a lambda-list and source location to a symbol in the context of
  LOCATIVE-TYPE. The defined macro's arglist is (SYMBOL LAMBDA-LIST
  &OPTIONAL DOCSTRING). LOCATIVE-TYPE is assumed to have been defined
  with 40ANTS-DOC-FULL/LOCATIVES/DEFINERS:DEFINE-SYMBOL-LOCATIVE-TYPE."
  `(defmacro ,name (symbol lambda-list &body docstring)
     ,@docstring
     `,(expand-define-definer-for-symbol-as-locative-definer-body
        symbol ',locative-type lambda-list docstring)))
