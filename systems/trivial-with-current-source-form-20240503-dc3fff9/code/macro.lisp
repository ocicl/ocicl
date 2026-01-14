;;;; macro.lisp --- Macros provided by the trivial-with-current-source-form system.
;;;;
;;;; Copyright (C) 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:trivial-with-current-source-form)

(defmacro with-current-source-form ((form &rest forms) &body body)
  "In a macroexpander, indicate that FORM, FORMS are being processed by BODY.

FORMS are usually sub-forms of the whole form passed to the expander.

If more than one form is supplied, FORMS should be ordered by
specificity, with the most specific form first. This allows the
compiler to try and obtain a source path using subsequent elements of
FORMS if it fails for the first one.

Indicating the processing of sub-forms lets the compiler report
precise source locations in case conditions are signaled during the
execution of BODY."
  (expand (list* form forms) body))
