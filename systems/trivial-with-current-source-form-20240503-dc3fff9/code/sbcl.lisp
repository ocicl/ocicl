;;;; sbcl.lisp --- SBCL implementation.
;;;;
;;;; Copyright (C) 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:trivial-with-current-source-form)

;;; http://jcsu.jesus.cam.ac.uk/~csr21/papers/features.pdf
(eval-when (:compile-toplevel :execute)
  (when (and (find-package '#:sb-ext)
             (find-symbol (string '#:with-current-source-form) '#:sb-ext))
    (pushnew 'sb-ext-with-current-source-form *features*)))

#-trivial-with-current-source-form::sb-ext-with-current-source-form
(defun expand (forms body)
  `(progn ,@body))

#+trivial-with-current-source-form::sb-ext-with-current-source-form
(defun expand (forms body)
  `(sb-ext:with-current-source-form (,@forms) ,@body))
