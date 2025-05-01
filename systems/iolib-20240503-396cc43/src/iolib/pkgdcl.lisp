;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Package definition.
;;;

(in-package :iolib/common-lisp-user)

(macrolet
    ((defconduit (name &body clauses)
       (assert (= 1 (length clauses)))
       (assert (eql :use (caar clauses)))
       (flet ((get-symbols (packages)
                (let (symbols)
                  (with-package-iterator (iterator packages :external)
                    (loop (multiple-value-bind (morep symbol) (iterator)
                            (unless morep (return))
                            (push symbol symbols))))
                  (remove-duplicates symbols))))
         `(defpackage ,name
            (:use #:common-lisp ,@(cdar clauses))
            (:export ,@(get-symbols (cdar clauses)))))))

  (defconduit :iolib
    (:use :iolib/multiplex :iolib/streams :iolib/sockets)))

;; SBCL changes *package* if LOAD-OPing :iolib in the REPL
t
