;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Match utils
;;;

(in-package :iolib/base)

(defmacro multiple-value-case ((values &key (test 'eql)) &body body)
  (setf values (ensure-list values))
  (when (symbolp test) (setf test `(quote ,test)))
  (assert values () "Must provide at least one value to test")
  (let ((test-name (alexandria::extract-function-name test)))
    (labels ((%do-var (var val)
               (cond
                 ((and (symbolp var) (member var '("_" "*") :test #'string=))
                  t)
                 ((consp var)
                  `(member ,val ',var :test ,test))
                 (t
                  `(,test-name ,val ',var))))
             (%do-clause (c gensyms)
               (destructuring-bind (vals &rest code) c
                 (let* ((tests (remove t (mapcar #'%do-var (ensure-list vals) gensyms)))
                        (clause-test (if (> 2 (length tests))
                                         (first tests)
                                         `(and ,@tests))))
                   `(,clause-test ,@code))))
             (%do-last-clause (c gensyms)
               (when c
                 (destructuring-bind (test &rest code) c
                   (if (member test '(otherwise t))
                       `((t ,@code))
                       `(,(%do-clause c gensyms)))))))
      (let ((gensyms (mapcar (lambda (v) (gensym (string v)))
                             values)))
        `(let ,(mapcar #'list gensyms values)
           (declare (ignorable ,@gensyms))
           (cond ,@(append (mapcar (lambda (c) (%do-clause c gensyms))
                                   (butlast body))
                           (%do-last-clause (lastcar body) gensyms))))))))

(defmacro flags-case (mask &body clauses)
  (once-only (mask)
    `(progn ,@(mapcar (lambda (clause)
                        `(when
                           (logtest ,(let ((flags (first clause)))
                                       (if (listp flags)
                                           `(logior ,@flags)
                                           flags))
                                    ,mask)
                           ,@(rest clause)))
                      clauses))))
