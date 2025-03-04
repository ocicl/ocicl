#|
  This file is a part of type-i project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage type-i
  (:use :trivia :cl :alexandria :introspect-environment)
  (:export
   #:?
   #:test-type
   #:define-inference-rule
   #:inference-rules
   #:type-tests
   #:failed-type-inference))
(in-package :type-i)

;;;    Type and Test Inference
;;;
;;; these works as the hints for the compiler.
;;; include only those which is not trivially defined.
;;; By default, type-tests finds appropriate functions automatically
;;; e.g. for type X, predicates like X-p or Xp, and also (typep ? X).
;;; 
;;; CL allows abbreviated compound specifier, like (array type
;;; dimensions), (array type), (array), array. The purpose of
;;; define-compound-type-tests is to ease adding these types of variations.

(lispn:define-namespace inference-rules function)

(defmacro define-inference-rule (name args &body body)
  (assert (= (length args) 1))
  `(setf (symbol-inference-rules ',name)
         #+sbcl
         (sb-int:named-lambda ',name ,args ,@body)
         #-sbcl
         (lambda ,args ,@body)))

(defun ? (symbol test)
  "substitute symbol with '?, canonicalizing the test form amenable for comparison"
  (subst '? symbol test))

(define-condition failed-type-inference (warning)
  ((test :initarg :test))
  (:report (lambda (condition stream)
             (with-slots (test) condition
               (format stream "failed to infer the type from test ~a !" test)))))

(defun test-type (test &optional verbose)
  "infer the type which the given test form is trying to test against.
If the values are NIL,T , it means the type is definitely NIL.
If the values are NIL,NIL , it means the type is not successfully inferred."
  ;; if the speed matters, it is possible to memoize the result.
  (let (closed)
    (do ((open nil (cdr open))
         (now test (car open)))
        ((null now)
         (warn 'failed-type-inference :test test)
         (values nil nil))
      (push now closed)
      (maphash (lambda (key fn)
                 (declare (ignorable key))
                 (match now
                   ((list 'typep '? (list 'quote type))
                    (when verbose
                      (format t "~& test: ~a inferred type: ~a" test type))
                    (return-from test-type
                      (values type t)))
                   (_
                    (when-let ((successors (funcall fn now)))
                      (when verbose
                        (format t "~& ~<expanded ~a with rule ~a -> ~_~{~a~^, ~:_~}~:>" (list now key successors)))
                      (unionf open (set-difference successors closed :test #'equal)
                              :test #'equal)))))
               *INFERENCE-RULES-TABLE*))))

(defun type-tests (type &optional verbose)
  (let (closed)
    (do ((open nil (cdr open))
         (now `(typep ? ',type) (car open)))
        ((null now) closed)
      (push now closed)
      (maphash (lambda (key fn)
                 (when-let ((successors (funcall fn now)))
                   (when verbose
                     (format t "~& ~<expanded ~a with rule ~a -> ~_~{~a~^, ~:_~}~:>" (list now key successors)))
                   (unionf open (set-difference successors closed :test #'equal)
                           :test #'equal)))
               *INFERENCE-RULES-TABLE*))))





