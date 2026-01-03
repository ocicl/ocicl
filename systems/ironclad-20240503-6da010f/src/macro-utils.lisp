;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; macro-utils.lisp -- things to make compiler macros easier

(in-package :crypto)

(defun quotationp (thing)
  (and (consp thing) (consp (rest thing))
       (null (cddr thing)) (eq (first thing) 'quote)))

(defun unquote (thing)
  (if (quotationp thing) (second thing) thing))

(defun massage-symbol (symbol)
  (let ((package (symbol-package symbol))
        (ironclad (load-time-value (find-package :ironclad))))
    (cond
      ((eq package ironclad) symbol)
      ((eq package (load-time-value (find-package :keyword)))
       (find-symbol (symbol-name symbol) ironclad))
      (t nil))))


;;; a few functions that are useful during compilation

(defun make-circular-list (&rest elements)
  (let ((list (copy-seq elements)))
    (setf (cdr (last list)) list)))

;;; SUBSEQ is defined to error on circular lists, so we define our own
(defun circular-list-subseq (list start end)
  (let* ((length (- end start))
         (subseq (make-list length)))
    (do ((i 0 (1+ i))
         (list (nthcdr start list) (cdr list))
         (xsubseq subseq (cdr xsubseq)))
        ((>= i length) subseq)
      (setf (first xsubseq) (first list)))))


;;; Partial evaluation helpers

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun trivial-macroexpand-all (form env)
    "Trivial and very restricted code-walker used in partial evaluation macros.
Only supports atoms and function forms, no special forms."
    (let ((real-form (macroexpand form env)))
      (cond
        ((atom real-form)
         real-form)
        (t
         (list* (car real-form)
                (mapcar #'(lambda (x) (trivial-macroexpand-all x env))
                        (cdr real-form))))))))

(defmacro dotimes-unrolled ((var limit) &body body &environment env)
  "Unroll the loop body at compile-time."
  (loop for x from 0 below (eval (trivial-macroexpand-all limit env))
        collect `(symbol-macrolet ((,var ,x)) ,@body) into forms
        finally (return `(progn ,@forms))))

(defun symbolicate (&rest things)
  "Concatenate together the names of some strings and symbols,
producing a symbol in the current package."
  (flet ((stringify (x)
           (typecase x
             (integer (format nil "~D" x))
             (t (string x)))))
    (let* ((length (reduce #'+ things
                           :key (lambda (x) (length (stringify x)))))
           (name (make-array length :element-type 'character)))
      (let ((index 0))
        (dolist (thing things (values (intern name)))
          (let* ((x (stringify thing))
                 (len (length x)))
            (replace name x :start1 index)
            (incf index len)))))))
