;;;; expander.lisp

;;;; compiler macroexpansion utils

(in-package #:introspect-environment)

;;; the compiler macroexpansion utilities I looked at seemed not to expand in the funcall case, but it's not that hard to do.
;;; i imagine they don't because it's a bit tricky, and given the general underutilization of compiler macros, pointless.

;; excludes, e.g., lambda expressions
(defun valid-function-name-p (thing)
  (or (symbolp thing)
      (and (consp thing)
	   (cdr thing)
	   (consp (cdr thing))
	   (null (cddr thing))
	   (symbolp (second thing))
	   (eq (first thing) 'setf))))

(defun compiler-macroexpand-1 (form &optional env)
  "As MACROEXPAND-1, but uses compiler macros rather than macros.  (as a consequence of this, forms like (FUNCALL (FUNCTION FOO) ...) may be expanded with FOO's compiler macro.)"
  (flet ((try-named (name)
	   ;; ha! ha! diagonally is a good direction for code to go
	   (when (valid-function-name-p name)
	     (let ((cmf (compiler-macro-function name env)))
	       (when cmf
		 (let ((new (funcall *macroexpand-hook* cmf form env)))
		   (unless (eql form new)
		     (return-from compiler-macroexpand-1
		       (values new t)))))))))
    (when (consp form)
      (or (try-named (first form))
	  (and (eql (first form) 'funcall)
	       (consp (cdr form))
	       (consp (second form))
	       (eql (first (second form)) 'function)
	       (try-named (second (second form))))))
    (values form nil)))

(defun compiler-macroexpand (form &optional env)
  "As MACROEXPAND, but uses compiler macros rather than macros.  (as a consequence of this, forms like (FUNCALL (FUNCTION FOO) ...) may be expanded with FOO's compiler macro.)"
  (labels ((mx-loop (form ever-expanded?)
	     (multiple-value-bind (new expanded?)
		 (compiler-macroexpand-1 form env)
	       (if expanded?
		   (mx-loop new t)
		   (values form ever-expanded?)))))
    (mx-loop form nil)))
