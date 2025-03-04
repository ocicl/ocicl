;;;; doc.lisp

;;;; documentation is put in this file so that it doesn't have to be
;;;;  copied all throughout.
;;;; docstrings in the implementation-specific files are appended to
;;;; this general documentation and noted as implementation-specific.

(in-package #:introspect-environment)

(defmacro defdoc (slot-name doc-type doc)
  `(let () ; toplevel is for sissies
     ;; don't document implementation syms
     (when (eq (symbol-package ',slot-name)
	       (find-package "INTROSPECT-ENVIRONMENT"))
       (let ((old-doc (documentation ',slot-name ',doc-type))
	     (doc ,doc))
	 (setf (documentation ',slot-name ',doc-type)
	       (if old-doc
		   (format nil "~a~2%~a note: ~a" doc (lisp-implementation-type) old-doc)
		   doc))))))

(defdoc variable-type function
  "Return a known supertype of NAME, a variable, in ENV.")

(defdoc function-type function
  "Return a known supertype of the function named NAME in ENV.")

(defdoc constant-form-value function
  "Return the value of a constant form.
Results of using this function on a form not previously tested with CONSTANTP are undefined.")

(defdoc policy-quality function
  "Return the value of QUALITY, an optimization quality (e.g. COMPILATION-SPEED) in ENV, if possible.")

(defdoc policy function
  "Evaluate EXPR in an environment in which all optimization qualities (e.g. COMPILATION-SPEED) refer to their known values in ENV if possible.

For instance, (locally (optimize (speed 3) (space 0)) (policy (> speed space) env)) will evaluate to true (if supported).")

(defdoc typexpand-1 function
  "Like MACROEXPAND-1, but with type specifiers (type \"macros\" being established with DEFTYPE).")

(defdoc typexpand function
  "Like MACROEXPAND, but with type specifiers (type \"macros\" being established with DEFTYPE).")

(defdoc parse-macro function
  "Given a NAME, LAMBDA-LIST, BODY, and optionally an ENVIRONMENT (default NIL, designating the null lexical environment), return a lambda expression, the evaluation of which is suitable as a macroexpansion function.
I.e., (eval (parse-macro 'name 'lambda-list 'body env)) and (macro-function (defmacro name lambda-list . body) env) [evaluated in env] should be functions with the same effects.")

(defdoc parse-compiler-macro function
  "Like PARSE-MACRO, but the lambda expression is suited for use as a compiler macro. Meaning it's the same, except that if it's provided a form beginning with FUNCALL to expand it will still destructure its arguments appropriately.")
