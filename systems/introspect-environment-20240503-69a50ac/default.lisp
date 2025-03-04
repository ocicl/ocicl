;;;; default.lisp

;;;; implementation- (or implementationlessness-) independent documentation is in doc
;;;; docstrings here are appended as implementation notes

;;; TODO: compile-time warning of lack of implementation support?

(in-package #:introspect-environment)

;;; try to have these functions output something usable, if not useful,
;;;  since we lack the implementation hooks to do useful.
;;; so basically degrade "gracefully".  except for parse-*macro,
;;;  which has to work anyway.

(defun specialp (name &optional env)
  "This implementation is not supported; this function always returns NIL."
  (declare (ignore name env))
  nil)

(defun variable-type (name &optional env)
  "This implementation is not supported; this function doesn't know how to query an environment for type declaration information, and so always returns T."
  (declare (ignore env))
  (check-type name symbol)
  't)

(defun function-type (name &optional env)
  "This implementation is not supported; this function doesn't know how to query an environment for ftype declaration information, and so always returns (FUNCTION * *)."
  (declare (ignore env))
  (check-type name (or symbol (cons (eql setf) (cons symbol null))) "a function name")
  '(function * *))

(defun constant-form-value (form &optional env)
  "This implementation is not supported; if there is more environment dependence than macroexpansion this function will not work."
  (eval (macroexpand form env)))

(defun policy-quality (quality &optional env)
  "This implementation is not supported; this function doesn't know how to query an environment for optimize declaration information, and so returns 1 for all qualities for all environments."
  (declare (ignore env))
  (unless (member quality '(speed safety space debug compilation-speed))
    (error "Unknown policy quality ~s" quality))
  ;; ehhhhh
  1)

(defmacro policy (expr &optional env)
  "This implementation is not supported; this macro treats all optimization qualities as being 1 at all times."
  (declare (ignore env))
  ;; ehhhhhhhhhhhhhhhhhhhhhhh
  `(symbol-macrolet ((speed 1) (safety 1) (space 1) (debug 1) (compilation-speed 1))
     ,expr))

;; NOTE: Implementation-specific versions are kept in this file
;; because the rest of the protocol isn't supported for them.
;; If it is in the future, they should get their own files.
;; NOTE: Also the second return value is pretty unreliable
;; for these.

#+(or abcl allegro clisp ecl lispworks)
(progn
#+(or abcl ecl)
(defun expand-deftype-1 (type)
  ;; See ABCL, ECL expand-deftype
  (multiple-value-bind (base args)
      (if (atom type)
          (values type nil)
          (values (car type) (cdr type)))
    (let ((expander (system::get-sysprop base 'system::DEFTYPE-DEFINITION)))
      (if expander
          (values
           #+ecl(funcall expander args)
           #+abcl(apply expander args)
           t)
          (values type nil)))))

(defun typexpand-1 (type &optional env)
  (declare (type (or symbol cons class) type)
           #+(or abcl ecl lispworks) (ignore env))
  #+(or abcl ecl)
  (expand-deftype-1 type)
  #+allegro
  (excl::deftype-expand-1 type env)
  #+clisp
  (ext:type-expand type t)
  #+lispworks
  (type::expand-user-type-1 type))

(defun typexpand (type &optional env)
  (declare (type (or symbol cons class) type)
           #+(or abcl ecl lispworks) (ignore env))
  #+abcl
  (system::expand-deftype type)
  #+allegro
  (excl:normalize-type type
                       :default type
                       :environment env)
  #+clisp
  (ext:type-expand type env)
  #+ecl
  (si::expand-deftype type)
  #+lispworks
  (type::expand-user-type type))
)

#-(or abcl allegro clisp ecl lispworks)
(progn
(defun typexpand (type &optional env)
  "This implementation is not supported; no types are expanded."
  (declare (ignore env))
  (check-type type (or symbol cons class) "a type specifier")
  (values type nil))

(defun typexpand-1 (type &optional env)
  "This implementation is not supported; no types are expanded."
  (declare (ignore env))
  (check-type type (or symbol cons class) "a type specifier")
  (values type nil))
)

;;; this is basically intended to be functional, 
;;;  in that it will deal with well-formed code without spurious
;;;  warnings/etc., without being terribly... good.
;;; it's bad in that the macroexpander will cons and there's basically
;;;  no error checking. but I think that's ok, because this is just
;;;  the default for when the implementation doesn't expose its own
;;;  functionality. They all HAVE the functionality, so...

;;; though TODO: figure out how to deal with docstrings

(defun function-name->block-name (name)
  (if (consp name)
      (second name)
      name))

(defun %parse-macro (name lambda-list body cm-p)
  (check-type name (or symbol (cons (eql setf) (cons symbol null)))
	      "a function name")
  (let ((whole (gensym "WHOLE"))
	(env (gensym "ENV"))
	(rebind-whole nil)
	(rebind-env nil)
	(doc nil))
    ;; pull out docstring
    (let (res)
      (loop (cond ((and (stringp (first body)) (rest body))
		   (setf doc (first body)
			 body (nconc (nreverse res) (rest body)))
		   (return))
		  ((null body)
		   (setf body (nreverse res))
		   (return))
		  ((and (consp (first body)) (eql (first (first body)) 'declare))
		   (push (first body) res)
		   (setf body (rest body)))
		  (t
		   (setf body (nconc (nreverse res) body))
		   (return)))))
    ;; pull out toplevel &whole
    (when (eql (first lambda-list) '&whole)
      (setf whole (second lambda-list)
	    rebind-whole t
	    lambda-list (cddr lambda-list)))
    ;; pull out toplevel &environment (which can be anywhere in the list)
    (let (res)
      (loop (cond ((atom lambda-list) ; macro lambda lists can be dotted
		   (setf lambda-list (nconc (nreverse res) lambda-list))
		   (return))
		  ((eql (first lambda-list) '&environment)
		   (setf env (second lambda-list)
			 rebind-env t
			 lambda-list (nconc (nreverse res) (cddr lambda-list)))
		   (return))
		  (t
		   (push (first lambda-list) res)
		   (setf lambda-list (cdr lambda-list))))))
    (when rebind-whole (setf lambda-list (cons whole lambda-list)))
    (when rebind-env (setf lambda-list (cons env lambda-list)))
    `(lambda (,whole ,env)
       ,@(when doc (list doc))
       (declare (ignorable ,whole ,env))
       (block ,(function-name->block-name name)
	 ;; this weird rebinding is to avoid finagling with declarations, 
	 ;; which may apply to whole and env in addition to the macro parameters, etc.
	 (destructuring-bind (,@lambda-list)
	     (list* ,@(when rebind-env (list env))
		    ,@(when rebind-whole (list whole))
		    ,(if cm-p
			 `(if (eq (first ,whole) 'funcall) (cddr ,whole) (cdr ,whole))
			 `(cdr ,whole)))
	   ,@body)))))

(defun parse-macro (name lambda-list body &optional env)
  "This implementation is not supported; this function works as defined, but performs minimal error checking."
  (declare (ignore env))
  (%parse-macro name lambda-list body nil))

(defun parse-compiler-macro (name lambda-list body &optional env)
  "This implementation is not supported; this function works as defined, but performs minimal error checking."
  (declare (ignore env))
  (%parse-macro name lambda-list body t))
