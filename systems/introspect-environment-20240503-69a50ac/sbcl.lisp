;;;; sbcl.lisp

;;;; implementation-independent documentation can be found in doc.lisp

(in-package #:introspect-environment)

;;; implementations implementing the CLtL2 non-standard have this easy.

(defun specialp (name &optional env)
  (eq (variable-information name env) :special))

(defun variable-type (name &optional env)
  (or (cdr (assoc 'type (nth-value 2 (variable-information name env))))
      't))

(defun function-type (name &optional env)
  (or (cdr (assoc 'ftype (nth-value 2 (function-information name env))))
      '(function * *)))

(defun policy-quality (quality &optional env)
  (or (second (assoc quality (declaration-information 'optimize env)))
      (error "Unknown policy quality ~s" quality)))

(defmacro policy (expr &optional env)
  ;; conveniently, declaration-information 'optimize is specified to
  ;;   always return an alist with all optimization qualities.
  (let ((qualities (mapcar #'car (declaration-information 'optimize)))
	(optvar (gensym "POLICY")))
    `(let ((,optvar (declaration-information 'optimize ,env)))
       ;; cltl2 has an alist of lists instead of just conses.
       ;; dunno why. anyway it means we use second for cdr.
       (symbol-macrolet
	   ,(mapcar (lambda (quality)
		      `(,quality (second (assoc ',quality ,optvar))))
		    qualities)
	 ;; CLHS 11.1.2.1.2.1 (ref because wow obscure) explicitly
	 ;;  allows standard symbols that aren't variables
	 ;;  to be symbol-macrolut.
	 ;; This may not be true of implementation-specific packages.
	 ;; (sbcl is fine though, woo)
	 ,expr))))

(macrolet
    ((body ()
       ;; This function uses SBCL internals and is therefore brittle.
       ;; This macrolet allows an implementation appropriate to the SBCL
       ;; version to be chosen at build time.
       (let ((make-macro-lambda (find-symbol "MAKE-MACRO-LAMBDA" "SB-INT"))
             (parse-defmacro (find-symbol "PARSE-DEFMACRO" "SB-KERNEL")))
         (cond (make-macro-lambda
                `(values
                  (,make-macro-lambda
                   (list 'compiler-macro name)
		   lambda-list body
                   'define-compiler-macro
		   name
		   ;; weirdness here to avoid breaking compatibility with
                   ;; sbcl 1.2.13-15
		   :allow-other-keys t
		   :accessor (find-symbol "COMPILER-MACRO-ARGS" "SB-C"))))
               (parse-defmacro
                `(let ((whole-var (gensym "WHOLE"))
	               (env-var (gensym "ENV")))
                   (multiple-value-bind (body local-decls doc)
	               (,parse-defmacro
	                lambda-list whole-var body name
	                'define-compiler-macro
	                ;; the d-c-m context tells sbcl to build the body to
                        ;; handle FUNCALL forms correctly.
	                ;; at least, "correctly" if you don't want a compiler
                        ;; macro on CL:FUNCALL, which is undefined for users
                        ;; anyway.
	                :environment env-var)
	             (declare (ignore doc)) ; welp.
	             `(lambda (,whole-var ,env-var)
	                ,@local-decls
	                ,body))))
               (t
                (warn
                 "Don't know how to PARSE-COMPILER-MACRO on this SBCL version.")
                '(error
                  "Don't know how to PARSE-COMPILER-MACRO on this SBCL version."))))))
  (defun parse-compiler-macro (name lambda-list body &optional env)
    (declare (ignore env)) ; env is just for evenness with parse-macro
    (body)))

;;; alternate sbcl-specific definitions, probably less stable than the cltl2 half-standard

#+(or)
(defmacro policy (expr &optional env)
  (sb-c:policy env expr))

#+(or)
(defun policy-quality (quality &optional env)
  (let ((policy (sb-c::%coerce-to-policy env)))
    ;; %coerce instead of just ::lexenv-policy in case of a nil argument.
    ;; though, maybe it would be better to make+use a null lexenv in the NIL case, since NIL is coerced to a lack of policy.
    ;; (i.e., it's unaffected by toplevel declaims, etc)
    (cond ((member quality sb-c::*policy-qualities*)
	   (sb-c::policy-quality policy quality))
	  ((member quality sb-c::*policy-dependent-qualities*)
	   (let ((info (cdr (assoc quality sb-c::*policy-dependent-qualities*))))
	     (funcall (sb-c::policy-dependent-quality-getter info) policy)))
	  (t (error "Unknown policy quality ~s" quality)))))
