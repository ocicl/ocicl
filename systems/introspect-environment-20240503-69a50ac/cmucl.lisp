;;;; cmucl.lisp

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
  ;; always return an alist with all optimization qualities.
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
	 ;; allows standard symbols that aren't variables
	 ;; to be symbol-macrolut.
	 ;; This may not be true of implementation-specific packages.
	 ,expr))))

(defun parse-compiler-macro (name lambda-list body &optional env)
  ;; largely copied from CMUCL's define-compiler-macro, unsurprisingly.
  (declare (ignore env)) ; env is just for evenness with parse-macro
  ;; variables for the expansion
  (let ((whole-var (gensym "WHOLE"))
	(env-var (gensym "ENV")))
    (multiple-value-bind (validp block-name)
	(ext:valid-function-name-p name)
      (unless validp
	(error "~S is not a valid function name." name))
      (multiple-value-bind (body local-decls doc)
	  (lisp::parse-defmacro lambda-list whole-var body name
				'define-compiler-macro
				;; the d-c-m context tells cmucl to
				;; build the body to handle FUNCALL
				;; forms correctly. at least,
				;; "correctly" if you don't want a
				;; compiler macro on CL:FUNCALL, which
				;; is undefined for users anyway.
				:environment env-var)
	(declare (ignore doc)) ; welp.
	`(lambda (,whole-var ,env-var)
	   ,@local-decls
	   (block ,block-name
	     ,body))))))

(defun typexpand-1 (form &optional env)
  ;; This code is taken from the function KERNEL:TYPE-EXPAND.
  (declare (ignore env))
  (let ((def (cond ((symbolp form)
		    (ext:info type expander form))
		   ((and (consp form) (symbolp (car form)))
		    (ext:info type expander (car form)))
		   (t nil))))
    (if def
	(values (funcall def (if (consp form) form (list form)))
		t)
	(values form nil))))
(defun typexpand (type &optional env)
  (labels ((tx-loop (type ever-expanded?)
	     (multiple-value-bind (new expanded?) (typexpand-1 type env)
	       (if expanded?
		   (tx-loop new t)
		   (values type ever-expanded?)))))
    (tx-loop type nil)))

(defun constant-form-value (form &optional env)
  (eval:internal-eval form t env))
