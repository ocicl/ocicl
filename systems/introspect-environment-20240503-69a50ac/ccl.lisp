;;;; ccl.lisp

;;;; implementation-independent documentation can be found in doc

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

(defun constant-form-value (form &optional env)
  (declare (ignore env))
  (ccl::eval-constant form))

(defun policy-quality (quality &optional env)
  (or (second (assoc quality (declaration-information 'optimize env)))
      (error "Unknown policy quality ~s" quality)))

(defmacro policy (expr &optional env)
  ;; conveniently, declaration-information 'optimize is specified to
  ;;   always return an alist with all optimization qualities.
  (let ((qualities (mapcar #'car (declaration-information 'optimize)))
	(optvar (gensym "POLICY")))
    `(let ((,optvar (declaration-information 'optimize ,env)))
       ;; sbcl, at least, has an alist of lists rather than alist of conses, hence SECOND.  weird.
       ;; ccl does this too.  probably i overlooked something in cltl2.
       (symbol-macrolet (,@(mapcar (lambda (quality) `(,quality (second (assoc ',quality ,optvar)))) qualities))
	 ;; CLHS 11.1.2.1.2.1 (ref because wow obscure) explicitly allows standard symbols that aren't variables
	 ;;  (e.g., SPEED) to be symbol-macrolut.
	 ;; This may not be true of implementation-specific packages.  (it would be nice, but)
	 ,expr))))

(defun parse-compiler-macro (name lambda-list body &optional env)
  "This function relies on the behavior of undocumented internal functions in order to support FUNCALL destructuring."
  ;; CCL's definition of define-compiler-macro is slightly incorrect in
  ;;  that it doesn't deal with funcall forms correctly.
  ;; (e.g. the SQUARE definition in CLHS's define-compiler-macro page)
  ;; fairly easy fix though. if inefficient, but hey, metacompile time!
  ;; (more importantly, it's quite brittle.
  ;;  if the format of parse-macro's result changes
  ;;  we'll have to patch again, and this has already happened.)
  (let ((done nil))
    (labels ((hack (thing whole-name)
	       ;; I don't actually know how CCL's destructuring works,
	       ;;  but it seems that make-destructure-state
	       ;; (changed before 1.10-416196 to prepare-to-destructure)
	       ;;  just takes a list and some other crap and makes
	       ;;  some CCL internal gizmo from it.
	       ;; This function just blows through the forms and
	       ;;  replaces that argument appropriately, 
	       ;;  using the whole-var from the lambda expression
	       ;;  (that is, (lambda (whole env) ...))
	       ;; for added paranoia this replacement is only done once.
	       ;; it's mildly more efficient (ha!)
	       ;;  and more defensive against the pathological case of
	       ;;  user code with a call to make-destructure-state in it
	       ;; (oh, and if you're wondering why we bother with this
	       ;;  crap "code walker" instead of working against
	       ;;  the stereotyped form of the output - that output is
	       ;;  even less stable.  &whole introduces another binding,
	       ;;  gensyms, bla bla bla.)
	       (if (and (not done) (consp thing))
		   (cond ((eql (car thing) 'ccl::prepare-to-destructure)
			  (setf done t)
			  `(ccl::prepare-to-destructure
			    (if (eql (car ,whole-name) 'funcall) (cdr ,(second thing)) ,(second thing))
			    ,@(cddr thing)))
			 (t (cons (hack (car thing) whole-name) (hack (cdr thing) whole-name))))
		   thing)))
      (let ((body (parse-macro name lambda-list body env)))
	(hack body (first (second body)))))))

(defun typexpand-1 (type &optional env)
  "This function relies on the behavior of undocumented internal functions."
  ;; cargo-culted from ccl::type-expand.
  (let ((expander (cond ((symbolp type)
			 (ccl::%deftype-expander type))
			((and (consp type) (symbolp (car type)))
			 (ccl::%deftype-expander (first type))))))
    (if expander
	(values	(funcall expander
			 (if (consp type) type (list type)) ; ok then
			 env)
		;; hopefully all of CCL's deftypes fulfill the "expansion must halt" rule.
		t)
	(values type nil))))

(defun typexpand (type &optional env)
  (labels ((tx-loop (type ever-expanded?)
	     (multiple-value-bind (new expanded?) (typexpand-1 type env)
	       (if expanded?
		   (tx-loop new t)
		   (values type ever-expanded?)))))
    (tx-loop type nil)))
