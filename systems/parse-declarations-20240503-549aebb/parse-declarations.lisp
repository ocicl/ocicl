
;;; Copyright (c) 2008, Tobias C. Rittweiler <tcr@freebits.de>
;;;
;;; All rights reserved.
;;;
;;; This file is published under the MIT license.
;;; See LICENSE for details.


;;; The library Parse-Declarations comes with an elaborate manual
;;; which includes a specification of the operators provided. The code
;;; should reflect the specification, not the other way around.

(defpackage :tcr.parse-declarations-1.0
  (:use :cl)
  (:export #:declaration-env #:declaration-env-p
	   #:declaration-env.policy #:declaration-env.affected-variables
	   #:filter-declaration-env #:merge-declaration-envs #:map-declaration-env
	   #:parse-declarations #:build-declarations #:check-declaration-env
	   #:analyze-declaration-specifier #:build-declaration-specifier
	   ))

(in-package :tcr.parse-declarations-1.0)

(defstruct (declspec (:conc-name declspec.))
  identifier affected-variables context unknownp)

(defstruct (declaration-env
	     (:conc-name declaration-env.)
	     (:constructor make-declaration-env ()))
  (%table (make-hash-table :test 'eq) :type hash-table))

;;; Implementation Note:
;;
;; Making a DECLARATION-ENV be essentially a HASH-TABLE has a couple
;; of consequences; it won't be serializable, yet externalizable. We
;; don't believe that either property is important for its intended
;; use cases. And even if so, then more likely the latter than the
;; first one.
;;
;; Iterating over a hash-table returns its stored elements in an
;; unpredictable order. Declarations that are built using a
;; DECLARATION-ENV will hence not necessarily reflect the order they
;; have been parsed. It's our understanding that declarations should
;; be interpreted in parallel within one declaration block (such as
;; LET), making this a non-issue. Furthermore, given that there is no
;; environment access in ANSI CL, there is likely no way a user could
;; possibly try to rely on sequential interpretation anyway.
;;
;; (An exception may OPTIMIZE declarations where a user may think
;; that in
;;         (LOCALLY (DECLARE (OPTIMIZE (SPEED 1)))
;;                  (DECLARE (OPTIMIZE (SPEED 2)))
;;            ...body..)
;; BODY is evaluated with a SPEED quality of 2. The above snippet
;; is specified to have undefined behaviour, however.)
;;
;; Future revisions of this library may make the effort to preserve
;; the order of parsed declarations.
;;


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +standard-declaration-identifiers+
    '(declaration dynamic-extent ftype ignore ignorable inline notinline
      optimize special type)))

					;INTERNAL ONLY.
(defmacro define-declaration-methods (generic-fn-name &body clauses)
  "Used to conveniently define methods pattern matching on the
standard declaration identifiers.

GENERIC-FN-NAME must be the name of a generic function which takes a
declaration identifier as first argument.

CLAUSES ::= ((&rest DECL-IDS) SIMPLE-LAMBDA-LIST      &body BODY)*
          | (:remaining       SIMPLE-LAMBDA-LIST      &body BODY)
          | (:default         SIMPLE-LAMBDA-LIST      &body BODY)
          | (:method          SPECIALIZED-LAMBDA-LIST &body BODY)*

The first clause defines methods that EQL specialize on the given
declaration identifiers.

The :remaining clause defines methods specializing on all remaining
standard declaration identifiers that haven't yet been defined via
first clauses.

The :default clause defines the default method \(i.e. specializing on
nothing.\)

The :method clause is like the :method clause in DEFGENERIC.

The lambda lists must be congruent to the lambda list of the generic
function. A SIMPLE-LAMBDA-LIST is a lambda-list with required
arguments only, where each parameter named \"_\" is substituted by
a gensym.
"
  (labels ((frob-lambda-list (simple-lambda-list)
	     ;; Replace _ in SIMPLE-LAMBDA-LIST to a gensym. Return all
	     ;; gensyms as secondary value.
	     (let ((gensyms))
	       (values
		 (mapcar #'(lambda (arg)
			     (assert (not (member arg lambda-list-keywords))
				     (simple-lambda-list)
				     "~S contains ~S, and is hence not a simple-lambda-list"
				     simple-lambda-list arg)
			     (if (eq arg '_)
				 (let ((g (gensym "IGNORE+")))
				   (prog1 g (push g gensyms)))
				 arg))
			 simple-lambda-list)
		 gensyms)))
	   (generate-default-method (simple-lambda-list body)
	     (multiple-value-bind (lambda-list gensyms) (frob-lambda-list simple-lambda-list)
	       `(defmethod ,generic-fn-name ,lambda-list
		  ,@(loop for g in gensyms collect `(declare (ignore ,g)))
		  ,@body)))
	   (generate-method (method-lambda-list body)
	     `(defmethod ,generic-fn-name ,method-lambda-list
		,@body))
	   (generate-remaining-methods (decl-ids simple-lambda-list body)
	     (loop for decl-id in decl-ids
		   collecting (generate-eql-method decl-id simple-lambda-list body)))
	   (generate-eql-method (decl-id simple-lambda-list body)
	     (multiple-value-bind (lambda-list gensyms) (frob-lambda-list simple-lambda-list)
	       (destructuring-bind (arg1 . rest-args) lambda-list
		   `(defmethod ,generic-fn-name ((,arg1 (eql ',decl-id)) ,@rest-args)
		      ,@(loop for g in gensyms collect `(declare (ignore ,g)))
		      ,@body)))))
    (let ((used-decl-ids))
      `(progn
	 ,@(loop for (decl-ids lambda-list . body) in clauses
		 if (eq decl-ids :remaining)
		   nconc (let ((remaining-ids (set-difference +standard-declaration-identifiers+
							      used-decl-ids)))
			   (generate-remaining-methods remaining-ids lambda-list body))
		 else if (eq decl-ids :default)
		   collect (generate-default-method lambda-list body)
		 else if (eq decl-ids :method)
		   collect (generate-method lambda-list body)
		 else
		   nconc (loop for decl-id in decl-ids
			       collect (generate-eql-method decl-id lambda-list body)
			       do (push decl-id used-decl-ids)))))))


(defgeneric analyze-declaration-specifier
    (declaration-identifier declaration-args compilation-env)
  (:documentation
   "Split a declaration specifier into parts.
Should return exactly three values: The declaration identifier, a list
of binding names affected by the declaration specifier, and an
arbitrary context that can be used to reconstruct the specifier from
these parts."))

(define-declaration-methods analyze-declaration-specifier
  ;; CLHS 3.3.3.1: ``A type specifier can be used as a declaration identifier.''    
  ;; CLHS 4.2.3: Conses are compound type specifiers, and symbols and
  ;; classes(!) are atomic type specifiers.
  (:method ((typespec cons) args lexenv)
    (analyze-declaration-specifier 'type `(,typespec ,@args) lexenv))
  (:method ((typespec class) args lexenv)
    (analyze-declaration-specifier 'type `(,typespec ,@args) lexenv))
  (:method ((identifier symbol) args lexenv)
    (assert (not (member identifier +standard-declaration-identifiers+)))
    (cond ((atomic-type-specifier-p identifier lexenv)
	   (analyze-declaration-specifier 'type `(,identifier ,@args) lexenv))
	  (t ;; This will probably result in an unknown entry.
	   (call-next-method))))
  ;; CLHS 4.2.3, Fig. 4-2: Standardized Atomic Declaration Identifiers.
  (#.+standard-declaration-identifiers+ (identifier args _)
    (values identifier
	    (compute-affected-variables identifier args)
	    (compute-declaration-context identifier args)))
  (:default (identifier args _)
    (values identifier '() args '%unknown-decl-spec)))

(defun atomic-type-specifier-p (symbol &optional env)
  (declare (type symbol symbol))
  (declare (special *standard-typespec-identifiers*))
  (or (member symbol *standard-typespec-identifiers*)
      ;; The SUBTYPEP form will accurately determine only those derived
      ;; types that do not expand to too complex compound type
      ;; specifiers. Hence these implementation-dependant forms:
      #+sbcl (sb-int:info :type :kind symbol)
      #+clozure (ccl::specifier-type symbol env)
      (documentation symbol 'type)
      (ignore-errors (nth-value 1 (subtypep symbol 'nil env)))))


;;; Implementation Note:
;;
;; These are used internally only. We use generic functions so we can
;; use DEFINE-DECLARATION-METHODS on them.
;;
;; In principle, the :DEFAULT method of ANALYZE-DECLARATION-SPECIFIER
;; could always involve these, giving the user another way to extend
;; the library. We don't do that for sake of simplicity (no gain of
;; actual extensibility), and because keeping track of unknown
;; declaration specifiers would be more complicated.
;;
(defgeneric compute-affected-variables (declaration-identifier decl-args))
(defgeneric compute-declaration-context (declaration-identifier decl-args))

(defun normalize-function-names (fn-names)
  (mapcar #'(lambda (fn-name) `#',fn-name) fn-names ))

(defun denormalize-function-names (function-expressions)
  (mapcar #'(lambda (fun-expr)
	      (assert (eql (first fun-expr) 'function))
	      (second fun-expr))
	  function-expressions))

(define-declaration-methods compute-affected-variables
  ((special ignore ignorable dynamic-extent) (_ args) args)
  ((inline notinline)                        (_ args) (normalize-function-names args))
  ((type)                                    (_ args) (cdr args))
  ((ftype)                                   (_ args) (normalize-function-names (cdr args)))
  (:remaining (_ _) nil))

(define-declaration-methods compute-declaration-context
  ((optimize)   (_ settings) settings)
  ((ftype type) (_ args)     (first args))
  (:remaining   (_ _)        nil))


(defgeneric build-declaration-specifier
    (declaration-identifier variables context)
  (:documentation "Reconstruct a declaration specifier."))

(define-declaration-methods build-declaration-specifier
  ((optimize)         (_ _ settings) `(optimize ,@settings))
  ((inline notinline) (id vars _)    `(,id ,@(denormalize-function-names vars)))
  ((ftype)            (_ vars ftype) `(ftype ,ftype ,@(denormalize-function-names vars)))
  ((type)             (_ vars type)  `(type  ,type  ,@vars))
  (:remaining         (id vars _)    `(,id ,@vars))
  (:default           (id _ ctx)     `(,id ,@ctx))) ; reconstruct unknown specifiers.


(deftype declaration-identifier ()  `(or symbol cons))
(deftype declaration-specifier ()   `(cons declaration-identifier *))

(locally (declare #+sbcl (sb-ext:muffle-conditions style-warning))
         ;; Make SBCL shuddup about using &OPTIONAL and &KEY together.
  (defun parse-declarations (declarations &optional compilation-env &key nostrip)
    "Parse DECLARATIONS and return a declaration-env. If NOSTRIP is
true, DECLARATIONS should be declaration specifiers rather than
declaration expressions."
    (flet ((normalize-args (args)
	     (if nostrip
		 args
		 (mapcan #'copy-list (mapcar #'rest args))))
	   (parse-decl (decl-id decl-args)
	     (multiple-value-bind (decl-id affected-variables decl-ctx unknown-flag)
		 (analyze-declaration-specifier decl-id decl-args compilation-env)
	       (make-declspec :identifier decl-id
			      :affected-variables affected-variables
			      :context decl-ctx
			      :unknownp (when unknown-flag
					  (assert (eq unknown-flag '%unknown-decl-spec))
					  t)))))
      (let ((env (make-declaration-env)))
	(dolist (decl (normalize-args declarations) env)
	  (check-type decl declaration-specifier)
	  (destructuring-bind (identifier . args) decl
	    (collect-declspec-into env (parse-decl identifier args))))))))

(defun collect-declspec-into (env spec)
  (push spec (gethash (declspec.identifier spec)
		      (declaration-env.%table env))))


(defun declaration-env.policy (env)
  "Return the optimization settings stored in ENV."
  (check-type env declaration-env)
  (loop for declspec in (gethash 'optimize (declaration-env.%table env))
	append (cdr (build-declspec declspec))))

(defun declaration-env.affected-variables (env &optional allowed-decl-ids)
  "Return a list of all binding names that are affected by the
declaration specifiers described by ENV. ALLOWED-DECL-IDS is the list
of declaration identifiers to be considered."
  (check-type env declaration-env)
  (delete-duplicates
   (let ((tbl (declaration-env.%table env)))
     (loop for decl-id being each hash-key of tbl using (hash-value declspecs)
	   when (or (null allowed-decl-ids) (member decl-id allowed-decl-ids))
	   nconcing (loop for spec in declspecs
			  nconcing (copy-list (declspec.affected-variables spec)))))))

(defun for-each-declspec (env fn)
  (let ((tbl (declaration-env.%table env)))
    (loop for decl-id being each hash-key of tbl using (hash-value declspecs)
	  do (mapc fn declspecs))))

(defmacro do-declspec ((var env &optional result) &body body)
  `(block nil
     (for-each-declspec ,env #'(lambda (,var)
				 (declare (type declspec ,var))
				 ,@body))
     ,result))


(defun build-declarations (tag env &rest more-envs)
  "Builds the declaration specifiers described by ENV. If TAG is
given, not declaration specifiers are returned, but declaration
expression where the value of TAG is used as CAR of those
expressions."
  (check-type tag (or null symbol))
  (let ((result))
    (dolist (env (cons env more-envs) result)
      (check-type env declaration-env)
      (do-declspec (spec env)
	(let ((built-spec (build-declspec spec)))
	  (if tag
	      (push `(,tag ,built-spec) result)
	      (push built-spec result)))))))

(defun build-declspec (declspec)
  (declare (type declspec declspec))
  (build-declaration-specifier (declspec.identifier declspec)
			       (declspec.affected-variables declspec)
			       (declspec.context declspec)))


(defun map-declaration-env (function env)
  "Map over ENV, calling FUNCTION with the identifier, the affected
variables, and the context of the declaration specifiers of
ENV. FUNCTION should return the new values of these as
multiple-values. The result is fresh declaration-env."
  (check-type function (or symbol function)) (check-type env declaration-env)
  (let ((new-env (make-declaration-env)))
    (do-declspec (spec env new-env)
      (let ((old-identifier (declspec.identifier spec))
	    (old-variables  (declspec.affected-variables spec))
	    (old-context    (declspec.context spec)))
	(multiple-value-bind (new-identifier new-variables new-context)
	    (funcall function old-identifier old-variables old-context)
	  (collect-declspec-into
	   new-env
	   (make-declspec :identifier new-identifier
			  :affected-variables new-variables
			  :context new-context
			  :unknownp (if (or (not (eql old-identifier new-identifier))
					    (not (eql old-variables new-variables))
					    (not (eql old-context new-context)))
					nil ; values were changed by FUNCTION.
					(declspec.unknownp spec)))))))))

(defun merge-declaration-envs (env1 env2)
  "Merge ENV1 and ENV2. The result is a fresh declaration-env."
  (check-type env1 declaration-env) (check-type env2 declaration-env)
  (flet ((multiple-value-identity (&rest args) (values-list args)))
    (let ((new-env (map-declaration-env #'multiple-value-identity env1)))
      (do-declspec (spec env2 new-env)
	(collect-declspec-into new-env spec)))))


(defun filter-declaration-env (env &key affecting not-affecting
			                (include :everything) exclude filter-function)
  "Return a fresh declaration-env that is the result of filtering ENV
according to the given parameters: :AFFECTING may be a list of binding names,
:INCLUDE may be :EVERYTHING, :BOUND, :FREE, :UNKNOWN, or a list of
declaration identifiers, likewise for :EXCLUDE. :FILTER-FUNCTION may
be a function which is called with the identifier, the affected
variables, and the context of each declaration specifier of ENV; the
FUNCTION should return a boolean."
  (check-type env declaration-env)
  (check-type affecting list) (check-type not-affecting list)
  (check-type include (or (member :everything :bound :free :unknown) list))
  (check-type exclude (or (member :everything :bound :free :unknown) list))
  (check-type filter-function (or null function))
  (flet ((designated-p (spec designator)
	   (let ((boundp   (declspec.affected-variables spec))
		 (unknownp (declspec.unknownp spec)))
	     (cond ((eq designator :everything) t)
		   ((eq designator :unknown) unknownp)
		   ((eq designator :free)    (and (not unknownp) (not boundp)))
		   ((eq designator :bound)   boundp)
		   ((listp designator)
		    (member (declspec.identifier spec) designator))
		   (t			; CHECK-TYPE above should have prevented this.
		    (error "FILTER-DECLARATION-ENV; fall through.")))))
	 (compute-intersection (bindings1 bindings2)
	   (intersection bindings1 bindings2 :test #'equal-binding-name))
	 (compute-difference (bindings1 bindings2)
	   (set-difference bindings1 bindings2 :test 'equal-binding-name))
	 (invoke-filter-function (id vars ctx)
	   (or (not filter-function)
	       (funcall filter-function id vars ctx))))
    (let ((new-env (make-declaration-env)))
      (do-declspec (spec env new-env)
	(when (and (designated-p spec include)
		   (not (designated-p spec exclude)))
	  (let* ((identifier   (declspec.identifier spec))
		 (variables    (declspec.affected-variables spec))
		 (context      (declspec.context spec))
		 (unknownp     (declspec.unknownp spec)))
	    (cond
	      ((and (null affecting) (null not-affecting))
	       (when (invoke-filter-function identifier variables context)
		 (collect-declspec-into new-env spec)))
      	      ;; So :NOT-AFFECTING includes free and unknown specs:
	      ((and (null affecting) (null variables))
	       (when (invoke-filter-function identifier variables context)
		 (collect-declspec-into new-env spec)))
	      (t
	       (let* ((intersection  (compute-intersection (or affecting variables) variables))
		      (affected-vars (compute-difference intersection not-affecting)))
		 (when (and affected-vars
			    (invoke-filter-function identifier affected-vars context))
		   (collect-declspec-into new-env
					  (make-declspec :identifier identifier
							 :affected-variables affected-vars
							 :context context
							 :unknownp unknownp))))))))))))

(defun equal-binding-name (name1 name2)
  (assert (typep name1 '(or symbol (cons (eql function)))))
  (assert (typep name2 '(or symbol (cons (eql function)))))
  (equal name1 name2))

(defmacro check-declaration-env (place &key unknown-allowed warn-only
				 &environment env)
  "Check that PLACE is a declaration-env. If UNKNOWN-ALLOWED is NIL,
further check that the declaration-env does not contain any unknown
declaration specifiers."
  (multiple-value-bind (gvars vals gstorevars setter getter)
      (get-setf-expansion place env)
    (when (second gstorevars)
      (error "CHECK-DECLARATION-ENV does not support the (VALUES ...) place."))
    (let ((gstorevar (first gstorevars)))
      `(let ,(mapcar #'list gvars vals)
	 (let ((,gstorevar (%check-declaration-env ,getter ,warn-only ,unknown-allowed)))
	   ,setter)))))

(defun %check-declaration-env (env &optional warn unknown-allowed)
  (check-type env declaration-env)
  (flet ((barf (continue-string datum &rest arguments)
	   (if warn
	       (apply #'warn datum arguments)
	       (apply #'cerror continue-string datum arguments)))
	 (unknown-specifiers (env)
	   (let (r)
	     (do-declspec (spec env r)
	       (when (declspec.unknownp spec)
		 (push (build-declspec spec) r))))))
    (unless unknown-allowed
      (let ((unknown-specs (unknown-specifiers env)))	
	(when unknown-specs
	  (barf "Ignore 'em" "Unknown declaration specifiers: ~{~S~^, ~}~%" unknown-specs)
	  (setf env (filter-declaration-env env :exclude :unknown)))))
    env))


;;; Implementation Note:
;;
;; PARSE-BODY is not exported to avoid a name conflict with Alexandria;
;; it is however guaranteed to be here as unexported symbol, so people
;; who don't want to use Alexandria can safely reference this symbol
;; directly.

(defun parse-body (body &key documentation whole)
  (flet ((starts-with (thing list)
	   (and (consp list) (eq (first list) thing))))
  ;; C.f. CLHS 1.4.1.2.1 and 3.4.11.
    (prog (current decls doc)
     :scan-body
       (setf current (car body))
       (cond ((and documentation
		   (stringp current)) (go :scan-string))
	     ((starts-with 'declare current)
	      (push (pop body) decls) (go :scan-body))
	     (t (go :finish)))
     :scan-string
       (cond ((null (cdr body))       (go :finish)) ; string counts as form
	     ((not doc)
	      (setf doc (pop body))   (go :scan-body))
	     (t (error "Too many documentation strings in ~S." (or whole body))))
     :finish
       (return (values body (nreverse decls) doc)))))


(defparameter *standard-typespec-identifiers*
  '(and array
    base-string bit-vector
    complex cons
    double-float
    eql
    float function
    integer
    long-float
    member mod
    not
    or
    rational real
    satisfies  short-float signed-byte
    simple-array simple-base-string simple-bit-vector simple-string
    single-float simple-vector string
    unsigned-byte
    values
    vector))
