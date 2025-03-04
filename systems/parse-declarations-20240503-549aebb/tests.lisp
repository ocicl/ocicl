(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-rt)
  (require :alexandria)
  (require :parse-declarations-1.0))

(defpackage :tcr.parse-declaration-tests-1.0
  (:use :cl :tcr.parse-declarations-1.0 :sb-rt)
  (:import-from :tcr.parse-declarations-1.0
		#:declspec.identifier
		#:declspec.affected-variables
		#:declspec.context
		#:declspec.unknownp
		#:do-declspec
		#:declaration-env.%table))

(in-package :tcr.parse-declaration-tests-1.0)

;;; This may not work in the general case, but for the declaration
;;; specifiers used in the test suite it does.
;;;
(defun declspec= (spec1 spec2)
  (let ((identifier1 (declspec.identifier spec1))
	(identifier2 (declspec.identifier spec2))
	(variables1  (declspec.affected-variables spec1))
	(variables2  (declspec.affected-variables spec2))
	(context1    (declspec.context spec1))
	(context2    (declspec.context spec2))
	(unknownp1   (declspec.unknownp spec1))
	(unknownp2   (declspec.unknownp spec2)))
    (and
     (eql   identifier1 identifier2)
     (equal variables1  variables2)
     (equal context1    context2)
     (eql   unknownp1   unknownp2))))

(defun declaration-env= (env1 env2)
  (flet ((subenvp (env1 env2)
	   (do-declspec (spec1 env1 t)
	     (unless (some #'(lambda (spec2) (declspec= spec1 spec2))
			   (gethash (declspec.identifier spec1)
				    (declaration-env.%table env2)))
	       (return nil)))))
    (and (subenvp env1 env2) (subenvp env2 env1))))

(defun declaration-env.count (env)
  (let ((count 0))
    (do-declspec (spec env count)
      (declare (ignore spec))
      (incf count))))

(defun multiple-value-identity (&rest args)
  (values-list args))

(defun parse-decl-specs (specs)
  (parse-declarations specs nil :nostrip t))


(defparameter *sample-declaration-specifiers*
  '((optimize (speed 3) (safety 0))
    (special *a*)
    (inline f)
    (author "Tobias C Rittweiler")
    (type integer x y)
    (optimize (debug 0))
    (type fixnum z)
    ((string 512) output)
    (type (vector unsigned-byte 32) chunk)
    (quux *a*)
    (float *f*)
    (ftype (function (number) float) f)
    (type #.(find-class 'declaration-env) env1 env2)
    ))

(defparameter *sample-declaration-env*
  (parse-decl-specs *sample-declaration-specifiers*))


(deftest parse-declarations.1
    (values
     (declaration-env= *sample-declaration-env* *sample-declaration-env*)
     (declaration-env= *sample-declaration-env* (parse-decl-specs *sample-declaration-specifiers*))
     (declaration-env= *sample-declaration-env*
		       (parse-decl-specs (alexandria:shuffle *sample-declaration-specifiers*)))
     (declaration-env= *sample-declaration-env*
		       (parse-declarations (mapcar #'(lambda (spec) `(declare ,spec))
						   *sample-declaration-specifiers*))))
  t t t t)


(deftest build-declarations.1
    (values
     (declaration-env= *sample-declaration-env*
		       (parse-declarations (build-declarations 'declare *sample-declaration-env*)))
     (declaration-env= *sample-declaration-env*
		       (parse-decl-specs (build-declarations nil *sample-declaration-env*))))
  t t)


(deftest declaration-env.policy.1
    (alexandria:set-equal (declaration-env.policy *sample-declaration-env*)
			  '((debug 0) (speed 3) (safety 0))
			  :test 'equal)
  t)


(deftest declaration-env.affected-variables.1
    (alexandria:set-equal (declaration-env.affected-variables *sample-declaration-env*)
			  '(*A* OUTPUT ENV1 ENV2 CHUNK *F* Z X Y #'F #'F)
			  :test 'equal)
  t)

(deftest declaration-env.affected-variables.2
    (values (alexandria:set-equal (declaration-env.affected-variables *sample-declaration-env*
								      '(type))
				  '(env1 env2 *f* chunk output z x y)
				  :test 'equal)
	    (alexandria:set-equal (declaration-env.affected-variables *sample-declaration-env*
								      '(type))
				  (declaration-env.affected-variables
				   (filter-declaration-env *sample-declaration-env*
							   :include '(type)))
				  :test 'equal))
  t t)


(deftest merge-declaration-envs.1
    (let ((new-decl-specs '((optimize (safety 2))
			    (notinline g)
			    (ftype (function (string) string) g))))
      (values
       (declaration-env= (merge-declaration-envs *sample-declaration-env* *sample-declaration-env*)
			 *sample-declaration-env*)
       (declaration-env= (merge-declaration-envs *sample-declaration-env*
						 (parse-decl-specs new-decl-specs))
			 (parse-decl-specs (append new-decl-specs *sample-declaration-specifiers*)))))
  t t)

(deftest merge-declaration-envs.2	; test for freshness.
    (eq *sample-declaration-env*
	(merge-declaration-envs *sample-declaration-env* *sample-declaration-env*))
  nil)


(deftest map-declaration-env.1
    (let ((env (map-declaration-env #'multiple-value-identity
				    *sample-declaration-env*)))
      (values (declaration-env= env *sample-declaration-env*)
	      (eq env *sample-declaration-env*)))
    t nil)

(deftest map-declaration-env.2
    (flet ((resolve-unknowns (id args ctx)
	     (cond ((eq id 'author)
		    (values id args (copy-list ctx)))
		   ((eq id 'quux)
		    (values 'type ctx 'quux))
		   (t
		    (values id args ctx)))))
      (let ((env (map-declaration-env #'resolve-unknowns *sample-declaration-env*)))
	(values
	 (declaration-env= env *sample-declaration-env*)
	 ;; Number of specifiers hasn't changed..
	 (= (declaration-env.count env) (declaration-env.count *sample-declaration-env*))
	 ;; But there are no unknown specifiers anymore..
	 (zerop (declaration-env.count (filter-declaration-env env :include :unknown)))
	 ;; Instead, (QUUX *A*) became a type declaration..
	 (= (declaration-env.count (filter-declaration-env env :include '(type)))
	    (1+ (declaration-env.count (filter-declaration-env *sample-declaration-env*
							       :include '(type)))))
	 ;; And, (AUTHOR ...) a free declaration..
	 (= (declaration-env.count (filter-declaration-env env :include :free))
	    (1+ (declaration-env.count (filter-declaration-env *sample-declaration-env*
							       :include :free)))))))
  nil
  t
  t
  t
  t)


(deftest filter-declaration-env.1	; test for freshness..
    (values
     (eq *sample-declaration-env* (filter-declaration-env *sample-declaration-env*))
     (declaration-env= *sample-declaration-env* (filter-declaration-env *sample-declaration-env*)))
  nil t)

(deftest filter-declaration-env.2	; test :INCLUDE
    (values
     ;; UNKNOWN
     (alexandria:set-equal
      (build-declarations nil (filter-declaration-env *sample-declaration-env* :include :unknown))
      '((quux *a*) (author "Tobias C Rittweiler"))
      :test 'equal)
     ;; BOUND
     (declaration-env= (filter-declaration-env *sample-declaration-env* :include :bound)
		       (parse-decl-specs '((ftype (function (number) float) f)
					   (type #.(find-class 'declaration-env) env1 env2)
					   (type float *f*)
					   (type (vector unsigned-byte 32) chunk)
					   (type (string 512) output)
					   (type fixnum z)
					   (type integer x y)
					   (inline f)
					   (special *a*))))
     ;; FREE
     (alexandria:set-equal
      (build-declarations nil (filter-declaration-env *sample-declaration-env* :include :free))
      '((optimize (debug 0)) (optimize (speed 3) (safety 0)))
      :test 'equal)
     ;; SPECIFIC
     (alexandria:set-equal
      (build-declarations nil (filter-declaration-env *sample-declaration-env* :include '(inline special)))
      '((inline f) (special *a*))
      :test 'equal))
  t t t t)

(deftest filter-declaration-env.3	; test :EXCLUDE
    (values
     ;; :exclude :UNKNOWN == :include :bound + :free
     (declaration-env= (filter-declaration-env *sample-declaration-env* :exclude :unknown)
		       (merge-declaration-envs (filter-declaration-env *sample-declaration-env*
								       :include :bound)
					       (filter-declaration-env *sample-declaration-env*
								       :include :free)))
     ;; :exclude :BOUND == :include :unknown + :free
     (declaration-env= (filter-declaration-env *sample-declaration-env* :exclude :bound)
		       (merge-declaration-envs (filter-declaration-env *sample-declaration-env*
								       :include :unknown)
					       (filter-declaration-env *sample-declaration-env*
								       :include :free)))
     ;; :exclude :FREE == :include :bound + :unknown
     (declaration-env= (filter-declaration-env *sample-declaration-env* :exclude :free)
		       (merge-declaration-envs (filter-declaration-env *sample-declaration-env*
								       :include :bound)
					       (filter-declaration-env *sample-declaration-env*
								       :include :unknown)))
     ;; SPECIFIC
     (alexandria:set-equal
      (build-declarations nil (filter-declaration-env *sample-declaration-env* :exclude '(type ftype)))
      '((quux *a*)
	(author "Tobias C Rittweiler")
	(inline f)
	(special *a*)
	(optimize (debug 0))
	(optimize (speed 3) (safety 0)))
      :test 'equal))
  t t t t)

(deftest filter-declaration-env.4	; test :INCLUDE + :EXCLUDE
    (values
     (zerop (declaration-env.count
	     (filter-declaration-env *sample-declaration-env* :include '(does-not-exists))))
     (declaration-env=
      (filter-declaration-env *sample-declaration-env* :exclude '(does-not-exists))
      *sample-declaration-env*)
     (zerop (declaration-env.count
	     (filter-declaration-env *sample-declaration-env* :include '(type) :exclude '(type))))
     (declaration-env= (filter-declaration-env *sample-declaration-env*
					       :include '(ftype type) :exclude '(type))
		       (filter-declaration-env *sample-declaration-env*
					       :include '(ftype))))
  t t t t)

(deftest filter-declaration-env.5	; test :AFFECTING
    (values
     (alexandria:set-equal
      (build-declarations nil (filter-declaration-env *sample-declaration-env*
						      :affecting '(*a* #'f)))
      '((ftype (function (number) float) f)
	(inline f)
	(special *a*))
      :test 'equal)
     (null (build-declarations nil (filter-declaration-env *sample-declaration-env*
							   :affecting '(f)))) ; not #'f!
     (equal (build-declarations nil (filter-declaration-env *sample-declaration-env*
							    :affecting '(z)))
	    '((type fixnum z))))
    t t t)

(deftest filter-declaration-env.6	; test :NOT-AFFECTING
    (values
     (declaration-env=
      (filter-declaration-env *sample-declaration-env*
			      :not-affecting (declaration-env.affected-variables *sample-declaration-env*))
      (merge-declaration-envs (filter-declaration-env *sample-declaration-env* :include :free)
			      (filter-declaration-env *sample-declaration-env* :include :unknown)))
     (declaration-env= *sample-declaration-env*
		       (filter-declaration-env *sample-declaration-env* :not-affecting '(does-not-exist))))
    t t)

(deftest filter-declaration-env.7	; test :FILTER-FUNCTION
    (flet ((filter-bound-decls (id args ctx)
	     (declare (ignore ctx))
	     (let ((vars (declaration-env.affected-variables *sample-declaration-env* `(,id))))
	       (when vars
		 (assert (intersection vars args :test 'equal))
		 t))))
      
      (declaration-env=
       (filter-declaration-env *sample-declaration-env* :filter-function #'filter-bound-decls)
       (filter-declaration-env *sample-declaration-env* :include :bound)))
    t)

(deftest filter-declaration-env.8	; test :AFFECTING + :NOT-AFFECTING
    (values
     (declaration-env= (filter-declaration-env *sample-declaration-env*
					       :affecting '(*a* #'f x y z)
					       :not-affecting '(x y))
		       (filter-declaration-env *sample-declaration-env*
					       :affecting '(*a* #'f z))))
    t)

(deftest filter-declaration-env.9	; test everything together
    (values
     (alexandria:set-equal
      (build-declarations nil (filter-declaration-env *sample-declaration-env*
						      :affecting '(#'f) :include '(ftype)))
      '((ftype (function (number) float) f))
      :test 'equal)
     (alexandria:set-equal
      (build-declarations nil (filter-declaration-env *sample-declaration-env*
						      :affecting '(#'f) :exclude '(ftype)))
      '((inline f))
      :test 'equal))
  t t)