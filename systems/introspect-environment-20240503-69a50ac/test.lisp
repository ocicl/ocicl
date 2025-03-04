(defpackage #:introspect-environment-test
  (:use #:cl #:introspect-environment #:5am))

(in-package #:introspect-environment-test)

(def-suite :introspect-environment)

(in-suite :introspect-environment)

(deftype foo (&optional dim) `(array integer ,dim))
(deftype bar () '(foo (7)))

(defvar *x*)
(defconstant +y+ t)

(test typexpand-1
  (is (equal '(array integer *) (typexpand-1 '(foo))))
  (is (equal '(array integer (4 * 7)) (typexpand-1 '(foo (4 * 7)))))
  (is (equal '(foo (7)) (typexpand-1 '(bar)))))

(test typexpand
  ;; might need to change this if an impl implements array as a macro
  ;; which is unlikely, of course, but...
  (is (equal '(array integer (7)) (typexpand '(bar)))))

(defmacro in-environment (eparam name wrapper &body expr)
  `(macrolet ((,name (&environment ,eparam)
		(declare (ignorable ,eparam))
		;; sorry for the ``` programming
		;; ",@expr" is at in-environment's expansion time.
		;; the rest are all in ,name's.
		#+(or)`(quote ,(progn ,@expr)) ; misses mult values
		`(values ,@(mapcar (lambda (x) `(quote ,x))
				   (multiple-value-list
				    (progn ,@expr))))))
     ,wrapper))

;;; hypothetically an implementation could have some tighter type,
;;;  so we try to use subtypep.
(test variable-type
  (is-true
   (in-environment
       env testmacro
       (let ((x 'x))
	 (declare (symbol x) (ignorable x))
	 (testmacro))
     (subtypep (variable-type 'x env) 'symbol))))

(test specialp
  (is-false (specialp '+y+))
  (is-true (specialp '*x*))
  (is-false (in-environment
		env testmacro
		(let ((x nil))
		  (declare (ignorable x))
		  (testmacro))
	      (specialp 'x env)))
  (is-true (in-environment
	       env testmacro
	       (let ((x nil))
		 (declare (special x) (ignorable x))
		 (testmacro))
	     (specialp 'x env)))
  (is-false (in-environment
		env testmacro
		(let ((*x* nil))
		  (testmacro))
	      (specialp 'x env))))

(test function-type-local
  ;; no subtypep on function types. sad, no?
  (is (tree-equal
       ;; sbcl does normalization to its weird ideas about VALUES.
       #-sbcl'(function () symbol)
       #+sbcl'(function () (values &optional symbol &rest t))
       (in-environment
	   env testmacro
	   (flet ((x () 'x))
	     (declare (ftype (function () symbol) x)
		      (ignorable #'x))
	     (testmacro))
	 (function-type 'x env)))))

(test constant-form-value
  (is-false (constant-form-value nil))
  (is-true (constant-form-value '+y+)))

(test policy
  (multiple-value-bind (p pq)
      (in-environment
	  env testmacro
	  (locally (declare (optimize (debug 3) (speed 2)
				      (compilation-speed 1) (space 0)))
	    (testmacro))
	(values (policy (> debug compilation-speed space) env)
		(policy-quality 'speed env)))
    (is-true p)
    (is (= 2 pq))))

(declaim (ftype (function (t list) list) memq))
(test function-type-global
  ;; sbcl does normalization to its weird ideas about VALUES.
  (is (tree-equal
       #-sbcl'(function (t list) list)
       #+sbcl'(function (t list) (values &optional list &rest t))
       (function-type 'memq))))

(declaim (type integer *foo*))
(test variable-type-global
  (is (subtypep (variable-type '*foo*) 'integer)))

(defun memq (item list)
  (member item list :test #'eq))

(define-compiler-macro memq (&whole whole item list &environment env)
  (if (constantp list env)
      (let ((list (constant-form-value list env))
	    (isym (gensym "ITEM")))
	`(let ((,isym ,item))
	   (or ,@(mapcar (lambda (com) `(eq ,isym ',com)) list))))
      whole))

(defun memb (item list &key (test #'eql))
  (member item list :test test))

(define-compiler-macro memb (&whole whole item list &key test)
  (if (equal test '(function eq))
      `(memq ,item ,list)
      whole))

(test compiler-macroexpand-1
  (is (equal '(memq x (foo))
	     (compiler-macroexpand-1 '(memb x (foo) :test #'eq))))
  #+(or ccl cmucl)
  (skip "~a's compiler macros do not correctly expand ~s forms"
	(lisp-implementation-type) 'funcall)
  #-(or ccl cmucl)
  (is (equal '(memq x (foo))
	     (compiler-macroexpand-1
	      '(funcall #'memb x (foo) :test #'eq))))
  (let ((form '(memb x (foo)))
	#-(or ccl cmucl)
	(funcall-form '(funcall #'memb x (foo))))
    (is (eql form (compiler-macroexpand-1 form)))
    #+(or ccl cmucl)
    (skip "~a's compiler macros do not correctly expand ~s forms"
	  (lisp-implementation-type) 'funcall)
    #-(or ccl cmucl)
    (is (eql funcall-form (compiler-macroexpand-1 funcall-form)))))

(test compiler-macroexpand
  (is (equal '(memq x (foo))
	     (compiler-macroexpand '(memb x (foo) :test #'eq))))
  (is (not
       (eq (first (compiler-macroexpand '(memb x '(a b) :test #'eq)))
	   'memq))))

(test compiler-macroexpand-environment
  (is-false
   (nth-value
    1
    (in-environment env testmacro
	(flet ((memb (&rest args) 11))
	  (declare (ignorable #'memb))
	  (testmacro))
      (compiler-macroexpand '(memb x (foo) :test #'eq) env)))
   "~s does not properly respect shadowing"
   'compiler-macroexpand))

(test compiler-macroexpand-hook
  (let* ((*counter* 0)
	 (*macroexpand-hook*
	  (compile nil
		   '(lambda (expander form env)
		      (declare (special *counter*))
		      (incf *counter*)
		      (funcall expander form env)))))
    (declare (special *counter*))
    (compiler-macroexpand '(memb x '(a b) :test #'eq))
    (is-true (> *counter* 0)
	     "~s does not respect ~s"
	     'compiler-macroexpand '*macroexpand-hook*)))

(defmacro compiler-lambda (name lambda-list &body body &environment env)
  (parse-compiler-macro name lambda-list body env))

(defun expansion (expander expandee env)
  (funcall *macroexpand-hook* expander expandee env))

(defmacro expands-to (expander expandee expansion env
		      &rest reason-args)
  `(is (equal ',expansion (expansion ,expander ',expandee ,env))
       ,@reason-args))

(test parse-compiler-macro
  (expands-to (compiler-lambda foo () '(bar)) (foo) (bar) nil)
  (let ((fn (compiler-lambda foo (&whole whole ((a) &rest b)
				  &optional (why "hey"))
	      (cond ((eq a 'none)
		     `(bar ,a why '(,@b ,why) a))
		    ((eq a 'return)
		     (return-from foo whole))))))
    (is (functionp fn))
    (expands-to fn
		(foo ((none) 67 68))
		(bar none why '(67 68 "hey") a)
		nil
		"basic failure in lambda list parsing")
    (expands-to fn
		(foo ((return)))
		(foo ((return)))
		nil
		"parsed compiler macro does not establish block")
    (expands-to fn
		(funcall #'foo ((none)) 'x)
		(bar none why '('x) a)
		nil
		"parsed compiler macro does not deal with funcall correctly"))
  ;; haha oh man
  (multiple-value-bind (e1 e2)
      (in-environment
	  env testmacro
	  (macrolet ((foo () 'baz)) (testmacro))
	(values (expansion
		 (compiler-lambda foo (bar &environment env)
		   (macroexpand bar env))
		 '(foo (foo)) env)
		(expansion
		 (compiler-lambda foo (&environment env bar)
		   (macroexpand bar env))
		 '(foo (foo)) env)))
    (is (eql e1 'baz) "&environment parsing")
    (is (eql e2 'baz) "&environment parsing in a different position")))
