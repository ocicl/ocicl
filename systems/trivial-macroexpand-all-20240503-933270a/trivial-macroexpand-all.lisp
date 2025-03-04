(in-package #:trivial-macroexpand-all)

#+abcl
(defun macroexpand-all (form &optional env)
  (values (ext:macroexpand-all form env) t t))

#+allegro
(defun macroexpand-all (form &optional env)
  (declare (ignore env))
  #+(and allegro-version>= (version>= 8 0))
  (values (excl::walk-form form) t nil)
  #-(and allegro-version>= (version>= 8 0))
  (values (excl::walk form) t nil))

#+ccl
(defun macroexpand-all (form &optional env)
  (values (ccl:macroexpand-all form env) t t))

#+clisp
(defun macroexpand-all (form &optional env)
  (declare (ignore env))
  (values (ext:expand-form form) t nil))

#+cmucl
(defun macroexpand-all (form &optional env)
  (values (walker:macroexpand-all form env) t t))

#+corman
(defun macroexpand-all (form &optional env)
  (declare (ignore env))
  (values (ccl:macroexpand-all form) t nil))

#+ecl
(defun macroexpand-all (form &optional env)
  (values (walker:macroexpand-all form env) t t))

#+lispworks
(defun macroexpand-all (form &optional env)
  (declare (ignore env))
  (values (walker:walk-form form) t nil))

#+mkcl
(defun macroexpand-all (form &optional env)
  (declare (ignore env))
  (values (walker:macroexpand-all form) t nil))

#+sbcl
(defun macroexpand-all (form &optional env)
  (values (sb-cltl2:macroexpand-all form env) t t))

#+scl
(defun macroexpand-all (form &optional env)
  (declare (ignore env))
  (values (macroexpand form) t nil))

#-(or abcl allegro ccl clisp cmucl corman ecl lispworks mkcl sbcl scl)
(defun macroexpand-all (form &optional env)
  (declare (ignore env))
  (values form nil nil))
