(defpackage :mgl-pax.asdf
  (:use :common-lisp)
  (:export #:compile-per-file
           #:compile-with-source-info
           #:compile-pax
           #:call-with-wrappers
           #:compose-wrappers))

(in-package :mgl-pax.asdf)

(defun compile-per-file (continuation)
  (with-compilation-unit (:override t)
    (funcall continuation)))

(defun compile-with-source-info (continuation)
  (let (#+(and allegro (version>= 8 2))
        (compiler:save-source-level-debug-info-switch t)
        #+allegro
        (excl:*load-source-file-info* t)
        #+(and allegro (version>= 8 2))
        (excl:*load-source-debug-info* t))
    (funcall continuation)))

(defun compile-without-some-warnings (continuation)
  (let (#+allegro (compiler:*cltl1-compile-file-toplevel-compatibility-p* nil)
        #+allegro (excl:*redefinition-warnings* nil))
    (funcall continuation)))

(defun compile-pax (continuation)
  (call-with-wrappers '(compile-per-file compile-with-source-info
                        compile-without-some-warnings)
                      continuation))

(defun call-with-wrappers (wrappers fn)
  (if wrappers
      (funcall (first wrappers)
               (lambda ()
                 (call-with-wrappers (rest wrappers) fn)))
      (funcall fn)))

(defun compose-wrappers (wrappers)
  (lambda (continuation)
    (call-with-wrappers wrappers continuation)))
