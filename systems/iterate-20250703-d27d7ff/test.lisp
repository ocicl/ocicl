(in-package :common-lisp-user)

(require #-clisp :asdf #+clisp "asdf")

(asdf:initialize-output-translations
 `(:output-translations
   (t ,(merge-pathnames (make-pathname
                         :directory '(:relative "test" "cache" :wild-inferiors)
                         :name :wild :type :wild :version :wild)
                        *load-truename*))
   :ignore-inherited-configuration
   ))

(handler-bind ((error #'(lambda (c)
                          (format t "~&Received error attempting to load ITERATE ASDF system definition:~%~a~%" c)
                          (uiop:die 4))))
  (asdf:load-asd (merge-pathnames (make-pathname
                                   :name "iterate"
                                   :type "asd")
                                  *load-truename*)))

(format t "~&Successfully loaded the iterate ASD file.~%")

#-sbcl
(asdf:load-asd (merge-pathnames (make-pathname
                                 :name "rt"
                                 :type "asd"
                                 :directory
                                 '(:relative "ext" "rt"))
                                *load-truename*))


(if (not (asdf:find-system "iterate"))
    (progn
     (format t "~&Unable to find the iterate ASDF system definition.~%")
     (uiop:quit 1))
    (format t "~&Found iterate ASDF system definition.~%"))

(defvar *build-error* nil)
(defvar *build-warning* nil)

(catch 'build-failed
 (handler-bind ((warning #'(lambda (x)
                             ;; this is necessary because on SBCL
                             ;; there's an EXTERNAL handler for some
                             ;; uninteresting warnings.
                             (signal x)
                             (push x *build-warning*)))
                (error #'(lambda (x)
                           (setf *build-error* x)
                           (throw 'build-failed t))))
   (asdf:load-system "iterate")))

(cond
  (*build-error*
   (uiop:die 1 "ITERATE build failed with an error: ~a.~%" *build-error*))
  (*build-warning*
   (uiop:die 2 "ITERATE build failed with warnings:~%~{~t~a~%~}" *build-warning*))
  (t
   (format t "ITERATE build successful.~%")))

;;; pre-loading the iterate tests system is only necessary on ECL, and I have no idea
;;; why that is. [2021/12/03:rpg]
#+ecl
(progn
 (catch 'build-failed
   (handler-bind ((error #'(lambda (x)
                             (setf *build-error* x)
                             (throw 'build-failed t))))
     (asdf:load-system "iterate/tests")))
 (cond
  (*build-error*
   (uiop:die 1 "ITERATE/TESTS load failed with an error: ~a.~%" *build-error*))
  (t
   (format t "ITERATE/TESTS load successful.~%"))))


(handler-bind ((error #'(lambda (e)
                          (if (typep e (intern (symbol-name '#:unexpected-failures-error) 'iterate.test))
                              (uiop:die 2 "~&Catching unexpected failures error: ~a~%" e)
                              (uiop:die 3 "~&Caught unexpected error ~a~%" e)))))
  (asdf:test-system "iterate")
  (uiop:quit 0))
