;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-INTERPOL-TEST; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-interpol/test/tests.lisp,v 1.4 2008/07/23 16:10:13 edi Exp $

;;; Copyright (c) 2008, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-interpol-test)

(defvar *this-file* (load-time-value
                     (or #.*compile-file-pathname* *load-pathname*))
  "The location of this source file.  Needed to find the data files.")

(defmacro do-tests ((name &optional show-progress-p) &body body)
  "Helper macro which repeatedly executes BODY until the code in body
calls the function DONE.  It is assumed that each invocation of BODY
will be the execution of one test which returns NIL in case of success
and a list of strings describing errors otherwise.

The macro prints a simple progress indicator \(one dots for ten tests)
to *STANDARD-OUTPUT* unless SHOW-PROGRESS-P is NIL and returns a true
value iff all tests succeeded.  Errors in BODY are caught and reported
\(and counted as failures)."
  `(let ((successp t)
         (testcount 1))
     (block test-block
       (flet ((done ()
                (return-from test-block successp)))
         (format t "~&Test: ~A~%" ,name)
         (loop
          (when (and ,show-progress-p (zerop (mod testcount 10)))
            (format t ".")
            (when (zerop (mod testcount 100))
              (terpri))
            (force-output))
          (let ((errors
                 (handler-case
                     (progn ,@body)
                   (error (msg)
                     (list (format nil "~&got an unexpected error: ~A" msg))))))
            (setq successp (and successp (null errors)))
            (when errors
              (format t "~&~4@A:~{~&   ~A~}~%" testcount errors))
            (incf testcount)))))
     successp))

(defun simple-tests (&key (file-name
                           (make-pathname :name "simple"
                                          :type nil :version nil
                                          :defaults *this-file*))                          
                          (external-format '(:latin-1 :eol-style :lf))
                          verbose
                          named-readtables)
  "Loops through all the forms in the file FILE-NAME and executes each
of them using EVAL.  The CL-INTERPOL syntax is enabled when the forms
are read.  It is assumed that each FORM specifies a test which returns
a true value iff it succeeds.  Prints each test form to
*STANDARD-OUTPUT* if VERBOSE is true and shows a simple progress
indicator otherwise.  EXTERNAL-FORMAT is the FLEXI-STREAMS external
format which is used to read the file.  Returns a true value iff all
tests succeeded.

\(SETQ ...) forms are treated special in that they're just EVALuated
but not counted as tests.  The global special variables exported by
CL-INTERPOL \(and some from CL-UNICODE as well) are rebound during the
tests so that they can be safely set in the test files."
  (if named-readtables
      (named-readtables:in-readtable :interpol-syntax)
      (enable-interpol-syntax))

  (unwind-protect
      (with-open-file (binary-stream file-name :element-type 'flex:octet)
        (let* ((stream (flex:make-flexi-stream binary-stream :external-format external-format))
               (*package* (find-package :cl-interpol-test))
               (*list-delimiter* *list-delimiter*)
               (*outer-delimiters* *outer-delimiters*)
               (*inner-delimiters* *inner-delimiters*)
               (*optional-delimiters-p* *optional-delimiters-p*)
               (*scripts-to-try* *scripts-to-try*)
               (*try-abbreviations-p* *try-abbreviations-p*)
               (*try-hex-notation-p* *try-hex-notation-p*)
               (*try-lisp-names-p* *try-lisp-names-p*)
               (*try-unicode1-names-p* *try-unicode1-names-p*))
          (do-tests ((format nil "Simple tests from file ~S" (file-namestring file-name))
                     (not verbose))
            (let* ((form (or (read stream nil) (done)))
                   (setqp (eq (first form) 'setq)))
              (when (and verbose (not setqp))
                (format t "~&~S" form))
              (cond (setqp (eval form) nil)
                    ((eval form) nil)
                    (t (list (format nil "~S returned NIL" form))))))))
    (if named-readtables
        (named-readtables:in-readtable :standard)
        (disable-interpol-syntax))))

(defun run-all-tests (&key verbose)
  "Runs all tests for CL-INTERPOL and returns a true value iff all
tests succeeded.  VERBOSE is interpreted by the individual test suites
above."
  (let ((successp t))
    (macrolet ((run-test-suite (&body body)
                 `(unless (progn ,@body)
                    (setq successp nil))))
      ;; run the automatically generated Perl tests
      (loop for named-readtables in (list nil t)
            do (format t "~2&Testing with activation through ~A~2%"
                       (if named-readtables
                           "named-readtables"
                           "enable-interpol-syntax"))
               (run-test-suite (simple-tests :file-name (make-pathname :name "perltests"
                                                                       :type nil :version nil
                                                                       :defaults *this-file*)
                                             :verbose verbose
                                             :named-readtables named-readtables))
               (run-test-suite (simple-tests :verbose verbose
                                             :named-readtables named-readtables))))
    (format t "~2&~:[Some tests failed~;All tests passed~]." successp)
    successp))
