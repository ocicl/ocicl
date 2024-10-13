;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-UNICODE-TEST; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-unicode/test/tests.lisp,v 1.18 2012-05-04 21:17:49 edi Exp $

;;; Copyright (c) 2008-2012, Dr. Edmund Weitz. All rights reserved.

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

(in-package :cl-unicode-test)

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
                       verbose)
  "Loops through all the forms in the file FILE-NAME and executes each
of them using EVAL.  Reads all forms with the alternative character
syntax enabled.  It is assumed that each FORM specifies a test which
returns a true value iff it succeeds.  Prints each test form to
*STANDARD-OUTPUT* if VERBOSE is true and shows a simple progress
indicator otherwise.  Returns a true value iff all tests succeeded."
  (enable-alternative-character-syntax)
  (unwind-protect
      (with-open-file (stream file-name)
        (let ((*package* (find-package :cl-unicode-test))
              (*try-unicode1-names-p* t)
              (*try-abbreviations-p* t)
              (*scripts-to-try* '("Hebrew"))
              (*try-hex-notation-p* t)
              (*try-lisp-names-p* t))
          (do-tests ((format nil "Simple tests from file ~S" (file-namestring file-name))
                     (not verbose))
            (let ((form (or (read stream nil) (done))))
              (when verbose
                (format t "~&~S" form))
              (cond ((eval form) nil)
                    (t (list (format nil "~S returned NIL" form))))))))
    (disable-alternative-character-syntax)))

(defun property-tests (&key (file-name
                             (make-pathname :name "properties"
                                            :type nil :version nil
                                            :defaults *this-file*))
                            verbose)
  "Loops through all the forms in the file FILE-NAME and executes each
of them as a test for a property.  The forms must be lists \(C S B)
where C is a code point \(an integer), S is a string denoting the
property, and B is boolean denoting whether the character has the
property or not.  Tests are performed using HAS-PROPERTY.  Prints each
test to *STANDARD-OUTPUT* if VERBOSE is true and shows a simple
progress indicator otherwise.  Returns a true value iff all tests
succeeded."
  (with-open-file (stream file-name)
    (do-tests ((format nil "Properties from file ~S" (file-namestring file-name))
               (not verbose))
      (let ((input-line (or (read stream nil) (done))))
        (destructuring-bind (char-code property-name expected-result)
            input-line
          (when verbose
            (format t "~&~A: #x~X" property-name char-code))
          (let* ((char (and (< char-code char-code-limit) (code-char char-code)))
                 (result-1 (has-property char-code property-name))
                 (result-2 (and char (has-property char property-name)))
                 errors)
            (unless (eq expected-result (not (not result-1)))
              (push (format nil "code point #x~X should ~:[not ~;~]have property \"~A\""
                            char-code expected-result property-name)
                    errors))
            (when char
              (unless (eq expected-result (not (not result-2)))
                (push (format nil "\(code-char #x~X) should ~:[not ~;~]have property \"~A\""
                              char-code expected-result property-name)
                      errors)))
            errors))))))

(defun normalization-tests (&key (file-name
                                  (make-pathname :name "normalization-forms"
                                                 :type nil :version nil
                                                 :defaults *this-file*))
                              verbose)
  "Loops through all the forms in the file FILE-NAME and executes each
of them as a test for a property.  The forms must be lists \(C S B)
where C is a code point \(an integer), S is a string denoting the
property, and B is boolean denoting whether the character has the
property or not.  Tests are performed using HAS-PROPERTY.  Prints each
test to *STANDARD-OUTPUT* if VERBOSE is true and shows a simple
progress indicator otherwise.  Returns a true value iff all tests
succeeded."
  (with-open-file (stream file-name)
    (do-tests ((format nil "Normalization forms from file ~S" (file-namestring file-name))
               (not verbose))
      (let ((input-line (or (read stream nil) (done))))
        (destructuring-bind (source nfc nfd nfkc nfkd)
            input-line
          (when verbose
            (format t "~&~A: " source))
          (remove-if #'null
                     (mapcar #'(lambda (name expected result)
                                 (unless (equal expected result)
                                   (format nil "~A~A should be ~A, got ~A"
                                           name source expected result)))
                             '("NFC" "NFD" "NFKC" "NFKD")
                             (list nfc nfd nfkc nfkd)
                             (list (cl-unicode:normalization-form-c source)
                                   (cl-unicode:normalization-form-d source)
                                   (cl-unicode:normalization-form-k-c source)
                                   (cl-unicode:normalization-form-k-d source)))))))))

(defun run-all-tests (&key verbose)
  "Runs all tests for CL-UNICODE and returns a true value iff all
tests succeeded.  VERBOSE is interpreted by the individual test suites
above."
  (let ((successp t))
    (macrolet ((run-test-suite (&body body)
                 `(unless (progn ,@body)
                    (setq successp nil))))
      ;; run the automatically generated tests for derived properties
      (run-test-suite (property-tests :verbose verbose
                                      :file-name (make-pathname :name "derived-properties"
                                                                :type nil :version nil
                                                                :defaults *this-file*)))
      (run-test-suite (property-tests :verbose verbose))
      (run-test-suite (simple-tests :verbose verbose))
      (run-test-suite (normalization-tests :verbose verbose)))
    (format t "~2&~:[Some tests failed~;All tests passed~]." successp)
    successp))
