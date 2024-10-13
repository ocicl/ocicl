;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-UNICODE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-unicode/test-functions.lisp,v 1.7 2012-05-04 21:17:44 edi Exp $

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

(in-package :cl-unicode)

(defun install-one-test (property-symbol test-function)
  (setf (gethash property-symbol *property-tests*)
        test-function
        (gethash (canonicalize-name (property-name property-symbol)) *property-map*)
        property-symbol))

(defun install-script-tests ()
  (dolist (script *scripts*)
    (install-one-test script 
                      (let ((script script))
                        (lambda (c)
                          (eq (nth-value 1 (script c)) script))))))

(defun install-code-block-tests ()
  (dolist (code-block *code-blocks*)
    (install-one-test (register-property-symbol
                       (format nil "Block:~A" (property-name code-block)))
                      (let ((code-block code-block))
                        (lambda (c)
                          (eq (nth-value 1 (code-block c)) code-block))))))

(defun install-general-category-tests ()
  (dolist (general-category *general-categories*)
    (install-one-test general-category
                      (let ((general-category general-category))
                        (lambda (c)
                          (eq (nth-value 1 (general-category c)) general-category))))))

(defun install-binary-properties-tests ()
  (dolist (binary-property *binary-properties*)
    (install-one-test binary-property
                      (let ((binary-property binary-property))
                        (lambda (c)
                          (has-binary-property c binary-property))))))

(defun install-bidi-class-tests ()
  (dolist (bidi-class *bidi-classes*)
    (install-one-test (register-property-symbol
                       (format nil "BidiClass:~A" (property-name bidi-class)))
                      (let ((bidi-class bidi-class))
                        (lambda (c)
                          (eq (nth-value 1 (bidi-class c)) bidi-class))))))

(defun install-miscellaneous-tests ()
  (install-one-test (register-property-symbol "ASCII")
                    (lambda (c)
                      (<= 0 (ensure-code-point c) 127)))
  (install-one-test (register-property-symbol "Assigned")
                    (lambda (c)
                      (not (eq (nth-value 1 (general-category c))
                               '#.(property-symbol "Cn")))))
  (install-one-test (register-property-symbol "GraphemeLink")
                    (lambda (c)
                      ;; Canonical_Combining_Class=Virama
                      (eql (combining-class c) 9))))

(defun install-tests ()
  (install-script-tests)
  (install-code-block-tests)
  (install-general-category-tests)
  (install-binary-properties-tests)
  (install-bidi-class-tests)
  (install-miscellaneous-tests))