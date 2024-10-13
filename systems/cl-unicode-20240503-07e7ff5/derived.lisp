;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-UNICODE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-unicode/derived.lisp,v 1.15 2012-05-04 21:17:44 edi Exp $

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

(defconstant +xid-difference+
  ;; the usual mumbo jumbo for SBCL...
  (if (boundp '+xid-difference+)
    (symbol-value '+xid-difference+)
    '(#x37a
      (#x309b . #x309c)
      (#xfc5e . #xfc63)
      (#xfdfa . #xfdfb)
      #xfe70
      #xfe72
      #xfe74
      #xfe76
      #xfe78
      #xfe7a
      #xfe7c
      #xfe7e)))

(defvar *derived-map*
  `(("Any")
    ("LC" "Lu" "Ll" "Lt")
    ("L" "LC" "Lm" "Lo")
    ("M" "Mn" "Mc" "Me")
    ("N" "Nd" "Nl" "No")
    ("P" "Pc" "Pd" "Ps" "Pe" "Pi" "Pf" "Po")
    ("S" "Sm" "Sc" "Sk" "So")
    ("Z" "Zs" "Zl" "Zp")
    ("C" "Cc" "Cf" "Cs" "Co" "Cn")
    ("Math" "Sm" "OtherMath")
    ("Alphabetic" "L" "Nl" "OtherAlphabetic")
    ("Lowercase" "Ll" "OtherLowercase")
    ("Uppercase" "Lu" "OtherUppercase")
    ("Cased" "Lowercase" "Uppercase" "Lt")
    ("CaseIgnorable" "Mn" "Me" "Cf" "Lm" "Sk"
                     ,(lambda (c) (find (word-break c)
                                        `("Single_Quote"
                                          "MidLetter"
                                          "MidNumLet")
                                        :test 'equal)))
    ("GraphemeExtend" "Me" "Mn" "OtherGraphemeExtend")
    ("GraphemeBase" ("C" "Zl" "Zp" "GraphemeExtend"))
    ("IDStart" "L" "Nl" "OtherIDStart" ("PatternSyntax" "PatternWhiteSpace"))
    ("IDContinue" "IDStart" "Mn" "Mc" "Nd" "Pc" "OtherIDContinue" ("PatternSyntax" "PatternWhiteSpace"))
    ("XIDStart" "IDStart" (,@+xid-difference+ #xe33 #xeb3 (#xff9e . #xff9f)))
    ("XIDContinue" "IDContinue" ,+xid-difference+)
    ("DefaultIgnorableCodePoint" "OtherDefaultIgnorableCodePoint" "Cf" "VariationSelector"
                                 ("WhiteSpace" (#xfff9 . #xfffb) (#x600 . #x603) #x6dd #x70f))
    ("ChangesWhenLowercased" ,(lambda (c)
                                (let ((nfd (canonical-decomposition c)))
                                  (not (equal (lowercase-mapping nfd :want-special-p t) nfd)))))
    ("ChangesWhenUppercased" ,(lambda (c)
                                (let ((nfd (canonical-decomposition c)))
                                  (not (equal (uppercase-mapping nfd :want-special-p t) nfd)))))
    ("ChangesWhenTitlecased" ,(lambda (c)
                                (let ((nfd (canonical-decomposition c)))
                                  (not (equal (titlecase-mapping nfd :want-special-p t) nfd)))))
    ("ChangesWhenCasemapped" ,(lambda (c)
                                (let ((nfd (canonical-decomposition c)))
                                  (or (not (equal (lowercase-mapping nfd :want-special-p t) nfd))
                                      (not (equal (uppercase-mapping nfd :want-special-p t) nfd))
                                      (not (equal (titlecase-mapping nfd :want-special-p t) nfd))))))
    ("ChangesWhenCasefolded" ,(lambda (c)
                                (let ((nfd (canonical-decomposition c)))
                                  (not (equal (case-fold-mapping nfd :want-code-point-p t) nfd)))))))

(defun build-derived-test-function (property-designators)
  (labels ((build-test-function (designator)
             (etypecase designator
               (string
                (let ((test-function (gethash (gethash designator *property-map*) *property-tests*)))
                  (assert test-function (designator)
                          "Unknown property name ~S." designator)
                  test-function))
               (integer
                (lambda (c)
                  (= (ensure-code-point c) designator)))
               (cons
                (let ((from (car designator))
                      (to (car designator)))
                  (assert (and (typep from 'integer) (typep to 'integer)) (designator)
                          "Car and cdr of ~S must both be integers." designator)
                  (lambda (c)
                    (<= from (ensure-code-point c) to))))
               (function
                designator)))
           (collect-test-functions (designators)
             (loop for designator in designators
                   collect (build-test-function designator))))
    (let ((positive-test-functions
            (collect-test-functions (remove-if-not 'atom property-designators)))
          (negative-test-functions
            (collect-test-functions (find-if-not 'atom property-designators))))
      (cond (negative-test-functions
             (lambda (c)
               (and (or (null positive-test-functions)
                        (loop for test-function in positive-test-functions
                                thereis (funcall (the function test-function) c)))
                    (not (loop for test-function in negative-test-functions
                                 thereis (funcall (the function test-function) c))))))
            (t
             (lambda (c)
               (or (null positive-test-functions)
                   (loop for test-function in positive-test-functions
                           thereis (funcall (the function test-function) c)))))))))

(defun build-derived-test-functions ()
  (loop for (name . property-names) in *derived-map*
        for symbol = (register-property-symbol name) do
          (assert (null (gethash symbol *property-tests*)) (name)
                  "There is already a property named ~S." name)
          (setf (gethash symbol *property-tests*)
                (build-derived-test-function property-names)
                (gethash name *property-map*)
                symbol)))
