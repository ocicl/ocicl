;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-UNICODE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-unicode/alias.lisp,v 1.9 2012-05-04 21:17:43 edi Exp $

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

(defvar *alias-map*
  '(("L" . "Letter")
    ("LC" . "CasedLetter")
    ("Lu" . "UppercaseLetter")
    ("Ll" . "LowercaseLetter")
    ("Lt" . "TitlecaseLetter")
    ("Lm" . "ModifierLetter")
    ("Lo" . "OtherLetter")
    ("M" . "Mark")
    ("Mn" . "NonspacingMark")
    ("Mc" . "SpacingMark")
    ("Me" . "EnclosingMark")
    ("N" . "Number")
    ("Nd" . "DecimalNumber")
    ("Nl" . "LetterNumber")
    ("No" . "OtherNumber")
    ("P" . "Punctuation")
    ("Pc" . "ConnectorPunctuation")
    ("Pd" . "DashPunctuation")
    ("Ps" . "OpenPunctuation")
    ("Pe" . "ClosePunctuation")
    ("Pi" . "InitialPunctuation")
    ("Pf" . "FinalPunctuation")
    ("Po" . "OtherPunctuation")
    ("S" . "Symbol")
    ("Sm" . "MathSymbol")
    ("Sc" . "CurrencySymbol")
    ("Sk" . "ModifierSymbol")
    ("So" . "OtherSymbol")
    ("Z" . "Separator")
    ("Zs" . "SpaceSeparator")
    ("Zl" . "LineSeparator")
    ("Zp" . "ParagraphSeparator")
    ("C" . "Other")
    ("Cc" . "Control")
    ("Cf" . "Format")
    ("Cs" . "Surrogate")
    ("Co" . "PrivateUse")
    ("Cn" . "Unassigned")
    ("Cn" . "NoncharacterCodePoint")))

(defvar *bidi-alias-map*
  '(("L" . "LeftToRight")
    ("LRE" . "LeftToRightEmbedding")
    ("LRO" . "LeftToRightOverride")
    ("R" . "RightToLeft")
    ("AL" . "RightToLeftArabic")
    ("RLE" . "RightToLeftEmbedding")
    ("RLO" . "RightToLeftOverride")
    ("PDF" . "PopDirectionalFormat")
    ("EN" . "EuropeanNumber")
    ("ES" . "EuropeanNumberSeparator")
    ("ET" . "EuropeanNumberTerminator")
    ("AN" . "ArabicNumber")
    ("CS" . "CommonNumberSeparator")
    ("NSM" . "NonSpacingMark")
    ("BN" . "BoundaryNeutral")
    ("B" . "ParagraphSeparator")
    ("S" . "SegmentSeparator")
    ("WS" . "Whitespace")
    ("ON" . "OtherNeutral")))

(defun create-alias (new-name old-name &optional only-if-unambiguous)
  (setq new-name (canonicalize-name new-name)
        old-name (canonicalize-name old-name))
  (unless only-if-unambiguous
    (assert (null (gethash new-name *property-map*)) (new-name)
      "There is already a property named ~S." new-name))
  (when (gethash new-name *property-map*)
    (return-from create-alias))
  (assert (gethash old-name *property-map*) (old-name)
    "There is no property named ~S." old-name)
  (setf (gethash new-name *property-map*)
        (gethash old-name *property-map*)))

(defun create-aliases ()
  (loop for (old-name . new-name) in *alias-map*
        do (create-alias new-name old-name))
  (loop for (old-name . new-name) in *bidi-alias-map*
        do (create-alias (format nil "BidiClass:~A" new-name)
                         (format nil "BidiClass:~A" old-name)))
  (loop for name in (scripts)
        do (create-alias (format nil "Script:~A" name) name))
  (loop for name in (loop for name being the hash-keys of *property-map*
                          collect name)
        unless (ppcre:scan ":" name)
        do (create-alias (format nil "Is~A" name) name))
  (loop for name in (code-blocks)
        do (create-alias (format nil "In~A" name) (format nil "Block:~A" name)))
  (loop for name in (code-blocks)
        do (create-alias name (format nil "Block:~A" name) t))
  (loop for name in (bidi-classes)
        do (create-alias name (format nil "BidiClass:~A" name) t))
  (loop for (old-name . new-name) in *bidi-alias-map*
        do (create-alias new-name (format nil "BidiClass:~A" old-name) t)))

(defun build-all-property-tests ()
  (clrhash *property-map*)
  (clrhash *property-tests*)
  (install-tests)
  (build-derived-test-functions)
  (create-aliases))

(build-all-property-tests)