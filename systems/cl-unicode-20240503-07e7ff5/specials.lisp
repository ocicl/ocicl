;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-UNICODE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-unicode/specials.lisp,v 1.17 2012-05-04 21:17:44 edi Exp $

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

(defconstant +code-point-limit+ #x110000
  "The smallest integer which is not a code point in the Unicode codespace.")

(defvar *canonical-names* (make-hash-table :test 'eq :size 500)
  "A hash tables which maps property symbols \(see PROPERTY-SYMBOL) to
their \"canonical names\", i.e. to strings.")

(defvar *names-to-code-points* (make-hash-table :test 'equalp :size 20000)
  "A hash table which \(case-insensitively) maps \"canonicalized\"
character names to their code points.")

(defvar *unicode1-names-to-code-points* (make-hash-table :test 'equalp :size 2000)
  "A hash table which \(case-insensitively) maps \"canonicalized\"
Unicode 1.0 character names to their code points.")

(defvar *code-points-to-names* (make-hash-table :size 20000)
  "A hash table which maps code points to the corresponding character
names.")

(defvar *code-points-to-unicode1-names* (make-hash-table :size 2000)
  "A hash table which maps code points to the corresponding Unicode
1.0 character names.")

(defvar *case-mappings* (make-hash-table :size 2100)
  "A hash table which maps code points to three-element lists
containing the lowercase, uppercase, and titlecasse mapping of the
corresponding character \(unless all of them are NIL).")

(defvar *special-case-mappings* (make-hash-table)
  "A hash table which maps code points to a list of special case mapping rules.")

(defvar *composition-mappings* (make-hash-table))

(defvar *general-categories* nil
  "A list of all property symbols which denote general categories.")

(defvar *compatibility-formatting-tags* nil
  "A list of Character Decomposition compatibility formatting tags.")

(defvar *scripts* nil
  "A list of all property symbols which denote scripts.")

(defvar *code-blocks* nil
  "A list of all property symbols which denote blocks.")

(defvar *binary-properties* nil
  "A list of all property symbols which denote binary properties.")

(defvar *bidi-classes* nil
  "A list of all property symbols which denote Bidi classes.")

(defvar *property-map* (make-hash-table :test 'equalp :size 1000)
  "A hash table which \(case-insensitively) maps \"canonicalized\"
property names \(including aliases) to the corresponding property
symbols.")

(defvar *property-tests* (make-hash-table :test 'eq :size 360)
  "A hash table which maps property symbols to a test function which
tests for the corresponding property.")

(defvar *property-aliases* (make-hash-table :test 'equalp :size 360)
  "A hash table which maps property names to the long name for
the property.")

(defvar *jamo-short-names* (make-hash-table :size 70)
  "A hash table which maps code points to their Jamo short names.
Needed to compute Hangul syllable names - see COMPUTE-HANGUL-NAME.")

(defvar *hangul-syllables-to-code-points* (make-hash-table :test 'equalp :size 12000)
  "A hash table which \(case-insensitively) maps Hangul syllable name
parts to their code points.")

(defvar *try-unicode1-names-p* t
  "This is the default value for the :TRY-UNICODE1-NAMES-P keyword
argument to CHARACTER-NAMED.")

(defvar *try-abbreviations-p* nil
  "This is the default value for the :TRY-ABBREVIATIONS-P keyword
argument to CHARACTER-NAMED.")

(defvar *scripts-to-try* nil
  "This is the default value for the :SCRIPTS-TO-TRY keyword argument
to CHARACTER-NAMED.")

(defvar *try-hex-notation-p* nil
  "This is the default value for the :TRY-HEX-NOTATION-P keyword
argument to CHARACTER-NAMED.")

(defvar *try-lisp-names-p* nil
  "This is the default value for the :TRY-LISP-NAMES-P keyword
argument to CHARACTER-NAMED.")

(defvar *previous-readtables* nil
  "A stack which holds the previous readtables that have been pushed
here by ENABLE-ALTERNATIVE-CHARACTER-SYNTAX.")

(pushnew :cl-unicode *features*)

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>
;; also used by LW-ADD-ONS

(defvar *hyperdoc-base-uri* "http://weitz.de/cl-unicode/")

(let ((exported-symbols-alist
       (loop for symbol being the external-symbols of :cl-unicode
             collect (cons symbol
                           (concatenate 'string
                                        "#"
                                        (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol
                exported-symbols-alist
                :test #'eq))))
