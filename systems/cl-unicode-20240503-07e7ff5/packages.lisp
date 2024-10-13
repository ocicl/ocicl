;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-unicode/packages.lisp,v 1.25 2012-05-04 21:17:44 edi Exp $

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

(in-package :cl-user)

(defpackage :cl-unicode
  (:use :cl)
  (:import-from :cl-ppcre
                :*standard-optimize-settings*
                :with-rebinding)
  (:export :+code-point-limit+
           :*scripts-to-try*
           :*try-abbreviations-p*
           :*try-hex-notation-p*
           :*try-lisp-names-p*
           :*try-unicode1-names-p*
           :age
           :bidi-class
           :bidi-classes
           :bidi-mirroring-glyph
           :binary-properties
           :canonicalize-name
           :character-named
           :code-block
           :code-blocks
           :combining-class
           :disable-alternative-character-syntax
           :enable-alternative-character-syntax
           :general-categories
           :general-category
           :has-binary-property
           :has-property
           :list-all-characters
           :lowercase-mapping
           :numeric-type
           :numeric-value
           :property-name
           :property-symbol
           :property-test
           :recognized-properties
           :script
           :scripts
           :titlecase-mapping
           :word-break
           :unicode-error
           :unicode-name
           :unicode1-name
           :uppercase-mapping
           :case-fold-mapping
           :idna-mapping
           :canonical-decomposition
           :normalization-form-c
           :normalization-form-d
           :normalization-form-k-c
           :normalization-form-k-d))

(defpackage :cl-unicode-names
  (:use))