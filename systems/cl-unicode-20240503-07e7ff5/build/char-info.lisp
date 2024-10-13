;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-UNICODE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-unicode/build/char-info.lisp,v 1.6 2012-05-04 21:17:45 edi Exp $

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

(defclass char-info ()
  ((code-point :initarg :code-point
               :reader code-point
               :type fixnum
               :documentation "The code point of the character.  This
is redundant information, but convenient.")
   (name :initarg :name
         :initform nil
         :reader name
         :type (or string null)
         :documentation "The name of the character - a string.")
   (script :initform nil
           :accessor script*
           :type (or symbol null)
           :documentation "The script the character belongs to - a
property symbol.")
   (code-block :initform nil
               :accessor code-block*
               :type (or symbol null)
               :documentation "The block the character belongs to - a
property symbol.")
   (word-break :initarg :word-break
                                        ;"other" is value unless indicated otherwise
               :initform (register-property-symbol "Other")
               :accessor word-break*
               :type symbol
               :documentation "The word_break value of the character
- a property symbol")
   (unicode1-name :initarg :unicode1-name
                  :initform nil
                  :reader unicode1-name*
                  :type (or string null)
                  :documentation "The Unicode 1.0 name of the
character - a string.")
   (age :initform nil
        :accessor age*
        :type list
        :documentation "The Unicode version this character first
appeared in, a cons of two integers which denote the major and minor
version.")
   (general-category :initarg :general-category
                     ;; this is the default for unassigned characters
                     ;; - see READ-BINARY-PROPERTIES
                     :initform (property-symbol "Cn")
                     :reader general-category*
                     :type symbol
                     :documentation "The general category of this
character - a property symbol.")
   (bidi-class :initarg :bidi-class
               ;; will be defaulted later, see
               ;; SET-DEFAULT-BIDI-CLASSES
               :initform nil
               :accessor bidi-class*
               :type symbol
               :documentation "The Bidi class of the character - a
property symbol.")
   (bidi-mirroring-glyph :initform nil
                         :accessor bidi-mirroring-glyph*
                         :type (or fixnum null)
                         :documentation "The code point of the mirror
image of the character, if there is one.")
   (binary-props :initarg :binary-props
                 :initform nil
                 :accessor binary-props*
                 :type list
                 :documentation "A list of property symbols denoting
the binary properties of the character.")
   (combining-class :initarg :combining-class
                    ;; the default combining class
                    :initform 0
                    :reader combining-class*
                    :type fixnum
                    :documentation "The combining class of the
character - an integer.")
   (numeric-type :initarg :numeric-type
                 :initform nil
                 :reader numeric-type*
                 :type symbol
                 :documentation "The numeric type \(one of
\"Decimal\", \"Digit\", or \"Numeric\") of the character if it has one
- a property symbol.")
   (numeric-value :initarg :numeric-value
                  :initform nil
                  :reader numeric-value*
                  :type (or rational null)
                  :documentation "The numeric value of the character
if it has one - a Lisp rational.")
   (uppercase-mapping :initarg :uppercase-mapping
                      :initform nil
                      :reader uppercase-mapping*
                      :type (or fixnum null)
                      :documentation "The simple uppercase mapping of
the character \(as a code point) if explicitly specified.")
   (lowercase-mapping :initarg :lowercase-mapping
                      :initform nil
                      :reader lowercase-mapping*
                      :type (or fixnum null)
                      :documentation "The simple lowercase mapping of
the character \(as a code point) if explicitly specified.")
   (titlecase-mapping :initarg :titlecase-mapping
                      :initform nil
                      :reader titlecase-mapping*
                      :type (or fixnum null)
                      :documentation "The simple titlecase mapping of
the character \(as a code point) if explicitly specified.")
   (case-folding-mapping :initarg :case-folding-mapping
                         :initform nil
                         :accessor case-folding-mapping*
                         :type list
                         :documentation "The case folding mapping of
the character \(as a list of \(status, code point)) if explicitly specified.")
   (idna-mapping :initarg :idna-mapping
                 :initform nil
                 :accessor idna-mapping*
                 :type list
                 :documentation "IDNA Mapping table entry for the code point")
   (decomposition-mapping :initarg :decomposition-mapping
                          :initform nil
                          :accessor decomposition-mapping*
                          :type list
                          :documentation "Character decomposition mapping including optional Compatibility Formatting Tag"))
  (:documentation "A CHAR-INFO object is a datastructure which is used
to \(temporarily) hold the information about one character as gathered
from parsing the Unicode data files - see the code in read.lisp."))

