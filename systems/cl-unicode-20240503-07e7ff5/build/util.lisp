;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-UNICODE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-unicode/build/util.lisp,v 1.13 2012-05-04 21:17:45 edi Exp $

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

(defparameter *this-file* (load-time-value
                           (or #.*compile-file-pathname* *load-pathname*))
  "The location of this source file.  Needed to find the data files.")

(defvar *char-database* nil
  "This will be a vector of CHAR-INFO objects which keeps track of the
information parsed from the Unicode files while the build process
runs.  This \"database\" is not used by CL-UNICODE itself.  It only
serves as a temporary storage during the build process.")

(defun make-empty-char-database ()
  "Creates and returns an array of length +CODE-POINT-LIMIT+
initialized with NILs."
  (make-array +code-point-limit+ :initial-element nil))

(defun initialize-property-symbols ()
  "Clears the hash table *CANONICAL-NAMES* and initializes it with
certain values which might otherwise not be seen when the Unicode
files are parsed."
  (clrhash *canonical-names*)
  (dolist (name '("Cn" "AL" "R" "L" "Decimal" "Digit" "Numeric" "BidiMirrored" "NoncharacterCodePoint"))
    (register-property-symbol name)))

(defun extract-variables (bindings)
  "Helper function for the WITH-UNICODE-FILE macro.  Collects the
variables from the list BINDINGS where atoms as well as the first
element of BINDINGS are left as is and lists are replaced with their
first elements.  The symbol _ \(underline) is skipped, i.e. not
returned."
  (cons (first bindings)
        (loop for binding in (rest bindings)
              unless (eq binding '_)
              when (listp binding)
              collect (first binding)
              else collect binding)))

(defun extract-types (bindings)
  "Helper function for the WITH-UNICODE-FILE macro.  Collects the type
spec from the list BINDINGS where it is assumed the for lists the type
spec is the second element and for atoms the type spec is defaulted to
STRING.  The first argument of BINDINGS is skipped, and the type spec
NIL is included whenever the symbol _ \(underline) is seen."
  (loop for binding in (rest bindings)
        when (eq binding '_)
        collect nil
        else when (listp binding)
        collect (second binding)
        else collect 'string))

(defun extract-defaults (bindings)
  "Helper function for the WITH-UNICODE-FILE macro.  Collects the
default values from the list BINDINGS where it is assumed the for
lists the default value is the third element and for atoms the default
value is :ERROR \(meaning to signal an error).  The first argument of
BINDINGS is skipped, and the default value NIL is included whenever
the symbol _ \(underline) is seen."
  (loop for binding in (rest bindings)
        when (eq binding '_)
        collect nil
        else when (and (listp binding) (cddr binding))
        collect (third binding)
        else collect :error))

(defun code-point-range-start (range)
  "Returns the first code point of the code point \"range\" RANGE
where RANGE is either an integer denoting one code point or a cons of
the form (A . B) denoting the code points from A to B \(inclusive)."
  (cond ((atom range) range)
        (t (car range))))

(defun parse-code-point (string)
  "Parses a string which is supposed to be the hexadecimal
representation of one code point or a range of code points \(written
with two dots between them like \"0AE0..0AF3\").  Returns a single
integer \(for one code point) or a cons of two integers \(for a
range)."
  (destructuring-bind (first &optional second)
      (mapcar 'parse-hex (ppcre:split "\\.\\." string))
    (if second (cons first second) first)))

(defgeneric parse-value (value type default)
  (:documentation "Parses the string VALUE coming from a Unicode data
file and converts it according to the type spec TYPE \(a symbol
denoting a type which is not necessarily a Lisp type).  If VALUE is
the empty string, DEFAULT is returned instead unless DEFAULT is :ERROR
in which case an error is signalled.")
  (:method (value type default)
    "The default method for unrecognized type specs."
    (error "Don't know how to parse type ~S." type))
  (:method :around (value type default)
    "The method to take care of default values."
    (cond ((and (string= value "") (eq default :error))
           (error "No value and no default provided"))
          ((string= value "") default)
          (t (call-next-method)))))

(defmethod parse-value (value (type (eql 'string)) default)
  "The method for strings simply returns VALUE."
  value)

(defmethod parse-value (value (type (eql 'boolean)) default)
  "The method for BOOLEAN only accepts the strings \"Y\" and \"N\"."
  (cond ((string= value "Y") t)
        ((string= value "N") nil)
        (t (error "Expected boolean, but got ~S." value))))

(defmethod parse-value (value (type (eql 'symbol)) default)
  "The method for symbol which converts the string to a \"property
symbol\" \(see PROPERTY-SYMBOL) and registers it \(see
REGISTER-PROPERTY-SYMBOL)."
  (if value
      (register-property-symbol value)
      (if (symbolp default)
          default
          (register-property-symbol value))))

(defmethod parse-value (value (type (eql 'integer)) default)
  "The method for \(decimal) integers."
  (parse-integer value))

(defmethod parse-value (value (type (eql 'hex)) default)
  "The method for hexadecimal integers."
  (parse-hex value))

(defmethod parse-value (value (type (eql 'hex-list)) default)
  "The method for lists of hexadecimal integers."
  (loop for val in (ppcre:split " " value)
        collect (parse-value val 'hex default)))

(defmethod parse-value (value (type (eql 'tagged-hex-list)) default)
  "The method for hexadecimal integers."
  (loop for val in (ppcre:split " " value)
        collect (parse-value val
                             (if (and val
                                      (char= #\< (char val 0)))
                                 'symbol
                                 'hex)
                             default)))


(defmethod parse-value (value (type (eql 'rational)) default)
  "The method for rationals which are written like Lisp rationals."
  (destructuring-bind (numerator &optional (denominator 1))
      (mapcar 'parse-integer (ppcre:split "/" value))
    (/ numerator denominator)))

(defmethod parse-value (value (type (eql 'age)) default)
  "The method for Unicode \"age\" values which are converted to a
list of two integers, the major and minor version."
  (destructuring-bind (major minor)
      (mapcar 'parse-integer (ppcre:split "\\." value))
    (list major minor)))

(defun parse-one-line (parts &optional types defaults)
  "Parses one line of a Unicode data file and returns a list of Lisp
objects as determined by TYPES and DEFAULTS.  It is assumed that the
line was already split into a list PARTS of individual strings, one
for each value/object.  The elements of TYPES and DEFAULTS are
interpreted as by PARSE-VALUE except that we skip one element of PARTS
for each NIL in TYPES.  The first element of PARTS is always
interpreted as a code point \(range), i.e. TYPES and DEFAULTS only
apply to the rest of PARTS.

Note that a call like \(PARSE-ONE-LINE PARTS) means that just the code
point part is parsed and returned."
  (cons (parse-code-point (first parts))
        (loop for type in types
              for default in defaults
              for data = (rest parts) then (rest data)
              when type
                collect (parse-value (car data) type default))))
