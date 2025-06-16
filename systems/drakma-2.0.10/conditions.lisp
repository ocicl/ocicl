;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ODD-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/odd-streams/conditions.lisp,v 1.5 2007/12/31 01:08:45 edi Exp $

;;; Copyright (c) 2008-2012, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :drakma)

(define-condition drakma-condition (condition)
  ()
  (:documentation "Superclass for all conditions related to Drakma."))

(define-condition drakma-error (drakma-condition error)
  ()
  (:documentation "Superclass for all errors related to Drakma."))

(define-condition drakma-simple-error (drakma-error simple-condition)
  ()
  (:documentation "Like DRAKMA-ERROR but with formatting capabilities."))

(define-condition drakma-warning (drakma-condition warning)
  ()
  (:documentation "Superclass for all warnings related to Drakma."))

(define-condition drakma-simple-warning (drakma-warning simple-condition)
  ()
  (:documentation "Like DRAKMA-WARNING but with formatting capabilities."))

(defun drakma-warn (format-control &rest format-arguments)
  "Signals a warning of type DRAKMA-SIMPLE-WARNING with the
provided format control and arguments."
  (warn 'drakma-simple-warning
        :format-control format-control
        :format-arguments format-arguments))

(define-condition parameter-error (drakma-simple-error)
  ()
  (:documentation "Signalled if a function was called with
inconsistent or illegal parameters."))

(defun parameter-error (format-control &rest format-arguments)
  "Signals an error of type PARAMETER-ERROR with the provided
format control and arguments."
  (error 'parameter-error
         :format-control format-control
         :format-arguments format-arguments))

(define-condition syntax-error (drakma-simple-error)
  ()
  (:documentation "Signalled if Drakma encounters wrong or unknown
syntax when reading the reply from the server."))

(defun syntax-error (format-control &rest format-arguments)
  "Signals an error of type SYNTAX-ERROR with the provided
format control and arguments."
  (error 'syntax-error
         :format-control format-control
         :format-arguments format-arguments))

(define-condition cookie-error (drakma-simple-error)
  ((cookie :initarg :cookie
           :initform nil
           :reader cookie-error-cookie
           :documentation "The COOKIE object that caused this error.
Can be NIL in case such an object couldn't be initialized."))
  (:documentation "Signalled if someone tries to create a COOKIE object that's not valid."))

(defun cookie-error (cookie format-control &rest format-arguments)
  "Signals an error of type COOKIE-ERROR with the provided cookie
\(can be NIL), format control and arguments."
  (error 'cookie-error
         :cookie cookie
         :format-control format-control
         :format-arguments format-arguments))

(define-condition cookie-date-parse-error (cookie-error)
  ()
  (:documentation "Signalled if Drakma tries to parse the date of an
incoming cookie header and can't interpret it."))

(defun cookie-date-parse-error (format-control &rest format-arguments)
  "Signals an error of type COOKIE-DATE-PARSE-ERROR with the provided
format control and arguments."
  (error 'cookie-date-parse-error
         :format-control format-control
         :format-arguments format-arguments))
