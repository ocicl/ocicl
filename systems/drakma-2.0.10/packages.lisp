;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/drakma/packages.lisp,v 1.22 2008/01/14 01:57:01 edi Exp $

;;; Copyright (c) 2006-2012, Dr. Edmund Weitz.  All rights reserved.

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

(defpackage :drakma
  (:use :cl :flexi-streams :chunga)
  ;; the variable defined in the ASDF system definition
  (:shadow #:syntax-error #:parameter-error)
  (:export #:*drakma-version*
           #:*allow-dotless-cookie-domains-p*
           #:*body-format-function*
           #:*remove-duplicate-cookies-p*
           #:*default-http-proxy*
           #:*no-proxy-domains*
           #:*drakma-default-external-format*
           #:*header-stream*
           #:*ignore-unparseable-cookie-dates-p*
           #:*text-content-types*
           #:cookie
           #:cookie-error
           #:cookie-error-cookie
           #:cookie-date-parse-error
           #:cookie-domain
           #:cookie-expires
           #:cookie-http-only-p
           #:cookie-jar
           #:cookie-jar-cookies
           #:cookie-name
           #:cookie-path
           #:cookie-securep
           #:cookie-value
           #:cookie=
           #:delete-old-cookies
           #:decode-stream
           #:drakma-condition
           #:drakma-error
           #:drakma-warning
           #:get-content-type
           #:header-value
           #:http-request
           #:parameter-error
           #:parameter-present-p
           #:parameter-value
           #:parse-cookie-date
           #:read-tokens-and-parameters
           #:split-tokens
           #:syntax-error
           #:url-encode))
