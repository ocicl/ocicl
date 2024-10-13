;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-unicode/cl-unicode.asd,v 1.23 2012-05-04 21:17:44 edi Exp $

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

(defsystem :cl-unicode/base
  :depends-on (:cl-ppcre)
  :serial t
  :license "BSD-2-Clause"
  :components  ((:file "packages")
                (:file "specials")
                (:file "util")))

(defsystem :cl-unicode/build
  ;; FLEXI-STREAMS is only needed to /build/ CL-UNICODE
  :depends-on (:cl-unicode/base :flexi-streams)
  :license "BSD-2-Clause"
  :components ((:module "build"
                :serial t
                :components ((:file "util")
                             (:file "char-info")
                             (:file "read")
                             (:file "dump"))))
  :output-files (load-op (o c) (values '("lists.lisp" "hash-tables.lisp" "methods.lisp") t))
  :perform (load-op (o c) (symbol-call :cl-unicode '#:create-source-files)))

(defsystem :cl-unicode
  :version "0.1.6"
  :serial t
  :description "Portable Unicode Library"
  :depends-on (:cl-unicode/base)
  :license "BSD-2-Clause"
  :components ((:file "conditions")
               (:file "lists")
               (:file "hash-tables")
               (:file "api")
               (:file "methods")
               (:file "test-functions")
               (:file "derived")
               (:file "alias"))
  :in-order-to ((test-op (test-op "cl-unicode/test"))))

(defmethod component-depends-on ((o prepare-op) (c (eql (find-system :cl-unicode))))
  `(,@(unless (every 'probe-file (output-files 'load-op :cl-unicode/build))
       '((load-op :cl-unicode/build)))
    ,@(call-next-method)))

(defsystem :cl-unicode/test
  :depends-on (:cl-unicode)
  :components ((:module "test"
                        :serial t
                        :components ((:file "packages")
                                     (:file "tests"))))
  :perform (test-op (o c) (symbol-call :cl-unicode-test '#:run-all-tests)))
