;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/drakma/drakma.asd,v 1.49 2008/05/24 03:21:22 edi Exp $

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

#+:lispworks
(unless (find-symbol "STREAM-WRITE-TIMEOUT" :stream)
  (pushnew :lw-does-not-have-write-timeout *features*))

(defpackage :drakma-asd
  (:use :cl :asdf))

(in-package :drakma-asd)

(defsystem :drakma
  :description "Full-featured http/https client based on usocket"
  :author "Dr. Edi Weitz"
  :license "BSD"
  :serial t
  :version "2.0.10"
  :components ((:file "packages")
               (:file "specials")
               (:file "conditions")
               (:file "util")
               (:file "read")
               (:file "cookies")
               (:file "encoding")
               (:file "request"))
  :depends-on (:puri
               :cl-base64
               :chunga
               :flexi-streams
               :cl-ppcre
               #-:drakma-no-chipz :chipz
               #-:lispworks :usocket
               #-(or :lispworks7.1 (and :allegro (not :allegro-cl-express)) :mocl-ssl :drakma-no-ssl) :cl+ssl)
  :perform (test-op (o s)
                    (asdf:load-system :drakma-test)
                    (asdf:perform 'asdf:test-op :drakma-test)))
