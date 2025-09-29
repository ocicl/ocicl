;;; ocicl.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2023, 2024, 2025  Anthony Green <green@moxielogic.com>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.
;;;

(asdf:defsystem #:ocicl
  :description "Common Lisp system management"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :version "2.7.0"
  :serial t
  :components ((:module "runtime"
                :components ((:static-file "asdf.lisp")
                             (:static-file "ocicl-runtime.lisp")))
               (:file "http")
               (:file "tree")
               (:file "package")
               (:module "lint"
                :components ((:file "package")
                             (:file "config")
                             (:file "file-utils")
                             (:file "asd-support")
                             (:file "parsing")
                             (:file "linter")
                             (:module "rules"
                              :components ((:file "line-based")
                                           (:file "ast")
                                           (:file "single-pass")))
                             (:file "main")))
               (:file "ocicl" :depends-on ("runtime" "http" "package" "tree" "lint")))

  :depends-on ( :with-user-abort :unix-opts :drakma :cl-json :cl-interpol :tar
                :tar/simple-extract :copy-directory :diff :cl-template
                :version-string :alexandria
                :ecclesia :eclector

                ;; sbcl internals
                (:feature :sbcl :sb-posix)
                (:feature :sbcl :sb-bsd-sockets)
                (:feature :sbcl :sb-rotate-byte)
                (:feature :sbcl :sb-cltl2)
                (:feature :sbcl :sb-introspect)
                (:feature :sbcl :sb-concurrency)
                (:feature :sbcl :sb-sprof)
                (:feature :sbcl :sb-md5))
  :build-operation "program-op"
  :build-pathname "ocicl"
  :entry-point "ocicl:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
