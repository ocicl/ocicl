;;; ocicl.lint.asd
;;;
;;; ASDF system definition for the ocicl linter
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
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

(asdf:defsystem #:ocicl.lint
  :description "A comprehensive Common Lisp linter with style and error checking"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :version "2.7.5"
  :homepage "https://github.com/ocicl/ocicl"
  :bug-tracker "https://github.com/ocicl/ocicl/issues"
  :source-control (:git "https://github.com/ocicl/ocicl.git")
  :serial t
  :depends-on (:alexandria
               :serapeum
               :ecclesia
               :rewrite-cl)
  :components ((:file "package")
               (:file "config")
               (:file "file-utils")
               (:file "asd-support")
               (:file "parsing")
               (:file "fixer")
               (:module "rules"
                :components ((:file "line-based")
                             (:file "ast")
                             (:file "single-pass")))
               (:module "fixes"
                :components ((:file "whitespace")
                             (:file "style")))
               (:file "linter")
               (:file "main")))
