;;;; api.lisp - Public API for rewrite-cl
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl)

;;; This file serves as the main entry point for users of rewrite-cl.
;;; All public symbols are re-exported from the rewrite-cl package.
;;; See package.lisp for the full list of exports.

;;; Quick Start Examples:
;;;
;;; Parse and navigate:
;;;   (let ((z (of-string "(defun foo (x) x)")))
;;;     (zip-down* z)        ; move to 'defun'
;;;     (zip-right* *)       ; move to 'foo'
;;;     (zip-sexpr *))       ; => FOO
;;;
;;; Modify code:
;;;   (let ((z (of-string "(+ 1 2)")))
;;;     (setf z (zip-down* z))   ; move to '+'
;;;     (setf z (zip-right* z))  ; move to '1'
;;;     (setf z (zip-replace z (make-token-node 10)))
;;;     (zip-root-string z))    ; => "(+ 10 2)"
;;;
;;; Find and transform:
;;;   (zip-prewalk (of-string "(+ 1 (* 2 3))")
;;;     (lambda (z)
;;;       (if (and (eql :number (zip-tag z)))
;;;           (zip-edit z (lambda (n) (* 2 n)))
;;;           z)))
;;;
;;; Create nodes programmatically:
;;;   (make-list-node
;;;     (list (make-token-node 'defun)
;;;           (spaces 1)
;;;           (make-token-node 'bar)
;;;           (spaces 1)
;;;           (make-list-node nil)))

;;; The library is organized in layers:
;;;
;;; 1. rewrite-cl.node - Node types and protocols
;;;    - Defines node structures (whitespace, token, list, etc.)
;;;    - Protocol functions: node-tag, node-string, node-sexpr, etc.
;;;    - Constructors: make-token-node, make-list-node, etc.
;;;
;;; 2. rewrite-cl.parser - Parsing functions
;;;    - parse-string, parse-file - Parse source to nodes
;;;
;;; 3. rewrite-cl.zip - Zipper navigation and editing
;;;    - of-string, of-file - Create zippers from source
;;;    - Navigation: zip-up, zip-down, zip-left, zip-right, etc.
;;;    - Editing: zip-replace, zip-edit, zip-insert-*, zip-remove
;;;    - Finding: zip-find, zip-find-tag, zip-find-value
;;;    - Walking: zip-prewalk, zip-postwalk
;;;
;;; 4. rewrite-cl - Convenience re-exports of the above
