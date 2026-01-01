;;;; parsing.lisp
;;;;
;;;; rewrite-cl based parsing - thin wrapper over native zipper operations
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025, 2026 Anthony Green

(in-package #:ocicl.lint)

;;; Lint context - wraps a rewrite-cl zipper
(defstruct lint-context
  "Context for linting a file using rewrite-cl zippers."
  zipper        ; The rewrite-cl zipper at root
  content       ; Original file content string
  line-index)   ; For line-based rules


(defun make-lint-context-from-string (content)
  "Create a lint context from a string of Lisp source code."
  (handler-case
      (make-lint-context
       :zipper (rewrite-cl:of-string content)
       :content content
       :line-index (build-line-index content))
    (error (e)
      (when *verbose*
        (logf "; PARSE ERROR: ~A~%" e))
      (make-lint-context
       :zipper nil
       :content content
       :line-index (build-line-index content)))))


(defun make-lint-context-from-file (path)
  "Create a lint context from a file."
  (make-lint-context-from-string (uiop:read-file-string path)))


;;; Thin wrappers for common zipper operations

(defun zip-pos (z)
  "Get (line . column) from zipper, or (1 . 1) if unavailable."
  (if-let ((pos (rewrite-cl:zip-position z)))
    (cons (rewrite-cl.node:pos-line pos)
          (rewrite-cl.node:pos-column pos))
    (cons 1 1)))

(defun zip-line (z)
  "Get line number from zipper position."
  (first (zip-pos z)))

(defun zip-column (z)
  "Get column number from zipper position."
  (rest (zip-pos z)))

(defun zip-list-p (z)
  "Check if zipper points to a list node."
  (eql (rewrite-cl:zip-tag z) :list))

(defun zip-symbol-p (z)
  "Check if zipper points to a symbol node."
  (eql (rewrite-cl:zip-tag z) :symbol))

(defun zip-head (z)
  "Get the first symbol in a list node (the 'head' of the form)."
  (when (zip-list-p z)
    (when-let ((first-child (rewrite-cl:zip-down* z)))
      (when (zip-symbol-p first-child)
        (rewrite-cl:zip-sexpr first-child)))))

(defun zip-form (z)
  "Get sexpr from zipper, or nil for non-sexpr-able nodes."
  (when (rewrite-cl.node:node-sexpr-able-p (rewrite-cl:zip-node z))
    (ignore-errors (rewrite-cl:zip-sexpr z))))


;;; Predicates for finding specific forms

(defun zip-defun-p (z)
  "Check if zipper points to a defun form."
  (and (zip-list-p z) (eq (zip-head z) 'defun)))

(defun zip-defmacro-p (z)
  "Check if zipper points to a defmacro form."
  (and (zip-list-p z) (eq (zip-head z) 'defmacro)))

(defun zip-defvar-p (z)
  "Check if zipper points to a defvar form."
  (and (zip-list-p z) (eq (zip-head z) 'defvar)))

(defun zip-defparameter-p (z)
  "Check if zipper points to a defparameter form."
  (and (zip-list-p z) (eq (zip-head z) 'defparameter)))

(defun zip-defconstant-p (z)
  "Check if zipper points to a defconstant form."
  (and (zip-list-p z) (eq (zip-head z) 'defconstant)))

(defun zip-let-p (z)
  "Check if zipper points to a let or let* form."
  (and (zip-list-p z) (member (zip-head z) '(let let*))))

(defun zip-lambda-p (z)
  "Check if zipper points to a lambda form."
  (and (zip-list-p z) (eq (zip-head z) 'lambda)))

(defun zip-if-p (z)
  "Check if zipper points to an if form."
  (and (zip-list-p z) (eq (zip-head z) 'if)))


;;; Context-aware predicates (using zip-up to check parents)

(defun zip-in-quote-p (z)
  "Check if zipper is inside a quoted context.
Detects both (quote ...) forms and 'x reader macro quotes."
  (loop for parent = (rewrite-cl:zip-up z) then (rewrite-cl:zip-up parent)
        while parent
        thereis (or ;; Quote reader macro: 'x becomes a quote-node with tag :quote
                    (eql (rewrite-cl:zip-tag parent) :quote)
                    ;; Explicit (quote ...) form
                    (and (zip-list-p parent) (eq (zip-head parent) 'quote)))))

(defun zip-in-lambda-list-p (z)
  "Check if zipper is in a lambda-list position."
  (when-let ((parent (rewrite-cl:zip-up z)))
    (when (zip-list-p parent)
      (let ((head (zip-head parent)))
        (when (member head '(defun defmacro defmethod lambda flet labels macrolet))
          (let* ((ll-index (if (eq head 'lambda) 1 2))
                 (ll-child (rewrite-cl:zip-nth-child parent ll-index)))
            (and ll-child
                 (equal (zip-pos z) (zip-pos ll-child)))))))))


;;; High-level linting operations using rewrite-cl's native functions

(defun lint-walk (ctx fn)
  "Walk all nodes in lint context, calling FN with each zipper.
Uses rewrite-cl's native zip-walk."
  (when-let ((z (lint-context-zipper ctx)))
    (rewrite-cl:zip-walk z fn)))

(defun lint-collect (ctx predicate)
  "Collect all zippers matching PREDICATE.
Uses rewrite-cl's native zip-collect."
  (when-let ((z (lint-context-zipper ctx)))
    (rewrite-cl:zip-collect z predicate)))

(defun lint-find-all (ctx predicate)
  "Find all zippers matching PREDICATE.
Uses rewrite-cl's native zip-find-all."
  (when-let ((z (lint-context-zipper ctx)))
    (rewrite-cl:zip-find-all z predicate)))

(defun lint-top-level-forms (ctx)
  "Get list of zippers for top-level list forms."
  (when-let ((z (lint-context-zipper ctx)))
    (loop for child = (rewrite-cl:zip-down* z) then (rewrite-cl:zip-right* child)
          while child
          when (zip-list-p child)
          collect child)))
