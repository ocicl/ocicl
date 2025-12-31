;;;; ast.lisp
;;;;
;;;; AST-based linting rules for semantic analysis
;;;; Uses rewrite-cl's native zipper functions for traversal
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025 Anthony Green

(in-package #:ocicl.lint)

;;; Style helpers from The One True Lisp Style Guide

(defun symbol-name-has-underscore-p (sym)
  "Check if symbol SYM contains underscores in its name."
  (and (symbolp sym) (position #\_ (symbol-name sym))))

(defun star-delimited-name-p (sym)
  "Check if symbol SYM follows *GLOBAL-VARIABLE* naming convention."
  (and (symbolp sym)
       (let* ((n (symbol-name sym)) (len (length n)))
         (and (>= len 2)
              (char= (char n 0) #\*)
              (char= (char n (1- len)) #\*)))))

(defun plus-delimited-name-p (sym)
  "Check if symbol SYM follows +CONSTANT+ naming convention."
  (and (symbolp sym)
       (let* ((n (symbol-name sym)) (len (length n)))
         (and (>= len 2)
              (char= (char n 0) #\+)
              (char= (char n (1- len)) #\+)))))


;;; Rule: Naming conventions and package :use usage
(defun rule-naming-and-packages (path ctx)
  "Check naming conventions and package usage patterns.
CTX is a lint-context."
  (when *verbose* (logf "; naming-and-packages: checking ~A~%" path))
  (let ((issues nil))
    ;; Walk all forms (skip quoted contexts to avoid false positives)
    (lint-walk ctx
      (lambda (z)
        (when (and (zip-list-p z)
                   (not (zip-in-quote-p z)))
          (let ((head (zip-head z))
                (form (zip-form z)))
            (when (and head form)
              (let ((pos (zip-pos z))
                    (ln) (col))
                (setf ln (car pos) col (cdr pos))
                (case head ; lint:suppress
                  ((defvar defparameter) ; lint:suppress special-name-style
                   (let ((name (second form)))
                     (unless (star-delimited-name-p name)
                       (push (%make-issue path ln col "special-name-style"
                                          (format nil "~A ~A should be named like *special-var*" head name))
                             issues))
                     (when (symbol-name-has-underscore-p name)
                       (push (%make-issue path ln col "naming-underscore"
                                          (format nil "Symbol ~A contains underscore; prefer hyphens" name))
                             issues))))
                  ((defconstant) ; lint:suppress constant-name-style
                   (let ((name (second form)))
                     (unless (plus-delimited-name-p name)
                       (push (%make-issue path ln col "constant-name-style"
                                          (format nil "defconstant ~A should be named like +constant+" name))
                             issues))
                     (when (symbol-name-has-underscore-p name)
                       (push (%make-issue path ln col "naming-underscore"
                                          (format nil "Symbol ~A contains underscore; prefer hyphens" name))
                             issues))))
                  ((defun defmacro)
                   (when (and (>= (length form) 3)
                              (symbolp (second form))
                              (listp (third form)))
                     (let ((name (second form)))
                       (when (symbol-name-has-underscore-p name)
                         (push (%make-issue path ln col "naming-underscore"
                                            (format nil "Symbol ~A contains underscore; prefer hyphens" name))
                               issues)))))
                  ((defclass)
                   (when (and (>= (length form) 2)
                              (symbolp (second form)))
                     (let ((name (second form)))
                       (when (symbol-name-has-underscore-p name)
                         (push (%make-issue path ln col "naming-underscore"
                                            (format nil "Symbol ~A contains underscore; prefer hyphens" name))
                               issues)))))
                  ((defpackage)
                   (let ((opts (cddr form)))
                     (loop for (k . rest-opts) on opts by #'cddr do
                          (when (eql k :use)
                            (push (%make-issue path ln col "defpackage-use"
                                               ":use present; prefer :import-from and/or package-local nicknames")
                                  issues))
                          (when (eql k :export)
                            (let ((v (first rest-opts)))
                              (dolist (sym (cond
                                             ((and (consp v) (eq (first v) 'list)) (rest v))
                                             ((consp v) v)
                                             (t nil)))
                                (when (symbol-name-has-underscore-p sym)
                                  (push (%make-issue path ln col "naming-underscore"
                                                     (format nil "Exported symbol ~A contains underscore; prefer hyphens" sym))
                                        issues))))))))
                  (otherwise nil))))))))
    (nreverse issues)))


;;; Rule: Lambda-list validation via Ecclesia
(defun rule-lambda-list-ecclesia (path ctx)
  "Validate lambda lists using Ecclesia library.
CTX is a lint-context."
  (let ((issues nil))
    ;; Find all defun/defmacro forms (skip quoted contexts)
    (lint-walk ctx
      (lambda (z)
        (when (and (zip-list-p z)
                   (not (zip-in-quote-p z))
                   (member (zip-head z) '(defun defmacro)))
          (let ((form (zip-form z)))
            (when (and form
                       (>= (length form) 3)
                       (symbolp (second form))
                       (listp (third form)))
              (let* ((pos (zip-pos z))
                     (ln (car pos))
                     (col (cdr pos))
                     (lambda-list (third form)))
                (handler-case
                    (progn
                      (if (eq (first form) 'defmacro)
                          (canonicalize-macro-lambda-list lambda-list)
                          (canonicalize-ordinary-lambda-list lambda-list))
                      nil)
                  (condition (c)
                    (push (%make-issue path ln col "lambda-list-invalid"
                                       (princ-to-string c))
                          issues)))))))))
    (nreverse issues)))


;;; Rule: Unused function/macro parameters (heuristic)
(defun lambda-list-vars (lambda-list)
  "Extract all variable names from a lambda list."
  (let ((vars nil))
    (labels ((add (v)
               (when (symbolp v) (push v vars)))
             (add-opt (spec)
               (cond
                 ((symbolp spec) (add spec))
                 ((consp spec) (add (first spec)))
                 (t nil)))
             (add-key (spec)
               (cond
                 ((symbolp spec) (add spec))
                 ((consp spec)
                  (let ((var (or (second spec) (first spec))))
                    (add var)))
                 (t nil))))
      (let ((mode :req))
        (dolist (item lambda-list)
          (if (member item '(&optional &rest &key &aux &allow-other-keys &body &whole &environment))
              (setf mode item)
              (switch (mode :test #'eq)
                (&optional (add-opt item))
                (&key (add-key item))
                (&rest
                 (when (symbolp item) (add item))
                 (setf mode :after-rest))
                (otherwise (add item)))))))
    (remove-duplicates (nreverse vars))))

(defun symbol-used-p (sym form)
  "Check if symbol SYM is used anywhere within FORM."
  (cond
    ((eq sym form) t)
    ((consp form)
     (or (symbol-used-p sym (first form))
         (if (listp (rest form))
             (some (lambda (x) (symbol-used-p sym x)) (rest form))
             (symbol-used-p sym (rest form)))))
    (t nil)))

(defun rule-unused-parameters (path ctx)
  "Check for function parameters that are defined but never used.
CTX is a lint-context."
  (let ((issues nil))
    (lint-walk ctx
      (lambda (z)
        (when (and (zip-list-p z)
                   (not (zip-in-quote-p z))
                   (member (zip-head z) '(defun defmacro)))
          (let ((form (zip-form z)))
            (when (and form
                       (>= (length form) 3)
                       (symbolp (second form))
                       (listp (third form)))
              (let* ((pos (zip-pos z))
                     (ln (car pos))
                     (col (cdr pos))
                     (lambda-list (third form))
                     (body (cdddr form)))
                (multiple-value-bind (decls doc forms-left)
                    (separate-function-body body)
                  (declare (ignore doc))
                  (let* ((ignored
                           (loop for decl in decls
                                 when (and (consp decl) (eq (first decl) 'declare))
                                   append (loop for spec in (rest decl)
                                               when (and (consp spec) (member (first spec) '(ignore ignorable)))
                                                 append (rest spec))))
                         (vars (if (eq (first form) 'defun)
                                   (ignore-errors (extract-lambda-list-variables lambda-list))
                                   (lambda-list-vars lambda-list))))
                    (dolist (v vars)
                      (unless (or (member v ignored)
                                  (some (lambda (f) (symbol-used-p v f)) forms-left))
                        (push (%make-issue path ln col "unused-parameter"
                                           (format nil
                                                   "Parameter ~A appears unused; declare IGNORE/IGNORABLE if intentional"
                                                   v))
                              issues)))))))))))
    (nreverse issues)))


;;; Rule: Let validation
(defun rule-let-validation (path ctx)
  "Check for malformed let and let* binding structures.
CTX is a lint-context."
  (when *verbose* (logf "; let-validation: checking ~A~%" path))
  (let ((issues nil))
    (labels ((effective-symbol-p (form)
               "Check if FORM is effectively a symbol, including unquoted symbols."
               (or (symbolp form)
                   (and (consp form)
                        (eq (first form) 'unquote)
                        (= (length form) 2)
                        (symbolp (second form)))))
             (check-let-form (form ln col)
               (when (and (consp form)
                          (member (first form) '(let let*))
                          (>= (length form) 2)
                          (listp (second form)))
                 (when *verbose* (logf "; let-validation: found ~A form~%" (first form)))
                 (let ((operator (first form))
                       (bindings (second form))
                       (body (cddr form)))
                   ;; Check for missing binding list
                   (when (null (rest form))
                     (push (%make-issue path ln col "malformed-let"
                                       (format nil "~A form missing binding list" operator))
                           issues))
                   ;; Check for non-list binding list
                   (when (and (rest form) (not (listp bindings)))
                     (push (%make-issue path ln col "malformed-let"
                                        (format nil "~A binding list must be a list, got ~A"
                                                operator (type-of bindings)))
                           issues))
                   ;; Check for missing body
                   (when (and (rest form) (cddr form) (null body))
                     (push (%make-issue path ln col "malformed-let"
                                       (format nil "~A form missing body expressions" operator))
                           issues))
                   ;; Check individual bindings
                   (when (and (listp bindings) bindings)
                     (dolist (binding bindings)
                       (cond
                         ((null binding)
                          (push (%make-issue path ln col "malformed-let"
                                            (format nil "~A has empty binding form ()" operator))
                                issues))
                         ((effective-symbol-p binding) nil)
                         ((consp binding)
                          (cond
                            ((and (= (length binding) 1) (effective-symbol-p (first binding))) nil)
                            ((= (length binding) 2)
                             (unless (effective-symbol-p (first binding))
                               (push (%make-issue path ln col "malformed-let"
                                                 (format nil "~A binding variable must be a symbol, got ~A"
                                                         operator (type-of (first binding))))
                                     issues)))
                            ((> (length binding) 2)
                             (push (%make-issue path ln col "malformed-let"
                                               (format nil "~A binding has too many elements: ~A" operator binding))
                                   issues))
                            (t
                             (push (%make-issue path ln col "malformed-let"
                                               (format nil "~A binding must be proper list, got ~A" operator binding))
                                   issues))))
                         (t
                          (push (%make-issue path ln col "malformed-let"
                                            (format nil "~A binding must be symbol or list, got ~A"
                                                    operator (type-of binding)))
                                issues)))))))))
      ;; Walk all forms looking for let/let* (skip quoted contexts)
      (lint-walk ctx
        (lambda (z)
          (when (and (zip-list-p z)
                     (not (zip-in-quote-p z)))
            (let ((form (zip-form z)))
              (when form
                (let* ((pos (zip-pos z))
                       (ln (car pos))
                       (col (cdr pos)))
                  (check-let-form form ln col))))))))
    (nreverse issues)))


;;; Rule: Unused local functions in flet/labels
(defun ignored-function-p (func-name)
  "Check if function name should be ignored (starts with underscore)."
  (and (symbolp func-name)
       (let ((name (symbol-name func-name)))
         (and (> (length name) 0)
              (char= (char name 0) #\_)))))

(defun extract-ignored-functions (body)
  "Extract function names from (declare (ignore ...)) forms."
  (let ((ignored '()))
    (dolist (form body)
      (when (and (consp form) (eq (first form) 'declare))
        (dolist (decl-spec (rest form))
          (when (and (consp decl-spec)
                     (member (first decl-spec) '(ignore ignorable)))
            (dolist (var (rest decl-spec))
              (when (symbolp var)
                (push var ignored)))))))
    ignored))

(defun function-referenced-p (func-name body)
  "Check if FUNC-NAME is referenced as a function in BODY."
  (labels ((check-form (form)
             (cond
               ((null form) nil)
               ((and (consp form)
                     (symbolp (first form))
                     (eq (first form) func-name))
                t)
               ((and (consp form)
                     (member (first form) '(function))
                     (consp (rest form))
                     (symbolp (second form))
                     (eq (second form) func-name))
                t)
               ((and (consp form)
                     (member (first form) '(funcall apply))
                     (consp (rest form))
                     (consp (second form))
                     (eq (first (second form)) 'function)
                     (consp (rest (second form)))
                     (symbolp (second (second form)))
                     (eq (second (second form)) func-name))
                t)
               ((and (consp form)
                     (member (first form) '(flet labels))
                     (consp (rest form))
                     (listp (second form)))
                (let ((bindings (second form))
                      (nested-body (cddr form)))
                  (if (some (lambda (binding)
                              (and (consp binding)
                                   (symbolp (first binding))
                                   (eq (first binding) func-name)))
                            bindings)
                      nil
                      (some #'check-form nested-body))))
               ((consp form)
                (or (check-form (first form))
                    (check-form (rest form))))
               (t nil))))
    (some #'check-form body)))

(defun rule-unused-local-functions (path ctx)
  "Check for local functions in flet/labels that are never used.
CTX is a lint-context."
  (when *verbose* (logf "; unused-local-functions: checking ~A~%" path))
  (let ((issues nil))
    (lint-walk ctx
      (lambda (z)
        (when (and (zip-list-p z)
                   (not (zip-in-quote-p z))
                   (member (zip-head z) '(flet labels)))
          (let ((form (zip-form z)))
            (when (and form
                       (consp (rest form))
                       (listp (second form))
                       (>= (length form) 2))
              (let* ((pos (zip-pos z))
                     (ln (car pos))
                     (col (cdr pos))
                     (bindings (second form))
                     (body (cddr form))
                     (ignored-funcs (extract-ignored-functions body)))
                (when *verbose*
                  (logf "; unused-local-functions: found ~A with ~D bindings~%"
                        (first form) (length bindings)))
                (dolist (binding bindings)
                  (when (and (consp binding)
                             (symbolp (first binding))
                             (>= (length binding) 3))
                    (let* ((func-name (first binding))
                           (is-labels (eq (first form) 'labels))
                           (search-bodies (if is-labels
                                              (append
                                               (loop for b in bindings
                                                     when (and (consp b)
                                                               (not (eq b binding))
                                                               (>= (length b) 3))
                                                     append (cddr b))
                                               body)
                                              body)))
                      (unless (or (member func-name ignored-funcs)
                                  (ignored-function-p func-name)
                                  (function-referenced-p func-name search-bodies))
                        (when *verbose*
                          (logf "; unused-local-functions: ~A ~A is unused~%"
                                (first form) func-name))
                        (push (%make-issue path ln col "unused-local-function"
                                          (format nil "Local function ~A is unused (declare IGNORE if intentional)"
                                                  func-name))
                              issues)))))))))))
    (nreverse issues)))


;;; Rule: Whitespace around parentheses (Google Lisp style)
;;; Uses AST to avoid false positives in comments and strings

(defun rule-whitespace-around-parens-ast (path ctx)
  "Check for whitespace immediately inside parentheses using AST.
CTX is a lint-context.
This avoids false positives from parens in comments or strings."
  (let ((issues nil))
    (lint-walk ctx
      (lambda (z)
        (when (zip-list-p z)
          (let* ((pos (zip-pos z))
                 (ln (car pos))
                 (col (cdr pos)))
            ;; Check first child for whitespace after open paren
            (when-let ((first-child (rewrite-cl:zip-down z)))
              (let ((tag (rewrite-cl:zip-tag first-child)))
                ;; :whitespace is spaces/tabs (bad), :newline is OK
                (when (eq tag :whitespace)
                  (push (%make-issue path ln (1+ col) "whitespace-after-open-paren"
                                     "Remove whitespace after opening parenthesis")
                        issues))))
            ;; Check last child for whitespace before close paren
            (when-let ((first-child (rewrite-cl:zip-down z)))
              (let* ((last-child (rewrite-cl:zip-rightmost first-child))
                     (tag (rewrite-cl:zip-tag last-child)))
                ;; :whitespace is spaces/tabs (bad), :newline is OK
                (when (eq tag :whitespace)
                  (push (%make-issue path ln col "whitespace-before-close-paren"
                                     "Remove whitespace before closing parenthesis")
                        issues))))))))
    (nreverse issues)))


;;; Rule: Check for in-package or defpackage (AST-based)
;;; Avoids false positives from commented-out package declarations

(defun rule-in-package-present-ast (path ctx)
  "Check that files contain either (in-package ...) or (defpackage ...) forms.
CTX is a lint-context.
Uses AST to avoid false positives from package forms in comments."
  (let ((type (string-downcase (or (pathname-type path) ""))))
    (if (string= type "asd")
        nil  ; Skip .asd files
        (let ((found nil))
          ;; Walk looking for in-package or defpackage forms
          (lint-walk ctx
            (lambda (z)
              (when (and (zip-list-p z)
                         (not (zip-in-quote-p z))
                         (member (zip-head z) '(in-package defpackage cl:in-package cl:defpackage
                                                common-lisp:in-package common-lisp:defpackage)))
                (setf found t))))
          (unless found
            (list (%make-issue path 1 1 "in-package"
                               "No package declaration found (file should contain in-package or defpackage)")))))))


;;; Rule: Consecutive closing parens should be on same line (AST-based)
;;; Avoids false positives from ) in comments or strings
;;;
;;; The rule: if a list's last non-whitespace child is itself a list,
;;; and there's a newline between that child and the parent's closing paren,
;;; then consecutive closing parens are improperly split across lines.

(defun has-split-closing-parens-p (z)
  "Check if list node Z has consecutive closing parens split across lines.
This happens when:
  1. The last non-whitespace child is a list (ends with ))
  2. There's a newline between that child and our closing paren
  3. There are NO comments between the child and closing paren"
  (when-let ((last-meaningful (rewrite-cl.zip:zip-last-child z)))
    ;; Only check if last child is also a list (so we'd have consecutive parens)
    (when (zip-list-p last-meaningful)
      ;; Check what's between this child and the closing paren
      ;; Return T if there's a newline but no comments
      (let ((has-newline nil))
        (loop for sibling = (rewrite-cl:zip-right last-meaningful)
                       then (rewrite-cl:zip-right sibling)
              while sibling
              for tag = (rewrite-cl:zip-tag sibling)
              do (cond
                   ((eql tag :comment)
                    ;; Comment found - don't flag this as an issue
                    (return-from has-split-closing-parens-p nil))
                   ((eql tag :newline)
                    (setf has-newline t))))
        has-newline))))

(defun rule-consecutive-closing-parens-ast (path ctx)
  "Check that consecutive closing parentheses are on the same line.
CTX is a lint-context.
Uses AST to avoid false positives from ) in comments or strings."
  (let ((issues nil))
    (lint-walk ctx
      (lambda (z)
        ;; Skip the root node (synthetic wrapper for multi-form files)
        (when (and (zip-list-p z)
                   (rewrite-cl:zip-up z)  ; Has a parent (not root)
                   (has-split-closing-parens-p z))
          ;; This list has consecutive closing parens split across lines
          (let* ((pos (zip-pos z))
                 (ln (car pos))
                 (col (cdr pos)))
            (push (%make-issue path ln col "closing-parens-same-line"
                               "Consecutive closing parentheses should be on same line")
                  issues)))))
    (nreverse issues)))
