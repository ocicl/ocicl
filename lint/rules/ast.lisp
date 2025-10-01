;;;; ast.lisp
;;;;
;;;; AST-based linting rules for semantic analysis
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025 Anthony Green

(in-package #:ocicl.lint)

;; AST helpers


;; Style helpers from The One True Lisp Style Guide
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

;; Rule: Naming conventions and package :use usage
(defun rule-naming-and-packages (path forms)
  "Check naming conventions and package usage patterns."
  (when *verbose* (logf "; naming-and-packages: checking ~D forms in ~A~%" (length forms) path))
  (let ((issues nil))
    (dolist (pair forms)
      (destructuring-bind (form . lc) pair
        (when (consp form)
          (let ((ln (first lc)) (col (rest lc)))
            (case (first form)
              ((defvar defparameter)  ; lint:suppress defvar-without-value
               (let ((name (second form)))
                 (unless (star-delimited-name-p name)
                   (push (%make-issue path ln col "special-name-style"
                                      (format nil "~A ~A should be named like *special-var*" (first form) name))  ; lint:suppress max-line-length special-name-style
                         issues))
                 (when (symbol-name-has-underscore-p name)
                   (push (%make-issue path ln col "naming-underscore"
                                      (format nil "Symbol ~A contains underscore; prefer hyphens" name)) ; lint:suppress
                         issues))))
              ((defconstant)
               (let ((name (second form)))
                 (unless (plus-delimited-name-p name)
                   (push (%make-issue path ln col "constant-name-style"
                                      (format nil "defconstant ~A should be named like +constant+" name))
                         issues))
                 (when (symbol-name-has-underscore-p name)
                   (push (%make-issue path ln col "naming-underscore"
                                      (format nil "Symbol ~A contains underscore; prefer hyphens" name)) ; lint:suppress
                         issues))))
              ((defun defmacro)
               (when (and (>= (length form) 3)           ; has minimum elements
                          (symbolp (second form))           ; name is a symbol
                          (listp (third form)))           ; lambda-list is a list
                 (let ((name (second form)))
                   (when (symbol-name-has-underscore-p name)
                     (push (%make-issue path ln col "naming-underscore"
                                        (format nil "Symbol ~A contains underscore; prefer hyphens" name)) ; lint:suppress
                           issues)))))
              ((defclass)
               (when (and (>= (length form) 2)           ; has minimum elements
                          (symbolp (second form)))          ; name is a symbol
                 (let ((name (second form)))
                   (when (symbol-name-has-underscore-p name)
                     (push (%make-issue path ln col "naming-underscore"
                                        (format nil "Symbol ~A contains underscore; prefer hyphens" name)) ; lint:suppress
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
                                                 (format nil "Exported symbol ~A contains underscore; prefer hyphens"
                                                         sym))
                                    issues))))))))
              (otherwise nil))))))
    (nreverse issues)))

;; Rule: Lambda-list validation via Ecclesia
(defun rule-lambda-list-ecclesia (path forms)
  "Validate lambda lists using Ecclesia library."
  (let ((issues nil))
    (dolist (pair forms)
      (destructuring-bind (form . lc) pair
        (when (and (consp form)
                   (member (first form) '(defun defmacro))
                   (>= (length form) 3)           ; has minimum elements
                   (symbolp (second form))          ; name is a symbol
                   (listp (third form)))          ; lambda-list is a list
          (let* ((ln (first lc)) (col (rest lc))
                 (lambda-list (third form)))
            (handler-case
                (progn
                  (if (eq (first form) 'defmacro)
                      (canonicalize-macro-lambda-list lambda-list)
                      (canonicalize-ordinary-lambda-list lambda-list))
                  nil)
              (condition (c2)
                (push (%make-issue path ln col "lambda-list-invalid"
                                   (princ-to-string c2))
                      issues)))))))
    (nreverse issues)))

;; Rule: unused function/macro parameters (heuristic)
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

(defun rule-unused-parameters (path forms)
  "Check for function parameters that are defined but never used."
  (let ((issues nil))
    (dolist (pair forms)
      (destructuring-bind (form . lc) pair
        (when (and (consp form)
                   (member (first form) '(defun defmacro))
                   (>= (length form) 3)           ; has minimum elements
                   (symbolp (second form))          ; name is a symbol
                   (listp (third form)))          ; lambda-list is a list
          (let* ((ln (first lc)) (col (rest lc))
                 (lambda-list (third form))
                 (body (cdddr form)))
            (multiple-value-bind (decls doc forms-left)
                (separate-function-body body)
              (declare (ignore doc))
              (let* ((ignored
                       ;; Extract ignored parameters from declare forms
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
                          issues)))))))))
    (nreverse issues)))

(defun rule-let-validation (path forms)
  "Check for malformed let and let* binding structures."
  (when *verbose* (logf "; let-validation: checking ~D forms in ~A~%" (length forms) path))
  (let ((issues nil))
    (labels ((effective-symbol-p (form)
               "Check if FORM is effectively a symbol, including unquoted symbols in macros."
               (or (symbolp form)
                   ;; Handle (eclector.reader:unquote symbol) from macro templates
                   (and (consp form)
                        (eq (first form) 'eclector.reader:unquote)
                        (= (length form) 2)
                        (symbolp (second form)))))
             (check-let-form (form ln col)
               (when (and (consp form)
                          (listp form)                   ; Ensure it's a proper list
                          (member (first form) '(let let*)) ; lint:suppress
                          (>= (length form) 2)           ; Must have at least operator and bindings
                          (listp (second form)))         ; Bindings must be a list
                 (when *verbose* (logf "; let-validation: found ~A form with bindings ~S~%" (first form) (second form)))
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
                   ;; Check individual bindings if bindings is a proper list
                   (when (and (listp bindings) bindings)
                     (dolist (binding bindings)
                       (cond
                         ;; Empty binding represented as NIL (from parsing ())
                         ((null binding)
                          (push (%make-issue path ln col "malformed-let"
                                            (format nil "~A has empty binding form ()" operator))
                                issues))
                         ;; Symbol binding (shorthand for (var nil))
                         ((effective-symbol-p binding)
                          ;; This is valid, nothing to check
                          nil)
                         ;; List binding
                         ((consp binding)
                          (cond
                            ;; Empty binding ()
                            ((null binding)
                             (push (%make-issue path ln col "malformed-let"
                                               (format nil "~A has empty binding form ()" operator))
                                   issues))
                            ;; Single element (var) - valid
                            ((and (= (length binding) 1) (effective-symbol-p (first binding)))
                             nil)
                            ;; Two elements (var value) - check variable is symbol
                            ((= (length binding) 2)
                             (unless (effective-symbol-p (first binding))
                               (push (%make-issue path ln col "malformed-let"
                                                 (format nil "~A binding variable must be a symbol, got ~A"
                                                         operator (type-of (first binding))))
                                     issues)))
                            ;; Too many elements
                            ((> (length binding) 2)
                             (push (%make-issue path ln col "malformed-let"
                                               (format nil "~A binding has too many elements: ~A" operator binding))
                                   issues))
                            ;; Improper list
                            (t
                             (push (%make-issue path ln col "malformed-let"
                                               (format nil "~A binding must be proper list, got ~A" operator binding))
                                   issues))))
                         ;; Neither symbol nor list
                         (t
                          (push (%make-issue path ln col "malformed-let"
                                            (format nil "~A binding must be symbol or list, got ~A"
                                                    operator (type-of binding)))
                                issues))))))))
             (walk-form (form ln col)
               (check-let-form form ln col)
               ;; Recursively walk sub-forms
               (when (consp form)
                 (dolist (sub-form (rest form))
                   (walk-form sub-form ln col)))))
      (dolist (pair forms)
        (handler-case
            (destructuring-bind (form . lc) pair
              (when (and (consp lc) (numberp (first lc)) (numberp (rest lc)))
                (let ((ln (first lc)) (col (rest lc)))
                  (walk-form form ln col))))
          (error (e)
            ;; Skip malformed entries but don't propagate error
            (when *verbose*
              (logf "; let-validation: skipping malformed form pair: ~S (error: ~A)~%" pair e))))))
    (nreverse issues)))
