;;;; style.lisp
;;;;
;;;; Fixers for style-related lint rules
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025 Anthony Green

(in-package #:ocicl.lint)

;;; Fix: use-first-rest (CAR -> FIRST, CDR -> REST)
;;; The issue is reported at the position of the (car ...) or (cdr ...) form

(defun fix-use-first-rest (content issue)
  "Replace CAR with FIRST or CDR with REST at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      ;; Find the form (car x) or (cdr x) at this position
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          ;; Go down to the first child (should be CAR or CDR)
          (when-let ((first-child (rewrite-cl:zip-down target)))
            ;; Skip any whitespace to find the actual symbol
            (loop while (and first-child
                             (member (rewrite-cl:zip-tag first-child) '(:whitespace :newline)))
                  do (setf first-child (rewrite-cl:zip-right first-child)))
            (when first-child
              (let ((sym (rewrite-cl:zip-sexpr first-child)))
                (cond
                  ((eq sym 'car)
                   (zip-root-content-string
                    (rewrite-cl:zip-replace first-child
                                            (rewrite-cl:make-token-node 'first "first"))))
                  ((eq sym 'cdr)
                   (zip-root-content-string
                    (rewrite-cl:zip-replace first-child
                                            (rewrite-cl:make-token-node 'rest "rest")))))))))))))

(register-fixer "use-first-rest" #'fix-use-first-rest)


;;; Fix: use-eql (EQ -> EQL for numbers, characters, keywords)

(defun fix-use-eql (content issue)
  "Replace EQ with EQL at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (when-let ((first-child (rewrite-cl:zip-down target)))
            ;; Skip any whitespace to find the actual symbol
            (loop while (and first-child
                             (member (rewrite-cl:zip-tag first-child) '(:whitespace :newline)))
                  do (setf first-child (rewrite-cl:zip-right first-child)))
            (when first-child
              (let ((sym (rewrite-cl:zip-sexpr first-child)))
                (when (eq sym 'eq)
                  (zip-root-content-string
                   (rewrite-cl:zip-replace first-child
                                           (rewrite-cl:make-token-node 'eql "eql"))))))))))))

(register-fixer "use-eql" #'fix-use-eql)


;;; Fix: plus-one - (+ x 1) or (+ 1 x) -> (1+ x)

(defun fix-plus-one (content issue)
  "Transform (+ x 1) or (+ 1 x) to (1+ x) at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (let ((form (rewrite-cl:zip-sexpr target)))
            (when (and (consp form)
                       (eq (first form) '+)
                       (= (length form) 3))
              ;; Find the non-1 argument
              (let ((var (if (and (numberp (second form)) (eql (second form) 1))
                             (third form)
                             (second form))))
                (zip-root-content-string
                 (rewrite-cl:zip-replace target
                   (rewrite-cl:coerce-to-node `(1+ ,var))))))))))))

(register-fixer "plus-one" #'fix-plus-one)


;;; Fix: minus-one - (- x 1) -> (1- x)

(defun fix-minus-one (content issue)
  "Transform (- x 1) to (1- x) at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (let ((form (rewrite-cl:zip-sexpr target)))
            (when (and (consp form)
                       (eq (first form) '-)
                       (= (length form) 3)
                       (numberp (third form))
                       (eql (third form) 1))
              (zip-root-content-string
               (rewrite-cl:zip-replace target
                 (rewrite-cl:coerce-to-node `(1- ,(second form))))))))))))

(register-fixer "minus-one" #'fix-minus-one)


;;; Fix: use-zerop - (= x 0) or (= 0 x) -> (zerop x)

(defun fix-use-zerop (content issue)
  "Transform (= x 0) or (= 0 x) to (zerop x) at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (let ((form (rewrite-cl:zip-sexpr target)))
            (when (and (consp form)
                       (eq (first form) '=)
                       (= (length form) 3))
              ;; Find the non-0 argument
              (let ((var (cond
                           ((and (numberp (second form)) (zerop (second form)))
                            (third form))
                           ((and (numberp (third form)) (zerop (third form)))
                            (second form))
                           (t nil))))
                (when var
                  (zip-root-content-string
                   (rewrite-cl:zip-replace target
                     (rewrite-cl:coerce-to-node `(zerop ,var)))))))))))))

(register-fixer "use-zerop" #'fix-use-zerop)
