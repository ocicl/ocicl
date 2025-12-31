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


;;; Fix: setf-vs-setq - SETQ -> SETF

(defun fix-setf-vs-setq (content issue)
  "Replace SETQ with SETF at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (when-let ((first-child (rewrite-cl:zip-down target)))
            (loop while (and first-child
                             (member (rewrite-cl:zip-tag first-child) '(:whitespace :newline)))
                  do (setf first-child (rewrite-cl:zip-right first-child)))
            (when (and first-child (eq (rewrite-cl:zip-sexpr first-child) 'setq))
              (zip-root-content-string
               (rewrite-cl:zip-replace first-child
                                       (rewrite-cl:make-token-node 'setf "setf"))))))))))

(register-fixer "setf-vs-setq" #'fix-setf-vs-setq)


;;; Fix: quoted-nil - 'NIL -> NIL

(defun fix-quoted-nil (content issue)
  "Replace 'NIL with NIL at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (let ((form (rewrite-cl:zip-sexpr target)))
            (when (and (consp form)
                       (eq (first form) 'quote)
                       (eq (second form) nil))
              (zip-root-content-string
               (rewrite-cl:zip-replace target
                 (rewrite-cl:make-token-node nil "nil"))))))))))

(register-fixer "quoted-nil" #'fix-quoted-nil)


;;; Fix: quote-keyword - ':foo -> :foo

(defun fix-quote-keyword (content issue)
  "Remove quote from keyword at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (let ((form (rewrite-cl:zip-sexpr target)))
            (when (and (consp form)
                       (eq (first form) 'quote)
                       (keywordp (second form)))
              (let ((kw (second form)))
                (zip-root-content-string
                 (rewrite-cl:zip-replace target
                   (rewrite-cl:make-token-node kw (format nil ":~A" (symbol-name kw)))))))))))))

(register-fixer "quote-keyword" #'fix-quote-keyword)


;;; Fix: quote-number - '42 -> 42

(defun fix-quote-number (content issue)
  "Remove quote from number at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (let ((form (rewrite-cl:zip-sexpr target)))
            (when (and (consp form)
                       (eq (first form) 'quote)
                       (numberp (second form)))
              (let ((num (second form)))
                (zip-root-content-string
                 (rewrite-cl:zip-replace target
                   (rewrite-cl:make-token-node num (princ-to-string num))))))))))))

(register-fixer "quote-number" #'fix-quote-number)


;;; Fix: car-cdr - (CAR (CDR x)) or (FIRST (REST x)) -> (CADR x)

(defun fix-car-cdr (content issue)
  "Transform (CAR (CDR x)) or (FIRST (REST x)) to (CADR x) at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (let ((form (rewrite-cl:zip-sexpr target)))
            (when (and (consp form)
                       (member (first form) '(car first))
                       (= (length form) 2)
                       (consp (second form))
                       (member (first (second form)) '(cdr rest))
                       (= (length (second form)) 2))
              (let ((arg (second (second form))))
                (zip-root-content-string
                 (rewrite-cl:zip-replace target
                   (rewrite-cl:coerce-to-node `(cadr ,arg))))))))))))

(register-fixer "car-cdr" #'fix-car-cdr)


;;; Fix: cdr-cdr - (CDR (CDR x)) or (REST (REST x)) -> (CDDR x)

(defun fix-cdr-cdr (content issue)
  "Transform (CDR (CDR x)) or (REST (REST x)) to (CDDR x) at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (let ((form (rewrite-cl:zip-sexpr target)))
            (when (and (consp form)
                       (member (first form) '(cdr rest))
                       (= (length form) 2)
                       (consp (second form))
                       (member (first (second form)) '(cdr rest))
                       (= (length (second form)) 2))
              (let ((arg (second (second form))))
                (zip-root-content-string
                 (rewrite-cl:zip-replace target
                   (rewrite-cl:coerce-to-node `(cddr ,arg))))))))))))

(register-fixer "cdr-cdr" #'fix-cdr-cdr)


;;; Fix: cons-with-nil - (CONS x NIL) -> (LIST x)

(defun fix-cons-with-nil (content issue)
  "Transform (CONS x NIL) to (LIST x) at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (let ((form (rewrite-cl:zip-sexpr target)))
            (when (and (consp form)
                       (eq (first form) 'cons)
                       (= (length form) 3)
                       (null (third form)))
              (let ((arg (second form)))
                (zip-root-content-string
                 (rewrite-cl:zip-replace target
                   (rewrite-cl:coerce-to-node `(list ,arg))))))))))))

(register-fixer "cons-with-nil" #'fix-cons-with-nil)


;;; Fix: not-null - (NOT (NULL x)) -> x

(defun fix-not-null (content issue)
  "Transform (NOT (NULL x)) to x at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (let ((form (rewrite-cl:zip-sexpr target)))
            (when (and (consp form)
                       (eq (first form) 'not)
                       (= (length form) 2)
                       (consp (second form))
                       (eq (first (second form)) 'null)
                       (= (length (second form)) 2))
              (let ((arg (second (second form))))
                (zip-root-content-string
                 (rewrite-cl:zip-replace target
                   (rewrite-cl:coerce-to-node arg)))))))))))

(register-fixer "not-null" #'fix-not-null)


;;; Fix: when-for-unless - (WHEN (NOT x) ...) -> (UNLESS x ...)

(defun fix-when-for-unless (content issue)
  "Transform (WHEN (NOT x) ...) to (UNLESS x ...) at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (let ((form (rewrite-cl:zip-sexpr target)))
            (when (and (consp form)
                       (eq (first form) 'when)
                       (>= (length form) 2)
                       (consp (second form))
                       (eq (first (second form)) 'not)
                       (= (length (second form)) 2))
              (let ((test (second (second form)))
                    (body (cddr form)))
                (zip-root-content-string
                 (rewrite-cl:zip-replace target
                   (rewrite-cl:coerce-to-node `(unless ,test ,@body))))))))))))

(register-fixer "when-for-unless" #'fix-when-for-unless)


;;; Fix: unless-for-when - (UNLESS (NOT x) ...) -> (WHEN x ...)

(defun fix-unless-for-when (content issue)
  "Transform (UNLESS (NOT x) ...) to (WHEN x ...) at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (let ((form (rewrite-cl:zip-sexpr target)))
            (when (and (consp form)
                       (eq (first form) 'unless)
                       (>= (length form) 2)
                       (consp (second form))
                       (eq (first (second form)) 'not)
                       (= (length (second form)) 2))
              (let ((test (second (second form)))
                    (body (cddr form)))
                (zip-root-content-string
                 (rewrite-cl:zip-replace target
                   (rewrite-cl:coerce-to-node `(when ,test ,@body))))))))))))

(register-fixer "unless-for-when" #'fix-unless-for-when)


;;; Fix: needless-and - (AND x) -> x

(defun fix-needless-and (content issue)
  "Transform (AND x) to x at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (let ((form (rewrite-cl:zip-sexpr target)))
            (when (and (consp form)
                       (eq (first form) 'and)
                       (= (length form) 2))
              (zip-root-content-string
               (rewrite-cl:zip-replace target
                 (rewrite-cl:coerce-to-node (second form)))))))))))

(register-fixer "needless-and" #'fix-needless-and)


;;; Fix: needless-or - (OR x) -> x

(defun fix-needless-or (content issue)
  "Transform (OR x) to x at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (let ((form (rewrite-cl:zip-sexpr target)))
            (when (and (consp form)
                       (eq (first form) 'or)
                       (= (length form) 2))
              (zip-root-content-string
               (rewrite-cl:zip-replace target
                 (rewrite-cl:coerce-to-node (second form)))))))))))

(register-fixer "needless-or" #'fix-needless-or)


;;; Fix: rplaca - (RPLACA x y) -> (SETF (CAR x) y)

(defun fix-rplaca (content issue)
  "Transform (RPLACA x y) to (SETF (CAR x) y) at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (let ((form (rewrite-cl:zip-sexpr target)))
            (when (and (consp form)
                       (eq (first form) 'rplaca)
                       (= (length form) 3))
              (let ((place (second form))
                    (value (third form)))
                (zip-root-content-string
                 (rewrite-cl:zip-replace target
                   (rewrite-cl:coerce-to-node `(setf (car ,place) ,value))))))))))))

(register-fixer "rplaca" #'fix-rplaca)


;;; Fix: rplacd - (RPLACD x y) -> (SETF (CDR x) y)

(defun fix-rplacd (content issue)
  "Transform (RPLACD x y) to (SETF (CDR x) y) at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (let ((form (rewrite-cl:zip-sexpr target)))
            (when (and (consp form)
                       (eq (first form) 'rplacd)
                       (= (length form) 3))
              (let ((place (second form))
                    (value (third form)))
                (zip-root-content-string
                 (rewrite-cl:zip-replace target
                   (rewrite-cl:coerce-to-node `(setf (cdr ,place) ,value))))))))))))

(register-fixer "rplacd" #'fix-rplacd)


;;; Fix: setf-incf - (SETF x (+ x n)) -> (INCF x n)

(defun fix-setf-incf (content issue)
  "Transform (SETF x (+ x n)) to (INCF x n) at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (let ((form (rewrite-cl:zip-sexpr target)))
            (when (and (consp form)
                       (member (first form) '(setf setq))
                       (= (length form) 3)
                       (consp (third form))
                       (eq (first (third form)) '+)
                       (>= (length (third form)) 3))
              (let* ((var (second form))
                     (plus-form (third form))
                     (plus-args (rest plus-form)))
                ;; Check if var appears in plus-args
                (when (member var plus-args :test #'equal)
                  (let ((delta (remove var plus-args :test #'equal :count 1)))
                    (when (= (length delta) 1)
                      (let ((amount (first delta)))
                        (zip-root-content-string
                         (rewrite-cl:zip-replace target
                           (if (eql amount 1)
                               (rewrite-cl:coerce-to-node `(incf ,var))
                               (rewrite-cl:coerce-to-node `(incf ,var ,amount)))))))))))))))))

(register-fixer "setf-incf" #'fix-setf-incf)


;;; Fix: setf-decf - (SETF x (- x n)) -> (DECF x n)

(defun fix-setf-decf (content issue)
  "Transform (SETF x (- x n)) to (DECF x n) at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (let ((form (rewrite-cl:zip-sexpr target)))
            (when (and (consp form)
                       (member (first form) '(setf setq))
                       (= (length form) 3)
                       (consp (third form))
                       (eq (first (third form)) '-)
                       (= (length (third form)) 3))
              (let* ((var (second form))
                     (minus-form (third form))
                     (minuend (second minus-form))
                     (subtrahend (third minus-form)))
                ;; Only match (- var n) pattern, not (- n var)
                (when (equal var minuend)
                  (zip-root-content-string
                   (rewrite-cl:zip-replace target
                     (if (eql subtrahend 1)
                         (rewrite-cl:coerce-to-node `(decf ,var))
                         (rewrite-cl:coerce-to-node `(decf ,var ,subtrahend))))))))))))))

(register-fixer "setf-decf" #'fix-setf-decf)


;;; Fix: add-zero - (+ x 0) or (+ 0 x) -> x

(defun fix-add-zero (content issue)
  "Transform (+ x 0) or (+ 0 x) to x at ISSUE location."
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
              (let ((arg (cond
                           ((and (numberp (second form)) (zerop (second form)))
                            (third form))
                           ((and (numberp (third form)) (zerop (third form)))
                            (second form))
                           (t nil))))
                (when arg
                  (zip-root-content-string
                   (rewrite-cl:zip-replace target
                     (rewrite-cl:coerce-to-node arg))))))))))))

(register-fixer "add-zero" #'fix-add-zero)
