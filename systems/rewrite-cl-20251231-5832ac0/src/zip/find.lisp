;;;; find.lisp - Find operations for zipper
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.zip)

;;; Generic find

(defun zip-find (zipper predicate &key (direction :next) (include-current nil))
  "Find next node matching PREDICATE.
DIRECTION is :next (forward) or :prev (backward).
If INCLUDE-CURRENT is true, tests current node first."
  (let ((move-fn (ecase direction
                   (:next #'zip-next)
                   (:prev #'zip-prev))))
    (if (and include-current (funcall predicate zipper))
        zipper
        (loop for z = (funcall move-fn zipper) then (funcall move-fn z)
              while z
              when (funcall predicate z)
                return z))))

;;; Find by tag

(defun zip-find-tag (zipper tag &key (direction :next))
  "Find next node with TAG."
  (zip-find zipper
            (lambda (z) (eq tag (zip-tag z)))
            :direction direction))

;;; Find by value

(defun zip-find-value (zipper value &key (direction :next) (test #'eql))
  "Find next node with sexpr equal to VALUE."
  (zip-find zipper
            (lambda (z)
              (and (node-sexpr-able-p (zip-node z))
                   (funcall test value (node-sexpr (zip-node z)))))
            :direction direction))

;;; Find by token string

(defun zip-find-token (zipper token-string &key (direction :next) (test #'string=))
  "Find next token with given string representation."
  (zip-find zipper
            (lambda (z)
              (and (member (zip-tag z) '(:symbol :keyword :number :token :integer :float :ratio))
                   (funcall test token-string (zip-string z))))
            :direction direction))

;;; Find depth-first

(defun zip-find-depth-first (zipper predicate)
  "Find first node matching PREDICATE in depth-first order from current position."
  (zip-find zipper predicate :direction :next :include-current t))

;;; Find all

(defun zip-find-all (zipper predicate)
  "Return list of all zippers at nodes matching PREDICATE."
  (loop for z = (zip-find zipper predicate :direction :next)
          then (zip-find z predicate :direction :next)
        while z
        collect z))

;;; Find by position

(defun zip-find-by-position (zipper target-line target-column)
  "Find deepest node containing the given position."
  (labels ((contains-pos-p (node)
             (let ((pos (node-position node)))
               (when pos
                 (and (<= (pos-line pos) target-line)
                      (<= (pos-column pos) target-column)
                      (or (null (pos-end-line pos))
                          (< target-line (pos-end-line pos))
                          (and (= target-line (pos-end-line pos))
                               (<= target-column (pos-end-column pos))))))))
           (find-deepest (z)
             (if (contains-pos-p (zip-node z))
                 (let ((child-result
                         (when (node-inner-p (zip-node z))
                           (loop for cz = (zip-down z) then (zip-right cz)
                                 while cz
                                 for result = (find-deepest cz)
                                 when result return result))))
                   (or child-result z))
                 nil)))
    (find-deepest zipper)))

;;; Specialized finders

(defun zip-find-next-token (zipper)
  "Find next token node (symbol, keyword, or number)."
  (zip-find zipper
            (lambda (z)
              (member (zip-tag z) '(:symbol :keyword :number :integer :float :ratio :token)))))

(defun zip-find-next-list (zipper)
  "Find next list node."
  (zip-find-tag zipper :list))

(defun zip-find-next-string (zipper)
  "Find next string node."
  (zip-find-tag zipper :string))

;;; Find within

(defun zip-find-in-children (zipper predicate)
  "Find first child matching PREDICATE."
  (loop for z = (zip-down zipper) then (zip-right z)
        while z
        when (funcall predicate z)
          return z))

(defun zip-find-child-value (zipper value &key (test #'eql))
  "Find child with sexpr equal to VALUE."
  (zip-find-in-children zipper
                        (lambda (z)
                          (and (node-sexpr-able-p (zip-node z))
                               (funcall test value (node-sexpr (zip-node z)))))))
