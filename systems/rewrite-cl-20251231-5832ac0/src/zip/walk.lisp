;;;; walk.lisp - Tree walking operations
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.zip)

;;; Prewalk - visit parent before children

(defun zip-prewalk (zipper fn)
  "Walk tree depth-first, applying FN to each node before its children.
FN receives a zipper and should return a (possibly modified) zipper.
Returns zipper at root of transformed tree."
  (labels ((walk (z)
             (when z
               ;; Apply function to current node
               (let ((z2 (funcall fn z)))
                 (if z2
                     ;; Walk children if any
                     (if (and (node-inner-p (zip-node z2))
                              (zip-down z2))
                         (walk-siblings (zip-down z2))
                         z2)
                     z))))
           (walk-siblings (z)
             (if z
                 (let ((z2 (walk z)))
                   (if (zip-right z2)
                       (walk-siblings (zip-right z2))
                       (zip-up z2)))
                 z)))
    (let ((result (walk zipper)))
      (if result
          (zip-root result)
          zipper))))

;;; Postwalk - visit children before parent

(defun zip-postwalk (zipper fn)
  "Walk tree depth-first, applying FN to each node after its children.
FN receives a zipper and should return a (possibly modified) zipper.
Returns zipper at root of transformed tree."
  (labels ((walk (z)
             (when z
               ;; First walk children if any
               (let ((z2 (if (and (node-inner-p (zip-node z))
                                  (zip-down z))
                             (zip-up (walk-siblings (zip-down z)))
                             z)))
                 ;; Then apply function to this node
                 (funcall fn z2))))
           (walk-siblings (z)
             (if z
                 (let ((z2 (walk z)))
                   (if (zip-right z2)
                       (walk-siblings (zip-right z2))
                       z2))
                 z)))
    (let ((result (walk zipper)))
      (if result
          (zip-root result)
          zipper))))

;;; Simple walk for side effects

(defun zip-walk (zipper fn)
  "Visit every node in depth-first order, calling FN for side effects.
FN receives a zipper. This does not modify the tree."
  (loop for z = zipper then (zip-next z)
        while z
        do (funcall fn z))
  zipper)

;;; Conditional walks

(defun zip-prewalk-while (zipper predicate fn)
  "Prewalk while PREDICATE returns true for nodes.
Stops descending into a subtree if PREDICATE returns NIL for its root."
  (labels ((walk (z)
             (if (and z (funcall predicate z))
                 (let ((z2 (funcall fn z)))
                   (if z2
                       (if (and (node-inner-p (zip-node z2))
                                (zip-down z2))
                           (walk-siblings (zip-down z2))
                           z2)
                       z))
                 z))
           (walk-siblings (z)
             (if z
                 (let ((z2 (walk z)))
                   (if (zip-right z2)
                       (walk-siblings (zip-right z2))
                       (zip-up z2)))
                 z)))
    (let ((result (walk zipper)))
      (if result
          (zip-root result)
          zipper))))

;;; Collect during walk

(defun zip-collect (zipper predicate)
  "Collect all nodes matching PREDICATE during depth-first walk.
Returns list of zippers."
  (let ((results nil))
    (zip-walk zipper
              (lambda (z)
                (when (funcall predicate z)
                  (push z results))))
    (nreverse results)))

(defun zip-collect-sexprs (zipper predicate)
  "Collect sexprs of all nodes matching PREDICATE."
  (mapcar #'zip-sexpr
          (remove-if-not (lambda (z) (node-sexpr-able-p (zip-node z)))
                         (zip-collect zipper predicate))))

;;; Transform walks

(defun zip-transform (zipper old-value new-value &key (test #'eql))
  "Replace all occurrences of OLD-VALUE with NEW-VALUE."
  (zip-prewalk zipper
               (lambda (z)
                 (if (and (node-sexpr-able-p (zip-node z))
                          (funcall test old-value (node-sexpr (zip-node z))))
                     (zip-replace z (coerce-to-node new-value))
                     z))))

(defun zip-transform-if (zipper predicate fn)
  "Apply FN to all nodes matching PREDICATE.
FN receives the zipper and should return a new zipper."
  (zip-prewalk zipper
               (lambda (z)
                 (if (funcall predicate z)
                     (funcall fn z)
                     z))))
