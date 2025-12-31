;;;; whitespace.lisp - Whitespace-aware zipper operations
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.zip)

;;; Predicates

(defun zip-whitespace-p (zipper)
  "Return T if current node is whitespace."
  (eql :whitespace (zip-tag zipper)))

(defun zip-newline-p (zipper)
  "Return T if current node is a newline."
  (eql :newline (zip-tag zipper)))

(defun zip-comment-p (zipper)
  "Return T if current node is a comment."
  (member (zip-tag zipper) '(:comment :block-comment)))

(defun zip-whitespace-or-comment-p (zipper)
  "Return T if current node is whitespace, newline, or comment."
  (whitespace-or-comment-p (zip-node zipper)))

;;; Whitespace-skipping navigation

(defun zip-skip-whitespace (zipper &key (direction :right))
  "Skip whitespace/comment nodes in DIRECTION (:left or :right)."
  (let ((move-fn (ecase direction
                   (:right #'zip-right)
                   (:left #'zip-left))))
    (loop for z = zipper then (funcall move-fn z)
          while (and z (zip-whitespace-or-comment-p z))
          finally (return z))))

(defun zip-right* (zipper)
  "Move right, skipping whitespace and comments."
  (when zipper
    (loop for z = (zip-right zipper) then (zip-right z)
          while z
          unless (zip-whitespace-or-comment-p z)
            return z)))

(defun zip-left* (zipper)
  "Move left, skipping whitespace and comments."
  (when zipper
    (loop for z = (zip-left zipper) then (zip-left z)
          while z
          unless (zip-whitespace-or-comment-p z)
            return z)))

(defun zip-down* (zipper)
  "Move down to first non-whitespace child."
  (let ((z (zip-down zipper)))
    (when z
      (if (zip-whitespace-or-comment-p z)
          (zip-right* z)
          z))))

(defun zip-up* (zipper)
  "Move up (same as zip-up, included for symmetry)."
  (zip-up zipper))

(defun zip-next* (zipper)
  "Move to next node in depth-first order, skipping whitespace."
  (loop for z = (zip-next zipper) then (zip-next z)
        while z
        unless (zip-whitespace-or-comment-p z)
          return z))

(defun zip-prev* (zipper)
  "Move to previous node in depth-first order, skipping whitespace."
  (loop for z = (zip-prev zipper) then (zip-prev z)
        while z
        unless (zip-whitespace-or-comment-p z)
          return z))

;;; Whitespace manipulation

(defun zip-ensure-space-left (zipper)
  "Ensure there is whitespace to the left of current node."
  (let ((left (zip-left zipper)))
    (if (or (null left)
            (zip-whitespace-p left)
            (zip-newline-p left))
        zipper
        (zip-insert-left zipper (spaces 1)))))

(defun zip-ensure-space-right (zipper)
  "Ensure there is whitespace to the right of current node."
  (let ((right (zip-right zipper)))
    (if (or (null right)
            (zip-whitespace-p right)
            (zip-newline-p right))
        zipper
        (zip-insert-right zipper (spaces 1)))))

(defun zip-remove-surrounding-whitespace (zipper)
  "Remove whitespace nodes immediately around current node."
  (let ((z zipper))
    ;; Remove whitespace on left
    (loop while (and (zip-left z) (zip-whitespace-p (zip-left z)))
          do (setf z (zip-left z))
             (setf z (zip-remove z)))
    ;; Remove whitespace on right
    (loop while (and (zip-right z) (zip-whitespace-p (zip-right z)))
          do (setf z (zip-right z))
             (setf z (zip-remove z))
             (setf z (zip-left z)))
    z))

(defun zip-normalize-whitespace (zipper)
  "Ensure exactly one space between siblings (for non-whitespace nodes)."
  (if (not (node-inner-p (zip-node zipper)))
      zipper
      (let ((child (zip-down zipper)))
        (if (not child)
            zipper
            (progn
              ;; Skip leading whitespace
              (loop while (and child (zip-whitespace-or-comment-p child))
                    do (setf child (zip-right child)))
              ;; If all children were whitespace/comments, return original zipper
              (if (not child)
                  zipper
                  (progn
                    ;; Process remaining children
                    (loop while (zip-right child)
                          do (let ((next (zip-right child)))
                               (cond
                                 ;; Both non-whitespace: insert space
                                 ((and (not (zip-whitespace-or-comment-p child))
                                       (not (zip-whitespace-or-comment-p next)))
                                  (setf child (zip-insert-right child (spaces 1)))
                                  (setf child (zip-right (zip-right child))))
                                 ;; Multiple spaces: collapse to one
                                 ((and (zip-whitespace-p child)
                                       (zip-whitespace-p next))
                                  (setf child (zip-remove next)))
                                 (t
                                  (setf child (zip-right child))))))
                    ;; Return zipper at parent using the modified child
                    (zip-up (zip-leftmost child)))))))))
