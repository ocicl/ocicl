;;;; move.lisp - Zipper navigation operations
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.zip)

;;; Basic navigation

(defun zip-up (zipper)
  "Move to parent node. Returns NIL at root."
  (when (%zip-parent zipper)
    (let* ((parent (%zip-parent zipper))
           ;; Reconstruct children from left, current, right
           (new-children (append (reverse (%zip-left zipper))
                                 (list (zip-node zipper))
                                 (%zip-right zipper))))
      (if (%zip-changed-p zipper)
          ;; If changed, create new parent node with updated children
          (%make-zipper
           :node (node-replace-children (zip-node parent) new-children)
           :parent (%zip-parent parent)
           :left (%zip-left parent)
           :right (%zip-right parent)
           :changed-p t
           :track-position-p (%zip-track-position-p zipper))
          ;; If unchanged, just return parent
          parent))))

(defun zip-down (zipper)
  "Move to first child. Returns NIL if no children."
  (when zipper
    (let ((children (node-children (zip-node zipper))))
      (when children
        (%make-zipper
         :node (first children)
         :parent zipper
         :left nil
         :right (rest children)
         :changed-p nil
         :track-position-p (%zip-track-position-p zipper))))))

(defun zip-left (zipper)
  "Move to left sibling. Returns NIL if none."
  (when (and zipper (%zip-left zipper))
    (%make-zipper
     :node (first (%zip-left zipper))
     :parent (%zip-parent zipper)
     :left (rest (%zip-left zipper))
     :right (cons (zip-node zipper) (%zip-right zipper))
     :changed-p (%zip-changed-p zipper)
     :track-position-p (%zip-track-position-p zipper))))

(defun zip-right (zipper)
  "Move to right sibling. Returns NIL if none."
  (when (and zipper (%zip-right zipper))
    (%make-zipper
     :node (first (%zip-right zipper))
     :parent (%zip-parent zipper)
     :left (cons (zip-node zipper) (%zip-left zipper))
     :right (rest (%zip-right zipper))
     :changed-p (%zip-changed-p zipper)
     :track-position-p (%zip-track-position-p zipper))))

;;; Extended navigation

(defun zip-leftmost (zipper)
  "Move to leftmost sibling."
  (let ((left (zip-left zipper)))
    (if left
        (zip-leftmost left)
        zipper)))

(defun zip-rightmost (zipper)
  "Move to rightmost sibling."
  (let ((right (zip-right zipper)))
    (if right
        (zip-rightmost right)
        zipper)))

(defun zip-next (zipper)
  "Move to next node in depth-first order.
Goes down if possible, then right, then up-and-right."
  (or (zip-down zipper)
      (zip-right zipper)
      (loop for z = (zip-up zipper) then (zip-up z)
            while z
            for right = (zip-right z)
            when right
              return right)))

(defun zip-prev (zipper)
  "Move to previous node in depth-first order."
  (let ((left (zip-left zipper)))
    (if left
        (zip-rightmost-descendant left)
        (zip-up zipper))))

(defun zip-rightmost-descendant (zipper)
  "Move to rightmost leaf descendant."
  (let ((children (node-children (zip-node zipper))))
    (if children
        (zip-rightmost-descendant
         (zip-rightmost (zip-down zipper)))
        zipper)))

(defun zip-end-p (zipper)
  "Return T if at end of depth-first traversal."
  (null (zip-next zipper)))

;;; Positional predicates

(defun zip-root-p (zipper)
  "Return T if at root."
  (null (%zip-parent zipper)))

(defun zip-branch-p (zipper)
  "Return T if current node can have children."
  (node-inner-p (zip-node zipper)))

(defun zip-leaf-p (zipper)
  "Return T if current node has no children."
  (null (node-children (zip-node zipper))))

(defun zip-leftmost-p (zipper)
  "Return T if at leftmost sibling."
  (null (%zip-left zipper)))

(defun zip-rightmost-p (zipper)
  "Return T if at rightmost sibling."
  (null (%zip-right zipper)))
