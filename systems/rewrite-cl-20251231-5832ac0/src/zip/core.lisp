;;;; core.lisp - Zipper data structure
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.zip)

;;; The zipper structure provides functional navigation and editing
;;; of the node tree. All operations return new zippers; the original
;;; is never modified.

(defstruct (zipper (:conc-name %zip-)
                   (:constructor %make-zipper)
                   (:copier nil))
  "Functional cursor into a node tree."
  ;; Current node
  (node nil :read-only t)
  ;; Parent zipper (nil at root)
  (parent nil :type (or null zipper) :read-only t)
  ;; Left siblings in reverse order (nearest first)
  (left nil :type list :read-only t)
  ;; Right siblings (nearest first)
  (right nil :type list :read-only t)
  ;; Has the tree been modified?
  (changed-p nil :type boolean :read-only t)
  ;; Track position information?
  (track-position-p nil :type boolean :read-only t))

;; Public accessor for node (others are internal)
(defun zip-node (zipper)
  "Get current node from zipper."
  (%zip-node zipper))

(defun make-zipper (node &key track-position parent left right changed)
  "Create a zipper at NODE."
  (%make-zipper :node node
                :parent parent
                :left left
                :right right
                :changed-p changed
                :track-position-p track-position))

;;; Creation from various sources

(defun of-node (node &key track-position)
  "Create a zipper at the root of NODE."
  (make-zipper node :track-position track-position))

(defun of-string (string &key track-position)
  "Parse STRING and return a zipper at the first form.
If there are multiple top-level forms, wraps them in a synthetic list."
  (let ((nodes (rewrite-cl.parser:parse-string-all string)))
    (cond
      ((null nodes)
       nil)
      ((null (rest nodes))
       (make-zipper (first nodes) :track-position track-position))
      (t
       ;; Multiple top-level forms - wrap in a list node
       ;; This is a synthetic wrapper for navigation purposes
       (make-zipper (make-list-node nodes)
                    :track-position track-position)))))

(defun of-file (pathname &key track-position)
  "Parse file at PATHNAME and return a zipper."
  (let ((nodes (rewrite-cl.parser:parse-file-all pathname)))
    (cond
      ((null nodes)
       nil)
      ((null (rest nodes))
       (make-zipper (first nodes) :track-position track-position))
      (t
       (make-zipper (make-list-node nodes)
                    :track-position track-position)))))

;;; Accessors

(defun zip-string (zipper)
  "Return string representation of current node."
  (when zipper
    (node-string (zip-node zipper))))

(defun zip-sexpr (zipper)
  "Return sexpr of current node."
  (when zipper
    (node-sexpr (zip-node zipper))))

(defun zip-tag (zipper)
  "Return tag of current node."
  (when zipper
    (node-tag (zip-node zipper))))

(defun zip-position (zipper)
  "Return position of current node."
  (when zipper
    (node-position (zip-node zipper))))

(defun zip-children (zipper)
  "Return children of current node."
  (when zipper
    (node-children (zip-node zipper))))

;;; Root access

(defun zip-root (zipper)
  "Navigate to root and return the zipper there."
  (if (%zip-parent zipper)
      (zip-root (zip-up zipper))
      zipper))

(defun zip-root-node (zipper)
  "Return the root node of the tree."
  (zip-node (zip-root zipper)))

(defun zip-root-string (zipper)
  "Return string representation of entire tree."
  (node-string (zip-root-node zipper)))
