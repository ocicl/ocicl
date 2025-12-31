;;;; seq.lisp - Sequence container nodes (lists, vectors)
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.node)

(defstruct (seq-node (:constructor %make-seq-node))
  "Container node for lists and vectors.
TAG identifies the type (:list, :vector).
OPEN and CLOSE are the delimiter strings.
CHILDREN is the list of child nodes (including whitespace)."
  (tag :list :type keyword :read-only t)
  (open "(" :type string :read-only t)
  (close ")" :type string :read-only t)
  (children nil :type list :read-only t)
  (position nil :type (or null source-position) :read-only t))

(defun make-list-node (children &optional position)
  "Create a list node with the given CHILDREN."
  (%make-seq-node :tag :list
                  :open "("
                  :close ")"
                  :children children
                  :position position))

(defun make-vector-node (children &optional position)
  "Create a vector node with the given CHILDREN."
  (%make-seq-node :tag :vector
                  :open "#("
                  :close ")"
                  :children children
                  :position position))

(defmethod node-tag ((node seq-node))
  (seq-node-tag node))

(defmethod node-string ((node seq-node))
  (concatenate 'string
               (seq-node-open node)
               (nodes-string (seq-node-children node))
               (seq-node-close node)))

(defmethod node-inner-p ((node seq-node))
  t)

(defmethod node-children ((node seq-node))
  (seq-node-children node))

(defmethod node-replace-children ((node seq-node) children)
  (%make-seq-node :tag (seq-node-tag node)
                  :open (seq-node-open node)
                  :close (seq-node-close node)
                  :children children
                  :position (seq-node-position node)))

(defmethod node-sexpr ((node seq-node))
  (let ((child-values (child-sexprs node)))
    (ecase (seq-node-tag node)
      (:list child-values)
      (:vector (coerce child-values 'vector)))))

(defmethod node-position ((node seq-node))
  (seq-node-position node))

(defmethod node-set-position ((node seq-node) position)
  (%make-seq-node :tag (seq-node-tag node)
                  :open (seq-node-open node)
                  :close (seq-node-close node)
                  :children (seq-node-children node)
                  :position position))
