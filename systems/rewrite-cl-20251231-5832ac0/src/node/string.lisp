;;;; string.lisp - String literal nodes
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.node)

(defstruct (string-node (:constructor %make-string-node))
  "Represents a string literal.
VALUE is the actual string value.
SOURCE-TEXT is the original source including quotes and escapes."
  (value "" :type string :read-only t)
  (source-text "" :type string :read-only t)
  (position nil :type (or null source-position) :read-only t))

(defun make-string-node (value &optional source-text position)
  "Create a string node.
VALUE is the string value.
SOURCE-TEXT defaults to the printed representation with escapes."
  (%make-string-node
   :value value
   :source-text (or source-text (prin1-to-string value))
   :position position))

(defmethod node-tag ((node string-node))
  :string)

(defmethod node-string ((node string-node))
  (string-node-source-text node))

(defmethod node-sexpr ((node string-node))
  (string-node-value node))

(defmethod node-position ((node string-node))
  (string-node-position node))

(defmethod node-set-position ((node string-node) position)
  (%make-string-node :value (string-node-value node)
                     :source-text (string-node-source-text node)
                     :position position))
