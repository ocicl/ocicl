;;;; comment.lisp - Comment nodes
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.node)

;;; Line comment node

(defstruct (comment-node (:constructor %make-comment-node))
  "Represents a line comment starting with semicolon.
TEXT includes the semicolon prefix but not the trailing newline."
  (text "" :type string :read-only t)
  (position nil :type (or null source-position) :read-only t))

(defun make-comment-node (text &optional position)
  "Create a line comment node. TEXT should include the ; prefix."
  (%make-comment-node :text text :position position))

(defmethod node-tag ((node comment-node))
  :comment)

(defmethod node-string ((node comment-node))
  (comment-node-text node))

(defmethod node-printable-only-p ((node comment-node))
  t)

(defmethod node-sexpr ((node comment-node))
  (error 'printable-only-error :node node))

(defmethod node-position ((node comment-node))
  (comment-node-position node))

(defmethod node-set-position ((node comment-node) position)
  (%make-comment-node :text (comment-node-text node)
                      :position position))

;;; Block comment node

(defstruct (block-comment-node (:constructor %make-block-comment-node))
  "Represents a block comment #| ... |#.
TEXT includes the delimiters."
  (text "" :type string :read-only t)
  (position nil :type (or null source-position) :read-only t))

(defun make-block-comment-node (text &optional position)
  "Create a block comment node. TEXT should include #| and |# delimiters."
  (%make-block-comment-node :text text :position position))

(defmethod node-tag ((node block-comment-node))
  :block-comment)

(defmethod node-string ((node block-comment-node))
  (block-comment-node-text node))

(defmethod node-printable-only-p ((node block-comment-node))
  t)

(defmethod node-sexpr ((node block-comment-node))
  (error 'printable-only-error :node node))

(defmethod node-position ((node block-comment-node))
  (block-comment-node-position node))

(defmethod node-set-position ((node block-comment-node) position)
  (%make-block-comment-node :text (block-comment-node-text node)
                            :position position))
