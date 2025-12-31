;;;; whitespace.lisp - Whitespace and newline nodes
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.node)

;;; Whitespace node (spaces and tabs)

(defstruct (whitespace-node (:constructor %make-whitespace-node))
  "Represents horizontal whitespace (spaces and tabs)."
  (text "" :type string :read-only t)
  (position nil :type (or null source-position) :read-only t))

(defun make-whitespace-node (text &optional position)
  "Create a whitespace node with the given TEXT."
  (%make-whitespace-node :text text :position position))

(defmethod node-tag ((node whitespace-node))
  :whitespace)

(defmethod node-string ((node whitespace-node))
  (whitespace-node-text node))

(defmethod node-printable-only-p ((node whitespace-node))
  t)

(defmethod node-sexpr ((node whitespace-node))
  (error 'printable-only-error :node node))

(defmethod node-position ((node whitespace-node))
  (whitespace-node-position node))

(defmethod node-set-position ((node whitespace-node) position)
  (%make-whitespace-node :text (whitespace-node-text node)
                         :position position))

;;; Newline node

(defstruct (newline-node (:constructor %make-newline-node))
  "Represents line breaks."
  (text (string #\Newline) :type string :read-only t)
  (position nil :type (or null source-position) :read-only t))

(defun make-newline-node (&optional (text (string #\Newline)) position)
  "Create a newline node."
  (%make-newline-node :text text :position position))

(defmethod node-tag ((node newline-node))
  :newline)

(defmethod node-string ((node newline-node))
  (newline-node-text node))

(defmethod node-printable-only-p ((node newline-node))
  t)

(defmethod node-sexpr ((node newline-node))
  (error 'printable-only-error :node node))

(defmethod node-position ((node newline-node))
  (newline-node-position node))

(defmethod node-set-position ((node newline-node) position)
  (%make-newline-node :text (newline-node-text node)
                      :position position))

;;; Convenience constructors

(defun spaces (n)
  "Create a whitespace node with N spaces."
  (make-whitespace-node (make-string n :initial-element #\Space)))

(defun newlines (n)
  "Create a newline node with N newlines."
  (make-newline-node (make-string n :initial-element #\Newline)))

;;; Predicates

(defun whitespace-or-comment-p (node)
  "Return T if NODE is whitespace, newline, or comment."
  (member (node-tag node) '(:whitespace :newline :comment :block-comment)))
