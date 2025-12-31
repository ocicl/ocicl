;;;; quote.lisp - Quote-like syntax nodes
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.node)

(defstruct (quote-node (:constructor %make-quote-node))
  "Represents quote-like syntax: ' ` , ,@
TAG identifies the type.
PREFIX is the source prefix string.
CHILD is the quoted form."
  (tag :quote :type keyword :read-only t)
  (prefix "'" :type string :read-only t)
  (child nil :read-only t)
  (position nil :type (or null source-position) :read-only t))

(defun make-quote-node (child &optional position)
  "Create a quote node: 'form"
  (%make-quote-node :tag :quote
                    :prefix "'"
                    :child child
                    :position position))

(defun make-syntax-quote-node (child &optional position)
  "Create a backquote node: `form"
  (%make-quote-node :tag :syntax-quote
                    :prefix "`"
                    :child child
                    :position position))

(defun make-unquote-node (child &optional position)
  "Create an unquote node: ,form"
  (%make-quote-node :tag :unquote
                    :prefix ","
                    :child child
                    :position position))

(defun make-unquote-splicing-node (child &optional position)
  "Create an unquote-splicing node: ,@form"
  (%make-quote-node :tag :unquote-splicing
                    :prefix ",@"
                    :child child
                    :position position))

(defmethod node-tag ((node quote-node))
  (quote-node-tag node))

(defmethod node-string ((node quote-node))
  (concatenate 'string
               (quote-node-prefix node)
               (node-string (quote-node-child node))))

(defmethod node-inner-p ((node quote-node))
  t)

(defmethod node-children ((node quote-node))
  (list (quote-node-child node)))

(defmethod node-replace-children ((node quote-node) children)
  (unless (= 1 (length children))
    (error "Quote node must have exactly one child"))
  (%make-quote-node :tag (quote-node-tag node)
                    :prefix (quote-node-prefix node)
                    :child (first children)
                    :position (quote-node-position node)))

(defmethod node-sexpr ((node quote-node))
  (let ((child-sexpr (node-sexpr (quote-node-child node))))
    (ecase (quote-node-tag node)
      (:quote `(quote ,child-sexpr))
      ;; For backquote forms, we return a list representation
      ;; since quasiquote isn't a standard CL form
      (:syntax-quote (list 'backquote child-sexpr))
      (:unquote (list 'unquote child-sexpr))
      (:unquote-splicing (list 'unquote-splicing child-sexpr)))))

(defmethod node-position ((node quote-node))
  (quote-node-position node))

(defmethod node-set-position ((node quote-node) position)
  (%make-quote-node :tag (quote-node-tag node)
                    :prefix (quote-node-prefix node)
                    :child (quote-node-child node)
                    :position position))
