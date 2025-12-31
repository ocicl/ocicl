;;;; character.lisp - Character literal nodes
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.node)

(defstruct (character-node (:constructor %make-character-node))
  "Represents a character literal #\\x.
VALUE is the character.
SOURCE-TEXT is the original source text (e.g., \"#\\Space\")."
  (value #\Space :type character :read-only t)
  (source-text "" :type string :read-only t)
  (position nil :type (or null source-position) :read-only t))

(defun make-character-node (value &optional source-text position)
  "Create a character node.
VALUE is the character.
SOURCE-TEXT defaults to standard character syntax."
  (%make-character-node
   :value value
   :source-text (or source-text (format nil "#\\~A" (char-name-or-char value)))
   :position position))

(defun char-name-or-char (char)
  "Return the character name if it has one, otherwise the character itself."
  (or (char-name char) char))

(defmethod node-tag ((node character-node))
  :character)

(defmethod node-string ((node character-node))
  (character-node-source-text node))

(defmethod node-sexpr ((node character-node))
  (character-node-value node))

(defmethod node-position ((node character-node))
  (character-node-position node))

(defmethod node-set-position ((node character-node) position)
  (%make-character-node :value (character-node-value node)
                        :source-text (character-node-source-text node)
                        :position position))
