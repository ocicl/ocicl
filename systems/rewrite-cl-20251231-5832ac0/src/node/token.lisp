;;;; token.lisp - Token nodes (symbols, numbers, keywords)
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.node)

(defstruct (token-node (:constructor %make-token-node))
  "Represents atomic tokens: symbols, keywords, numbers.
VALUE is the Lisp value, STRING-VALUE is the original source text."
  (value nil :read-only t)
  (string-value "" :type string :read-only t)
  (position nil :type (or null source-position) :read-only t))

(defun make-token-node (value &optional string-value position)
  "Create a token node for VALUE.
STRING-VALUE defaults to the printed representation of VALUE."
  (%make-token-node
   :value value
   :string-value (or string-value (token-to-string value))
   :position position))

(defun token-to-string (value)
  "Convert a token value to its string representation."
  (typecase value
    (null "nil")
    (keyword (format nil ":~A" (symbol-name value)))
    (symbol (let ((pkg (symbol-package value)))
              (cond
                ((null pkg)
                 (format nil "#:~A" (symbol-name value)))
                ((eq pkg (find-package :keyword))
                 (format nil ":~A" (symbol-name value)))
                ((eq pkg (find-package :common-lisp))
                 (symbol-name value))
                (t
                 ;; For now, just use the symbol name
                 ;; Full handling would need to consider *package*
                 (symbol-name value)))))
    (otherwise (princ-to-string value))))

(defmethod node-tag ((node token-node))
  (let ((value (token-node-value node)))
    (typecase value
      (keyword :keyword)
      (symbol :symbol)
      (integer :integer)
      (ratio :ratio)
      (float :float)
      (number :number)
      (otherwise :token))))

(defmethod node-string ((node token-node))
  (token-node-string-value node))

(defmethod node-sexpr ((node token-node))
  (token-node-value node))

(defmethod node-position ((node token-node))
  (token-node-position node))

(defmethod node-set-position ((node token-node) position)
  (%make-token-node :value (token-node-value node)
                    :string-value (token-node-string-value node)
                    :position position))
