;;;; protocols.lisp - Generic functions defining the node protocol
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.node)

;;; Core node protocol

(defgeneric node-tag (node)
  (:documentation
   "Return a keyword identifying the type of NODE.
Common tags include: :whitespace, :newline, :comment, :token,
:symbol, :keyword, :number, :string, :character, :list, :vector,
:quote, :syntax-quote, :unquote, :unquote-splicing, :function, etc."))

(defgeneric node-string (node)
  (:documentation
   "Return the source code string representation of NODE.
This should produce exactly the text that was parsed (for parsed nodes)
or valid source text (for constructed nodes). Concatenating node-string
of all nodes should reproduce the original source."))

(defgeneric node-sexpr (node)
  (:documentation
   "Convert NODE to its Lisp form (S-expression).
Signals PRINTABLE-ONLY-ERROR for nodes that cannot be converted
to an S-expression (whitespace, comments, etc.)."))

(defgeneric node-sexpr-able-p (node)
  (:documentation
   "Return T if NODE can be converted to an S-expression.
Returns NIL for whitespace, comments, and similar nodes."))

(defgeneric node-printable-only-p (node)
  (:documentation
   "Return T if NODE is printable-only (whitespace, comment, etc.).
Such nodes are preserved for formatting but have no semantic value."))

(defgeneric node-length (node)
  (:documentation
   "Return the character length of NODE's string representation."))

(defgeneric node-children (node)
  (:documentation
   "Return a list of child nodes, or NIL for leaf nodes."))

(defgeneric node-inner-p (node)
  (:documentation
   "Return T if NODE can have children (is a container node)."))

(defgeneric node-replace-children (node children)
  (:documentation
   "Return a new node with NODE's properties but with new CHILDREN.
The original NODE is not modified."))

(defgeneric node-position (node)
  (:documentation
   "Return the SOURCE-POSITION of NODE, or NIL if not tracked."))

(defgeneric node-set-position (node position)
  (:documentation
   "Return a new node with the given POSITION.
The original NODE is not modified."))

;;; Default implementations

(defmethod node-sexpr-able-p ((node t))
  "Default: nodes are sexpr-able unless they're printable-only."
  (not (node-printable-only-p node)))

(defmethod node-printable-only-p ((node t))
  "Default: nodes are not printable-only."
  nil)

(defmethod node-length ((node t))
  "Default: compute length from string representation."
  (length (node-string node)))

(defmethod node-children ((node t))
  "Default: leaf nodes have no children."
  nil)

(defmethod node-inner-p ((node t))
  "Default: nodes are not inner nodes."
  nil)

(defmethod node-replace-children ((node t) children)
  "Default: error for leaf nodes."
  (declare (ignore children))
  (error "Cannot replace children of leaf node: ~S" node))

(defmethod node-position ((node t))
  "Default: no position information."
  nil)

(defmethod node-set-position ((node t) position)
  "Default: return node unchanged (no position slot)."
  (declare (ignore position))
  node)

;;; Utility functions

(defun nodes-string (nodes)
  "Concatenate the string representations of NODES."
  (with-output-to-string (out)
    (dolist (node nodes)
      (write-string (node-string node) out))))

(defun child-sexprs (node)
  "Return a list of sexprs for all sexpr-able children of NODE."
  (loop for child in (node-children node)
        when (node-sexpr-able-p child)
        collect (node-sexpr child)))
