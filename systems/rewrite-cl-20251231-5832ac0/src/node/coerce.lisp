;;;; coerce.lisp - Convert Lisp forms to nodes
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.node)

(defgeneric coerce-to-node (value)
  (:documentation "Convert a Lisp VALUE to an appropriate node.
Adds sensible spacing between elements in sequences."))

(defmethod coerce-to-node ((value symbol))
  "Symbols become token nodes."
  (make-token-node value))

(defmethod coerce-to-node ((value number))
  "Numbers become token nodes."
  (make-token-node value))

(defmethod coerce-to-node ((value string))
  "Strings become string nodes."
  (make-string-node value))

(defmethod coerce-to-node ((value character))
  "Characters become character nodes."
  (make-character-node value))

(defmethod coerce-to-node ((value cons))
  "Conses become list nodes with spacing between elements."
  (make-list-node (coerce-list-elements value)))

(defmethod coerce-to-node ((value null))
  "NIL becomes a token node."
  (make-token-node nil "nil"))

(defmethod coerce-to-node ((value vector))
  "Vectors become vector nodes with spacing between elements."
  (make-vector-node (coerce-vector-elements value)))

(defmethod coerce-to-node ((value pathname))
  "Pathnames become pathname nodes."
  (make-pathname-node (make-string-node (namestring value))
                      "#P"))

;;; Helper functions

(defun coerce-list-elements (list)
  "Convert list elements to nodes with spacing."
  (loop for (elem . rest) on list
        for first = t then nil
        unless first
          collect (spaces 1)
        collect (coerce-to-node elem)))

(defun coerce-vector-elements (vector)
  "Convert vector elements to nodes with spacing."
  (loop for elem across vector
        for first = t then nil
        unless first
          collect (spaces 1)
        collect (coerce-to-node elem)))

;;; Convenience function for quoting

(defun coerce-quoted (value)
  "Convert VALUE to a quoted node."
  (make-quote-node (coerce-to-node value)))
