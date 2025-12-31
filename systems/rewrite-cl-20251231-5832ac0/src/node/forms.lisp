;;;; forms.lisp - Public node construction API
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.node)

;;; This file re-exports and documents the public node API.
;;; All constructors are defined in their respective node files.

;;; Additional utility functions

(defun wrap-in-list (nodes)
  "Wrap NODES in a list node."
  (make-list-node nodes))

(defun wrap-in-parens (node)
  "Wrap NODE in parentheses (a list node)."
  (make-list-node (list node)))

(defun tag-predicate (tag)
  "Return a predicate that tests for nodes with TAG."
  (lambda (node) (eq tag (node-tag node))))

(defun list-node-p (node)
  "Return T if NODE is a list node."
  (and (seq-node-p node)
       (eql :list (seq-node-tag node))))

(defun vector-node-p (node)
  "Return T if NODE is a vector node."
  (and (seq-node-p node)
       (eql :vector (seq-node-tag node))))

;;; Building forms programmatically

(defun build-list (&rest elements)
  "Build a list node from ELEMENTS (Lisp values, converted via coerce)."
  (make-list-node (coerce-list-elements elements)))

(defun build-call (operator &rest arguments)
  "Build a function call node: (OPERATOR ARGUMENTS...)"
  (apply #'build-list operator arguments))

(defun build-defun (name lambda-list &rest body)
  "Build a defun form node."
  (make-list-node
   (list* (make-token-node 'defun)
          (spaces 1)
          (make-token-node name)
          (spaces 1)
          (make-list-node (coerce-list-elements lambda-list))
          (mapcan (lambda (form)
                    (list (newlines 1)
                          (spaces 2)
                          (coerce-to-node form)))
                  body))))

(defun build-let (bindings &rest body)
  "Build a let form node."
  (make-list-node
   (list* (make-token-node 'let)
          (spaces 1)
          (make-list-node
           (loop for (var val) in bindings
                 for first = t then nil
                 unless first
                   collect (newlines 1)
                   and collect (spaces 6)
                 collect (make-list-node
                          (list (make-token-node var)
                                (spaces 1)
                                (coerce-to-node val)))))
          (mapcan (lambda (form)
                    (list (newlines 1)
                          (spaces 2)
                          (coerce-to-node form)))
                  body))))
