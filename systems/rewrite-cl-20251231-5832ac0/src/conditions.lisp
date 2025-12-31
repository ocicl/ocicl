;;;; conditions.lisp - Error conditions for rewrite-cl
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.node)

;;; Base condition for all rewrite-cl errors

(define-condition rewrite-cl-error (error)
  ()
  (:documentation "Base condition for all rewrite-cl errors."))

;;; Parse errors

(define-condition reader-error* (rewrite-cl-error)
  ((line :initarg :line :reader reader-error-line :initform nil)
   (column :initarg :column :reader reader-error-column :initform nil)
   (message :initarg :message :reader reader-error-message :initform "Parse error"))
  (:report (lambda (c stream)
             (if (and (reader-error-line c) (reader-error-column c))
                 (format stream "Parse error at line ~D, column ~D: ~A"
                         (reader-error-line c)
                         (reader-error-column c)
                         (reader-error-message c))
                 (format stream "Parse error: ~A"
                         (reader-error-message c)))))
  (:documentation "Error during parsing."))

(define-condition unexpected-eof (reader-error*)
  ()
  (:default-initargs :message "Unexpected end of input")
  (:documentation "Reached end of input unexpectedly."))

(define-condition unexpected-character (reader-error*)
  ((char :initarg :char :reader unexpected-character-char))
  (:report (lambda (c stream)
             (format stream "Unexpected character '~A' at line ~D, column ~D"
                     (unexpected-character-char c)
                     (reader-error-line c)
                     (reader-error-column c))))
  (:documentation "Encountered an unexpected character."))

(define-condition unmatched-delimiter (reader-error*)
  ((delimiter :initarg :delimiter :reader unmatched-delimiter-char)
   (expected :initarg :expected :reader unmatched-delimiter-expected :initform nil))
  (:report (lambda (c stream)
             (if (unmatched-delimiter-expected c)
                 (format stream "Unmatched delimiter '~A' at line ~D, column ~D (expected '~A')"
                         (unmatched-delimiter-char c)
                         (reader-error-line c)
                         (reader-error-column c)
                         (unmatched-delimiter-expected c))
                 (format stream "Unmatched delimiter '~A' at line ~D, column ~D"
                         (unmatched-delimiter-char c)
                         (reader-error-line c)
                         (reader-error-column c)))))
  (:documentation "Found an unmatched closing delimiter."))

(define-condition unterminated-string (reader-error*)
  ()
  (:default-initargs :message "Unterminated string literal")
  (:documentation "String literal was not closed."))

(define-condition unterminated-block-comment (reader-error*)
  ()
  (:default-initargs :message "Unterminated block comment")
  (:documentation "Block comment was not closed."))

(define-condition invalid-character-name (reader-error*)
  ((name :initarg :name :reader invalid-character-name-name))
  (:report (lambda (c stream)
             (format stream "Invalid character name '~A' at line ~D, column ~D"
                     (invalid-character-name-name c)
                     (reader-error-line c)
                     (reader-error-column c))))
  (:documentation "Unknown character name in #\\ syntax."))

;;; Node errors

(define-condition printable-only-error (rewrite-cl-error)
  ((node :initarg :node :reader printable-only-error-node))
  (:report (lambda (c stream)
             (format stream "Cannot convert printable-only node to sexpr: ~S"
                     (printable-only-error-node c))))
  (:documentation "Attempted to get sexpr from a printable-only node."))

;;; Zipper errors

(define-condition zipper-error (rewrite-cl-error)
  ()
  (:documentation "Base condition for zipper errors."))

(define-condition no-parent-error (zipper-error)
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "Cannot move up: already at root")))
  (:documentation "Attempted to move up from root."))

(define-condition no-children-error (zipper-error)
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "Cannot move down: node has no children")))
  (:documentation "Attempted to move down into a leaf node."))

(define-condition no-sibling-error (zipper-error)
  ((direction :initarg :direction :reader no-sibling-error-direction))
  (:report (lambda (c stream)
             (format stream "Cannot move ~A: no sibling in that direction"
                     (no-sibling-error-direction c))))
  (:documentation "Attempted to move to non-existent sibling."))
