;;;; util.lisp - Shared utilities for rewrite-cl
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.node)

;;; Character classification

(declaim (inline whitespace-char-p))
(defun whitespace-char-p (char)
  "Return T if CHAR is a whitespace character (space or tab)."
  (and char (or (char= char #\Space) (char= char #\Tab))))

(declaim (inline newline-char-p))
(defun newline-char-p (char)
  "Return T if CHAR is a newline character."
  (and char (or (char= char #\Newline)
                (char= char #\Return))))

(declaim (inline whitespace-or-newline-p))
(defun whitespace-or-newline-p (char)
  "Return T if CHAR is whitespace or newline."
  (or (whitespace-char-p char) (newline-char-p char)))

(declaim (inline delimiter-char-p))
(defun delimiter-char-p (char)
  "Return T if CHAR is a delimiter that terminates tokens."
  (and char
       (or (whitespace-or-newline-p char)
           (char= char #\()
           (char= char #\))
           (char= char #\")
           (char= char #\')
           (char= char #\`)
           (char= char #\,)
           (char= char #\;))))

(declaim (inline constituent-char-p))
(defun constituent-char-p (char)
  "Return T if CHAR is a constituent character (part of a token)."
  (and char (not (delimiter-char-p char))))

;;; String utilities

(defun string-concat (&rest strings)
  "Concatenate strings efficiently."
  (apply #'concatenate 'string strings))

(defun char-to-string (char)
  "Convert a single character to a string."
  (make-string 1 :initial-element char))
