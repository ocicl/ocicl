;;;; whitespace.lisp - Parse whitespace and newlines
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.parser)

(defun parse-whitespace (reader)
  "Parse horizontal whitespace (spaces and tabs)."
  (let ((pos (reader-position reader))
        (text (reader-read-while reader #'whitespace-char-p)))
    (make-whitespace-node text pos)))

(defun parse-newline (reader)
  "Parse a newline character or CRLF sequence."
  (let ((pos (reader-position reader))
        (char (reader-read reader)))
    (cond
      ((and (char= char #\Return)
            (eql (reader-peek reader) #\Newline))
       (reader-read reader)
       (make-newline-node (coerce '(#\Return #\Newline) 'string) pos))
      (t
       (make-newline-node (string char) pos)))))

(defun parse-whitespace-or-newline (reader)
  "Parse whitespace or newline, returning appropriate node."
  (let ((char (reader-peek reader)))
    (cond
      ((whitespace-char-p char)
       (parse-whitespace reader))
      ((newline-char-p char)
       (parse-newline reader))
      (t nil))))
