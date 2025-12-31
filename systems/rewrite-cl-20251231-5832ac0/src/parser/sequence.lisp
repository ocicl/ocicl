;;;; sequence.lisp - Parse lists and other sequences
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.parser)

(defun parse-list (reader)
  "Parse a list: (...)"
  (let ((pos (reader-position reader)))
    (reader-read reader)  ; consume (
    (let ((children (parse-sequence-contents reader #\))))
      (reader-read reader)  ; consume )
      (make-list-node children pos))))

(defun parse-sequence-contents (reader end-char)
  "Parse contents of a sequence until END-CHAR.
END-CHAR is not consumed."
  (loop for char = (reader-peek reader)
        while (and char (char/= char end-char))
        ;; Check for unexpected closing delimiters
        when (and (member char '(#\) #\]))
                  (char/= char end-char))
          do (error 'unmatched-delimiter
                    :delimiter char
                    :expected end-char
                    :line (reader-line reader)
                    :column (reader-column reader))
        collect (parse-next reader)))
