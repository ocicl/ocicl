;;;; string.lisp - Parse string literals
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.parser)

(defun parse-string-literal (reader)
  "Parse a string literal \"...\" with escape handling."
  (let* ((pos (reader-position reader))
         (source-text
          (with-output-to-string (out)
            ;; Read opening quote
            (write-char (reader-read reader) out)
            ;; Read string content
            (loop for char = (reader-read reader)
                  do (unless char
                       (error 'unterminated-string
                              :line (pos-line pos)
                              :column (pos-column pos)))
                     (write-char char out)
                     ;; Handle escape
                     (when (char= char #\\)
                       (let ((escaped (reader-read reader)))
                         (unless escaped
                           (error 'unterminated-string
                                  :line (pos-line pos)
                                  :column (pos-column pos)))
                         (write-char escaped out)))
                  until (char= char #\")))))
    (make-string-node (string-source-to-value source-text)
                      source-text
                      pos)))

(defun string-source-to-value (source-text)
  "Convert string source text to actual string value.
   CL strings only support \\\" and \\\\ escapes."
  (let ((content (subseq source-text 1 (1- (length source-text)))))
    (with-output-to-string (out)
      (loop with i = 0
            while (< i (length content))
            for char = (char content i)
            do (cond
                 ((char= char #\\)
                  (incf i)
                  (when (< i (length content))
                    (write-char (char content i) out)))
                 (t
                  (write-char char out)))
               (incf i)))))
