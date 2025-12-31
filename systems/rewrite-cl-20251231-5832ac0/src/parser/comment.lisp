;;;; comment.lisp - Parse comments
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.parser)

(defun parse-line-comment (reader)
  "Parse a line comment starting with semicolon.
Does not consume the trailing newline."
  (let ((pos (reader-position reader))
        (text (with-output-to-string (out)
                (loop for char = (reader-peek reader)
                      while (and char (not (newline-char-p char)))
                      do (write-char (reader-read reader) out)))))
    (make-comment-node text pos)))

(defun parse-block-comment (reader pos)
  "Parse a block comment #| ... |# with nesting support.
Called after #| has been recognized (# consumed, | is next)."
  (reader-read reader)  ; consume |
  (let ((text (with-output-to-string (out)
                (write-string "#|" out)
                (let ((depth 1))
                  (loop while (> depth 0)
                        for char = (reader-read reader)
                        do (unless char
                             (error 'unterminated-block-comment
                                    :line (pos-line pos)
                                    :column (pos-column pos)))
                           (write-char char out)
                           (cond
                             ;; Check for |#
                             ((and (char= char #\|)
                                   (eql (reader-peek reader) #\#))
                              (write-char (reader-read reader) out)
                              (decf depth))
                             ;; Check for nested #|
                             ((and (char= char #\#)
                                   (eql (reader-peek reader) #\|))
                              (write-char (reader-read reader) out)
                              (incf depth))
                             ;; Regular character - continue
                             (t nil)))))))
    (make-block-comment-node text pos)))
