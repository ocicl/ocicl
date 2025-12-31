;;;; quote.lisp - Parse quote syntax
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.parser)

(defun parse-quote (reader)
  "Parse a quote: 'form"
  (let ((pos (reader-position reader)))
    (reader-read reader)  ; consume '
    (let ((child (parse-next reader)))
      (unless child
        (error 'unexpected-eof
               :line (pos-line pos)
               :column (pos-column pos)
               :message "Expected form after '"))
      (make-quote-node child pos))))

(defun parse-backquote (reader)
  "Parse a backquote: `form"
  (let ((pos (reader-position reader)))
    (reader-read reader)  ; consume `
    (let ((child (parse-next reader)))
      (unless child
        (error 'unexpected-eof
               :line (pos-line pos)
               :column (pos-column pos)
               :message "Expected form after `"))
      (make-syntax-quote-node child pos))))

(defun parse-unquote (reader)
  "Parse an unquote: ,form or ,@form"
  (let ((pos (reader-position reader)))
    (reader-read reader)  ; consume ,
    (let ((splicing-p (eql (reader-peek reader) #\@)))
      (when splicing-p
        (reader-read reader))  ; consume @
      (let ((child (parse-next reader)))
        (unless child
          (error 'unexpected-eof
                 :line (pos-line pos)
                 :column (pos-column pos)
                 :message (if splicing-p
                              "Expected form after ,@"
                              "Expected form after ,")))
        (if splicing-p
            (make-unquote-splicing-node child pos)
            (make-unquote-node child pos))))))
