;;;; core.lisp - Main parser dispatch
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.parser)

(defun parse-next (reader)
  "Parse the next node from READER. Returns NIL at EOF."
  (let ((char (reader-peek reader)))
    (cond
      ;; EOF
      ((null char) nil)

      ;; Whitespace
      ((whitespace-char-p char)
       (parse-whitespace reader))

      ;; Newline
      ((newline-char-p char)
       (parse-newline reader))

      ;; Line comment
      ((char= char #\;)
       (parse-line-comment reader))

      ;; List
      ((char= char #\()
       (parse-list reader))

      ;; Unmatched closing paren
      ((char= char #\))
       (error 'unmatched-delimiter
              :delimiter char
              :line (reader-line reader)
              :column (reader-column reader)))

      ;; String
      ((char= char #\")
       (parse-string-literal reader))

      ;; Quote
      ((char= char #\')
       (parse-quote reader))

      ;; Backquote
      ((char= char #\`)
       (parse-backquote reader))

      ;; Unquote
      ((char= char #\,)
       (parse-unquote reader))

      ;; Dispatch macro
      ((char= char #\#)
       (parse-dispatch reader))

      ;; Token (symbol, number, keyword)
      (t
       (parse-token reader)))))

(defun parse-all (reader)
  "Parse all nodes from READER into a list."
  (loop for node = (parse-next reader)
        while node
        collect node))

;;; Public API

(defun parse-string (string)
  "Parse STRING and return the first node."
  (let ((reader (make-string-reader string)))
    (parse-next reader)))

(defun parse-string-all (string)
  "Parse STRING and return list of all nodes."
  (let ((reader (make-string-reader string)))
    (parse-all reader)))

(defun parse-file (pathname)
  "Parse file at PATHNAME and return the first node."
  (with-open-file (stream pathname :direction :input)
    (let ((reader (make-source-reader stream)))
      (parse-next reader))))

(defun parse-file-all (pathname)
  "Parse file at PATHNAME and return list of all nodes."
  (with-open-file (stream pathname :direction :input)
    (let ((reader (make-source-reader stream)))
      (parse-all reader))))
