;;;; character.lisp - Parse character literals
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.parser)

(defun parse-character (reader pos)
  "Parse a character literal #\\x or #\\Name.
Called after # is consumed, \\ is next."
  (reader-read reader)  ; consume \\
  (let ((source-text
          (with-output-to-string (out)
            (write-string "#\\" out)
            ;; Read character or character name
            (let ((first-char (reader-read reader)))
              (unless first-char
                (error 'unexpected-eof
                       :line (pos-line pos)
                       :column (pos-column pos)
                       :message "Expected character after #\\"))
              (write-char first-char out)
              ;; Check if it's a character name (alphabetic followed by more alphabetics)
              (when (alpha-char-p first-char)
                (loop for char = (reader-peek reader)
                      while (and char (alpha-char-p char))
                      do (write-char (reader-read reader) out)))))))
    (make-character-node (char-source-to-value source-text pos)
                         source-text
                         pos)))

(defun char-source-to-value (source-text &optional pos)
  "Convert character source text #\\... to character value."
  (let ((name (subseq source-text 2)))
    (cond
      ;; Single character
      ((= (length name) 1)
       (char name 0))
      ;; Named characters (case-insensitive)
      ((string-equal name "Newline") #\Newline)
      ((string-equal name "Linefeed") #\Linefeed)
      ((string-equal name "Return") #\Return)
      ((string-equal name "Space") #\Space)
      ((string-equal name "Tab") #\Tab)
      ((string-equal name "Page") #\Page)
      ((string-equal name "Backspace") #\Backspace)
      ((string-equal name "Rubout") #\Rubout)
      ((string-equal name "Nul") (code-char 0))
      ((string-equal name "Null") (code-char 0))
      ((string-equal name "Escape") (code-char 27))
      ((string-equal name "Esc") (code-char 27))
      ((string-equal name "Bell") (code-char 7))
      ;; Unknown - signal error
      (t
       (error 'invalid-character-name
              :name name
              :line (when pos (pos-line pos))
              :column (when pos (pos-column pos))
              :message (format nil "Invalid character name: ~A" name))))))
