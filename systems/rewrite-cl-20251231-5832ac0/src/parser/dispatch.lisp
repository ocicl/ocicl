;;;; dispatch.lisp - Parse # dispatch macros
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.parser)

(defun parse-dispatch (reader)
  "Parse a # dispatch macro."
  (let ((pos (reader-position reader)))
    (reader-read reader)  ; consume #
    (let ((dispatch-char (reader-peek reader)))
      (case dispatch-char
        ;; #'function
        (#\' (parse-function-shorthand reader pos))
        ;; #(vector)
        (#\( (parse-vector-literal reader pos))
        ;; #\character
        (#\\ (parse-character reader pos))
        ;; #|block comment|#
        (#\| (parse-block-comment reader pos))
        ;; #+feature, #-feature
        (#\+ (parse-feature-expr reader pos :positive))
        (#\- (parse-feature-expr reader pos :negative))
        ;; #.read-eval
        (#\. (parse-read-eval reader pos))
        ;; #:uninterned
        (#\: (parse-uninterned-symbol reader pos))
        ;; #p"pathname" or #P"pathname"
        ((#\p #\P) (parse-pathname-literal reader pos))
        ;; #*bitvector
        (#\* (parse-bit-vector-literal reader pos))
        ;; #c(complex)
        ((#\c #\C) (parse-complex-literal reader pos))
        ;; #b, #o, #x, #r - radix
        ((#\b #\B) (parse-radix reader pos 2 "b"))
        ((#\o #\O) (parse-radix reader pos 8 "o"))
        ((#\x #\X) (parse-radix reader pos 16 "x"))
        ;; #nA array, #nr radix, #n= #n# circular
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (parse-numeric-dispatch reader pos))
        ;; #S struct, #s struct
        ((#\s #\S) (parse-struct-literal reader pos))
        (otherwise
         (parse-unknown-dispatch reader pos))))))

;;; #'function

(defun parse-function-shorthand (reader pos)
  "Parse #'function"
  (reader-read reader)  ; consume '
  (let ((child (parse-next reader)))
    (unless child
      (error 'unexpected-eof
             :line (pos-line pos)
             :column (pos-column pos)
             :message "Expected form after #'"))
    (make-function-node child pos)))

;;; #(vector)

(defun parse-vector-literal (reader pos)
  "Parse #(...) vector literal."
  (reader-read reader)  ; consume (
  (let ((children (parse-sequence-contents reader #\))))
    (reader-read reader)  ; consume )
    (make-vector-node children pos)))

;;; #+feature, #-feature

(defun parse-feature-expr (reader pos polarity)
  "Parse #+feature form or #-feature form."
  (reader-read reader)  ; consume + or -
  (let ((feature (parse-next reader)))
    (unless feature
      (error 'unexpected-eof
             :line (pos-line pos)
             :column (pos-column pos)
             :message "Expected feature expression"))
    (let ((form (parse-next reader)))
      (unless form
        (error 'unexpected-eof
               :line (pos-line pos)
               :column (pos-column pos)
               :message "Expected form after feature expression"))
      (make-feature-node polarity feature form pos))))

;;; #.form

(defun parse-read-eval (reader pos)
  "Parse #.form read-time evaluation."
  (reader-read reader)  ; consume .
  (let ((child (parse-next reader)))
    (unless child
      (error 'unexpected-eof
             :line (pos-line pos)
             :column (pos-column pos)
             :message "Expected form after #."))
    (make-read-eval-node child pos)))

;;; #:uninterned

(defun parse-uninterned-symbol (reader pos)
  "Parse #:symbol uninterned symbol."
  (reader-read reader)  ; consume :
  (let ((name (read-token-string reader)))
    (make-token-node (make-symbol (string-upcase name))
                     (concatenate 'string "#:" name)
                     pos)))

;;; #p"pathname"

(defun parse-pathname-literal (reader pos)
  "Parse #p\"path\" or #P\"path\"."
  (let ((prefix-char (reader-read reader)))  ; consume p or P
    (let ((string-node (parse-string-literal reader)))
      (make-pathname-node string-node
                          (format nil "#~A" prefix-char)
                          pos))))

;;; #*bitvector

(defun parse-bit-vector-literal (reader pos)
  "Parse #*bits bit vector."
  (reader-read reader)  ; consume *
  (let ((source (with-output-to-string (out)
                  (write-string "#*" out)
                  (loop for char = (reader-peek reader)
                        while (and char (member char '(#\0 #\1)))
                        do (write-char (reader-read reader) out)))))
    (make-bit-vector-node source
                          (bits-string-to-bit-vector (subseq source 2))
                          pos)))

(defun bits-string-to-bit-vector (string)
  "Convert a string of 0s and 1s to a bit-vector."
  (let* ((len (length string))
         (bv (make-array len :element-type 'bit)))
    (dotimes (i len bv)
      (setf (aref bv i) (if (char= (char string i) #\1) 1 0)))))

;;; #c(complex)

(defun parse-complex-literal (reader pos)
  "Parse #c(real imag) or #C(real imag)."
  (reader-read reader)  ; consume c or C
  (let ((list-node (parse-next reader)))
    (make-complex-node list-node pos)))

;;; #b, #o, #x radix

(defun parse-radix (reader pos radix char)
  "Parse #b, #o, or #x number."
  (reader-read reader)  ; consume radix char
  (let* ((num-text (read-token-string reader))
         (source (concatenate 'string "#" (string (char-downcase (char char 0))) num-text)))
    (make-token-node (parse-integer-radix num-text radix) source pos)))

(defun parse-integer-radix (string radix)
  "Parse STRING as an integer in given RADIX."
  (let ((result 0)
        (negative nil)
        (start 0))
    ;; Handle sign
    (when (and (> (length string) 0)
               (member (char string 0) '(#\+ #\-)))
      (setf negative (char= (char string 0) #\-))
      (setf start 1))
    ;; Parse digits
    (loop for i from start below (length string)
          for char = (char-upcase (char string i))
          for digit = (digit-char-p char radix)
          do (if digit
                 (setf result (+ (* result radix) digit))
                 (return-from parse-integer-radix nil)))
    (if negative (- result) result)))

;;; #n... (numeric prefix)

(defun parse-numeric-dispatch (reader pos)
  "Parse #n followed by various things."
  ;; Read the numeric prefix
  (let ((prefix (with-output-to-string (out)
                  (write-string "#" out)
                  (loop for char = (reader-peek reader)
                        while (and char (digit-char-p char))
                        do (write-char (reader-read reader) out)))))
    ;; Now check what follows
    (let ((next-char (reader-peek reader)))
      (case next-char
        ;; #nA array
        ((#\a #\A)
         (reader-read reader)
         (let ((contents (parse-next reader)))
           (make-array-node (concatenate 'string prefix (string next-char))
                            contents
                            pos)))
        ;; #nr radix
        ((#\r #\R)
         (reader-read reader)
         (let* ((num-text (read-token-string reader))
                (source (concatenate 'string prefix (string next-char) num-text))
                (radix (parse-integer (subseq prefix 1))))  ; extract radix from "#36" -> 36
           (make-token-node (parse-integer-radix num-text radix) source pos)))
        ;; #n= define circular
        (#\=
         (reader-read reader)
         (let ((form (parse-next reader)))
           (make-unknown-macro-node (concatenate 'string prefix "=")
                                    (list form)
                                    pos)))
        ;; #n# reference circular
        (#\#
         (reader-read reader)
         (make-unknown-macro-node (concatenate 'string prefix "#")
                                  nil
                                  pos))
        (otherwise
         (make-unknown-macro-node prefix nil pos))))))

;;; #s(...) struct

(defun parse-struct-literal (reader pos)
  "Parse #s(...) or #S(...) structure literal."
  (let ((prefix-char (reader-read reader)))  ; consume s or S
    (let ((contents (parse-next reader)))
      (make-unknown-macro-node (format nil "#~A" prefix-char)
                               (list contents)
                               pos))))

;;; Unknown dispatch

(defun parse-unknown-dispatch (reader pos)
  "Parse an unknown # dispatch macro."
  (let ((char (reader-peek reader)))
    (cond
      (char
       (reader-read reader)
       (let ((form (parse-next reader)))
         (make-unknown-macro-node (format nil "#~A" char)
                                  (if form (list form) nil)
                                  pos)))
      (t
       (make-token-node (intern "#") "#" pos)))))
