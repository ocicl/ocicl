;;;; token.lisp - Parse tokens (symbols, numbers, keywords)
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.parser)

(defun parse-token (reader)
  "Parse a token: symbol, keyword, or number."
  (let* ((pos (reader-position reader))
         (text (read-token-string reader)))
    (make-token-node (token-string-to-value text) text pos)))

(defun read-token-string (reader)
  "Read a token string, handling escape characters."
  (with-output-to-string (out)
    (loop for char = (reader-peek reader)
          while (and char (token-constituent-p char))
          do (cond
               ;; Escape: single character escape
               ((char= char #\\)
                (write-char (reader-read reader) out)
                (let ((next (reader-read reader)))
                  (when next (write-char next out))))
               ;; Multiple escape: |...|
               ((char= char #\|)
                (write-char (reader-read reader) out)
                (let ((terminated nil))
                  (loop for c = (reader-read reader)
                        while c
                        do (cond
                             ((char= c #\|)
                              (setf terminated t)
                              (loop-finish))
                             ((char= c #\\)
                              (write-char c out)
                              (let ((escaped (reader-read reader)))
                                (when escaped (write-char escaped out))))
                             (t
                              (write-char c out))))
                  (unless terminated
                    (error 'unexpected-eof
                           :message "Unterminated | escape in token"))
                  (write-char #\| out)))
               (t
                (write-char (reader-read reader) out))))))

(defun token-constituent-p (char)
  "Return T if CHAR can be part of a token."
  (not (or (whitespace-or-newline-p char)
           (member char '(#\( #\) #\' #\` #\, #\; #\")))))

(defun token-string-to-value (string)
  "Convert a token string to its Lisp value without using READ."
  (cond
    ;; Empty string - shouldn't happen but handle it
    ((zerop (length string))
     (intern ""))
    ;; Keyword - starts with :
    ((char= (char string 0) #\:)
     (intern (normalize-symbol-name (subseq string 1)) :keyword))
    (t
     ;; Try to parse as number first
     (or (parse-number-string string)
         ;; Package-qualified symbol
         (if (position #\: string)
             (parse-qualified-symbol string)
             ;; Plain symbol in current package
             (intern (normalize-symbol-name string)))))))

(defun normalize-symbol-name (string)
  "Normalize symbol name: handle escapes and case."
  (let ((result (make-array (length string) :element-type 'character
                            :fill-pointer 0 :adjustable t))
        (i 0)
        (len (length string))
        (in-escape nil))
    (loop while (< i len)
          for char = (char string i)
          do (cond
               ;; Single escape - next char is literal
               ((and (not in-escape) (char= char #\\))
                (incf i)
                (when (< i len)
                  (vector-push-extend (char string i) result)))
               ;; Multiple escape toggle
               ((char= char #\|)
                (setf in-escape (not in-escape)))
               ;; Inside multiple escape - literal
               (in-escape
                (vector-push-extend char result))
               ;; Normal - upcase
               (t
                (vector-push-extend (char-upcase char) result)))
             (incf i))
    (coerce result 'string)))

(defun parse-number-string (string)
  "Try to parse STRING as a number. Returns number or NIL."
  (let* ((str (string-trim '(#\Space #\Tab) string))
         (len (length str)))
    (when (zerop len) (return-from parse-number-string nil))
    (let ((start 0)
          (negative nil))
      ;; Handle sign
      (when (and (> len 0) (member (char str 0) '(#\+ #\-)))
        (setf negative (char= (char str 0) #\-))
        (setf start 1))
      ;; Try integer
      (multiple-value-bind (int pos) (parse-integer-part str start)
        (when int
          (cond
            ;; Just an integer
            ((= pos len)
             (return-from parse-number-string (if negative (- int) int)))
            ;; Ratio: n/d
            ((and (< pos len) (char= (char str pos) #\/))
             (let ((denom (parse-integer-part str (1+ pos))))
               (when (and denom (= (nth-value 1 (parse-integer-part str (1+ pos))) len))
                 (return-from parse-number-string
                   (/ (if negative (- int) int) denom)))))
            ;; Float with decimal point
            ((and (< pos len) (char= (char str pos) #\.))
             (return-from parse-number-string
               (parse-float-string str start negative)))
            ;; Float with exponent only
            ((and (< pos len) (member (char-upcase (char str pos)) '(#\E #\D #\F #\S #\L)))
             (return-from parse-number-string
               (parse-float-string str start negative)))
            ;; Not a recognized number format
            (t nil)))))))

(defun parse-integer-part (string start)
  "Parse integer starting at START. Returns (values integer end-position) or NIL."
  (let ((end start)
        (len (length string)))
    (loop while (and (< end len) (digit-char-p (char string end)))
          do (incf end))
    (if (> end start)
        (values (parse-integer string :start start :end end) end)
        nil)))

(defun parse-float-string (string start negative)
  "Parse a float from STRING starting at START."
  (let* ((len (length string))
         (pos start)
         (int-part 0)
         (frac-part 0)
         (frac-digits 0)
         (exp-part 0)
         (exp-negative nil)
         (float-type *read-default-float-format*))
    ;; Integer part
    (loop while (and (< pos len) (digit-char-p (char string pos)))
          do (setf int-part (+ (* int-part 10) (digit-char-p (char string pos))))
             (incf pos))
    ;; Fractional part
    (when (and (< pos len) (char= (char string pos) #\.))
      (incf pos)
      (loop while (and (< pos len) (digit-char-p (char string pos)))
            do (setf frac-part (+ (* frac-part 10) (digit-char-p (char string pos))))
               (incf frac-digits)
               (incf pos)))
    ;; Exponent part
    (when (and (< pos len) (member (char-upcase (char string pos)) '(#\E #\D #\F #\S #\L)))
      (setf float-type (case (char-upcase (char string pos))
                         (#\E *read-default-float-format*)
                         (#\D 'double-float)
                         (#\F 'single-float)
                         (#\S 'short-float)
                         (#\L 'long-float)
                         (otherwise *read-default-float-format*)))
      (incf pos)
      (when (and (< pos len) (member (char string pos) '(#\+ #\-)))
        (setf exp-negative (char= (char string pos) #\-))
        (incf pos))
      (loop while (and (< pos len) (digit-char-p (char string pos)))
            do (setf exp-part (+ (* exp-part 10) (digit-char-p (char string pos))))
               (incf pos)))
    ;; Compute result
    (let* ((mantissa (+ (float int-part 1.0d0)
                        (/ (float frac-part 1.0d0)
                           (expt 10.0d0 frac-digits))))
           (exp-multiplier (expt 10.0d0 (if exp-negative (- exp-part) exp-part)))
           (result (* mantissa exp-multiplier)))
      (coerce (if negative (- result) result) float-type))))

(defun parse-qualified-symbol (string)
  "Parse a package-qualified symbol like PKG:SYM or PKG::SYM."
  (let* ((colon-pos (position #\: string))
         (double-colon (and colon-pos
                            (< (1+ colon-pos) (length string))
                            (char= (char string (1+ colon-pos)) #\:)))
         (pkg-name (subseq string 0 colon-pos))
         (sym-start (if double-colon (+ colon-pos 2) (1+ colon-pos)))
         (sym-name (normalize-symbol-name (subseq string sym-start)))
         (pkg (find-package (normalize-symbol-name pkg-name))))
    (if pkg
        (if double-colon
            (intern sym-name pkg)
            (multiple-value-bind (sym status) (find-symbol sym-name pkg)
              (if (eql status :external)
                  sym
                  (intern sym-name pkg))))  ; Allow even if not exported
        ;; Package doesn't exist - create uninterned symbol
        (make-symbol sym-name))))

;;; Keyword parsing

(defun parse-keyword (reader)
  "Parse a keyword starting with colon."
  (let* ((pos (reader-position reader))
         (text (read-token-string reader)))
    (make-token-node (token-string-to-value text) text pos)))
