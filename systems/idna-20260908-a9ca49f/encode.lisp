;;;; encode.lisp - functions to encode Unicode strings into IDNA Punycode.
;;; An almost-literal translation of the javascript IDN state machine
;;; implementation from
;;; http://stackoverflow.com/questions/183485/can-anyone-recommend-a-good-free-javascript-for-punycode-to-unicode-conversion

(in-package #:idna)

(defconstant +delimiter+ (code-char #x2d))
(defconstant +initial-n+ #x80)
(defconstant +initial-bias+ 72)
(defconstant +base+ 36)
(defconstant +maxint+ #x7FFFFFFF)
(defconstant +tmin+ 1)
(defconstant +tmax+ 26)
(defconstant +damp+ 700)
(defconstant +skew+ 38)

(defun adapt (delta numpoints first-time)
  (setf delta (if first-time
                  (truncate delta +damp+)
                  (truncate delta 2)))
  (incf delta (truncate delta numpoints))
  (+ (do ((k 0 (+ k +base+)))
         ((<= delta (truncate (* (- +base+ +tmin+) +tmax+) 2)) k)
       (setf delta (truncate delta (- +base+ +tmin+))))
     (truncate (* (1+ (- +base+ +tmin+)) delta)
               (+ delta +skew+))))

(defun encode-basic (c flag)
  (if flag
      c
      (char-downcase c)))

(defun encode-digit (d flag)
  (cond ((<= 0 d 25) (code-char (+ (char-code (if flag #\A #\a)) d)))
        ((< d 36) (code-char (+ (char-code #\0) (- d 26))))
        (t (error "Can't encode digit ~d" d))))

(defun punycode-encode (string &key preserve-case)
  "Encode STRING with the punycode algorithm documented in RFC3492.

When PRESERVE-CASE is true, emit case annotations and do not perform
case folding (to downcase), as required for ToASCII."
  (let* ((input-length (length string))
         (h 0)
         (b 0)
         (m +maxint+)
         (n +initial-n+)
         (delta 0)
         (bias +initial-bias+)
         (encodedp)
         (downcased-string (string-downcase string))
         (string (if preserve-case
                     string
                     downcased-string)))
    (when preserve-case
      (error "preserve-case is currently broken )-:"))

    (values
     (with-output-to-string (output)
       (loop for c across string
             for c-code = (char-code c)
             do (when (< c-code n)
                  (write-char (encode-basic c nil) output)
                  (incf b)
                  (incf h)))
       ;; h is the number of code points that have been handled, b is
       ;; the number of basic code points.
       (when (and (> b 0) (< h input-length))
         (write-char +delimiter+ output))
       ;; Main encoding loop:
       (loop
         (unless (< h input-length)
           (return))
         (setf encodedp t)
         (setf m +maxint+)
         ;; All non-basic code points below n have been handled
         ;; already. Find the next larger one:
         (loop for c across string
               for c-code = (char-code c)
               when (and (>= c-code n) (> m c-code))
                 do (setf m c-code))

         ;; Increase delta enough to advance the decoder's <n,i>
         ;; state to <m,0> but guard against overflow:
         (when (> (- m n) (truncate (- +maxint+ delta) (1+ h)))
           (error "punycode_overflow(1)"))

         (incf delta (* (- m n) (1+ h)))
         (setf n m)

         (loop for c across string
               for downcased-c across downcased-string
               for c-code = (char-code c)
               do (when (and (< c-code n) (> (incf delta) +maxint+))
                    (error "punycode_overflow(2)"))
                  (when (= c-code n)
                    ;; represent delta as a generalized variable-length integer:
                    (let ((q (do* ((q delta (truncate (- q tee) (- +base+ tee)))
                                   (k +base+ (+ k +base+))
                                   (tee (if (<= k bias)
                                            +tmin+
                                            (if (>= k (+ bias +tmax+))
                                                +tmax+
                                                (- k bias)))
                                        (if (<= k bias)
                                            +tmin+
                                            (if (>= k (+ bias +tmax+))
                                                +tmax+
                                                (- k bias)))))
                                  ((< q tee) q)
                               (write-char (encode-digit (+ tee (rem (- q tee) (- +base+ tee))) nil)
                                           output))))
                      (write-char (encode-digit q (not (eql downcased-c c))) output))
                    (setf bias (adapt delta (1+ h) (= h b)))
                    (setf delta 0)
                    (incf h)))
         (incf delta)
         (incf n)))
     encodedp)))

(defun to-ascii (string)
  "Encode string to IDNA punycode format using the ToASCII algorithm."
  (with-output-to-string (output)
    (loop for (component . rest) on (split-sequence:split-sequence #\. string)
          do (multiple-value-bind (punycode encodedp) (punycode-encode component :preserve-case nil)
               (cond (encodedp
                      (write-string "xn--" output)
                      (write-string punycode output))
                     (t (write-string component output)))
               (when rest
                 (write-char #\. output))))))