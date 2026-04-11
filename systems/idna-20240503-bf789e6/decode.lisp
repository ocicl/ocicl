;;;; decode.lisp - decode IDNA punycode into unicode strings
;;; Again, an almost-literal translation of the javascript IDN state
;;; machine implementation from
;;; http://stackoverflow.com/questions/183485/can-anyone-recommend-a-good-free-javascript-for-punycode-to-unicode-conversion

(in-package #:idna)

(defun decode-digit (cp)
  (cond ((< (- cp 48) 10) (- cp 22))
        ((< (- cp 65) 26) (- cp 65))
        ((< (- cp 97) 26) (- cp 97))
        (t +base+)
        ))

(defun punycode-decode (input &key preserve-case)
  (declare (optimize debug))
  (let ((output nil)
        (output-length 0)
        (case-flags (make-array (length input) :initial-element nil))
        (n +initial-n+)
        (i 0)
        (bias +initial-bias+)
        (basic (or (position +delimiter+ input :from-end t)
                   0))
        (oldi 0))

    (when preserve-case
      (error "preserve-case is currently broken )-:"))
    
    (loop for j from 0 below basic
          for char across input
          for char-code = (char-code char)
          do (when preserve-case
               (setf (aref case-flags output-length)
                     (< (- char-code 65)
                        26)))
             (when (>= char-code #x80)
               (error "Invalid input ~x >=#x80" char-code))
             (setf output (nconc output (list char-code)))
             (incf output-length))

    ;; Main decoding loop: Start just after the last delimiter if any
    ;; basic code points were copied; start at beginning otherwise.

    (loop with ic = (if (zerop basic) 0 (1+ basic))
          with out = 0
          while (< ic (length input))
          ;; ic is the index of the next character to be consumed.
          
          ;; Decode a generalized variable-length integer into delta,
          ;; which gets added to i. The overflow checking is easier if
          ;; we increase i as we go, then subtract off its starting
          ;; value at the end to obtain delta.
          do (setf oldi i)
             (loop with w = 1
                   with digit
                   with tee
                   for k from +base+ by +base+
                   do
                      (assert (< ic (length input)) () "punycode_bad_input(1)")
                      (setf digit (decode-digit (char-code (aref input ic))))
                      (incf ic)
                      (setf tee (cond
                                  ((<= k bias) +tmin+)
                                  ((>= k (+ bias +tmax+)) +tmax+)
                                  (t (- k bias))))
                      (assert (< digit +base+) () "punycode_bad_input(2)")
                      (assert (< digit (truncate (- +maxint+ i) w)) () "punycode_overflow(1)")
                      (incf i (* digit w))
                      (when (< digit tee) (return))
                      (assert (<= w (truncate +maxint+ (- +base+ tee))) () "punycode_overflow(2)")
                      (setf w (* w (- +base+ tee))))
             (setf out (1+ output-length))
             (setf bias (adapt (- i oldi) out (zerop oldi)))

             ;; i was supposed to wrap around from out to 0,
             ;; incrementing n each time, so we'll fix that now:
             (assert (< (truncate i out) (- +maxint+ n)) () "punycode_overflow(3)")
             (incf n (truncate i out))
             (setf i (rem i out))

             ;; case of last character determines uppercase flag:
             (when preserve-case
               (setf case-flags (nconc (subseq case-flags 0 i)
                                       (list (< (- (char-code (aref input (1- ic))) 65)
                                                26))
                                       (subseq case-flags i))))

             (setf output (nconc (subseq output 0 i) (list n) (subseq output i)))
             (incf output-length)
             (incf i))

    (when preserve-case
      (loop with len = output-length
            for i from 0 below len
            do (when (elt case-flags i)
                 (setf (aref output i)
                       (char-code (char-upcase (code-char (aref output i))))))))
    (map 'string #'code-char output)))

(defun to-unicode (string)
  "Encode string from IDNA punycode format using the ToUnicode algorithm."
  (with-output-to-string (output)
    (loop for (component . rest) on (split-sequence:split-sequence #\. string)
          for decoded = (if (eql 4 (mismatch "xn--" component))
                            (punycode-decode (subseq component 4) :preserve-case nil)
                            component)
          do (write-string decoded output)
             (when rest (write-char #\. output)))))