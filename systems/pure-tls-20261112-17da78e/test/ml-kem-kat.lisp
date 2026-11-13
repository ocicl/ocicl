;;; ml-kem-kat.lisp --- ML-KEM-768 Known Answer Tests (FIPS 203)
;;;
;;; Tests ML-KEM-768 decapsulation against official NIST test vectors from:
;;; https://github.com/post-quantum-cryptography/KAT/tree/main/MLKEM
;;;
;;; Download test vectors first:
;;;   curl -sL https://raw.githubusercontent.com/post-quantum-cryptography/KAT/main/MLKEM/kat_MLKEM_768.rsp \
;;;        -o test/vectors/kat_MLKEM_768.rsp
;;;
;;; Usage:
;;;   (asdf:load-system :pure-tls)
;;;   (load "test/ml-kem-kat.lisp")
;;;   (ml-kem-kat:run-tests)

(defpackage #:ml-kem-kat
  (:use #:cl)
  (:export #:run-tests #:run-tests-verbose))

(in-package #:ml-kem-kat)

(defun hex-to-octets (hex-string)
  "Convert a hex string to an octet vector."
  (let* ((len (/ (length hex-string) 2))
         (result (make-array len :element-type '(unsigned-byte 8))))
    (loop for i from 0 below len
          for pos = (* i 2)
          do (setf (aref result i)
                   (parse-integer hex-string :start pos :end (+ pos 2) :radix 16)))
    result))

(defun parse-kat-file (path &optional (max-count nil))
  "Parse a KAT response file, returning a list of test vectors.
Each vector is a plist with :count :pk :sk :ct :ss :ct-n :ss-n."
  (let ((vectors '())
        (current nil))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
                 (when (and (> (length trimmed) 0)
                           (not (char= (char trimmed 0) #\#)))
                   (let ((eq-pos (position #\= trimmed)))
                     (when eq-pos
                       (let ((key (string-trim '(#\Space) (subseq trimmed 0 eq-pos)))
                             (val (string-trim '(#\Space) (subseq trimmed (1+ eq-pos)))))
                         (cond
                           ((string= key "count")
                            (when current
                              (push current vectors))
                            (let ((count-val (parse-integer val)))
                              (when (and max-count (>= count-val max-count))
                                (return))
                              (setf current (list :count count-val))))
                           ((string= key "pk")
                            (setf (getf current :pk) val))
                           ((string= key "sk")
                            (setf (getf current :sk) val))
                           ((string= key "ct")
                            (setf (getf current :ct) val))
                           ((string= key "ss")
                            (setf (getf current :ss) val))
                           ((string= key "ct_n")
                            (setf (getf current :ct-n) val))
                           ((string= key "ss_n")
                            (setf (getf current :ss-n) val))))))))))
    (when current
      (push current vectors))
    (nreverse vectors)))

(defun test-decapsulation (vector &key verbose)
  "Test ML-KEM-768 decapsulation against a single test vector.
Returns T if test passes, NIL otherwise."
  (let* ((count (getf vector :count))
         (sk-hex (getf vector :sk))
         (ct-hex (getf vector :ct))
         (ss-hex (getf vector :ss))
         (sk (hex-to-octets sk-hex))
         (ct (hex-to-octets ct-hex))
         (expected-ss (hex-to-octets ss-hex)))
    (when verbose
      (format t "~&Test ~D: Decapsulation... " count))
    (handler-case
        (let ((computed-ss (pure-tls::ml-kem-768-decaps sk ct)))
          (if (equalp computed-ss expected-ss)
              (progn
                (when verbose (format t "PASS~%"))
                t)
              (progn
                (format t "~&Test ~D: FAIL - Shared secret mismatch~%" count)
                (format t "  Expected: ~A~%" (subseq ss-hex 0 (min 32 (length ss-hex))))
                (format t "  Got:      ~A~%"
                        (with-output-to-string (s)
                          (loop for b across (subseq computed-ss 0 (min 16 (length computed-ss)))
                                do (format s "~2,'0x" b))))
                nil)))
      (error (e)
        (format t "~&Test ~D: ERROR - ~A~%" count e)
        nil))))

(defun test-implicit-rejection (vector &key verbose)
  "Test ML-KEM-768 implicit rejection with invalid ciphertext.
Returns T if test passes, NIL otherwise."
  (let* ((count (getf vector :count))
         (sk-hex (getf vector :sk))
         (ct-n-hex (getf vector :ct-n))
         (ss-n-hex (getf vector :ss-n)))
    ;; Skip if no invalid ciphertext test
    (unless (and ct-n-hex ss-n-hex)
      (return-from test-implicit-rejection t))
    (let ((sk (hex-to-octets sk-hex))
          (ct-n (hex-to-octets ct-n-hex))
          (expected-ss-n (hex-to-octets ss-n-hex)))
      (when verbose
        (format t "~&Test ~D: Implicit rejection... " count))
      (handler-case
          (let ((computed-ss-n (pure-tls::ml-kem-768-decaps sk ct-n)))
            (if (equalp computed-ss-n expected-ss-n)
                (progn
                  (when verbose (format t "PASS~%"))
                  t)
                (progn
                  (format t "~&Test ~D: FAIL - Implicit rejection mismatch~%" count)
                  (format t "  Expected: ~A~%" (subseq ss-n-hex 0 (min 32 (length ss-n-hex))))
                  (format t "  Got:      ~A~%"
                          (with-output-to-string (s)
                            (loop for b across (subseq computed-ss-n 0 (min 16 (length computed-ss-n)))
                                  do (format s "~2,'0x" b))))
                  nil)))
        (error (e)
          (format t "~&Test ~D: ERROR in implicit rejection - ~A~%" count e)
          nil)))))

(defun run-tests (&key (max-count 100) verbose)
  "Run ML-KEM-768 KAT tests.
MAX-COUNT limits number of test vectors (default 100).
Returns T if all tests pass."
  (let* ((kat-file (merge-pathnames "test/vectors/kat_MLKEM_768.rsp"
                                    (asdf:system-source-directory :pure-tls)))
         (vectors (parse-kat-file kat-file max-count))
         (total (length vectors))
         (decaps-pass 0)
         (reject-pass 0)
         (decaps-fail 0)
         (reject-fail 0))
    (format t "~&")
    (format t "========================================~%")
    (format t "  ML-KEM-768 FIPS 203 KAT Tests~%")
    (format t "========================================~%")
    (format t "~%")
    (format t "Test vectors: ~D~%" total)
    (format t "Source: NIST FIPS 203 KAT~%")
    (format t "~%")

    (loop for vector in vectors
          do (if (test-decapsulation vector :verbose verbose)
                 (incf decaps-pass)
                 (incf decaps-fail))
             (if (test-implicit-rejection vector :verbose verbose)
                 (incf reject-pass)
                 (incf reject-fail)))

    (format t "~%")
    (format t "========================================~%")
    (format t "  Results~%")
    (format t "========================================~%")
    (format t "~%")
    (format t "Decapsulation:      ~D/~D passed~%" decaps-pass total)
    (format t "Implicit rejection: ~D/~D passed~%" reject-pass total)
    (format t "~%")

    (let ((all-pass (and (= decaps-fail 0) (= reject-fail 0))))
      (if all-pass
          (format t "*** ALL TESTS PASSED ***~%")
          (format t "*** FAILURES DETECTED ***~%"))
      (format t "~%")
      all-pass)))

(defun run-tests-verbose (&key (max-count 10))
  "Run ML-KEM-768 KAT tests with verbose output."
  (run-tests :max-count max-count :verbose t))
