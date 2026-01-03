;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
(in-package :crypto-tests)

(defun hex-string-to-byte-array (string &key (start 0) (end nil))
  ;; This function disappears from profiles if SBCL can inline the
  ;; POSITION call, so declare SPEED high enough to trigger that.
  (declare (type string string) (optimize (speed 2)))
  (let* ((end (or end (length string)))
         (length (/ (- end start) 2))
         (key (make-array length :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) key))
    (flet ((char-to-digit (char)
             (declare (type base-char char))
             (let ((x (cl:position char #.(coerce "0123456789abcdef" 'simple-base-string)
                                   :test #'char-equal)))
               (or x (error "Invalid hex key ~A specified" string)))))
      (loop for i from 0
            for j from start below end by 2
            do (setf (aref key i)
                     (+ (* (char-to-digit (char string j)) 16)
                        (char-to-digit (char string (1+ j)))))
            finally (return key)))))


;;; test vector files

(defun test-vector-filename (ident)
  (merge-pathnames (make-pathname :directory '(:relative "test-vectors")
                                  :name (substitute #\- #\/ (format nil "~(~A~)" ident))
                                  :type "testvec")
                   #.*compile-file-pathname*))

(defun sharp-a (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (crypto:ascii-string-to-byte-array (read stream t nil t)))

(defun sharp-h (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (hex-string-to-byte-array (read stream t nil t)))

(defun run-test-vector-file (name function-map)
  (let ((filename (test-vector-filename name))
        (*readtable* (copy-readtable)))
    (set-dispatch-macro-character #\# #\a #'sharp-a *readtable*)
    (set-dispatch-macro-character #\# #\h #'sharp-h *readtable*)
    (with-open-file (stream filename :direction :input
                            :element-type 'character
                            :if-does-not-exist :error)
      (loop for form = (read stream nil stream)
         until (eq form stream) do
         (cond
           ((not (listp form))
            (error "Invalid form in test vector file ~A: ~A" filename form))
           (t
            (let ((test-function (cdr (assoc (car form) function-map))))
              (unless test-function
                (error "No test function defined for ~A" (car form)))
              (apply test-function name (cdr form)))))
         finally (return t)))))

;;; cipher testing

(defun cipher-test-guts (cipher-name mode key input output
                         &optional extra-make-cipher-args)
  (let ((cipher (apply #'crypto:make-cipher cipher-name
                       :key key :mode mode
                       extra-make-cipher-args))
        (scratch (copy-seq input)))
    (crypto:encrypt cipher input scratch)
    (when (mismatch scratch output)
      (error "encryption failed for ~A on key ~A, input ~A, output ~A"
             cipher-name key input output))
    (apply #'reinitialize-instance cipher :key key extra-make-cipher-args)
    (crypto:decrypt cipher output scratch)
    (when (mismatch scratch input)
      (error "decryption failed for ~A on key ~A, input ~A, output ~A"
             cipher-name key output input))))

#+(or lispworks sbcl cmucl openmcl allegro abcl ecl clisp)
(defun cipher-stream-test-guts (cipher-name mode key input output
                                &optional extra-args)
  (let* ((out-stream (crypto:make-octet-output-stream))
         (enc-stream (apply #'crypto:make-encrypting-stream
                            out-stream cipher-name mode key extra-args))
         (in-stream (crypto:make-octet-input-stream output))
         (dec-stream (apply #'crypto:make-decrypting-stream
                            in-stream cipher-name mode key extra-args)))
    (write-byte (aref input 0) enc-stream)
    (write-sequence input enc-stream :start 1)
    (let ((result (crypto:get-output-stream-octets out-stream)))
      (when (mismatch result output)
        (error "stream encryption failed for ~A on key ~A, input ~A, output ~A"
               cipher-name key input output)))
    (let ((result (copy-seq output)))
      (setf (aref result 0) (read-byte dec-stream))
      (read-sequence result dec-stream :start 1)
      (when (mismatch result input)
        (error "stream decryption failed for ~A on key ~A, input ~A, output ~A"
               cipher-name key output input)))))

(defun ecb-mode-test (cipher-name hexkey hexinput hexoutput)
  (cipher-test-guts cipher-name :ecb hexkey hexinput hexoutput))

(defun ecb-tweak-mode-test (cipher-name hexkey hextweak hexinput hexoutput)
  (cipher-test-guts cipher-name :ecb hexkey hexinput hexoutput
                    (list :tweak hextweak)))

(defun stream-mode-test (cipher-name hexkey hexinput hexoutput)
  (cipher-test-guts cipher-name :stream hexkey hexinput hexoutput))

(defun stream-nonce-mode-test (cipher-name hexkey hexiv hexinput hexoutput)
  (cipher-test-guts cipher-name :stream hexkey hexinput hexoutput
                    (list :initialization-vector hexiv)))

(defun keystream-test (cipher-name key iv keystream)
  (let* ((mode (if (= 1 (crypto:block-length cipher-name)) :stream :ctr))
         (cipher (crypto:make-cipher cipher-name :key key :mode mode :initialization-vector iv))
         (buffer (make-array 1000 :element-type '(unsigned-byte 8) :initial-element 0)))
    (crypto:keystream-position cipher 100)
    (crypto:encrypt-in-place cipher buffer :start 100 :end 213)
    (crypto:keystream-position cipher 500)
    (crypto:encrypt-in-place cipher buffer :start 500 :end 1000)
    (crypto:keystream-position cipher 213)
    (crypto:encrypt-in-place cipher buffer :start 213 :end 500)
    (crypto:keystream-position cipher 0)
    (crypto:encrypt-in-place cipher buffer :end 100)
    (crypto:keystream-position cipher 765)
    (when (or (/= (crypto:keystream-position cipher) 765)
              (mismatch buffer keystream))
      (error "getting/setting key stream position failed for ~A on key ~A" cipher-name key))))

#+(or lispworks sbcl cmucl openmcl allegro abcl ecl clisp)
(defun stream-mode-test/stream (cipher-name hexkey hexinput hexoutput)
  (cipher-stream-test-guts cipher-name :stream hexkey hexinput hexoutput))

#+(or lispworks sbcl cmucl openmcl allegro abcl ecl clisp)
(defun stream-nonce-mode-test/stream (cipher-name hexkey hexiv hexinput hexoutput)
  (cipher-stream-test-guts cipher-name :stream hexkey hexinput hexoutput
                           (list :initialization-vector hexiv)))

(defparameter *cipher-tests*
  (list (cons :ecb-mode-test 'ecb-mode-test)
        (cons :ecb-tweak-mode-test 'ecb-tweak-mode-test)
        (cons :stream-mode-test 'stream-mode-test)
        (cons :stream-nonce-mode-test 'stream-nonce-mode-test)
        (cons :keystream-test 'keystream-test)))

#+(or lispworks sbcl cmucl openmcl allegro abcl ecl clisp)
(defparameter *cipher-stream-tests*
  (list (cons :ecb-mode-test 'ignore-test)
        (cons :ecb-tweak-mode-test 'ignore-test)
        (cons :stream-mode-test 'stream-mode-test/stream)
        (cons :stream-nonce-mode-test 'stream-nonce-mode-test/stream)
        (cons :keystream-test 'ignore-test)))


;;; encryption mode consistency checking

;;; tests from NIST

(defun mode-test (mode cipher-name key iv input output)
  (labels ((frob-hex-string (cipher func input)
             (let ((scratch (copy-seq input)))
               (funcall func cipher input scratch)
               scratch))
           (cipher-test (cipher func input output)
             (not (mismatch (frob-hex-string cipher func input) output))))
    (let ((cipher (crypto:make-cipher cipher-name :key key :mode mode
                                      :initialization-vector iv)))
      (unless (cipher-test cipher 'crypto:encrypt input output)
        (error "encryption failed for ~A on key ~A, input ~A, output ~A"
               cipher-name key input output))
      (reinitialize-instance cipher :key key :mode mode
                             :initialization-vector iv)
      (unless (cipher-test cipher 'crypto:decrypt output input)
        (error "decryption failed for ~A on key ~A, input ~A, output ~A"
               cipher-name key output input)))))

(defun mode-padding-test (mode cipher-name padding key iv input output)
  (let ((cipher (crypto:make-cipher cipher-name
                                    :key key
                                    :mode mode
                                    :initialization-vector iv
                                    :padding padding))
        (buffer1 (make-array (length input) :element-type '(unsigned-byte 8)))
        (buffer2 (make-array (length output) :element-type '(unsigned-byte 8))))
    (crypto:encrypt cipher input buffer2 :handle-final-block t)
    (when (mismatch buffer2 output)
      (error "encryption failed for ~A on key ~A, input ~A, output ~A"
             cipher-name key input output))
    (reinitialize-instance cipher
                           :key key
                           :mode mode
                           :initialization-vector iv
                           :padding padding)
    (crypto:decrypt cipher output buffer1 :handle-final-block t)
    (when (mismatch buffer1 input)
      (error "decryption failed for ~A on key ~A, input ~A, output ~A"
             cipher-name key output input))))

(defparameter *mode-tests*
  (list (cons :mode-test 'mode-test)
        (cons :mode-padding-test 'ignore-test)))

(defparameter *mode-padding-tests*
  (list (cons :mode-test 'ignore-test)
        (cons :mode-padding-test 'mode-padding-test)))


;;; digest testing routines

(defun digest-test/base (digest-name input expected-digest)
  (let ((result (crypto:digest-sequence digest-name input)))
    (when (mismatch result expected-digest)
      (error "one-shot ~A digest of ~S failed" digest-name input))))

(defun digest-test/incremental (digest-name input expected-digest)
  (loop with length = (length input)
     with digester = (crypto:make-digest digest-name)
     for i from 0 below length
     do (crypto:update-digest digester input :start i :end (1+ i))
     finally
     (let ((result (crypto:produce-digest digester)))
       (when (mismatch result expected-digest)
         (error "incremental ~A digest of ~S failed" digest-name input)))))

(defun digest-test/fill-pointer (digest-name octets expected-digest)
  (let* ((input (let ((x (make-array (* 2 (length octets))
                                     :fill-pointer 0
                                     :element-type '(unsigned-byte 8))))
                  (dotimes (i (length octets) x)
                    (vector-push (aref octets i) x))))
         (result (crypto:digest-sequence digest-name input)))
    (when (mismatch result expected-digest)
      (error "fill-pointer'd ~A digest of ~S failed" digest-name input))))

#+(or lispworks sbcl cmucl openmcl allegro abcl ecl clisp)
(defun digest-test/stream (digest-name input expected-digest)
  (let* ((stream (crypto:make-digesting-stream digest-name)))
    (when (plusp (length input))
      (write-byte (aref input 0) stream)
      (write-sequence input stream :start 1))
    (crypto:produce-digest stream) ; Calling produce-digest twice should not give a wrong hash
    (when (mismatch (crypto:produce-digest stream) expected-digest)
      (error "stream-y ~A digest of ~S failed" digest-name input))))

(defun digest-test/reinitialize-instance (digest-name input expected-digest)
  (let* ((digest (crypto:make-digest digest-name))
         (result (progn
                   (crypto:digest-sequence digest input)
                   (crypto:digest-sequence (reinitialize-instance digest) input))))
    (when (mismatch result expected-digest)
      (error "testing reinitialize-instance ~A digest of ~S failed" digest-name input))))

(defun digest-bit-test (digest-name leading byte trailing expected-digest)
  (let* ((input (let ((vector (make-array (+ 1 leading trailing)
                                          :element-type '(unsigned-byte 8)
                                          :initial-element 0)))
                  (setf (aref vector leading) byte)
                  vector))
         (result (crypto:digest-sequence digest-name input)))
    (when (mismatch result expected-digest)
      (error "individual bit test ~A digest of (~D #x~2,'0X ~D) failed"
             digest-name leading byte trailing))))

(defun xof-digest-test (digest-name output-length input expected-digest)
  (let* ((digest (crypto:make-digest digest-name :output-length output-length))
         (result (crypto:digest-sequence digest input)))
    (when (mismatch result expected-digest)
      (error "one-shot ~A xof digest of ~S failed" digest-name input))))

(defparameter *digest-tests*
  (list (cons :digest-test 'digest-test/base)
        (cons :digest-bit-test 'digest-bit-test)
        (cons :xof-digest-test 'xof-digest-test)))

(defun ignore-test (&rest args)
  (declare (ignore args))
  nil)

(defparameter *digest-incremental-tests*
  (list (cons :digest-test 'digest-test/incremental)
        (cons :digest-bit-test 'ignore-test)
        (cons :xof-digest-test 'ignore-test)))

(defparameter *digest-fill-pointer-tests*
  (list (cons :digest-test 'digest-test/fill-pointer)
        (cons :digest-bit-test 'ignore-test)
        (cons :xof-digest-test 'ignore-test)))

#+(or lispworks sbcl cmucl openmcl allegro abcl ecl clisp)
(defparameter *digest-stream-tests*
  (list (cons :digest-test 'digest-test/stream)
        (cons :digest-bit-test 'ignore-test)
        (cons :xof-digest-test 'ignore-test)))

(defparameter *digest-reinitialize-instance-tests*
  (list (cons :digest-test 'digest-test/reinitialize-instance)
        (cons :digest-bit-test 'ignore-test)
        (cons :xof-digest-test 'ignore-test)))


;;; mac testing routines

(defun mac-test/base (mac-name key data expected-digest &rest args)
  (let ((mac (apply #'crypto:make-mac mac-name key args)))
    (crypto:update-mac mac data)
    (let ((result (crypto:produce-mac mac)))
      (when (mismatch result expected-digest)
        (error "one-shot ~A mac of ~A failed on key ~A, args ~A"
               mac-name data key args)))))

(defun mac-test/incremental (mac-name key data expected-digest &rest args)
  (loop with length = (length data)
        with mac = (apply #'crypto:make-mac mac-name key args)
        for i from 0 below length
        do (crypto:update-mac mac data :start i :end (1+ i))
        finally (let ((result (crypto:produce-mac mac)))
                  (when (mismatch result expected-digest)
                    (error "incremental ~A mac of ~A failed on key ~A, args ~A"
                           mac-name data key args)))))

#+(or lispworks sbcl cmucl openmcl allegro abcl ecl clisp)
(defun mac-test/stream (mac-name key data expected-digest &rest args)
  (let ((stream (apply #'crypto:make-authenticating-stream mac-name key args)))
    (when (plusp (length data))
      (write-byte (aref data 0) stream)
      (write-sequence data stream :start 1))
    (crypto:produce-mac stream) ; Calling produce-mac twice should not give a wrong MAC
    (let ((result (crypto:produce-mac stream)))
      (when (mismatch result expected-digest)
        (error "stream ~A mac of ~A failed on key ~A, args ~A"
               mac-name data key args)))))

(defun mac-test/reinitialize-instance (mac-name key data expected-digest &rest args)
  (let* ((mac (apply #'crypto:make-mac mac-name key args))
         (result1 (progn
                    (crypto:update-mac mac data)
                    (crypto:produce-mac mac))))
    (declare (ignorable result1))
    (cond
      ((typep mac 'ironclad:gmac)
       (reinitialize-instance mac :key key :initialization-vector (car (last args))))
      (t
       (reinitialize-instance mac :key key)))
    (let ((result2 (progn
                     (crypto:update-mac mac data)
                     (crypto:produce-mac mac))))
      (when (mismatch result2 expected-digest)
        (error "testing reinitialize-instance ~A mac of ~A failed on key ~A, args ~A"
               mac-name data key args)))))

(defparameter *mac-tests*
  (list (cons :mac-test 'mac-test/base)))

(defparameter *mac-incremental-tests*
  (list (cons :mac-test 'mac-test/incremental)))

#+(or lispworks sbcl cmucl openmcl allegro abcl ecl clisp)
(defparameter *mac-stream-tests*
  (list (cons :mac-test 'mac-test/stream)))

(defparameter *mac-reinitialize-instance-tests*
  (list (cons :mac-test 'mac-test/reinitialize-instance)))


;;; PRNG testing routines
(defun fortuna-test (name seed entropy expected-sequence)
  (declare (ignore name))
  (let ((prng (crypto:make-prng :fortuna
                                :seed (coerce seed 'crypto::simple-octet-vector)))
        (num-bytes (length expected-sequence)))
    (loop for (source pool-id event) in entropy
       do (crypto:add-random-event source pool-id event prng))
    (assert (equalp expected-sequence
            (crypto:random-data num-bytes prng)))))

(defun generator-test (name cipher seeds expected-sequences)
  (declare (ignore name))
  (let ((generator (make-instance 'crypto:fortuna-generator :cipher cipher)))
    (loop for seed in seeds
       do (crypto:prng-reseed (coerce seed '(vector (unsigned-byte 8))) generator))
    (every (lambda (sequence)
             (assert (zerop (mod (length sequence) 16)))
             (assert (equalp sequence
                             (crypto:random-data (length sequence)
                                         generator))))
           expected-sequences)))

(defparameter *prng-tests*
  `((:fortuna-test . ,'fortuna-test)
    (:generator-test . ,'generator-test)))


;;; Public key testing routines

(defun rsa-oaep-encryption-test (name n e d input seed output)
  ;; Redefine oaep-encode to use a defined seed for the test instead of a random one
  (setf (symbol-function 'ironclad::oaep-encode)
        (lambda (digest-name message num-bytes &optional label)
          (let* ((digest-name (if (eq digest-name t) :sha1 digest-name))
                 (digest-len (ironclad:digest-length digest-name)))
            (assert (<= (length message) (- num-bytes (* 2 digest-len) 2)))
            (let* ((digest (ironclad:make-digest digest-name))
                   (label (or label (coerce #() '(vector (unsigned-byte 8)))))
                   (padding-len (- num-bytes (length message) (* 2 digest-len) 2))
                   (padding (make-array padding-len :element-type '(unsigned-byte 8) :initial-element 0))
                   (l-hash (ironclad:digest-sequence digest label))
                   (db (concatenate '(vector (unsigned-byte 8)) l-hash padding #(1) message))
                   (db-mask (ironclad::mgf digest-name seed (- num-bytes digest-len 1)))
                   (masked-db (map '(vector (unsigned-byte 8)) #'logxor db db-mask))
                   (seed-mask (ironclad::mgf digest-name masked-db digest-len))
                   (masked-seed (map '(vector (unsigned-byte 8)) #'logxor seed seed-mask)))
              (concatenate '(vector (unsigned-byte 8)) #(0) masked-seed masked-db)))))

  (let* ((pk (ironclad:make-public-key :rsa :n n :e e))
         (sk (ironclad:make-private-key :rsa :n n :d d))
         (c (ironclad:encrypt-message pk input :oaep t))
         (m (ironclad:decrypt-message sk output :oaep t)))
    (when (mismatch c output)
      (error "encryption failed for ~A on pkey (~A ~A), input ~A, output ~A"
             name n e input output))
    (when (mismatch m input)
      (error "decryption failed for ~A on skey (~A ~A), input ~A, output ~A"
             name n d input output))))

(defun elgamal-encryption-test (name p g x y input k output)
  (let* ((ironclad::*signature-nonce-for-test* k)
         (pk (ironclad:make-public-key :elgamal :p p :g g :y y))
         (sk (ironclad:make-private-key :elgamal :p p :g g :x x :y y))
         (c (ironclad:encrypt-message pk input))
         (m (ironclad:decrypt-message sk output)))
    (when (mismatch c output)
      (error "encryption failed for ~A on pkey (~A ~A ~A), input ~A, output ~A"
             name p g y input output))
    (when (mismatch m input)
      (error "decryption failed for ~A on skey (~A ~A ~A ~A), input ~A, output ~A"
             name p g x y input output))))

(defun rsa-pss-signature-test (name n e d input salt signature)
  ;; Redefine pss-encode to use a defined salt for the test instead of a random one
  (setf (symbol-function 'ironclad::pss-encode)
        (lambda (digest-name message num-bytes)
          (let* ((digest-name (if (eq digest-name t) :sha1 digest-name))
                 (digest-len (ironclad:digest-length digest-name)))
            (assert (>= num-bytes (+ (* 2 digest-len) 2)))
            (let* ((m-hash (ironclad:digest-sequence digest-name message))
                   (m1 (concatenate '(vector (unsigned-byte 8)) #(0 0 0 0 0 0 0 0) m-hash salt))
                   (h (ironclad:digest-sequence digest-name m1))
                   (ps (make-array (- num-bytes (* 2 digest-len) 2)
                                   :element-type '(unsigned-byte 8)
                                   :initial-element 0))
                   (db (concatenate '(vector (unsigned-byte 8)) ps #(1) salt))
                   (db-mask (ironclad::mgf digest-name h (- num-bytes digest-len 1)))
                   (masked-db (map '(vector (unsigned-byte 8)) #'logxor db db-mask)))
              (setf (ldb (byte 1 7) (elt masked-db 0)) 0)
              (concatenate '(vector (unsigned-byte 8)) masked-db h #(188))))))

  (let* ((pk (ironclad:make-public-key :rsa :n n :e e))
         (sk (ironclad:make-private-key :rsa :n n :d d))
         (s (ironclad:sign-message sk input :pss t)))
    (when (mismatch s signature)
      (error "signature failed for ~A on skey (~A ~A), input ~A, signature ~A"
             name n d input signature))
    (unless (ironclad:verify-signature pk input signature :pss t)
      (error "signature verification failed for ~A on pkey (~A ~A), input ~A, signature ~A"
             name n e input signature))))

(defun elgamal-signature-test (name p g x y input k signature)
  (let* ((ironclad::*signature-nonce-for-test* k)
         (pk (ironclad:make-public-key :elgamal :p p :g g :y y))
         (sk (ironclad:make-private-key :elgamal :p p :g g :x x :y y))
         (s (ironclad:sign-message sk input)))
    (when (mismatch s signature)
      (error "signature failed for ~A on skey (~A ~A ~A ~A), input ~A, signature ~A"
             name p g x y input signature))
    (unless (ironclad:verify-signature pk input signature)
      (error "signature verification failed for ~A on pkey (~A ~A ~A), input ~A, signature ~A"
             name p g y input signature))))

(defun dsa-signature-test (name p q g x y input k signature)
  (let* ((ironclad::*signature-nonce-for-test* k)
         (sk (ironclad:make-private-key :dsa :p p :q q :g g :x x :y y))
         (pk (ironclad:make-public-key :dsa :p p :q q :g g :y y))
         (s (ironclad:sign-message sk input)))
    (when (mismatch s signature)
      (error "signature failed for ~A on skey (~A ~A ~A ~A ~A), input ~A, signature ~A"
             name p q g x y input signature))
    (unless (ironclad:verify-signature pk input signature)
      (error "signature verification failed for ~A on pkey (~A ~A ~A ~A), input ~A, signature ~A"
             name p q g y input signature))))

(defun ed25519-signature-test (name skey pkey input signature)
  (let* ((sk (ironclad:make-private-key :ed25519 :x skey :y pkey))
         (pk (ironclad:make-public-key :ed25519 :y pkey))
         (s (ironclad:sign-message sk input)))
    (when (mismatch s signature)
      (error "signature failed for ~A on skey ~A, input ~A, signature ~A"
             name skey input signature))
    (unless (ironclad:verify-signature pk input signature)
      (error "signature verification failed for ~A on pkey ~A, input ~A, signature ~A"
             name pkey input signature))))

(defun ed448-signature-test (name skey pkey input signature)
  (let* ((sk (ironclad:make-private-key :ed448 :x skey :y pkey))
         (pk (ironclad:make-public-key :ed448 :y pkey))
         (s (ironclad:sign-message sk input)))
    (when (mismatch s signature)
      (error "signature failed for ~A on skey ~A, input ~A, signature ~A"
             name skey input signature))
    (unless (ironclad:verify-signature pk input signature)
      (error "signature verification failed for ~A on pkey ~A, input ~A, signature ~A"
             name pkey input signature))))

(defun secp256k1-signature-test (name skey pkey input k signature)
  (let* ((ironclad::*signature-nonce-for-test* k)
         (sk (ironclad:make-private-key :secp256k1 :x skey :y pkey))
         (pk (ironclad:make-public-key :secp256k1 :y pkey))
         (s (ironclad:sign-message sk input)))
    (when (mismatch s signature)
      (error "signature failed for ~A on skey ~A, input ~A, signature ~A"
             name skey input signature))
    (unless (ironclad:verify-signature pk input signature)
      (error "signature verification failed for ~A on pkey ~A, input ~A, signature ~A"
             name pkey input signature))))

(defun secp256r1-signature-test (name skey pkey input k signature)
  (let* ((ironclad::*signature-nonce-for-test* k)
         (sk (ironclad:make-private-key :secp256r1 :x skey :y pkey))
         (pk (ironclad:make-public-key :secp256r1 :y pkey))
         (s (ironclad:sign-message sk input)))
    (when (mismatch s signature)
      (error "signature failed for ~A on skey ~A, input ~A, signature ~A"
             name skey input signature))
    (unless (ironclad:verify-signature pk input signature)
      (error "signature verification failed for ~A on pkey ~A, input ~A, signature ~A"
             name pkey input signature))))

(defun secp384r1-signature-test (name skey pkey input k signature)
  (let* ((ironclad::*signature-nonce-for-test* k)
         (sk (ironclad:make-private-key :secp384r1 :x skey :y pkey))
         (pk (ironclad:make-public-key :secp384r1 :y pkey))
         (s (ironclad:sign-message sk input)))
    (when (mismatch s signature)
      (error "signature failed for ~A on skey ~A, input ~A, signature ~A"
             name skey input signature))
    (unless (ironclad:verify-signature pk input signature)
      (error "signature verification failed for ~A on pkey ~A, input ~A, signature ~A"
             name pkey input signature))))

(defun secp521r1-signature-test (name skey pkey input k signature)
  (let* ((ironclad::*signature-nonce-for-test* k)
         (sk (ironclad:make-private-key :secp521r1 :x skey :y pkey))
         (pk (ironclad:make-public-key :secp521r1 :y pkey))
         (s (ironclad:sign-message sk input)))
    (when (mismatch s signature)
      (error "signature failed for ~A on skey ~A, input ~A, signature ~A"
             name skey input signature))
    (unless (ironclad:verify-signature pk input signature)
      (error "signature verification failed for ~A on pkey ~A, input ~A, signature ~A"
             name pkey input signature))))

(defun curve25519-dh-test (name skey1 pkey1 skey2 pkey2 shared-secret)
  (let* ((sk1 (ironclad:make-private-key :curve25519 :x skey1 :y pkey1))
         (pk1 (ironclad:make-public-key :curve25519 :y pkey1))
         (sk2 (ironclad:make-private-key :curve25519 :x skey2 :y pkey2))
         (pk2 (ironclad:make-public-key :curve25519 :y pkey2))
         (ss1 (ironclad:diffie-hellman sk1 pk2))
         (ss2 (ironclad:diffie-hellman sk2 pk1)))
    (when (mismatch ss1 shared-secret)
      (error "shared secret computation failed for ~A on skey ~A, pkey ~A, secret ~A"
             name skey1 pkey2 shared-secret))
    (when (mismatch ss2 shared-secret)
      (error "shared secret computation failed for ~A on skey ~A, pkey ~A, secret ~A"
             name skey2 pkey1 shared-secret))))

(defun curve448-dh-test (name skey1 pkey1 skey2 pkey2 shared-secret)
  (let* ((sk1 (ironclad:make-private-key :curve448 :x skey1 :y pkey1))
         (pk1 (ironclad:make-public-key :curve448 :y pkey1))
         (sk2 (ironclad:make-private-key :curve448 :x skey2 :y pkey2))
         (pk2 (ironclad:make-public-key :curve448 :y pkey2))
         (ss1 (ironclad:diffie-hellman sk1 pk2))
         (ss2 (ironclad:diffie-hellman sk2 pk1)))
    (when (mismatch ss1 shared-secret)
      (error "shared secret computation failed for ~A on skey ~A, pkey ~A, secret ~A"
             name skey1 pkey2 shared-secret))
    (when (mismatch ss2 shared-secret)
      (error "shared secret computation failed for ~A on skey ~A, pkey ~A, secret ~A"
             name skey2 pkey1 shared-secret))))

(defun elgamal-dh-test (name p g skey1 pkey1 skey2 pkey2 shared-secret)
  (let* ((sk1 (ironclad:make-private-key :elgamal :p p :g g :x skey1 :y pkey1))
         (pk1 (ironclad:make-public-key :elgamal :p p :g g :y pkey1))
         (sk2 (ironclad:make-private-key :elgamal :p p :g g :x skey2 :y pkey2))
         (pk2 (ironclad:make-public-key :elgamal :p p :g g :y pkey2))
         (ss1 (ironclad:diffie-hellman sk1 pk2))
         (ss2 (ironclad:diffie-hellman sk2 pk1)))
    (when (mismatch ss1 shared-secret)
      (error "shared secret computation failed for ~A on skey ~A, pkey ~A, secret ~A"
             name skey1 pkey2 shared-secret))
    (when (mismatch ss2 shared-secret)
      (error "shared secret computation failed for ~A on skey ~A, pkey ~A, secret ~A"
             name skey2 pkey1 shared-secret))))

(defun secp256k1-dh-test (name skey1 pkey1 skey2 pkey2 shared-secret)
  (let* ((sk1 (ironclad:make-private-key :secp256k1 :x skey1 :y pkey1))
         (pk1 (ironclad:make-public-key :secp256k1 :y pkey1))
         (sk2 (ironclad:make-private-key :secp256k1 :x skey2 :y pkey2))
         (pk2 (ironclad:make-public-key :secp256k1 :y pkey2))
         (ss1 (ironclad:diffie-hellman sk1 pk2))
         (ss2 (ironclad:diffie-hellman sk2 pk1)))
    (when (mismatch ss1 shared-secret)
      (error "shared secret computation failed for ~A on skey ~A, pkey ~A, secret ~A"
             name skey1 pkey2 shared-secret))
    (when (mismatch ss2 shared-secret)
      (error "shared secret computation failed for ~A on skey ~A, pkey ~A, secret ~A"
             name skey2 pkey1 shared-secret))))

(defun secp256r1-dh-test (name skey1 pkey1 skey2 pkey2 shared-secret)
  (let* ((sk1 (ironclad:make-private-key :secp256r1 :x skey1 :y pkey1))
         (pk1 (ironclad:make-public-key :secp256r1 :y pkey1))
         (sk2 (ironclad:make-private-key :secp256r1 :x skey2 :y pkey2))
         (pk2 (ironclad:make-public-key :secp256r1 :y pkey2))
         (ss1 (ironclad:diffie-hellman sk1 pk2))
         (ss2 (ironclad:diffie-hellman sk2 pk1)))
    (when (mismatch ss1 shared-secret)
      (error "shared secret computation failed for ~A on skey ~A, pkey ~A, secret ~A"
             name skey1 pkey2 shared-secret))
    (when (mismatch ss2 shared-secret)
      (error "shared secret computation failed for ~A on skey ~A, pkey ~A, secret ~A"
             name skey2 pkey1 shared-secret))))

(defun secp384r1-dh-test (name skey1 pkey1 skey2 pkey2 shared-secret)
  (let* ((sk1 (ironclad:make-private-key :secp384r1 :x skey1 :y pkey1))
         (pk1 (ironclad:make-public-key :secp384r1 :y pkey1))
         (sk2 (ironclad:make-private-key :secp384r1 :x skey2 :y pkey2))
         (pk2 (ironclad:make-public-key :secp384r1 :y pkey2))
         (ss1 (ironclad:diffie-hellman sk1 pk2))
         (ss2 (ironclad:diffie-hellman sk2 pk1)))
    (when (mismatch ss1 shared-secret)
      (error "shared secret computation failed for ~A on skey ~A, pkey ~A, secret ~A"
             name skey1 pkey2 shared-secret))
    (when (mismatch ss2 shared-secret)
      (error "shared secret computation failed for ~A on skey ~A, pkey ~A, secret ~A"
             name skey2 pkey1 shared-secret))))

(defun secp521r1-dh-test (name skey1 pkey1 skey2 pkey2 shared-secret)
  (let* ((sk1 (ironclad:make-private-key :secp521r1 :x skey1 :y pkey1))
         (pk1 (ironclad:make-public-key :secp521r1 :y pkey1))
         (sk2 (ironclad:make-private-key :secp521r1 :x skey2 :y pkey2))
         (pk2 (ironclad:make-public-key :secp521r1 :y pkey2))
         (ss1 (ironclad:diffie-hellman sk1 pk2))
         (ss2 (ironclad:diffie-hellman sk2 pk1)))
    (when (mismatch ss1 shared-secret)
      (error "shared secret computation failed for ~A on skey ~A, pkey ~A, secret ~A"
             name skey1 pkey2 shared-secret))
    (when (mismatch ss2 shared-secret)
      (error "shared secret computation failed for ~A on skey ~A, pkey ~A, secret ~A"
             name skey2 pkey1 shared-secret))))

(defparameter *public-key-encryption-tests*
  (list (cons :rsa-oaep-encryption-test 'rsa-oaep-encryption-test)
        (cons :elgamal-encryption-test 'elgamal-encryption-test)))

(defparameter *public-key-signature-tests*
  (list (cons :rsa-pss-signature-test 'rsa-pss-signature-test)
        (cons :elgamal-signature-test 'elgamal-signature-test)
        (cons :dsa-signature-test 'dsa-signature-test)
        (cons :ed25519-signature-test 'ed25519-signature-test)
        (cons :ed448-signature-test 'ed448-signature-test)
        (cons :secp256k1-signature-test 'secp256k1-signature-test)
        (cons :secp256r1-signature-test 'secp256r1-signature-test)
        (cons :secp384r1-signature-test 'secp384r1-signature-test)
        (cons :secp521r1-signature-test 'secp521r1-signature-test)))

(defparameter *public-key-diffie-hellman-tests*
  (list (cons :curve25519-dh-test 'curve25519-dh-test)
        (cons :curve448-dh-test 'curve448-dh-test)
        (cons :elgamal-dh-test 'elgamal-dh-test)
        (cons :secp256k1-dh-test 'secp256k1-dh-test)
        (cons :secp256r1-dh-test 'secp256r1-dh-test)
        (cons :secp384r1-dh-test 'secp384r1-dh-test)
        (cons :secp521r1-dh-test 'secp521r1-dh-test)))


;;; authenticated encryption testing routines

(defun aead-test (mode-name input ad output tag &rest args)
  (let* ((parameters (case mode-name
                       ((:gcm gcm crypto:gcm :eax eax crypto:eax)
                        (list :cipher-name (car args)
                              :key (cadr args)
                              :initialization-vector (caddr args)))
                       ((:etm etm crypto:etm)
                        (destructuring-bind (cipher-name ckey mode iv mac-name mkey mparam) args
                          (let ((cipher (crypto:make-cipher cipher-name
                                                            :key ckey
                                                            :mode mode
                                                            :initialization-vector iv))
                                (mac (if mparam
                                         (crypto:make-mac mac-name mkey mparam)
                                         (crypto:make-mac mac-name mkey))))
                            (list :cipher cipher :mac mac))))))
         (ae (apply #'crypto:make-authenticated-encryption-mode mode-name parameters))
         (ciphertext (crypto:encrypt-message ae input :associated-data ad)))
    (when (or (mismatch ciphertext output)
              (mismatch (crypto:produce-tag ae) tag))
      (error "encryption failed for ~A, input ~A, output ~A" mode-name input output))
    (setf parameters (case mode-name
                       ((:gcm gcm crypto:gcm :eax eax crypto:eax)
                        parameters)
                       ((:etm etm crypto:etm)
                        (destructuring-bind (cipher-name ckey mode iv mac-name mkey mparam) args
                          (let ((cipher (crypto:make-cipher cipher-name
                                                            :key ckey
                                                            :mode mode
                                                            :initialization-vector iv))
                                (mac (if mparam
                                         (crypto:make-mac mac-name mkey mparam)
                                         (crypto:make-mac mac-name mkey))))
                            (list :cipher cipher :mac mac))))))
    (apply #'reinitialize-instance ae :tag tag parameters)
    (let ((plaintext (crypto:decrypt-message ae output :associated-data ad)))
      (when (or (mismatch plaintext input)
                (mismatch (crypto:produce-tag ae) tag))
        (error "decryption failed for ~A, input ~A, output ~A" mode-name input output)))))

(defun aead-test/incremental (mode-name input ad output tag &rest args)
  (let* ((parameters (case mode-name
                       ((:gcm gcm crypto:gcm :eax eax crypto:eax)
                        (list :cipher-name (car args)
                              :key (cadr args)
                              :initialization-vector (caddr args)))
                       ((:etm etm crypto:etm)
                        (destructuring-bind (cipher-name ckey mode iv mac-name mkey mparam) args
                          (let ((cipher (crypto:make-cipher cipher-name
                                                            :key ckey
                                                            :mode mode
                                                            :initialization-vector iv))
                                (mac (if mparam
                                         (crypto:make-mac mac-name mkey mparam)
                                         (crypto:make-mac mac-name mkey))))
                            (list :cipher cipher :mac mac))))))
         (ae (apply #'crypto:make-authenticated-encryption-mode mode-name parameters))
         (plaintext (make-array (length input) :element-type '(unsigned-byte 8)))
         (ciphertext (make-array (length output) :element-type '(unsigned-byte 8))))
    (dotimes (i (length ad))
      (crypto:process-associated-data ae ad :start i :end (1+ i)))
    (dotimes (i (length input))
      (crypto:encrypt ae input ciphertext
                      :plaintext-start i :plaintext-end (1+ i)
                      :ciphertext-start i
                      :handle-final-block (= i (1- (length input)))))
    (when (or (mismatch ciphertext output)
              (mismatch (crypto:produce-tag ae) tag))
      (error "encryption failed for ~A, input ~A, output ~A" mode-name input output))
    (setf parameters (case mode-name
                       ((:gcm gcm crypto:gcm :eax eax crypto:eax)
                        parameters)
                       ((:etm etm crypto:etm)
                        (destructuring-bind (cipher-name ckey mode iv mac-name mkey mparam) args
                          (let ((cipher (crypto:make-cipher cipher-name
                                                            :key ckey
                                                            :mode mode
                                                            :initialization-vector iv))
                                (mac (if mparam
                                         (crypto:make-mac mac-name mkey mparam)
                                         (crypto:make-mac mac-name mkey))))
                            (list :cipher cipher :mac mac))))))
    (apply #'reinitialize-instance ae :tag tag parameters)
    (dotimes (i (length ad))
      (crypto:process-associated-data ae ad :start i :end (1+ i)))
    (dotimes (i (length output))
      (crypto:decrypt ae output plaintext
                      :ciphertext-start i :ciphertext-end (1+ i)
                      :plaintext-start i
                      :handle-final-block (= i (1- (length output)))))
    (when (or (mismatch plaintext input)
              (mismatch (crypto:produce-tag ae) tag))
      (error "decryption failed for ~A, input ~A, output ~A" mode-name input output))))

(defparameter *authenticated-encryption-tests*
  (list (cons :aead-test 'aead-test)))

(defparameter *authenticated-encryption-incremental-tests*
  (list (cons :aead-test 'aead-test/incremental)))
