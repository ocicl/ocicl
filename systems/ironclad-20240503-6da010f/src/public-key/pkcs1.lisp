;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; pkcs1.lisp -- implementation of OAEP and PSS schemes

(in-package :crypto)


;;; Mask generation function
(defun mgf (digest-name seed num-bytes)
  "Expand the SEED to a NUM-BYTES bytes vector using the DIGEST-NAME digest."
  (loop
     with result = #()
     with digest-len = (digest-length digest-name)
     for digest = (make-digest digest-name) then (reinitialize-instance digest)
     for counter from 0 to (floor num-bytes digest-len)
     for counter-bytes = (integer-to-octets counter :n-bits 32)
     for tmp = (digest-sequence digest (concatenate '(vector (unsigned-byte 8))
                                                    seed
                                                    counter-bytes))
     do (setf result (concatenate '(vector (unsigned-byte 8)) result tmp))
     finally (return (subseq result 0 num-bytes))))

(declaim (notinline oaep-encode))
;; In the tests, this function is redefined to use a constant value
;; instead of a random one. Therefore it must not be inlined or the tests
;; will fail.
(defun oaep-encode (digest-name message num-bytes &optional label)
  "Return a NUM-BYTES bytes vector containing the OAEP encoding of the MESSAGE
using the DIGEST-NAME digest (and the optional LABEL octet vector)."
  (let* ((digest-name (if (eq digest-name t) :sha1 digest-name))
         (digest-len (digest-length digest-name)))
    (assert (<= (length message) (- num-bytes (* 2 digest-len) 2)))
    (let* ((digest (make-digest digest-name))
           (label (or label (coerce #() '(vector (unsigned-byte 8)))))
           (padding-len (- num-bytes (length message) (* 2 digest-len) 2))
           (padding (make-array padding-len :element-type '(unsigned-byte 8) :initial-element 0))
           (l-hash (digest-sequence digest label))
           (db (concatenate '(vector (unsigned-byte 8)) l-hash padding #(1) message))
           (seed (random-data digest-len))
           (db-mask (mgf digest-name seed (- num-bytes digest-len 1)))
           (masked-db (map '(vector (unsigned-byte 8)) #'logxor db db-mask))
           (seed-mask (mgf digest-name masked-db digest-len))
           (masked-seed (map '(vector (unsigned-byte 8)) #'logxor seed seed-mask)))
      (concatenate '(vector (unsigned-byte 8)) #(0) masked-seed masked-db))))

(defun oaep-decode (digest-name message &optional label)
  "Return an octet vector containing the data that was encoded in the MESSAGE with OAEP
using the DIGEST-NAME digest (and the optional LABEL octet vector)."
  (let* ((digest-name (if (eq digest-name t) :sha1 digest-name))
         (digest-len (digest-length digest-name)))
    (assert (>= (length message) (+ (* 2 digest-len) 2)))
    (let* ((digest (make-digest digest-name))
           (label (or label (coerce #() '(vector (unsigned-byte 8)))))
           (zero-byte (elt message 0))
           (masked-seed (subseq message 1 (1+ digest-len)))
           (masked-db (subseq message (1+ digest-len)))
           (seed-mask (mgf digest-name masked-db digest-len))
           (seed (map '(vector (unsigned-byte 8)) #'logxor masked-seed seed-mask))
           (db-mask (mgf digest-name seed (- (length message) digest-len 1)))
           (db (map '(vector (unsigned-byte 8)) #'logxor masked-db db-mask))
           (l-hash1 (digest-sequence digest label))
           (l-hash2 (subseq db 0 digest-len))
           (padding-len (loop
                           for i from digest-len below (length db)
                           while (zerop (elt db i))
                           finally (return (- i digest-len))))
           (one-byte (elt db (+ digest-len padding-len))))
      (unless (and (zerop zero-byte) (= 1 one-byte) (equalp l-hash1 l-hash2))
        (error 'oaep-decoding-error))
      (subseq db (+ digest-len padding-len 1)))))

(declaim (notinline pss-encode))
;; In the tests, this function is redefined to use a constant value
;; instead of a random one. Therefore it must not be inlined or the tests
;; will fail.
(defun pss-encode (digest-name message num-bytes)
  (let* ((digest-name (if (eq digest-name t) :sha1 digest-name))
         (digest-len (digest-length digest-name)))
    (assert (>= num-bytes (+ (* 2 digest-len) 2)))
    (let* ((m-hash (digest-sequence digest-name message))
           (salt (random-data digest-len))
           (m1 (concatenate '(vector (unsigned-byte 8)) #(0 0 0 0 0 0 0 0) m-hash salt))
           (h (digest-sequence digest-name m1))
           (ps (make-array (- num-bytes (* 2 digest-len) 2)
                           :element-type '(unsigned-byte 8)
                           :initial-element 0))
           (db (concatenate '(vector (unsigned-byte 8)) ps #(1) salt))
           (db-mask (mgf digest-name h (- num-bytes digest-len 1)))
           (masked-db (map '(vector (unsigned-byte 8)) #'logxor db db-mask)))
      (setf (ldb (byte 1 7) (elt masked-db 0)) 0)
      (concatenate '(vector (unsigned-byte 8)) masked-db h #(188)))))

(defun pss-verify (digest-name message encoded-message)
  (let* ((digest-name (if (eq digest-name t) :sha1 digest-name))
         (digest-len (digest-length digest-name))
         (em-len (length encoded-message)))
    (assert (>= em-len (+ (* 2 digest-len) 2)))
    (assert (= (elt encoded-message (- em-len 1)) 188))
    (let* ((m-hash (digest-sequence digest-name message))
           (masked-db (subseq encoded-message 0 (- em-len digest-len 1)))
           (h (subseq encoded-message (- em-len digest-len 1) (- em-len 1)))
           (db-mask (mgf digest-name h (- em-len digest-len 1)))
           (db (map '(vector (unsigned-byte 8)) #'logxor masked-db db-mask)))
      (setf (ldb (byte 1 7) (elt db 0)) 0)
      (let* ((ps (subseq db 0 (- em-len (* 2 digest-len) 2)))
             (one-byte (elt db (- em-len (* 2 digest-len) 2)))
             (salt (subseq db (- (length db) digest-len)))
             (m1 (concatenate '(vector (unsigned-byte 8)) #(0 0 0 0 0 0 0 0) m-hash salt))
             (h1 (digest-sequence digest-name m1)))
        (and (= 1 one-byte)
             (loop for i across ps always (zerop i))
             (equalp h h1))))))
