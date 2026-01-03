;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
(in-package :crypto-tests)

;;; Test vectors for "bare" PBKDF* are impossible to find; RSA has some,
;;; but they're tied up with other PKCS bits, making the tests a wee bit
;;; more complicated than I'd like.  The ones used here are from:
;;;
;;; http://www.di-mgt.com.au/cryptoKDFs.html

(defvar *password*
  (coerce #(#x70 #x61 #x73 #x73 #x77 #x6F #x72 #x64)
          '(vector (unsigned-byte 8))))
(defvar *salt*
  (coerce #(#x78 #x57 #x8E #x5A #x5D #x63 #xCB #x6)
          '(vector (unsigned-byte 8))))

(defun run-kdf-test (kdf password salt iteration-count key-length expected-key)
  (let ((key (ironclad:derive-key kdf password salt iteration-count key-length)))
    (not (mismatch key expected-key))))

(defvar *pbkdf1-key*
  (coerce #(#xDC #x19 #x84 #x7E #x5 #xC6 #x4D #x2F
            #xAF #x10 #xEB #xFB #x4A #x3D #x2A #x20)
          '(vector (unsigned-byte 8))))

(rtest:deftest pbkdf1
    (run-kdf-test (crypto:make-kdf 'crypto:pbkdf1 :digest 'ironclad:sha1)
                  *password* *salt* 1000 16 *pbkdf1-key*)
  t)

(rtest:deftest pbkdf1.valid-hashes
    (loop with valid-hashes = '(:md2 :md5 :sha1)
       for hash in (crypto:list-all-digests)
       when (handler-case (crypto:make-kdf 'crypto:pbkdf1 :digest hash)
              (error () nil)
              (:no-error (kdf)
                (declare (ignore kdf))
                t))
       collect hash into candidates
       finally (return (set-difference candidates valid-hashes)))
  nil)

(defvar *pbkdf2-key*
  (coerce #(#xBF #xDE #x6B #xE9 #x4D #xF7 #xE1 #x1D
            #xD4 #x9 #xBC #xE2 #xA #x2 #x55 #xEC
            #x32 #x7C #xB9 #x36 #xFF #xE9 #x36 #x43)
          '(vector (unsigned-byte 8))))

(rtest:deftest pbkdf2
    (run-kdf-test (crypto:make-kdf 'crypto:pbkdf2 :digest 'ironclad:sha1)
                  *password* *salt* 2048 24 *pbkdf2-key*)
  t)

(rtest:deftest pbkdf2-convenience
    (ironclad:pbkdf2-check-password
     *password*
     "PBKDF2$SHA256:1000$78578e5a5d63cb06$aa2ae650dc866dc4de4fc3c8f06eddac1abc3011a99402fbc46d7e131fac06d5")
  t)

(rtest:deftest unsupported-kdf
  (handler-case
      (crypto:make-kdf :random-name)
    (crypto:unsupported-kdf () :ok)
    (:no-error () :error))
  :ok)

(rtest:deftest pbkdf1-invalid-iteration-count
  (handler-case
      (ironclad:derive-key (ironclad:make-kdf 'ironclad:pbkdf1 :digest 'ironclad:sha1)
                           *password* *salt* -1 24)
    (type-error () :ok)
    (:no-error () :error))
  :ok)

(rtest:deftest pbkdf1-invalid-key-length
  (handler-case
      (ironclad:derive-key (ironclad:make-kdf 'ironclad:pbkdf1 :digest 'ironclad:sha1)
                           *password* *salt* 2048 -1)
    (type-error () :ok)
    (:no-error () :error))
  :ok)

(rtest:deftest pbkdf2-invalid-iteration-count
  (handler-case
      (ironclad:derive-key (ironclad:make-kdf 'ironclad:pbkdf2 :digest 'ironclad:sha1)
                           *password* *salt* -1 24)
    (type-error () :ok)
    (:no-error () :error))
  :ok)

(rtest:deftest pbkdf2-invalid-key-length
  (handler-case
      (ironclad:derive-key (ironclad:make-kdf 'ironclad:pbkdf2 :digest 'ironclad:sha1)
                           *password* *salt* 2048 -1)
    (type-error () :ok)
    (:no-error () :error))
  :ok)

