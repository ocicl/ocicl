;;;; -*- mode: lisp; indent-tabs-mode: nil -*-

(cl:defpackage #:ironclad-system
  (:use :cl :asdf))

(cl:in-package #:ironclad-system)

(defclass ironclad-source-file (cl-source-file) ())

(defclass ironclad-system (system)
  ()
  (:default-initargs
   :version "0.59"
   :author "Nathan Froyd <froydnj@gmail.com>"
   :maintainer "Guillaume LE VAILLANT <glv@posteo.net>"
   :description "A cryptographic toolkit written in pure Common Lisp"
   :license "BSD 3-Clause"
   :default-component-class 'ironclad-source-file))

(defmacro define-ironclad-subsystems (aggregate-system kind path &body components)
  (let ((subsystems (loop for spec in (mapcar #'uiop:ensure-list components)
                          collect (format nil "ironclad/~a/~a" kind (first spec)))))
    `(progn
       ,@(loop for (component . options) in (mapcar #'uiop:ensure-list components)
               for subsystem in subsystems
               collect `(defsystem ,subsystem
                          :class ironclad-system
                          :description ,(format nil "Ironclad ~a: ~a" kind component)
                          :depends-on ,(cons "ironclad/core" (getf options :depends-on))
                          :pathname ,path
                          :serial t
                          :components ,(or (getf options :components)
                                           `((:file ,component)))))
       (defsystem ,aggregate-system
         :class ironclad-system
         :depends-on ("ironclad/core" ,@subsystems)))))

(defsystem "ironclad/core"
  :class ironclad-system
  :depends-on (#+sbcl "sb-rotate-byte" #+sbcl "sb-posix" "bordeaux-threads")
  :serial t
  :components ((:static-file "LICENSE")
               (:static-file "NEWS")
               (:static-file "README.org")
               (:static-file "TODO")
               (:module "doc"
                :components ((:html-file "ironclad")))
               (:module "src"
                :serial t
                :components ((:file "package")
                             (:file "conditions")
                             (:file "generic")
                             (:file "macro-utils")
                             (:file "util")
                             (:module "opt"
                              :serial t
                              :components ((:module "ccl"
                                            :if-feature :ccl
                                            :components ((:file "x86oid-vm")))
                                           (:module "ecl"
                                            :if-feature :ecl
                                            :components ((:file "c-functions")))
                                           (:module "sbcl"
                                            :if-feature :sbcl
                                            :serial t
                                            :components ((:file "fndb")
                                                         (:file "x86oid-vm")
                                                         (:file "cpu-features")))))
                             (:file "common")
                             (:module "ciphers"
                              :serial t
                              :components ((:file "cipher")
                                           (:file "padding")
                                           (:file "make-cipher")
                                           (:file "modes")))
                             (:module "digests"
                              :serial t
                              :components ((:file "digest")))
                             (:module "macs"
                              :serial t
                              :components ((:file "mac")))
                             (:module "prng"
                              :serial t
                              :components ((:file "prng")
                                           (:file "os-prng")))
                             (:file "math")
                             #+(or lispworks sbcl openmcl cmu allegro abcl ecl clisp)
                             (:file "octet-stream")
                             (:module "aead"
                              :serial t
                              :components ((:file "aead")))
                             (:module "kdf"
                              :serial t
                              :components ((:file "kdf")))
                             (:module "public-key"
                              :serial t
                              :components ((:file "public-key")
                                           (:file "pkcs1")
                                           (:file "elliptic-curve")))))))

(define-ironclad-subsystems "ironclad/ciphers" "cipher" #p"src/ciphers/"
  "aes"
  "arcfour"
  "aria"
  "blowfish"
  "camellia"
  "cast5"
  "chacha"
  ("xchacha" :depends-on ("ironclad/cipher/chacha"))
  "des"
  "idea"
  "kalyna"
  ("keystream" :depends-on ("ironclad/cipher/chacha"
                            "ironclad/cipher/salsa20"))
  "kuznyechik"
  "misty1"
  "rc2"
  "rc5"
  "rc6"
  "salsa20"
  ("xsalsa20" :depends-on ("ironclad/cipher/salsa20"))
  "seed"
  "serpent"
  "sm4"
  "sosemanuk"
  "square"
  "tea"
  "threefish"
  "twofish"
  "xor"
  "xtea")

(define-ironclad-subsystems "ironclad/digests" "digest" #p"src/digests/"
  "adler32"
  "blake2"
  "blake2s"
  "crc24"
  "crc32"
  "groestl"
  "jh"
  ("kupyna" :depends-on ("ironclad/cipher/kalyna"))
  "md2"
  "md4"
  ("md5" :components ((:file "md5")
                      (:file "md5-lispworks-int32")))
  "ripemd-128"
  "ripemd-160"
  "sha1"
  "sha256"
  "sha3"
  "sha512"
  ("skein" :depends-on ("ironclad/cipher/threefish"))
  "sm3"
  "streebog"
  "tiger"
  ("tree-hash" :depends-on ("ironclad/digest/tiger"))
  "whirlpool")

(define-ironclad-subsystems "ironclad/macs" "mac" #p"src/macs/"
  ("blake2-mac" :depends-on ("ironclad/digest/blake2"))
  ("blake2s-mac" :depends-on ("ironclad/digest/blake2s"))
  "cmac"
  "hmac"
  "gmac"
  "poly1305"
  "siphash"
  ("skein-mac" :depends-on ("ironclad/cipher/threefish"
                            "ironclad/digest/skein")))

(define-ironclad-subsystems "ironclad/prngs" "prng" #p"src/prng/"
  ("fortuna" :depends-on ("ironclad/cipher/aes"
                          "ironclad/digest/sha256")
             :components ((:file "generator")
                          (:file "fortuna"))))

(define-ironclad-subsystems "ironclad/aeads" "aead" #p"src/aead/"
  ("eax" :depends-on ("ironclad/mac/cmac"))
  "etm"
  ("gcm" :depends-on ("ironclad/mac/gmac")))

(define-ironclad-subsystems "ironclad/kdfs" "kdf" #p"src/kdf/"
  ("argon2" :depends-on ("ironclad/mac/blake2-mac"))
  ("bcrypt" :depends-on ("ironclad/cipher/blowfish"
                         "ironclad/digest/sha512"))
  ("hmac" :depends-on ("ironclad/mac/hmac"))
  "pkcs5"
  ("password-hash" :depends-on ("ironclad/digest/sha256"))
  ("scrypt" :depends-on ("ironclad/cipher/salsa20"
                         "ironclad/digest/sha256"
                         "ironclad/kdf/pkcs5")))

(define-ironclad-subsystems "ironclad/public-keys" "public-key" #p"src/public-key/"
  "dsa"
  "rsa"
  "elgamal"
  "curve25519"
  "curve448"
  ("ed25519" :depends-on ("ironclad/digest/sha512"))
  ("ed448" :depends-on ("ironclad/digest/sha3"))
  "secp256k1"
  "secp256r1"
  "secp384r1"
  "secp521r1")

(defsystem "ironclad"
  :class ironclad-system
  :in-order-to ((test-op (test-op "ironclad/tests")))
  :depends-on ("ironclad/core"
               "ironclad/ciphers"
               "ironclad/digests"
               "ironclad/macs"
               "ironclad/prngs"
               "ironclad/aeads"
               "ironclad/kdfs"
               "ironclad/public-keys"))

(macrolet ((do-silently (&body body)
             `(handler-bind ((style-warning #'muffle-warning)
                             ;; It's about as fast as we can make it,
                             ;; and a number of the notes relate to code
                             ;; that we're running at compile time,
                             ;; which we don't care about the speed of
                             ;; anyway...
                             #+sbcl (sb-ext:compiler-note #'muffle-warning))
                ,@body)))
  (defmethod perform :around ((op compile-op) (c ironclad-source-file))
    (let ((*print-base* 10)               ; INTERN'ing FORMAT'd symbols
          (*print-case* :upcase)
          #+sbcl (sb-ext:*inline-expansion-limit* (max sb-ext:*inline-expansion-limit* 1000))
          #+cmu (ext:*inline-expansion-limit* (max ext:*inline-expansion-limit* 1000)))
      (do-silently (call-next-method))))

  (defmethod perform :around ((op load-op) (c ironclad-source-file))
    (do-silently (call-next-method))))

(defmethod perform :after ((op load-op) (c (eql (find-system "ironclad"))))
  (provide :ironclad))


;;; testing

(defclass test-vector-file (static-file)
  ((type :initform "testvec")))

(defpackage :ironclad-tests
  (:nicknames :crypto-tests)
  (:use :cl))

(defsystem "ironclad/tests"
  :depends-on ("ironclad" "rt")
  :version "0.59"
  :in-order-to ((test-op (load-op "ironclad/tests")))
  :perform (test-op (o s)
             (or (funcall (intern "DO-TESTS" (find-package "RTEST")))
                 (error "TEST-OP failed for IRONCLAD/TESTS")))
  :components ((:module "testing"
                :components ((:file "testfuns")
                             (:module "test-vectors"
                              :depends-on ("testfuns")
                              :components ((:file "ironclad")
                                           (:file "padding")
                                           ;; aead
                                           (:file "aead")
                                           (:test-vector-file "eax")
                                           (:test-vector-file "etm")
                                           (:test-vector-file "gcm")
                                           ;; ciphers
                                           (:file "ciphers")
                                           (:file "modes")
                                           (:test-vector-file "3des")
                                           (:test-vector-file "aes")
                                           (:test-vector-file "arcfour")
                                           (:test-vector-file "aria")
                                           (:test-vector-file "blowfish")
                                           (:test-vector-file "camellia")
                                           (:test-vector-file "cast5")
                                           (:test-vector-file "cbc")
                                           (:test-vector-file "cfb")
                                           (:test-vector-file "cfb8")
                                           (:test-vector-file "chacha")
                                           (:test-vector-file "chacha-12")
                                           (:test-vector-file "chacha-8")
                                           (:test-vector-file "ctr")
                                           (:test-vector-file "des")
                                           (:test-vector-file "idea")
                                           (:test-vector-file "kalyna128")
                                           (:test-vector-file "kalyna256")
                                           (:test-vector-file "kalyna512")
                                           (:test-vector-file "kuznyechik")
                                           (:test-vector-file "misty1")
                                           (:test-vector-file "xor")
                                           (:test-vector-file "ofb")
                                           (:test-vector-file "rc2")
                                           (:test-vector-file "rc5")
                                           (:test-vector-file "rc6")
                                           (:test-vector-file "salsa20")
                                           (:test-vector-file "salsa20-12")
                                           (:test-vector-file "salsa20-8")
                                           (:test-vector-file "seed")
                                           (:test-vector-file "serpent")
                                           (:test-vector-file "sm4")
                                           (:test-vector-file "sosemanuk")
                                           (:test-vector-file "square")
                                           (:test-vector-file "tea")
                                           (:test-vector-file "threefish1024")
                                           (:test-vector-file "threefish256")
                                           (:test-vector-file "threefish512")
                                           (:test-vector-file "twofish")
                                           (:test-vector-file "xchacha")
                                           (:test-vector-file "xchacha-12")
                                           (:test-vector-file "xchacha-8")
                                           (:test-vector-file "xsalsa20")
                                           (:test-vector-file "xsalsa20-12")
                                           (:test-vector-file "xsalsa20-8")
                                           (:test-vector-file "xtea")
                                           ;; digests
                                           (:file "digests")
                                           (:test-vector-file "adler32")
                                           (:test-vector-file "blake2")
                                           (:test-vector-file "blake2-160")
                                           (:test-vector-file "blake2-256")
                                           (:test-vector-file "blake2-384")
                                           (:test-vector-file "blake2s")
                                           (:test-vector-file "blake2s-128")
                                           (:test-vector-file "blake2s-160")
                                           (:test-vector-file "blake2s-224")
                                           (:test-vector-file "crc24")
                                           (:test-vector-file "crc32")
                                           (:test-vector-file "groestl")
                                           (:test-vector-file "groestl-224")
                                           (:test-vector-file "groestl-256")
                                           (:test-vector-file "groestl-384")
                                           (:test-vector-file "jh")
                                           (:test-vector-file "jh-224")
                                           (:test-vector-file "jh-256")
                                           (:test-vector-file "jh-384")
                                           (:test-vector-file "keccak")
                                           (:test-vector-file "keccak-224")
                                           (:test-vector-file "keccak-256")
                                           (:test-vector-file "keccak-384")
                                           (:test-vector-file "kupyna")
                                           (:test-vector-file "kupyna-256")
                                           (:test-vector-file "md2")
                                           (:test-vector-file "md4")
                                           (:test-vector-file "md5")
                                           (:test-vector-file "ripemd-128")
                                           (:test-vector-file "ripemd-160")
                                           (:test-vector-file "sha1")
                                           (:test-vector-file "sha224")
                                           (:test-vector-file "sha256")
                                           (:test-vector-file "sha3")
                                           (:test-vector-file "sha3-224")
                                           (:test-vector-file "sha3-256")
                                           (:test-vector-file "sha3-384")
                                           (:test-vector-file "sha384")
                                           (:test-vector-file "sha512")
                                           (:test-vector-file "shake128")
                                           (:test-vector-file "shake256")
                                           (:test-vector-file "skein1024")
                                           (:test-vector-file "skein1024-384")
                                           (:test-vector-file "skein1024-512")
                                           (:test-vector-file "skein256")
                                           (:test-vector-file "skein256-128")
                                           (:test-vector-file "skein256-160")
                                           (:test-vector-file "skein256-224")
                                           (:test-vector-file "skein512")
                                           (:test-vector-file "skein512-128")
                                           (:test-vector-file "skein512-160")
                                           (:test-vector-file "skein512-224")
                                           (:test-vector-file "skein512-256")
                                           (:test-vector-file "skein512-384")
                                           (:test-vector-file "sm3")
                                           (:test-vector-file "streebog")
                                           (:test-vector-file "streebog-256")
                                           (:test-vector-file "tiger")
                                           (:test-vector-file "tree-hash")
                                           (:test-vector-file "whirlpool")
                                           ;; kdf
                                           (:file "pkcs5")
                                           (:file "argon2d")
                                           (:file "argon2i")
                                           (:file "bcrypt")
                                           (:file "scrypt")
                                           (:file "hmac-kdf")
                                           ;; macs
                                           (:file "macs")
                                           (:test-vector-file "blake2-mac")
                                           (:test-vector-file "blake2s-mac")
                                           (:test-vector-file "cmac")
                                           (:test-vector-file "hmac")
                                           (:test-vector-file "gmac")
                                           (:test-vector-file "poly1305")
                                           (:test-vector-file "siphash")
                                           (:test-vector-file "skein-mac")
                                           ;; prng
                                           (:file "prng-tests")
                                           (:test-vector-file "prng")
                                           ;; public key
                                           (:file "public-key")
                                           (:test-vector-file "curve25519")
                                           (:test-vector-file "curve448")
                                           (:test-vector-file "dsa")
                                           (:test-vector-file "ed25519")
                                           (:test-vector-file "ed448")
                                           (:test-vector-file "elgamal-dh")
                                           (:test-vector-file "elgamal-enc")
                                           (:test-vector-file "elgamal-sig")
                                           (:test-vector-file "rsa-enc")
                                           (:test-vector-file "rsa-sig")
                                           (:test-vector-file "secp256k1-dh")
                                           (:test-vector-file "secp256k1-sig")
                                           (:test-vector-file "secp256r1-dh")
                                           (:test-vector-file "secp256r1-sig")
                                           (:test-vector-file "secp384r1-dh")
                                           (:test-vector-file "secp384r1-sig")
                                           (:test-vector-file "secp521r1-dh")
                                           (:test-vector-file "secp521r1-sig")))))))
