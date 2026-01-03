;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
(in-package :crypto-tests)

(rtest:deftest verify-key.bad-cipher
  (handler-case (crypto::verify-key :error
                                    (make-array 0
                                                :element-type '(unsigned-byte 8)))
    (crypto:unsupported-cipher () :ok)
    (:no-error () :error))
  :ok)

(rtest:deftest verify-key.bad-key0
  (handler-case (crypto::verify-key :aes "")
    (type-error () :ok)
    (:no-error () :error))
  :ok)

(rtest:deftest verify-key.bad-key1
  (handler-case (crypto::verify-key :aes nil)
    (crypto:key-not-supplied () :ok)
    (:no-error () :error))
  :ok)

(rtest:deftest unprovided-key
  (handler-case
      (crypto:make-cipher :blowfish :mode :ecb
                          :initialization-vector (make-array 8 :element-type '(unsigned-byte 8)))
    (crypto:key-not-supplied () :ok)
    (:no-error () :error))
  :ok)

(rtest:deftest unsupported-mode.1
  (handler-case
      (crypto:make-cipher :blowfish :mode :stream
                          :key (make-array 8 :element-type '(unsigned-byte 8))
                          :initialization-vector (make-array 8 :element-type '(unsigned-byte 8)))
    (crypto:unsupported-mode () :ok)
    (:no-error () :error))
  :ok)

(rtest:deftest unsupported-mode.2
  (handler-case
      (crypto:make-cipher :salsa20 :mode :cbc
                          :key (make-array 16 :element-type '(unsigned-byte 8)))
    (crypto:unsupported-mode () :ok)
    (:no-error () :error))
  :ok)

(rtest:deftest block-length.known-ciphers
  (dolist (name (crypto:list-all-ciphers) :ok)
    (unless (crypto:block-length name)
      (return :error)))
  :ok)

(rtest:deftest block-length.bad-cipher
  (crypto:block-length :error)
  nil)

(rtest:deftest key-lengths.known-ciphers
  (dolist (name (crypto:list-all-ciphers) :ok)
    (unless (crypto:key-lengths name)
      (return :error)))
  :ok)

(rtest:deftest key-lengths.bad-cipher
  (crypto:key-lengths :error)
  nil)

#.(loop for cipher in (crypto:list-all-ciphers)
        collect `(rtest:deftest ,cipher
                   (run-test-vector-file ',cipher *cipher-tests*) t) into forms
        finally (return `(progn ,@forms)))

#.(if (boundp '*cipher-stream-tests*)
      (loop for cipher in (crypto:list-all-ciphers)
            collect `(rtest:deftest ,(crypto::symbolicate cipher '#:/stream)
                       (run-test-vector-file ',cipher *cipher-stream-tests*) t)
              into forms
         finally (return `(progn ,@forms)))
      nil)

(rtest:deftest ciphers.crypto-package
  (every #'(lambda (s)
             (eq (nth-value 1 (find-symbol (symbol-name s)
                                           (find-package :ironclad)))
                 :external))
         (crypto:list-all-ciphers))
  t)

(rtest:deftest clean-symbols.ciphers
    (loop with n-ciphers = (length (crypto:list-all-ciphers))
     for s being each symbol of :crypto
     when (crypto::%find-cipher s)
     count s into computed-n-ciphers
     finally (return (= n-ciphers computed-n-ciphers)))
  t)
