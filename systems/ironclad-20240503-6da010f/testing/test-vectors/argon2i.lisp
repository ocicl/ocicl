;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
(in-package :crypto-tests)


(rtest:deftest argon2i-1
    (run-kdf-test (crypto:make-kdf 'crypto:argon2i
                                   :block-count 12
                                   :additional-key (ironclad:hex-string-to-byte-array "0303030303030303")
                                   :additional-data (ironclad:hex-string-to-byte-array "040404040404040404040404"))
                  (ironclad:hex-string-to-byte-array "0101010101010101010101010101010101010101010101010101010101010101")
                  (ironclad:hex-string-to-byte-array "02020202020202020202020202020202")
                  3
                  32
                  (ironclad:hex-string-to-byte-array "25b608be8ddbe53bb027bfd0f2a5c827e819b6fee0c28eca883f27d724ee7a3a"))
  t)

(rtest:deftest argon2i-2
    (run-kdf-test (crypto:make-kdf 'crypto:argon2i
                                   :block-count 133
                                   :additional-key (ironclad:hex-string-to-byte-array "0303030303030303")
                                   :additional-data (ironclad:hex-string-to-byte-array "040404040404040404040404"))
                  (ironclad:hex-string-to-byte-array "0101010101010101010101010101010101010101010101010101010101010101")
                  (ironclad:hex-string-to-byte-array "02020202020202020202020202020202")
                  1
                  32
                  (ironclad:hex-string-to-byte-array "6d3976937b9750ea8db2ad0a45e9da6967377a49d1a5fddfdee74cbf63eaf181"))
  t)

(rtest:deftest argon2i-3
    (run-kdf-test (crypto:make-kdf 'crypto:argon2i
                                   :block-count 8
                                   :additional-key (ironclad:hex-string-to-byte-array "0303030303030303")
                                   :additional-data (ironclad:hex-string-to-byte-array "040404040404040404040404"))
                  (ironclad:hex-string-to-byte-array "0101010101010101010101010101010101010101010101010101010101010101")
                  (ironclad:hex-string-to-byte-array "02020202020202020202020202020202")
                  3
                  32
                  (ironclad:hex-string-to-byte-array "afe519be3ab0e92375df221dfb17347080c7000b1be85f9ee39978bf11e7cc3a"))
  t)

(rtest:deftest argon2i-4
    (run-kdf-test (crypto:make-kdf 'crypto:argon2i
                                   :block-count 4096)
                  (ironclad:hex-string-to-byte-array "70617373776f7264")
                  (ironclad:hex-string-to-byte-array "73616c7473616c74")
                  3
                  32
                  (ironclad:hex-string-to-byte-array "965bd476aa7af72d9107adbd742b86e36911e72f8e71cff388a579927deb48e3"))
  t)
