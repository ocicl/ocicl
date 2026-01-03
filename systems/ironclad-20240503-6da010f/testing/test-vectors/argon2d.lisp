;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
(in-package :crypto-tests)


(rtest:deftest argon2d-1
    (run-kdf-test (crypto:make-kdf 'crypto:argon2d :block-count 12)
                  (crypto:ascii-string-to-byte-array "somepassword")
                  (crypto:ascii-string-to-byte-array "somesalt")
                  3
                  32
                  (ironclad:hex-string-to-byte-array "4b2506d85e002568380d0f4332b39fd0e6d17a36ceb8ea6cda4b42328715316a"))
  t)

(rtest:deftest argon2d-2
    (run-kdf-test (crypto:make-kdf 'crypto:argon2d :block-count 32)
                  (crypto:ascii-string-to-byte-array "0123456789abcdefgh")
                  (crypto:ascii-string-to-byte-array "0123456789")
                  3
                  32
                  (ironclad:hex-string-to-byte-array "74093b9772cc719dccf296bcdafc6d198beeb3b4ccb103f275489581497774de"))
  t)

(rtest:deftest argon2d-3
    (run-kdf-test (crypto:make-kdf 'crypto:argon2d :block-count 64)
                  (crypto:ascii-string-to-byte-array "0000000000000000")
                  (crypto:ascii-string-to-byte-array "00000000")
                  4
                  32
                  (ironclad:hex-string-to-byte-array "11180186e2608884c32539561128f6870f077319dfa29316ea4c065c815d0637"))
  t)

(rtest:deftest argon2d-4
    (run-kdf-test (crypto:make-kdf 'crypto:argon2d :block-count 128)
                  (crypto:ascii-string-to-byte-array "zzzzzzzzyyyyyyyyxxxxx")
                  (crypto:ascii-string-to-byte-array "wwwwwwwwvvvvv")
                  3
                  111
                  (ironclad:hex-string-to-byte-array "b98e59af3b82d241ee76f1a21262e8be0adcf9a673cbee7ff2b2e61ed938b2f2d709c925e067ef61f94b00478f91e9c773e79e66263ac6b8935e81afae94f44e1bb9daeae34e732e6be82438900ba1a865c159e16de16df2a738f00fdf1b4cf5e5c8b7a79703471c52b48152b2d55c"))
  t)
