;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; ironclad.lisp -- tests for non-cryptography functionality

(in-package :crypto-tests)

(rtest:deftest quotationp.1
  (crypto::quotationp '(quote foo))
  t)

(rtest:deftest quotationp.2
  (crypto::quotationp '(quote foo bar))
  nil)

(rtest:deftest unquote.1
  (crypto::unquote (quote foo))
  foo)

(rtest:deftest unquote.2
  (crypto::unquote 2)
  2)

(rtest:deftest unquote.3
  (crypto::unquote '#1=(list 'foo 'bar))
  #1#)
