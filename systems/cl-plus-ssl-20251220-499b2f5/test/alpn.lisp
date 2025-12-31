;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;
;;; Copyright (C) contributors as per cl+ssl git history
;;;
;;; See LICENSE for details.

(in-package :cl+ssl.test)

(def-suite :cl+ssl.alpn :in :cl+ssl
  :description "ALPN tests")

(in-suite :cl+ssl.alpn)

(test alpn-client
  (if (not (uiop:getenv "TEST_ALPN"))

      (skip "Skipping ALPN tests because the example.com server
now has some request throttling and our test suite sometimes
fails with timeout when repeated many times for different lisps
and openssl versions in Github Actions.
(https://github.com/cl-plus-ssl/cl-plus-ssl/issues/194)

Set environment variable TEST_ALPN to enable it.")

      (when (cl+ssl::openssl-is-at-least 1 0 2)
        (flet ((test-alpn-result (target proposed expected)
                 (usocket:with-client-socket (socket stream target 443
                                                     :element-type '(unsigned-byte 8))
                   (is
                    (equal expected
                           (cl+ssl:get-selected-alpn-protocol
                            (cl+ssl:make-ssl-client-stream stream
                                                           :hostname target
                                                           :alpn-protocols proposed)))))))
          (test-alpn-result "example.com" nil nil)
          (test-alpn-result "example.com" '( "should-not-exist" "h2" "also-should-not-exist")
                            "h2")))))
