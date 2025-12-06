;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2013, Anton Vodonosov.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defpackage :drakma-test
  (:use :cl :fiveam))

(in-package :drakma-test)

(def-suite :drakma)
(in-suite :drakma)

(test get-google
  (let ((drakma:*header-stream* *standard-output*))
    (multiple-value-bind (body-or-stream status-code)
        (drakma:http-request "http://google.com/")
      (is (> (length body-or-stream) 0))
      (is (= 200 status-code)))))

(test get-google-ssl
  (let ((drakma:*header-stream* *standard-output*))
    (multiple-value-bind (body-or-stream status-code)
        (drakma:http-request "https://google.com/")
      (is (> (length body-or-stream) 0))
      (is (= 200 status-code)))))

(test post-google
  (let ((drakma:*header-stream* *standard-output*))
    (multiple-value-bind (body-or-stream status-code headers uri stream must-close reason-phrase)
        (drakma:http-request "http://google.com/" :method :post :parameters '(("a" . "b")))
      (declare (ignore headers uri stream must-close))
      (is (> (length body-or-stream) 0))
      (is (= 405 status-code))
      (is (string= "Method Not Allowed" reason-phrase)))))

(test post-google-ssl
  (let ((drakma:*header-stream* *standard-output*))
    (multiple-value-bind (body-or-stream status-code headers uri stream must-close reason-phrase)
        (drakma:http-request "https://google.com/" :method :post :parameters '(("a" . "b")))
      (declare (ignore headers uri stream must-close))
      (is (> (length body-or-stream) 0))
      (is (= 405 status-code))
      (is (string= "Method Not Allowed" reason-phrase)))))


(test gzip-content
  (let ((drakma:*header-stream* *standard-output*)
        (drakma:*text-content-types* (cons '(nil . "json") drakma:*text-content-types*)))
    (multiple-value-bind (body-or-stream status-code)
        (drakma:http-request "http://httpbin.org/gzip" :decode-content t)
      (is (= 200 status-code))
      (is (typep body-or-stream 'string))
      (is (search "\"gzipped\": true" body-or-stream)))))

(test deflate-content
  (let ((drakma:*header-stream* *standard-output*)
        (drakma:*text-content-types* (cons '(nil . "json") drakma:*text-content-types*)))
    (multiple-value-bind (body-or-stream status-code)
        (drakma:http-request "http://httpbin.org/deflate" :decode-content t)
      (is (= 200 status-code))
      (is (typep body-or-stream 'string))
      (is (search "\"deflated\": true" body-or-stream)))))

(test gzip-content-undecoded
      (let ((drakma:*header-stream* *standard-output*))
        (multiple-value-bind (body-or-stream status-code)
            (drakma:http-request "http://httpbin.org/gzip")
          (is (= 200 status-code))
          (is (typep body-or-stream '(vector flexi-streams:octet)))
          (is (> (length body-or-stream) 0))
          (is (equalp #(#x1f #x8b)
                      (subseq body-or-stream 0 2))))))

(test deflate-content-undecoded
      (let ((drakma:*header-stream* *standard-output*))
        (multiple-value-bind (body-or-stream status-code)
            (drakma:http-request "http://httpbin.org/deflate")
          (is (= 200 status-code))
          (is (typep body-or-stream '(vector flexi-streams:octet)))
          (is (> (length body-or-stream) 0))
          (is (equalp #x78 (aref body-or-stream 0))))))

(test stream
  (multiple-value-bind (stream status-code)
      (drakma:http-request "http://google.com/" :want-stream t)
    (is (streamp stream))
    (is (= 200 status-code))
    (is (subtypep (stream-element-type stream) 'character))
    (let ((buffer (make-string 1)))
      (read-sequence buffer stream))))

(test force-binary
  (multiple-value-bind (stream status-code)
      (drakma:http-request "http://google.com/" :want-stream t :force-binary t)
    (is (streamp stream))
    (is (= 200 status-code))
    (is (subtypep (stream-element-type stream) 'flexi-streams:octet))
    (let ((buffer (make-array 1 :element-type 'flexi-streams:octet)))
      (read-sequence buffer stream))))

(test verify.wrong.host
  (signals error
    (drakma:http-request "https://wrong.host.badssl.com/" :verify :required))
  (signals error
    (drakma:http-request "https://wrong.host.badssl.com/" :verify :optional))
  (finishes
    (drakma:http-request "https://wrong.host.badssl.com//" :verify nil)))

(test verify.expired
  (signals error
    (drakma:http-request "https://expired.badssl.com//" :verify :required))
  (signals error
    (drakma:http-request "https://expired.badssl.com/" :verify :optional))
  (finishes
    (drakma:http-request "https://expired.badssl.com/" :verify nil)))

(test verify.self-signed
  (signals error
    (drakma:http-request "https://self-signed.badssl.com/" :verify :required)))

(test verify.untrusted-root
  (signals error
    (drakma:http-request "https://untrusted-root.badssl.com/" :verify :required)))
