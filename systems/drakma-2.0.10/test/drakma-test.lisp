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

(defparameter *http-server* nil)

(def-fixture init-destroy-httpd ()
  (unwind-protect
       (progn
         (setf *http-server*
               (make-instance 'easy-routes:easy-routes-acceptor
                              :port 10456
                              :address "127.0.0.1"))
         (hunchentoot:start *http-server*)
         (&body))
    (progn
      (when *http-server*
        (hunchentoot:stop *http-server*)
        (setf *http-server* nil)))))

(test get-google
  (let ((drakma:*header-stream* *standard-output*))
    (multiple-value-bind (body-or-stream status-code)
        (drakma:http-request "http://google.com/")
      (is (> (length body-or-stream) 0))
      (is (= 200 status-code)))))

#-:drakma-no-chipz
(test get-google-gzip
  (let ((drakma:*header-stream* *standard-output*))
    (multiple-value-bind (body-or-stream status-code)
        (drakma:http-request "https://www.google.com/"
                             :additional-headers '(("Accept-Encoding" . "gzip"))
                             :decode-content t)
      (is (> (length body-or-stream) 0))
      (is (= 200 status-code)))))

#-:drakma-no-chipz
(test get-google-gzip-no-close
  (let ((drakma:*header-stream* *standard-output*))
    (multiple-value-bind (body-or-stream status-code headers uri stream must-close)
        (drakma:http-request "https://www.google.com/"
                             :additional-headers '(("Accept-Encoding" . "gzip"))
                             :decode-content t
                             :close nil)
      (declare (ignore headers uri))
      (unless must-close
        (close stream))
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

(test post-x-www-form
  (with-fixture init-destroy-httpd ()
    (let ((drakma:*header-stream* *standard-output*))
      (multiple-value-bind (body-or-stream status-code headers uri stream must-close)
          (drakma:http-request "http://127.0.0.1:10456/x-www-form/post"
                               :method :post :parameters '(("a" . "b")))
        (declare (ignore headers uri stream must-close))
        (is (= (length body-or-stream) 0))
        (is (= 201 status-code))))))

(test put-x-www-form
  "Beware, we just support this because it happens in the wild.
But this is not according to HTTP spec."
  (with-fixture init-destroy-httpd ()
    (let ((drakma:*header-stream* *standard-output*))
      (multiple-value-bind (body-or-stream status-code headers uri stream must-close)
          (drakma:http-request "http://127.0.0.1:10456/x-www-form/put"
                               :method :put :parameters '(("a" . "b")))
        (declare (ignore headers uri stream must-close))
        (is (= (length body-or-stream) 0))
        (is (= 200 status-code))))))

(test post-multipart-form
  (with-fixture init-destroy-httpd ()
    (let ((drakma:*header-stream* *standard-output*))
      (multiple-value-bind (body-or-stream status-code headers uri stream must-close)
          (drakma:http-request "http://127.0.0.1:10456/multipart-form/post"
                               :method :post :parameters '(("a" . #P"README.md")))
        (declare (ignore headers uri stream must-close))
        (is (= (length body-or-stream) 0))
        (is (= 201 status-code))))))

(test put-multipart-form
  "Beware, we just support this because it happens in the wild.
But this is not according to HTTP spec."
  (with-fixture init-destroy-httpd ()
    (let ((drakma:*header-stream* *standard-output*))
      (multiple-value-bind (body-or-stream status-code headers uri stream must-close)
          (drakma:http-request "http://127.0.0.1:10456/multipart-form/put"
                               :method :put :parameters '(("a" . #P"README.md")))
        (declare (ignore headers uri stream must-close))
        (is (= (length body-or-stream) 0))
        (is (= 200 status-code))))))

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


;; ------------------- server routes --------------------

(defun content-type-eq-p (expected-content-type header)
  (string= header
           expected-content-type
           :end1 (length expected-content-type)))

(easy-routes:defroute x-www-form-post ("/x-www-form/post" :method :post) ()
  (let ((expected-content-type "application/x-www-form-urlencoded"))
    (assert (content-type-eq-p
             expected-content-type
             (hunchentoot:header-in "Content-Type" hunchentoot:*request*)))
    (assert (equalp '(("a" . "b")) (hunchentoot:post-parameters*))))
  (setf (hunchentoot:return-code hunchentoot:*reply*) 201)
  "")

(easy-routes:defroute x-www-form-put ("/x-www-form/put" :method :put) ()
  (let ((expected-content-type "application/x-www-form-urlencoded")
        (raw-body (hunchentoot:raw-post-data :force-text t)))
    (assert (content-type-eq-p
             expected-content-type
             (hunchentoot:header-in "Content-Type" hunchentoot:*request*)))
    (assert (string= "a=b" raw-body)))
  (setf (hunchentoot:return-code hunchentoot:*reply*) 200)
  "")

(easy-routes:defroute multipart-form-post ("/multipart-form/post" :method :post) ()
  (let ((expected-content-type "multipart/form-data")
        (post-param (car (hunchentoot:post-parameters*))))
    (assert (content-type-eq-p
             expected-content-type
             (hunchentoot:header-in "Content-Type" hunchentoot:*request*)))
    (assert (string= "a" (first post-param)))
    (assert (string= "README.md" (third post-param)))
    (assert (string= "application/octet-stream" (fourth post-param))))
  (setf (hunchentoot:return-code hunchentoot:*reply*) 201)
  "")

(easy-routes:defroute multipart-form-put ("/multipart-form/put" :method :put) ()
  (let ((expected-content-type "multipart/form-data")
        (raw-body (hunchentoot:raw-post-data :force-text t)))
    (assert (content-type-eq-p
             expected-content-type
             (hunchentoot:header-in "Content-Type" hunchentoot:*request*)))
    (assert (ppcre:scan "Content-Disposition: form-data; name=\"a\"; filename=\"README.md\""
                        raw-body))
    (assert (ppcre:scan "Content-Type: application/octet-stream"
                        raw-body)))
  (setf (hunchentoot:return-code hunchentoot:*reply*) 200)
  "")
