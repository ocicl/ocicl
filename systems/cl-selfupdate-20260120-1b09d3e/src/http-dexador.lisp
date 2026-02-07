;;; http-dexador.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Dexador HTTP backend for cl-selfupdate.

(in-package #:cl-selfupdate)

;;; Register this backend

(setf *http-backend* :dexador)

;;; Dexador implementation

(defmethod http-request ((backend (eql :dexador)) url &key (method :get) headers)
  "Make an HTTP request using dexador, returning body as string."
  (handler-case
      (multiple-value-bind (body status response-headers)
          (dex:request url
                       :method method
                       :headers headers
                       :want-stream nil)
        (values (if (stringp body)
                    body
                    (flexi-streams:octets-to-string body :external-format :utf-8))
                status
                response-headers))
    (dex:http-request-failed (e)
      (error 'http-request-error
             :status-code (dex:response-status e)
             :url url))))

(defmethod http-get ((backend (eql :dexador)) url &key headers)
  "Make an HTTP GET request using dexador, returning body as octets."
  (handler-case
      (multiple-value-bind (body status response-headers)
          (dex:get url
                   :headers headers
                   :want-stream nil
                   :force-binary t)
        (values body status response-headers))
    (dex:http-request-failed (e)
      (error 'http-request-error
             :status-code (dex:response-status e)
             :url url))))

(defmethod http-get-stream ((backend (eql :dexador)) url &key headers)
  "Make an HTTP GET request using dexador, returning a binary stream."
  (handler-case
      (multiple-value-bind (stream status response-headers)
          (dex:get url
                   :headers headers
                   :want-stream t
                   :force-binary t)
        (values stream status response-headers))
    (dex:http-request-failed (e)
      (error 'http-request-error
             :status-code (dex:response-status e)
             :url url))))
