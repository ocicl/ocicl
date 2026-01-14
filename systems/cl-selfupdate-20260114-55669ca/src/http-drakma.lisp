;;; http-drakma.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Drakma HTTP backend for cl-selfupdate.

(in-package #:cl-selfupdate)

;;; Register this backend

(setf *http-backend* :drakma)

;;; Drakma implementation

(defmethod http-request ((backend (eql :drakma)) url &key (method :get) headers)
  "Make an HTTP request using drakma, returning body as string."
  (multiple-value-bind (body status-code response-headers uri stream must-close reason-phrase)
      (drakma:http-request url
                           :method method
                           :additional-headers headers
                           :want-stream nil
                           :force-binary nil)
    (declare (ignore uri stream must-close reason-phrase))
    (unless (<= 200 status-code 299)
      (error 'http-request-error
             :status-code status-code
             :url url
             :body body))
    (values (if (stringp body)
                body
                (flexi-streams:octets-to-string body :external-format :utf-8))
            status-code
            response-headers)))

(defmethod http-get ((backend (eql :drakma)) url &key headers)
  "Make an HTTP GET request using drakma, returning body as octets."
  (multiple-value-bind (body status-code response-headers uri stream must-close reason-phrase)
      (drakma:http-request url
                           :method :get
                           :additional-headers headers
                           :want-stream nil
                           :force-binary t)
    (declare (ignore uri stream must-close reason-phrase))
    (unless (<= 200 status-code 299)
      (error 'http-request-error
             :status-code status-code
             :url url
             :body body))
    (values body status-code response-headers)))

(defmethod http-get-stream ((backend (eql :drakma)) url &key headers)
  "Make an HTTP GET request using drakma, returning a binary stream."
  (multiple-value-bind (stream status-code response-headers)
      (drakma:http-request url
                           :method :get
                           :additional-headers headers
                           :want-stream t
                           :force-binary t)
    (unless (<= 200 status-code 299)
      (when stream (close stream))
      (error 'http-request-error
             :status-code status-code
             :url url))
    (values stream status-code response-headers)))
