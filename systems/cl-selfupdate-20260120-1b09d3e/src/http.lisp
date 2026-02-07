;;; http.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; HTTP abstraction layer for pluggable HTTP backends.

(in-package #:cl-selfupdate)

;;; HTTP Backend Protocol
;;;
;;; To use cl-selfupdate, load one of the backend systems:
;;;   - cl-selfupdate/dexador (uses dexador)
;;;   - cl-selfupdate/drakma (uses drakma)
;;;
;;; Custom backends can be implemented by defining methods for:
;;;   - http-request
;;;   - http-get
;;;   - http-get-stream

;;; Backend selection

(defvar *http-backend* nil
  "Current HTTP backend. Set automatically when loading a backend system.
Should be a keyword like :dexador or :drakma.")

(define-condition no-http-backend (error)
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "No HTTP backend loaded. Load one of:~%  ~
                            - cl-selfupdate/dexador~%  ~
                            - cl-selfupdate/drakma"))))

(defun check-http-backend ()
  "Signal an error if no HTTP backend is loaded."
  (unless *http-backend*
    (error 'no-http-backend)))

;;; HTTP Response Condition

(define-condition http-request-error (error)
  ((status-code :initarg :status-code :reader http-error-status-code)
   (url :initarg :url :reader http-error-url)
   (body :initarg :body :reader http-error-body :initform nil))
  (:report (lambda (c stream)
             (format stream "HTTP request failed with status ~A for ~A"
                     (http-error-status-code c)
                     (http-error-url c)))))

;;; HTTP Interface Generic Functions

(defgeneric http-request (backend url &key method headers)
  (:documentation "Make an HTTP request and return the response body as a string.
Returns (VALUES body status-code response-headers).
BACKEND is a keyword identifying the HTTP backend (e.g., :dexador, :drakma).
METHOD defaults to :GET.
HEADERS is an alist of (header-name . value)."))

(defgeneric http-get (backend url &key headers)
  (:documentation "Make an HTTP GET request and return the response body as octets.
Returns (VALUES body status-code response-headers).
BACKEND is a keyword identifying the HTTP backend.
HEADERS is an alist of (header-name . value)."))

(defgeneric http-get-stream (backend url &key headers)
  (:documentation "Make an HTTP GET request and return a binary stream.
Returns (VALUES stream status-code response-headers).
BACKEND is a keyword identifying the HTTP backend.
HEADERS is an alist of (header-name . value).
Caller is responsible for closing the stream."))

;;; Convenience wrappers that use *http-backend*

(defun do-http-request (url &key (method :get) headers)
  "Make an HTTP request using the current backend.
Returns response body as a string."
  (check-http-backend)
  (http-request *http-backend* url :method method :headers headers))

(defun do-http-get (url &key headers)
  "Make an HTTP GET request using the current backend.
Returns response body as octets."
  (check-http-backend)
  (http-get *http-backend* url :headers headers))

(defun do-http-get-stream (url &key headers)
  "Make an HTTP GET request using the current backend.
Returns a binary stream."
  (check-http-backend)
  (http-get-stream *http-backend* url :headers headers))
