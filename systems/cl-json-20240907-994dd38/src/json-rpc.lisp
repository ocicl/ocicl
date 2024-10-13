;;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Base: 10 ; Package: JSON-RPC -*-
;;;; Copyright (c) 2006-2008 Henrik Hjelte
;;;; Modifications copyright (c) 2009 by Robert P. Goldman and SIFT, LLC.
;;;; All rights reserved.
;;;; See the file LICENSE for terms of use and distribution.

(in-package :json-rpc)

;; http://json-rpc.org/wiki/specification
;; http://json-rpc.org/wd/JSON-RPC-1-1-WD-20060807.html

;;;---------------------------------------------------------------------------
;;; I have added support for a partial implementation of the JSON RPC 2.0
;;; draft specification.  The implementation is partial because:
;;; 1.  There is no support for batch/multicall and
;;; 2.  There is no support for positional arguments which, in JSON RPC 2.0,
;;;      are handled by passing a singleton object as the params, rather than
;;;      passing a parameter array.
;;;  [2010/01/03:rpg]
;;;---------------------------------------------------------------------------
;;; this version is similar to the one in the SBCL manual, but allows you to
;;; override previous values.  The one in the SBCL manual quietly throws new
;;; values on the floor, AFAICT.  [2010/01/09:rpg]
(defmacro defconstant (name value &optional doc)
    `(cl:defconstant ,name (if (and (boundp ',name)
                                 (equalp (symbol-value ',name)
                                         ,value))
                            (symbol-value ',name) ,value)
                       ,@(when doc (list doc))))

(defconstant +json-rpc-1.1+ "1.1")
(defconstant +json-rpc-2.0+ "2.0")

(defvar *json-rpc-version* +json-rpc-1.1+
  "Bind this variable to influence whether you want to use
JSON-RPC version 1.1 or 2.0.")

(defvar *json-rpc-functions* (make-hash-table :test #'equal))

(defun clear-exported ()
  (clrhash *json-rpc-functions*))

(defmacro defun-json-rpc (name type lambda-list &body body)
  "Defines a function and registers it as a json-rpc target."
  (unless (json-rpc-encoding-p type)
    (error "New version of defun-json-rpc requires a TYPE argument"))
  `(progn
     (defun ,name ,lambda-list ,@body)
     (export-as-json-rpc ',name (lisp-to-camel-case (symbol-name ',name))
                         ,type)))

(defgeneric json-rpc-encoding-p (keyword)
  (:documentation "Is KEYWORD a valid JSON-RPC value encoding?")
  (:method (keyword)
    "Default is no."
    (declare (ignore keyword))
    nil)
  ;;; built-in methods
  (:method ((keyword (eql :guessing)))
    t)
  (:method ((keyword (eql :streaming)))
    t)
  (:method ((keyword (eql :explicit)))
    t))

(defgeneric encode-json-rpc-value (raw-value encoding)
  (:documentation "Translate RAW-VALUE according to JSON-RPC
value encoding ENCODING")
  (:method :before (raw-value encoding)
    (declare (ignore raw-value))
    (unless (json-rpc-encoding-p encoding)
      (error "Invalid JSON-RPC encoding spec: ~a" encoding)))
  (:method (raw-value (encoding (eql :guessing)))
    (list :json
          (with-guessing-encoder
            (encode-json-to-string raw-value))))
  (:method (raw-value (encoding (eql :streaming)))
    (if (stringp raw-value)
        (list :json raw-value)
        (error "Can't stream non-string return value ~a" raw-value)))
  (:method (raw-value (encoding (eql :explicit)))
    raw-value))

(defmacro def-json-rpc-encoding (keyword (var) &rest body)
  "Define a new encoding keyword, KEYWORD.  When the encoding
is invoked, the raw value will be bound to VAR, and the
BODY should return the encoded value."
  `(progn
     (defmethod json-rpc-encoding-p ((keyword (eql ',keyword)))
       t)
     (defmethod encode-json-rpc-value (,var (keyword (eql ',keyword)))
       ,@body)))

(def-json-rpc-encoding :boolean (raw-val)
    (list :json
          (with-explicit-encoder
            (encode-json-to-string (if raw-val '(:true) '(:false))))))

(defconstant +empty-array+ '(:array))

(def-json-rpc-encoding :array (raw-val)
  (list :json
        (with-explicit-encoder
          (encode-json-to-string
           (cond ((null raw-val) +empty-array+)
                 ((listp raw-val)
                  (cons :array raw-val))
                 ((arrayp raw-val)
                  raw-val)
                 (t (error "don't know how to encode value ~a as array." raw-val)))))))

(defun export-as-json-rpc (func function-name &optional type)
  "Registers a lambda function FUNC as a json-rpc function.
TYPE determines how the return value of FUNC should be interpreted:
:explicit using the explicit encoder syntax,
:guessing using the guessing encode syntax
:streaming as a raw JSON string.
"
  (setf (gethash function-name *json-rpc-functions*) (cons func (or type :guessing))))

(defun make-rpc-response (&key result error id)
  "When the method invocation completes, the service must reply with a response. The response is a single object serialized using JSON.

It has three properties:

    * result - The Object that was returned by the invoked method. This must be null in case there was an error invoking the method.
    * error - An Error object(unspecified in json-rpc 1.0) if there was an error invoking the method. Null if there was no error.
    * id - This must be the same id as the request it is responding to. "
  (cond ((equalp *json-rpc-version* +json-rpc-1.1+)
         (with-explicit-encoder
           (json:encode-json-to-string
            `(:object
              (:result . ,result)
              (:error . ,error)
              (:id . ,id)))))
        ((equalp *json-rpc-version* +json-rpc-2.0+)
         (cond (result
                (when error
                  (error "Forbidden to have both a JSON-RPC result AND a JSON-RPC error."))
                (with-explicit-encoder
                  (json:encode-json-to-string
                   `(:object
                     (:jsonrpc . ,+json-rpc-2.0+)
                     (:result . ,result)
                     (:id . ,id)))))
               (error
                (let ((error (cdr error)))
                  ;; check the slots
                  (unless (and (assoc :code error)
                               (integerp (cdr (assoc :code error)))
                               (assoc :message error)
                               (stringp (cdr (assoc :message error))))
                    (cerror "Just return it anyway."
                            "Ill-formed JSON-RPC error, ~a, for version ~a"
                            error *json-rpc-version*)))
                (with-explicit-encoder
                  (json:encode-json-to-string
                   `(:object
                     (:jsonrpc . ,+json-rpc-2.0+)
                     (:error . ,error)
                     (:id . ,id)))))
               (t
                (error "Response must have either result or error."))))
        (t (error "Unknown JSON-RPC protocol version ~a." *json-rpc-version*))))


(defun make-json-rpc-error-object-1.1 (message &key code error-object)
  "This code is based on the Working Draft 7 August 2006 of Json-rpc 1.1 specification.
  http://json-rpc.org/wd/JSON-RPC-1-1-WD-20060807.html
"
  (let ((eo `(:object
              (:name . "JSONRPCError")
              (:code . ,(or code 999))
              (:message . ,message))))
    (if error-object
        (append eo `((:error . ,error-object)))
        eo)))

(defun make-json-rpc-error-object-2.0 (&key message code data error-object)
  (unless (stringp message)
    (error "Message attribute of JSON-RPC object must be a string."))
  ;; symbolic error codes:
  ;;-32700              Parse error.    Invalid JSON. An error occurred on the server while parsing the JSON text.
  ;;-32600              Invalid Request.        The received JSON is not a valid JSON-RPC Request.
  ;;-32601              Method not found.       The requested remote-procedure does not exist / is not available.
  ;;-32602              Invalid params.         Invalid method parameters.
  ;;-32603              Internal error.         Internal JSON-RPC error.
  ;;-32099..-32000      Server error.   Reserved for implementation-defined server-errors.
  (if (symbolp code)
      (setf code
            (ecase code
              (:parse-error -32700)
              (:invalid-request -32600)
              (:method-not-found -32601)
              (:invalid-params -32602)
              (:internal-error -32603)
              (:service-error -32000)))
      (unless (integerp code)
        (error "Code attribute of JSON-RPC object must be an integer.")))
  (let (data-obj)
    (when error-object
      (push `(:error . ,error-object) data-obj))
    (when data
      (push `(:data ,data) data-obj))
    (let ((eo `(:object
                (:name . "JSONRPCError")
                (:code . ,code)
                (:message . ,message)
                ,@(when data-obj
                        `((:data . (:object ,@data-obj)))))))
      eo)))

(defun invoke-rpc (json-source)
  "A remote method is invoked by sending a request to a remote service. The request is a single object serialized using JSON.

It has three properties:

    * method - A String containing the name of the method to be invoked.
    * params - An Array of objects to pass as arguments to the method.
    * id - The request id. This can be of any type. It is used to match the response with the request that it is replying to. "
  (json-bind (method params id) json-source
    (invoke-rpc-parsed method params id)))

(define-condition json-rpc-call-error (error)
  ((encapsulated-error
    :initarg :error
    :reader encapsulated-error
    )))


;;; FIXME:  I discovered that if the METHOD argument is NIL, this just does nothing.  Not sure why... [2010/01/15:rpg]
(defun invoke-rpc-parsed (method params &optional id)
  (flet ((json-rpc-2.0 ()
           (equalp *json-rpc-version* +json-rpc-2.0+)))
    (restart-case
        (let ((func-type (gethash method *json-rpc-functions*)))
          (if func-type
              (handler-bind
                  ((error #'(lambda (err)
                              (error 'json-rpc-call-error :error err))))
                (destructuring-bind (func . type) func-type
                  (let ((retval (restart-case (apply func params)
                                  (use-value (value)
                                    value)))
                        explicit-retval)
                    (when id
                      ;; if there's no id, this is a notification, and no response should be sent
                      ;; [2009/12/30:rpg]
                      (setf explicit-retval
                            (encode-json-rpc-value retval type)))
                    (make-rpc-response :id id :result explicit-retval))))

              (when id
                (make-rpc-response :id id :error (cond ((json-rpc-2.0)
                                                        (make-json-rpc-error-object-2.0
                                                         :message (format nil "Procedure ~a not found." method)
                                                         :code :method-not-found))
                                                       (t
                                                        (make-json-rpc-error-object-1.1
                                                         (format nil "Procedure ~a not found." method))))))))
      (send-error (message &optional code error-object)
        :test (lambda (c) (declare (ignore c)) id)
        (make-rpc-response :id id
                           :error
                           (if (json-rpc-2.0)
                               (progn
                                 (unless code
                                   (assert code (code)
                                           "Error code is mandatory in JSON-RPC version 2.0."))
                                 (if error-object
                                     (make-json-rpc-error-object-2.0
                                      :message message
                                      :code code
                                      :error-object error-object)
                                     (make-json-rpc-error-object-2.0
                                      :message message
                                      :code code)))
                               (make-json-rpc-error-object-1.1 message
                                                               :code code
                                                               :error-object error-object))))
      (send-error-object (error-object)
        :test (lambda (c) (declare (ignore c)) id)
        (make-rpc-response :id id :error error-object))
      (send-nothing ()
        nil)
      (send-internal-error ()
        :test (lambda (c) (declare (ignore c)) id)
        (format t "~&invoking send-internal-error restart.~%")
        (make-rpc-response :id id
                           :error
                           (if (json-rpc-2.0)
                               (make-json-rpc-error-object-2.0
                                :message "Service error"
                                :code :service-error)
                               (make-json-rpc-error-object-1.1 "Service error")))))))

(defmacro def-restart (restart-name &rest (params))
  `(defun ,restart-name (,@params &optional condition)
     (let ((restart (find-restart ',restart-name condition)))
       (invoke-restart restart ,@params))))

(def-restart send-error (errmsg code))
(def-restart send-error-object (explicit-errobject))
(def-restart send-nothing ())
(def-restart send-internal-error ())



;;;; This is a fragment of how to generate a smd description
;;;; If someone is interested you might want to clean this
;;;; up and incorporated this into json-rpc
;;;;
;;;;     (defmacro defun-schat-api (name params &body body)
;;;;        .....
;;;;         (setf (gethash ',name *json-rpc-method-definitions*)
;;;;          (list :parameters params))
;;;;
;;;;     (defun generate-smd (stream service-url)
;;;;       (let ((json-data
;;;;               `(:object
;;;;                  :service-type  "JSON-RPC"
;;;;                  :service-+url+ ,service-url
;;;;                 :methods
;;;;                  (:list
;;;;                   ,@(loop for fname being each hash-key of *json-rpc-method-definitions*
;;;;                        using (hash-value description)
;;;;                        for params = (getf description :parameters)
;;;;                        collect `(:object
;;;;                                  :name ,fname
;;;;                                  :parameters (:list
;;;;                                               ,@(loop for param in params
;;;;                                                    collect `(:object :name ,param)))))))))
;;;;           (json:with-explicit-encoder
;;;;             (json:encode-json json-data stream))))
;;;;
;;;;     (defun generate-smd-file (filename)
;;;;       (with-open-file (s filename :direction :output :if-exists :supersede)
;;;;         (generate-smd s "/schat/json-rpc")))
;;;;
