;;;; Most code taken from S-XML-RPC and is licensed under the terms of
;;;; the Lisp Lesser General Public License (LLGPL)
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-base64")
  (ql:quickload "s-xml"))

(defpackage #:xmlrpc
  (:use #:cl)
  (:export #:call
	   #:xmlrpc-struct
	   #:xmlrpc-member))

(in-package #:xmlrpc)

;;; conditions

(define-condition xml-rpc-condition (error)
  ()
  (:documentation "Parent condition for all conditions thrown by the XML-RPC package"))

(define-condition xml-rpc-fault (xml-rpc-condition)
  ((code :initarg :code :reader xml-rpc-fault-code)
   (string :initarg :string :reader xml-rpc-fault-string))
  (:report (lambda (condition stream)
			 (format stream
					 "XML-RPC fault with message '~a' and code ~d."
					 (xml-rpc-fault-string condition)
					 (xml-rpc-fault-code condition))))
  (:documentation "This condition is thrown when the XML-RPC server returns a fault"))

(setf (documentation 'xml-rpc-fault-code 'function) "Get the code from an XML-RPC fault")
(setf (documentation 'xml-rpc-fault-string 'function) "Get the string from an XML-RPC fault")

(define-condition xml-rpc-error (xml-rpc-condition)
  ((place :initarg :code :reader xml-rpc-error-place)
   (data :initarg :data :reader xml-rpc-error-data))
  (:report (lambda (condition stream)
			 (format stream
					 "XML-RPC error ~a at ~a."
					 (xml-rpc-error-data condition)
					 (xml-rpc-error-place condition))))
  (:documentation "This condition is thrown when an XML-RPC protocol error occurs"))

(setf (documentation 'xml-rpc-error-place 'function)
      "Get the place from an XML-RPC error"
      (documentation 'xml-rpc-error-data 'function)
      "Get the data from an XML-RPC error")

;;; iso8601 support (the xml-rpc variant)

(defun universal-time->iso8601 (time &optional (stream nil))
  "Convert a Common Lisp universal time to a string in the XML-RPC variant of ISO8601"
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (format stream
			"~d~2,'0d~2,'0dT~2,'0d:~2,'0d:~2,'0d"
			year
			month
			date
			hour
			minute
			second)))

(defun iso8601->universal-time (string)
  "Convert string in the XML-RPC variant of ISO8601 to a Common Lisp universal time"
  (let (year month date (hour 0) (minute 0) (second 0))
    (when (< (length string) 9)
      (error "~s is to short to represent an iso8601" string))
    (setf year (parse-integer string :start 0 :end 4)
		  month (parse-integer string :start 4 :end 6)
		  date (parse-integer string :start 6 :end 8))
    (when (and (>= (length string) 17) (char= #\T (char string 8)))
      (setf hour (parse-integer string :start 9 :end 11)
			minute (parse-integer string :start 12 :end 14)
			second (parse-integer string :start 15 :end 17)))
    (encode-universal-time second minute hour date month year)))

(defstruct (xml-rpc-time (:print-function print-xml-rpc-time))
  "A wrapper around a Common Lisp universal time to be interpreted as an XML-RPC-TIME"
  universal-time)

(setf (documentation 'xml-rpc-time-p 'function)
      "Return T when the argument is an XML-RPC time"
      (documentation 'xml-rpc-time-universal-time 'function)
      "Return the universal time from an XML-RPC time")

(defun print-xml-rpc-time (xml-rpc-time stream depth)
  (declare (ignore depth))
  (format stream
		  "#<XML-RPC-TIME ~a>"
		  (universal-time->iso8601 (xml-rpc-time-universal-time xml-rpc-time))))

(defun xml-rpc-time (&optional (universal-time (get-universal-time)))
  "Create a new XML-RPC-TIME struct with the universal time specified, defaulting to now"
  (make-xml-rpc-time :universal-time universal-time))

;;; a wrapper for literal strings, where escaping #\< and #\& is not
;;; desired

(defstruct (xml-literal (:print-function print-xml-literal))
  "A wrapper around a Common Lisp string that will be sent over
  the wire unescaped"
  content)

(setf (documentation 'xml-literal-p 'function)
      "Return T when the argument is an unescaped xml string"
      (documentation 'xml-literal-content 'function)
      "Return the content of a literal xml string")

(defun print-xml-literal (xml-literal stream depth)
  (declare (ignore depth))
  (format stream
		  "#<XML-LITERAL \"~a\" >"
		  (xml-literal-content xml-literal)))

(defun xml-literal (content)
  "Create a new XML-LITERAL struct with the specified content."
  (make-xml-literal :content content))

;;; an extra datatype for xml-rpc structures (associative maps)

(defstruct (xml-rpc-struct (:print-function print-xml-rpc-struct))
  "An XML-RPC-STRUCT is an alist mapping member names to values"
  alist)

(setf (documentation 'xml-rpc-struct-p 'function)
      "Return T when the argument is an XML-RPC struct"
      (documentation 'xml-rpc-struct-alist 'function)
      "Return the alist of member names and values from an XML-RPC struct")

(defun print-xml-rpc-struct (xml-element stream depth)
  (declare (ignore depth))
  (format stream "#<XML-RPC-STRUCT :KEYS ~S>"
		  (mapcar #'car (xml-rpc-struct-alist xml-element))))

(defun (setf xml-rpc-struct-member) (value struct &rest keys)
  "Set the value of a specific member of an XML-RPC-STRUCT"
  (do ((keys keys (cdr keys))
       (obj struct (if (integerp (car keys))
		       (elt obj (car keys))
		       (cdr (assoc (car keys) (xml-rpc-struct-alist obj))))))
      ((null (cdr keys))
       (if (integerp (car keys))
	   (setf (elt obj (car keys)) value)
	   (setf (cdr (assoc (car keys) (xml-rpc-struct-alist obj))) value))))
  value)

(defun xml-rpc-struct-member (struct &rest keys)
  "Find a member of a structure."
  (do ((keys keys (cdr keys))
       (obj struct (let ((key (car keys)))
		     (if (integerp key)
			 (elt obj key)
			 (cdr (assoc key (xml-rpc-struct-alist obj) :test #'string-equal))))))
      ((null keys) obj)))

(defun xml-rpc-struct (&rest args)
  "Create a new XML-RPC-STRUCT from the arguments: alternating member names and values"
  (unless (evenp (length args))
    (error "~s must contain an even number of elements" args))
  (let (alist)
    (loop (if (null args)
			  (return)
			  (push (cons (pop args) (pop args)) alist)))
    (make-xml-rpc-struct :alist alist)))

(defun xml-rpc-struct-equal (struct1 struct2)
  "Compare two XML-RPC-STRUCTs for equality"
  (equal (xml-rpc-struct-alist struct1)
		 (xml-rpc-struct-alist struct2)))

;;; encoding support

(defun encode-xml-rpc-struct (struct stream)
  (princ "<struct>" stream)
  (mapcar (lambda (pair)
			(destructuring-bind (key . value) pair
			  (princ "<member>" stream)
			  (format stream "<name>~a</name>" key) ; assuming name contains no special characters
			  (encode-xml-rpc-value value stream)
			  (princ "</member>" stream)))
		  (xml-rpc-struct-alist struct))
  (princ "</struct>" stream))

(defun encode-xml-rpc-array (sequence stream)
  (princ "<array><data>" stream)
  (map 'nil #'(lambda (element) (encode-xml-rpc-value element stream)) sequence)
  (princ "</data></array>" stream))

(defun encode-xml-rpc-value (arg stream)
  (princ "<value>" stream)
  (cond ((or (null arg) (eq arg t))
		 (princ "<boolean>" stream)
		 (princ (if arg 1 0) stream)
		 (princ "</boolean>" stream))
		((or (stringp arg) (symbolp arg))
		 (princ "<string>" stream)
		 (s-xml:print-string-xml (string arg) stream)
		 (princ "</string>" stream))
		((integerp arg) (format stream "<int>~d</int>" arg))
		((floatp arg) (format stream "<double>~f</double>" arg))	
		((and (arrayp arg)
			  (= (array-rank arg) 1)
			  (subtypep (array-element-type arg)
						'(unsigned-byte 8)))
		 (princ "<base64>" stream)
		 (cl-base64:usb8-array-to-base64-stream arg stream)
		 (princ "</base64>" stream))
		((xml-rpc-time-p arg)
		 (princ "<dateTime.iso8601>" stream)
		 (universal-time->iso8601 (xml-rpc-time-universal-time arg) stream)
		 (princ "</dateTime.iso8601>" stream))
        ((xml-literal-p arg)
         (princ (xml-literal-content arg) stream))
		((or (listp arg) (vectorp arg)) (encode-xml-rpc-array arg stream))
		((xml-rpc-struct-p arg) (encode-xml-rpc-struct arg stream))
		;; add generic method call
		(t (error "cannot encode ~s" arg)))
  (princ "</value>" stream))

(defun encode-xml-rpc-args (args stream)
  (princ "<params>" stream)
  (dolist (arg args)
    (princ "<param>" stream)
    (encode-xml-rpc-value arg stream)
    (princ "</param>" stream))
  (princ "</params>" stream))

(defun encode-xml-rpc-call (name &rest args)
  "Encode an XML-RPC call with name and args as an XML string"
  (with-output-to-string (stream)
    (princ "<methodCall>" stream)
    ;; Spec says: The string may only contain identifier characters,
    ;; upper and lower-case A-Z, the numeric characters, 0-9,
    ;; underscore, dot, colon and slash.
    (format stream "<methodName>~a</methodName>" (string name)) ; assuming name contains no special characters
    (when args
      (encode-xml-rpc-args args stream))
    (princ "</methodCall>" stream)))

(defun encode-xml-rpc-result (value)
  (with-output-to-string (stream)
    (princ "<methodResponse>" stream)
    (encode-xml-rpc-args (list value) stream)
    (princ "</methodResponse>" stream)))

(defun encode-xml-rpc-fault-value (fault-string &optional (fault-code 0))
  ;; for system.multicall
  (with-output-to-string (stream)
    (princ "<struct>" stream)
    (format stream "<member><name>faultCode</name><value><int>~d</int></value></member>" fault-code)
    (princ "<member><name>faultString</name><value><string>" stream)
    (s-xml:print-string-xml fault-string stream)
    (princ "</string></value></member>" stream)
    (princ "</struct>" stream)))

(defun encode-xml-rpc-fault (fault-string &optional (fault-code 0))
  (with-output-to-string (stream)
    (princ "<methodResponse><fault><value>" stream)
    (princ (encode-xml-rpc-fault-value fault-string fault-code) stream)
    (princ "</value></fault></methodResponse>" stream)))

;;; decoding support

(defparameter *decode-value-types* nil)

(defun decode-xml-rpc-new-element (name attributes seed)
  (declare (ignore seed name attributes))
  '())

(defun decode-xml-rpc-finish-element (name attributes parent-seed seed)
  (declare (ignore attributes))
  (cons (ecase name
		  ((:|int| :|i4|)
		   (if *decode-value-types*
			   :int
			   (parse-integer seed)))
		  (:|double|
			(if *decode-value-types*
				:double
				(read-from-string seed)))
		  (:|boolean|
			(if *decode-value-types*
				:boolean
				(= 1 (parse-integer seed))))
		  (:|string|
			(if *decode-value-types*
				:string
				(if (null seed) "" seed)))
		  (:|dateTime.iso8601|
			(if *decode-value-types*
				:datetime
				(xml-rpc-time (iso8601->universal-time seed))))
		  (:|base64|
			(if (null seed)
				(make-array 0 :element-type '(unsigned-byte 8))
				(cl-base64:base64-string-to-usb8-array seed)))
		  (:|array|	(car seed))
		  (:|data| (nreverse seed))  ; potential problem with empty data i.e. <data>\n</data> parsed as "\n"
		  (:|value| (if (stringp seed) seed (car seed)))
		  (:|struct| (make-xml-rpc-struct :alist seed))
		  (:|member| (cons (cadr seed) (car seed)))
		  (:|name| seed) ;;(intern seed :keyword))
		  (:|params| (nreverse seed))  ; potential problem with empty params <params>\n</params> parsed as "\n"
		  (:|param| (car seed))
		  (:|fault| (make-condition 'xml-rpc-fault
									:string (xml-rpc-struct-member (car seed) :|faultString|)
									:code (xml-rpc-struct-member (car seed) :|faultCode|)))
		  (:|methodName| seed)
		  (:|methodCall| (let ((pair (nreverse seed)))
						   (cons (car pair) (cadr pair))))
		  (:|methodResponse| (car seed)))
		parent-seed))

;; fixes issue with empty params, data 
(defun decode-xml-rpc-text (string seed)
  (declare (ignore seed))
  (if (> (length (string-trim '(#\Space #\Newline #\Return)
							  string))
		 0)
      string
      nil))


(defun decode-xml-rpc (stream)
  (car (s-xml:start-parse-xml
		stream
		(make-instance 's-xml:xml-parser-state
					   :new-element-hook #'decode-xml-rpc-new-element
					   :finish-element-hook #'decode-xml-rpc-finish-element
					   :text-hook #'decode-xml-rpc-text))))

  

;;;; Copyright (c) Frank James 2017 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(defun call (url method &rest args)
  "Make an XML-RPC call.
URL ::= url of server.
METHOD ::= xmlrpc method name.
ARGS ::= list of arguments.
" 
  (let ((encoded (apply #'encode-xml-rpc-call method args)))
    (multiple-value-bind (resp status-code)
	(winhttp:http-request url
			      :method :post
			      :post-data encoded
			      :timeout 5000
			      :ignore-certificates-p t)
      (unless (= status-code 200) (error "HTTP Status ~A" status-code))
      (with-input-from-string (s resp)
        (let ((ret (decode-xml-rpc s)))
          (if (typep ret 'xml-rpc-fault)
              (error ret)
              (car ret)))))))

(defun xmlrpc-struct (&rest args)
  (apply #'xml-rpc-struct args))
(defun xmlrpc-member (struct &rest keys)
  (apply #'xml-rpc-struct-member struct keys))
