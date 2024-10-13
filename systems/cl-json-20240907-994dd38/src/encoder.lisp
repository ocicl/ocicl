;;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Base: 10; Package: JSON -*-
;;;; Copyright (c) 2006-2008 Henrik Hjelte
;;;; Copyright (c) 2008 Hans Hübner (marked parts)
;;;; All rights reserved.
;;;; See the file LICENSE for terms of use and distribution.

(in-package :json)

(defvar *json-output* (make-synonym-stream '*standard-output*)
  "The default output stream for encoding operations.")

(define-condition unencodable-value-error (type-error)
  ((context :accessor unencodable-value-error-context :initarg :context))
  (:documentation
   "Signalled when a datum is passed to ENCODE-JSON (or another
encoder function) which actually cannot be encoded.")
  (:default-initargs :expected-type t)
  (:report
   (lambda (condition stream)
     (with-accessors ((datum type-error-datum)
                      (context unencodable-value-error-context))
         condition
       (format stream
               "Value ~S is not of a type which can be encoded~@[ by ~A~]."
               datum context)))))

(defun unencodable-value-error (value &optional context)
  "Signal an UNENCODABLE-VALUE-ERROR."
  (error 'unencodable-value-error :datum value :context context))

(defmacro with-substitute-printed-representation-restart ((object stream)
                                                          &body body)
  "Establish a SUBSTITUTE-PRINTED-REPRESENTATION restart for OBJECT
and execute BODY."
  `(restart-case (progn ,@body)
     (substitute-printed-representation ()
       (let ((repr (with-output-to-string (s)
                     (write ,object :stream s :escape nil)
                     nil)))
         (write-json-string repr ,stream)))))

(defgeneric encode-json (object &optional stream)
  (:documentation "Write a JSON representation of OBJECT to STREAM and
return NIL."))

(defun encode-json-to-string (object)
  "Return the JSON representation of OBJECT as a string."
  (with-output-to-string (stream)
    (encode-json object stream)))

(defmethod encode-json (anything &optional (stream *json-output*))
  "If OBJECT is not handled by any specialized encoder signal an error
which the user can correct by choosing to encode the string which is
the printed representation of the OBJECT."
  (declare (ignore stream))
  (unencodable-value-error anything 'encode-json))

(defmethod encode-json ((nr number) &optional (stream *json-output*))
  "Write the JSON representation of the number NR to STREAM (or to
*JSON-OUTPUT*)."
  (write-json-number nr stream))

(defmethod encode-json ((s string) &optional (stream *json-output*)) 
  "Write the JSON representation of the string S to STREAM (or to
*JSON-OUTPUT*)."
  (write-json-string s stream))

(defmethod encode-json ((c character) &optional (stream *json-output*))
  "JSON does not define a character type, we encode characters as Strings."
  (encode-json (string c) stream))

(defmethod encode-json ((s symbol) &optional (stream *json-output*))
  "Write the JSON representation of the symbol S to STREAM (or to
*JSON-OUTPUT*).  If S is boolean, a boolean literal is written.
Otherwise, the name of S is passed to *LISP-IDENTIFIER-NAME-TO-JSON*
and the result is written as String."
  (let ((mapped (car (rassoc s +json-lisp-symbol-tokens+))))
    (if mapped
        (progn (write-string mapped stream) nil)
        (let ((s (funcall *lisp-identifier-name-to-json* (symbol-name s))))
          (write-json-string s stream)))))


;;; The code below is from Hans Hübner's YASON (with modifications).

(defvar *json-aggregate-context* nil
  "NIL outside of any aggregate environment, 'ARRAY or 'OBJECT within
the respective environments.")

(defvar *json-aggregate-first* t
  "T when the first member of a JSON Object or Array is encoded,
afterwards NIL.")

(defun next-aggregate-member (context stream)
  "Between two members of an Object or Array, print a comma separator."
  (if (not (eq context *json-aggregate-context*))
      (error "Member encoder used ~:[outside any~;in inappropriate~] ~
              aggregate environment"
             *json-aggregate-context*))
  (prog1 *json-aggregate-first*
    (unless *json-aggregate-first*
      (write-char #\, stream))
    (setq *json-aggregate-first* nil)))

(defmacro with-aggregate ((context begin-char end-char
                           &optional (stream '*json-output*))
                          &body body)
  "Run BODY to encode a JSON aggregate type, delimited by BEGIN-CHAR
and END-CHAR."
  `(let ((*json-aggregate-context* ',context)
         (*json-aggregate-first* t))
     (declare (special *json-aggregate-context* *json-aggregate-first*))
     (write-char ,begin-char ,stream)
     (unwind-protect (progn ,@body)
       (write-char ,end-char ,stream))))

(defmacro with-array ((&optional (stream '*json-output*)) &body body)
  "Open a JSON Array, run BODY, then close the Array.  Inside the BODY,
AS-ARRAY-MEMBER or ENCODE-ARRAY-MEMBER should be called to encode
Members of the Array."
  `(with-aggregate (array #\[ #\] ,stream) ,@body))

(defmacro as-array-member ((&optional (stream '*json-output*))
                           &body body)
  "BODY should be a program which encodes exactly one JSON datum to
STREAM.  AS-ARRAY-MEMBER ensures that the datum is properly formatted
as a Member of an Array, i. e. separated by comma from any preceding
or following Member."
  `(progn
     (next-aggregate-member 'array ,stream)
     ,@body))

(defun encode-array-member (object &optional (stream *json-output*))
  "Encode OBJECT as the next Member of the innermost JSON Array opened
with WITH-ARRAY in the dynamic context.  OBJECT is encoded using the
ENCODE-JSON generic function, so it must be of a type for which an
ENCODE-JSON method is defined."
  (next-aggregate-member 'array stream)
  (encode-json object stream)
  object)

(defun stream-array-member-encoder (stream
                                    &optional (encoder #'encode-json))
  "Return a function which takes an argument and encodes it to STREAM
as a Member of an Array.  The encoding function is taken from the
value of ENCODER (default is #'ENCODE-JSON)."
  (lambda (object)
    (as-array-member (stream)
      (funcall encoder object stream))))

(defmacro with-object ((&optional (stream '*json-output*)) &body body)
  "Open a JSON Object, run BODY, then close the Object.  Inside the BODY,
AS-OBJECT-MEMBER or ENCODE-OBJECT-MEMBER should be called to encode
Members of the Object."
  `(with-aggregate (object #\{ #\} ,stream) ,@body))

(defmacro as-object-member ((key &optional (stream '*json-output*))
                             &body body)
  "BODY should be a program which writes exactly one JSON datum to
STREAM.  AS-OBJECT-MEMBER ensures that the datum is properly formatted
as a Member of an Object, i. e. preceded by the (encoded) KEY and
colon, and separated by comma from any preceding or following Member."
  `(progn
     (next-aggregate-member 'object ,stream)
     (let ((key (encode-json-to-string ,key)))
       (if (char= (aref key 0) #\")
           (progn (write-string key ,stream) nil)
           (encode-json key ,stream)))
     (write-char #\: ,stream)
     ,@body))

(defun encode-object-member (key value
                             &optional (stream *json-output*))
  "Encode KEY and VALUE as a Member pair of the innermost JSON Object
opened with WITH-OBJECT in the dynamic context.  KEY and VALUE are
encoded using the ENCODE-JSON generic function, so they both must be
of a type for which an ENCODE-JSON method is defined.  If KEY does not
encode to a String, its JSON representation (as a string) is encoded
over again."
  (as-object-member (key stream)
    (encode-json value stream))
  value)

(defun stream-object-member-encoder (stream
                                     &optional (encoder #'encode-json))
  "Return a function which takes two arguments and encodes them to
STREAM as a Member of an Object (String : Value pair)."
  (lambda (key value)
    (as-object-member (key stream)
      (funcall encoder value stream))))

;;; End of YASON code.


;;; You can use the streaming encoder above, or
;;; two differnet types of sexp based encoders below

(defun encode-json-list-guessing-encoder (s stream)
  "Write the JSON representation of the list S to STREAM (or to
*JSON-OUTPUT*).  If S is not encodable as a JSON Array, try to encode
it as an Object (per ENCODE-JSON-ALIST)."
  (restart-case
      (handler-bind ((unencodable-value-error
                       (lambda (e)
                         (with-accessors ((datum type-error-datum)) e
                           (if (and (consp datum)
                                    (ignore-errors (every #'consp s)))
                               (invoke-restart 'try-as-alist)
                               (error e)))))
                     (type-error
                       (lambda (e)
                         (declare (ignore e))
                         (unencodable-value-error s 'encode-json))))
        (write-string
         (with-output-to-string (temp)
           (with-array (temp)
             (mapcar (stream-array-member-encoder temp) s)))
         stream))
    (try-as-alist ()
      (encode-json-alist s stream)))
  (values))

(defun json-bool (value)
  "Intended for the JSON-EXPLICT-ENCODER. Converts a non-nil value
to a value (:true) that creates a json true value when used in the 
explict encoder. Or (:false)."
  (if value
      '(:true)
      '(:false)))

(defun json-or-null (value)
  "Intended for the JSON-EXPLICT-ENCODER. Returns a non-nil value
as itself, or a nil value as a json null-value"
  (or value '(:null)))

(defun encode-json-list-explicit-encoder (s stream)
  (handler-bind ((type-error
                  (lambda (e)
                    (declare (ignore e))
                    (unencodable-value-error s 'encode-json))))
    (ecase (car s)
      (:json (mapcar (lambda (str) (write-string str stream))
                     (cdr s)))
      (:true (write-json-chars "true" stream))
      (:false (write-json-chars "false" stream))
      (:null (write-json-chars "null" stream))
      ((:list :array)
       (with-array (stream)
         (mapcar (stream-array-member-encoder stream)
                 (cdr s))))
      (:object (if (consp (cadr s))
                  (encode-json-alist (cdr s) stream)
                  (encode-json-plist (cdr s) stream)))
      (:alist (encode-json-alist (cdr s) stream))
      (:plist (encode-json-plist (cdr s) stream)))
    nil))

(defparameter *json-list-encoder-fn* 'encode-json-list-guessing-encoder)

(defun use-guessing-encoder ()
  (setf *json-list-encoder-fn* 'encode-json-list-guessing-encoder))

(defun use-explicit-encoder ()
  (setf *json-list-encoder-fn* 'encode-json-list-explicit-encoder))

(defmacro with-local-encoder (&body body)
  `(let (*json-list-encoder-fn*)
     (declare (special *json-list-encoder-fn*))
     ,@body))

(defmacro with-guessing-encoder  (&body body)
  `(with-local-encoder (use-guessing-encoder)
     ,@body))

(defmacro with-explicit-encoder  (&body body)
  `(with-local-encoder (use-explicit-encoder)
     ,@body))

(defmethod encode-json ((s list) &optional (stream *json-output*))
  "Write the JSON representation of the list S to STREAM (or to
*JSON-OUTPUT*), using one of the two rules specified by 
first calling USE-GUESSING-ENCODER or USE-EXPLICIT-ENCODER.
The guessing encoder: If S is a list encode S as a JSON Array, if
S is a dotted list encode it as an Object (per ENCODE-JSON-ALIST).
The explicit decoder: If S is a list, the first symbol defines
the encoding: 
If (car S) is 'TRUE return a JSON true value.
If (car S) is 'FALSE return a JSON false value.
If (car S) is 'NULL return a JSON null value.
If (car S) is 'JSON princ the strings in (cdr s) to stream
If (car S) is 'LIST or 'ARRAY encode (cdr S) as a a JSON Array.
If (car S) is 'OBJECT encode (cdr S) as A JSON Object, 
interpreting (cdr S) either as an A-LIST or a P-LIST."
  (funcall *json-list-encoder-fn* s stream))

(defmethod encode-json ((s sequence) &optional (stream *json-output*))
  "Write the JSON representation (Array) of the sequence S (not an
alist) to STREAM (or to *JSON-OUTPUT*)."
  (with-array (stream)
    (map nil (stream-array-member-encoder stream) s)))

(defmethod encode-json ((h hash-table) &optional (stream *json-output*))
  "Write the JSON representation (Object) of the hash table H to
STREAM (or to *JSON-OUTPUT*)."
  (with-object (stream)
    (maphash (stream-object-member-encoder stream) h)))

#+cl-json-clos
(defmethod encode-json ((o standard-object)
                        &optional (stream *json-output*))
  "Write the JSON representation (Object) of the CLOS object O to
STREAM (or to *JSON-OUTPUT*)."
  (with-object (stream)
    (map-slots (stream-object-member-encoder stream) o)))

(defun encode-json-alist (alist &optional (stream *json-output*))
  "Write the JSON representation (Object) of ALIST to STREAM (or to
*JSON-OUTPUT*).  Return NIL."
  (write-string
   (with-output-to-string (temp)
     (with-object (temp)
       (loop
          with bindings = alist
          do (if (listp bindings)
                 (if (endp bindings)
                     (return)
                     (let ((binding (pop bindings)))
                       (if (consp binding)
                           (destructuring-bind (key . value) binding
                             (encode-object-member key value temp))
                           (unless (null binding)
                             (unencodable-value-error
                              alist 'encode-json-alist)))))
                 (unencodable-value-error alist 'encode-json-alist)))))
   stream)
  nil)

(defun encode-json-alist-to-string (alist)
  "Return the JSON representation (Object) of ALIST as a string."
  (with-output-to-string (stream)
    (encode-json-alist alist stream)))

(defun encode-json-plist (plist &optional (stream *json-output*))
  "Write the JSON representation (Object) of PLIST to STREAM (or to
*JSON-OUTPUT*).  Return NIL."
  (write-string
   (with-output-to-string (temp)
     (with-object (temp)
       (loop
          with properties = plist
          do (if (listp properties)
                 (if (endp properties)
                     (return)
                     (let ((indicator (pop properties)))
                       (if (and (listp properties)
                                (not (endp properties)))
                           (encode-object-member
                            indicator (pop properties) temp)
                           (unencodable-value-error
                            plist 'encode-json-plist))))
                 (unencodable-value-error plist 'encode-json-plist)))))
   stream)
  nil)

(defun encode-json-plist-to-string (plist)
  "Return the JSON representation (Object) of PLIST as a string."
  (with-output-to-string (stream)
    (encode-json-plist plist stream)))

(defun write-json-string (s stream)
  "Write a JSON representation (String) of S to STREAM."
  (write-char #\" stream)
  (if (stringp s)
      (write-json-chars s stream)
      (encode-json s stream))
  (write-char #\" stream)
  nil)

(defun write-json-chars (s stream)
  "Write JSON representations (chars or escape sequences) of
characters in string S to STREAM."
  (loop for ch across s
     for code = (char-code ch)
     with special
     if (setq special (car (rassoc ch +json-lisp-escaped-chars+)))
       do (write-char #\\ stream) (write-char special stream)
     else if (< #x1f code #x7f)
       do (write-char ch stream)
     else
       do (let ((special '#.(rassoc-if #'consp +json-lisp-escaped-chars+)))
            (destructuring-bind (esc . (width . radix)) special
              (format stream "\\~C~V,V,'0R" esc radix width code)))))

(eval-when (:compile-toplevel :execute)
    (if (subtypep 'long-float 'single-float)
        ;; only one float type
        (pushnew :cl-json-only-one-float-type *features*)
        ;; else -- we check here only for the case where there are two
        ;; float types, single- and double- --- we don't consider the
        ;; "only single and short" case.  Could be added if necessary.
        (progn
          (when (subtypep 'single-float 'short-float)
            (pushnew :cl-json-single-float-is-subsumed *features*))
          (when (subtypep 'long-float 'double-float)
            (pushnew :cl-json-double-float-is-subsumed *features*)))))

(defun write-json-number (nr stream)
  "Write the JSON representation of the number NR to STREAM."
  (typecase nr
    (integer (format stream "~d" nr))
    (real (let ((*read-default-float-format*
                 (etypecase nr
                   (short-float 'short-float)
                   (rational 'single-float)
                   #-(or cl-json-single-float-is-subsumed
                         cl-json-only-one-float-type)
                   (single-float 'single-float)
                   #-(or cl-json-double-float-is-subsumed
                         cl-json-only-one-float-type)
                   (double-float 'double-float)
                   #-cl-json-only-one-float-type
                   (long-float 'long-float))))
            (format stream "~f" nr)))
    (t (unencodable-value-error nr 'write-json-number))))

