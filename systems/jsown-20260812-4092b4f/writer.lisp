(in-package :jsown)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 3) (safety 0))))

;;;;;;;;;;;;;;;;;;
;; generic writing
(defgeneric to-json (object)
  (:documentation "Writes the given object to json in a generic way."))

(defclass json-encoded-content ()
  ((content :initarg :content
            :reader content))
  (:documentation "describes a json object whos content is serialized already."))

(defmethod initialize-instance ((jec json-encoded-content)
                                &key
                                  (content nil contentp)
                                  (unencoded-content nil unencoded-content-p)
                                  &allow-other-keys)
  (assert (not (and contentp unencoded-content-p))
          (content unencoded-content)
          "Please supply either content or unencoded-content, but not both.")
  (assert (or contentp unencoded-content-p)
          (content unencoded-content)
          "Please supply either content or unencoded-content, neither was supplied.")
  (setf (slot-value jec 'content)
        (if contentp content (to-json unencoded-content))))

(defmethod to-json ((content json-encoded-content))
  (content content))

(declaim (inline supplementary-plane-p))
(defun supplementary-plane-p (char-code)
  "character numbers between U+010000 and U+10FFFF (inclusive) are encoded as a
surrogate pair."
  (declare (type (integer 0 #.char-code-limit) char-code))
  (and (<= #x10000 char-code) (>= #x10FFFF char-code)))

(defmethod to-json ((string string))
  (with-output-to-string (stream)
    (flet ((write-characters (string)
             (loop for c across string
                do (write-char c stream))))
      (write-char #\" stream)
      (loop for char across string
         do (case char
              (#\newline (write-characters "\\n"))
              (#\return (write-characters "\\r"))
              (#\tab (write-characters "\\t"))
              (#\" (write-characters "\\\""))
              (#\\ (write-characters "\\\\"))
              (t (cond
                  ;; assume our characterset contains the ascii
                  ;; characters in the same order as ascii and
                  ;; that they're presented in one block without
                  ;; other characters in between.  This seems to
                  ;; be a reasonable assumption to make.
                   ((and (>= (char-code char) (char-code #\ ))
                         (<= (char-code char) (char-code #\~)))
                    (write-char char stream))

                   ;; encode UTF-16 surrogate pairs https://en.wikipedia.org/wiki/UTF-16#U+010000_to_U+10FFFF
                   ((supplementary-plane-p (char-code char))
                    (write-characters
                     (format nil "\\u~4,'0X\\u~4,'0X"
                             (+ (ash (- (char-code char) #x10000) -10)
                                #xD800)
                             (+ (logand (- (char-code char) #x10000)
                                        (- (ash 1 10) 1))
                                #xDC00))))

                   (t (write-characters (format nil "\\u~4,'0X" (char-code char))))))))
      (write-char #\" stream))))

(defmethod to-json ((number number))
  (with-output-to-string (stream)
    (write number :stream stream :pretty nil)))
(defmethod to-json ((ratio ratio))
  (to-json (coerce ratio 'float)))
(defmethod to-json ((float float))
  (let ((*read-default-float-format* (type-of float)))
    (format nil "~F" float)))

(defmethod to-json ((list list))
  (let ((*print-pretty* nil)) ;; *pretty-print* makes printing very slow, internal json objects needn't have this
    (if (eq (car list) :obj)
        (object-to-json (cdr list))
        (list-to-json list))))

(defmethod to-json ((array array))
  (labels ((array-to-list (array &rest dims)
             (if (= (length dims) (length (array-dimensions array)))
                 (apply #'aref array dims)
                 (loop for i from 0 below (elt (array-dimensions array) (length dims))
                    collect
                      (apply #'array-to-list array `(,@dims ,i))))))
    (to-json (array-to-list array))))

(defmethod to-json ((table hash-table))
  (let ((json-object (empty-object)))
    (loop for k being the hash-keys of table
       using (hash-value v)
       do (setf (val json-object k) v))
    (to-json json-object)))

(defmethod to-json ((s symbol))
  (to-json (symbol-name s)))

(defmethod to-json ((true (eql t)))
  "true")
(defmethod to-json ((true (eql :t)))
  "true")
(defmethod to-json ((true (eql :true)))
  "true")
(defmethod to-json ((false (eql :false)))
  "false")
(defmethod to-json ((false (eql :f)))
  "false")
(defmethod to-json ((false (eql :null)))
  "null")
(defmethod to-json ((false (eql :n)))
  "null")
(defmethod to-json ((empty-list (eql :empty-list)))
  "[]")

(defmethod to-json ((n (eql nil)))
  "[]")

(defun object-to-json (list)
  (format nil "{~{~{~A:~A~}~^,~}}"
          (loop for item in list collect
               (list (to-json (princ-to-string (car item)))
                     (to-json (cdr item))))))

(defun list-to-json (list)
  (format nil "[~{~A~^,~}]"
          (mapcar #'to-json list)))

;;;;;;;;;;;;;;;;;
;; speedy writing
(defun list-is-object-p (list)
  (declare (type (or cons nil) list))
  (and (consp list)
       (eq (car list) :obj)))

(defun to-json* (object)
  "Converts an object in internal jsown representation to a string containing the json representation"
  (let ((*print-pretty* nil))
    (with-output-to-string (output)
      (write-object-to-stream object output))))

(defun write-number* (object output)
  (declare (type number object)
           (type stream output))
  (typecase object
    (float (format output "~F" object))
    (otherwise (write object :stream output))))

(defun write-string* (object output)
  (declare (type string object)
           (type stream output))
  (write object :stream output :pretty nil))

(declaim (inline write-number* write-string*))

(defun write-object-to-stream (object output)
  (declare (type stream output))
  (typecase object
    (ratio  (write-number* (coerce object 'float) output))
    (number (write-number* object output))
    (string (write-string* object output))
    (null (format output "[]"))
    (t (cond ((eq object jsown:*parsed-true-value*)
              (format output "true"))
             ((eq object jsown:*parsed-false-value*)
              (format output "false"))
             ((list-is-object-p object)
              (if (null (cdr object))
                  (format output "{}")
                  (progn
                    (format output "{")
                    (write-string* (caadr object) output)
                    (format output ":")
                    (write-object-to-stream (cdadr object) output)
                    (loop
                       for curr-obj in (cddr object)
                       do (progn
                            (let ((k (car curr-obj))
                                  (v (cdr curr-obj)))
                              (declare (type string k))
                              (format output ",")
                              (write-string* k output)
                              (format output ":")
                              (write-object-to-stream v output))))
                    (format output "}"))))
             (t
              (progn ; we know the object isn't nil
                (format output "[")
                (write-object-to-stream (first object) output)
                (loop for item in (rest object)
                   do (progn
                        (format output ",")
                        (write-object-to-stream item output)))
                (format output "]")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; translating lispy content to json

(defun as-js-null (value)
  "returns <value> with nil being javascript's null (in jsown format)."
  (if value value :null))

(defun as-js-bool (value)
  "returns <value> as a boolean value (in jsown format)"
  (if value :true :false))
