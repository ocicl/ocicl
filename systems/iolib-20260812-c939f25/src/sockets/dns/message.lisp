;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- DNS message creation.
;;;

(in-package :iolib/sockets)

(defclass dns-message ()
  ((id    :initform 0 :initarg :id    :accessor dns-message-id)
   (flags :initform 0 :initarg :flags :accessor dns-message-flags)
   (decoded-flags :initform nil :accessor dns-message-decoded-flags)
   (qdcount :initarg :qdcount :accessor dns-message-question-count)
   (ancount :initarg :ancount :accessor dns-message-answer-count)
   (nscount :initarg :nscount :accessor dns-message-authority-count)
   (arcount :initarg :arcount :accessor dns-message-additional-count)
   (question   :accessor dns-message-question)
   (answer     :accessor dns-message-answer)
   (authority  :accessor dns-message-authority)
   (additional :accessor dns-message-additional))
  (:default-initargs :qdcount 1 :ancount 0 :nscount 0 :arcount 0))

(defmacro %define-dns-field-reader (name offset type length)
  `(defgeneric ,name (message)
     (:method ((message dns-message))
       ,(ecase type
               (:boolean `(logbitp ,offset (dns-message-flags message)))
               (:integer `(ldb (byte ,length ,offset)
                               (dns-message-flags message)))
               (:rcode `(rcode-id
                         (ldb (byte ,length ,offset)
                              (dns-message-flags message))))))))

(defmacro %define-dns-field-writer (name offset type length)
  `(defgeneric (setf ,name) (value message)
     (:method (value (message dns-message))
       ,(ecase type
               (:boolean `(setf (ldb (byte 1 ,offset)
                                     (dns-message-flags message))
                                (lisp->c-bool value)))
               (:integer `(setf (ldb (byte ,length ,offset)
                                     (dns-message-flags message))
                                value))
               (:rcode `(setf (ldb (byte ,length ,offset)
                                   (dns-message-flags message))
                              (rcode-number value)))))))

(defmacro define-dns-field (name offset type &key length)
  (let ((method-name (format-symbol t "~A-~A" name '#:field)))
    `(progn
       (%define-dns-field-reader ,method-name ,offset ,type ,length)
       (%define-dns-field-writer ,method-name ,offset ,type ,length))))

(define-dns-field response            15 :boolean)
(define-dns-field opcode              11 :integer :length 4)
(define-dns-field authoritative       10 :boolean)
(define-dns-field truncated            9 :boolean)
(define-dns-field recursion-desired    8 :boolean)
(define-dns-field recursion-available  7 :boolean)
(define-dns-field rcode                0 :rcode :length 4)

(defgeneric decode-flags (message)
  (:method ((msg dns-message))
    (let (flags)
      (push (if (= (opcode-field msg) +opcode-standard+)
                :op/s :op/u)
            flags)
      (when (authoritative-field msg) (push :auth flags))
      (when (truncated-field msg) (push :trunc flags))
      (when (recursion-desired-field msg) (push :rd flags))
      (when (recursion-available-field msg) (push :ra flags))
      (push (or (rcode-field msg) :rc/u) flags)
      (nreverse flags))))

(defgeneric dns-flag-p (message flag)
  (:method ((msg dns-message) flag)
    (member flag (dns-message-decoded-flags msg) :test #'eq)))

(defmethod initialize-instance :after ((msg dns-message) &key
                                       (qdcount 0) (ancount 0)
                                       (nscount 0) (arcount 0))
  (with-accessors ((id dns-message-id) (flags dns-message-flags)
                   (decoded-flags dns-message-decoded-flags)
                   (question dns-message-question) (answer dns-message-answer)
                   (authority dns-message-authority) (additional dns-message-additional))
      msg
    (setf decoded-flags (decode-flags msg)
          question      (make-array qdcount :adjustable t :fill-pointer 0)
          answer        (make-array ancount :adjustable t :fill-pointer 0)
          authority     (make-array nscount :adjustable t :fill-pointer 0)
          additional    (make-array arcount :adjustable t :fill-pointer 0))))

(defmethod print-object ((msg dns-message) stream)
  (print-unreadable-object (msg stream :type nil :identity nil)
    (with-accessors ((id dns-message-id) (decoded-flags dns-message-decoded-flags)
                     (question dns-message-question)
                     (qdcount dns-message-question-count) (ancount dns-message-answer-count)
                     (nscount dns-message-authority-count) (arcount dns-message-additional-count))
        msg
      (format stream "DNS ~A Id: ~A, Question: ~A Flags:~{ ~S~}, Sections: QD(~A) AN(~A) NS(~A) AD(~A)"
              (if (response-field msg) :response :query)
              id question decoded-flags
              qdcount ancount nscount arcount))))

(defclass dns-record ()
  ((name  :initarg :name  :accessor dns-record-name)
   (type  :initarg :type  :accessor dns-record-type)
   (class :initarg :class :accessor dns-record-class)))

(defmethod initialize-instance :after ((record dns-record) &key)
  (with-accessors ((name dns-record-name)
                   (type dns-record-type)
                   (class dns-record-class))
      record
    (check-type name string "a string")
    (check-type type (satisfies dns-record-type-p) "a valid record type")
    (check-type class (member :in) ":IN")))

(defclass dns-question (dns-record) ())

(defmethod print-object ((question dns-question) stream)
  (print-unreadable-object (question stream :type nil :identity nil)
    (with-accessors ((name dns-record-name)
                     (type dns-record-type)
                     (class dns-record-class))
        question
      (format stream "~S ~A ~A" name type class))))

(defmethod initialize-instance :after ((record dns-question) &key)
  (with-accessors ((name dns-record-name)) record
    (let ((name-length (length name)))
      (when (char/= #\. (aref name (1- name-length)))
        (setf name (concatenate 'string name "."))))))

;;;; Constructors

(defun make-question (qname qtype qclass)
  (make-instance 'dns-question
                 :name qname
                 :type qtype
                 :class qclass))

(defun make-query (id question &optional recursion-desired)
  (let ((msg (make-instance 'dns-message :id id)))
    (setf (opcode-field msg) +opcode-standard+)
    (setf (recursion-desired-field msg) recursion-desired)
    (vector-push-extend question (dns-message-question msg))
    (values msg)))

;;;; DNS types

(defgeneric write-dns-string (buffer string)
  (:method ((buffer dynamic-buffer) (string string))
    (write-ub8 buffer (length string))
    ;; Probably want to use punycode here.
    (write-vector buffer (babel:string-to-octets string :encoding :ascii))))

(defun domain-name-to-dns-format (domain-name)
  (let* ((octets (babel:string-to-octets domain-name :encoding :ascii))
         (tmp-vec (make-array (1+ (length octets)) :element-type 'ub8)))
    (replace tmp-vec octets :start1 1)
    (let ((vector-length (length tmp-vec)))
      (loop :for start-off := 1 :then (1+ end-off)
            :for end-off := (or (position (char-code #\.) tmp-vec
                                          :start start-off)
                                vector-length)
            :do (setf (aref tmp-vec (1- start-off)) (- end-off start-off))
            :when (>= end-off vector-length) do (loop-finish)))
    (values tmp-vec)))

(defgeneric write-domain-name (buffer name)
  (:method ((buffer dynamic-buffer)
            (domain-name string))
    (write-vector buffer (domain-name-to-dns-format domain-name))))

(defgeneric write-record (buffer record)
  (:method ((buffer dynamic-buffer)
            (record dns-question))
    (with-accessors ((name dns-record-name)
                     (type dns-record-type)
                     (class dns-record-class))
        record
      (write-domain-name buffer name)
      (write-ub16 buffer (query-type-number type))
      (write-ub16 buffer (query-class-number class)))))

(defgeneric write-message-header (buffer message)
  (:method ((buffer dynamic-buffer)
            (message dns-message))
    (with-accessors ((id dns-message-id) (flags dns-message-flags)
                     (question dns-message-question) (answer dns-message-answer)
                     (authority dns-message-authority) (additional dns-message-additional))
        message
      (write-ub16 buffer id)
      (write-ub16 buffer flags)
      (write-ub16 buffer (length question))
      (write-ub16 buffer (length answer))
      (write-ub16 buffer (length authority))
      (write-ub16 buffer (length additional)))))

(defgeneric write-dns-message (message)
  (:method ((message dns-message))
    (with-accessors ((question dns-message-question)) message
      (let ((buffer (make-instance 'dynamic-buffer)))
        (write-message-header buffer message)
        (write-record buffer (aref question 0))
        (values buffer)))))

;;;; Resource Record Encoding

(defclass dns-rr (dns-record)
  ((ttl  :initarg :ttl  :accessor dns-rr-ttl)
   (data :initarg :data :accessor dns-rr-data)))

(defmethod print-object ((rr dns-rr) stream)
  (print-unreadable-object (rr stream :type nil :identity nil)
    (with-accessors ((name dns-record-name) (type dns-record-type)
                     (class dns-record-class) (ttl dns-rr-ttl)
                     (data dns-rr-data))
        rr
      (format stream "~S ~A ~A: ~A" name type class
              (decode-rr rr)))))

(defmethod initialize-instance :after ((rr dns-rr) &key)
  (with-accessors ((ttl dns-rr-ttl)) rr
    (check-type ttl (unsigned-byte 32) "a valid TTL")))

(defgeneric add-question (message question)
  (:method ((message dns-message)
            (question dns-question))
    (vector-push-extend question (dns-message-question message))))

(defgeneric add-answer-rr (message record)
  (:method ((message dns-message)
            (record dns-rr))
    (vector-push-extend record (dns-message-answer message))))

(defgeneric add-authority-rr (message record)
  (:method ((message dns-message)
            (record dns-rr))
    (vector-push-extend record (dns-message-authority message))))

(defgeneric add-additional-rr (message record)
  (:method ((message dns-message)
            (record dns-rr))
    (vector-push-extend record (dns-message-additional message))))


(define-condition dns-message-error (error) ()
  (:documentation
   "Signaled when a format error is encountered while parsing a DNS message"))

(defgeneric read-dns-string (buffer)
  (:method ((buffer dynamic-buffer))
    (let ((length (read-ub8 buffer)))
      (babel:octets-to-string (read-vector buffer length) :encoding :ascii))))

(defun read-dns-pointer-recursively (sequence position
                                     &optional (depth 5))
  (when (or (<= depth 0)                          ; too deep recursion
            (>= position (length sequence)))      ; invalid offset
    (error 'dns-message-error))
  (let* ((value (aref sequence position))
         (ms2bits (logand value #xC0)))
    (cond
      ;; it's not a pointer
      ((zerop ms2bits) (cons position (< depth 5)))
      ;; it's a pointer
      ((= ms2bits #xC0)
       ;; there must be at least two bytes to read
       (when (>= position (1+ (length sequence)))
         (error 'dns-message-error))
       (read-dns-pointer-recursively
        sequence
        (logand (read-ub16-from-vector sequence position)
                (lognot #xC000))
        (1- depth)))
      ;; the most significant 2 bits are either 01 or 10
      (t (error 'dns-message-error)))))

(defgeneric dns-domain-name-to-string (buffer)
  (:method ((buffer dynamic-buffer))
    (let (string offset pointer-seen)
      (labels ((%deref-dns-string (pointer rec)
                 (when (not pointer-seen)
                   (cond (rec
                          (setf pointer-seen t)
                          (setf offset (+ (read-cursor-of buffer) 2)))
                         (t
                          (setf offset (+ (read-cursor-of buffer) 1)))))
                 (seek-read-cursor buffer pointer)
                 (setf string (read-dns-string buffer)))
               (%read-tags ()
                 (loop :for (pointer . rec) := (read-dns-pointer-recursively
                                                (sequence-of buffer)
                                                (read-cursor-of buffer))
                       :do (%deref-dns-string pointer rec)
                       :collect string
                       :until (string= string ""))))
        (values (apply #'join "." (%read-tags)) offset)))))

(defgeneric read-domain-name (buffer)
  (:method ((buffer dynamic-buffer))
    (multiple-value-bind (string offset)
        (dns-domain-name-to-string buffer)
      (seek-read-cursor buffer offset)
      (values string))))

(defgeneric read-question (buffer)
  (:method ((buffer dynamic-buffer))
    (let ((name (read-domain-name buffer))
          (type (query-type-id (read-ub16 buffer)))
          (class (query-class-id (read-ub16 buffer))))
      (make-question name type class))))

(defgeneric read-rr-data (buffer type class length))

(defmethod read-rr-data ((buffer dynamic-buffer)
                         (type (eql :a)) (class (eql :in))
                         resource-length)
  (unless (= resource-length 4)
    (error 'dns-message-error))
  (let ((address (make-array 4 :element-type 'ub8)))
    (dotimes (i 4)
      (setf (aref address i) (read-ub8 buffer)))
    address))

(defmethod read-rr-data ((buffer dynamic-buffer)
                         (type (eql :aaaa)) (class (eql :in))
                         resource-length)
  (unless (= resource-length 16)
    (error 'dns-message-error))
  (let ((address (make-array 8 :element-type '(unsigned-byte 16))))
    (dotimes (i 8)
      (setf (aref address i) (read-ub16 buffer)))
    address))

(defmethod read-rr-data ((buffer dynamic-buffer)
                         (type (eql :cname)) (class (eql :in))
                         resource-length)
  (declare (ignore resource-length))
  (read-domain-name buffer))            ; CNAME

(defmethod read-rr-data ((buffer dynamic-buffer)
                         (type (eql :hinfo)) (class (eql :in))
                         resource-length)
  (declare (ignore resource-length))
  (list (read-dns-string buffer)        ; CPU
        (read-dns-string buffer)))      ; OS

(defmethod read-rr-data ((buffer dynamic-buffer)
                         (type (eql :mx)) (class (eql :in))
                         resource-length)
  (declare (ignore resource-length))
  (list (read-ub16 buffer)              ; PREFERENCE
        (read-domain-name buffer)))     ; EXCHANGE

(defmethod read-rr-data ((buffer dynamic-buffer)
                         (type (eql :ns)) (class (eql :in))
                         resource-length)
  (declare (ignore resource-length))
  (read-domain-name buffer))            ; NSDNAME

(defmethod read-rr-data ((buffer dynamic-buffer)
                         (type (eql :ptr)) (class (eql :in))
                         resource-length)
  (declare (ignore resource-length))
  (read-domain-name buffer))            ; PTRDNAME

(defmethod read-rr-data ((buffer dynamic-buffer)
                         (type (eql :soa)) (class (eql :in))
                         resource-length)
  (declare (ignore type class resource-length))
  (list (read-domain-name buffer)       ; MNAME
        (read-domain-name buffer)       ; RNAME
        (read-ub32 buffer)              ; SERIAL
        (read-ub32 buffer)              ; REFRESH
        (read-ub32 buffer)              ; RETRY
        (read-ub32 buffer)              ; EXPIRE
        (read-ub32 buffer)))            ; MINIMUM

(defmethod read-rr-data ((buffer dynamic-buffer)
                         (type (eql :srv)) (class (eql :in))
                         resource-length)
  (declare (ignore resource-length))
  (list (read-ub16 buffer)              ; PRIORITY
        (read-ub16 buffer)              ; WEIGHT
        (read-ub16 buffer)              ; PORT
        (read-domain-name buffer)))     ; TARGET

(defmethod read-rr-data ((buffer dynamic-buffer)
                         (type (eql :txt)) (class (eql :in))
                         resource-length)
  (declare (ignore type class))
  (loop :for string := (read-dns-string buffer) ; TXT-DATA
        :for total-length := (1+ (length string))
        :then (+ total-length 1 (length string))
        :collect string
        :until (>= total-length resource-length)
        :finally (when (> total-length resource-length)
                   (error 'dns-message-error))))

(defmethod read-rr-data ((buffer dynamic-buffer)
                         type class resource-length)
  (declare (ignore buffer type class resource-length))
  (error 'dns-message-error))

(defgeneric read-dns-rr (buffer)
  (:method ((buffer dynamic-buffer))
    (let* ((name (read-domain-name buffer))
           (type (query-type-id (read-ub16 buffer)))
           (class (query-class-id (read-ub16 buffer)))
           (ttl (read-ub32 buffer))
           (rdlen (read-ub16 buffer))
           (rdata (read-rr-data buffer type class rdlen)))
      (make-instance 'dns-rr
                     :name name
                     :type type
                     :class class
                     :ttl ttl
                     :data rdata))))

(defgeneric read-message-header (buffer)
  (:method ((buffer dynamic-buffer))
    (let ((id (read-ub16 buffer))
          (flags (read-ub16 buffer))
          (qdcount (read-ub16 buffer))
          (ancount (read-ub16 buffer))
          (nscount (read-ub16 buffer))
          (arcount (read-ub16 buffer)))
      (make-instance 'dns-message
                     :id id :flags flags
                     :qdcount qdcount :ancount ancount
                     :nscount nscount :arcount arcount))))

(defgeneric read-dns-message (buffer)
  (:method ((buffer dynamic-buffer))
    (let ((msg (read-message-header buffer)))
      (with-accessors ((qdcount dns-message-question-count)
                       (ancount dns-message-answer-count)
                       (nscount dns-message-authority-count)
                       (arcount dns-message-additional-count))
          msg
        (loop :for i :below (dns-message-question-count msg)
              :for q := (read-question buffer)
              :do (add-question msg q))
        (loop :for i :below (dns-message-answer-count msg)
              :for rr := (read-dns-rr buffer)
              :do (add-answer-rr msg rr))
        (loop :for i :below (dns-message-authority-count msg)
              :for rr := (read-dns-rr buffer)
              :do (add-authority-rr msg rr))
        (loop :for i :below (dns-message-additional-count msg)
              :for rr := (read-dns-rr buffer)
              :do (add-additional-rr msg rr)))
      (values msg))))
