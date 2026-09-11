;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Make DNS queries.
;;;

(in-package :iolib/sockets)

(defvar *dns-recursion-desired* t
  "Whether the \"RECURSION-DESIRED\" field should be set ot not.")

(defvar *dns-repeat* 3
  "The number of times a failed query will be retried.")

(defvar *dns-timeout* 10
  "Timeout for DNS queries in seconds.")

(defconstant +max-16-bits+ (1- (expt 2 16)))

(defun prepare-query (name type)
  (let* ((question (make-question name type :in))
         (query (make-query (random +max-16-bits+)
                            question *dns-recursion-desired*)))
    (write-dns-message query)))

(defun reverse-vector (vector)
  (let* ((vector-length (length vector))
         (reverse-vector
          (make-array vector-length
                      :element-type (array-element-type vector))))
    (loop :for target-index :below vector-length
          :for source-index := (- vector-length target-index 1)
          :do (setf (aref reverse-vector target-index)
                    (aref vector source-index)))
    (values reverse-vector)))

(defun ipv4-dns-ptr-name (address)
  (declare (type ipv4-array address))
  (concatenate 'string (vector-to-dotted (reverse-vector address))
               ".in-addr.arpa."))

(defun ipv6-vector-to-dotted (vector)
  (declare (type ipv6-array vector))
  (with-standard-io-syntax
    (let ((*print-base* 16))
      (with-output-to-string (dotted-address)
        (loop :for index :below (length vector)
              :for element := (aref vector index) :do
              (when (plusp index)
                (princ #\. dotted-address))
              (princ (ldb (byte 4  0) element) dotted-address)
              (princ #\. dotted-address)
              (princ (ldb (byte 4  4) element) dotted-address)
              (princ #\. dotted-address)
              (princ (ldb (byte 4  8) element) dotted-address)
              (princ #\. dotted-address)
              (princ (ldb (byte 4 12) element) dotted-address))))))

(defun ipv6-dns-ptr-name (address)
  (declare (type (simple-array ub16 (8)) address))
  (concatenate 'string (ipv6-vector-to-dotted (reverse-vector address))
               ".ip6.arpa."))

(defun dns-ptr-name (address)
  (multiple-value-bind (vector address-type)
      (address-to-vector address)
    (when (null address)
      (error "The argument is not a valid IP address"))
    (ecase address-type
      (:ipv4 (ipv4-dns-ptr-name vector))
      (:ipv6 (ipv6-dns-ptr-name vector)))))

;;;; Resource Record Decoding

(defgeneric %decode-rr (rr type class))

(defmethod %decode-rr ((rr dns-rr) type class)
  (declare (ignore type class))
  (cons (dns-rr-ttl rr) (dns-rr-data rr)))

(defmethod %decode-rr ((rr dns-rr) (type (eql :cname)) class)
  (declare (ignore class))
  (let ((cname (dns-rr-data rr)))
    (cons (dns-rr-ttl rr)
          (subseq cname 0 (1- (length cname))))))

(defmethod %decode-rr ((rr dns-rr) (type (eql :a)) (class (eql :in)))
  (let ((address (dns-rr-data rr)))
    (cons (dns-rr-ttl rr)
          (make-address address))))

(defmethod %decode-rr ((rr dns-rr) (type (eql :aaaa)) (class (eql :in)))
  (let ((address (dns-rr-data rr)))
    (cons (dns-rr-ttl rr)
          (make-address address))))

(defmethod %decode-rr ((rr dns-rr) (type (eql :ptr)) class)
  (declare (ignore class))
  (let ((name (dns-rr-data rr)))
    (cons (dns-rr-ttl rr)
          (subseq name 0 (1- (length name))))))

(defmethod %decode-rr ((rr dns-rr) (type (eql :mx)) class)
  (declare (ignore class))
  (destructuring-bind (preference name) (dns-rr-data rr)
    (cons (dns-rr-ttl rr)
          (cons preference
                (subseq name 0 (1- (length name)))))))

(defmethod %decode-rr ((rr dns-rr) (type (eql :srv)) class)
  (declare (ignore class))
  (destructuring-bind (priority weight port target) (dns-rr-data rr)
    (list* (dns-rr-ttl rr)
           priority
           weight
           port
           (subseq target 0 (1- (length target))))))

(defun decode-rr (rr)
  (%decode-rr rr (dns-record-type rr) (dns-record-class rr)))

;;;; Response Decoding

(defgeneric %decode-response (dns-message question-type))

(defmethod %decode-response :around ((msg dns-message) question-type)
  (declare (ignore question-type))
  (let ((return-code (rcode-field msg)))
    (if (eql :no-error return-code) ; no error
        (call-next-method)
        (values return-code))))

(defun remove-trailing-dot (string)
  (assert (>= (length string) 2) (string)
          "String length must be at least 2: ~S" string)
  (assert (char= #\. (char string (1- (length string)))) (string)
          "Must end with a dot: ~S" string)
  (subseq string 0 (1- (length string))))

(defun find-cname (msg)
  (let ((answer (dns-message-answer msg))
        (answer-count (dns-message-answer-count msg))
        (cnames (make-hash-table :test 'equal :size 3))
        (consumed 0))
    (loop :for i :below answer-count
          :for ans := (aref answer i) :do
          (if (eql :cname (dns-record-type ans))
              (setf (gethash (dns-record-name ans) cnames)
                    (dns-rr-data ans))
              (loop-finish))
          :finally (setf consumed i))
    (do ((cname (dns-record-name (aref (dns-message-question msg) 0)))
         (exit nil))
        (exit (values (remove-trailing-dot cname) consumed))
      (let ((name (gethash cname cnames)))
        (cond (name
               (remhash cname cnames)
               (setf cname name))
              (t (setf exit t)))))))

(defun decode-a-or-aaaa-response (msg)
  (declare (type dns-message msg))
  (let ((answer (dns-message-answer msg))
        (answer-count (dns-message-answer-count msg))
        (cname nil)
        (first-address-place 0)
        (first-address nil)
        (other-addresses nil))
    ;; when the address is valid(we have at least one answer)
    (when (plusp answer-count)
      (setf (values cname first-address-place) (find-cname msg))
      ;; this means the message actually contains addresses
      (when (> (dns-message-answer-count msg) first-address-place)
        (setf first-address (decode-rr (aref answer first-address-place))))
      (setf other-addresses
            (loop :for i :from (1+ first-address-place)
                  :below (dns-message-answer-count msg)
                  :collect (decode-rr (aref answer i)))))
    (values cname first-address other-addresses)))

(defmethod %decode-response ((msg dns-message) (question-type (eql :a)))
  (declare (ignore question-type))
  (decode-a-or-aaaa-response msg))

(defmethod %decode-response ((msg dns-message) (question-type (eql :aaaa)))
  (declare (ignore question-type))
  (decode-a-or-aaaa-response msg))

(defmethod %decode-response ((msg dns-message) (question-type (eql :ptr)))
  (declare (ignore question-type))
  (decode-rr (aref (dns-message-answer msg) 0)))

;; TODO: got a lot to do here
(defmethod %decode-response ((msg dns-message) (question-type (eql :mx)))
  (declare (ignore question-type))
  (let ((rr (aref (dns-message-answer msg) 0)))
    (decode-rr rr)))

;; TODO: randomly choose between same priority by weight
(defmethod %decode-response ((msg dns-message) (question-type (eql :srv)))
  (declare (ignore question-type))
  (let* ((decoded-rrs (map 'vector #'decode-rr (dns-message-answer msg))))
    (aref (sort decoded-rrs #'< :key #'second) 0)))

(defmethod %decode-response ((msg dns-message) (question-type (eql :txt)))
  (declare (ignore question-type))
  (decode-rr (aref (dns-message-answer msg) 0)))

(defmethod %decode-response ((msg dns-message) question-type)
  (declare (ignore question-type))
  (values msg))

(defun decode-response (message)
  (%decode-response message
                    (dns-record-type
                     (aref (dns-message-question message) 0))))

;;;; DNS-QUERY

(defconstant +dns-port+ 53)

(defun dns-query/udp (buffer length nameserver timeout)
  (with-open-socket
      (socket :type :datagram :ipv6 (ipv6-address-p nameserver)
              :remote-host nameserver :remote-port +dns-port+)
    (send-to socket buffer :end length)
    (iomux:wait-until-fd-ready (fd-of socket) :input timeout t)
    (multiple-value-bind (buf len)
        (receive-from socket :size +dns-max-datagram-size+)
      (values buf len))))

(defun wait-until-socket-connected (socket timeout)
  (if (nth-value 1 (iomux:wait-until-fd-ready (fd-of socket) :output timeout))
      (let ((errcode (socket-option socket :error)))
        (when (minusp errcode) (signal-socket-error errcode (fd-of socket))))
      (error 'socket-connection-timeout-error)))

(defun send-tcp-dns-query (socket buffer length)
  (let ((minibuf (make-array (+ length 2) :element-type 'ub8)))
    ;; two-octet length prefix
    (replace minibuf (ub16-to-vector length))
    (replace minibuf buffer :start1 2 :end2 length)
    (send-to socket minibuf :end (+ length 2))))

(defun get-tcp-query-length (socket timeout)
  (iomux:wait-until-fd-ready (fd-of socket) :input timeout t)
  (multiple-value-bind (minibuf)
      (receive-from socket :size 2)
    (+ (ash (aref minibuf 0) 8)
       (aref minibuf 1))))

(defun receive-tcp-dns-message (socket time-fn)
  (with-accessors ((fd fd-of)) socket
    (let* ((message-length (get-tcp-query-length socket (funcall time-fn)))
           (input-buffer (make-array message-length :element-type 'ub8)))
      (loop :with off := 0 :do
         (iomux:wait-until-fd-ready fd :input (funcall time-fn) t)
         (let ((inbytes (nth-value 1 (receive-from socket :buffer input-buffer :start off))))
           (incf off inbytes)
           (when (= off message-length)
             (return (values input-buffer off))))))))

(defun dns-query/tcp (buffer length nameserver timeout)
  (let* ((t0 (isys:get-monotonic-time))
         (tend (+ t0 timeout)))
    (flet ((remtime ()
             (let ((rem (- tend (isys:get-monotonic-time))))
               (if (not (minusp rem))
                   rem
                   (error 'socket-connection-timeout-error)))))
      (with-open-socket
          (socket :connect :active :type :stream
                  :ipv6 (ipv6-address-p nameserver))
        (handler-case
            (connect socket nameserver :port +dns-port+)
          (socket-connection-in-progress-error ()
            (wait-until-socket-connected socket (remtime))))
        (send-tcp-dns-query socket buffer length)
        (receive-tcp-dns-message socket #'remtime)))))

(defun do-one-dns-query (name type search ns repeat timeout)
  ;; TODO: implement search
  (declare (ignore search))
  (let* ((query (prepare-query name type))
         (buffer (sequence-of query))
         (bufflen (write-cursor-of query))
         (tries-left repeat))
    (labels
        ((start (protocol)
           ;; if the query size fits into a datagram(512 bytes max) do a
           ;; UDP query, otherwise use TCP
           (ecase protocol
             (:udp (query/udp))
             (:tcp (query/tcp))))
         (query/udp ()
           ;; do a UDP query; in case of a socket error, try again
           (handler-case
               (dns-query/udp buffer bufflen ns timeout)
             (socket-error () (%error "UDP socket error"))
             (iomux:poll-timeout () (try-again :udp))
             (:no-error (buf bytes) (parse-response buf bytes))))
         (query/tcp ()
           ;; do a TCP query; in case of a socket error, try again
           (handler-case
               (dns-query/tcp buffer bufflen ns timeout)
             (socket-connection-timeout-error () (try-again :tcp))
             (socket-error () (%error "TCP socket error"))
             (iomux:poll-timeout () (try-again :tcp))
             (:no-error (buf bytes) (parse-response buf bytes t))))
         (parse-response (buf bytes &optional on-tcp)
           ;; try to parse the response; in case of a parse error, try again
           (handler-case
               (read-dns-message (make-instance 'dynamic-buffer :sequence buf :size bytes))
             (dynamic-buffer-input-error () (try-again :tcp))
             (dns-message-error () (try-again :tcp))
             (:no-error (response)
               ;; if a truncated response was received by UDP, try TCP
               ;; if it was received by TCP, err
               (if (truncated-field response)
                   (if on-tcp (%error "TCP truncated messae") (try-again :tcp))
                   (return-response response)))))
         (try-again (protocol)
           ;; if no response received and there are tries left, try again
           (if (plusp (decf tries-left)) (start protocol) (%error "No more retries left")))
         (return-response (response) response)
         (%error (&optional cause) (declare (ignore cause))))
      (start :udp))))

(defun preprocess-dns-name (name type)
  (if (eql :ptr type)
      (dns-ptr-name name)
      name))

(defun dns-query (name &key (type :a) (search *dns-search-domain*)
                  (nameservers *dns-nameservers*) decode
                  (repeat *dns-repeat*) (timeout *dns-timeout*))
  (setf nameservers (ensure-list nameservers))
  (assert nameservers (nameservers) "Must supply a nameserver")
  (let ((pname (preprocess-dns-name name type)))
    (dolist (ns (mapcar #'ensure-address nameservers))
      (when-let (response (do-one-dns-query pname type search
                                            ns repeat timeout))
        (return* (if decode (decode-response response) response))))))
