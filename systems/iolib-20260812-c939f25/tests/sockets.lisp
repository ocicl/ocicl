;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- iolib/sockets test suite.
;;;

(in-package :iolib/tests)

(in-suite :iolib/sockets)

(iolib.base:enable-literal-reader)

(defparameter *echo-address* (lookup-hostname "iolib.cddr.org"))
(defparameter *echo-port* 25877)
(defparameter *echo-timeout* 2)

;;;; Addresses

;;; a real address
(test (address-to-vector.ipv4.string.1 :compile-at :definition-time)
  (is (equalp (address-to-vector "127.0.0.1")
              (values #(127 0 0 1) :ipv4))))

;;; and an address with bit 8 set on some octets
(test (address-to-vector.ipv4.string.2 :compile-at :definition-time)
  (is (equalp (address-to-vector "242.1.211.3")
              (values #(242 1 211 3) :ipv4))))

;;; a real address
(test (address-to-vector.ipv4.vector.1 :compile-at :definition-time)
  (is (equalp (address-to-vector #(127 0 0 1))
              (values #(127 0 0 1) :ipv4))))

(test (address-to-vector.ipv6.string.1 :compile-at :definition-time)
  (is (equalp (address-to-vector "::")
              (values #(0 0 0 0 0 0 0 0) :ipv6))))

(test (address-to-vector.ipv6.vector.1 :compile-at :definition-time)
  (is (equalp (address-to-vector #(0 0 0 0 0 0 0 0))
              (values #(0 0 0 0 0 0 0 0) :ipv6))))

;;; RT: used to return the PARSE-ERROR as a secondary value.
(test (string-address-to-vector.1 :compile-at :definition-time)
  (is-false (string-address-to-vector "256.0.0.1")))

;;; RT: should only ignore PARSE-ERRORs.
(test (string-address-to-vector.2 :compile-at :definition-time)
  (signals type-error
    (string-address-to-vector 'not-a-string)))

;;; RT: should signal a PARSE-ERROR when given an invalid string.
(test (ensure-address.1 :compile-at :definition-time)
  (signals parse-error
    (ensure-address "ff0x::114")))

;;; ditto
(test (ensure-address.2 :compile-at :definition-time)
  (signals parse-error
    (ensure-address "127.0.256.1")))

(test (ensure-address.3 :compile-at :definition-time)
  (is-false
   (or (ensure-address "ff0x::114" :errorp nil)
       (ensure-address "127.0.256.1" :errorp nil))))

(test (integer-to-dotted-and-back :compile-at :definition-time)
  (is-true
   (every (lambda (s) (string= s (integer-to-dotted (dotted-to-integer s))))
          '("0.0.255.0" "0.255.255.0" "0.255.255.1"))))

(test (integer-to-dotted.1 :compile-at :definition-time)
  (is (string= (integer-to-dotted 0)
               "0.0.0.0")))

(test (integer-to-dotted.2 :compile-at :definition-time)
  (is (string= (integer-to-dotted +max-ipv4-value+)
               "255.255.255.255")))

(test (integer-to-dotted.3 :compile-at :definition-time)
  (signals type-error
    (integer-to-dotted (1+ +max-ipv4-value+))))

(test (integer-to-dotted.4 :compile-at :definition-time)
  (signals type-error
    (integer-to-dotted -1)))

(test (dotted-to-vector.1 :compile-at :definition-time)
  (is (equalp (mapcar #'dotted-to-vector '("255.255.255.255" "0.0.0.0" "127.0.0.1"))
              '(#(255 255 255 255) #(0 0 0 0) #(127 0 0 1)))))

(test (dotted-to-vector.2 :compile-at :definition-time)
  (signals parse-error
    (dotted-to-vector "127.0.0.0.0")))

(test (dotted-to-vector.3 :compile-at :definition-time)
  (signals type-error
    (dotted-to-vector 'not-a-string)))

(test (vector-to-dotted.1 :compile-at :definition-time)
  (is (equalp (mapcar #'vector-to-dotted '(#(255 255 255 255) #(0 0 0 0) (127 0 0 1)))
              '("255.255.255.255" "0.0.0.0" "127.0.0.1"))))

(test (vector-to-dotted.2 :compile-at :definition-time)
  (signals type-error
    (vector-to-dotted #(127 0 0 256))))

(test (address-to-string.1 :compile-at :definition-time)
  (is (equalp (mapcar (lambda (x) (address-to-string (make-address x)))
                      '(#(127 0 0 1) #(255 255 255 255) #(0 0 0 0 0 0 0 0)
                        #(0 0 0 0 0 0 0 1) #(1 0 0 0 0 0 0 0)))
              '("127.0.0.1" "255.255.255.255" "::" "::1" "1::"))))

(test (vector-to-colon-separated.1 :compile-at :definition-time)
  (is (equalp (let ((ip  #(0 0 0 255 255 255 0 0)))
                (values (vector-to-colon-separated ip)
                        (vector-to-colon-separated ip :downcase)
                        (vector-to-colon-separated ip :upcase)))
              (values "::ff:ff:ff:0:" "::ff:ff:ff:0:" "::FF:FF:FF:0:"))))

(test (vector-to-colon-separated.2 :compile-at :definition-time)
  (is (string= (vector-to-colon-separated #(1 2 3 4 5 0 6 7))
               "1:2:3:4:5::6:7")))

(test (vector-to-colon-separated.3 :compile-at :definition-time)
  (is (string= (vector-to-colon-separated #(0 2 3 4 5 0 6 7))
               ":2:3:4:5::6:7")))

(test (vector-to-colon-separated.4 :compile-at :definition-time)
  (is (string= (vector-to-colon-separated #(1 2 3 4 5 0 6 0))
               "1:2:3:4:5::6:")))

(test (colon-separated-to-vector.1 :compile-at :definition-time)
  (is (equalp (mapcar #'colon-separated-to-vector
                      '(":ff::ff:" "::" "::1" "1::" ":2:3:4:5:6:7:8" "1:2:3:4:5:6:7:"
                        ":1::2:" "::127.0.0.1" ":1::127.0.0.1"))
              '(#(0 #xff 0 0 0 0 #xff 0)
                #(0 0 0 0 0 0 0 0)
                #(0 0 0 0 0 0 0 1)
                #(1 0 0 0 0 0 0 0)
                #(0 2 3 4 5 6 7 8)
                #(1 2 3 4 5 6 7 0)
                #(0 1 0 0 0 0 2 0)
                #(0 0 0 0 0 0 #x7f00 1)
                #(0 1 0 0 0 0 #x7f00 1)))))

(test (address=.1 :compile-at :definition-time)
  (is-true (address= +ipv4-loopback+ (make-address #(127 0 0 1)))))

(test (address=.2 :compile-at :definition-time)
  (is-true (address= +ipv6-loopback+ (ensure-address "::1"))))

(test (copy-address.1 :compile-at :definition-time)
  (is-true (loop for designator in (list "127.0.0.1" +max-ipv4-value+ "::" "::1")
              for addr1 = (ensure-address designator)
              for addr2 = (ensure-address designator)
              for addr3 = (copy-address addr1)
              always (and (address= addr1 addr2)
                          (address= addr1 addr3)
                          (address= addr2 addr3)))))

(test (make-address.1 :compile-at :definition-time)
  (signals type-error
    (make-address 'not-a-valid-designator)))

(test (address.unspecified.1 :compile-at :definition-time)
  (is-true (every #'inet-address-unspecified-p
                  (mapcar #'ensure-address '("0.0.0.0" "::" "0:0:0:0:0:0:0:0")))))

(test (address.loopback.1 :compile-at :definition-time)
  (is-true (every #'inet-address-loopback-p
                  (mapcar #'ensure-address '("127.0.0.1" "::1" "0:0::1")))))

(test (address.multicast.1 :compile-at :definition-time)
  (is-true (every #'inet-address-multicast-p
                  (mapcar #'ensure-address
                          '("224.0.0.0" "224.1.2.3" "232.0.0.127" "239.255.255.255"
                            "ff02::2" "ff0a::114" "ff05::1:3")))))

(test (address.ipv6-ipv4-mapped.1 :compile-at :definition-time)
  (is-true (ipv6-ipv4-mapped-p (ensure-address "::ffff:127.0.0.1"))))

(test (address.ipv6.1 :compile-at :definition-time)
  (is (equalp (address-to-vector "::1.2.3.4")
              (values #(0 0 0 0 0 0 #x0102 #x0304) :ipv6))))

;;;; Host Lookup

(defparameter *google-ns*
  (list #/ip/8.8.8.8 #/ip/8.8.4.4))

(test (missing-hosts-file :compile-at :definition-time)
  (is-false (iolib/sockets::parse-/etc/hosts "/foo/no-such-file")))

#-no-internet-available
(test (lookup-hostname.1 :compile-at :definition-time)
  (is (equalp (multiple-value-bind (address addresses truename)
                  (lookup-hostname "a.root-servers.net" :ipv6 nil :ns *google-ns*)
                (declare (ignore addresses))
                (values (address-equal-p address #(198 41 0 4))
                        truename))
              (values t "a.root-servers.net"))))

#-no-internet-available
(test (lookup-hostname.2 :compile-at :definition-time)
  (is-true (string= (nth-value 2 (lookup-hostname #(198 41 0 4) :ns *google-ns*))
                    "a.root-servers.net")))

(test (lookup-hostname.3 :compile-at :definition-time)
  (signals resolver-no-name-error
    (lookup-hostname "foo.tninkpad.telent.net." :ns *google-ns*)))

(test (lookup-hostname.4 :compile-at :definition-time)
  (is-true (address-equal-p (lookup-hostname #(127 0 0 1) :ipv6 nil :ns *google-ns*)
                            #(127 0 0 1))))

(test (lookup-hostname.5 :compile-at :definition-time)
  (signals parse-error
    (lookup-hostname #(127 0 0) :ns *google-ns*)))

(test (lookup-hostname.6 :compile-at :definition-time)
  (signals resolver-no-name-error
    (lookup-hostname #(127 0 0 1) :ipv6 :ipv6 :ns *google-ns*)))

;;;; Service Lookup

(test (lookup-service.1 :compile-at :definition-time)
  (is (equalp (lookup-service "ssh")
              (values 22 "ssh" :tcp))))

(test (lookup-service.2 :compile-at :definition-time)
  (is (equalp (lookup-service 53 :udp)
              (values 53 "domain" :udp))))

;;; looks up a reserved service port
(test (lookup-service.3 :compile-at :definition-time)
  ;; TODO: check for a more specific error.
  (signals unknown-service
    (lookup-service 1024)))

;;;; Protocol Lookup

(test (lookup-protocol.1 :compile-at :definition-time)
  (is (equalp (multiple-value-bind (number name)
                  (lookup-protocol "tcp")
                (values number name))
              (values 6 "tcp"))))

(test (lookup-protocol.2 :compile-at :definition-time)
  (is (equalp (multiple-value-bind (number name)
                  (lookup-protocol "udp")
                (values number name))
              (values 17 "udp"))))

(test (lookup-protocol.3 :compile-at :definition-time)
  (signals unknown-protocol
    (lookup-protocol "nonexistent-protocol")))

(test (lookup-protocol.4 :compile-at :definition-time)
  (is-true (string= (nth-value 1 (lookup-protocol 6))
                    "tcp")))

;;;; Network Interfaces

(test (list-network-interfaces.1 :compile-at :definition-time)
  (is-true (<= 1 (length (list-network-interfaces)))))

(test (network-interfaces.1 :compile-at :definition-time)
  (is-true
   (flet ((nif-equal (if1 if2)
            (check-type if1 cons)
            (check-type if2 cons)
            (and (string= (interface-name if1) (interface-name if2))
                 (eql (interface-index if1) (interface-index if2)))))
     (loop for nif in (list-network-interfaces)
        always (and (nif-equal nif (lookup-interface (interface-name nif)))
                    (nif-equal nif (lookup-interface (interface-index nif))))))))

;;;; Sockets

;;; RT: don't accept unknown keyword arguments, such as typos.
(test (make-socket.unknown-keyword.error.function)
  (signals error
    (locally
        (declare (notinline make-socket))
      (make-socket :this-kw-arg-doesnt-exist t))))

;;; RT: don't accept unknown keyword arguments, such as typos.
(test (make-socket.unknown-keyword.error.compiler-macro :compile-at :definition-time)
  (signals error
    (funcall
     (alexandria:ignore-some-conditions (warning error)
       (compile nil '(lambda () (make-socket :this-kw-arg-doesnt-exist t)))))))

(test (make-socket.2 :compile-at :definition-time)
  (is (equalp (with-open-socket (s :address-family :ipv4)
                (values (socket-connected-p s)
                        (socket-open-p s)
                        (> (socket-os-fd s) 1)
                        (socket-address-family s)
                        (socket-protocol s)))
              (values nil t t :ipv4 :default)))) ; why isn't it :TCP?

(test (make-socket.3 :compile-at :definition-time)
  (is-true (with-open-socket (s :address-family :ipv4)
             (typep s 'socket))))

;;; Given the functions we've got so far, if you can think of a better
;;; way to make sure the bind succeeded than trying it twice, let me
;;; know. 1974 has no special significance, unless you're the same age
;;; as me.
(test (inet.socket-bind.1 :compile-at :definition-time)
  (signals socket-address-in-use-error
    (with-open-socket (s1 :address-family :ipv4 :connect :passive
                          :local-host #(127 0 0 1) :local-port 1974)
      (with-open-socket (s2 :address-family :ipv4 :connect :passive
                            :local-host #(127 0 0 1) :local-port 1974)
        (values s1 s2)))))

(test (sockopt.1 :compile-at :definition-time)
  (is-true (with-open-socket (s :address-family :ipv4)
             (setf (socket-option s :reuse-address) t)
             (socket-option s :reuse-address))))

;;; Like READ-SEQUENCE, but returns early if the full quantity of data
;;; isn't there to be read.  Blocks if no input at all.
(defun read-buf-nonblock (buffer stream)
  (let ((eof (gensym)))
    (do ((i 0 (1+ i))
         (c (read-char stream nil eof)
            (read-char-no-hang stream nil eof)))
        ((or (>= i (length buffer)) (not c) (eq c eof)) i)
      (setf (elt buffer i) c))))

(test (simple-tcp-client :compile-at :definition-time)
  (is-true
   (with-open-socket (s :remote-host *echo-address* :remote-port *echo-port*
                        :address-family :ipv4)
     (setf (socket-option s :receive-timeout) *echo-timeout*)
     (let ((data (make-string 200)))
       (format s "here is some text")
       (finish-output s)
       (let ((data (subseq data 0 (read-buf-nonblock data s))))
         ;; (format t "~&Got ~S back from TCP echo server~%" data)
         (> (length data) 0))))))

(test (sockaddr-return-type :compile-at :definition-time)
  (is-true
   (with-open-socket (s :remote-host *echo-address* :remote-port *echo-port*
                        :address-family :ipv4)
     (setf (socket-option s :receive-timeout) *echo-timeout*)
     (and (ipv4-address-p (remote-host s))
          (numberp (remote-port s))))))

;;; We don't support streams with UDP sockets ATM.  But when we do,
;;; let's add a similar test using stream functions.
;;;
;;; FIXME: figure out why this test blocks with the inetd services on
;;; my machines, on both Darwin and Linux/x86-64.  Works with
;;; echo-server.c though --luis
(test (simple-udp-client.1 :compile-at :definition-time)
  (is-true
   (with-open-socket (s :remote-host *echo-address* :remote-port *echo-port*
                        :type :datagram :address-family :ipv4)
     (send-to s #(1 2 3 4 5))
     (let ((nbytes (nth-value 1 (handler-bind ((isys:ewouldblock
                                                (lambda (e)
                                                  (invoke-restart (find-restart 'retry-syscall e)
                                                                  *echo-timeout*))))
                                  (receive-from s :size 200)))))
       (plusp nbytes)))))

(test (simple-udp-client.2 :compile-at :definition-time)
  (is-true
   (with-open-socket (s :type :datagram :address-family :ipv4)
     (send-to s #(1 2 3 4 5)
              :remote-host *echo-address*
              :remote-port *echo-port*)
     (let ((nbytes (nth-value 1 (handler-bind ((isys:ewouldblock
                                                (lambda (e)
                                                  (invoke-restart (find-restart 'retry-syscall e)
                                                                  *echo-timeout*))))
                                  (receive-from s :size 200)))))
       (plusp nbytes)))))

(test (simple-local-sockets :compile-at :definition-time)
  (is (string= (let ((file (namestring
                            (make-pathname :name "local-socket" :type nil
                                           :defaults (asdf:component-pathname
                                                      (asdf:find-system :iolib/tests))))))
                 (ignore-errors (delete-file file))
                 (with-open-socket (p :address-family :local :connect :passive :local-filename file)
                   (with-open-socket (a :address-family :local :remote-filename file)
                     (format a "local socket test")
                     (finish-output a))
                   (let ((s (accept-connection p)))
                     (prog1 (read-line s)
                       (close s)
                       (delete-file file)))))
               "local socket test")))

(defmacro with-http-stream ((var host port request) &body body)
  `(with-open-socket (,var :address-family :ipv4 :remote-host ,host :remote-port ,port)
     (format ,var ,(concatenate 'string request " HTTP/1.0~%~%"))
     (finish-output ,var)
     ,@body))

#-no-internet-available
(test (simple-http-client :compile-at :definition-time)
  (is-true
   (with-http-stream (s "www.google.com" 80 "HEAD /")
     (let ((data (make-string 200)))
       (setf data (subseq data 0 (read-buf-nonblock data s)))
       ;; (princ data)
       (> (length data) 0)))))

#-no-internet-available
(test (sockopt-receive-buffer :compile-at :definition-time)
  ;; on Linux x86, the receive buffer size appears to be doubled in the
  ;; kernel: we set a size of x and then getsockopt() returns 2x.
  ;; This is why we compare with >= instead of =
  (is-true
   (with-http-stream (s "www.google.com" 80 "HEAD/")
     (setf (socket-option s :receive-buffer) 1975)
     (let ((data (make-string 200)))
       (setf data (subseq data 0 (read-buf-nonblock data s)))
       (and (> (length data) 0)
            (>= (socket-option s :receive-buffer) 1975))))))

(test (socket-open-p.1 :compile-at :definition-time)
  (is-true (with-open-socket (s)
             (socket-open-p s))))

(test (socket-open-p.2 :compile-at :definition-time)
  (is-true (with-open-socket (s :remote-host *echo-address* :remote-port *echo-port*
                                :address-family :ipv4)
             (socket-open-p s))))

(test (socket-open-p.3 :compile-at :definition-time)
  (is-false (with-open-socket (s)
              (close s)
              (socket-open-p s))))

(test (socket-open-p.4 :compile-at :definition-time)
  (is-false (with-open-socket (s :remote-host *echo-address* :remote-port *echo-port*
                                 :address-family :ipv4)
              (close s)
              (socket-open-p s))))

(test (open-stream-p.1 :compile-at :definition-time)
  (is-true (with-open-socket (s)
             (open-stream-p s))))

(test (open-stream-p.2 :compile-at :definition-time)
  (is-true (with-open-socket (s :remote-host *echo-address* :remote-port *echo-port*
                                :address-family :ipv4)
             (open-stream-p s))))

(test (open-stream-p.3 :compile-at :definition-time)
  (is-false (with-open-socket (s)
              (close s)
              (open-stream-p s))))

(test (open-stream-p.4 :compile-at :definition-time)
  (is-false (with-open-socket (s :remote-host *echo-address* :remote-port *echo-port*
                                 :address-family :ipv4)
              (close s)
              (open-stream-p s))))

;;; we don't have an automatic test for some of this yet.  There's no
;;; simple way to run servers and have something automatically connect
;;; to them as client, unless we spawn external programs.  Then we
;;; have to start telling people what external programs they should
;;; have installed.  Which, eventually, we will, but not just yet

;;; to check with this: can display packets from multiple peers
;;; peer address is shown correctly for each packet
;;; packet length is correct
;;; long (>500 byte) packets have the full length shown (doesn't work)
#-(and)
(defun udp-server (port)
  (with-open-socket (s :type :datagram :local-port port)
    (loop
       (multiple-value-bind (buf len address port)
           (receive-from s :size 500)
         (format t "Received ~A bytes from ~A:~A - ~A ~%"
                 len address port (subseq buf 0 (min 10 len)))))))
