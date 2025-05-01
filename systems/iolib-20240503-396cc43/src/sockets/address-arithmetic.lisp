;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Arithmetic with addresses and network masks.
;;;

(in-package :iolib/sockets)

(defun make-netmask (&key cidr class)
  "Create a subnet mask by specifying either its class(:A, :B or :C) or
a CIDR suffix(a number between 0 and 32)."
  (assert (or cidr class) (cidr class) "You must either specify a CIDR or a network class.")
  (cond
    (cidr (check-type cidr (mod 33) "a number between 0 and 32"))
    (class (check-type class (member :a :b :c)
                       "a valid network class - one of :A, :B or :C")
           (setf cidr (case class (:a 8) (:b 16) (:c 24)))))
  (let ((mask #xFFFFFFFF))
    (declare (type ub32 mask))
    (setf (ldb (byte (- 32 cidr) 0) mask) 0)
    (make-instance 'ipv4-address :name (integer-to-vector mask))))

(defun ensure-netmask (thing)
  "If THING is of type IPV4-ADDRESS it is returned as is; if keyword it must be one of
:A, :B or :C otherwise it's treated as a CIDR suffix."
  (etypecase thing
    (ipv4-address  thing)
    (unsigned-byte (make-netmask :cidr thing))
    (keyword       (make-netmask :class thing))))

(defgeneric inet-address-network-portion (address netmask)
  (:documentation "Apply network netmask NETMASK to ADDRESS in order to calculate the
network part of ADDRESS.")
  (:method ((address ipv4-address) netmask)
    (setf netmask (ensure-netmask netmask))
    (let ((v (make-array 4 :element-type 'ub8))
          (av (address-name address))
          (mv (address-name netmask)))
      (dotimes (i 4)
        (setf (aref v i)
              (logand (aref av i)
                      (aref mv i))))
      (make-instance 'ipv4-address :name v))))

(defgeneric inet-address-host-portion (address netmask)
  (:documentation "Apply network netmask NETMASK to ADDRESS in order to calculate the
host part of ADDRESS.")
  (:method ((address ipv4-address) netmask)
    (setf netmask (ensure-netmask netmask))
    (let ((v (make-array 4 :element-type 'ub8))
          (av (address-name address))
          (mv (address-name netmask)))
      (dotimes (i 4)
        (setf (aref v i)
              (logand (aref av i)
                      (logxor (aref mv i) 255))))
      (make-instance 'ipv4-address :name v))))

(defclass ipv4-network ()
  ((address :accessor address-of)
   (netmask :accessor netmask-of)
   (cidr :accessor cidr-of))
  (:documentation "IPv4 network: an address plus a netmask."))

(declaim (inline count-trailing-zeroes/32))
(defun count-trailing-zeroes/32 (n)
  (declare (optimize speed)
           (type (unsigned-byte 32) n))
  (1- (integer-length (logand n (- n)))))

(defun cidr-subnet-zeroes (netmask)
  (count-trailing-zeroes/32 (vector-to-integer (address-name netmask))))

(defmethod initialize-instance :after ((network ipv4-network)
                                       &key address netmask)
  (check-type address ipv4-address "an Ipv4 address")
  (check-type netmask ipv4-address "an Ipv4 netmask")
  (setf (cidr-of network) (- 32 (cidr-subnet-zeroes netmask)))
  (setf (netmask-of network) netmask)
  (setf (address-of network)
        (inet-address-network-portion address netmask)))

(defmethod print-object ((network ipv4-network) stream)
  (let ((namestring
         (format nil "~A/~A"
                 (address-to-string (address-of network))
                 (cidr-of network))))
    (if (or *print-readably* *print-escape*)
        (format stream "#/~S/~A" 'net namestring)
        (write-string namestring stream))))

(defgeneric ipv4-network= (net1 net2)
  (:documentation "Returns T if the addresses and the netmasks of the
two arguments are respectively ADDRESS=.")
  (:method ((net1 ipv4-network) (net2 ipv4-network))
    (and (address= (address-of net1) (address-of net2))
         (address= (netmask-of net1) (netmask-of net2)))))

(defgeneric inet-address-in-network-p (address network)
  (:documentation "Return T if ADDRESS is part of the subnet specified by NETWORK.")
  (:method ((address ipv4-address) (network ipv4-network))
    (address= (inet-address-network-portion address (netmask-of network))
              (address-of network))))

(defgeneric inet-addresses-in-same-network-p (address1 address2 network)
  (:documentation "Return T if ADDRESS1 and ADDRESS2 are both part part of the
subnet specified by NETWORK.")
  (:method ((address1 ipv4-address) (address2 ipv4-address) (network ipv4-network))
    (let ((address1-network (inet-address-network-portion address1 (netmask-of network)))
          (address2-network (inet-address-network-portion address2 (netmask-of network))))
      (and (address= address1-network (address-of network))
           (address= address2-network (address-of network))))))

(defgeneric inet-address-network-class (address)
  (:documentation "Return the network class of ADDRESS: one of :A, :B, :C, :D or :E .")
  (:method ((address ipv4-address))
    (let ((octet (aref (address-name address) 0)))
      (cond
        ((= #b0000 (ldb (byte 1 7) octet)) :a) ;   0.0.0.0 - 127.255.255.255
        ((= #b0010 (ldb (byte 2 6) octet)) :b) ; 128.0.0.0 - 191.255.255.255
        ((= #b0110 (ldb (byte 3 5) octet)) :c) ; 192.0.0.0 - 223.255.255.255
        ((= #b1110 (ldb (byte 4 4) octet)) :d) ; 224.0.0.0 - 239.255.255.255
        ((= #b1111 (ldb (byte 4 4) octet)) :e) ; 240.0.0.0 - 255.255.255.255
        ))))

(defgeneric inet-address-private-p (address)
  (:documentation "Returns T if ADDRESS is in a private network range.
Private IPv4 networks are 10.0.0.0/8, 172.16.0.0/12 and 192.168.0.0/16.
See http://en.wikipedia.org/wiki/Private_network for details.")
  (:method ((address ipv4-address))
    (let* ((address-name (address-name address))
           (first (aref address-name 0))
           (second (aref address-name 1)))
      (values (or (= first 10)
                  (and (= first 172)
                       (<= 16 second 31))
                  (and (= first 192)
                       (= second 168)))
              (inet-address-network-class address))))
  (:method ((address address))
    nil))
