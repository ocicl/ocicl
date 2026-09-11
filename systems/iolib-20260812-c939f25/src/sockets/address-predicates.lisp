;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Predicate functions for addresses.
;;;

(in-package :iolib/sockets)

;;;; Well-known addresses

;;; Defines a constant address.  We need ENSURE-ADDRESS at compile
;;; time which is why these functions are in a separate file from
;;; address.lisp.
(defmacro define-address (name address-string docstring)
  `(defconstant (,name :test 'address=) (ensure-address ,address-string)
     ,(format nil "~A (~A)" docstring address-string)))

(define-address +ipv4-unspecified+ "0.0.0.0"   "Unspecified IPv4 address.")
(define-address +ipv4-loopback+    "127.0.0.1" "Loopback IPv4 address.")
(define-address +ipv6-unspecified+ "::"        "Unspecified IPv6 address.")
(define-address +ipv6-loopback+    "::1"       "Loopback IPv6 address.")

(define-symbol-macro +any-host+
    (if *ipv6* +ipv6-unspecified+ +ipv4-unspecified+))

(define-symbol-macro +loopback+
    (if *ipv6* +ipv6-loopback+ +ipv4-loopback+))

;;;; Multicast addresses replacing IPv4 broadcast addresses

(define-address +ipv6-interface-local-all-nodes+ "ff01::1"
  "Interface local all nodes address.")

(define-address +ipv6-link-local-all-nodes+ "ff02::1"
  "Link local all nodes address.")

(define-address +ipv6-interface-local-all-routers+ "ff01::2"
  "Interface local all routers address.")

(define-address +ipv6-link-local-all-routers+ "ff02::2"
  "Link local all routers address.")

(define-address +ipv6-site-local-all-routers+ "ff05::2"
  "Site local all routers address.")

;;;; Predicates

(defun addressp (address)
  "Returns T if ADDRESS is an object of class ADDRESS.  Does not
return T for other low-level address representations."
  (typep address 'address))

(defun inet-address-p (address)
  "Returns T if ADDRESS is an Inet address object."
  (typep address 'inet-address))

(defun ipv4-address-p (address)
  "Returns T if ADDRESS is an IPv4 address object."
  (typep address 'ipv4-address))

(defun ipv6-address-p (address)
  "Returns T if ADDRESS is an IPv6 address object."
  (typep address 'ipv6-address))

(defun local-address-p (address)
  "Returns T if ADDRESS is a local address object."
  (typep address 'local-address))

(defgeneric address-type (address)
  (:documentation "Returns a keyword symbol denoting the kind of
ADDRESS (:IPV4, :IPV6 or :LOCAL).  If ADDRESS is not a known
address object, NIL is returned.")
  (:method ((address ipv4-address)) :ipv4)
  (:method ((address ipv6-address)) :ipv6)
  (:method ((address local-address)) :local)
  (:method (address)
    (declare (ignore address))
    nil))

(defgeneric inet-address-unspecified-p (addr)
  (:documentation "Returns T if ADDR is an \"unspecified\" internet address.")
  (:method ((address ipv4-address))
    (address= address +ipv4-unspecified+))
  (:method ((address ipv6-address))
    (address= address +ipv6-unspecified+)))

(defgeneric inet-address-loopback-p (address)
  (:documentation "Returns T if ADDRESS is a loopback internet address.")
  (:method ((address ipv4-address))
    (address= address +ipv4-loopback+))
  (:method ((address ipv6-address))
    (address= address +ipv6-loopback+)))

(defgeneric inet-address-multicast-p (address)
  (:documentation "Returns T if ADDRESS is an multicast internet address.")
  (:method ((address ipv4-address))
    (= (logand (aref (address-name address) 0) #xE0)
       #xE0))
  (:method ((address ipv6-address))
    (= (logand (aref (address-name address) 0) #xFF00)
       #xFF00)))

(defgeneric inet-address-unicast-p (address)
  (:documentation "Returns T if ADDRESS is an unicast internet address.")
  (:method ((address ipv4-address))
    (and (not (inet-address-unspecified-p address))
         (not (inet-address-loopback-p address))
         (not (inet-address-multicast-p address))))
  (:method ((address ipv6-address))
    (or (ipv6-link-local-unicast-p address)
        (and (not (inet-address-unspecified-p address))
             (not (inet-address-loopback-p address))
             (not (inet-address-multicast-p address))))))

(defun ipv6-ipv4-mapped-p (address)
  "Returns T if ADDRESS is an IPv6 address representing an IPv4
mapped address."
  (and (ipv6-address-p address)
       (ipv4-on-ipv6-mapped-vector-p (address-name address))))

(defun ipv6-interface-local-multicast-p (address)
  "Returns T if ADDRESS is an interface-local IPv6 address."
  (check-type address ipv6-address "an IPv6 address")
  (= (logand (aref (address-name address) 0) #xFF0F)
     #xFF01))

(defun ipv6-link-local-multicast-p (address)
  "Returns T if ADDRESS is a link-local IPv6 address."
  (check-type address ipv6-address "an IPv6 address")
  (= (logand (aref (address-name address) 0) #xFF0F)
     #xFF02))

(defun ipv6-admin-local-multicast-p (address)
  "Returns T if ADDRESS is a admin-local multicast IPv6 address."
  (check-type address ipv6-address "an IPv6 address")
  (= (logand (aref (address-name address) 0) #xFF0F)
     #xFF04))

(defun ipv6-site-local-multicast-p (address)
  "Returns T if ADDRESS is an site-local multicast IPv6 address."
  (check-type address ipv6-address "an IPv6 address")
  (= (logand (aref (address-name address) 0) #xFF0F)
     #xFF05))

(defun ipv6-organization-local-multicast-p (address)
  "Returns T if ADDRESS is an organization-local multicast IPv6 address."
  (check-type address ipv6-address "an IPv6 address")
  (= (logand (aref (address-name address) 0) #xFF0F)
     #xFF08))

(defun ipv6-global-multicast-p (address)
  "Returns T if ADDRESS is a global multicast IPv6 address."
  (check-type address ipv6-address "an IPv6 address")
  (= (logand (aref (address-name address) 0) #xFF0F)
     #xFF0E))

(defun ipv6-reserved-multicast-p (address)
  "Returns T if ADDRESS is a reserved multicast IPv6 address."
  (check-type address ipv6-address "an IPv6 address")
  (member (logand (aref (address-name address) 0) #xFF0F)
          '(#xFF00 #xFF03 #xFF0F)))

(defun ipv6-unassigned-multicast-p (address)
  "Returns T if ADDRESS is an unassigned multicast IPv6 address."
  (check-type address ipv6-address "an IPv6 address")
  (member (logand (aref (address-name address) 0) #xFF0F)
          '(#xFF06 #xFF07 #xFF09 #xFF0A #xFF0B #xFF0C #xFF0D)))

(defun ipv6-transient-multicast-p (address)
  "Returns T if ADDRESS is a transient multicast IPv6 address."
  (check-type address ipv6-address "an IPv6 address")
  (= (logand (aref (address-name address) 0) #xFF10)
     #xFF10))

(defun ipv6-solicited-node-multicast-p (address)
  "Returns T if ADDRESS is a solicited-node multicast IPv6 address."
  (check-type address ipv6-address "an IPv6 address")
  (let ((vec (address-name address)))
    (and (= (aref vec 0) #xFF02)      ; link-local permanent multicast
         (= (aref vec 5) 1)
         (= (logand (aref vec 6) #xFF00)
            #xFF00))))

(defun ipv6-link-local-unicast-p (address)
  "Returns T if ADDRESS is an link-local unicast IPv6 address."
  (check-type address ipv6-address "an IPv6 address")
  (= (aref (address-name address) 0) #xFE80))

(defun ipv6-site-local-unicast-p (address)
  "Returns T if ADDRESS is an site-local unicast IPv6 address."
  (check-type address ipv6-address "an IPv6 address")
  (= (aref (address-name address) 0) #xFEC0))

(defun ipv6-global-unicast-p (address)
  "Returns T if ADDRESS is an global unicasst IPv6 address."
  (check-type address ipv6-address "an IPv6 address")
  (and (not (inet-address-unspecified-p address))
       (not (inet-address-loopback-p address))
       (not (inet-address-multicast-p address))
       (not (ipv6-link-local-unicast-p address))))

(defun ipv6-multicast-type (address)
  "Returns the multicast type of ADDRESS or NIL if it's not a
multicast address."
  (check-type address ipv6-address "an IPv6 address")
  (cond
    ((ipv6-interface-local-multicast-p address) :interface-local)
    ((ipv6-link-local-multicast-p address) :link-local)
    ((ipv6-admin-local-multicast-p address) :admin-local)
    ((ipv6-site-local-multicast-p address) :site-local)
    ((ipv6-organization-local-multicast-p address) :organization-local)
    ((ipv6-global-multicast-p address) :global)
    ((ipv6-reserved-multicast-p address) :reserved)
    ((ipv6-unassigned-multicast-p address) :unassigned)))

(defun ipv6-unicast-type (address)
  "Returns the unicast type of ADDRESS or NIL if it's not a
unicast address."
  (check-type address ipv6-address "an IPv6 address")
  (cond
    ((ipv6-link-local-unicast-p address) :link-local)
    ((ipv6-site-local-unicast-p address) :site-local)
    ((ipv6-global-unicast-p address) :global)))

(defgeneric inet-address-type (address)
  (:documentation "Returns the address type of ADDRESS as 2 values:

  * protocol, one of :IPV4 or :IPV6
  * kind, one of :UNSPECIFIED, :LOOPBACK, :MULTICAST or :UNICAST

For unicast or multicast IPv6 addresses, a third value is
returned which corresponds to the return value of
IPV6-UNICAST-TYPE or IPV6-MULTICAST-TYPE, respectively." ))

(defmethod inet-address-type ((addr ipv6-address))
  (cond
    ((inet-address-unspecified-p addr) (values :ipv6 :unspecified))
    ((inet-address-loopback-p addr) (values :ipv6 :loopback))
    ((inet-address-multicast-p addr)
     (values :ipv6 :multicast (ipv6-multicast-type addr)))
    (t (values :ipv6 :unicast (ipv6-unicast-type addr)))))

(defmethod inet-address-type ((addr ipv4-address))
  (values :ipv4
          (cond
            ((inet-address-unspecified-p addr) :unspecified)
            ((inet-address-loopback-p addr) :loopback)
            ((inet-address-multicast-p addr) :multicast)
            ((inet-address-unicast-p addr) :unicast))))
