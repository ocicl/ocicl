;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- High-level name lookup.
;;;

(in-package :iolib/sockets)

(defconstant +max-ipv4-value+ (1- (expt 2 32))
  "Integer denoting 255.255.255.255")

;;;; High-level Interface

;;; TODO: caching

(defun reply-error-condition (reply query-type)
  (cond ((null reply) 'resolver-again-error)
        ((dns-flag-p reply :name-error) 'resolver-no-name-error)
        ((or (dns-flag-p reply :format-error)
             (dns-flag-p reply :server-failure)
             (dns-flag-p reply :not-implemented))
         'resolver-fail-error)
        ((loop :for rr :across (dns-message-answer reply)
               :never (eql query-type (dns-record-type rr)))
         'resolver-no-name-error)))

(defun check-reply-for-errors (reply host query-type)
  (when-let (condition (reply-error-condition reply query-type))
    (error condition :data host)))

(defun dns-lookup-host-by-address (address)
  (let ((reply (dns-query address :type :ptr)))
    (check-reply-for-errors reply address :ptr)
    (let ((hostname (remove-trailing-dot
                     (dns-rr-data (aref (dns-message-answer reply) 0)))))
      (values address '()
              hostname
              (list (cons hostname address))))))

(defun lookup-host-by-address (address ipv6)
  (cond ((and (eql :ipv6 ipv6)
              (ipv4-address-p address))
         (setf address (map-ipv4-address-to-ipv6 address)))
        ((and (member ipv6 '(nil t))
              (ipv6-ipv4-mapped-p address))
         (setf address (map-ipv6-address-to-ipv4 address))))
  (nth-value-or 0
    (search-host-by-address address)
    (dns-lookup-host-by-address address)))

(defun process-one-reply (reply query-type)
  (let ((truename (dns-record-name (aref (dns-message-question reply) 0)))
        addresses aliases)
    (loop :for rr :across (dns-message-answer reply) :do
       (switch ((dns-record-type rr) :test #'eq)
         (:cname (setf truename (dns-rr-data rr)))
         (query-type (let ((address (ensure-address (dns-rr-data rr)))
                           (name (remove-trailing-dot (dns-record-name rr))))
                       (push address addresses)
                       (push (cons name address) aliases)))
         (t (warn "Invalid RR type: ~S" (dns-record-type rr)))))
    (let ((addresses (nreverse addresses)))
      (values (car addresses) (cdr addresses)
              (remove-trailing-dot truename)
              (nreverse aliases)))))

(defun dns-lookup-host-in-one-domain (host query-type)
  (let ((reply (dns-query host :type query-type)))
    (check-reply-for-errors reply host query-type)
    (process-one-reply reply query-type)))

(defun merge-a-and-aaaa-replies (4-reply 6-reply)
  (multiple-value-bind (4-main 4-addresses 4-truename 4-aliases)
      (process-one-reply 4-reply :a)
    (multiple-value-bind (6-main 6-addresses 6-truename 6-aliases)
        (process-one-reply 6-reply :aaaa)
      (declare (ignore 6-truename))
      (values 4-main (nconc 4-addresses (list 6-main) 6-addresses)
              4-truename
              (nconc 4-aliases 6-aliases)))))

(defun dns-lookup-host-in-a-and-aaaa (host)
  (let* ((4-reply (dns-query host :type :a))
         (4-err (reply-error-condition 4-reply :a))
         (6-reply (dns-query host :type :aaaa))
         (6-err (reply-error-condition 6-reply :aaaa)))
    (cond
      ((and 4-err 6-err)
       (if (member 'resolver-fail-error (list 4-err 6-err))
           (error 'resolver-fail-error :data host)
           (error 'resolver-no-name-error :data host)))
      (4-err (process-one-reply 6-reply :aaaa))
      (6-err (process-one-reply 4-reply :a))
      (t (merge-a-and-aaaa-replies 4-reply 6-reply)))))

(defun dns-lookup-host-by-name (host ipv6)
  (case ipv6
    ((nil)   (dns-lookup-host-in-one-domain host :a))
    ((:ipv6) (dns-lookup-host-in-one-domain host :aaaa))
    ((t)     (dns-lookup-host-in-a-and-aaaa host))))

(defun lookup-host-by-name (host ipv6)
  (nth-value-or 0
    (search-host-by-name host ipv6)
    (dns-lookup-host-by-name host ipv6)))

;; TODO: * implement address selection as per RFC 3484
;;       * add caching
;;       * profile the whole thing
(defun lookup-hostname (host &key (ipv6 *ipv6*) ns)
  "Looks up a host by name or address. IPV6 determines the
IPv6 behaviour, defaults to *IPV6*.
Use NS to specify a (list of) nameservers, overriding *DNS-NAMESERVERS*.
Returns 4 values:
* an address
* a list of additional addresses(if existent)
* the canonical name of the host
* an alist of all the host's names with their respective addresses."
  (check-type ipv6 *ipv6*-type "one of T, NIL or :IPV6")
  (let ((address (ensure-address host :errorp (not (stringp host))))
        (ns (mapcar #'ensure-address (ensure-list ns))))
    (update-monitor *resolv.conf-monitor*)
    (update-monitor *hosts-monitor*)
    (let ((*dns-nameservers* (or ns *dns-nameservers*)))
      (cond
        (address
         (lookup-host-by-address address ipv6))
        (t
         (check-type host string "a string")
         (lookup-host-by-name (idna:to-ascii host) ipv6))))))

(defun lookup-host (&rest args)
  (apply #'lookup-hostname args))

(defobsolete lookup-host lookup-hostname)

(defun ensure-hostname (address &key (ipv6 *ipv6*) (errorp t))
  "If ADDRESS is an inet-address designator, it is converted, if
necessary, to an INET-ADDRESS object and returned.  Otherwise it
is assumed to be a host name which is then looked up in order to
return its primary address as the first return value and the
remaining address list as the second return value."
  (flet ((%do-ensure-hostname ()
           (or (ensure-address address :family :internet :errorp nil)
               (nth-value 0 (lookup-hostname address :ipv6 ipv6)))))
    (if errorp
        (%do-ensure-hostname)
        (ignore-some-conditions (socket-error resolver-error)
          (%do-ensure-hostname)))))
