;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Various helpers for bsd-sockets.
;;;

(in-package :iolib/sockets)

;;;; Types

(deftype ipv4-array () '(ub8-sarray 4))
(deftype ipv6-array () '(ub16-sarray 8))

;;;; Conversion between address formats

(defun copy-simple-array-ub16-to-alien-vector (lisp-vec alien-vec)
  (declare (type ipv6-array lisp-vec))
  (dotimes (i 8)
    (setf (mem-aref alien-vec :uint16 i)
          (htons (aref lisp-vec i)))))

(defun map-ipv4-vector-to-ipv6 (addr)
  (declare (type ipv4-array addr))
  (let ((ipv6addr (make-array 8 :element-type 'ub16
                              :initial-element 0)))
    ;; setting the IPv4 marker
    (setf (aref ipv6addr 5) #xFFFF)
    ;; setting the first two bytes
    (setf (aref ipv6addr 6) (+ (ash (aref addr 0) 8)
                               (aref addr 1)))
    ;; setting the last two bytes
    (setf (aref ipv6addr 7) (+ (ash (aref addr 2) 8)
                               (aref addr 3)))
    (values ipv6addr)))

(defun map-ipv6-vector-to-ipv4 (addr)
  (declare (type ipv6-array addr))
  (let ((ipv4addr (make-array 4 :element-type 'ub8
                              :initial-element 0)))
    (setf (aref ipv4addr 0) (ldb (byte 8 8) (aref addr 6)))
    (setf (aref ipv4addr 1) (ldb (byte 8 0) (aref addr 6)))
    (setf (aref ipv4addr 2) (ldb (byte 8 8) (aref addr 7)))
    (setf (aref ipv4addr 3) (ldb (byte 8 0) (aref addr 7)))
    (values ipv4addr)))

;;; From CLOCC's PORT library.
(defun vector-to-integer (vector)
  "Convert a vector to a 32-bit unsigned integer."
  (coercef vector 'ipv4-array)
  (+ (ash (aref vector 0) 24)
     (ash (aref vector 1) 16)
     (ash (aref vector 2) 8)
     (aref vector 3)))

(defun integer-to-vector (ipaddr)
  "Convert a 32-bit unsigned integer to a vector."
  (check-type ipaddr ub32 "an '(unsigned-byte 32)")
  (let ((vector (make-array 4 :element-type 'ub8)))
    (setf (aref vector 0) (ldb (byte 8 24) ipaddr)
          (aref vector 1) (ldb (byte 8 16) ipaddr)
          (aref vector 2) (ldb (byte 8  8) ipaddr)
          (aref vector 3) (ldb (byte 8  0) ipaddr))
    vector))

(defun in6-addr-to-ipv6-array (in6-addr)
  (let ((vector (make-array 8 :element-type 'ub16)))
    (dotimes (i 8)
      (setf (aref vector i)
            (ntohs (mem-aref in6-addr :uint16 i))))
    vector))

;;;; Constructors for SOCKADDR_* structs

(defun make-sockaddr-in (sin ub8-vector &optional (portno 0))
  (declare (type ipv4-array ub8-vector) (type ub16 portno))
  (isys:bzero sin (isys:sizeof '(:struct sockaddr-in)))
  (with-foreign-slots ((family addr port) sin (:struct sockaddr-in))
    (setf family af-inet)
    (setf addr (htonl (vector-to-integer ub8-vector)))
    (setf port (htons portno)))
  (values sin))

(defmacro with-sockaddr-in ((var address &optional (port 0)) &body body)
  `(with-foreign-object (,var '(:struct sockaddr-in))
     (make-sockaddr-in ,var ,address ,port)
     ,@body))

(defun make-sockaddr-in6 (sin6 ub16-vector &optional (portno 0))
  (declare (type ipv6-array ub16-vector) (type ub16 portno))
  (isys:bzero sin6 (isys:sizeof '(:struct sockaddr-in6)))
  (with-foreign-slots ((family addr port) sin6 (:struct sockaddr-in6))
    (setf family af-inet6)
    (copy-simple-array-ub16-to-alien-vector ub16-vector addr)
    (setf port (htons portno)))
  (values sin6))

(defmacro with-sockaddr-in6 ((var address &optional port) &body body)
  `(with-foreign-object (,var '(:struct sockaddr-in6))
     (make-sockaddr-in6 ,var ,address ,port)
     ,@body))

(defun make-sockaddr-un (sun string abstract)
  (declare (type string string))
  (isys:bzero sun (isys:sizeof '(:struct sockaddr-un)))
  (with-foreign-slots ((family path) sun (:struct sockaddr-un))
    (setf family af-local)
    (let* ((address-string
            (concatenate 'string (when abstract (string #\Null)) string))
           (path-length (length address-string))
           (sun-path-len
            (load-time-value
             (- (isys:sizeof '(:struct sockaddr-un))
                (foreign-slot-offset '(:struct sockaddr-un) 'path)))))
      (assert (< path-length sun-path-len))
      (with-foreign-string (c-string address-string :null-terminated-p nil)
        (isys:memcpy (foreign-slot-pointer sun '(:struct sockaddr-un) 'path)
                     c-string path-length))))
  (values sun))

(defun actual-size-of-sockaddr-un (sun)
  (let ((path-ptr (foreign-slot-pointer sun '(:struct sockaddr-un) 'path))
        (sun-path-len
         (load-time-value
          (- (isys:sizeof '(:struct sockaddr-un))
             (foreign-slot-offset '(:struct sockaddr-un) 'path)))))
    (loop :for i :from 1 :below sun-path-len
          :if (zerop (mem-aref path-ptr :char i))
          :do (return (+ i (foreign-slot-offset '(:struct sockaddr-un) 'path)))
          :finally (bug "Invalid sockaddr_un struct: slot sun_path contains invalid C string"))))

(defmacro with-sockaddr-un ((var address abstract) &body body)
  `(with-foreign-object (,var '(:struct sockaddr-un))
     (make-sockaddr-un ,var ,address ,abstract)
     ,@body))

#+linux
(defun make-sockaddr-nl (snl multicast-groups &optional (portno 0))
  (declare (type ub32 multicast-groups)
           (type ub32 portno))
  (isys:bzero snl (isys:sizeof '(:struct sockaddr-nl)))
  (with-foreign-slots ((family groups port) snl (:struct sockaddr-nl))
    (setf family af-netlink)
    (setf groups multicast-groups)
    (setf port portno))
  (values snl))

#+linux
(defmacro with-sockaddr-nl ((var multicast-groups &optional (port 0)) &body body)
  `(with-foreign-object (,var '(:struct sockaddr-nl))
     (make-sockaddr-nl ,var ,multicast-groups ,port)
     ,@body))

(defmacro with-sockaddr-storage ((var) &body body)
  `(with-foreign-object (,var '(:struct sockaddr-storage))
     (isys:bzero ,var (isys:sizeof '(:struct sockaddr-storage)))
     ,@body))

(defmacro with-socklen ((var value) &body body)
  `(with-foreign-object (,var 'socklen-t)
     (setf (mem-aref ,var 'socklen-t) ,value)
     ,@body))

(defmacro with-sockaddr-storage-and-socklen ((ss-var size-var) &body body)
  `(with-sockaddr-storage (,ss-var)
     (with-socklen (,size-var (isys:sizeof '(:struct sockaddr-storage)))
       ,@body)))

;;;; Misc

(defun ensure-number (value &key (start 0) end (radix 10) (type t) (errorp t))
  (let ((parsed
         (typecase value
           (string
            (ignore-errors (parse-integer value :start start :end end
                                          :radix radix :junk-allowed nil)))
           (t value))))
    (cond
      ((typep parsed type) parsed)
      (errorp (error 'parse-error)))))

(defun ensure-string-or-unsigned-byte (thing &key (type t) (radix 10) (errorp t))
  (or (and (symbolp thing) (string-downcase thing))
      (ensure-number thing :type type :radix radix :errorp nil)
      (and (stringp thing) thing)
      (if errorp (error 'parse-error) nil)))

(defun lisp->c-bool (val)
  (if val 1 0))

;; FIXME: perhaps return some very large value instead of NIL
(defun wait->timeout (wait)
  (case wait
    ((nil) 0)
    ((t)   nil)
    (t     wait)))

(defun compute-flags (flags args &optional env)
  (loop :with flag-combination := 0
        :for cons :on args :by #'cddr
        :for flag := (car cons)
        :for val := (cadr cons)
        :for const := (cdr (assoc flag flags))
        :when const
        :do (when (not (constantp val env)) (return* nil))
            (setf flag-combination (logior flag-combination const))
        :finally (return flag-combination)))

(defun set-function-docstring (function docstring)
  (setf (documentation function 'function) docstring))

(defun unset-method-docstring (gf qualifiers specializers)
  (setf (documentation (find-method gf qualifiers (mapcar #'find-class specializers)) t) nil))
