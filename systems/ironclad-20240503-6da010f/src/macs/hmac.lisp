;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; hmac.lisp -- RFC 2104 keyed hashing for message authentication

(in-package :crypto)

(defclass hmac (mac)
  ((inner-digest :reader inner-digest :initarg :inner-digest)
   (outer-digest :reader outer-digest :initarg :outer-digest)))

(defmethod print-object ((mac hmac) stream)
  (print-unreadable-object (mac stream :type nil :identity t)
    (format stream "HMAC(~A)" (type-of (inner-digest mac)))))

(defun make-hmac (key digest-name)
  (make-instance 'hmac :key key
                 :inner-digest (make-digest digest-name)
                 :outer-digest (make-digest digest-name)))

(defmethod reinitialize-instance ((mac hmac) &rest initargs
                                  &key key &allow-other-keys)
  (declare (ignore key initargs))
  (reinitialize-instance (inner-digest mac))
  (reinitialize-instance (outer-digest mac))
  (call-next-method))

(defmethod shared-initialize :after ((mac hmac) slot-names &rest initargs
                              &key key &allow-other-keys)
  (declare (ignore slot-names initargs))
  (declare (type (simple-array (unsigned-byte 8) (*)) key))
  (let* ((inner (inner-digest mac))
         (outer (outer-digest mac))
         (block-length (block-length inner))
         (inner-padding (make-array block-length
                                    :element-type '(unsigned-byte 8)
                                    :initial-element #x36))
         (outer-padding (make-array block-length
                                    :element-type '(unsigned-byte 8)
                                    :initial-element #x5c))
         (padded-key (make-array block-length
                                 :element-type '(unsigned-byte 8)
                                 :initial-element 0)))
    (declare (type simple-octet-vector
                   inner-padding outer-padding padded-key)
             (fixnum block-length))
    ;; XXX: SBCL bogusly ignores this because we use :INITIAL-ELEMENT.
    ;; see also https://bugs.launchpad.net/sbcl/+bug/902351
    (declare (dynamic-extent inner-padding outer-padding padded-key))
    (when (> (length key) block-length)
      (setf key (digest-sequence (type-of inner) key)))
    (replace padded-key key)
    (xor-block block-length padded-key 0 inner-padding 0 inner-padding 0)
    (update-digest inner inner-padding)
    (xor-block block-length padded-key 0 outer-padding 0 outer-padding 0)
    (update-digest outer outer-padding)
    mac))

(defun update-hmac (hmac sequence &key (start 0) (end (length sequence)))
  (declare (type (simple-array (unsigned-byte 8) (*)) sequence))
  (update-digest (inner-digest hmac) sequence :start start :end end)
  hmac)

(defun hmac-digest (hmac &key buffer (buffer-start 0))
  (let* ((x (copy-digest (inner-digest hmac)))
         (inner-hash (produce-digest x :digest buffer :digest-start buffer-start)))
    (copy-digest (outer-digest hmac) x)
    (update-digest x inner-hash :digest buffer :digest-start buffer-start)
    (produce-digest x :digest buffer :digest-start buffer-start)))

(defmac hmac
        make-hmac
        update-hmac
        hmac-digest)
