;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; dsa.lisp -- implementation of the Digital Signature Algorithm

(in-package :crypto)


;;; class definitions

(defclass dsa-key ()
  ((group :initarg :group :reader group)))

(defclass dsa-public-key (dsa-key)
  ((y :initarg :y :reader dsa-key-y :type integer)))

(defclass dsa-private-key (dsa-key)
  ((y :initarg :y :reader dsa-key-y :type integer)
   (x :initarg :x :reader dsa-key-x :type integer)))

(defun dsa-key-p (dsa-key)
  (group-pval (group dsa-key)))
(defun dsa-key-q (dsa-key)
  (group-qval (group dsa-key)))
(defun dsa-key-g (dsa-key)
  (group-gval (group dsa-key)))


;;; function definitions

(defmethod make-public-key ((kind (eql :dsa))
                            &key p q g y &allow-other-keys)
  (unless p
    (error 'missing-key-parameter
           :kind 'dsa
           :parameter 'p
           :description "modulus"))
  (unless q
    (error 'missing-key-parameter
           :kind 'dsa
           :parameter 'q
           :description "subgroup modulus"))
  (unless g
    (error 'missing-key-parameter
           :kind 'dsa
           :parameter 'g
           :description "generator"))
  (unless y
    (error 'missing-key-parameter
           :kind 'dsa
           :parameter 'y
           :description "public key"))
  (let ((group (make-instance 'discrete-logarithm-group :p p :q q :g g)))
    (make-instance 'dsa-public-key :group group :y y)))

(defmethod destructure-public-key ((public-key dsa-public-key))
  (list :p (dsa-key-p public-key)
        :q (dsa-key-q public-key)
        :g (dsa-key-g public-key)
        :y (dsa-key-y public-key)))

(defmethod make-private-key ((kind (eql :dsa))
                             &key p q g y x &allow-other-keys)
  (unless p
    (error 'missing-key-parameter
           :kind 'dsa
           :parameter 'p
           :description "modulus"))
  (unless q
    (error 'missing-key-parameter
           :kind 'dsa
           :parameter 'q
           :description "subgroup modulus"))
  (unless g
    (error 'missing-key-parameter
           :kind 'dsa
           :parameter 'g
           :description "generator"))
  (unless x
    (error 'missing-key-parameter
           :kind 'dsa
           :parameter 'x
           :description "private key"))
  (let ((group (make-instance 'discrete-logarithm-group :p p :q q :g g)))
    (make-instance 'dsa-private-key :group group :x x :y (or y (expt-mod g x p)))))

(defmethod destructure-private-key ((private-key dsa-private-key))
  (list :p (dsa-key-p private-key)
        :q (dsa-key-q private-key)
        :g (dsa-key-g private-key)
        :x (dsa-key-x private-key)
        :y (dsa-key-y private-key)))

(defmethod generate-key-pair ((kind (eql :dsa)) &key num-bits &allow-other-keys)
  (unless num-bits
    (error 'missing-key-parameter
           :kind 'dsa
           :parameter 'num-bits
           :description "modulus size"))
  (let* ((n (cond ((< num-bits 512) (error 'ironclad-error
                                           :format-control "NUM-BITS is too small for a DSA key."))
                  ((<= num-bits 1024) 160)
                  ((<= num-bits 2048) 224)
                  ((<= num-bits 3072) 256)
                  ((<= num-bits 7680) 384)
                  ((<= num-bits 15360) 512)
                  (t (error 'ironclad-error
                            :format-control "NUM-BITS is too big for a DSA key."))))
         (q (generate-prime n))
         (p (loop for z = (logior (ash 1 (- num-bits n 1))
                                  (dpb 0 (byte 1 0) (random-bits (- num-bits n))))
                  for p = (1+ (* z q))
                  until (and (= num-bits (integer-length p))
                             (prime-p p))
                  finally (return p)))
         (g (find-subgroup-generator p q))
         (x (+ 2 (strong-random (- q 2))))
         (y (expt-mod g x p)))
    (values (make-private-key :dsa :p p :q q :g g :y y :x x)
            (make-public-key :dsa :p p :q q :g g :y y))))

(defmethod generate-signature-nonce ((key dsa-private-key) message &optional q)
  (declare (ignore key message))
  (or *signature-nonce-for-test*
      (1+ (strong-random (1- q)))))

(defmethod make-signature ((kind (eql :dsa)) &key r s n-bits &allow-other-keys)
  (unless r
    (error 'missing-signature-parameter
           :kind 'dsa
           :parameter 'r
           :description "first signature element"))
  (unless s
    (error 'missing-signature-parameter
           :kind 'dsa
           :parameter 's
           :description "second signature element"))
  (unless n-bits
    (error 'missing-signature-parameter
           :kind 'dsa
           :parameter 'n-bits
           :description "subgroup modulus size"))
  (concatenate '(simple-array (unsigned-byte 8) (*))
               (integer-to-octets r :n-bits n-bits)
               (integer-to-octets s :n-bits n-bits)))

(defmethod destructure-signature ((kind (eql :dsa)) signature)
  (let ((length (length signature)))
    (if (oddp length)
        (error 'invalid-signature-length :kind 'dsa)
        (let* ((middle (/ length 2))
               (n-bits (* middle 8))
               (r (octets-to-integer signature :start 0 :end middle))
               (s (octets-to-integer signature :start middle)))
          (list :r r :s s :n-bits n-bits)))))

;;; Note that hashing is not performed here.
(defmethod sign-message ((key dsa-private-key) message &key (start 0) end &allow-other-keys)
  (let* ((end (or end (length message)))
         (q (dsa-key-q key))
         (qbits (integer-length q)))
    (when (> (* 8 (- end start)) qbits)
      ;; Only keep the required number of bits of message
      (setf end (+ start (/ qbits 8))))
    (let* ((m (octets-to-integer message :start start :end end))
           (p (dsa-key-p key))
           (g (dsa-key-g key))
           (x (dsa-key-x key))
           (k (generate-signature-nonce key message q))
           (r (mod (expt-mod g k p) q))
           (k-inverse (modular-inverse-with-blinding k q))
           (s (mod (* k-inverse (+ (* x r) m)) q)))
      (assert (= (mod (* k k-inverse) q) 1))
      (if (not (or (zerop r) (zerop s)))
          (make-signature :dsa :r r :s s :n-bits qbits)
          (sign-message key message :start start :end end)))))

(defmethod verify-signature ((key dsa-public-key) message signature &key (start 0) end &allow-other-keys)
  (let* ((end (or end (length message)))
         (q (dsa-key-q key))
         (qbits (integer-length q)))
    (unless (= (* 4 (length signature)) qbits)
      (error 'invalid-signature-length :kind 'dsa))
    (when (> (* 8 (- end start)) qbits)
      ;; Only keep the required number of bits of message
      (setf end (+ start (/ qbits 8))))
    (let* ((m (octets-to-integer message :start start :end end))
           (p (dsa-key-p key))
           (g (dsa-key-g key))
           (y (dsa-key-y key))
           (signature-elements (destructure-signature :dsa signature))
           (r (getf signature-elements :r))
           (s (getf signature-elements :s)))
      (unless (and (< 0 r q) (< 0 s q))
        (return-from verify-signature nil))
      (let* ((w (modular-inverse s q))
             (u1 (mod (* m w) q))
             (u2 (mod (* r w) q))
             (v (mod (mod (* (expt-mod g u1 p) (expt-mod y u2 p)) p) q)))
        (= v r)))))
