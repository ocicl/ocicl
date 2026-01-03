;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; elgamal.lisp -- implementation of the ElGamal encryption and signature scheme

(in-package :crypto)


;;; class definitions

(defclass elgamal-key ()
  ((group :initarg :group :reader group)))

(defclass elgamal-public-key (elgamal-key)
  ((y :initarg :y :reader elgamal-key-y :type integer)))

(defclass elgamal-private-key (elgamal-key)
  ((y :initarg :y :reader elgamal-key-y :type integer)
   (x :initarg :x :reader elgamal-key-x :type integer)))

(defun elgamal-key-p (elgamal-key)
  (group-pval (group elgamal-key)))

(defun elgamal-key-g (elgamal-key)
  (group-gval (group elgamal-key)))


;;; function definitions

(defmethod make-public-key ((kind (eql :elgamal))
                            &key p g y &allow-other-keys)
  (unless p
    (error 'missing-key-parameter
           :kind 'elgamal
           :parameter 'p
           :description "modulus"))
  (unless g
    (error 'missing-key-parameter
           :kind 'elgamal
           :parameter 'g
           :description "generator"))
  (unless y
    (error 'missing-key-parameter
           :kind 'elgamal
           :parameter 'y
           :description "public key"))
  (let ((group (make-instance 'discrete-logarithm-group :p p :g g)))
    (make-instance 'elgamal-public-key :group group :y y)))

(defmethod destructure-public-key ((public-key elgamal-public-key))
  (list :p (elgamal-key-p public-key)
        :g (elgamal-key-g public-key)
        :y (elgamal-key-y public-key)))

(defmethod make-private-key ((kind (eql :elgamal))
                             &key p g y x &allow-other-keys)
  (unless p
    (error 'missing-key-parameter
           :kind 'elgamal
           :parameter 'p
           :description "modulus"))
  (unless g
    (error 'missing-key-parameter
           :kind 'elgamal
           :parameter 'g
           :description "generator"))
  (unless x
    (error 'missing-key-parameter
           :kind 'elgamal
           :parameter 'x
           :description "private key"))
  (let ((group (make-instance 'discrete-logarithm-group :p p :g g)))
    (make-instance 'elgamal-private-key :group group :x x :y (or y (expt-mod g x p)))))

(defmethod destructure-private-key ((private-key elgamal-private-key))
  (list :p (elgamal-key-p private-key)
        :g (elgamal-key-g private-key)
        :x (elgamal-key-x private-key)
        :y (elgamal-key-y private-key)))

(defmethod generate-key-pair ((kind (eql :elgamal)) &key num-bits compatible-with-key &allow-other-keys)
  (if compatible-with-key
      (let* ((p (elgamal-key-p compatible-with-key))
             (g (elgamal-key-g compatible-with-key))
             (x (+ 2 (strong-random (- p 3))))
             (y (expt-mod g x p)))
        (values (make-private-key :elgamal :p p :g g :y y :x x)
                (make-public-key :elgamal :p p :g g :y y)))
      (progn
        (unless num-bits
          (error 'missing-key-parameter
                 :kind 'elgamal
                 :parameter 'num-bits
                 :description "modulus size"))
        (let* ((n (if (< num-bits 512)
                      (error 'ironclad-error
                             :format-control "NUM-BITS is too small for an Elgamal key.")
                      256))
               (q (generate-prime n))
               (p (loop for z = (logior (ash 1 (- num-bits n 1))
                                        (dpb 0 (byte 1 0) (random-bits (- num-bits n))))
                        for p = (1+ (* z q))
                        until (and (= num-bits (integer-length p))
                                   (prime-p p))
                        finally (return p)))
               (g (find-subgroup-generator p q))
               (x (+ 2 (strong-random (- p 3))))
               (y (expt-mod g x p)))
          (values (make-private-key :elgamal :p p :g g :y y :x x)
                  (make-public-key :elgamal :p p :g g :y y))))))

(defmethod generate-signature-nonce ((key elgamal-private-key) message &optional p)
  (declare (ignore key message))
  (or *signature-nonce-for-test*
      (loop
        for k = (+ 1 (strong-random (- p 2)))
        until (= 1 (gcd k (- p 1)))
        finally (return k))))
(defmethod generate-signature-nonce ((key elgamal-public-key) message &optional p)
  ;; The name 'generate-signature-nonce' is not really adapted here as it is
  ;; used for encryption. But is it worth adding a new generic function just
  ;; for this case?
  (declare (ignore key message))
  (or *signature-nonce-for-test*
      (loop
        for k = (+ 1 (strong-random (- p 2)))
        until (= 1 (gcd k (- p 1)))
        finally (return k))))

(defmethod make-message ((kind (eql :elgamal)) &key c1 c2 n-bits &allow-other-keys)
  (unless c1
    (error 'missing-message-parameter
           :kind 'elgamal
           :parameter 'c1
           :description "first ciphertext element"))
  (unless c2
    (error 'missing-message-parameter
           :kind 'elgamal
           :parameter 'c2
           :description "second ciphertext element"))
  (unless n-bits
    (error 'missing-message-parameter
           :kind 'elgamal
           :parameter 'n-bits
           :description "modulus size"))
  (concatenate '(simple-array (unsigned-byte 8) (*))
               (integer-to-octets c1 :n-bits n-bits)
               (integer-to-octets c2 :n-bits n-bits)))

(defmethod destructure-message ((kind (eql :elgamal)) message)
  (let ((length (length message)))
    (if (oddp length)
        (error 'invalid-message-length :kind 'elgamal)
        (let* ((middle (/ length 2))
               (n-bits (* middle 8))
               (c1 (octets-to-integer message :start 0 :end middle))
               (c2 (octets-to-integer message :start middle)))
          (list :c1 c1 :c2 c2 :n-bits n-bits)))))

(defmethod encrypt-message ((key elgamal-public-key) msg &key (start 0) end oaep &allow-other-keys)
  (let* ((p (elgamal-key-p key))
         (pbits (integer-length p))
         (g (elgamal-key-g key))
         (y (elgamal-key-y key))
         (m (if oaep
                (octets-to-integer (oaep-encode oaep (subseq msg start end) (/ pbits 8)))
                (octets-to-integer msg :start start :end end)))
         (k (generate-signature-nonce key msg p))
         (c1 (expt-mod g k p))
         (c2 (mod (* m (expt-mod y k p)) p)))
    (unless (< m p)
      (error 'invalid-message-length :kind 'elgamal))
    (make-message :elgamal :c1 c1 :c2 c2 :n-bits pbits)))

(defmethod decrypt-message ((key elgamal-private-key) msg &key (start 0) end n-bits oaep &allow-other-keys)
  (let* ((p (elgamal-key-p key))
         (pbits (integer-length p))
         (end (or end (length msg))))
    (unless (= (* 4 (- end start)) pbits)
      (error 'invalid-message-length :kind 'elgamal))
    (let* ((x (elgamal-key-x key))
           (message-elements (destructure-message :elgamal (subseq msg start end)))
           (c1 (getf message-elements :c1))
           (c2 (getf message-elements :c2))
           (m (mod (* c2 (modular-inverse-with-blinding (expt-mod c1 x p) p)) p)))
      (if oaep
          (oaep-decode oaep (integer-to-octets m :n-bits pbits))
          (integer-to-octets m :n-bits n-bits)))))

(defmethod make-signature ((kind (eql :elgamal)) &key r s n-bits &allow-other-keys)
  (unless r
    (error 'missing-signature-parameter
           :kind 'elgamal
           :parameter 'r
           :description "first signature element"))
  (unless s
    (error 'missing-signature-parameter
           :kind 'elgamal
           :parameter 's
           :description "second signature element"))
  (unless n-bits
    (error 'missing-signature-parameter
           :kind 'elgamal
           :parameter 'n-bits
           :description "modulus size"))
  (concatenate '(simple-array (unsigned-byte 8) (*))
               (integer-to-octets r :n-bits n-bits)
               (integer-to-octets s :n-bits n-bits)))

(defmethod destructure-signature ((kind (eql :elgamal)) signature)
  (let ((length (length signature)))
    (if (oddp length)
        (error 'invalid-signature-length :kind 'elgamal)
        (let* ((middle (/ length 2))
               (n-bits (* middle 8))
               (r (octets-to-integer signature :start 0 :end middle))
               (s (octets-to-integer signature :start middle)))
          (list :r r :s s :n-bits n-bits)))))

(defmethod sign-message ((key elgamal-private-key) msg &key (start 0) end &allow-other-keys)
  (let* ((m (octets-to-integer msg :start start :end end))
         (p (elgamal-key-p key))
         (pbits (integer-length p)))
    (unless (< m (- p 1))
      (error 'invalid-message-length :kind 'elgamal))
    (let* ((g (elgamal-key-g key))
           (x (elgamal-key-x key))
           (k (generate-signature-nonce key msg p))
           (r (expt-mod g k p))
           (s (mod (* (- m (* r x)) (modular-inverse-with-blinding k (- p 1))) (- p 1))))
      (if (not (zerop s))
          (make-signature :elgamal :r r :s s :n-bits pbits)
          (sign-message key msg :start start :end end)))))

(defmethod verify-signature ((key elgamal-public-key) msg signature &key (start 0) end &allow-other-keys)
  (let* ((m (octets-to-integer msg :start start :end end))
         (p (elgamal-key-p key))
         (pbits (integer-length p)))
    (unless (= (* 4 (length signature)) pbits)
      (error 'invalid-signature-length :kind 'elgamal))
    (unless (< m (- p 1))
      (error 'invalid-message-length :kind 'elgamal))
    (let* ((g (elgamal-key-g key))
           (y (elgamal-key-y key))
           (signature-elements (destructure-signature :elgamal signature))
           (r (getf signature-elements :r))
           (s (getf signature-elements :s)))
      (and (< 0 r p)
           (< 0 s (- p 1))
           (= (expt-mod g m p)
              (mod (* (expt-mod y r p) (expt-mod r s p)) p))))))

(defmethod diffie-hellman ((private-key elgamal-private-key) (public-key elgamal-public-key))
  (let ((p (elgamal-key-p private-key))
        (p1 (elgamal-key-p public-key))
        (g (elgamal-key-g private-key))
        (g1 (elgamal-key-g public-key)))
    (unless (and (= p p1) (= g g1))
      (error 'incompatible-keys :kind 'elgamal))
    (let ((pbits (integer-length p))
          (x (elgamal-key-x private-key))
          (y (elgamal-key-y public-key)))
      (integer-to-octets (expt-mod y x p) :n-bits pbits))))
