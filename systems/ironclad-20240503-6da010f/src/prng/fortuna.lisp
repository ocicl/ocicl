;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; fortuna.lisp -- Fortuna PRNG

(in-package :crypto)


(defparameter +min-pool-size+
  128
  "Minimum pool size before a reseed is allowed.  This should be the
  number of bytes of pool data that are likely to contain 128 bits of
  entropy.  Defaults to a pessimistic estimate of 1 bit of entropy per
  byte.")

(defparameter +fortuna-seed-length+ 64)

(defclass fortuna-pool ()
  ((digest :initform (make-digest :sha256))
   (length :initform 0))
  (:documentation "A Fortuna entropy pool.  DIGEST contains its current
  state; LENGTH the length in bytes of the entropy it contains."))

(defclass fortuna-prng ()
  ((pools :initform (loop for i from 1 to 32
                       collect (make-instance 'fortuna-pool)))
   (reseed-count :initform 0)
   (last-reseed :initform 0)
   (generator))
  (:documentation "A Fortuna random number generator.  Contains 32
  entropy pools which are used to reseed GENERATOR."))

(defmethod prng-random-data (num-bytes (prng fortuna-prng))
  (when (plusp num-bytes)
    (with-slots (pools generator reseed-count last-reseed) prng
      (when (and (>= (slot-value (first pools) 'length) +min-pool-size+)
                 (> (- (get-internal-run-time) last-reseed) 100))
        (incf reseed-count)
        (loop for i from 0 below (length pools)
           with seed = (make-array (* (digest-length :sha256)
                                      (integer-length
                                       (logand reseed-count
                                               (- reseed-count))))
                                   :element-type '(unsigned-byte 8))
           while (zerop (mod reseed-count (expt 2 i)))
           collect (with-slots (digest length) (nth i pools)
                     (let ((digest-length (digest-length digest)))
                       (produce-digest digest
                                       :digest seed
                                       :digest-start (* i digest-length))
                       (reinitialize-instance digest)
                       (digest-sequence digest seed
                                        :digest seed
                                        :start (* i digest-length)
                                        :end (* (1+ i) digest-length)
                                        :digest-start (* i digest-length))
                       (setf length 0)
                       (reinitialize-instance digest)))
           finally (prng-reseed seed generator)))
      (assert (plusp reseed-count))
      (prng-random-data num-bytes generator))))

(defun add-random-event (source pool-id event &optional (prng *prng*))
  (declare (type fortuna-prng prng))
  (assert (and (<= 1 (length event) 32)
               (<= 0 source 255)
               (<= 0 pool-id 31)))
  (let ((pool (nth pool-id (slot-value prng 'pools))))
    (update-digest (slot-value pool 'digest)
                            (concatenate '(vector (unsigned-byte 8))
                                         (integer-to-octets source)
                                         (integer-to-octets
                                          (length event))
                                         event))
    (incf (slot-value pool 'length) (length event))))

(defmethod prng-seed-length ((prng fortuna-prng))
  +fortuna-seed-length+)

(defmethod prng-reseed (seed (prng fortuna-prng))
  (declare (type simple-octet-vector seed))
  (assert (= (length seed) +fortuna-seed-length+))
  (prng-reseed seed (slot-value prng 'generator))
  (incf (slot-value prng 'reseed-count)))

(defun make-fortuna (cipher)
  (let ((prng (make-instance 'fortuna-prng)))
    (setf (slot-value prng 'generator)
          (make-instance 'fortuna-generator :cipher cipher))
    prng))

(defmethod make-prng ((name (eql :fortuna)) &key seed (cipher :aes))
  (declare (ignorable seed))
  (make-fortuna cipher))

;; FIXME: this is more than a little ugly; maybe there should be a
;; prng-registry or something?
(defmethod make-prng ((name (eql 'fortuna)) &key seed (cipher :aes))
  (declare (ignorable seed))
  (make-fortuna cipher))
