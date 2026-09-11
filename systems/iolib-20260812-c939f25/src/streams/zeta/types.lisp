;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Various types.
;;;

(in-package :iolib/zstreams)

(deftype octet () '(unsigned-byte  8))
(deftype ub8   () '(unsigned-byte  8))
(deftype ub16  () '(unsigned-byte 16))
(deftype ub32  () '(unsigned-byte 32))
(deftype ub64  () '(unsigned-byte 64))
(deftype sb8   () '(signed-byte  8))
(deftype sb16  () '(signed-byte 16))
(deftype sb32  () '(signed-byte 32))
(deftype sb64  () '(signed-byte 64))

(deftype ub8-vector (&optional (size '*))
  `(array ub8 (,size)))

(deftype ub8-simple-vector (&optional (size '*))
  `(simple-array ub8 (,size)))

(deftype ub8-complex-vector (&optional (size '*))
  `(and (ub8-vector ,size)
        (not (ub8-simple-vector ,size))))

(deftype ub16-vector (&optional (size '*))
  `(array ub16 (,size)))

(deftype ub16-simple-vector (&optional (size '*))
  `(simple-array ub16 (,size)))

(deftype ub16-complex-vector (&optional (size '*))
  `(and (ub16-vector ,size)
        (not (ub16-simple-vector ,size))))

(deftype ub32-vector (&optional (size '*))
  `(array ub32 (,size)))

(deftype ub32-simple-vector (&optional (size '*))
  `(simple-array ub32 (,size)))

(deftype ub32-complex-vector (&optional (size '*))
  `(and (ub32-vector ,size)
        (not (ub32-simple-vector ,size))))

(deftype ub64-vector (&optional (size '*))
  `(array ub64 (,size)))

(deftype ub64-simple-vector (&optional (size '*))
  `(simple-array ub64 (,size)))

(deftype ub64-complex-vector (&optional (size '*))
  `(and (ub64-vector ,size)
        (not (ub64-simple-vector ,size))))

;;;-------------------------------------------------------------------------
;;; Argument Types
;;;-------------------------------------------------------------------------

(deftype stream-buffering ()
  '(member nil :line :full))

(deftype file-direction ()
  '(member :input :output :io))

(deftype file-if-exists ()
  '(member :default :error :error-if-symlink :delete :overwrite))

(deftype file-if-does-not-exist ()
  '(member :default :error :create))

(deftype file-flags ()
  '(unsigned-byte 32))

(deftype file-mode ()
  '(unsigned-byte 32))
