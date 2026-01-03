;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; adler32.lisp - computing adler32 checksums (rfc1950) of a byte array

(in-package :crypto)

;;; smallest prime < 65536
(defconstant adler32-modulo 65521)

(defstruct (adler32
             (:constructor %make-adler32-digest nil)
             (:constructor %make-adler32-state (s1 s2))
             (:copier nil))
  (s1 1 :type fixnum)
  (s2 0 :type fixnum))

(defmethod reinitialize-instance ((state adler32) &rest initargs)
  (declare (ignore initargs))
  (setf (adler32-s1 state) 1
        (adler32-s2 state) 0)
  state)

(defmethod copy-digest ((state adler32) &optional copy)
  (check-type copy (or null adler32))
  (cond
    (copy
     (setf (adler32-s1 copy) (adler32-s1 state)
           (adler32-s2 copy) (adler32-s2 copy))
     copy)
    (t
     (%make-adler32-state (adler32-s1 state) (adler32-s2 state)))))

(define-digest-updater adler32
  ;; many thanks to Xach for his code from Salza.
  (let ((length (- end start))
        (i 0)
        (k 0)
        (s1 (adler32-s1 state))
        (s2 (adler32-s2 state)))
    (declare (type index i k length)
             (type fixnum s1 s2))
    (unless (zerop length)
      (tagbody
       loop
         (setf k (min 16 length))
         (decf length k)
       sum
         (setf s1 (+ (aref sequence (+ start i)) s1))
         (setf s2 (+ s1 s2))
         (decf k)
         (incf i)
         (unless (zerop k)
           (go sum))
         (setf s1 (mod s1 adler32-modulo))
         (setf s2 (mod s2 adler32-modulo))
         (unless (zerop length)
           (go loop))
         (setf (adler32-s1 state) s1
               (adler32-s2 state) s2)))
    state))

(define-digest-finalizer (adler32 4)
  (flet ((stuff-state (state digest start)
           (declare (type (simple-array (unsigned-byte 8) (*)) digest))
           (declare (type (integer 0 #.(- array-dimension-limit 4)) start))
           (setf (ub32ref/be digest start)
                 (logior (ash (adler32-s2 state) 16)
                         (adler32-s1 state)))
           digest))
    (declare (inline stuff-state))
    (stuff-state state digest digest-start)))

(defdigest adler32 :digest-length 4 :block-length 1)
