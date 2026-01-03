;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; os-prng.lisp -- OS-provided pseudo-random number generator

(in-package :crypto)

#+unix
(defparameter *os-prng-stream* nil)
#+unix
(defparameter *os-prng-stream-lock* (bt:make-lock))

(defclass os-prng ()
  ())

(defmethod prng-random-data (num-bytes (prng os-prng))
  #+unix
  (let* ((seq (make-array num-bytes :element-type '(unsigned-byte 8)))
         (n (bt:with-lock-held (*os-prng-stream-lock*)
              (unless (and *os-prng-stream* (open-stream-p *os-prng-stream*))
                (setf *os-prng-stream* (open #P"/dev/urandom"
                                             #+ccl :sharing #+ccl :external
                                             :element-type '(unsigned-byte 8))))
              (read-sequence seq *os-prng-stream*))))
    (if (< n num-bytes)
        (error 'ironclad-error :format-control "Failed to get random data.")
        seq))

  #+(and win32 sbcl)
  (sb-win32:crypt-gen-random num-bytes)

  #+(and os-windows ccl)
  (multiple-value-bind (buff buffp)
      (ccl:make-heap-ivector num-bytes '(unsigned-byte 8))
    (when (zerop (ccl:external-call "SystemFunction036"
                                    :address buffp
                                    :unsigned-long num-bytes
                                    :boolean))
      (error 'ironclad-error :format-control "RtlGenRandom failed"))
    (let ((copy (copy-seq buff)))
      (ccl:dispose-heap-ivector buff)
      (ccl:dispose-heap-ivector buffp)
      copy))

  #+(and os-windows allegro)
  (let ((buff (make-array num-bytes :element-type '(unsigned-byte 8))))
    (when (zerop (rtl-gen-random buff num-bytes))
      (error 'ironclad-error :format-control "RtlGenRandom failed"))
    buff)

  #+(and mswindows lispworks)
  (let ((buff (sys:in-static-area (make-array num-bytes :element-type '(unsigned-byte 8)))))
    (unless (fli:with-dynamic-lisp-array-pointer (buff buff)
              (rtl-gen-random buff num-bytes))
      (error 'ironclad-error :format-control "RtlGenRandom failed"))
    (copy-seq buff))

  #-(or unix
        (and win32 sbcl)
        (and os-windows ccl)
        (and os-windows allegro)
        (and mswindows lispworks))
  (error 'ironclad-error
         :format-control "OS-RANDOM-SEED is not supported on your platform."))

(defmethod make-prng ((name (eql :os)) &key seed)
  (declare (ignorable seed))
  (make-instance 'os-prng))

(setf *prng* (make-prng :os))
#+thread-support
(pushnew '(*prng* . (make-prng :os)) bt:*default-special-bindings* :test #'equal)
