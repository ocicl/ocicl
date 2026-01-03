;;;; -*- mode: lisp; indent-tabs-mode: nil -*-

(in-package :crypto)

;;; Check what features are supported by the CPU

#+(and sbcl x86-64)
(sb-ext:defglobal aes-ni-support-known-p nil)
#+(and sbcl x86-64)
(sb-ext:defglobal aes-ni-supported-p nil)
#+(and sbcl x86-64)
(declaim (inline aes-ni-supported-p))
#+(and sbcl x86-64)
(defun aes-ni-supported-p ()
  (declare (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  #+ironclad-assembly (if aes-ni-support-known-p
                          aes-ni-supported-p
                          (setf aes-ni-support-known-p t
                                aes-ni-supported-p (aes-ni-support-p)))
  #-ironclad-assembly nil)

#+(and sbcl x86-64)
(sb-ext:defglobal pclmulqdq-support-known-p nil)
#+(and sbcl x86-64)
(sb-ext:defglobal pclmulqdq-supported-p nil)
#+(and sbcl x86-64)
(declaim (inline pclmulqdq-supported-p))
#+(and sbcl x86-64)
(defun pclmulqdq-supported-p ()
  (declare (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  #+ironclad-assembly (if pclmulqdq-support-known-p
                          pclmulqdq-supported-p
                          (setf pclmulqdq-support-known-p t
                                pclmulqdq-supported-p (pclmulqdq-support-p)))
  #-ironclad-assembly nil)
