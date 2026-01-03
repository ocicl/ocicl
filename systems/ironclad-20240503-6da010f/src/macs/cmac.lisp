;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;; cmac.lisp -- The CMAC algorithm from NIST 800-38B.

(in-package :crypto)

(defclass cmac (mac)
  ((cipher :reader cmac-cipher :initarg :cipher)
   (subkey1 :reader cmac-subkey1 :initarg :subkey1
            :type (simple-array (unsigned-byte 8) (*)))
   (subkey2 :reader cmac-subkey2 :initarg :subkey2
            :type (simple-array (unsigned-byte 8) (*)))
   (buffer :reader cmac-buffer :initarg :buffer
           :type (simple-array (unsigned-byte 8) (*)))
   (buffer-index :accessor cmac-buffer-index :initform 0 :type index)))

(defun make-cmac (key cipher-name)
  (declare (type (simple-array (unsigned-byte 8) (*)) key))
  (let* ((cipher (make-cipher cipher-name :key key :mode :ecb))
         (block-length (block-length cipher-name))
         (L (make-array block-length :element-type '(unsigned-byte 8)
                        :initial-element 0)))
    (encrypt-in-place cipher L)
    (flet ((gen-subkey (b)
             (let* ((n-bits (* block-length 8))
                    (k (integer-to-octets
                        (ldb (byte n-bits 0) (ash (octets-to-integer b) 1))
                        :n-bits n-bits)))
               (when (logbitp 7 (aref b 0))
                 (ecase block-length
                   (8 (setf (aref k 7) (logxor (aref k 7) #x1b)))
                   (16 (setf (aref k 15) (logxor (aref k 15) #x87)))
                   (32 (setf (aref k 30) (logxor (aref k 30) #x4)
                             (aref k 31) (logxor (aref k 31) #x25)))
                   (64 (setf (aref k 62) (logxor (aref k 62) #x1)
                             (aref k 63) (logxor (aref k 63) #x25)))
                   (128 (setf (aref k 125) (logxor (aref k 125) #x8)
                              (aref k 126) (logxor (aref k 126) #x0)
                              (aref k 127) (logxor (aref k 127) #x43)))))
               k)))
      (let ((L.u (gen-subkey L)))
        (make-instance 'cmac
                       :cipher cipher
                       :subkey1 L.u
                       :subkey2 (gen-subkey L.u)
                       :buffer (make-array block-length
                                           :element-type '(unsigned-byte 8)
                                           :initial-element 0))))))

(defmethod reinitialize-instance ((mac cmac) &rest initargs
                                  &key key &allow-other-keys)
  (declare (ignore initargs)
           (type (simple-array (unsigned-byte 8) (*)) key))
  (fill (cmac-buffer mac) 0)
  (setf (cmac-buffer-index mac) 0)
  (reinitialize-instance (cmac-cipher mac) :key key :mode :ecb)
  mac)

(defun update-cmac (cmac sequence &key (start 0) (end (length sequence)))
  (declare (type (simple-array (unsigned-byte 8) (*)) sequence)
           (type index start end)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let* ((cipher (cmac-cipher cmac))
         (encryption-function (encrypt-function cipher))
         (buffer (cmac-buffer cmac))
         (buffer-index (cmac-buffer-index cmac))
         (block-length (length buffer))
         (remaining (- end start)))
    (declare (type (simple-array (unsigned-byte 8) (*)) buffer))

    (when (< 0 buffer-index block-length)
      (dotimes (i (min remaining (- block-length buffer-index)))
        (setf (aref buffer buffer-index) (logxor (aref buffer buffer-index)
                                                 (aref sequence start)))
        (incf buffer-index)
        (incf start)
        (decf remaining)))

    (when (and (= buffer-index block-length)
               (plusp remaining))
      (funcall encryption-function cipher buffer 0 buffer 0)
      (setf buffer-index 0))

    (loop while (> remaining block-length) do
      (xor-block block-length buffer 0 sequence start buffer 0)
      (funcall encryption-function cipher buffer 0 buffer 0)
      (incf start block-length)
      (decf remaining block-length))

    (loop while (plusp remaining) do
      (setf (aref buffer buffer-index) (logxor (aref buffer buffer-index)
                                               (aref sequence start)))
      (incf buffer-index)
      (incf start)
      (decf remaining))

    (setf (cmac-buffer-index cmac) buffer-index)
    (values)))

(defun cmac-digest (cmac)
  (let* ((block-length (length (cmac-buffer cmac)))
         (x (copy-seq (cmac-buffer cmac)))
         (L (cond
              ((= block-length (cmac-buffer-index cmac))
               (cmac-subkey1 cmac))
              (t
               (setf (aref x (cmac-buffer-index cmac))
                     (logxor (aref x (cmac-buffer-index cmac)) #x80))
               (cmac-subkey2 cmac)))))
    (xor-block block-length L 0 x 0 x 0)
    (encrypt-in-place (cmac-cipher cmac) x)
    x))

(defmac cmac
        make-cmac
        update-cmac
        cmac-digest)
