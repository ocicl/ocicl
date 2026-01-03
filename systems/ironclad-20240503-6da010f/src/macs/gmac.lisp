;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; gmac.lisp -- GMAC message authentication code

;; See nistspecialpublication800-38d.pdf about GCM and GMAC.

(in-package :crypto)


(defclass gmac (mac)
  ((accumulator :accessor gmac-accumulator
                :initform (make-array 16 :element-type '(unsigned-byte 8))
                :type (simple-array (unsigned-byte 8) (16)))
   (key :accessor gmac-key
        :type (or null
                  (simple-array (unsigned-byte 8) (16))
                  (simple-array (unsigned-byte 64) (128 2 2))))
   (total-length :accessor gmac-total-length
                 :initform 0
                 :type (unsigned-byte 64))
   (cipher :accessor gmac-cipher
           :initform nil)
   (j0 :accessor gmac-j0
       :type (simple-array (unsigned-byte 8) (16)))
   (iv :accessor gmac-iv
       :initform (make-array 16 :element-type '(unsigned-byte 8))
       :type (simple-array (unsigned-byte 8) (16)))
   (buffer :accessor gmac-buffer
           :initform (make-array 16 :element-type '(unsigned-byte 8))
           :type (simple-array (unsigned-byte 8) (16)))
   (buffer-length :accessor gmac-buffer-length
                  :initform 0
                  :type (integer 0 16))))

(defun make-gmac (key cipher-name initialization-vector)
  (unless (member (length key) (key-lengths cipher-name))
    (error 'invalid-mac-parameter
           :mac-name 'gmac
           :message "The key length is not compatible with the cipher"))
  (unless (= (block-length cipher-name) 16)
    (error 'invalid-mac-parameter
           :mac-name 'gmac
           :message "GMAC only supports 128-bit block ciphers"))
  (make-instance 'gmac
                 :key key
                 :cipher-name cipher-name
                 :initialization-vector initialization-vector))

(declaim (inline gmac-swap-16))
(defun gmac-swap-16 (data)
  (declare (type (simple-array (unsigned-byte 8) (16)) data)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let ((x (ub64ref/be data 8)))
    (declare (type (unsigned-byte 64) x))
    (setf (ub64ref/le data 8) (ub64ref/be data 0)
          (ub64ref/le data 0) x))
  (values))

(defun ghash (h x)
  (multiple-value-bind (q r) (floor (length x) 16)
    (assert (zerop r))
    (let ((z (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))
          (i 0))
      (if #+(and sbcl x86-64 ironclad-assembly) (pclmulqdq-supported-p)
          #-(and sbcl x86-64 ironclad-assembly) nil
          (let ((y (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
            (dotimes (j q)
              (replace y x :start2 i)
              (ironclad::gmac-swap-16 y)
              (ironclad::xor-block 16 z 0 y 0 z 0)
              (ironclad::gmac-mul z h)
              (incf i 16))
            (ironclad::gmac-swap-16 z))
          (dotimes (j q)
            (ironclad::xor-block 16 z 0 x i z 0)
            (ironclad::gmac-mul z h)
            (incf i 16)))
      z)))

(defun j0 (h iv)
  (labels ((pad (n)
             (make-array (- (* 16 (ceiling n 16)) n)
                         :element-type '(unsigned-byte 8)
                         :initial-element 0))
           (ac (a c)
             (let ((an (length a))
                   (cn (length c)))
               (concatenate '(simple-array (unsigned-byte 8) (*))
                            a
                            (pad an)
                            c
                            (pad cn)
                            (integer-to-octets (* 8 an) :n-bits 64)
                            (integer-to-octets (* 8 cn) :n-bits 64)))))
    (let* ((n (length iv))
           (n*8 (* 8 n)))
      (cond
        ((= 12 n)
         (concatenate '(simple-array (unsigned-byte 8) (16)) iv #(0 0 0 1)))
        ((< 0 n*8 #.(expt 2 64))
         (ghash h (ac nil iv)))
        (t
         (error 'invalid-mac-parameter
                :mac-name 'gmac
                :message "iv size not in range 0<|iv|<2^64 bits"))))))

(defmethod shared-initialize :after ((mac gmac) slot-names &rest initargs &key key cipher-name initialization-vector &allow-other-keys)
  (declare (ignore slot-names initargs)
           (type (simple-array (unsigned-byte 8) (*)) key))
  (when (and cipher-name (/= (block-length cipher-name) 16))
    (error 'invalid-mac-parameter
           :mac-name 'gmac
           :message "GMAC only supports 128-bit block ciphers"))
  (if #+(and sbcl x86-64 ironclad-assembly) (pclmulqdq-supported-p)
      #-(and sbcl x86-64 ironclad-assembly) nil
      (let ((cipher (if (or cipher-name (null (gmac-cipher mac)))
                        (make-cipher cipher-name :key key :mode :ecb)
                        (reinitialize-instance (gmac-cipher mac) :key key :mode :ecb)))
            (hkey (make-array 16 :element-type '(unsigned-byte 8)))
            (iv (gmac-iv mac)))
        (declare (type (simple-array (unsigned-byte 8) (16)) hkey))
        (setf (gmac-key mac) hkey
              (gmac-total-length mac) 0
              (gmac-buffer-length mac) 0
              (gmac-cipher mac) cipher)
        (fill (gmac-accumulator mac) 0)
        (fill hkey 0)
        (encrypt-in-place cipher hkey)
        (gmac-swap-16 hkey)
        (let ((j0 (j0 hkey initialization-vector)))
          (setf (gmac-j0 mac) j0)
          (replace iv j0))
        (encrypt-in-place cipher iv)
        mac)
      (let ((table (make-array '(128 2 2) :element-type '(unsigned-byte 64)
                                          :initial-element 0))
            (cipher (if cipher-name
                        (make-cipher cipher-name :key key :mode :ecb)
                        (gmac-cipher mac)))
            (iv (gmac-iv mac))
            (hkey (make-array 16 :element-type '(unsigned-byte 8)
                                 :initial-element 0)))
        (declare (type (simple-array (unsigned-byte 64) (128 2 2)) table)
                 (type (simple-array (unsigned-byte 8) (16)) hkey)
                 (dynamic-extent hkey))
        (setf (gmac-key mac) table
              (gmac-total-length mac) 0
              (gmac-buffer-length mac) 0
              (gmac-cipher mac) cipher)
        (fill (gmac-accumulator mac) 0)
        (encrypt-in-place cipher hkey)

        (setf (aref table 0 1 0) (ub64ref/be hkey 0)
              (aref table 0 1 1) (ub64ref/be hkey 8))
        (dotimes (i 127)
          (let ((c (if (logbitp 0 (aref table i 1 1)) #xe100000000000000 0)))
            (declare (type (unsigned-byte 64) c))
            (setf (aref table (1+ i) 1 1) (logior (mod64ash (aref table i 1 1) -1)
                                                  (mod64ash (aref table i 1 0) 63))
                  (aref table (1+ i) 1 0) (logxor (mod64ash (aref table i 1 0) -1) c))))
        (let ((j0 (j0 table initialization-vector)))
          (setf (gmac-j0 mac) j0)
          (replace iv j0))
        (encrypt-in-place cipher iv)
        mac)))

(defun gmac-mul (accumulator key)
  (declare (type (simple-array (unsigned-byte 8) (16)) accumulator)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (if #+(and sbcl x86-64 ironclad-assembly) (pclmulqdq-supported-p)
      #-(and sbcl x86-64 ironclad-assembly) nil
      #+(and sbcl x86-64 ironclad-assembly) (gmac-mul-fast accumulator key)
      #-(and sbcl x86-64 ironclad-assembly) nil
      (let ((x 0)
            (z0 0)
            (z1 0)
            (b 0))
        (declare (type (simple-array (unsigned-byte 64) (128 2 2)) key)
                 (type (unsigned-byte 8) x)
                 (type (unsigned-byte 64) z0 z1)
                 (type bit b))
        (dotimes-unrolled (i 16)
          (setf x (aref accumulator i))
          (dotimes-unrolled (j 8)
            (setf b (logand (ash x (- j 7)) 1)
                  z0 (logxor z0 (aref key (+ (* i 8) j) b 0))
                  z1 (logxor z1 (aref key (+ (* i 8) j) b 1)))))
        (setf (ub64ref/be accumulator 0) z0
              (ub64ref/be accumulator 8) z1)))
  (values))

(defun update-gmac (mac data &key (start 0) (end (length data)))
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type index start end)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let ((accumulator (gmac-accumulator mac))
        (key (gmac-key mac))
        (total-length (gmac-total-length mac))
        (buffer (gmac-buffer mac))
        (buffer-length (gmac-buffer-length mac))
        (remaining (- end start)))
    (declare (type (simple-array (unsigned-byte 8) (16)) accumulator buffer)
             (type (unsigned-byte 64) total-length)
             (type (integer 0 16) buffer-length)
             (type index remaining))

    ;; Fill the buffer with new data if necessary
    (when (plusp buffer-length)
      (let ((n (min remaining (- 16 buffer-length))))
        (declare (type (integer 0 16) n))
        (replace buffer data
                 :start1 buffer-length
                 :start2 start
                 :end2 (+ start n))
        (incf buffer-length n)
        (incf start n)
        (decf remaining n)))

    ;; Process the buffer
    (when (= buffer-length 16)
      (when #+(and sbcl x86-64 ironclad-assembly) (pclmulqdq-supported-p)
            #-(and sbcl x86-64 ironclad-assembly) nil
        (gmac-swap-16 buffer))
      (xor-block 16 accumulator 0 buffer 0 accumulator 0)
      (gmac-mul accumulator key)
      (incf total-length 16)
      (setf buffer-length 0))

    ;; Process the data
    (if #+(and sbcl x86-64 ironclad-assembly) (pclmulqdq-supported-p)
        #-(and sbcl x86-64 ironclad-assembly) nil
        (loop while (> remaining 16) do
          (setf (ub64ref/le buffer 8) (ub64ref/be data start)
                (ub64ref/le buffer 0) (ub64ref/be data (+ start 8)))
          (xor-block 16 accumulator 0 buffer 0 accumulator 0)
          (gmac-mul accumulator key)
          (incf total-length 16)
          (incf start 16)
          (decf remaining 16))
        (loop while (> remaining 16) do
          (xor-block 16 accumulator 0 data start accumulator 0)
          (gmac-mul accumulator key)
          (incf total-length 16)
          (incf start 16)
          (decf remaining 16)))

    ;; Put the remaining data in the buffer
    (when (plusp remaining)
      (replace buffer data :start1 0 :start2 start :end2 end)
      (setf buffer-length remaining))

    ;; Save the state
    (setf (gmac-total-length mac) total-length
          (gmac-buffer-length mac) buffer-length)
    (values)))

(defun gmac-digest (mac &optional (encrypted-data-length 0))
  (let ((accumulator (copy-seq (gmac-accumulator mac)))
        (key (gmac-key mac))
        (total-length (gmac-total-length mac))
        (iv (copy-seq (gmac-iv mac)))
        (buffer (copy-seq (gmac-buffer mac)))
        (buffer-length (gmac-buffer-length mac)))
    (declare (type (simple-array (unsigned-byte 8) (16)) accumulator buffer iv)
             (type (unsigned-byte 64) total-length)
             (type (integer 0 16) buffer-length))

    ;; Process the buffer
    (when (plusp buffer-length)
      (fill buffer 0 :start buffer-length)
      (when #+(and sbcl x86-64 ironclad-assembly) (pclmulqdq-supported-p)
            #-(and sbcl x86-64 ironclad-assembly) nil
        (gmac-swap-16 buffer))
      (xor-block 16 accumulator 0 buffer 0 accumulator 0)
      (gmac-mul accumulator key)
      (incf total-length buffer-length))

    ;; Padding
    (if #+(and sbcl x86-64 ironclad-assembly) (pclmulqdq-supported-p)
        #-(and sbcl x86-64 ironclad-assembly) nil
        (setf (ub64ref/le buffer 0) (mod64* 8 encrypted-data-length)
              (ub64ref/le buffer 8) (mod64* 8 (- total-length encrypted-data-length)))
        (setf (ub64ref/be buffer 0) (mod64* 8 (- total-length encrypted-data-length))
              (ub64ref/be buffer 8) (mod64* 8 encrypted-data-length)))
    (xor-block 16 accumulator 0 buffer 0 accumulator 0)
    (gmac-mul accumulator key)

    ;; Produce the tag
    (when #+(and sbcl x86-64 ironclad-assembly) (pclmulqdq-supported-p)
          #-(and sbcl x86-64 ironclad-assembly) nil
      (gmac-swap-16 accumulator))
    (xor-block 16 accumulator 0 iv 0 accumulator 0)
    accumulator))

(defmac gmac
        make-gmac
        update-gmac
        gmac-digest)
