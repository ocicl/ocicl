;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; blake2s.lisp -- implementation of the BLAKE2s hash function (RFC 7693)

(in-package :crypto)


;;;
;;; Parameters
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +blake2s-rounds+ 10)
  (defconstant +blake2s-block-size+ 64)
  (defconst +blake2s-sigma+
    (make-array '(10 16)
                :element-type '(integer 0 15)
                :initial-contents '((0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
                                    (14 10 4 8 9 15 13 6 1 12 0 2 11 7 5 3)
                                    (11 8 12 0 5 2 15 13 10 14 3 6 7 1 9 4)
                                    (7 9 3 1 13 12 11 14 2 6 5 10 4 0 15 8)
                                    (9 0 5 7 2 4 10 15 14 1 11 12 6 8 3 13)
                                    (2 12 6 10 0 11 8 3 4 13 7 5 15 14 1 9)
                                    (12 5 1 15 14 13 4 10 0 7 6 3 9 2 8 11)
                                    (13 11 7 14 12 1 3 9 5 0 15 4 8 6 2 10)
                                    (6 15 14 9 11 3 0 8 12 2 13 7 1 4 10 5)
                                    (10 2 8 4 7 6 1 5 15 11 9 14 3 12 13 0))))
  (defconst +blake2s-iv+
    (make-array 8
                :element-type '(unsigned-byte 32)
                :initial-contents '(#x6A09E667
                                    #xBB67AE85
                                    #x3C6EF372
                                    #xA54FF53A
                                    #x510E527F
                                    #x9B05688C
                                    #x1F83D9AB
                                    #x5BE0CD19))))

(defun blake2s-make-initial-state (output-length &optional (key-length 0))
  (when (> output-length 32)
    (error 'ironclad-error :format-control "The output length must be at most 32 bytes."))
  (when (> key-length 32)
    (error 'ironclad-error :format-control "The key length must be at most 32 bytes."))
  (let ((state (copy-seq +blake2s-iv+)))
    (setf (aref state 0) (logxor (aref state 0)
                                 #x01010000
                                 (ash key-length 8)
                                 output-length))
    state))


;;;
;;; Blake2s rounds
;;;

(declaim (ftype (function ((simple-array (unsigned-byte 32) (8))
                           (simple-array (unsigned-byte 8) (*))
                           fixnum
                           (unsigned-byte 64)
                           boolean))
                blake2s-rounds))
(defun blake2s-rounds (state input start offset final)
  (declare (type (simple-array (unsigned-byte 32) (8)) state)
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type fixnum start)
           (type (unsigned-byte 64) offset)
           (type boolean final)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (macrolet ((blake2s-mixing (va vb vc vd x y)
               ;; Bug in SBCL (< 1.3.6), ror32 doesn't give the right result
               ;; because it is not compiled correctly.
               ;; Using rol32 instead for now.
               ;; `(setf ,va (mod32+ (mod32+ ,va ,vb) ,x)
               ;;        ,vd (ror32 (logxor ,vd ,va) 16)
               ;;        ,vc (mod32+ ,vc ,vd)
               ;;        ,vb (ror32 (logxor ,vb ,vc) 12)
               ;;        ,va (mod32+ (mod32+ ,va ,vb) ,y)
               ;;        ,vd (ror32 (logxor ,vd ,va) 8)
               ;;        ,vc (mod32+ ,vc ,vd)
               ;;        ,vb (ror32 (logxor ,vb ,vc) 7))))
               #+(and sbcl x86-64 ironclad-assembly)
               `(multiple-value-setq (,va ,vb ,vc ,vd)
                  (fast-blake2s-mixing ,va ,vb ,vc ,vd ,x ,y))
               #-(and sbcl x86-64 ironclad-assembly)
               `(setf ,va (mod32+ (mod32+ ,va ,vb) ,x)
                      ,vd (rol32 (logxor ,vd ,va) 16)
                      ,vc (mod32+ ,vc ,vd)
                      ,vb (rol32 (logxor ,vb ,vc) 20)
                      ,va (mod32+ (mod32+ ,va ,vb) ,y)
                      ,vd (rol32 (logxor ,vd ,va) 24)
                      ,vc (mod32+ ,vc ,vd)
                      ,vb (rol32 (logxor ,vb ,vc) 25))))
    (let ((v0 (aref state 0))
          (v1 (aref state 1))
          (v2 (aref state 2))
          (v3 (aref state 3))
          (v4 (aref state 4))
          (v5 (aref state 5))
          (v6 (aref state 6))
          (v7 (aref state 7))
          (v8 (aref +blake2s-iv+ 0))
          (v9 (aref +blake2s-iv+ 1))
          (v10 (aref +blake2s-iv+ 2))
          (v11 (aref +blake2s-iv+ 3))
          (v12 (aref +blake2s-iv+ 4))
          (v13 (aref +blake2s-iv+ 5))
          (v14 (aref +blake2s-iv+ 6))
          (v15 (aref +blake2s-iv+ 7))
          (m (make-array 16 :element-type '(unsigned-byte 32) :initial-element 0)))
      (declare (type (unsigned-byte 32) v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15)
               (type (simple-array (unsigned-byte 32) (16)) m)
               (dynamic-extent m))
      (setf v12 (logxor v12 (ldb (byte 32 0) offset))
            v13 (logxor v13 (ldb (byte 32 32) offset)))
      (when final
        (setf v14 (logxor v14 #xFFFFFFFF)))

      ;; Get input data as 32-bit little-endian integers
      (dotimes-unrolled (i 16)
        (setf (aref m i) (ub32ref/le input (+ start (* i 4)))))

      ;; Mixing rounds
      (dotimes-unrolled (i +blake2s-rounds+)
        (blake2s-mixing v0 v4 v8 v12 (aref m (aref +blake2s-sigma+ i 0)) (aref m (aref +blake2s-sigma+ i 1)))
        (blake2s-mixing v1 v5 v9 v13 (aref m (aref +blake2s-sigma+ i 2)) (aref m (aref +blake2s-sigma+ i 3)))
        (blake2s-mixing v2 v6 v10 v14 (aref m (aref +blake2s-sigma+ i 4)) (aref m (aref +blake2s-sigma+ i 5)))
        (blake2s-mixing v3 v7 v11 v15 (aref m (aref +blake2s-sigma+ i 6)) (aref m (aref +blake2s-sigma+ i 7)))
        (blake2s-mixing v0 v5 v10 v15 (aref m (aref +blake2s-sigma+ i 8)) (aref m (aref +blake2s-sigma+ i 9)))
        (blake2s-mixing v1 v6 v11 v12 (aref m (aref +blake2s-sigma+ i 10)) (aref m (aref +blake2s-sigma+ i 11)))
        (blake2s-mixing v2 v7 v8 v13 (aref m (aref +blake2s-sigma+ i 12)) (aref m (aref +blake2s-sigma+ i 13)))
        (blake2s-mixing v3 v4 v9 v14 (aref m (aref +blake2s-sigma+ i 14)) (aref m (aref +blake2s-sigma+ i 15))))

      ;; Compute new state
      (setf (aref state 0) (logxor (aref state 0) v0 v8)
            (aref state 1) (logxor (aref state 1) v1 v9)
            (aref state 2) (logxor (aref state 2) v2 v10)
            (aref state 3) (logxor (aref state 3) v3 v11)
            (aref state 4) (logxor (aref state 4) v4 v12)
            (aref state 5) (logxor (aref state 5) v5 v13)
            (aref state 6) (logxor (aref state 6) v6 v14)
            (aref state 7) (logxor (aref state 7) v7 v15))))

  (values))


;;;
;;; Digest structures and functions
;;;

(defstruct (blake2s
             (:constructor %make-blake2s-digest nil)
             (:copier nil))
  (state (blake2s-make-initial-state 32)
         :type (simple-array (unsigned-byte 32) (8)))
  (offset 0 :type (unsigned-byte 64))
  (buffer (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)
          :type (simple-array (unsigned-byte 8) (64)))
  (buffer-index 0 :type (integer 0 64)))

(defstruct (blake2s/224
             (:include blake2s)
             (:constructor %make-blake2s/224-digest
                           (&aux (state (blake2s-make-initial-state 28))))
             (:copier nil)))

(defstruct (blake2s/160
             (:include blake2s)
             (:constructor %make-blake2s/160-digest
                           (&aux (state (blake2s-make-initial-state 20))))
             (:copier nil)))

(defstruct (blake2s/128
             (:include blake2s)
             (:constructor %make-blake2s/128-digest
                           (&aux (state (blake2s-make-initial-state 16))))
             (:copier nil)))

(defmethod reinitialize-instance ((state blake2s) &rest initargs)
  (declare (ignore initargs))
  (setf (blake2s-state state) (etypecase state
                                (blake2s/128 (blake2s-make-initial-state 16))
                                (blake2s/160 (blake2s-make-initial-state 20))
                                (blake2s/224 (blake2s-make-initial-state 28))
                                (blake2s (blake2s-make-initial-state 32)))
        (blake2s-offset state) 0
        (blake2s-buffer-index state) 0)
  state)

(defmethod copy-digest ((state blake2s) &optional copy)
  (check-type copy (or null blake2s))
  (let ((copy (if copy
                  copy
                  (etypecase state
                    (blake2s/128 (%make-blake2s/128-digest))
                    (blake2s/160 (%make-blake2s/160-digest))
                    (blake2s/224 (%make-blake2s/224-digest))
                    (blake2s (%make-blake2s-digest))))))
    (declare (type blake2s copy))
    (replace (blake2s-state copy) (blake2s-state state))
    (setf (blake2s-offset copy) (blake2s-offset state))
    (replace (blake2s-buffer copy) (blake2s-buffer state))
    (setf (blake2s-buffer-index copy) (blake2s-buffer-index state))
    copy))

(defun blake2s-update (state input start end final)
  (declare (type blake2s state)
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type fixnum start end)
           (type boolean final)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let ((blake2s-state (blake2s-state state))
        (offset (blake2s-offset state))
        (buffer (blake2s-buffer state))
        (buffer-index (blake2s-buffer-index state))
        (length (- end start))
        (n 0))
    (declare (type (simple-array (unsigned-byte 32) (8)) blake2s-state)
             (type (unsigned-byte 64) offset)
             (type (simple-array (unsigned-byte 8) (64)) buffer)
             (type (integer 0 64) buffer-index)
             (type fixnum length n))

    ;; Try to fill the buffer with the new data
    (setf n (min length (- +blake2s-block-size+ buffer-index)))
    (replace buffer input :start1 buffer-index :start2 start :end2 (+ start n))
    (unless final
      (incf offset n))
    (incf buffer-index n)
    (incf start n)
    (decf length n)

    ;; Process as many blocks as we can, but unless we are in the
    ;; final call, keep some data in the buffer (so that it can be
    ;; processed with the 'final' flag in the final call

    ;; Process data in buffer
    (when (and (= buffer-index +blake2s-block-size+)
               (or final (plusp length)))
      (blake2s-rounds blake2s-state buffer 0 offset final)
      (setf buffer-index 0))

    ;; Process data in message
    (unless final
      (loop until (<= length +blake2s-block-size+) do
        (incf offset +blake2s-block-size+)
        (blake2s-rounds blake2s-state input start offset nil)
        (incf start +blake2s-block-size+)
        (decf length +blake2s-block-size+)))

    ;; Put remaining message data in buffer
    (when (plusp length)
      (replace buffer input :end1 length :start2 start)
      (incf offset length)
      (incf buffer-index length))

    ;; Save the new state
    (setf (blake2s-offset state) offset
          (blake2s-buffer-index state) buffer-index)
    (values)))

(defun blake2s-finalize (state digest digest-start)
  (let* ((digest-length (digest-length state))
         (blake2s-state (blake2s-state state))
         (buffer-index (blake2s-buffer-index state))
         (padding-length (- +blake2s-block-size+ buffer-index))
         (padding (make-array padding-length
                              :element-type '(unsigned-byte 8)
                              :initial-element 0)))

    ;; Process remaining data after padding it
    (blake2s-update state padding 0 padding-length t)

    ;; Get output
    (let ((output (make-array +blake2s-block-size+ :element-type '(unsigned-byte 8) :initial-element 0)))
      (dotimes (i 8)
        (setf (ub32ref/le output (* i 4)) (aref blake2s-state i)))
      (replace digest output :start1 digest-start :end2 digest-length)
      digest)))

(define-digest-updater blake2s
  (blake2s-update state sequence start end nil))

(define-digest-finalizer ((blake2s 32)
                          (blake2s/224 28)
                          (blake2s/160 20)
                          (blake2s/128 16))
  (blake2s-finalize state digest digest-start))

(defdigest blake2s :digest-length 32 :block-length 64)
(defdigest blake2s/224 :digest-length 28 :block-length 64)
(defdigest blake2s/160 :digest-length 20 :block-length 64)
(defdigest blake2s/128 :digest-length 16 :block-length 64)
