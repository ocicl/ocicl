;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; blake2.lisp -- implementation of the BLAKE2b hash function (RFC 7693)

(in-package :crypto)


;;;
;;; Parameters
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +blake2-rounds+ 12)
  (defconstant +blake2-block-size+ 128)
  (defconst +blake2-sigma+
    (make-array '(12 16)
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
                                    (10 2 8 4 7 6 1 5 15 11 9 14 3 12 13 0)
                                    (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
                                    (14 10 4 8 9 15 13 6 1 12 0 2 11 7 5 3))))
  (defconst +blake2-iv+
    (make-array 8
                :element-type '(unsigned-byte 64)
                :initial-contents '(#x6A09E667F3BCC908
                                    #xBB67AE8584CAA73B
                                    #x3C6EF372FE94F82B
                                    #xA54FF53A5F1D36F1
                                    #x510E527FADE682D1
                                    #x9B05688C2B3E6C1F
                                    #x1F83D9ABFB41BD6B
                                    #x5BE0CD19137E2179))))

(defun blake2-make-initial-state (output-length &optional (key-length 0))
  (when (> output-length 64)
    (error 'ironclad-error :format-control "The output length must be at most 64 bytes."))
  (when (> key-length 64)
    (error 'ironclad-error :format-control "The key length must be at most 64 bytes."))
  (let ((state (copy-seq +blake2-iv+)))
    (setf (aref state 0) (logxor (aref state 0)
                                 #x01010000
                                 (ash key-length 8)
                                 output-length))
    state))


;;;
;;; Blake2b rounds
;;;

(declaim (ftype (function ((simple-array (unsigned-byte 64) (8))
                           (simple-array (unsigned-byte 8) (*))
                           fixnum
                           (unsigned-byte 128)
                           boolean))
                blake2-rounds))
(defun blake2-rounds (state input start offset final)
  (declare (type (simple-array (unsigned-byte 64) (8)) state)
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type fixnum start)
           (type (unsigned-byte 128) offset)
           (type boolean final)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (macrolet ((blake2-mixing (va vb vc vd x y)
               `(setf ,va (mod64+ (mod64+ ,va ,vb) ,x)
                      ,vd (ror64 (logxor ,vd ,va) 32)
                      ,vc (mod64+ ,vc ,vd)
                      ,vb (ror64 (logxor ,vb ,vc) 24)
                      ,va (mod64+ (mod64+ ,va ,vb) ,y)
                      ,vd (ror64 (logxor ,vd ,va) 16)
                      ,vc (mod64+ ,vc ,vd)
                      ,vb (ror64 (logxor ,vb ,vc) 63))))
    (let ((v0 (aref state 0))
          (v1 (aref state 1))
          (v2 (aref state 2))
          (v3 (aref state 3))
          (v4 (aref state 4))
          (v5 (aref state 5))
          (v6 (aref state 6))
          (v7 (aref state 7))
          (v8 (aref +blake2-iv+ 0))
          (v9 (aref +blake2-iv+ 1))
          (v10 (aref +blake2-iv+ 2))
          (v11 (aref +blake2-iv+ 3))
          (v12 (aref +blake2-iv+ 4))
          (v13 (aref +blake2-iv+ 5))
          (v14 (aref +blake2-iv+ 6))
          (v15 (aref +blake2-iv+ 7))
          (m (make-array 16 :element-type '(unsigned-byte 64) :initial-element 0)))
      (declare (type (unsigned-byte 64) v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15)
               (type (simple-array (unsigned-byte 64) (16)) m)
               (dynamic-extent m))
      (setf v12 (logxor v12 (ldb (byte 64 0) offset))
            v13 (logxor v13 (ldb (byte 64 64) offset)))
      (when final
        (setf v14 (logxor v14 #xFFFFFFFFFFFFFFFF)))

      ;; Get input data as 64-bit little-endian integers
      (dotimes-unrolled (i 16)
        (setf (aref m i) (ub64ref/le input (+ start (* i 8)))))

      ;; Mixing rounds
      (dotimes-unrolled (i +blake2-rounds+)
        (blake2-mixing v0 v4 v8 v12 (aref m (aref +blake2-sigma+ i 0)) (aref m (aref +blake2-sigma+ i 1)))
        (blake2-mixing v1 v5 v9 v13 (aref m (aref +blake2-sigma+ i 2)) (aref m (aref +blake2-sigma+ i 3)))
        (blake2-mixing v2 v6 v10 v14 (aref m (aref +blake2-sigma+ i 4)) (aref m (aref +blake2-sigma+ i 5)))
        (blake2-mixing v3 v7 v11 v15 (aref m (aref +blake2-sigma+ i 6)) (aref m (aref +blake2-sigma+ i 7)))
        (blake2-mixing v0 v5 v10 v15 (aref m (aref +blake2-sigma+ i 8)) (aref m (aref +blake2-sigma+ i 9)))
        (blake2-mixing v1 v6 v11 v12 (aref m (aref +blake2-sigma+ i 10)) (aref m (aref +blake2-sigma+ i 11)))
        (blake2-mixing v2 v7 v8 v13 (aref m (aref +blake2-sigma+ i 12)) (aref m (aref +blake2-sigma+ i 13)))
        (blake2-mixing v3 v4 v9 v14 (aref m (aref +blake2-sigma+ i 14)) (aref m (aref +blake2-sigma+ i 15))))

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

(defstruct (blake2
             (:constructor %make-blake2-digest nil)
             (:copier nil))
  (state (blake2-make-initial-state 64)
         :type (simple-array (unsigned-byte 64) (8)))
  (offset 0 :type (unsigned-byte 128))
  (buffer (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)
          :type (simple-array (unsigned-byte 8) (128)))
  (buffer-index 0 :type (integer 0 128)))

(defstruct (blake2/384
             (:include blake2)
             (:constructor %make-blake2/384-digest
                           (&aux (state (blake2-make-initial-state 48))))
             (:copier nil)))

(defstruct (blake2/256
             (:include blake2)
             (:constructor %make-blake2/256-digest
                           (&aux (state (blake2-make-initial-state 32))))
             (:copier nil)))

(defstruct (blake2/160
             (:include blake2)
             (:constructor %make-blake2/160-digest
                           (&aux (state (blake2-make-initial-state 20))))
             (:copier nil)))

(defmethod reinitialize-instance ((state blake2) &rest initargs)
  (declare (ignore initargs))
  (setf (blake2-state state) (etypecase state
                               (blake2/160 (blake2-make-initial-state 20))
                               (blake2/256 (blake2-make-initial-state 32))
                               (blake2/384 (blake2-make-initial-state 48))
                               (blake2 (blake2-make-initial-state 64)))
        (blake2-offset state) 0
        (blake2-buffer-index state) 0)
  state)

(defmethod copy-digest ((state blake2) &optional copy)
  (check-type copy (or null blake2))
  (let ((copy (if copy
                  copy
                  (etypecase state
                    (blake2/160 (%make-blake2/160-digest))
                    (blake2/256 (%make-blake2/256-digest))
                    (blake2/384 (%make-blake2/384-digest))
                    (blake2 (%make-blake2-digest))))))
    (declare (type blake2 copy))
    (replace (blake2-state copy) (blake2-state state))
    (setf (blake2-offset copy) (blake2-offset state))
    (replace (blake2-buffer copy) (blake2-buffer state))
    (setf (blake2-buffer-index copy) (blake2-buffer-index state))
    copy))

(defun blake2-update (state input start end final)
  (declare (type blake2 state)
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type fixnum start end)
           (type boolean final)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let ((blake2-state (blake2-state state))
        (offset (blake2-offset state))
        (buffer (blake2-buffer state))
        (buffer-index (blake2-buffer-index state))
        (length (- end start))
        (n 0))
    (declare (type (simple-array (unsigned-byte 64) (8)) blake2-state)
             (type (unsigned-byte 128) offset)
             (type (simple-array (unsigned-byte 8) (128)) buffer)
             (type (integer 0 128) buffer-index)
             (type fixnum length n))

    ;; Try to fill the buffer with the new data
    (setf n (min length (- +blake2-block-size+ buffer-index)))
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
    (when (and (= buffer-index +blake2-block-size+)
               (or final (plusp length)))
      (blake2-rounds blake2-state buffer 0 offset final)
      (setf buffer-index 0))

    ;; Process data in message
    (unless final
      (loop until (<= length +blake2-block-size+) do
        (incf offset +blake2-block-size+)
        (blake2-rounds blake2-state input start offset nil)
        (incf start +blake2-block-size+)
        (decf length +blake2-block-size+)))

    ;; Put remaining message data in buffer
    (when (plusp length)
      (replace buffer input :end1 length :start2 start)
      (incf offset length)
      (incf buffer-index length))

    ;; Save the new state
    (setf (blake2-offset state) offset
          (blake2-buffer-index state) buffer-index)
    (values)))

(defun blake2-finalize (state digest digest-start)
  (let* ((digest-length (digest-length state))
         (blake2-state (blake2-state state))
         (buffer-index (blake2-buffer-index state))
         (padding-length (- +blake2-block-size+ buffer-index))
         (padding (make-array padding-length
                              :element-type '(unsigned-byte 8)
                              :initial-element 0)))

    ;; Process remaining data after padding it
    (blake2-update state padding 0 padding-length t)

    ;; Get output
    (let ((output (make-array +blake2-block-size+ :element-type '(unsigned-byte 8) :initial-element 0)))
      (dotimes (i 8)
        (setf (ub64ref/le output (* i 8)) (aref blake2-state i)))
      (replace digest output :start1 digest-start :end2 digest-length)
      digest)))

(define-digest-updater blake2
  (blake2-update state sequence start end nil))

(define-digest-finalizer ((blake2 64)
                          (blake2/384 48)
                          (blake2/256 32)
                          (blake2/160 20))
  (blake2-finalize state digest digest-start))

(defdigest blake2 :digest-length 64 :block-length 128)
(defdigest blake2/384 :digest-length 48 :block-length 128)
(defdigest blake2/256 :digest-length 32 :block-length 128)
(defdigest blake2/160 :digest-length 20 :block-length 128)
