;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; ripemd-128.lisp -- the RIPEMD-128 digest function

(in-package :crypto)

(define-digest-registers (ripemd-128 :endian :little)
  (a #x67452301)
  (b #xefcdab89)
  (c #x98badcfe)
  (d #x10325476))

(defconst +pristine-ripemd-128-registers+ (initial-ripemd-128-regs))

(defun update-ripemd-128-block (regs block)
  (declare (type ripemd-128-regs regs)
           (type (simple-array (unsigned-byte 32) (16)) block)
           #.(burn-baby-burn))
  (let* ((a1 (ripemd-128-regs-a regs)) (a2 a1)
         (b1 (ripemd-128-regs-b regs)) (b2 b1)
         (c1 (ripemd-128-regs-c regs)) (c2 c1)
         (d1 (ripemd-128-regs-d regs)) (d2 d1))
    (declare (type (unsigned-byte 32) a1 a2 b1 b2 c1 c2 d1 d2))
    ;; define the necessary logical functions
    (flet ((f (x y z)
             (declare (type (unsigned-byte 32) x y z))
             (ldb (byte 32 0) (logxor x y z)))
           (g (x y z)
             (declare (type (unsigned-byte 32) x y z))
             (ldb (byte 32 0) (logxor z (logand x (logxor y z)))))
           (h (x y z)
             (declare (type (unsigned-byte 32) x y z))
             (ldb (byte 32 0) (logxor z (logior x (lognot y)))))
           (i (x y z)
             (declare (type (unsigned-byte 32) x y z))
             (ldb (byte 32 0) (logxor y (logand z (logxor x y))))))
      #+ironclad-fast-mod32-arithmetic
      (declare (inline f g h i))
      (macrolet ((subround (func a b c d x s k)
                   `(progn
                     (setf ,a (mod32+ ,a
                               (mod32+ (funcall (function ,func) ,b ,c ,d)
                                       (mod32+ ,x ,k))))
                     (setf ,a (rol32 ,a ,s))))
                 (with-ripemd-round ((block func constant) &rest clauses)
                   (loop for (a b c d i s) in clauses
                         collect `(subround ,func ,a ,b ,c ,d (aref ,block ,i)
                                   ,s ,constant)
                         into result
                         finally (return `(progn ,@result)))))
        (with-ripemd-round (block f 0)
          (a1 b1 c1 d1  0 11) (d1 a1 b1 c1  1 14)
          (c1 d1 a1 b1  2 15) (b1 c1 d1 a1  3 12)
          (a1 b1 c1 d1  4  5) (d1 a1 b1 c1  5  8)
          (c1 d1 a1 b1  6  7) (b1 c1 d1 a1  7  9)
          (a1 b1 c1 d1  8 11) (d1 a1 b1 c1  9 13)
          (c1 d1 a1 b1 10 14) (b1 c1 d1 a1 11 15)
          (a1 b1 c1 d1 12  6) (d1 a1 b1 c1 13  7)
          (c1 d1 a1 b1 14  9) (b1 c1 d1 a1 15  8))
        (with-ripemd-round (block g #x5a827999)
          (a1 b1 c1 d1  7  7) (d1 a1 b1 c1  4  6)
          (c1 d1 a1 b1 13  8) (b1 c1 d1 a1  1 13)
          (a1 b1 c1 d1 10 11) (d1 a1 b1 c1  6  9)
          (c1 d1 a1 b1 15  7) (b1 c1 d1 a1  3 15)
          (a1 b1 c1 d1 12  7) (d1 a1 b1 c1  0 12)
          (c1 d1 a1 b1  9 15) (b1 c1 d1 a1  5  9)
          (a1 b1 c1 d1  2 11) (d1 a1 b1 c1 14  7)
          (c1 d1 a1 b1 11 13) (b1 c1 d1 a1  8 12))
        (with-ripemd-round (block h #x6ed9eba1)
          (a1 b1 c1 d1  3 11) (d1 a1 b1 c1 10 13)
          (c1 d1 a1 b1 14  6) (b1 c1 d1 a1  4  7)
          (a1 b1 c1 d1  9 14) (d1 a1 b1 c1 15  9)
          (c1 d1 a1 b1  8 13) (b1 c1 d1 a1  1 15)
          (a1 b1 c1 d1  2 14) (d1 a1 b1 c1  7  8)
          (c1 d1 a1 b1  0 13) (b1 c1 d1 a1  6  6)
          (a1 b1 c1 d1 13  5) (d1 a1 b1 c1 11 12)
          (c1 d1 a1 b1  5  7) (b1 c1 d1 a1 12  5))
        (with-ripemd-round (block i #x8f1bbcdc)
          (a1 b1 c1 d1  1 11) (d1 a1 b1 c1  9 12)
          (c1 d1 a1 b1 11 14) (b1 c1 d1 a1 10 15)
          (a1 b1 c1 d1  0 14) (d1 a1 b1 c1  8 15)
          (c1 d1 a1 b1 12  9) (b1 c1 d1 a1  4  8)
          (a1 b1 c1 d1 13  9) (d1 a1 b1 c1  3 14)
          (c1 d1 a1 b1  7  5) (b1 c1 d1 a1 15  6)
          (a1 b1 c1 d1 14  8) (d1 a1 b1 c1  5  6)
          (c1 d1 a1 b1  6  5) (b1 c1 d1 a1  2 12))
        (with-ripemd-round (block i #x50a28be6)
          (a2 b2 c2 d2  5  8) (d2 a2 b2 c2 14  9)
          (c2 d2 a2 b2  7  9) (b2 c2 d2 a2  0 11)
          (a2 b2 c2 d2  9 13) (d2 a2 b2 c2  2 15)
          (c2 d2 a2 b2 11 15) (b2 c2 d2 a2  4  5)
          (a2 b2 c2 d2 13  7) (d2 a2 b2 c2  6  7)
          (c2 d2 a2 b2 15  8) (b2 c2 d2 a2  8 11)
          (a2 b2 c2 d2  1 14) (d2 a2 b2 c2 10 14)
          (c2 d2 a2 b2  3 12) (b2 c2 d2 a2 12  6))
        (with-ripemd-round (block h #x5c4dd124)
          (a2 b2 c2 d2  6  9) (d2 a2 b2 c2 11 13)
          (c2 d2 a2 b2  3 15) (b2 c2 d2 a2  7  7)
          (a2 b2 c2 d2  0 12) (d2 a2 b2 c2 13  8)
          (c2 d2 a2 b2  5  9) (b2 c2 d2 a2 10 11)
          (a2 b2 c2 d2 14  7) (d2 a2 b2 c2 15  7)
          (c2 d2 a2 b2  8 12) (b2 c2 d2 a2 12  7)
          (a2 b2 c2 d2  4  6) (d2 a2 b2 c2  9 15)
          (c2 d2 a2 b2  1 13) (b2 c2 d2 a2  2 11))
        (with-ripemd-round (block g #x6d703ef3)
          (a2 b2 c2 d2 15  9) (d2 a2 b2 c2  5  7)
          (c2 d2 a2 b2  1 15) (b2 c2 d2 a2  3 11)
          (a2 b2 c2 d2  7  8) (d2 a2 b2 c2 14  6)
          (c2 d2 a2 b2  6  6) (b2 c2 d2 a2  9 14)
          (a2 b2 c2 d2 11 12) (d2 a2 b2 c2  8 13)
          (c2 d2 a2 b2 12  5) (b2 c2 d2 a2  2 14)
          (a2 b2 c2 d2 10 13) (d2 a2 b2 c2  0 13)
          (c2 d2 a2 b2  4  7) (b2 c2 d2 a2 13  5))
        (with-ripemd-round (block f 0)
          (a2 b2 c2 d2  8 15) (d2 a2 b2 c2  6  5)
          (c2 d2 a2 b2  4  8) (b2 c2 d2 a2  1 11)
          (a2 b2 c2 d2  3 14) (d2 a2 b2 c2 11 14)
          (c2 d2 a2 b2 15  6) (b2 c2 d2 a2  0 14)
          (a2 b2 c2 d2  5  6) (d2 a2 b2 c2 12  9)
          (c2 d2 a2 b2  2 12) (b2 c2 d2 a2 13  9)
          (a2 b2 c2 d2  9 12) (d2 a2 b2 c2  7  5)
          (c2 d2 a2 b2 10 15) (b2 c2 d2 a2 14  8))
        (setf d2 (mod32+ (ripemd-128-regs-b regs) (mod32+ c1 d2))
              (ripemd-128-regs-b regs) (mod32+ (ripemd-128-regs-c regs) (mod32+ d1 a2))
              (ripemd-128-regs-c regs) (mod32+ (ripemd-128-regs-d regs) (mod32+ a1 b2))
              (ripemd-128-regs-d regs) (mod32+ (ripemd-128-regs-a regs) (mod32+ b1 c2))
              (ripemd-128-regs-a regs) d2)
        regs))))

(defstruct (ripemd-128
             (:constructor %make-ripemd-128-digest nil)
             (:constructor %make-ripemd-128-state (regs amount block buffer buffer-index))
             (:copier nil)
             (:include mdx))
  (regs (initial-ripemd-128-regs) :type ripemd-128-regs :read-only t)
  (block (make-array 16 :element-type '(unsigned-byte 32))
    :type (simple-array (unsigned-byte 32) (16)) :read-only t))

(defmethod reinitialize-instance ((state ripemd-128) &rest initargs)
  (declare (ignore initargs))
  (replace (ripemd-128-regs state) +pristine-ripemd-128-registers+)
  (setf (ripemd-128-amount state) 0
        (ripemd-128-buffer-index state) 0)
  state)

(defmethod copy-digest ((state ripemd-128) &optional copy)
  (check-type copy (or null ripemd-128))
  (cond
    (copy
     (replace (ripemd-128-regs copy) (ripemd-128-regs state))
     (replace (ripemd-128-buffer copy) (ripemd-128-buffer state))
     (setf (ripemd-128-amount copy) (ripemd-128-amount state)
           (ripemd-128-buffer-index copy) (ripemd-128-buffer-index state))
     copy)
    (t
     (%make-ripemd-128-state (copy-seq (ripemd-128-regs state))
                             (ripemd-128-amount state)
                             (copy-seq (ripemd-128-block state))
                             (copy-seq (ripemd-128-buffer state))
                             (ripemd-128-buffer-index state)))))

(define-digest-updater ripemd-128
  "Update the given ripemd-128-state from sequence, which is either a
simple-string or a simple-array with element-type (unsigned-byte 8),
bounded by start and end, which must be numeric bounding-indices."
  (flet ((compress (state sequence offset)
           (let ((block (ripemd-128-block state)))
             (fill-block-ub8-le block sequence offset)
             (update-ripemd-128-block (ripemd-128-regs state) block))))
    (declare (dynamic-extent #'compress))
    (declare (notinline mdx-updater))
    (mdx-updater state #'compress sequence start end)))

(define-digest-finalizer (ripemd-128 16)
  "If the given ripemd-128-state has not already been finalized, finalize it,
by processing any remaining input in its buffer, with suitable padding
and appended bit-length, as specified by the RIPEMD-128 standard.

The resulting RIPEMD-128 message-digest is returned as an array of twenty
 (unsigned-byte 8) values.  Calling UPDATE-RIPEMD-128-STATE after a call to
FINALIZE-RIPEMD-128-STATE results in unspecified behaviour."
  (let ((regs (ripemd-128-regs state))
        (block (ripemd-128-block state))
        (buffer (ripemd-128-buffer state))
        (buffer-index (ripemd-128-buffer-index state))
        (total-length (* 8 (ripemd-128-amount state))))
    (declare (type ripemd-128-regs regs)
             (type (integer 0 63) buffer-index)
             (type (simple-array (unsigned-byte 32) (16)) block)
             (type (simple-array (unsigned-byte 8) (*)) buffer))
    ;; Add mandatory bit 1 padding
    (setf (aref buffer buffer-index) #x80)
    ;; Fill with 0 bit padding
    (loop for index of-type (integer 0 64)
       from (1+ buffer-index) below 64
       do (setf (aref buffer index) #x00))
    (fill-block-ub8-le block buffer 0)
    ;; Flush block first if length wouldn't fit
    (when (>= buffer-index 56)
      (update-ripemd-128-block regs block)
      ;; Create new fully 0 padded block
      (loop for index of-type (integer 0 16) from 0 below 16
         do (setf (aref block index) #x00000000)))
    ;; Add 64bit message bit length
    (store-data-length block total-length 14)
    ;; Flush last block
    (update-ripemd-128-block regs block)
    ;; Done, remember digest for later calls
    (finalize-registers state regs)))

(defdigest ripemd-128 :digest-length 16 :block-length 64)
