;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; md4.lisp -- the MD4 digest algorithm as given in RFC1320

(in-package :crypto)


(define-digest-registers (md4 :endian :little)
  (a #x67452301)
  (b #xefcdab89)
  (c #x98badcfe)
  (d #x10325476))

(defconst +pristine-md4-registers+ (initial-md4-regs))

(defun update-md4-block (regs block)
  (declare (type md4-regs regs))
  (declare (type (simple-array (unsigned-byte 32) (16)) block)
           #.(burn-baby-burn))
  (let ((a (md4-regs-a regs))
        (b (md4-regs-b regs))
        (c (md4-regs-c regs))
        (d (md4-regs-d regs)))
    (declare (type (unsigned-byte 32) a b c d))
    (flet ((f (x y z)
             (declare (type (unsigned-byte 32) x y z))
             (logior (logand x y) (logandc1 x z)))
           (g (x y z)
             (declare (type (unsigned-byte 32) x y z))
             (logior (logand x y) (logand x z) (logand y z)))
           (h (x y z)
             (declare (type (unsigned-byte 32) x y z))
             (logxor x y z)))
      #+ironclad-fast-mod32-arithmetic
      (declare (inline f g h))
      (macrolet ((with-md4-round ((op block constant) &rest clauses)
                   (loop for (a b c d k s) in clauses
                         collect `(setq ,a (rol32 (mod32+
                                                   (mod32+ ,a
                                                           (mod32+ (,op ,b ,c ,d)
                                                                   (aref ,block ,k)))
                                                   ,constant) ,s)) into result
                         finally (return `(progn ,@result)))))
        (with-md4-round (f block 0)
          (a b c d 0 3) (d a b c 1 7) (c d a b 2 11) (b c d a 3 19)
          (a b c d 4 3) (d a b c 5 7) (c d a b 6 11) (b c d a 7 19)
          (a b c d 8 3) (d a b c 9 7) (c d a b 10 11) (b c d a 11 19)
          (a b c d 12 3) (d a b c 13 7) (c d a b 14 11) (b c d a 15 19))
        (with-md4-round (g block #x5a827999)
          (a b c d 0 3) (d a b c 4 5) (c d a b 8 9) (b c d a 12 13)
          (a b c d 1 3) (d a b c 5 5) (c d a b 9 9) (b c d a 13 13)
          (a b c d 2 3) (d a b c 6 5) (c d a b 10 9) (b c d a 14 13)
          (a b c d 3 3) (d a b c 7 5) (c d a b 11 9) (b c d a 15 13))
        (with-md4-round (h block #x6ed9eba1)
          (a b c d 0 3) (d a b c 8 9) (c d a b 4 11) (b c d a 12 15)
          (a b c d 2 3) (d a b c 10 9) (c d a b 6 11) (b c d a 14 15)
          (a b c d 1 3) (d a b c 9 9) (c d a b 5 11) (b c d a 13 15)
          (a b c d 3 3) (d a b c 11 9) (c d a b 7 11) (b c d a 15 15))
        (setf (md4-regs-a regs) (mod32+ (md4-regs-a regs) a)
              (md4-regs-b regs) (mod32+ (md4-regs-b regs) b)
              (md4-regs-c regs) (mod32+ (md4-regs-c regs) c)
              (md4-regs-d regs) (mod32+ (md4-regs-d regs) d))
        regs))))

(defstruct (md4
             (:constructor %make-md4-digest nil)
             (:constructor %make-md4-state (regs amount block buffer buffer-index))
             (:copier nil)
             (:include mdx))
  (regs (initial-md4-regs) :type md4-regs :read-only t)
  (block (make-array 16 :element-type '(unsigned-byte 32))
    :type (simple-array (unsigned-byte 32) (16)) :read-only t))

(defmethod reinitialize-instance ((state md4) &rest initargs)
  (declare (ignore initargs))
  (replace (md4-regs state) +pristine-md4-registers+)
  (setf (md4-amount state) 0
        (md4-buffer-index state) 0)
  state)

(defmethod copy-digest ((state md4) &optional copy)
  (check-type copy (or null md4))
  (cond
    (copy
     (replace (md4-regs copy) (md4-regs state))
     (replace (md4-buffer copy) (md4-buffer state))
     (setf (md4-amount copy) (md4-amount state)
           (md4-buffer-index copy) (md4-buffer-index state))
     copy)
    (t
     (%make-md4-state (copy-seq (md4-regs state))
                      (md4-amount state)
                      (copy-seq (md4-block state))
                      (copy-seq (md4-buffer state))
                      (md4-buffer-index state)))))

(define-digest-updater md4
  "Update the given md4-state from sequence, which is either a
simple-string or a simple-array with element-type (unsigned-byte 8),
bounded by start and end, which must be numeric bounding-indices."
  (flet ((compress (state sequence offset)
           (let ((block (md4-block state)))
             (fill-block-ub8-le block sequence offset)
             (update-md4-block (md4-regs state) block))))
    (declare (dynamic-extent #'compress))
    (declare (notinline mdx-updater))
    (mdx-updater state #'compress sequence start end)))

(define-digest-finalizer (md4 16)
  "If the given md4-state has not already been finalized, finalize it,
by processing any remaining input in its buffer, with suitable padding
and appended bit-length, as specified by the MD4 standard.

The resulting MD4 message-digest is returned as an array of sixteen
 (unsigned-byte 8) values.  Calling UPDATE-MD4-STATE after a call to
FINALIZE-MD4-STATE results in unspecified behaviour."
  (let ((regs (md4-regs state))
        (block (md4-block state))
        (buffer (md4-buffer state))
        (buffer-index (md4-buffer-index state))
        (total-length (* 8 (md4-amount state))))
    (declare (type md4-regs regs)
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
      (update-md4-block regs block)
      ;; Create new fully 0 padded block
      (loop for index of-type (integer 0 16) from 0 below 16
         do (setf (aref block index) #x00000000)))
    ;; Add 64bit message bit length
    (store-data-length block total-length 14)
    ;; Flush last block
    (update-md4-block regs block)
    ;; Done, remember digest for later calls
    (finalize-registers state regs)))

(defdigest md4 :digest-length 16 :block-length 64)
