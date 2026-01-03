;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; This file implements the Whirlpool message-digest algoritm, as
;;;; defined in The WHIRLPOOL Hashing Function, by Paulo S.L.M. Barreto1
;;;; and Vincent Rijmen, revised on Revised on May 24, 2003 (1).
;;;;
;;;; It was written by Peter Gijsels.
;;;; Copyright (c) 2007, Peter Gijsels
;;;; All rights reserved.
;;;;
;;;; This software is "as is", and has no warranty of any kind.  The
;;;; authors assume no responsibility for the consequences of any use of
;;;; this software.

(in-package :crypto)

(eval-when (:compile-toplevel :load-toplevel :execute)
(deftype whirlpool-regs () '(simple-array (unsigned-byte 32) (64)))
(defun initial-whirlpool-regs ()
  (make-array 64 :element-type '(unsigned-byte 32) :initial-element 0))
(defconstant +whirlpool-regs-hash-offset+ 0)
(defconstant +whirlpool-regs-k-offset+ 16)
(defconstant +whirlpool-regs-state-offset+ 32)
(defconstant +whirlpool-regs-l-offset+ 48)
) ; EVAL-WHEN

(defconst +pristine-whirlpool-registers+ (initial-whirlpool-regs))

(defun whirlpool-regs-digest (regs buffer &optional (start 0))
  (declare (type whirlpool-regs regs)
           (type (integer 0 #.(- array-dimension-limit 64)) start))
  (flet ((stuff-registers (buffer start)
             (dotimes (i 16 buffer)
               (setf (ub32ref/be buffer (+ start (* 4 i))) (aref regs i)))))
    (declare (inline stuff-registers))
    (cond
      (buffer (stuff-registers buffer start))
      (t (stuff-registers (make-array 64 :element-type '(unsigned-byte 8)) 0)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant +whirlpool-rounds+ 10 "The number of rounds. The default is 10.")
)

(eval-when (:compile-toplevel)
  ;;; Code to generate lookup tables +C-EVEN+ and +C-ODD+.

  (defconst +e+ #(#x1 #xB #x9 #xC #xD #x6 #xF #x3 #xE #x8 #x7 #x4 #xA #x2 #x5 #x0))
  (defconst +r+ #(#x7 #xC #xB #xD #xE #x4 #x9 #xF #x6 #x3 #x8 #xA #x2 #x5 #x1 #x0))

  (defun e (i) (aref +e+ i))

  (defun r (i) (aref +r+ i))

  (defun e-1 (i) (position i +e+))

  (defun byte-xor (i1 i2) (logxor i1 i2))

  (defun s-internal (u v)
    "The S-box internals. Corresponds to equations on page 10 of (1)."
    (let ((r (r (byte-xor (e u) (e-1 v)))))
      (values (e (byte-xor (e u) r))
              (e-1 (byte-xor (e-1 v) r)))))

  (defun s (i)
    "The S-box function."
    (let ((u (ldb (byte 4 4) i))
          (v (ldb (byte 4 0) i)))
      (multiple-value-bind (u_ v_) (s-internal u v)
        (let ((result 0))
          (setf (ldb (byte 4 4) result) u_
                (ldb (byte 4 0) result) v_)
          result))))

  (defconstant +p8+ #.(reduce #'+ (mapcar #'(lambda (x) (expt 2 x)) '(8 4 3 2 0)))
               "The primitive polynomial of degree 8 for GF(2^8).")

  ;; Arithmetic in the Galois Field GF(2^8).
  (defun gf-add (x y)
    (logxor x y))

  (defun gf-shift (x n)
    (ash x n))
   
  (defun gf-reduce (x)
    (let ((result x))
      (loop until (< (integer-length result) (integer-length +p8+))
        do (setf result (gf-add result (gf-shift +p8+ (- (integer-length result) (integer-length +p8+))))))
      result))

  (defun gf-mult (x y)
    (loop with result = 0
       for i downfrom (integer-length y) to 0
       do (progn
            (setf result (gf-reduce (gf-shift result 1)))
            (unless (zerop (ldb (byte 1 i) y))
              (setf result (gf-add result x))))
       finally (return result)))

  (defun cir (vector)
    "The circulant matrix whose first row is VECTOR."
    (loop with n = (length vector)
       with result = (make-array (list n n))
       for i below n
       do (loop for j below n
             do (setf (aref result i j) (aref vector (mod (- j i) n))))
       finally (return result)))
  
  (defparameter *c* (cir #(1 1 4 1 8 5 2 9)))

  (defun calculate-table-word (i j offset)
    (loop with sx = (s j)
       with result = 0
       for k below 4
       do (setf (ldb (byte 8 (- 32 (* (1+ k) 8))) result) 
                (gf-mult sx (aref *c* i (+ k offset))))
       finally (return result)))

  (defun calculate-c-even ()
    (loop with result = (make-array '(8 256) :element-type '(unsigned-byte 32)
                                    :initial-element 0)
       for i below 8
       do (dotimes (j 256)
            (setf (aref result i j) (calculate-table-word i j 0)))
       finally (return result)))

  (defun calculate-c-odd ()
    (loop with result = (make-array '(8 256) :element-type '(unsigned-byte 32)
                                     :initial-element 0)
       for i below 8
       do (dotimes (j 256)
            (setf (aref result i j) (calculate-table-word i j 4)))
       finally (return result)))
) ; EVAL-WHEN

(declaim (type (simple-array (unsigned-byte 32) (22)) +rc+))
(defconst +rc+
  #.(loop with result = (make-array 22 :element-type '(unsigned-byte 32)
                                    :initial-element 0)
       with one-row-of-bytes = (make-array 8 :element-type '(unsigned-byte 8))
       for r from 1 to +whirlpool-rounds+
       do (progn
            (loop for j below 8 do
                 (setf (aref one-row-of-bytes j) (s (+ (* 8 (- r 1)) j))))
            (setf (aref result (* 2 r)) (ub32ref/be one-row-of-bytes 0))
            (setf (aref result (+ (* 2 r) 1)) (ub32ref/be one-row-of-bytes 4)))
       finally (return result)))

(declaim (type (simple-array (unsigned-byte 32) (8 256)) +c-even+ +c-odd+))
(defconst +c-even+ #.(calculate-c-even))
(defconst +c-odd+ #.(calculate-c-odd))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;; Macro helper functions.
  (defun extract-byte (k row column)
    (if (>= column 4)
        `(ldb (byte 8 ,(- 24 (* 8 (- column 4)))) (,k ,(1+ (* 2 row))))
        `(ldb (byte 8 ,(- 24 (* 8 column))) (,k ,(* 2 row)))))
  
  (defun split (lst)
    (let* ((n (length lst))
           (mid (floor n 2)))
      (values
       (subseq lst 0 mid)
       (subseq lst mid))))
  
  (defun generate-xor (terms)
    (if (endp (cdr terms))
        (car terms)
        (multiple-value-bind (terms1 terms2) (split terms)
          `(logxor ,(generate-xor terms1) ,(generate-xor terms2)))))
  
  (defun one-slice (to from i)
    (let ((indices (loop for n below 8 collect (gensym))))
      `(let (,@(loop for index in indices
                     for j below 8
                     collect `(,index ,(extract-byte from (mod (- i j) 8) j))))
        (setf (,to ,(* 2 i))
         ,(generate-xor `,(loop for index in indices
                                for j below 8
                                collect `(aref +c-even+ ,j ,index))))
        (setf (,to ,(1+ (* 2 i)))
         ,(generate-xor `,(loop for index in indices
                                for j below 8
                                collect `(aref +c-odd+ ,j ,index)))))))
) ; EVAL-WHEN

(defmacro lookup-in-c (to from)
  `(progn
    ,@(loop for i below 8 collect (one-slice to from i))))

(defun update-whirlpool-block (regs block)
  "this is the core part of the whirlpool algorithm. it takes a complete 16
word block of input, and updates the working state in the regs."
  (declare (type whirlpool-regs regs)
           (type (simple-array (unsigned-byte 32) (16)) block))
  (macrolet ((hash (i)
               `(aref regs (+ ,i +whirlpool-regs-hash-offset+)))
             (k (i)
               `(aref regs (+ ,i +whirlpool-regs-k-offset+)))
             (state (i)
               `(aref regs (+ ,i +whirlpool-regs-state-offset+)))
             (l (i)
               `(aref regs (+ ,i +whirlpool-regs-l-offset+))))
    ;; Compute and apply K^0 to the cipher state
    (loop for i below 16
       do (setf (state i) (logxor (aref block i) (setf (k i) (hash i)))))
    ;; Iterate over all rounds
    (loop for r of-type (integer 1 11) from 1 to +whirlpool-rounds+
       do (progn
            ;; Compute K^r from K^{r-1}
            (lookup-in-c l k)
            (setf (l 0) (logxor (l 0) (aref +rc+ (* 2 r))))
            (setf (l 1) (logxor (l 1) (aref +rc+ (+ (* 2 r) 1))))
            (loop for i below 16
               do (setf (k i) (l i)))
            ;; Apply the r-th round transformation
            (lookup-in-c l state)
            (loop for i below 16
               do (setf (l i) (logxor (l i) (k i))))
            (loop for i below 16
               do (setf (state i) (l i)))))
    ;; Apply the Miyaguchi-Preneel compression function
    (loop for i below 16
       do (setf (hash i)
                (logxor (hash i)
                        (logxor (state i)
                                (aref block i)))))
    regs))

;;; Mid-Level Drivers

(defstruct (whirlpool
             (:constructor %make-whirlpool-digest nil)
             (:constructor %make-whirlpool-state
                           (regs amount block buffer buffer-index))
             (:copier nil)
             (:include mdx))
  (regs (initial-whirlpool-regs) :type whirlpool-regs :read-only t)
  (block (make-array 16 :element-type '(unsigned-byte 32))
    :type (simple-array (unsigned-byte 32) (16)) :read-only t))

(defmethod reinitialize-instance ((state whirlpool) &rest initargs)
  (declare (ignore initargs))
  (replace (whirlpool-regs state) +pristine-whirlpool-registers+)
  (setf (whirlpool-amount state) 0
        (whirlpool-buffer-index state) 0)
  state)

(defmethod copy-digest ((state whirlpool) &optional copy)
  (check-type copy (or whirlpool null))
  (cond
    (copy
     (replace (whirlpool-regs copy) (whirlpool-regs state))
     (replace (whirlpool-buffer copy) (whirlpool-buffer state))
     (setf (whirlpool-amount copy) (whirlpool-amount state)
           (whirlpool-buffer-index copy) (whirlpool-buffer-index state))
     copy)
    (t
     (%make-whirlpool-state (copy-seq (whirlpool-regs state))
                            (whirlpool-amount state)
                            (copy-seq (whirlpool-block state))
                            (copy-seq (whirlpool-buffer state))
                            (whirlpool-buffer-index state)))))

(define-digest-updater whirlpool
  "Update the given whirlpool state from sequence, which is either a
simple-string or a simple-array with element-type (unsigned-byte 8),
bounded by start and end, which must be numeric bounding-indices."
  (flet ((compress (state sequence offset)
           (let ((block (whirlpool-block state)))
             (fill-block-ub8-be block sequence offset)
             (update-whirlpool-block (whirlpool-regs state) block))))
    (declare (dynamic-extent #'compress))
    (declare (notinline mdx-updater))
    (mdx-updater state #'compress sequence start end)))

(define-digest-finalizer (whirlpool 64)
  "If the given whirlpool-state has not already been finalized, finalize it,
by processing any remaining input in its buffer, with suitable padding
and appended bit-length, as specified by the Whirlpool standard.

The resulting whirlpool message-digest is returned as an array of 64
 (unsigned-byte 8) values.  Calling UPDATE-WHIRLPOOL-STATE after a call to
FINALIZE-WHIRLPOOL-STATE results in unspecified behaviour."
  (let ((regs (whirlpool-regs state))
        (block (whirlpool-block state))
        (buffer (whirlpool-buffer state))
        (buffer-index (whirlpool-buffer-index state))
        (total-length (* 8 (whirlpool-amount state))))
    (declare (type whirlpool-regs regs)
             (type (integer 0 63) buffer-index)
             (type (simple-array (unsigned-byte 32) (16)) block)
             (type (simple-array (unsigned-byte 8) (64)) buffer))
    ;; Add mandatory bit 1 padding
    (setf (aref buffer buffer-index) #x80)
    ;; Fill with 0 bit padding
    (loop for index of-type (integer 0 64)
       from (1+ buffer-index) below 64
       do (setf (aref buffer index) #x00))
    (fill-block-ub8-be block buffer 0)
    ;; Flush block first if length wouldn't fit
    (when (>= buffer-index 32)
      (update-whirlpool-block regs block)
      ;; Create new fully 0 padded block
      (loop for index of-type (integer 0 16) from 0 below 16
         do (setf (aref block index) #x00000000)))
    ;; Add 256 bit message bit length
    (loop for i of-type (integer 0 8) from 0 below 8
       do (setf (aref block (+ 8 i))
                (ldb (byte 32 (- 256 (* 32 (1+ i)))) total-length)))
    ;; Flush last block
    (update-whirlpool-block regs block)
    ;; Done, remember digest for later calls
    (finalize-registers state regs)))

(defdigest whirlpool :digest-length 64 :block-length 64)
