;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; MD5-LISPWORKS-INT32 - MD5 implementation using SYS:INT32 in Lispworks
;;;;
;;;; This file implements The MD5 Message-Digest Algorithm, as defined in 
;;;; RFC 1321 by R. Rivest, published April 1992.
;;;;
;;;; It was written by Pierre R. Mai, with copious input from the
;;;; cmucl-help mailing-list hosted at cons.org, in November 2001 and
;;;; has been placed into the public domain.
;;;;
;;;; This software is "as is", and has no warranty of any kind.  The
;;;; authors assume no responsibility for the consequences of any use
;;;; of this software.
;;;;
;;;; Subsequent modifications:
;;;; - LispWorks 4.4 sys:int32 port by Dmitriy Ivanov.
;;;; - Ironclad integration by Klaus Harbo <klaus@harbo.net>
;;;;

#+ironclad-md5-lispworks-int32
(cl:defpackage :md5-lispworks-int32
  (:use :cl))

#+ironclad-md5-lispworks-int32
(in-package :md5-lispworks-int32)
#-ironclad-md5-lispworks-int32
(in-package :cl-user)

#+ironclad-md5-lispworks-int32
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp (find-symbol "INT32+" '#:SYSTEM))
    (error 'ironclad-error :format-control "It does not look like this version of Lispworks supports the SYS:INT32 API.")))

#+ironclad-md5-lispworks-int32
(eval-when (:compile-toplevel :load-toplevel :execute)

(defun %optimization-settings ()
  '(optimize (speed 3) (safety 0) (space 0) (debug 0) (float 0)))

(defun %optimization-settings/no-fixnum-safety ()
  '(optimize (speed 3) (safety 0) (space 0) (debug 0) (float 0) (hcl:fixnum-safety 0)))

(defmacro assemble-ub32 (a b c d)
  "Assemble an ub32 value from the given (unsigned-byte 8) values,
where a is the intended low-order byte and d the high-order byte."
  `(sys:int32-logior (sys:int32<< ,d 24)
                     (sys:int32-logior (sys:int32<< ,c 16)
                                       (sys:int32-logior (sys:int32<< ,b 8) ,a))))

(defmacro make-ub32-vector (length &rest args)
  `(sys:make-simple-int32-vector ,length ,@args))

(defparameter *t*
  (make-ub32-vector 64 :initial-contents
                    (flet ((int32-unsigned-to-signed (unsigned)
                             (dpb (ldb (byte 32 0) unsigned) (byte 32 0) (if (logbitp 31 unsigned) -1 0))))
                      (loop for i from 1 to 64
                            collect (int32-unsigned-to-signed
                                     (truncate (* 4294967296
                                                  (abs (sin (float i 0.0d0)))))))))))

#+ironclad-md5-lispworks-int32
(progn
  ;;; This PROGN covers the rest of the file.
  ;;; Non-Lispworks implementation of MD5 in md5.lisp
  
  ;;; --------------------------------------------------------------------------------
  ;;; Section 2:  Basic Datatypes
  ;;; --------------------------------------------------------------------------------

(defmacro int32-vector-store-int32u (int32-vec index unsigned-int)
  "Stores the lowest 32 bit of the non-negative integer `INT' in the
the sys:simple-int32-vector `INT32-VEC' at index `INDEX'.  Since the
sys:int32 API uses signed integers, the most significant bit of
`UNSIGNED-INT' must be handled explicitly.  If `UNSIGNED-INT' is >=
2^31, the sys:int32 value stored will appear to be a negative value.
Use `INT32-VECTOR-READ-INT32U' to read the value as an unsigned
integer.  For maximum efficiency the entire function must be compiled
with `(OPTIMIZE (FLOAT 0))'."
  (let ((_tmp (gensym "TMP"))
        (_int (gensym "INT")))
    `(let* ((,_int ,unsigned-int)
            (,_tmp (sys:integer-to-int32 (ldb (byte 31 0) ,_int))))
       (setf (sys:int32-aref ,int32-vec ,index)
             (if (>= (ldb (byte 32 0) ,_int) #x80000000)
               (sys:int32-logior ,_tmp #.(sys:int32<< sys:+int32-1+ 31))
               ,_tmp)))))

(deftype ub32 ()
  "Corresponds to the 32bit quantity word of the MD5 Spec"
  'sys:int32)


;;; --------------------------------------------------------------------------------
;;; Section 3.4:  Auxilliary functions
;;; --------------------------------------------------------------------------------

(declaim (inline f g h i)
         (ftype (function (ub32 ub32 ub32) ub32) f g h i))

(defun f (x y z)
  (declare (type ub32 x y z)
           #.(%optimization-settings))
  (sys:int32-logior (sys:int32-logand x y) (sys:int32-logandc1 x z)))

(defun g (x y z)
  (declare (type ub32 x y z)
           #.(%optimization-settings))
  (sys:int32-logior (sys:int32-logand x z) (sys:int32-logandc2 y z)))

(defun h (x y z)
  (declare (type ub32 x y z)
           #.(%optimization-settings))
  (sys:int32-logxor x (sys:int32-logxor y z)))

(defun i (x y z)
  (declare (type ub32 x y z)
           #.(%optimization-settings))
  (sys:int32-logxor y (sys:int32-logorc2 x z)))

(declaim (inline mod32+)
         (ftype (function (ub32 ub32) ub32) mod32+))
(defun mod32+ (a b)
  (declare (type ub32 a b)
           #.(%optimization-settings))
  (sys:int32+ a b))

(define-compiler-macro mod32+ (a b)
  `(sys:int32+ ,a ,b))

(declaim (inline int32>>logical)
         (ftype (function (sys:int32 (unsigned-byte 5)) sys:int32) int32>>logical))
(defun int32>>logical (a s)
  ;; Logical right shift, suppressing sign bits.
  ;; Args: 0 < s < 32 as sys:int32<< does not work with zero or negative shift offsets.
  (if (sys:int32-minusp a)
    (sys:int32-logandc2 (sys:int32>> a s) (sys:int32<< -1 (- 32 s)))
    (sys:int32>> a s)))

(declaim (inline rol32)
         (ftype (function (ub32 (unsigned-byte 5)) ub32) rol32))
(defun rol32 (a s)
  (declare (type ub32 a) (type (unsigned-byte 5) s)
           #.(%optimization-settings))
  (sys:int32-logior (sys:int32<< a s) (int32>>logical a (- 32 s))))

;;; --------------------------------------------------------------------------------
;;; Section 3.4:  Table T
;;; --------------------------------------------------------------------------------

(deftype ub32-vector (length)
  `(sys:simple-int32-vector ,length))

(defmacro ub32-aref (vector index)
  `(sys:int32-aref ,vector ,index))

(defmacro with-md5-round ((op block) &rest clauses)
  (loop for (a b c d k s i) in clauses
        collect
        `(setq ,a (mod32+ ,b (rol32 (mod32+ (mod32+ ,a (,op ,b ,c ,d))
                                            (mod32+ (ub32-aref ,block ,k)
                                                    ,(sys:int32-to-integer        ; due to LW bug
                                                      (ub32-aref *t* (1- i)))))
                                    ,s)))
        into result
        finally
        (return `(progn ,@result))))

;;; --------------------------------------------------------------------------------
;;; Section 3.3:  (Initial) MD5 Working Set
;;; --------------------------------------------------------------------------------

(deftype md5-regs ()
  "The working state of the MD5 algorithm, which contains the 4 32-bit
registers A, B, C and D."
  `(ub32-vector 4))

(defmacro md5-regs-a (regs)
  `(ub32-aref ,regs 0))
(defmacro md5-regs-b (regs)
  `(ub32-aref ,regs 1))
(defmacro md5-regs-c (regs)
  `(ub32-aref ,regs 2))
(defmacro md5-regs-d (regs)
  `(ub32-aref ,regs 3))

(defconstant +md5-magic-a+ (assemble-ub32 #x01 #x23 #x45 #x67)
  "Initial value of Register A of the MD5 working state.")
(defconstant +md5-magic-b+ (assemble-ub32 #x89 #xab #xcd #xef)
  "Initial value of Register B of the MD5 working state.")
(defconstant +md5-magic-c+ (assemble-ub32 #xfe #xdc #xba #x98)
  "Initial value of Register C of the MD5 working state.")
(defconstant +md5-magic-d+ (assemble-ub32 #x76 #x54 #x32 #x10)
  "Initial value of Register D of the MD5 working state.")

(declaim (inline initial-md5-regs))
(defun initial-md5-regs ()
  "Create the initial working state of an MD5 run."
  (declare #.(%optimization-settings))
  (let ((regs (make-ub32-vector 4)))
    (declare (type md5-regs regs))
    (setf (md5-regs-a regs) +md5-magic-a+
          (md5-regs-b regs) +md5-magic-b+
          (md5-regs-c regs) +md5-magic-c+
          (md5-regs-d regs) +md5-magic-d+)
    regs))

;;; --------------------------------------------------------------------------------
;;; Section 3.4:  Operation on 16-Word Blocks
;;; --------------------------------------------------------------------------------

(defun update-md5-block (regs block)
  "This is the core part of the MD5 algorithm.  It takes a complete 16
word block of input, and updates the working state in A, B, C, and D
accordingly."
  (declare (type md5-regs regs)
           (type (ub32-vector 16) block)
           #.(%optimization-settings))
  (let ((a (md5-regs-a regs)) (b (md5-regs-b regs))
        (c (md5-regs-c regs)) (d (md5-regs-d regs)))
    (declare (type ub32 a b c d))
    ;; Round 1
    (with-md5-round (f block)
                    (A B C D  0  7  1)(D A B C  1 12  2)(C D A B  2 17  3)(B C D A  3 22  4)
                    (A B C D  4  7  5)(D A B C  5 12  6)(C D A B  6 17  7)(B C D A  7 22  8)
                    (A B C D  8  7  9)(D A B C  9 12 10)(C D A B 10 17 11)(B C D A 11 22 12)
                    (A B C D 12  7 13)(D A B C 13 12 14)(C D A B 14 17 15)(B C D A 15 22 16))
    ;; Round 2
    (with-md5-round (g block)
                    (A B C D  1  5 17)(D A B C  6  9 18)(C D A B 11 14 19)(B C D A  0 20 20)
                    (A B C D  5  5 21)(D A B C 10  9 22)(C D A B 15 14 23)(B C D A  4 20 24)
                    (A B C D  9  5 25)(D A B C 14  9 26)(C D A B  3 14 27)(B C D A  8 20 28)
                    (A B C D 13  5 29)(D A B C  2  9 30)(C D A B  7 14 31)(B C D A 12 20 32))
    ;; Round 3
    (with-md5-round (h block)
                    (A B C D  5  4 33)(D A B C  8 11 34)(C D A B 11 16 35)(B C D A 14 23 36)
                    (A B C D  1  4 37)(D A B C  4 11 38)(C D A B  7 16 39)(B C D A 10 23 40)
                    (A B C D 13  4 41)(D A B C  0 11 42)(C D A B  3 16 43)(B C D A  6 23 44)
                    (A B C D  9  4 45)(D A B C 12 11 46)(C D A B 15 16 47)(B C D A  2 23 48))
    ;; Round 4
    (with-md5-round (i block)
                    (A B C D  0  6 49)(D A B C  7 10 50)(C D A B 14 15 51)(B C D A  5 21 52)
                    (A B C D 12  6 53)(D A B C  3 10 54)(C D A B 10 15 55)(B C D A  1 21 56)
                    (A B C D  8  6 57)(D A B C 15 10 58)(C D A B  6 15 59)(B C D A 13 21 60)
                    (A B C D  4  6 61)(D A B C 11 10 62)(C D A B  2 15 63)(B C D A  9 21 64))
    ;; Update and return
    (setf (md5-regs-a regs) (mod32+ (md5-regs-a regs) a)
          (md5-regs-b regs) (mod32+ (md5-regs-b regs) b)
          (md5-regs-c regs) (mod32+ (md5-regs-c regs) c)
          (md5-regs-d regs) (mod32+ (md5-regs-d regs) d))
    regs))

;;; --------------------------------------------------------------------------------
;;; Section 3.4:  Converting 8bit-vectors into 16-Word Blocks
;;; --------------------------------------------------------------------------------

(declaim (inline fill-block-ub8))
(defun fill-block-ub8 (block buffer offset)
  "Convert a complete 64 (unsigned-byte 8) input vector segment
starting from offset into the given 16 word MD5 block."
  (declare (type (integer 0 #.(- most-positive-fixnum 64)) offset)
           (type (ub32-vector 16) block)
           (type (simple-array (unsigned-byte 8) (*)) buffer)
           #.(%optimization-settings/no-fixnum-safety))
  (loop for i of-type (integer 0 16) from 0
        for j of-type (integer 0 #.most-positive-fixnum)
        from offset to (+ offset 63) by 4
        do
        (setf (ub32-aref block i)
              (assemble-ub32 (aref buffer j)
                             (aref buffer (+ j 1))
                             (aref buffer (+ j 2))
                             (aref buffer (+ j 3))))))

;;; --------------------------------------------------------------------------------
;;; Section 3.5:  Message Digest Output
;;; --------------------------------------------------------------------------------

(declaim (inline md5regs-digest))
(defun md5regs-digest (regs buffer buffer-start)
  "Create the final 16 byte message-digest from the MD5 working state
in regs.  Returns a (simple-array (unsigned-byte 8) (16))."
  (declare #.(%optimization-settings/no-fixnum-safety)
           (type md5-regs regs))
  (macrolet ((frob (reg offset)
               (let ((var (gensym)))
                 `(let ((,var (sys:int32-to-integer ,reg)))
                    (declare (type (unsigned-byte 32) ,var))
                    (setf
                     (aref buffer (+ buffer-start ,offset)) (ldb (byte 8 0) ,var)
                     (aref buffer (+ buffer-start ,(+ offset 1))) (ldb (byte 8 8) ,var)
                     (aref buffer (+ buffer-start ,(+ offset 2))) (ldb (byte 8 16) ,var)
                     (aref buffer (+ buffer-start ,(+ offset 3))) (ldb (byte 8 24) ,var))))))
    (frob (md5-regs-a regs) 0)
    (frob (md5-regs-b regs) 4)
    (frob (md5-regs-c regs) 8)
    (frob (md5-regs-d regs) 12))
  buffer)

(defstruct (ironclad::md5 (:constructor make-md5-int32 ())
                          (:copier))
  (regs (initial-md5-regs) :type md5-regs :read-only t)
  (amount 0 :type (integer 0 *))
  (block (make-ub32-vector 16) :read-only t :type (ub32-vector 16))
  (buffer (make-array 64 :element-type '(unsigned-byte 8)) :read-only t
          :type (simple-array (unsigned-byte 8) (64)))
  (buffer-index 0 :type (integer 0 63))
  (finalized-p nil))

(defmacro %md5-regs (x)
  `(md5-regs ,x))
(defmacro %md5-amount (x)
  `(md5-amount ,x))
(defmacro %md5-block (x)
  `(md5-block ,x))
(defmacro %md5-buffer (x)
  `(md5-buffer ,x))
(defmacro %md5-buffer-index (x)
  `(md5-buffer-index ,x))
(defmacro %md5-finalized-p (x)
  `(md5-finalized-p ,x))

(declaim (inline copy-to-buffer))
(defun copy-to-buffer (from from-offset count buffer buffer-offset)
  "Copy a partial segment from input vector from starting at
from-offset and copying count elements into the 64 byte buffer
starting at buffer-offset."
  (declare #.(%optimization-settings/no-fixnum-safety)
           (type fixnum from-offset)
           (type (integer 0 63) count buffer-offset)
           (type (simple-array * (*)) from)
           (type (simple-array (unsigned-byte 8) (64)) buffer))
  (etypecase from
    ((simple-array (unsigned-byte 8) (*))
     (loop for buffer-index of-type (integer 0 64) from buffer-offset
           for from-index of-type fixnum from from-offset
           below (+ from-offset count)
           do
           (setf (aref buffer buffer-index)
                 (aref (the (simple-array (unsigned-byte 8) (*)) from)
                       from-index))))))

(defun %md5-lispworks-int32-update-state (state sequence &key (start 0) (end (length sequence)))
  "Update the given md5-state from sequence, which must be
simple-array with element-type (unsigned-byte 8), bounded by start and
end, which must be numeric bounding-indices."
  (declare (type ironclad::md5 state)
           (type (simple-array * (*)) sequence)
           (type fixnum start end)
           #.(%optimization-settings/no-fixnum-safety))
  (let ((regs (%md5-regs state))
        (block (%md5-block state))
        (buffer (%md5-buffer state))
        (buffer-index (%md5-buffer-index state))
        (length (- end start)))
    (declare (type md5-regs regs) (type fixnum length)
             (type (integer 0 63) buffer-index)
             (type (ub32-vector 16) block)
             (type (simple-array (unsigned-byte 8) (64)) buffer))
    ;; Handle old rest
    (unless (zerop buffer-index)
      (let ((amount (min (- 64 buffer-index) length)))
        (declare (type (integer 0 63) amount))
        (copy-to-buffer sequence start amount buffer buffer-index)
        (setq start (the fixnum (+ start amount)))
        (let ((new-index (mod (+ buffer-index amount) 64)))
          (when (zerop new-index)
            (fill-block-ub8 block buffer 0)
            (update-md5-block regs block))
          (when (>= start end)
            (setf (%md5-buffer-index state) new-index)
            (incf (%md5-amount state) length)
            (return-from %md5-lispworks-int32-update-state state)))))
    ;; Handle main-part and new-rest
    (etypecase sequence
      ((simple-array (unsigned-byte 8) (*))
       (locally
         (declare (type (simple-array (unsigned-byte 8) (*)) sequence))
         (loop for offset of-type fixnum from start below end by 64
               until (< (- end offset) 64)
               do
               (fill-block-ub8 block sequence offset)
               (update-md5-block regs block)
               finally
               (let ((amount (- end offset)))
                 (unless (zerop amount)
                   (copy-to-buffer sequence offset amount buffer 0))
                 (setf (%md5-buffer-index state) amount))))))
    (locally
      (declare (optimize (hcl:fixnum-safety 3)))
      (setf (%md5-amount state) (+ (%md5-amount state) length)))
    state))

(defun %md5-lispworks-int32-finalize-state (state digest-buffer digest-buffer-start)
  "If the given md5-state has not already been finalized, finalize it,
by processing any remaining input in its buffer, with suitable padding
and appended bit-length, as specified by the MD5 standard.

The resulting MD5 message-digest is returned as an array of sixteen
\(unsigned-byte 8) values.  Calling `update-md5-state' after a call to
`finalize-md5-state' results in unspecified behaviour."
  (declare (type ironclad::md5 state)
           (optimize (speed 0) (space 0) (debug 2) (float 0))
           (optimize (hcl:fixnum-safety 0)))
  (or (%md5-finalized-p state)
      (let ((regs (%md5-regs state))
            (block (%md5-block state))
            (buffer (%md5-buffer state))
            (buffer-index (%md5-buffer-index state))
            (total-length (* 8 (%md5-amount state))))        ; potentially bignum
        (declare (type md5-regs regs)
                 (type (integer 0 63) buffer-index)
                 (type (ub32-vector 16) block)
                 (type (simple-array (unsigned-byte 8) (*)) buffer))
        ;; Add mandatory bit 1 padding
        (setf (aref buffer buffer-index) #x80)
        ;; Fill with 0 bit padding
        (loop for index of-type (integer 0 64)
              from (1+ buffer-index) below 64
              do (setf (aref buffer index) #x00))
        (fill-block-ub8 block buffer 0)
        ;; Flush block first if length wouldn't fit
        (when (>= buffer-index 56)
          (update-md5-block regs block)
          ;; Create new fully 0 padded block
          (loop for index of-type (integer 0 16) from 0 below 16
                do (setf (ub32-aref block index) #x00000000)))
        ;; Add 64bit message bit length
        (int32-vector-store-int32u block 14 total-length)
        (int32-vector-store-int32u block 15 (ldb (byte 32 32) total-length))
        ;; Flush last block
        (update-md5-block regs block)
        ;; Done, remember digest for later calls
        (setf (%md5-finalized-p state)
              (md5regs-digest regs digest-buffer digest-buffer-start)))))

;;; --------------------------------------------------------------------------------
;;; IRONCLAD INTERFACING SUPPORT FUNCTIONS
;;; --------------------------------------------------------------------------------

(defun %md5-lispworks-int32-reinitialize (state)
  (let ((regs (%md5-regs state)))
    (setf (md5-regs-a regs) +md5-magic-a+
          (md5-regs-b regs) +md5-magic-b+
          (md5-regs-c regs) +md5-magic-c+
          (md5-regs-d regs) +md5-magic-d+))
  (setf (%md5-amount state) 0)
  (setf (%md5-buffer-index state) 0)
  (setf (%md5-finalized-p state) nil)
  state)

(defmethod %md5-lispworks-int32-copy ((old ironclad::md5)
                                      copy)
  (let* ((new (or copy (make-md5-int32)))
         (old-regs (%md5-regs old))
         (new-regs (%md5-regs new)))
    (setf (md5-regs-a new-regs) (md5-regs-a old-regs)
          (md5-regs-b new-regs) (md5-regs-b old-regs)
          (md5-regs-c new-regs) (md5-regs-c old-regs)
          (md5-regs-d new-regs) (md5-regs-d old-regs))
    (replace (%md5-buffer new) (%md5-buffer old))
    (setf (%md5-amount new) (%md5-amount old)
          (%md5-buffer-index new) (%md5-buffer-index old)
          (%md5-finalized-p new) (%md5-finalized-p old))
    new))


;;; --------------------------------------------------------------------------------
;;; IRONCLAD INTERFACE
;;; --------------------------------------------------------------------------------

(defmethod reinitialize-instance ((state ironclad::md5) &rest initargs)
  (%md5-lispworks-int32-reinitialize state))

(defmethod ironclad:copy-digest ((state ironclad::md5) &optional copy)
  (%md5-lispworks-int32-copy state (or copy (ironclad::%make-md5-digest))))

(ironclad::define-digest-updater ironclad::md5
  (%md5-lispworks-int32-update-state ironclad::state ironclad::sequence
                                     :start ironclad::start
                                     :end ironclad::end ))

(ironclad::define-digest-finalizer (ironclad::md5 16)
  (%md5-lispworks-int32-finalize-state ironclad::state
                                       ironclad::%buffer
                                       ironclad::buffer-start))

(defun ironclad::%make-md5-digest ()
  (make-md5-int32))

(ironclad::defdigest ironclad::md5 :digest-length 16 :block-length 64)

) ; all-encompassing progn

;eof
