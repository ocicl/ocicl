;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;
;;; Copyright (C) contributors as per cl+ssl git history
;;;
;;; See LICENSE for details.

;;;; CLISP speedup comments by Pixel / pinterface, 2007,
;;;; copied from https://code.kepibu.org/cl+ssl/
;;;;
;;;; ## Speeding Up clisp
;;;; cl+ssl has some serious speed issues on CLISP. For small requests,
;;;; it's not enough to worry about, but on larger requests the speed
;;;; issue can mean the difference between a 15 second download and a 15
;;;; minute download. And that just won't do!
;;;;
;;;; ### What Makes cl+ssl on clisp so slow?
;;;; On clisp, cffi's with-pointer-to-vector-data macro uses copy-in,
;;;; copy-out semantics, because clisp doesn't offer a with-pinned-object
;;;; facility or some other way of getting at the pointer to a simple-array.
;;;; Very sad, I know. In addition to being a leaky abstraction, wptvd is really slow.
;;;;
;;;; ### How to Speed Things Up?
;;;; The simplest thing that can possibly work: break the abstraction.
;;;; I introduce several new functions (buffer-length, buffer-elt, etc.)
;;;; and use those wherever an ssl-stream-*-buffer happens to be used,
;;;; in place of the corresponding sequence functions.
;;;; Those buffer-* functions operate on clisp's ffi:pointer objects,
;;;; resulting in a tremendous speedup--and probably a memory leak or two.
;;;;
;;;; ### This Is Not For You If...
;;;; While I've made an effort to ensure this patch doesn't break other
;;;; implementations, if you have code which relies on ssl-stream-*-buffer
;;;; returning an array you can use standard CL functions on, it will break
;;;; on clisp under this patch. But you weren't relying on cl+ssl
;;;; internals anyway, now were you?

(in-package :cl+ssl)

(defclass clisp-ffi-buffer ()
  ((size
    :initarg :size
    :accessor clisp-ffi-buffer-size)
   (pointer
    :initarg :pointer
    :accessor clisp-ffi-buffer-pointer)))

(defun make-buffer (size)
  (make-instance 'clisp-ffi-buffer
                 :size size
                 :pointer (cffi-sys:%foreign-alloc size)))

(defun buffer-length (buf)
  (clisp-ffi-buffer-size buf))

(defun buffer-elt (buf index)
  (ffi:memory-as (clisp-ffi-buffer-pointer buf) 'ffi:uint8 index))
(defun set-buffer-elt (buf index val)
  (setf (ffi:memory-as (clisp-ffi-buffer-pointer buf) 'ffi:uint8 index) val))
(defsetf buffer-elt set-buffer-elt)

(defparameter *mem-max* 1024 "so *-REPLACE require the expected O(1) memory")

(defun s/b-replace (seq buf &key (start1 0) end1 (start2 0) end2)
  (when (null end1) (setf end1 (length seq)))
  (when (null end2) (setf end2 (buffer-length buf)))
  (assert (<= 0 start1 end1)) ;length expensive. code deals correctly.
  (assert (<= 0 start2 end2 (buffer-length buf)))
  (let ((ptr (clisp-ffi-buffer-pointer buf))
        (c-type (make-array 3 :initial-contents #(ffi:c-array ffi:uint8 0)))
        (n (min (- end1 start1) (- end2 start2))))
    (labels ((buf-mem (s2 count)
               "returns exactly COUNT elts of BUF starting at S2"
               (setf (svref c-type 2) count)
               (ffi:memory-as ptr c-type s2)))
      (etypecase seq
        (vector
         (do* ((remainder n (- remainder m))
               (s1 start1 e1)
               (s2 start2 (+ s2 m))
               (m (min remainder *mem-max*) (min remainder *mem-max*))
               (e1 (+ s1 m) (+ s1 m)))
              ((zerop m))
           (replace seq (buf-mem s2 m) :start1 s1 :end1 e1)))
        (list
         (do* ((remainder n (- remainder m))
               (s2 start2 (+ s2 m))
               (seq1 (nthcdr start1 seq))
               (m (min remainder *mem-max*) (min remainder *mem-max*)))
              ((zerop m))
           (let ((tmp (buf-mem s2 m)))
             (dotimes (i m)
               (setf (car seq1) (aref tmp i)
                     seq1 (cdr seq1)))))))))
  seq)

(defun b/s-replace (buf seq &key (start1 0) end1 (start2 0) end2)
  (when (null end1) (setf end1 (buffer-length buf)))
  (when (null end2) (setf end2 (length seq)))
  (assert (<= 0 start1 end1 (buffer-length buf)))
  (assert (<= 0 start2 end2)) ;length expensive. code deals correctly.
  (let ((ptr (clisp-ffi-buffer-pointer buf))
        (c-type (make-array 3 :initial-contents #(ffi:c-array ffi:uint8 0)))
        (n (min (- end1 start1) (- end2 start2))))
    (labels ((set-buf-mem (s1 count vec s2)
               "replaces exactly COUNT elts of BUF starting at S1 with elts of
                VEC starting at S2"
               (declare (type vector vec))
               (setf (svref c-type 2) count)
               (setf (ffi:memory-as ptr c-type s1)
                     (cond ((and (zerop s2) (= count (length vec))) vec)
                           (t (make-array count
                                          :element-type (array-element-type vec)
                                          :displaced-to vec
                                          :displaced-index-offset s2))))))
      (etypecase seq
        (vector
         (assert (<= end2 (length seq)))
         (set-buf-mem start1 n seq start2))
        (list
         (do* ((tmp (make-array (min n *mem-max*)
                                :element-type '(unsigned-byte 8)))
               (remainder n (- remainder m))
               (s1 start1 (+ s1 m))
               (seq2 (nthcdr start2 seq))
               (m (min remainder *mem-max*) (min remainder *mem-max*)))
              ((zerop m))
           (dotimes (i m)
             (assert seq2)
             (setf (aref tmp i) (car seq2)
                   seq2 (cdr seq2)))
           (set-buf-mem s1 m tmp 0))))))
  buf)

(defmacro with-pointer-to-vector-data ((ptr buf) &body body)
  `(let ((,ptr (clisp-ffi-buffer-pointer ,buf)))
    ,@body))
