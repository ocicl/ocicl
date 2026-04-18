;;; buffer-pool.lisp --- Buffer pooling and allocation context for pure-tls
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 John C. Mallery <jcma@csail.mit.edu>
;;;
;;; Reduces per-record allocation in pure-tls by pooling TLS record
;;; buffers and providing a scoped allocation context that automatically
;;; returns buffers to the pool on scope exit.
;;;
;;; Uses bordeaux-threads for portability (already a pure-tls dependency).

(in-package #:pure-tls)

;;;; Buffer Pool
;;;
;;; Three-tier thread-safe buffer pool: small (64B), medium (1KB),
;;; large (16KB+256 for max TLS record + AEAD overhead).
;;; Oversized buffers are allocated directly and not pooled.

(defstruct (buffer-pool (:constructor %make-buffer-pool))
  (lock (bt:make-lock "buffer-pool"))
  (small nil :type list)
  (medium nil :type list)
  (large nil :type list)
  (small-count 0 :type fixnum)
  (medium-count 0 :type fixnum)
  (large-count 0 :type fixnum))

(defvar *buffer-pool* (%make-buffer-pool)
  "Global buffer pool for TLS record buffers.")

(defconstant +pool-small-size+ 64)
(defconstant +pool-medium-size+ 1024)
(defconstant +pool-large-size+ 16640)  ; 16384 + 256 padding
(defconstant +pool-max-per-size+ 32)   ; max cached buffers per size class

(defun pool-acquire (pool size)
  "Acquire a buffer of at least SIZE bytes from POOL, or allocate fresh."
  (declare (type buffer-pool pool)
           (type fixnum size))
  (cond
    ((<= size +pool-small-size+)
     (bt:with-lock-held ((buffer-pool-lock pool))
       (let ((buf (pop (buffer-pool-small pool))))
         (cond (buf (decf (buffer-pool-small-count pool)) buf)
               (t (make-octet-vector +pool-small-size+))))))
    ((<= size +pool-medium-size+)
     (bt:with-lock-held ((buffer-pool-lock pool))
       (let ((buf (pop (buffer-pool-medium pool))))
         (cond (buf (decf (buffer-pool-medium-count pool)) buf)
               (t (make-octet-vector +pool-medium-size+))))))
    ((<= size +pool-large-size+)
     (bt:with-lock-held ((buffer-pool-lock pool))
       (let ((buf (pop (buffer-pool-large pool))))
         (cond (buf (decf (buffer-pool-large-count pool)) buf)
               (t (make-octet-vector +pool-large-size+))))))
    ;; Oversized — allocate directly, not poolable
    (t (make-octet-vector size))))

(defun pool-release (pool buf)
  "Return BUF to POOL for reuse.  Oversized buffers are dropped."
  (declare (type buffer-pool pool)
           (type (simple-array (unsigned-byte 8) (*)) buf))
  (let ((len (length buf)))
    (bt:with-lock-held ((buffer-pool-lock pool))
      (cond
        ((= len +pool-small-size+)
         (when (< (buffer-pool-small-count pool) +pool-max-per-size+)
           (push buf (buffer-pool-small pool))
           (incf (buffer-pool-small-count pool))))
        ((= len +pool-medium-size+)
         (when (< (buffer-pool-medium-count pool) +pool-max-per-size+)
           (push buf (buffer-pool-medium pool))
           (incf (buffer-pool-medium-count pool))))
        ((= len +pool-large-size+)
         (when (< (buffer-pool-large-count pool) +pool-max-per-size+)
           ;; Zero before returning to pool (security)
           (fill buf 0)
           (push buf (buffer-pool-large pool))
           (incf (buffer-pool-large-count pool))))))))

;;;; Buffer Allocation Context
;;;
;;; Region-based deallocation: all buffers acquired via BUFFER-POOL-ALLOCATE
;;; within a WITH-BUFFER-CONTEXT scope are automatically returned to the pool
;;; on scope exit (normal or abnormal) via unwind-protect.  Individual buffers
;;; can be deallocated early via BUFFER-POOL-DEALLOCATE.

(defstruct (buffer-context (:constructor %make-buffer-context))
  (pool nil :type buffer-pool)
  (allocated nil :type list))

(defvar *buffer-context* nil
  "Current buffer-context for this thread, or NIL outside any context.")

(defun buffer-pool-allocate (pool size)
  "Acquire a buffer of at least SIZE bytes from POOL and register it
with the current allocation context if one is active.
Note: The returned buffer may be larger than SIZE (tier-sized).
Callers must track the actual data length separately if needed."
  (let ((buf (pool-acquire pool size)))
    (when *buffer-context*
      (push buf (buffer-context-allocated *buffer-context*)))
    buf))

(defun buffer-pool-deallocate (pool buffer)
  "Return BUFFER to POOL and remove it from the current context list."
  (cond
    (*buffer-context*
     (let ((allocated (buffer-context-allocated *buffer-context*)))
       (cond
         ;; Head of list
         ((and allocated (eq (car allocated) buffer))
          (setf (buffer-context-allocated *buffer-context*) (cdr allocated))
          (pool-release pool buffer))
         ;; Scan interior
         (t
          (loop for prev on allocated
                for tail = (cdr prev)
                when (and tail (eq (car tail) buffer))
                  do (setf (cdr prev) (cdr tail))
                     (pool-release pool buffer)
                     (return)
                finally
                  (error "buffer-pool-deallocate: buffer not in current context."))))))
    (t
     (pool-release pool buffer))))

(defun buffer-pool-deallocate-all-buffers (pool)
  "Release every buffer on the current context list back to POOL.
Called by WITH-BUFFER-CONTEXT on scope exit via UNWIND-PROTECT."
  (when *buffer-context*
    (loop for buf in (buffer-context-allocated *buffer-context*)
          do (pool-release pool buf))
    (setf (buffer-context-allocated *buffer-context*) nil)))

(defmacro with-buffer-context ((pool) &body body)
  "Execute BODY with a fresh buffer allocation context.  All buffers
allocated via BUFFER-POOL-ALLOCATE are returned to POOL on exit
(normal or abnormal) unless explicitly deallocated earlier."
  (let ((pool-var (gensym "POOL")))
    `(let* ((,pool-var ,pool)
            (*buffer-context* (%make-buffer-context :pool ,pool-var)))
       (unwind-protect (progn ,@body)
         (buffer-pool-deallocate-all-buffers ,pool-var)))))
