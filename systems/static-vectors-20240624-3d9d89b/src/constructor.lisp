;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- MAKE-STATIC-VECTOR
;;;

(in-package :static-vectors)

(declaim (inline check-initial-element))
(defun check-initial-element (element-type initial-element)
  (when (not (typep initial-element element-type))
    ;; FIXME: signal SUBTYPE-ERROR
    (error "MAKE-STATIC-VECTOR: The type of :INITIAL-ELEMENT ~S is not a subtype ~
of the array's :ELEMENT-TYPE ~S"
           initial-element element-type)))

(declaim (inline check-initial-contents))
(defun check-initial-contents (length initial-contents)
  (let ((initial-contents-length (length initial-contents)))
    (when (/= length initial-contents-length)
      ;; FIXME: signal TYPE-ERROR
      (error "MAKE-STATIC-VECTOR: There are ~A elements in the :INITIAL-CONTENTS, ~
but requested vector length is ~A."
             initial-contents-length length))))

(declaim (inline check-initialization-arguments))
(defun check-initialization-arguments (initial-element-p initial-contents-p)
  (when (and initial-element-p initial-contents-p)
    ;; FIXME: signal ARGUMENT-LIST-ERROR
    (error "MAKE-STATIC-VECTOR: You must not specify both ~
:INITIAL-ELEMENT and :INITIAL-CONTENTS")))

(defun check-arguments (length element-type
                        initial-element initial-element-p
                        initial-contents initial-contents-p)
  (check-initialization-arguments initial-element-p initial-contents-p)
  (check-type length non-negative-fixnum)
  (when initial-element-p
    (check-initial-element element-type initial-element))
  (when initial-contents-p
    (check-initial-contents length initial-contents)))

(defconstant +default-alignment+ 16)
(defconstant +max-alignment+ 4096)

(declaim (inline make-static-vector))
(defun make-static-vector (length &key (element-type '(unsigned-byte 8))
                           (initial-element nil initial-element-p)
                           (initial-contents nil initial-contents-p)
                           (alignment nil alignp))
  "Create a simple vector of length LENGTH and type ELEMENT-TYPE which will
not be moved by the garbage collector. The vector might be allocated in
foreign memory so you must always call FREE-STATIC-VECTOR to free it."
  (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)
           (optimize speed))
  (check-arguments length element-type initial-element initial-element-p
                   initial-contents initial-contents-p)
  (when alignp
    ;; Check that the alignment is a power of 2 beteeen 16 and 4096.
    #+(and sbcl unix)
    (assert (and (<= +default-alignment+ alignment +max-alignment+)
                 (= 1 (logcount alignment))))
    #-(and sbcl unix)
    (error "Allocation alignment not supported on this implementation."))
  ;; TODO: fix %allocate-static-vector for all implementations
  (let ((vector
          (%allocate-static-vector length element-type
                                   #+sbcl
                                   (or alignment +default-alignment+))))
    (if initial-element-p
        (fill vector initial-element)
        (replace vector initial-contents))))

(defmacro with-static-vectors (((var length &rest args) &rest more-clauses)
                               &body body)
  "Allocate multiple static vectors at once."
  `(with-static-vector (,var ,length ,@args)
     ,@(if more-clauses
           `((with-static-vectors ,more-clauses
               ,@body))
           body)))
