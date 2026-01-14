;;;; Copyright (c) 2017-2019 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(cl:in-package #:esrap)

(defconstant +chunk-divisor+ 8)

;;; CHUNK
;;;
;;; A vector of fixed size (ash 1 +chunk-divisor+) the indices of
;;; which correspond to a range of input positions.

(deftype chunk ()
  '(simple-array t 1))

(declaim (inline make-chunk))
(defun make-chunk ()
  (make-array (ash 1 +chunk-divisor+) :initial-element nil))

;;; CHUNK-ARRAY
;;;
;;; An array of CHUNKs.

(deftype chunk-array ()
  '(simple-array (or (eql 0) chunk) 1))

(declaim (inline %make-chunk-array))
(defun %make-chunk-array (length)
  (declare (type array-index length) (optimize speed))
  (make-array length :initial-element 0))

(declaim (ftype (function (array-index) (values chunk-array &optional))
                make-chunk-array))
(defun make-chunk-array (length)
  (declare (optimize speed))
  (%make-chunk-array (1+ (ash length (- +chunk-divisor+)))))

;;; CHUNK-CACHE
;;;
;;; Maps input positions to CHUNKs, potentially allocating new CHUNKs.

(declaim (inline chunk-cache-chunks))
(defstruct (chunk-cache
             (:constructor
              make-chunk-cache
              (length &aux (chunks (make-chunk-array length))))
             (:predicate nil)
             (:copier nil))
  (chunks nil :type chunk-array))
#+sbcl (declaim (sb-ext:freeze-type chunk-cache))

(declaim (ftype (function (input-position chunk-cache) (values (or null chunk) &optional))
                find-chunk)
         (inline find-chunk))
(defun find-chunk (position chunk-cache)
  (declare (optimize speed))
  (let* ((chunks     (chunk-cache-chunks chunk-cache))
         (position-1 (ash position (- +chunk-divisor+))))
    (let ((current (aref chunks position-1)))
      (unless (eql current 0)
        current))))
(declaim (notinline find-chunk))

(declaim (ftype (function (input-position chunk-cache)
                          (values chunk &optional))
                ensure-chunk))
(defun ensure-chunk (position chunk-cache)
  (declare (optimize speed))
  (or (locally (declare (inline find-chunk))
        (find-chunk position chunk-cache))
      (let ((chunks     (chunk-cache-chunks chunk-cache))
            (position-1 (ash position (- +chunk-divisor+))))
        (setf (aref chunks position-1) (make-chunk)))))
