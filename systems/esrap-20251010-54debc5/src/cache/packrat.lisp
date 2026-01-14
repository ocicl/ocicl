;;;; Copyright (c) 2017-2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

;;; Packrat cache
;;;
;;; A cache mapping pairs (input position, rule name) to parse
;;; results. Its purpose is avoiding multiple identical applications
;;; of rules. This can improve performance and act as the foundation
;;; of a framework for handling left recursion.
;;;
;;; Since reads from and writes to this cache can be a performance
;;; bottleneck, the implementation tries to be as runtime and memory
;;; efficient as possible. A two-level scheme maps the pairs
;;; mentioned above to parse results:
;;; 1. an array maps the input position to a secondary structure
;;; 2. this structure maps the rule name to the cached parse results
;;;
;;; The interesting part about 1. is not allocating an array of the
;;; same size as the input upfront while keeping lookup performance
;;; reasonable. This trade-off is achieved using a "chunk cache".
;;;
;;; The difficulty with 2. is the variety of scenarios that have to be
;;; supported efficiently w.r.t. memory and runtime. To address this
;;; issue, the secondary structure uses one of multiple
;;; representations depending on the situation:
;;;
;;; + If only a mapping from a single rule to the associated parse
;;;   result has to be represented, a single cons cell is used.
;;;
;;; + For a small number of mapping entries, the number of entries and
;;;   the entries are stored in a vector.
;;;
;;; + In the (uncommon) case that more than a few entries have to be
;;;   stored, a hash-table is used.
;;;
;;; Switches between representations happen when entries are added.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; The max number of entries in the vector before switching to a
  ;; hash table.
  (defconstant +packrat-hash-table-switch-point+ 20)
  ;; The hash table is created with this :SIZE. There is some subtlety
  ;; here as SBCL's adaptive EQ hash tables perform a trick similar to
  ;; the vector representation (called "flat hash table" there) up to
  ;; SB-IMPL::+FLAT-LIMIT/EQ+ (16 or 32 depending on the platform
  ;; currently). There are many entries already, there'll likely be
  ;; more. To avoid the cost of SBCL having to switching away from the
  ;; flat representation, we specify a number greater than the maximum
  ;; flat size.
  (defconstant +packrat-initial-hash-table-size+
    #+sbcl (max 33 (* 2 +packrat-hash-table-switch-point+))
    #-sbcl (* 2 +packrat-hash-table-switch-point+))
  (defconstant +packrat-cache-array-size+
    (1+ (* 2 +packrat-hash-table-switch-point+))))

(declaim (ftype (function (symbol input-position chunk-cache)
                          (values t &optional))
                cached))
(defun cached (symbol position cache)
  (declare (optimize speed))
  (let* ((chunk      (find-chunk position cache))
         (position-2 (ldb (byte +chunk-divisor+ 0) position))
         (cell       (when chunk
                       (aref chunk position-2))))
    (cond ((null cell)
           nil)
          ((consp cell)
           (when (eq (car cell) symbol)
             (cdr cell)))
          ((typep cell '(simple-vector #.+packrat-cache-array-size+))
           (let ((fill-pointer (aref cell 0)))
             (declare (type (integer 0 #.+packrat-cache-array-size+)
                            fill-pointer))
             (loop for i upfrom 1 below fill-pointer by 2
                   when (eq (aref cell i) symbol)
                     do (return (aref cell (1+ i))))))
          (t
           (values (gethash symbol cell))))))

(declaim (ftype (function (t symbol input-position chunk-cache)
                          (values t &optional))
                (setf cached)))
(defun (setf cached) (result symbol position cache)
  (declare (optimize speed))
  (let* ((chunk      (ensure-chunk position cache))
         (position-2 (ldb (byte +chunk-divisor+ 0) position))
         (cell       (aref chunk position-2)))
    (cond

      ;; No entry (NIL) => single entry (CONS)
      ((null cell)
       (setf (aref chunk position-2) (cons symbol result)))

      ;; Single entry (CONS) => few entries (SIMPLE-VECTOR)
      ((consp cell)
       (if (eq (car cell) symbol)
           (setf (cdr cell) result)
           (let ((a (make-array +packrat-cache-array-size+)))
             (setf (aref a 0) 5         ; fill pointer
                   (aref a 1) (car cell)
                   (aref a 2) (cdr cell)
                   (aref a 3) symbol
                   (aref a 4) result
                   (aref chunk position-2) a))))

      ;; Few entries (SIMPLE-VECTOR) => many entries (HASH-TABLE)
      ((typep cell '(simple-vector #.+packrat-cache-array-size+))
       (let* ((a cell)
              (fill-pointer (aref a 0)))
         (declare (type (integer 0 #.+packrat-cache-array-size+)
                        fill-pointer))
         ;; When there is an entry for SYMBOL, update it and return.
         (loop for i upfrom 1 below fill-pointer by 2
               when (eq (aref a i) symbol)
                 do (setf (aref a (1+ i)) result)
                    (return-from cached result))
         ;; No existing entry
         (cond
           ;; With less than +PACKRAT-HASH-TABLE-SWITCH-POINT+
           ;; entries, increase the fill-pointer and add an entry.
           ((< fill-pointer +packrat-cache-array-size+)
            (setf (aref a 0) (+ fill-pointer 2)
                  (aref a fill-pointer) symbol
                  (aref a (1+ fill-pointer)) result))
           ;; With +PACKRAT-HASH-TABLE-SWITCH-POINT+ entries, upgrade
           ;; to a HASH-TABLE, then store the new entry.
           (t
            (let ((table (make-hash-table
                          :test #'eq :size +packrat-initial-hash-table-size+)))
              (loop for i upfrom 1 below fill-pointer by 2
                    do (setf (gethash (aref a i) table)
                             (aref a (1+ i))))
              (setf (aref chunk position-2) table)
              (setf (gethash symbol table) result))))))

      ;; Many entries (HASH-TABLE)
      (t
       (setf (gethash symbol cell) result))))
  result)
