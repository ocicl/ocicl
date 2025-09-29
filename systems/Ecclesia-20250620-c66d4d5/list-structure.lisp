(cl:in-package #:ecclesia)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tools for checking for list structure.

;;; For any object, return its structure as a list as two values: the
;;; first value contains the number of unique CONS cells in the list,
;;; and the second value is one of the keywords :proper, :dotted, and
;;; :circular.  For an atom, 0 and :dotted is returned.  
;;;
;;; This function is useful for processing code because lists
;;; representing code are not often very long, so the method used is
;;; fast and appropriate, and because we often need to check that such
;;; lists are proper, but the simple method would go into an infinite
;;; computation if the list is circular, whereas we would like to give
;;; an error message in that case.
(defun list-structure (object)
  ;; First we attempt to just traverse the list as usual,
  ;; assuming that it is fairly short.  If we reach the end,
  ;; then that's great, and we return the result.
  (loop for remaining = object then (cdr remaining)
	for count from 0 to 100
	while (consp remaining)
	finally (when (atom remaining)
		  (return-from list-structure
		    (values count
			    (if (null remaining)
				:proper
				:dotted)))))
  ;; Come here if the list has more than a few CONS cells.  We
  ;; traverse it again, this time entering each CONS cell in a hash
  ;; table.  Stop when we reach the end of the list, or when we see
  ;; the same CONS cell twice. 
  (let ((table (make-hash-table :test #'eq)))
    (loop for remaining = object then (cdr remaining)
	  while (consp remaining)
	  until (gethash remaining table)
	  do (setf (gethash remaining table) t)
	  finally (return (values (hash-table-count table)
				  (if (null remaining)
				      :proper
				      (if (atom remaining)
					  :dotted
					  :circular)))))))

;;; Check that an object is a proper list.  Return true if the object
;;; is a proper list.  Return false if the object is an atom other
;;; than NIL or if the list is dotted or circular.
;;;
;;; If LIST-LENGTH is given a proper list, then it returns the length
;;; of that list, which is a number.  If LIST-LENGTH is given a
;;; circular list, it returns NIL which is not a number.  If
;;; LIST-LENGTH is given anything other than a proper list or a
;;; circular list, it signals an error, so then IGNORE-ERRORS returns
;;; NIL as its first value, which again is not a number.
(defun proper-list-p (object)
  (numberp (ignore-errors (list-length object))))
	     
;;; Check that an object is a proper list, and if so, return the
;;; number of cons cells in the list.  Return false if the object is
;;; an atom other than NIL or if the list is dotted or circular.  If
;;; the object is NIL, 0 is returned.
(defun proper-list-length (object)
  (values (ignore-errors (list-length object))))

;;; Return true if and only if OBJECT is a circular list.
;;;
;;; This definition relies on the fact that LIST-LENGTH returns a
;;; single value when it does not signal an error.  So in that case,
;;; IGNORE-ERRORS returns a single value, and the value of the
;;; variable SECOND is thus NIL.  And if OBJECT is a circular list,
;;; then the only value returned by LIST-LENGTH is NIL, so that is
;;; also the value of FIRST.  If LIST-LENGTH does not signal an error
;;; and does not return NIL, then OBJECT is a proper list, so
;;; CIRCULAR-LIST-P returns NIL.  If LIST-LENGTH is given anything
;;; other than a circular list or a proper list, it signals an error,
;;; so then IGNORE-ERRORS returns two values, NIL and a condition.  In
;;; this case, SECOND is not NIL, so NIL is returned from
;;; CIRCULAR-LIST-P.
(defun circular-list-p (object)
  (and (consp object)
       (multiple-value-bind (first second)
           (ignore-errors (list-length object))
         (and (null first) (null second)))))
	     
;;; This function returns true if the object is an atom other than NIL
;;; (the degenerate case of a dotted list) or if the list is
;;; terminated by some atom other than NIL.  It returns false if the
;;; object is NIL, if the object is a list terminated by NIL, or of
;;; the object is a circular list.
(defun dotted-list-p (object)
  (and (not (proper-list-p object))
       (not (circular-list-p object))))
	     
;;; Check that an object is a dotted list, and if so, return the
;;; number of cons cells in the list.  Return false if the object is
;;; NIL, if the object is a list terminated by NIL, or of the object
;;; is a circular list.  Return 0 if the object is an atom other than
;;; NIL (the degenerate case of a dotted list).
(defun dotted-list-length (object)
  (cond  ((null object) nil)
	 ((atom object) 0)
	 (t (let ((slow object)
		  (fast (cdr object))
		  (count 1))
	      (declare (type cons slow))
	      ;; We assume that the implementation is such that a
	      ;; fixnum is able to hold the maximum number of CONS
	      ;; cells possible in the heap.
	      (declare (type fixnum count))
	      (tagbody
	       again
		 (unless (consp fast)
		   (return-from dotted-list-length
		     (if (null fast) nil count)))
		 (when (eq fast slow)
		   (return-from dotted-list-length nil))
		 (setq fast (cdr fast))
		 (unless (consp fast)
		   (return-from dotted-list-length
		     (if (null fast) nil (1+ count))))
		 (setq fast (cdr fast))
		 (setq slow (cdr slow))
		 (incf count 2)
		 (go again))))))
	     
(defun proper-or-dotted-list-length (object)
  (cond  ((atom object) 0)
	 (t (let ((slow object)
		  (fast (cdr object))
		  (count 1))
	      (declare (type cons slow))
	      ;; We assume that the implementation is such that a
	      ;; fixnum is able to hold the maximum number of CONS
	      ;; cells possible in the heap.
	      (declare (type fixnum count))
	      (tagbody
	       again
		 (unless (consp fast)
		   (return-from proper-or-dotted-list-length count))
		 (when (eq fast slow)
		   (return-from proper-or-dotted-list-length nil))
		 (setq fast (cdr fast))
		 (unless (consp fast)
		   (return-from proper-or-dotted-list-length (1+ count)))
		 (setq fast (cdr fast))
		 (setq slow (cdr slow))
		 (incf count 2)
		 (go again))))))
