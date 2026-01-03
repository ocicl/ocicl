;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
(in-package :crypto)
;;; See the spec at
;;; http://zgp.org/pipermail/p2p-hackers/2002-June/000621.html

(defparameter *leaf-byte* (make-array 1 :element-type '(unsigned-byte 8)
                                      :initial-element 0))
(defparameter *internal-byte* (make-array 1 :element-type '(unsigned-byte 8)
                                          :initial-element 1))

(defun make-tree-hash-leaf-digest (digest-spec)
  (let ((digest (make-digest digest-spec)))
    (update-digest digest *leaf-byte*)
    digest))

(defstruct (tree-hash
             (:constructor %make-tree-hash nil)
             (:constructor %make-tree-hash-state (block-length state block-index branch))
             (:copier nil))
  (block-length 1024 :type (integer 1 #.most-positive-fixnum))
  (state (make-tree-hash-leaf-digest :tiger))
  (block-index 0 :type (integer 0 #.most-positive-fixnum))
  (branch () :type list))

(defun %make-tree-hash-digest (&key (digest :tiger) (block-length 1024))
  (%make-tree-hash-state block-length (make-tree-hash-leaf-digest digest) 0 '()))

(defun make-tiger-tree-hash ()
  (%make-tree-hash-digest))

(defmethod block-length ((x tree-hash))
  (tree-hash-block-length x))

(defmethod digest-length ((x tree-hash))
  (digest-length (tree-hash-state x)))

(defmethod reinitialize-instance ((state tree-hash) &rest initargs)
  (declare (ignore initargs))
  (reinitialize-instance (tree-hash-state state))
  (update-digest (tree-hash-state state) *leaf-byte*)
  (setf (tree-hash-block-index state) 0)
  (setf (tree-hash-branch state) '())
  state)

(defmethod copy-digest ((state tree-hash) &optional copy)
  (check-type copy (or null tree-hash))
  (cond
    (copy
     (copy-digest (tree-hash-state state) (tree-hash-state copy))
     (setf (tree-hash-block-length copy) (tree-hash-block-length state))
     (setf (tree-hash-block-index copy) (tree-hash-block-index state))
     (setf (tree-hash-branch copy) (tree-hash-branch state))
     copy)
    (t
     (%make-tree-hash-state
      (tree-hash-block-length state)
      (copy-digest (tree-hash-state state))
      (tree-hash-block-index state)
      (tree-hash-branch state)))))

(define-digest-updater tree-hash
  "Update the given tree-hash state from sequence,
which is a simple-array with element-type (unsigned-byte 8),
bounded by start and end, which must be numeric bounding-indices."
  (assert (<= start end))
  (when (< start end)
    (loop :with block-length = (tree-hash-block-length state)
      :with digest = (tree-hash-state state)
      :for length fixnum = (- end start)
      :for block-index fixnum = (tree-hash-block-index state) :then 0
      :for block-remaining-length fixnum = (- block-length block-index)
      :for current-length fixnum = (min block-remaining-length length)
      :for new-index fixnum = (+ block-index current-length)
      :for new-start fixnum = (+ start current-length) :do
      (update-digest digest sequence :start start :end new-start)
      (when (= new-index block-length)
        (update-tree-hash-branch state)
        (reinitialize-instance digest)
        (update-digest digest *leaf-byte*)
        (setf new-index 0))
      (setf start new-start)
      (when (= start end)
        (setf (tree-hash-block-index state) new-index)
        (return)))))

(defun update-tree-hash-branch (state)
  (let ((digest (tree-hash-state state)))
    (setf (tree-hash-branch state)
          (merge-tree-hash-branch digest (tree-hash-branch state) (produce-digest digest)))))

(defun merge-tree-hash-branch (digest branch hash)
  (let ((other-hash (car branch)))
    (if (null other-hash)
        (cons hash (cdr branch)) ;; happens to work when branch is nil!
        (cons nil (merge-tree-hash-branch
                   digest
                   (cdr branch)
                   (combine-hash-tree-digests digest other-hash hash))))))

(defun combine-hash-tree-digests (digest hash1 hash2)
  (reinitialize-instance digest)
  (update-digest digest *internal-byte*)
  (update-digest digest hash1)
  (update-digest digest hash2)
  (produce-digest digest))

(defmethod produce-digest ((state tree-hash) &key digest (digest-start 0))
  (let ((state (copy-digest state)))
    (when (or (not (zerop (tree-hash-block-index state)))
              (null (tree-hash-branch state)))
      (update-tree-hash-branch state))
    (let* ((internal-state (tree-hash-state state))
           (result
             (reduce (lambda (hash2 hash1)
                       (cond
                         ((null hash2) hash1)
                         ((null hash1) hash2)
                         (t (combine-hash-tree-digests internal-state hash1 hash2))))
                     (tree-hash-branch state))))
      (if digest
          (if (<= (length result) (- (length digest) digest-start))
              (replace digest result :start1 digest-start)
              (error 'insufficient-buffer-space
                     :buffer digest :start digest-start
                     :length (length result)))
          result))))

(setf (get 'tree-hash '%digest-length) 24)
(setf (get 'tree-hash '%make-digest) (symbol-function '%make-tree-hash-digest))
