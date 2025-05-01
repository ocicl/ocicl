;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- GRAY stream mixin.
;;;

(in-package :iolib/common-lisp)

(defclass trivial-gray-stream-mixin ()
  ((%open :initform t)))

(defmethod close ((s trivial-gray-stream-mixin) &key abort)
  (declare (ignore abort))
  (prog1 (slot-value s '%open)
    (setf (slot-value s '%open) nil)))

(defmethod open-stream-p ((s trivial-gray-stream-mixin))
  (slot-value s '%open))

(defgeneric stream-read-sequence
    (stream sequence start end &key &allow-other-keys))

(defgeneric stream-write-sequence
    (stream sequence start end &key &allow-other-keys))

(defgeneric stream-file-position (stream))

(defgeneric (setf stream-file-position) (newval stream))

(defmethod stream-write-string
    ((stream trivial-gray-stream-mixin) seq &optional start end)
  (stream-write-sequence stream seq (or start 0) (or end (length seq))))

;; Implementations should provide this default method, I believe, but
;; at least sbcl and allegro don't.
(defmethod stream-terpri ((stream trivial-gray-stream-mixin))
  (write-char #\newline stream))

(defmethod stream-file-position ((stream trivial-gray-stream-mixin))
  nil)

(defmethod (setf stream-file-position)
    (newval (stream trivial-gray-stream-mixin))
  (declare (ignore newval))
  nil)

#+allegro
(progn
  (defmethod excl:stream-read-sequence
      ((s trivial-gray-stream-mixin) seq &optional start end)
    (stream-read-sequence s seq (or start 0) (or end (length seq))))

  (defmethod excl:stream-write-sequence
      ((s trivial-gray-stream-mixin) seq &optional start end)
    (stream-write-sequence s seq (or start 0) (or end (length seq)))))

#+cmu
(progn
  (defmethod ext:stream-read-sequence
      ((s trivial-gray-stream-mixin) seq &optional start end)
    (stream-read-sequence s seq (or start 0) (or end (length seq))))

  (defmethod ext:stream-write-sequence
      ((s trivial-gray-stream-mixin) seq &optional start end)
    (stream-write-sequence s seq (or start 0) (or end (length seq)))))

#+lispworks
(progn
  (defmethod stream:stream-read-sequence
      ((s trivial-gray-stream-mixin) seq start end)
    (stream-read-sequence s seq start end))

  (defmethod stream:stream-write-sequence
      ((s trivial-gray-stream-mixin) seq start end)
    (stream-write-sequence s seq start end))

  (defmethod stream:stream-file-position ((stream trivial-gray-stream-mixin))
    (stream-file-position stream))

  (defmethod (setf stream:stream-file-position)
      (newval (stream trivial-gray-stream-mixin))
    (setf (stream-file-position stream) newval)))

#+openmcl
(progn
  (defmethod ccl:stream-read-vector
      ((s trivial-gray-stream-mixin) seq start end)
    (stream-read-sequence s seq start end))

  (defmethod ccl:stream-write-vector
      ((s trivial-gray-stream-mixin) seq start end)
    (stream-write-sequence s seq start end)))

#+clisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((pkg (find-package :gray))
         (sym (and pkg (find-symbol (string '#:stream-read-sequence) pkg))))
    (unless (and sym (fboundp sym))
      (error "Your CLISP does not have ~A and is therefore unsupported"
             "gray:stream-read-sequence"))))

#+clisp
(progn
  (defmethod gray:stream-read-sequence
      ((s trivial-gray-stream-mixin) seq &key start end)
    (stream-read-sequence s seq (or start 0) (or end (length seq))))

  (defmethod gray:stream-write-sequence
      ((s trivial-gray-stream-mixin) seq &key start end)
    (stream-write-sequence s seq (or start 0) (or end (length seq))))

  (defmethod gray:stream-position ((stream trivial-gray-stream-mixin) position)
    (if position
        (setf (stream-file-position stream) position)
        (stream-file-position stream))))

#+sbcl
(progn
  (defmethod sb-gray:stream-read-sequence
      ((s trivial-gray-stream-mixin) seq &optional start end)
    (stream-read-sequence s seq (or start 0) (or end (length seq))))

  (defmethod sb-gray:stream-write-sequence
      ((s trivial-gray-stream-mixin) seq &optional start end)
    (stream-write-sequence s seq (or start 0) (or end (length seq))))

  ;; SBCL extension:
  (defmethod sb-gray:stream-line-length ((stream trivial-gray-stream-mixin))
    80))

#+ecl
(progn
  (defmethod gray:stream-read-sequence
      ((s trivial-gray-stream-mixin) seq &optional start end)
    (stream-read-sequence s seq (or start 0) (or end (length seq))))

  (defmethod gray:stream-write-sequence
      ((s trivial-gray-stream-mixin) seq &optional start end)
    (stream-write-sequence s seq (or start 0) (or end (length seq)))))
