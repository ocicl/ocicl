;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- New pathnames.
;;;

(in-package :iolib/pathnames)

;;;-------------------------------------------------------------------------
;;; Classes and Types
;;;-------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +file-path-host-type+
    #+unix    'unix-path
    #+windows 'unc-path))

(defclass file-path ()
  ((host :initarg :host)
   (device :initarg :device)
   (components :initarg :components
               :initform nil)))

(deftype file-path-designator ()
  `(or ,+file-path-host-type+ string))

(define-condition invalid-file-path (isys:iolib-error type-error)
  ((path :initarg :path :reader invalid-file-path-path)
   (reason :initform nil :initarg :reason :reader invalid-file-path-reason))
  (:report (lambda (condition stream)
             (format stream "Invalid file path: ~S."
                     (invalid-file-path-path condition))
             (when-let (reason (invalid-file-path-reason condition))
               (format stream "~%~A." reason)))))


;;;-------------------------------------------------------------------------
;;; Constants
;;;-------------------------------------------------------------------------

(defconstant +directory-delimiter+
  #+unix    #\/
  #+windows #\\)

(defconstant +alternative-delimiter+
  #+unix    nil
  #+windows #\/)

(defconstant (+directory-delimiters+ :test #'equal)
  (list* +directory-delimiter+ +alternative-delimiter+))

(defconstant +execution-path-delimiter+
  #+unix    #\:
  #+windows #\;)

(declaim (special *default-file-path-defaults*))


;;;-------------------------------------------------------------------------
;;; Generic Functions
;;;-------------------------------------------------------------------------

(defgeneric file-path (pathspec))

(defgeneric file-path-namestring (path))


;;;-------------------------------------------------------------------------
;;; Accessors
;;;-------------------------------------------------------------------------

(defun file-path-host (pathspec &key namestring)
  (let ((path (file-path pathspec)))
    (if namestring
        (%file-path-host-namestring path)
        (slot-value path 'host))))

(defun file-path-device (pathspec &key namestring)
  (let ((path (file-path pathspec)))
    (if namestring
        (%file-path-device-namestring path)
        (slot-value path 'device))))

(defun file-path-components (pathspec &key namestring)
  (let ((path (file-path pathspec)))
    (if namestring
       (%file-path-components-namestring path)
       (slot-value path 'components))))

(defun split-root/nodes (dir)
  (if (eql :root (car dir))
      (values :root (cdr dir))
      (values nil   dir)))

(defun %file-path-directory (path)
  (let ((components (slot-value path 'components)))
    (multiple-value-bind (root nodes)
        (split-root/nodes components)
      (if root
          (cons root (butlast nodes))
          (or (butlast nodes)
              (list "."))))))

(defun file-path-directory (pathspec &key namestring)
  (let ((path (file-path pathspec)))
    (if namestring
        (%file-path-directory-namestring path)
        (%file-path-directory path))))

(defun file-path-file (pathspec &key namestring)
  (declare (ignore namestring))
  (let* ((path (file-path pathspec))
         (components (slot-value path 'components)))
    (or (lastcar (nth-value 1 (split-root/nodes components)))
        ".")))

(defun split-name/type (path)
  (let ((dotpos (position #\. path :start 1 :from-end t)))
    (cond
      ((or (null dotpos) (member path '("." "..") :test #'string=))
       (values path nil))
      (t (values (subseq path 0 dotpos)
                 (full-string (subseq path (1+ dotpos))))))))

(defun file-path-file-name (pathspec)
  (let ((file (file-path-file pathspec)))
    (nth-value 0 (split-name/type file))))

(defun file-path-file-type (pathspec)
  (let ((file (file-path-file pathspec)))
    (nth-value 1 (split-name/type file))))


;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

(defun valid-component-types-p (components)
  (multiple-value-bind (root nodes)
      (split-root/nodes components)
    (and (member root '(nil :root))
         (every #'stringp nodes))))

(defmethod initialize-instance :after ((path file-path) &key components)
  (check-type components (and (not null) (satisfies valid-component-types-p)))
  (setf (slot-value path 'components) components)
  (dolist (node (cdr (slot-value path 'components)))
    (when (zerop (length node))
      (error 'invalid-file-path :path ""
             :reason "Null filenames are not valid"))
    (when (find-if (lambda (c) (member c +directory-delimiters+)) node)
      (error 'invalid-file-path :path node
             :reason (format nil
                             "Path components cannot contain delimiters(~A)"
                             (join* " and "
                                    (mapcar 'prin1-to-string
                                            +directory-delimiters+)))))))


;;;-------------------------------------------------------------------------
;;; Predicates
;;;-------------------------------------------------------------------------

(defun file-path-p (thing)
  (typep thing 'file-path))

(defun absolute-p (dir)
  (eql :root (car dir)))

(defun absolute-file-path-p (path)
  (check-type path file-path)
  (absolute-p (slot-value path 'components)))

(defun relative-file-path-p (path)
  (check-type path file-path)
  (not (absolute-p (slot-value path 'components))))


;;;-------------------------------------------------------------------------
;;; Operations
;;;-------------------------------------------------------------------------

(defmethod file-path ((path file-path))
  path)

(defmethod file-path (pathspec)
  (parse-file-path pathspec))

(defmethod file-path ((pathspec pathname))
  (parse-file-path (namestring pathspec)))


;;;-------------------------------------------------------------------------
;;; PRINT-OBJECT
;;;-------------------------------------------------------------------------

(defmethod print-object ((path file-path) stream)
  (let ((ns (file-path-namestring path)))
    (if (or *print-readably* *print-escape*)
        (format stream "#/~S/~S" 'p ns)
        (write-string ns stream))))

(define-literal-reader p (stream)
  (let ((token (read stream)))
    (check-type token string)
    (file-path token)))
