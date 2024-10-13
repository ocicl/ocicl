;;;; tar file entries
;;;;
;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar)

(defun permissions-list-p (list)
  (and (listp list)
       (alexandria:proper-list-p list)
       (null (set-difference list +mode-permissions+))))

(deftype mode-list ()
  "A list consisting of a subset of:
(:SET-USER-ID :SET-GROUP-ID :STICKY :USER-READ :USER-WRITE :USER-EXEC
:GROUP-READ :GROUP-WRITE :GROUP-EXEC :OTHER-READ :OTHER-WRITE :OTHER-EXEC)"
  `(and list (satisfies permissions-list-p)))

(defmacro define-entry-property (name type)
  `(progn
     (defgeneric ,name (entry)
       (:documentation
        ,(format nil "The ~A of ENTRY. Returns a ~A."
                 (string-downcase (string name))
                 type)))
     (defgeneric (setf ,name) (value entry)
       (:documentation
        ,(format nil "Set the ~A of ENTRY. VALUE must be a ~A."
                 (string-downcase (string name))
                 type)))))

(defmacro define-optional-entry-property (name type)
  `(progn
     (defgeneric ,name (entry)
       (:documentation
        ,(format nil "The ~A of ENTRY. Returns a ~A."
                 (string-downcase (string name))
                 type))
       (:method :around (entry)
         (if (or (not (slot-boundp entry 'archive))
                 (archive-supports-property-p (archive entry) ',name))
             (call-next-method)
             (restart-case
                 (error 'unsupported-property :name ',name)
               (use-value (value)
                 :interactive (lambda ()
                                (format *query-io* "Enter a new value (unevaluated): ")
                                (read *query-io*))
                 value)
               (ignore-unsupported-property ()
                 nil)))))
     (defgeneric (setf ,name) (value entry)
       (:documentation
        ,(format nil "Set the ~A of ENTRY. VALUE must be a ~A."
                 (string-downcase (string name))
                 type))
       (:method :around (value entry)
         (if (or (not (slot-boundp entry 'archive))
                 (archive-supports-property-p (archive entry) ',name))
             (call-next-method)
             (restart-case
                 (error 'unsupported-property :name ',name)
               (ignore-unsupported-property ()
                 value)))))))

(define-entry-property name string)
(define-entry-property mode mode-list)
(define-entry-property uid (integer 0))
(define-entry-property gid (integer 0))
(define-entry-property mtime local-time:timestamp)
(define-entry-property size (integer 0))
(define-entry-property devmajor (integer 0))
(define-entry-property devminor (integer 0))
(define-entry-property linkname string)

(define-optional-entry-property uname string)
(define-optional-entry-property gname string)
(define-optional-entry-property atime local-time:timestamp)
(define-optional-entry-property ctime local-time:timestamp)

(defgeneric entry-property-slot-names (entry)
  (:method-combination append))

(defclass entry ()
  ((archive
    :initarg :%archive
    :accessor archive)
   (name
    :initarg :name
    :type string
    :accessor name)
   (mode
    :initarg :mode
    :type list
    :accessor mode)
   (uid
    :initarg :uid
    :type (integer 0)
    :accessor uid)
   (gid
    :initarg :gid
    :type (integer 0)
    :accessor gid)
   (uname
    :initarg :uname
    :initform nil
    :type (or string null)
    :accessor uname)
   (gname
    :initarg :gname
    :initform nil
    :type (or string null)
    :accessor gname)
   (mtime
    :initarg :mtime
    :type local-time:timestamp
    :accessor mtime)
   (atime
    :initarg :atime
    :initform nil
    :type (or local-time:timestamp null)
    :accessor atime)
   (ctime
    :initarg :ctime
    :initform nil
    :type (or local-time:timestamp null)
    :accessor ctime))
  (:documentation
   "The base class of all entry types. Each ENTRY must contain a NAME, MODE,
UID, GID, and MTIME. Other common properties are UNAME, GNAME, ATIME, and
CTIME."))

(defmethod entry-property-slot-names append ((entry entry))
  (list 'name 'mode 'uid 'gid 'uname 'gname 'mtime 'atime 'ctime))

(defclass has-data-mixin ()
  ((physical-entry
    :initarg :%physical-entry
    :reader physical-entry)
   (data
    :initarg :data
    :reader data)))

(defclass file-entry (entry has-data-mixin)
  ((size
    :initarg :size
    :type integer
    :accessor size))
  (:documentation
   "An entry representing a regular file."))

(defmethod entry-property-slot-names append ((entry file-entry))
  (list 'size))

(defclass directory-entry (entry)
  ((size
    :initarg :size
    :initform 0
    :type integer
    :accessor size))
  (:documentation
   "An entry representing a directory."))

(defmethod entry-property-slot-names append ((entry directory-entry))
  (list 'size))

(defclass link-entry (entry)
  ((linkname
    :initarg :linkname
    :type string
    :accessor linkname)))

(defmethod entry-property-slot-names append ((entry link-entry))
  (list 'linkname))

(defclass symbolic-link-entry (link-entry)
  ()
  (:documentation
   "An entry representing a symbolic link."))

(defclass hard-link-entry (link-entry)
  ()
  (:documentation
   "An entry representing a hard link."))

(defclass fifo-entry (entry)
  ()
  (:documentation
   "An entry representing a FIFO."))

(defclass device-entry (entry)
  ((devmajor
    :initarg :devmajor
    :type integer
    :accessor devmajor)
   (devminor
    :initarg :devminor
    :type integer
    :accessor devminor)))

(defmethod entry-property-slot-names append ((entry device-entry))
  (list 'devmajor 'devminor))

(defclass block-device-entry (device-entry)
  ()
  (:documentation
   "An entry representing a block device."))

(defclass character-device-entry (device-entry)
  ()
  (:documentation
   "An entry representing a character device."))

(defclass unknown-entry (entry has-data-mixin)
  ((size
    :initarg :size
    :type integer
    :accessor size))
  (:documentation
   "An entry representing an unknown entry."))

(defmethod entry-property-slot-names append ((entry unknown-entry))
  (list 'size))

(defgeneric make-entry-stream (entry)
  (:documentation
   "Return a binary stream with the contents of ENTRY. Depending on the the
type of stream underlying the archive (if it is seekable or not) and the
BLOCKING-FACTOR given to OPEN-ARCHIVE, this may be callable either once or
multiple times per entry, may or may not be callable after the next call to
READ-ENTRY, and the the returned stream may or may not be readable after the
next call to READ-ENTRY.

For maximum compatibility, you should call this only once per ENTRY and
completely read from the stream before calling READ-ENTRY again."))

(defmethod make-entry-stream ((entry has-data-mixin))
  (tar-file:make-entry-stream (physical-entry entry)))
