;;;; pax archives
;;;;
;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar)

(defclass pax-archive (ustar-archive)
  ((default-attributes
    :initform nil
    :reader archive-default-attributes))
  (:documentation
   "An archive as specified by POSIX. Uses multiple physical entries to
represent a single logical entry when values do not fit into the standard
USTAR-ARCHIVE header."))

(defmethod archive-supports-property-p ((archive pax-archive) property)
  (or (not (null (member property '(atime ctime))))
      (call-next-method)))


;; Reading

(defmethod convert-from-physical-entry ((archive pax-archive)
                                        (physical-entry tar-file:pax-global-attributes-entry)
                                        &rest overrides)
  (apply #'convert-from-physical-entry archive (tar-file:read-entry (archive-file archive))
         overrides))

(defmethod convert-from-physical-entry ((archive pax-archive)
                                        (physical-entry tar-file:pax-extended-attributes-entry)
                                        &rest overrides)
  (let ((path (tar-file:attribute physical-entry "path"))
        (linkpath (tar-file:attribute physical-entry "linkpath"))
        (atime (tar-file:attribute physical-entry "atime"))
        (ctime (tar-file:attribute physical-entry "ctime"))
        (mtime (tar-file:attribute physical-entry "mtime"))
        (uname (tar-file:attribute physical-entry "uname"))
        (gname (tar-file:attribute physical-entry "gname"))
        (uid (tar-file:attribute physical-entry "uid"))
        (gid (tar-file:attribute physical-entry "gid"))
        (size (tar-file:attribute physical-entry "size")))
    (flet ((add-override (key value)
             (push value overrides)
             (push key overrides)))
      (unless (null path)
        (add-override :name path))
      (unless (null linkpath)
        (add-override :linkname linkpath))
      (unless (null atime)
        (add-override :atime (string-to-timestamp atime)))
      (unless (null ctime)
        (add-override :ctime (string-to-timestamp ctime)))
      (unless (null mtime)
        (add-override :mtime (string-to-timestamp mtime)))
      (unless (null uname)
        (add-override :uname (string-to-timestamp uname)))
      (unless (null gname)
        (add-override :gname (string-to-timestamp gname)))
      (unless (null uid)
        (add-override :uid (parse-integer uid)))
      (unless (null gid)
        (add-override :gid (parse-integer gid)))
      (unless (null size)
        (add-override :size (parse-integer size))))
    (apply #'convert-from-physical-entry archive (tar-file:read-entry (archive-file archive))
           overrides)))


;; Writing

(defmethod archive-supports-sub-seconds-p ((archive pax-archive))
  t)

(defmethod archive-supports-negative-time-p ((archive pax-archive))
  t)

(defmethod check-property-for-writing ((archive pax-archive) (entry entry) (name (eql 'name)))
  (check-required-property entry name 'string))

(defmethod check-property-for-writing ((archive pax-archive) (entry entry) (name (eql 'mode)))
  (check-required-property entry name 'mode-list))

(defmethod check-property-for-writing ((archive pax-archive) (entry entry) (name (eql 'uid)))
  (check-required-property entry name '(integer 0)))

(defmethod check-property-for-writing ((archive pax-archive) (entry entry) (name (eql 'gid)))
  (check-required-property entry name '(integer 0)))

(defmethod check-property-for-writing ((archive pax-archive) (entry entry) (name (eql 'size)))
  (check-required-property entry name '(integer 0)))

(defmethod check-property-for-writing ((archive pax-archive) (entry link-entry) (name (eql 'linkname)))
  (check-required-property entry name 'string))

(defmethod check-property-for-writing ((archive pax-archive) (entry entry) (name (eql 'uname)))
  t)

(defmethod check-property-for-writing ((archive pax-archive) (entry entry) (name (eql 'gname)))
  t)

(defmethod %write-entry ((archive pax-archive) entry &rest overrides)
  (declare (ignore overrides))
  (let ((properties nil)
        (overrides nil))
    (multiple-value-bind (prefix name) (split-name (name entry))
      (declare (ignore prefix))
      (when (> (length name) (string-max-length archive 'name))
        (push (cons "path" (name entry))
              properties)))
    (when (and (member 'size (entry-property-slot-names entry))
               (> (size entry) (integer-max-value archive 'size)))
      (push (cons "size" (format nil "~D" (size entry)))
            properties)
      (push #o77777777777 overrides)
      (push :size overrides))
    (when (or (minusp (local-time:timestamp-to-unix (mtime entry)))
              (not (zerop (local-time:nsec-of (mtime entry)))))
      (push (cons "mtime" (timestamp-to-string (mtime entry)))
            properties))
    (unless (null (atime entry))
      (push (cons "atime" (timestamp-to-string (atime entry)))
            properties))
    (unless (null (ctime entry))
      (push (cons "ctime" (timestamp-to-string (ctime entry)))
            properties))
    (when (and (typep entry 'link-entry)
               (> (length (linkname entry)) (string-max-length archive 'linkname)))
      (push (cons "linkname" (linkname entry))
            properties)
      (push "" overrides)
      (push :linkname overrides))
    (when (> (length (uname entry)) (string-max-length archive 'uname))
      (push (cons "uname" (uname entry))
            properties)
      (push "" overrides)
      (push :uname overrides))
    (when (> (length (gname entry)) (string-max-length archive 'gname))
      (push (cons "gname" (gname entry))
            properties)
      (push "" overrides)
      (push :gname overrides))
    (when (> (uid entry) (integer-max-value archive 'uid))
      (push (cons "uid" (format nil "~D" (uid entry)))
            properties)
      (push 0 overrides)
      (push :uid overrides))
    (when (> (gid entry) (integer-max-value archive 'gid))
      (push (cons "gid" (format nil "~D" (gid entry)))
            properties)
      (push 0 overrides)
      (push :gid overrides))
    (unless (null properties)
      (tar-file:write-pax-extended-attributes-entry (archive-file archive) "././@PaxHeaders"
                                                    :attributes (nreverse properties)))
    (apply #'call-next-method archive entry overrides)))
