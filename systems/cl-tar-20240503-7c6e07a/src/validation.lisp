;;;; tar file validation
;;;;
;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar)

(defgeneric string-max-length (archive name))

(defgeneric integer-max-value (archive name))

(defgeneric archive-supports-sub-seconds-p (archive))

(defgeneric archive-supports-negative-time-p (archive))

(defun check-required-property (entry name type)
  (unless (and (slot-boundp entry name)
               (typep (slot-value entry name) type))
    (restart-case
        (error 'required-property-missing :name name)
      (store-value (value)
        (setf (slot-value entry name) value)
        (check-required-property entry name type)))))

(defun check-string-length (entry name max-length)
  (let ((value (if (symbolp name)
                   (slot-value entry name)
                   name)))
    (unless (or (eql max-length '*)
                (<= (length value) max-length))
      (restart-case
          (error 'property-value-too-long :value value :name name)
        (truncate-value ()
          nil)))))

(defun check-integer-max-value (entry name max-value)
  (let ((value (slot-value entry name)))
    (unless (<= value max-value)
      (restart-case
          (error 'unsupported-property-value :value value :name name)))))

(defun check-timestamp (archive entry name)
  (let ((value (slot-value entry name)))
    (unless (or (zerop (local-time:nsec-of value))
                (archive-supports-sub-seconds-p archive))
      (restart-case
          (error 'unsupported-property-value :value value :name name)
        (truncate-value ()
          nil)))
    (unless (or (not (minusp (local-time:timestamp-to-unix value)))
                (archive-supports-negative-time-p archive))
      (restart-case
          (error 'unsupported-property-value :value value :name name)
        (truncate-value ()
          nil)))))

(defgeneric check-property-for-writing (archive entry name))

(defmethod check-property-for-writing ((archive archive) (entry entry) (name (eql 'name)))
  (check-required-property entry name 'string)
  (check-string-length entry name (string-max-length archive name)))

(defmethod check-property-for-writing ((archive archive) (entry entry) (name (eql 'mode)))
  (check-required-property entry name 'mode-list))

(defmethod check-property-for-writing ((archive archive) (entry entry) (name (eql 'uid)))
  (check-required-property entry name '(integer 0))
  (let ((max-value (integer-max-value archive name)))
    (unless (eql max-value '*)
      (check-integer-max-value entry name max-value))))

(defmethod check-property-for-writing ((archive archive) (entry entry) (name (eql 'gid)))
  (check-required-property entry name '(integer 0))
  (let ((max-value (integer-max-value archive name)))
    (unless (eql max-value '*)
      (check-integer-max-value entry name max-value))))

(defmethod check-property-for-writing ((archive archive) (entry entry) (name (eql 'mtime)))
  (check-required-property entry name 'local-time:timestamp)
  (check-timestamp archive entry name))

(defmethod check-property-for-writing ((archive archive) (entry entry) (name (eql 'size)))
  (check-required-property entry name '(integer 0))
  (let ((max-value (integer-max-value archive name)))
    (unless (eql max-value '*)
      (check-integer-max-value entry name max-value))))

(defmethod check-property-for-writing ((archive archive) (entry device-entry) (name (eql 'devmajor)))
  (check-required-property entry name '(integer 0))
  (let ((max-value (integer-max-value archive name)))
    (unless (eql max-value '*)
      (check-integer-max-value entry name max-value))))

(defmethod check-property-for-writing ((archive archive) (entry device-entry) (name (eql 'devminor)))
  (check-required-property entry name '(integer 0))
  (let ((max-value (integer-max-value archive name)))
    (unless (eql max-value '*)
      (check-integer-max-value entry name max-value))))

(defmethod check-property-for-writing ((archive archive) (entry link-entry) (name (eql 'linkname)))
  (check-required-property entry name 'string)
  (check-string-length entry name (string-max-length archive name)))

(defmethod check-property-for-writing ((archive archive) (entry entry) (name (eql 'uname)))
  (or (not (slot-boundp entry name))
      (check-string-length entry name (string-max-length archive name))))

(defmethod check-property-for-writing ((archive archive) (entry entry) (name (eql 'gname)))
  (or (not (slot-boundp entry name))
      (check-string-length entry name (string-max-length archive name))))

(defmethod check-property-for-writing ((archive archive) (entry entry) (name (eql 'atime)))
  (or (not (slot-boundp entry name))
      (check-timestamp archive entry name)))

(defmethod check-property-for-writing ((archive archive) (entry entry) (name (eql 'ctime)))
  (or (not (slot-boundp entry name))
      (check-timestamp archive entry name)))

(defgeneric check-properties (archive entry)
  (:documentation
   "Check that all properties defined in ENTRY are supported by ARCHIVE."))

(defmethod check-properties ((archive archive) (entry entry))
  (dolist (slot (entry-property-slot-names entry))
    (tagbody
       (when (and (slot-boundp entry slot)
                  (not (null (slot-value entry slot)))
                  (not (archive-supports-property-p archive slot)))
         (restart-case
             (error 'unsupported-property :name slot)
           (ignore-unsupported-property ()
             (go :end))))
       (unless (null (slot-value entry slot))
         (check-property-for-writing archive entry slot))
     :end)))
