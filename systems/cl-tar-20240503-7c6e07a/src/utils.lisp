;;;; utils
;;;;
;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar)

(alexandria:define-constant +mode-permissions+
    '(:set-user-id :set-group-id :sticky
      :user-read :user-write :user-exec
      :group-read :group-write :group-exec
      :other-read :other-write :other-exec)
  :test 'equal)

(defun mode-to-permissions (mode)
  (loop
    :for permission :in +mode-permissions+
    :for index :downfrom (1- (length +mode-permissions+))
    :when (logbitp index mode)
      :collect permission))

(defun permissions-to-mode (permissions)
  (loop
    :with out := 0
    :for permission :in +mode-permissions+
    :for index :downfrom (1- (length +mode-permissions+))
    :when (member permission permissions)
      :do (setf out (dpb 1 (byte 1 index) out))
    :finally (return out)))

(defun string-to-timestamp (string)
  (destructuring-bind (whole &optional fractional)
      (split-sequence:split-sequence #\. string)
    (let ((nsecs 0))
      (unless (null fractional)
        (let ((direct-fractional (parse-integer fractional))
              (num-digits (length fractional)))
          (setf nsecs (round (* direct-fractional (expt 10 (- 9 num-digits)))))))
      (local-time:unix-to-timestamp (parse-integer whole)
                                    :nsec nsecs))))

(defun timestamp-to-string (timestamp)
  (let ((raw-string (format nil "~D.~9,'0D" (local-time:timestamp-to-unix timestamp)
                            (local-time:nsec-of timestamp))))
    (loop :while (eql (aref raw-string (1- (length raw-string))) #\0)
          :do (setf raw-string (subseq raw-string 0 (1- (length raw-string)))))
    (when (eql (aref raw-string (1- (length raw-string))) #\.)
      (setf raw-string (subseq raw-string 0 (1- (length raw-string)))))
    raw-string))

(defun tar-file-entry-with-prefix-name (entry)
  (let* ((prefix (tar-file:prefix entry))
         (name (tar-file:name entry)))
    (if (equal "" prefix)
        name
        (concatenate 'string prefix "/" name))))

(defun maybe-truncate (string length)
  (if (<= (length string) length)
      string
      (subseq string 0 length)))
