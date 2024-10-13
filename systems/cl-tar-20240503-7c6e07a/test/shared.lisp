(in-package #:tar-test)

(defparameter *default-mtime* (local-time:unix-to-timestamp 1628903146))
(defparameter *default-pax-mtime* (local-time:unix-to-timestamp 1628903146
                                                                :nsec 500000000))
(defparameter *default-pax-atime* (local-time:unix-to-timestamp 1628903146
                                                                :nsec 750000000))
(defparameter *default-gnu-mtime* (local-time:unix-to-timestamp 1628903146))
(defparameter *default-gnu-atime* (local-time:unix-to-timestamp 1628903146))
(defparameter *keep-written-tar-files* nil)

(defclass file-comparison-result (para:result)
  ((expected-file
    :initarg :expected-file
    :reader expected-file)
   (actual-file
    :initarg :actual-file
    :reader actual-file)))

(defmethod para:eval-in-context (context (result file-comparison-result))
  (flet ((fail ()
             (setf (para:status result) :failed)
             (return-from para:eval-in-context nil)))
    (with-open-file (expected-stream (expected-file result) :element-type '(unsigned-byte 8))
      (with-open-file (actual-stream (actual-file result) :element-type '(unsigned-byte 8))
        (unless (= (file-length expected-stream)
                   (file-length actual-stream))
          (fail))))
    (with-open-file (expected-stream (expected-file result) :element-type '(unsigned-byte 8))
      (with-open-file (actual-stream (actual-file result) :element-type '(unsigned-byte 8))
        (let ((expected-buffer (make-array 8096 :element-type '(unsigned-byte 8) :initial-element 0))
              (actual-buffer (make-array 8096 :element-type '(unsigned-byte 8) :initial-element 0)))
          (loop
            (let ((expected-bytes-read (read-sequence expected-buffer expected-stream)))
              (read-sequence actual-buffer actual-stream)
              (unless (equalp expected-buffer actual-buffer)
                (fail))
              (unless (null expected-bytes-read)
                (return))))
          (setf (para:status result) :passed))))))

(defmacro files-equal (expected actual &optional description &rest format-args)
  `(para:eval-in-context para:*context*
                         (make-instance 'file-comparison-result
                                        :expression '(files-equal ,expected ,actual)
                                        :expected-file ,expected
                                        :actual-file ,actual
                                        ,@(when description
                                            `(:description (format NIL ,description ,@format-args))))))

(defun contents-as-string (entry)
  (let* ((stream (tar:make-entry-stream entry))
         (byte-contents (alexandria:read-stream-content-into-byte-vector stream)))
    (babel:octets-to-string byte-contents :encoding :utf-8)))


;; Creating entries.

(defun write-a.txt (archive)
  (tar:with-ignored-unsupported-properties ()
    (tar:with-truncated-unsupported-values ()
      (tar:write-entry archive
                       (make-instance 'tar:file-entry
                                      :size 14
                                      :atime *default-pax-atime*
                                      :mtime *default-pax-mtime*
                                      :gname "root"
                                      :uname "root"
                                      :gid 0
                                      :uid 0
                                      :mode (list :user-read :user-write :group-read :other-read)
                                      :name "a.txt"
                                      :data "Hello, world!
")))))

(defun write-a-symlink.txt (archive)
  (tar:with-ignored-unsupported-properties ()
    (tar:with-truncated-unsupported-values ()
      (tar:write-entry archive
                       (make-instance 'tar:symbolic-link-entry
                                      :atime *default-pax-atime*
                                      :mtime *default-pax-mtime*
                                      :gname "root"
                                      :uname "root"
                                      :gid 0
                                      :uid 0
                                      :mode (list :user-read :user-write :user-exec
                                                  :group-read :group-write :group-exec
                                                  :other-read :other-write :other-exec)
                                      :name "a-symlink.txt"
                                      :linkname "a.txt")))))

(defun write-a-hardlink.txt (archive)
  (tar:with-ignored-unsupported-properties ()
    (tar:with-truncated-unsupported-values ()
      (tar:write-entry archive
                       (make-instance 'tar:hard-link-entry
                                      :atime *default-pax-atime*
                                      :mtime *default-pax-mtime*
                                      :gname "root"
                                      :uname "root"
                                      :gid 0
                                      :uid 0
                                      :mode (list :user-read :user-write :group-read :other-read)
                                      :name "a-hardlink.txt"
                                      :linkname "a.txt")))))

(defun write-fifo (archive)
  (tar:with-ignored-unsupported-properties ()
    (tar:with-truncated-unsupported-values ()
      (tar:write-entry archive
                       (make-instance 'tar:fifo-entry
                                      :atime *default-pax-atime*
                                      :mtime *default-pax-mtime*
                                      :gname "root"
                                      :uname "root"
                                      :uid 0
                                      :gid 0
                                      :mode (list :user-read :user-write :group-read :other-read)
                                      :name "fifo")))))

(defun write-sda1 (archive)
  (tar:with-ignored-unsupported-properties ()
    (tar:with-truncated-unsupported-values ()
      (tar:write-entry archive
                       (make-instance 'tar:block-device-entry
                                      :name "sda1"
                                      :atime *default-pax-atime*
                                      :mtime *default-pax-mtime*
                                      :uname "root"
                                      :gname "root"
                                      :uid 0
                                      :gid 0
                                      :mode (list :user-read :user-write :group-read :other-read)
                                      :devmajor 8
                                      :devminor 1)))))

(defun write-tty0 (archive)
  (tar:with-ignored-unsupported-properties ()
    (tar:with-truncated-unsupported-values ()
      (tar:write-entry archive
                       (make-instance 'tar:character-device-entry
                                      :name "tty0"
                                      :atime *default-pax-atime*
                                      :mtime *default-pax-mtime*
                                      :uname "root"
                                      :gname "root"
                                      :uid 0
                                      :gid 0
                                      :mode (list :user-read :user-write :group-read :other-read)
                                      :devmajor 4
                                      :devminor 0)))))

(defun read-a.txt (type entry)
  (para:is equal "a.txt" (tar:name entry))
  (para:is = 14 (tar:size entry))
  (unless (eql type :v7)
    (para:is equal "root" (tar:uname entry))
    (para:is equal "root" (tar:gname entry)))
  (para:is alexandria:set-equal
           '(:user-read :user-write :group-read :other-read)
           (tar:mode entry))
  (para:true (typep entry 'tar:file-entry))

  (case type
    (:pax
     (para:is local-time:timestamp= *default-pax-mtime* (tar:mtime entry))
     (para:is local-time:timestamp= *default-pax-atime* (tar:atime entry)))
    (:gnu
     (para:is local-time:timestamp= *default-gnu-mtime* (tar:mtime entry)))
    (t
     (para:is local-time:timestamp= *default-mtime* (tar:mtime entry))))

  (para:is = 0 (tar:uid entry))
  (para:is = 0 (tar:gid entry))
  (para:is equal "Hello, world!
" (contents-as-string entry)))

(defun read-a-symlink.txt (type entry)
  (para:is equal "a-symlink.txt" (tar:name entry))
  (unless (eql type :v7)
    (para:is equal "root" (tar:uname entry))
    (para:is equal "root" (tar:gname entry)))
  (para:is alexandria:set-equal '(:user-read :user-write :user-exec
                                  :group-read :group-write :group-exec
                                  :other-read :other-write :other-exec)
           (tar:mode entry))
  (para:true (typep entry 'tar:symbolic-link-entry))
  (case type
    (:pax
     (para:is local-time:timestamp= *default-pax-mtime* (tar:mtime entry))
     (para:is local-time:timestamp= *default-pax-atime* (tar:atime entry)))
    (:gnu
     (para:is local-time:timestamp= *default-gnu-mtime* (tar:mtime entry)))
    (t
     (para:is local-time:timestamp= *default-mtime* (tar:mtime entry))))
  (para:is = 0 (tar:uid entry))
  (para:is = 0 (tar:gid entry))
  (para:is equal "a.txt" (tar:linkname entry)))

(defun read-a-hardlink.txt (type entry)
  (para:is equal "a-hardlink.txt" (tar:name entry))
  (unless (eql type :v7)
    (para:is equal "root" (tar:uname entry))
    (para:is equal "root" (tar:gname entry)))
  (para:is alexandria:set-equal
           '(:user-read :user-write :group-read :other-read)
           (tar:mode entry))
  (para:true (typep entry 'tar:hard-link-entry))
  (case type
    (:pax
     (para:is local-time:timestamp= *default-pax-mtime* (tar:mtime entry)))
    (:gnu
     (para:is local-time:timestamp= *default-gnu-mtime* (tar:mtime entry)))
    (t
     (para:is local-time:timestamp= *default-mtime* (tar:mtime entry))))
  (para:is = 0 (tar:uid entry))
  (para:is = 0 (tar:gid entry))
  (para:is equal "a.txt" (tar:linkname entry)))

(defun read-fifo (type entry)
  (para:is equal "fifo" (tar:name entry))
  (para:is equal "root" (tar:uname entry))
  (para:is equal "root" (tar:gname entry))
  (para:is alexandria:set-equal
           '(:user-read :user-write :group-read :other-read)
           (tar:mode entry))
  (para:true (typep entry 'tar:fifo-entry))
  (case type
    (:pax
     (para:is local-time:timestamp= *default-pax-mtime* (tar:mtime entry))
     (para:is local-time:timestamp= *default-pax-atime* (tar:atime entry)))
    (:gnu
     (para:is local-time:timestamp= *default-gnu-mtime* (tar:mtime entry)))
    (t
     (para:is local-time:timestamp= *default-mtime* (tar:mtime entry))))
  (para:is = 0 (tar:uid entry))
  (para:is = 0 (tar:gid entry)))

(defun read-sda1 (type entry)
  (para:is equal "sda1" (tar:name entry))
  (para:is equal "root" (tar:uname entry))
  (para:is equal "root" (tar:gname entry))
  (para:is alexandria:set-equal
           '(:user-read :user-write :group-read :other-read)
           (tar:mode entry))
  (para:true (typep entry 'tar:block-device-entry))
  (case type
    (:pax
     (para:is local-time:timestamp= *default-pax-mtime* (tar:mtime entry))
     (para:is local-time:timestamp= *default-pax-atime* (tar:atime entry)))
    (:gnu
     (para:is local-time:timestamp= *default-gnu-mtime* (tar:mtime entry)))
    (t
     (para:is local-time:timestamp= *default-mtime* (tar:mtime entry))))
  (para:is = 0 (tar:uid entry))
  (para:is = 0 (tar:gid entry))
  (para:is = 8 (tar:devmajor entry))
  (para:is = 1 (tar:devminor entry)))

(defun read-tty0 (type entry)
  (para:is equal "tty0" (tar:name entry))
  (para:is equal "root" (tar:uname entry))
  (para:is equal "root" (tar:gname entry))
  (para:is alexandria:set-equal
           '(:user-read :user-write :group-read :other-read)
           (tar:mode entry))
  (para:true (typep entry 'tar:character-device-entry))
  (case type
    (:pax
     (para:is local-time:timestamp= *default-pax-mtime* (tar:mtime entry))
     (para:is local-time:timestamp= *default-pax-atime* (tar:atime entry)))
    (:gnu
     (para:is local-time:timestamp= *default-gnu-mtime* (tar:mtime entry)))
    (t
     (para:is local-time:timestamp= *default-mtime* (tar:mtime entry))))
  (para:is = 0 (tar:uid entry))
  (para:is = 0 (tar:gid entry))
  (para:is = 4 (tar:devmajor entry))
  (para:is = 0 (tar:devminor entry)))

(defun read-sparse.txt (type entry)
  (ecase type
    (:gnu
     (para:is equal "sparse.txt" (tar-file:name entry)))
    (:pax
     (para:true (uiop:string-prefix-p "./GNUSparseFile." (tar-file:name entry)))
     (para:true (uiop:string-suffix-p (tar-file:name entry) "/sparse.txt"))))
  (ecase type
    (:gnu
     (para:is = 0 (tar-file:size entry)))
    (:pax
     (para:is = 512 (tar-file:size entry))))
  (when (eql type :gnu)
    (para:is = 5242880 (tar-file::offset-sparse-0 entry))
    (para:is = 0 (tar-file::numbytes-sparse-0 entry)))
  (para:is equal "root" (tar-file:uname entry))
  (para:is equal "root" (tar-file:gname entry))
  (para:is = #o644 (tar-file:mode entry))
  (para:is = *default-mtime* (tar-file:mtime entry))
  (para:is = 0 (tar-file:uid entry))
  (para:is = 0 (tar-file:gid entry)))
