;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar-simple-extract)

(defvar *deferred-links*)

(defun simple-extract-archive (archive
                               &key
                                 (directory *default-pathname-defaults*)

                                 (absolute-pathnames :error)
                                 (device-pathnames :error)
                                 (dot-dot :error)
                                 (strip-components 0)

                                 (if-exists :error)
                                 (if-newer-exists :error)

                                 (symbolic-links :dereference)
                                 (hard-links :dereference)
                                 (character-devices :skip)
                                 (block-devices :skip)
                                 (fifos :skip)

                                 (filter (constantly t)))
  "Extract all entries in ARCHIVE to DIRECTORY.

DIRECTORY defaults to *DEFAULT-PATHNAME-DEFAULTS* and must be a directory
pathname.

The following options configure how the final pathname of each entry is
computed:

ABSOLUTE-PATHANMES controls what happens when an entry is discovered that has
an absolute pathname. It defaults to :ERROR. The possible values are:

+ :ALLOW : Allow the pathname as is.
+ :SKIP : Silently skip the entry.
+ :RELATIVIZE : Strip the leading / and treat it as a relative pathname.
+ :ERROR : Signal an ENTRY-NAME-IS-ABSOLUTE-ERROR, with the restarts CONTINUE,
  SKIP-ENTRY, and RELATIVIZE-ENTRY-NAME active.

DEVICE-PATHNAMES controls what happens when an entry is discovered that has a
non-NIL device. It defaults to :ERROR. The possible values are:

+ :ALLOW : Allow the pathname as is.
+ :SKIP : Silently skip the entry.
+ :RELATIVIZE : Strip the device.
+ :ERROR : Signal an ENTRY-NAME-CONTAINS-DEVICE-ERROR, with the restarts
  CONTINUE, SKIP-ENTRY, and RELATIVIZE-ENTRY-NAME active.

DOT-DOT controls what happens when an entry is discovered that has a name
containing .. in a directory component. It defaults to :ERROR. The possible
values are:

+ :BACK : Allow the pathname as is, treating .. as :BACK.
+ :SKIP : Silently skip the entry.
+ :ERROR : Signal an ENTRY-NAME-CONTAINS-..-ERROR, with the restarts
  TREAT-..-AS-BACK and SKIP-ENTRY active.

STRIP-COMPONENTS is an integer specifying how many directory and file
components to strip. Defaults to 0.



The following options configure what happens to files that already exist on the
filesystem.

IF-NEWER-EXISTS controls what happens to files that already exist within
DIRECTORY if extracting ARCHIVE would overwrite them and the existing file has
a more recent mtime. It defaults to :ERROR. The possible values are:

+ NIL, :KEEP : existing files are skipped
+ :SUPERSEDE, :RENAME, :RENAME-AND-DELETE, :NEW-VERSION, :ERROR : Same behavior as OPEN.

IF-EXISTS controls what happens to files that already exist within
DIRECTORY. It defaults to :ERROR. The possible values are:

+ NIL, :KEEP : existing files are skipped
+ :SUPERSEDE, :RENAME, :RENAME-AND-DELETE, :NEW-VERSION, :ERROR : Same behavior as OPEN.


The following options configure how certain types of entries are extracted.

SYMBOLIC-LINKS controls how symbolic links are extracted from ARCHIVE. It
defaults to :DEREFERENCE. The possible values are:

+ :DEREFERENCE : any symlink entries are instead written as normal files with
  the contents of the file they point to.
+ :SKIP : Skip the symlink.
+ :ERROR : Signal a EXTRACT-SYMBOLIC-LINK-ENTRY-ERROR with the restarts
  DEREFERENCE-LINK and SKIP-ENTRY active.

HARD-LINKS controls how hard links are extracted from ARCHIVE. It defaults to
:DEREFERENCE. The possible values are:

+ :DEREFERENCE : any hard link entries are instead written as normal files with
  the contents of the file they point to.
+ :SKIP : Skip the hard link.
+ :ERROR : Signal a EXTRACT-HARD-LINK-ENTRY-ERROR with the restarts
  DEREFERENCE-LINK and SKIP-ENTRY active.

CHARACTER-DEVICES controls how character devices are extracted from ARCHIVE. It
defaults to :SKIP. The possible values are:

+ :SKIP : Skip the entry.
+ :ERROR : Signal a EXTRACT-CHARACTER-DEVICE-ENTRY-ERROR with the restart
  SKIP-ENTRY active.

BLOCK-DEVICES controls how block devices are extracted from ARCHIVE. It
defaults to :SKIP. The possible values are:

+ :SKIP : Skip the entry.
+ :ERROR : Signal a EXTRACT-BLOCK-DEVICE-ENTRY-ERROR with the restart
  SKIP-ENTRY active.

FIFOS controls how FIFOs are extracted from ARCHIVE. It defaults to :SKIP. The
possible values are:

+ :SKIP : Skip the entry.
+ :ERROR : Signal a EXTRACT-FIFO-ENTRY-ERROR with the restart SKIP-ENTRY
  active.

The following option controls what entries are extracted.

FILTER defaults to (CONSTANTLY T). Must be a function designator that takes two
arguments (the entry and the pathname were it will be extracted) and returns
non-NIL if the entry should be extracted.

If symbolic and hard links are dereferenced, there may be broken or circular
links. If that is detected, a BROKEN-OR-CIRCULAR-LINKS-ERROR is signalled with
the CONTINUE restart active."
  (assert (uiop:directory-pathname-p directory) (directory)
          "DIRECTORY must be a directory pathname")
  (let ((*default-pathname-defaults* (uiop:ensure-directory-pathname directory))
        (*deferred-links* nil))
    (ensure-directories-exist *default-pathname-defaults*)
    (handler-bind
        ((entry-name-contains-device-error
           (lambda (c)
             (case device-pathnames
               (:allow (continue c))
               (:skip (skip-entry c))
               (:relativize (relativize-entry-name c)))))
         (entry-name-contains-..-error
           (lambda (c)
             (case dot-dot
               (:back (treat-..-as-back c))
               (:skip (skip-entry c)))))
         (entry-name-is-absolute-error
           (lambda (c)
             (case absolute-pathnames
               (:allow (continue c))
               (:skip (skip-entry c))
               (:relativize (relativize-entry-name c)))))

         (extract-symbolic-link-entry-error
           (lambda (c)
             (case symbolic-links
               (:skip (skip-entry c))
               (:dereference (dereference-link c)))))
         (extract-hard-link-entry-error
           (lambda (c)
             (case hard-links
               (:skip (skip-entry c))
               (:dereference (dereference-link c)))))
         (extract-fifo-entry-error
           (lambda (c)
             (case fifos
               (:skip (skip-entry c)))))
         (extract-block-device-entry-error
           (lambda (c)
             (case block-devices
               (:skip (skip-entry c)))))
         (extract-character-device-entry-error
           (lambda (c)
             (case character-devices
               (:skip (skip-entry c))))))
      (tar:do-entries (entry archive)
        (restart-case
            (let* ((*current-entry* entry)
                   (pn (compute-extraction-pathname entry (tar:name entry) strip-components)))
              (when (and (not (null pn))
                         (funcall filter entry pn))
                (simple-extract-entry entry pn :if-exists if-exists
                                               :if-newer-exists if-newer-exists)))
          (skip-entry ())))
      (process-deferred-links *deferred-links* if-exists if-newer-exists)
      (values))))

(defun link-target-exists-p (pair)
  (let ((entry (first pair))
        (pn (second pair))
        (type (third pair)))
    (probe-file (merge-pathnames (tar:linkname entry)
                                 (if (eql type :hard)
                                     *default-pathname-defaults*
                                     (merge-pathnames pn))))))

(defun process-deferred-links-1 (deferred-links if-exists if-newer-exists)
  (let ((actionable-links (remove-if-not 'link-target-exists-p deferred-links)))
    (dolist (actionable-link actionable-links)
      (destructuring-bind (entry pn type) actionable-link
        (let ((pn (merge-pathnames pn)))
          (unless (null (probe-file pn))
            (let ((file-write-date (file-write-date pn)))
              (when (and (not (null file-write-date))
                         (> file-write-date (local-time:timestamp-to-universal (tar:mtime entry))))
                (setf if-exists if-newer-exists))))
          (when (eql if-exists :keep)
            (setf if-exists nil))
          (with-open-file (s pn :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists if-exists)
            (unless (null s)
              (with-open-file (source (merge-pathnames (tar:linkname entry)
                                                       (if (eql type :hard)
                                                           *default-pathname-defaults*
                                                           pn))
                                      :element-type '(unsigned-byte 8))
                (uiop:copy-stream-to-stream source s :element-type '(unsigned-byte 8))))))))
    (set-difference deferred-links actionable-links)))

(defun process-deferred-links (deferred-links if-exists if-newer-exists)
  (loop
    :for prev-links := deferred-links :then links
    :for links := (process-deferred-links-1 prev-links if-exists if-newer-exists)
    :while links
    :when (= (length links) (length prev-links))
      :do (restart-case
              (error 'broken-or-circular-links-error)
            (continue () (return)))))

(defgeneric simple-extract-entry (entry pn &key))

(defmethod simple-extract-entry :before ((entry tar:entry) pn &key &allow-other-keys)
  (ensure-directories-exist (merge-pathnames pn)))

(defmethod simple-extract-entry :around ((entry tar:symbolic-link-entry) pn &key &allow-other-keys)
  (restart-case
      (error 'extract-symbolic-link-entry-error :entry entry)
    (dereference-link ()
      (push (list entry pn :symbolic) *deferred-links*))))

(defmethod simple-extract-entry :around ((entry tar:hard-link-entry) pn &key &allow-other-keys)
  (restart-case
      (error 'extract-hard-link-entry-error :entry entry)
    (dereference-link ()
      (push (list entry pn :hard) *deferred-links*))))

(defmethod simple-extract-entry :before ((entry tar:fifo-entry) pn &key &allow-other-keys)
  (error 'extract-fifo-entry-error :entry entry))

(defmethod simple-extract-entry :before ((entry tar:block-device-entry) pn &key &allow-other-keys)
  (error 'extract-block-device-entry-error :entry entry))

(defmethod simple-extract-entry :before ((entry tar:character-device-entry) pn &key &allow-other-keys)
  (error 'extract-character-device-entry-error :entry entry))

(defmethod simple-extract-entry ((entry tar:entry) pn &key &allow-other-keys)
  (declare (ignore pn))
  t)

(defmethod simple-extract-entry ((entry tar:file-entry) pn &key if-exists if-newer-exists)
  (let ((pn (merge-pathnames pn)))
    (unless (null (probe-file pn))
      (let ((file-write-date (file-write-date pn)))
        (when (and (not (null file-write-date))
                   (> file-write-date (local-time:timestamp-to-universal (tar:mtime entry))))
          (setf if-exists if-newer-exists))))
    (when (eql if-exists :keep)
      (setf if-exists nil))
    (with-open-file (s pn :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists if-exists)
      (unless (null s)
        (uiop:copy-stream-to-stream (tar:make-entry-stream entry) s
                                    :element-type '(unsigned-byte 8))))))
