;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar-extract)

(defvar *deferred-links*)

(defvar *deferred-directories*)

#+tar-extract-use-openat
(defvar *destination-dir-fd*)


;;; Directory permissions

#+tar-extract-use-openat
(defun handle-deferred-directory (entry pn
                                  &key
                                    touch no-same-owner numeric-uid mask)
  (with-fd (fd (fd (openat *destination-dir-fd* pn
                           (logior nix:s-irusr
                                   nix:s-iwusr
                                   nix:s-ixusr))))
    (unless touch
      (set-utimes entry :fd fd))
    (set-permissions entry mask :fd fd)
    (unless no-same-owner
      (set-owner entry numeric-uid :fd fd))))

#-tar-extract-use-openat
(defun handle-deferred-directory (entry pn
                                  &key
                                    touch no-same-owner numeric-uid mask)

  (set-permissions entry mask :pn pn))


;;; Symbolic links

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


;;; Extracting entries

(defgeneric extract-entry (entry pn &key mask numeric-uid no-same-owner touch keep-directory-metadata))

(defmethod extract-entry ((entry tar:file-entry) pn
                          &key touch no-same-owner numeric-uid mask
                            &allow-other-keys)
  #+windows (declare (ignore no-same-owner numeric-uid))
  (with-open-stream (stream #+tar-extract-use-openat (openat *destination-dir-fd* pn
                                                             (logior nix:s-irusr
                                                                     nix:s-iwusr))
                            #-tar-extract-use-openat (my-open pn (logior nix:s-irusr nix:s-iwusr)))
    (uiop:copy-stream-to-stream (tar:make-entry-stream entry) stream
                                :element-type '(unsigned-byte 8))
    (unless touch
      (set-utimes entry :fd (fd stream)))
    (set-permissions entry mask :fd (fd stream) :pn pn)
    #-windows
    (unless no-same-owner
      (set-owner entry numeric-uid :fd (fd stream)))))

(defmethod extract-entry ((entry tar:directory-entry) pn &key keep-directory-metadata &allow-other-keys)
  #+tar-extract-use-openat
  (with-fd (dirfd (fd (openat *destination-dir-fd* pn
                              (logior nix:s-irusr
                                      nix:s-iwusr
                                      nix:s-ixusr))))
    (declare (ignore dirfd))
    ;; Enqueue this to later set its properties.
    (unless keep-directory-metadata
      (push (cons entry pn) *deferred-directories*)))
  #-tar-extract-use-openat
  (progn
    (ensure-directories-exist (merge-pathnames pn))
    (unless keep-directory-metadata
      (push (cons entry pn) *deferred-directories*))))

(defmethod extract-entry ((entry tar:symbolic-link-entry) pn &key touch no-same-owner numeric-uid
                          &allow-other-keys)
  #+windows (declare (ignore touch no-same-owner numeric-uid))
  (restart-case
      (error 'extract-symbolic-link-entry-error)
    #-windows
    (extract-link ()
      (let ((dir-fd (fd (openat *destination-dir-fd* (uiop:pathname-directory-pathname pn) nix:s-irwxu))))
        (with-fd (dir-fd)
          (nix:symlinkat (tar:linkname entry) dir-fd (file-namestring pn))
          ;; There's no great portable to get a file descriptor for a symlink,
          ;; so we have to change utimes and owner by pathname.
          (unless touch
            (set-utimes entry :dirfd dir-fd :pn (file-namestring pn)))
          (unless no-same-owner
            (set-owner entry numeric-uid :dirfd dir-fd :pn (file-namestring pn))))))
    (dereference-link ()
      (push (list entry pn :symbolic) *deferred-links*))))

(defmethod extract-entry ((entry tar:hard-link-entry) pn &key &allow-other-keys)
  (restart-case
      (error 'extract-hard-link-entry-error)
    #-windows
    (extract-link ()
      (let ((destination-pn (uiop:parse-unix-namestring (tar:linkname entry)
                                                        :dot-dot :back)))
        (with-fd (dir-fd (fd (openat *destination-dir-fd* (uiop:pathname-directory-pathname pn) nix:s-irwxu)))
          (with-fd (destination-dir-fd (fd (openat *destination-dir-fd*
                                                   (uiop:pathname-directory-pathname destination-pn)
                                                   nix:s-irwxu)))
            (nix:linkat destination-dir-fd (file-namestring destination-pn)
                        dir-fd (file-namestring pn) 0)))))
    (dereference-link ()
      (push (list entry pn :hard) *deferred-links*))))

(defmethod extract-entry ((entry tar:fifo-entry) pn &key mask touch no-same-owner numeric-uid &allow-other-keys)
  #+windows (declare (ignore mask touch no-same-owner numeric-uid))
  (restart-case
      (error 'extract-fifo-entry-error)
    #-windows
    (extract-fifo ()
      (let ((dir-fd (fd (openat *destination-dir-fd* (uiop:pathname-directory-pathname pn) nix:s-irwxu))))
        (with-fd (dir-fd)
          #+tar-extract-use-mkfifoat
          (nix:mkfifoat dir-fd (file-namestring pn) (tar::permissions-to-mode
                                                     (set-difference (tar:mode entry)
                                                                     mask)))
          #-tar-extract-use-mkfifoat
          (nix:mkfifo (merge-pathnames pn) (tar::permissions-to-mode
                                            (set-difference (tar:mode entry)
                                                            mask)))
          (with-fd (fifo-fd (nix:openat dir-fd (file-namestring pn)
                                        (logior nix:o-rdonly
                                                nix:o-nofollow
                                                nix:o-nonblock)))
            (unless touch
              (set-utimes entry :fd fifo-fd))
            (set-permissions entry mask :fd fifo-fd)
            (unless no-same-owner
              (set-owner entry numeric-uid :fd fifo-fd))))))))

(defmethod extract-entry ((entry tar:block-device-entry) pn &key touch no-same-owner numeric-uid mask
                          &allow-other-keys)
  #+windows (declare (ignore touch no-same-owner numeric-uid mask))
  (restart-case
      (error 'extract-block-device-entry-error :entry entry)
    #-windows
    (extract-device ()
      (let ((dir-fd (fd (openat *destination-dir-fd* (uiop:pathname-directory-pathname pn) nix:s-irwxu))))
        (with-fd (dir-fd)
          (nix:mknodat dir-fd (file-namestring pn)
                       (logior nix:s-ifblk
                               (tar::permissions-to-mode (tar:mode entry)))
                       (nix:makedev (tar:devmajor entry) (tar:devminor entry)))
          (with-fd (dev-fd (nix:openat dir-fd (file-namestring pn)
                                       (logior nix:o-rdonly
                                               nix:o-nofollow
                                               nix:o-nonblock)))
            (unless touch
              (set-utimes entry :fd dev-fd))
            (set-permissions entry mask :fd dev-fd)
            (unless no-same-owner
              (set-owner entry numeric-uid :fd dev-fd))))))))

(defmethod extract-entry ((entry tar:character-device-entry) pn &key touch no-same-owner numeric-uid mask
                          &allow-other-keys)
  #+windows (declare (ignore touch no-same-owner numeric-uid mask))
  (restart-case
      (error 'extract-character-device-entry-error :entry entry)
    #-windows
    (extract-device ()
      (let ((dir-fd (fd (openat *destination-dir-fd* (uiop:pathname-directory-pathname pn) nix:s-irwxu))))
        (with-fd (dir-fd)
          (nix:mknodat dir-fd (file-namestring pn)
                       (logior nix:s-ifchr
                               (tar::permissions-to-mode (tar:mode entry)))
                       (nix:makedev (tar:devmajor entry) (tar:devminor entry)))
          (with-fd (dev-fd (nix:openat dir-fd (file-namestring pn)
                                       (logior nix:o-rdonly
                                               nix:o-nofollow
                                               nix:o-nonblock)))
            (unless touch
              (set-utimes entry :fd dev-fd))
            (set-permissions entry mask :fd dev-fd)
            (unless no-same-owner
              (set-owner entry numeric-uid :fd dev-fd))))))))


;;; Extract archive

(defun extract-archive (archive
                        &key
                          (directory *default-pathname-defaults*)

                          (absolute-pathnames :error)
                          (device-pathnames :error)
                          (dot-dot :error)
                          (strip-components 0)

                          (if-exists :error)
                          (if-newer-exists :error)
                          (if-symbolic-link-exists :error)
                          (if-directory-symbolic-link-exists :error)

                          keep-directory-metadata
                          touch
                          (no-same-owner #+:windows t
                                         #-windows (rootp))
                          numeric-uid
                          mask

                          (symbolic-links #-windows t #+windows :dereference)
                          (hard-links #-windows t #+windows :dereference)
                          (character-devices #-windows (if (rootp) t :error) #+windows :error)
                          (block-devices #-windows (if (rootp) t :error) #+windows :error)
                          (fifos #-windows t #+windows :error)

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

IF-DIRECTORY-SYMBOLIC-LINK-EXISTS controls what happens to existing directories
that are symlinks. This includes any symlink on the destination path of the
entry. Defaults to :ERROR. The possible values are:

+ :ERROR : Signal a EXTRACTION-THROUGH-SYMBOLIC-LINK-ERROR with the restarts
  FOLLOW-SYMBOLIC-LINK, REPLACE-SYMBOLIC-LINK, and SKIP-ENTRY active.
+ NIL, :SKIP : Skip the entry.
+ :SUPERSEDE : replace the symlink with a new, empty directory.
+ :FOLLOW : keep the existing symlink.

IF-SYMBOLIC-LINK-EXISTS controls what happesn to existing files that are
symlinks. Defaults to :ERROR. The possible values are:

+ :ERROR : Signal a EXTRACTION-THROUGH-SYMBOLIC-LINK-ERROR with the restarts
  FOLLOW-SYMBOLIC-LINK, REPLACE-SYMBOLIC-LINK, and SKIP-ENTRY active.
+ NIL, :SKIP : Skip the entry.
+ :FOLLOW : Follow the symlink and write the contents of the entry, respecting
  IF-NEWER-EXISTS and IF-EXISTS.
+ :SUPERSEDE : Replace the symlink.


IF-NEWER-EXISTS controls what happens to files that already exist within
DIRECTORY if extracting ARCHIVE would overwrite them and the existing file has
a more recent mtime. It defaults to :ERROR. The possible values are:

+ :ERROR : A DESTINATION-EXISTS-ERROR is signaled, with the restarts
  SUPERSEDE-FILE, REMOVE-FILE, and SKIP-ENTRY active
+ NIL, :SKIP, :KEEP : existing files are skipped
+ :SUPERSEDE : Overwrite the file
+ :REPLACE : Delete file and replace it, atomically if possible.

IF-EXISTS controls what happens to files that already exist within
DIRECTORY. It defaults to :ERROR. The possible values are:

+ :ERROR : A DESTINATION-EXISTS-ERROR is signaled, with the restarts
  SUPERSEDE-FILE, REMOVE-FILE, and SKIP-ENTRY active
+ NIL, :SKIP, :KEEP : existing files are skipped
+ :SUPERSEDE : Overwrite the file
+ :REPLACE : Delete file and replace it, atomically if possible.


The following options configure how metadata is extracted.

If KEEP-DIRECTORY-METADATA is non-NIL, then metadata for existing directories
is kept.

If TOUCH is non-NIL, file mtimes will not be set on extraction.

If NO-SAME-OWNER is non-NIL, then the owner and group of extracted entries will
not be set. Defaults to T for non-root users and Windows. Must be T on Windows.

If NUMERIC-UID is non-NIL, UIDs and GIDs are preferred over UNAMEs and GNAMEs.

MASK is a list of permissions to remove from all entries.

The following options configure how certain types of entries are extracted.

SYMBOLIC-LINKS controls how symbolic links are extracted from ARCHIVE. It
defaults to T on non-Windows platforms and :DEREFERENCE on Windows. The
possible values are:

+ T : Extract the symlink as normal.
+ :DEREFERENCE : any symlink entries are instead written as normal files with
  the contents of the file they point to.
+ :SKIP : Skip the symlink.
+ :ERROR : Signal a EXTRACT-SYMBOLIC-LINK-ENTRY-ERROR with the restarts
  EXTRACT-LINK, DEREFERENCE-LINK and SKIP-ENTRY active.

HARD-LINKS controls how hard links are extracted from ARCHIVE. It defaults to T
on non-WINDOWS platforms and :DEREFERENCE on Windows. The possible values are:

+ T : Extract the hard link as normal.
+ :DEREFERENCE : any hard link entries are instead written as normal files with
  the contents of the file they point to.
+ :SKIP : Skip the hard link.
+ :ERROR : Signal a EXTRACT-HARD-LINK-ENTRY-ERROR with the restarts
  EXTRACT-LINK, DEREFERENCE-LINK and SKIP-ENTRY active.

CHARACTER-DEVICES controls how character devices are extracted from ARCHIVE. It
defaults to :ERROR on Windows or non-Windows and the current user is not root,
T otherwise. The possible values are:

+ T : Extract the character device.
+ :SKIP : Skip the entry.
+ :ERROR : Signal a EXTRACT-CHARACTER-DEVICE-ENTRY-ERROR with the restarts
  EXTRACT-DEVICE and SKIP-ENTRY active.

BLOCK-DEVICES controls how block devices are extracted from ARCHIVE. It
defaults to :ERROR on Windows or non-Windows and the current user is not root,
T otherwise. The possible values are:

+ T : Extract the block device.
+ :SKIP : Skip the entry.
+ :ERROR : Signal a EXTRACT-BLOCK-DEVICE-ENTRY-ERROR with the restarts
  EXTRACT-DEVICE and SKIP-ENTRY active.

FIFOS controls how FIFOs are extracted from ARCHIVE. It defaults to T on
non-Windows platforms and :ERROR on Windows. The possible values are:

+ T : Extract the FIFO.
+ :SKIP : Skip the entry.
+ :ERROR : Signal a EXTRACT-FIFO-ENTRY-ERROR with the restarts EXTRACT-FIFO
  and SKIP-ENTRY active.

The following option controls what entries are extracted.

FILTER defaults to (CONSTANTLY T). Must be a function designator that takes two
arguments (the entry and the pathname were it will be extracted) and returns
non-NIL if the entry should be extracted.

If symbolic and hard links are dereferenced, there may be broken or circular
links. If that is detected, a BROKEN-OR-CIRCULAR-LINKS-ERROR is signalled with
the CONTINUE restart active."
  (assert (uiop:directory-pathname-p directory) (directory)
          "DIRECTORY must be a directory pathname")
  #+windows
  (assert no-same-owner (no-same-owner) "NO-SAME-OWNER must be non-NIL on Windows.")
  (let ((*default-pathname-defaults* (uiop:ensure-directory-pathname directory))
        (*deferred-links* nil)
        (*deferred-directories* nil)
        #+tar-extract-use-openat *destination-dir-fd*)
    (ensure-directories-exist *default-pathname-defaults*)
    (#+tar-extract-use-openat with-fd
     #+tar-extract-use-openat (*destination-dir-fd* (nix:open *default-pathname-defaults*
                                                              nix:o-rdonly))
     #-tar-extract-use-openat progn
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
                 (:dereference (dereference-link c))
                 ((t) (extract-link c)))))
           (extract-hard-link-entry-error
             (lambda (c)
               (case hard-links
                 (:skip (skip-entry c))
                 (:dereference (dereference-link c))
                 ((t) (extract-link c)))))
           (extract-fifo-entry-error
             (lambda (c)
               (case fifos
                 (:skip (skip-entry c))
                 ((t) (extract-fifo c)))))
           (extract-block-device-entry-error
             (lambda (c)
               (case block-devices
                 (:skip (skip-entry c))
                 ((t) (extract-device c)))))
           (extract-character-device-entry-error
             (lambda (c)
               (case character-devices
                 (:skip (skip-entry c))
                 ((t) (extract-device c)))))

           (extraction-through-symbolic-link-error
             (lambda (c)
               (if (uiop:directory-pathname-p (extraction-through-symbolic-link-error-pathname c))
                   (case if-directory-symbolic-link-exists
                     ((nil :skip) (skip-entry c))
                     (:follow (follow-symbolic-link c))
                     (:supersede (replace-symbolic-link c)))
                   (case if-symbolic-link-exists
                     ((nil :skip) (skip-entry c))
                     (:follow (follow-symbolic-link c))
                     (:supersede (replace-symbolic-link c))))))

           (destination-exists-error
             (lambda (c)
               (let ((entry-mtime (tar:mtime (extraction-entry-error-entry c)))
                     (existing-mtime (destination-exists-error-mtime c)))
                 (if (local-time:timestamp< entry-mtime existing-mtime)
                     (case if-newer-exists
                       ((nil :skip :keep) (skip-entry c))
                       (:supersede (invoke-restart 'supersede-file))
                       (:replace (invoke-restart 'remove-file)))
                     (case if-exists
                       ((nil :skip :keep) (skip-entry c))
                       (:supersede (invoke-restart 'supersede-file))
                       (:replace (invoke-restart 'remove-file))))))))
        (tar:do-entries (entry archive)
          (let ((*current-entry* entry))
            (restart-case
                (let ((pn (compute-extraction-pathname entry (tar:name entry) strip-components)))
                  (when (and (not (null pn))
                             (funcall filter entry pn))
                    (tar:with-ignored-unsupported-properties ()
                      (extract-entry entry pn
                                     :mask mask
                                     :numeric-uid numeric-uid
                                     :no-same-owner no-same-owner
                                     :touch touch
                                     :keep-directory-metadata keep-directory-metadata))))
              (skip-entry ()))))
        (process-deferred-links *deferred-links* if-exists if-newer-exists)
        (dolist (pair *deferred-directories*)
          (handle-deferred-directory (car pair) (cdr pair)
                                     :mask mask
                                     :numeric-uid numeric-uid
                                     :no-same-owner no-same-owner
                                     :touch touch))
        (values)))))
