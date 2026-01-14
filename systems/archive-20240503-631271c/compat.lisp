;;; compat.lisp -- compatibility wrappers for accessing Unix-y things

(in-package :archive)

(defconstant +permissions-mask+ 
  #+use-sb-posix (logior sb-posix:s-irusr sb-posix:s-iwusr sb-posix:s-ixusr
			 sb-posix:s-irgrp sb-posix:s-iwgrp sb-posix:s-ixgrp
			 sb-posix:s-iroth sb-posix:s-iwoth sb-posix:s-ixoth)
  #-use-sb-posix 511)

;;; SYSTEM:GET-FILE-STAT is standard on Lispworks/Unix, but isn't
;;; available on Windows.  We provide our own here.
#+(and lispworks win32)
(progn
(fli:define-foreign-function (stat32 "_stat32")
    ((path :pointer)
     (struct-buf :pointer))
  :result-type :int)

;;; struct _stat comes in several different flavors under Win32.  This
;;; is the version with 32-bit time_t and st_size.  It'd be nice to
;;; figure out how to use the 64-bit st_size one, but trying to use that
;;; results in arcane errors on my copy of Lispworks Personal 5.1.1.
(fli:define-c-struct _stat32
  (dev :unsigned-int)
  (ino :unsigned-short)
  (mode :unsigned-short)
  (nlink :short)
  (uid :short)
  (gid :short)
  (rdev :unsigned-int)
  (size :long)
  (atime :long)
  (mtime :long)
  (ctime :long))

(defstruct file-stat
  inode device owner-id group-id size mode last-access last-change
  last-modify links device-type)

(defun convert-to-lisp-struct (stat)
  (fli:with-foreign-slots (dev ino mode nlink uid gid rdev size
                               atime mtime ctime)
      stat
    (make-file-stat :inode ino :device dev :owner-id uid :group-id gid
                    :size size :mode mode
                    :last-access atime :last-change ctime :last-modify mtime
                    :links nlink :device-type rdev)))

(defun get-file-stat (file)
  (fli:with-dynamic-foreign-objects ()
    (let ((stat (fli:allocate-dynamic-foreign-object :type '_stat32))
          (string (fli:convert-to-dynamic-foreign-string (namestring file))))
      (when (zerop (stat32 string stat))
        (convert-to-lisp-struct stat)))))
) ; PROGN

;;; CMUCL and Clozure CL returns multiple values from UNIX:UNIX-STAT or CCL::%STAT.  We need to
;;; package these up into something to which we can repeatedly reference.
#+(or cmucl ccl)
(defclass stat ()
  ((dev :initarg :dev :reader dev)
   (ino :initarg :ino :reader ino)
   (mode :initarg :mode :reader mode)
   (nlink :initarg :nlink :reader nlink)
   (uid :initarg :uid :reader uid)
   (gid :initarg :gid :reader gid)
   (rdev :initarg :rdev :reader rdev)
   (atime :initarg :atime :reader atime)
   (mtime :initarg :mtime :reader mtime)
   (ctime :initarg :ctime :reader ctime)
   (size :initarg :size :reader size)
   (blocks :initarg :blocks :reader blocks)
   (blksize :initarg :blksize :reader blksize)
   (flags :initarg :flags :reader flags)
   (gen :initarg :gen :reader gen)))

(defun stat (file)
  ;; Allow passing file descriptors, too.
  (let ((file (if (integerp file) file (merge-pathnames file))))
    #+sbcl
    (if (integerp file)
        (sb-posix:fstat file)
        (sb-posix:stat file))
    #+lispworks
    (if (integerp file)
        #+unix (get-file-stat file) #-unix (error "stat'ing file descriptions not supported on win32")
        (get-file-stat file))
    #+clisp
    (if (integerp file)
        #+unix (posix:file-stat file) #-unix (error "stat'ing file descriptions not supported on win32")
        (posix:file-stat file))
    #+cmucl
    (multiple-value-bind (successp dev ino mode nlink uid gid
                                           rdev atime mtime ctime size
                                           blocks blksize flags gen)
        (if (integerp file) (unix:unix-fstat file) (unix:unix-stat file))
      (unless successp
        (error "Could not get information on ~A" file))
              (make-instance 'stat
                             :dev dev :ino ino :mode mode :nlink nlink
                             :uid uid :gid gid :rdev rdev
                             :atime atime :mtime mtime :ctime ctime
                             :size size :blocks blocks :blksize blksize
                             :flags flags :gen gen))
    #+ccl
    (multiple-value-bind (successp mode size mtime ino uid blksize rdev gid dev)
        (if (integerp file)
            (ccl::%fstat file)
            (ccl::%stat (with-output-to-string (s)
                          (loop for char across (princ-to-string file)
                                ;; Sometimes a pathname contains some backslashes (#\\)
                                ;; and CCL::%STAT fails with it.
                                ;;   ex) "\\.travis.yml"
                                unless (char= char #\\) do
                                  (write-char char s)))))
      (unless successp
        (error "Could not get information on ~S" file))
      (make-instance 'stat
                     :dev dev :ino ino :mode mode
                     :uid uid :gid gid :rdev rdev
                     :mtime mtime
                     :size size :blksize blksize))
    #-(or sbcl lispworks clisp cmucl ccl) (error "Not implemented")))


;;; messing with stat modes
(macrolet ((define-file-type-test (fun unix-name mask)
             (declare (ignorable mask))
             `(defun ,fun (mode)
                #+use-sb-posix
                (,(intern (format nil "S-~A" unix-name) :sb-posix) mode)
                #-use-sb-posix
                (flet ((stat-file-type (mode) (logand mode #o170000)))
                  (= (stat-file-type mode) ,mask)))))
  (define-file-type-test isdir isdir #o40000)
  (define-file-type-test isreg isreg #o100000)
  (define-file-type-test islink islnk #o0140000)
  (define-file-type-test ischarfile ischr #o20000)
  (define-file-type-test isblockfile isblk #o0060000)
  (define-file-type-test isfifo isfifo #o0010000))

;;; stat field accessors
(defun stat-mode (stat)
  #+sbcl (sb-posix::stat-mode stat)
  #+lispworks (file-stat-mode stat)
  #+clisp (posix:convert-mode (posix:file-stat-mode stat))
  #+(or cmucl ccl) (mode stat)
  #-(or sbcl lispworks clisp cmucl ccl) (error "Not implemented"))

(defun stat-uid (stat)
  #+sbcl (sb-posix::stat-uid stat)
  #+lispworks (file-stat-owner-id stat)
  #+clisp (posix:file-stat-uid stat)
  #+(or cmucl ccl) (uid stat)
  #-(or sbcl lispworks clisp cmucl ccl) (error "Not implemented"))

(defun stat-gid (stat)
  #+sbcl (sb-posix::stat-gid stat)
  #+lispworks (file-stat-group-id stat)
  #+clisp (posix:file-stat-gid stat)
  #+(or cmucl ccl) (gid stat)
  #-(or sbcl lispworks clisp cmucl ccl) (error "Not implemented"))

(defun stat-size (stat)
  #+sbcl (sb-posix::stat-size stat)
  #+lispworks (file-stat-size stat)
  #+clisp (posix:file-stat-size stat)
  #+(or cmucl ccl) (size stat)
  #-(or sbcl lispworks clisp cmucl ccl) (error "Not implemented"))

(defun stat-mtime (stat)
  #+sbcl (sb-posix::stat-mtime stat)
  #+lispworks (file-stat-last-modify stat)
  #+clisp (posix:file-stat-mtime stat)
  #+(or cmucl ccl) (mtime stat)
  #-(or sbcl lispworks clisp cmucl ccl) (error "Not implemented"))

(defun stat-ino (stat)
  #+sbcl (sb-posix::stat-ino stat)
  #+lispworks (file-stat-inode stat)
  #+clisp (posix:file-stat-ino stat)
  #+(or cmucl ccl) (ino stat)
  #-(or sbcl lispworks clisp cmucl ccl) (error "Not implemented"))

(defun stat-nlink (stat)
  #+sbcl (sb-posix::stat-nlink stat)
  #+lispworks (file-stat-links stat)
  #+clisp (posix:file-stat-nlink stat)
  #+cmucl (nlink stat)
  #-(or sbcl lispworks clisp cmucl) (error "Not implemented"))
