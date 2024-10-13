;;;; This is part of tar-file. See README.org and LICENSE for more information.

(in-package #:tar-file)

(defgeneric close-tar-file (tar-file)
  (:documentation
   "Closes the stream associated with TAR-FILE and the tar-file itself.
Further operations on the tar-file are undefined.

Does NOT close the underlying STREAM that backed the TAR-FILE."))



(defgeneric name (entry)
  (:documentation "Return the name of the ENTRY (a string)."))

(defgeneric mode (entry)
  (:documentation "Return the mode of the ENTRY (an integer)."))

(defgeneric uid (entry)
  (:documentation "Return the uid of the ENTRY (an integer)."))

(defgeneric gid (entry)
  (:documentation "Return the gid of the ENTRY (an integer)."))

(defgeneric size (entry)
  (:documentation "Return the size of the ENTRY (an integer)."))

(defgeneric mtime (entry)
  (:documentation "Return the mtime of the ENTRY (an integer)."))

(defgeneric linkname (entry)
  (:documentation "Return the linkname of the ENTRY (a string)."))

(defgeneric uname (entry)
  (:documentation "Return the uname of the ENTRY (a string)."))

(defgeneric gname (entry)
  (:documentation "Return the gname  of the ENTRY (a string)."))

(defgeneric devmajor (entry)
  (:documentation "Return the major device of the ENTRY (an integer)."))

(defgeneric devminor (entry)
  (:documentation "Return the minor device of the ENTRY (an integer)."))

(defgeneric prefix (entry)
  (:documentation "Return the prefix of the ENTRY (a string)."))

(defgeneric atime (entry)
  (:documentation "Return the atime of the ENTRY (an integer)."))

(defgeneric ctime (entry)
  (:documentation "Return the ctime of the ENTRY (an integer)."))

(defgeneric offset (entry)
  (:documentation "Return the offset of the ENTRY (an integer)."))

(defgeneric offset-sparse-0 (entry)
  (:documentation "Return the offset of the first sparse block of the ENTRY (an integer)."))

(defgeneric numbytes-sparse-0 (entry)
  (:documentation "Return the numbytes of the first sparse block of the ENTRY (an integer)."))

(defgeneric offset-sparse-1 (entry)
  (:documentation "Return the offset of the second sparse block of the ENTRY (an integer)."))

(defgeneric numbytes-sparse-1 (entry)
  (:documentation "Return the numbytes of the second sparse block of the ENTRY (an integer)."))

(defgeneric offset-sparse-2 (entry)
  (:documentation "Return the offset of the third sparse block of the ENTRY (an integer)."))

(defgeneric numbytes-sparse-2 (entry)
  (:documentation "Return the numbytes of the third sparse block of the ENTRY (an integer)."))

(defgeneric offset-sparse-3 (entry)
  (:documentation "Return the offset of the fourth sparse block of the ENTRY (an integer)."))

(defgeneric numbytes-sparse-3 (entry)
  (:documentation "Return the numbytes of the fourth sparse block of the ENTRY (an integer)."))

(defgeneric isextended (entry)
  (:documentation "Return the isextended field of the ENTRY (an integer)."))

(defgeneric realsize (entry)
  (:documentation "Return the realsize of the ENTRY (an integer)."))

(defgeneric entry-file-p (entry)
  (:documentation "Returns non-NIL if ENTRY denotes a regular file.")
  (:method (entry)
    nil))

(defgeneric entry-directory-p (entry)
  (:documentation "Returns non-NIL if ENTRY denotes a directory.")
  (:method (entry)
    nil))

(defgeneric entry-symbolic-link-p (entry)
  (:documentation "Returns non-NIL if ENTRY denotes a symbolic link.")
  (:method (entry)
    nil))

(defgeneric entry-character-device-p (entry)
  (:documentation "Returns non-NIL if ENTRY denotes a character device.")
  (:method (entry)
    nil))

(defgeneric entry-block-device-p (entry)
  (:documentation "Returns non-NIL if ENTRY denotes a block device.")
  (:method (entry)
    nil))

(defgeneric entry-fifo-p (entry)
  (:documentation "Returns non-NIL if ENTRY denotes a fifo.")
  (:method (entry)
    nil))

(defgeneric entry-pax-extended-attributes-p (entry)
  (:documentation "Returns non-NIL if ENTRY contains PAX extended attributes.")
  (:method (entry)
    nil))

(defgeneric entry-pax-global-attributes-p (entry)
  (:documentation "Returns non-NIL if ENTRY contains PAX global attributes.")
  (:method (entry)
    nil))

(defgeneric entry-gnu-long-link-name-p (entry)
  (:documentation "Returns non-NIL if ENTRY contains a GNU long link name.")
  (:method (entry)
    nil))

(defgeneric entry-gnu-long-name-p (entry)
  (:documentation "Returns non-NIL if ENTRY contains a GNU long name.")
  (:method (entry)
    nil))

(defgeneric entry-gnu-directory-dump-p (entry)
  (:documentation "Returns non-NIL if ENTRY contains a GNU directory dump.")
  (:method (entry)
    nil))

(defgeneric entry-gnu-sparse-file-p (entry)
  (:documentation "Returns non-NIL if ENTRY contains a GNU sparse file.")
  (:method (entry)
    nil))

(defgeneric entry-gnu-volume-header-name-p (entry)
  (:documentation "Returns non-NIL if ENTRY contains a GNU volume header name.")
  (:method (entry)
    nil))

(defgeneric entry-unknown-p (entry)
  (:documentation "Returns non-NIL if ENTRY is unknown.")
  (:method (entry)
    nil))


;;; reading

(defgeneric read-entry (tar-file)
  (:documentation "Return the next entry in TAR-FILE or NIL if there is no
next entry"))


;;; writing

(defgeneric write-entry (tar-file entry
                         &key stream)
  (:documentation "Write ENTRY to TAR-FILE. Data associated with ENTRY is
written to TAR-FILE according to the :STREAM argument.  If :STREAM is T, the
expression (NAME ENTRY) is expected to refer to an existing file from which
data may be read.  If :STREAM is a stream, then data is read from that stream
and written to TAR-FILE.  If :STREAM is NIL, then no entry data is written."))

(defgeneric write-header-to-buffer (header buffer encoding &optional start)
  (:documentation "Write the information associated with HEADER into BUFFER,
beginning at position START."))

(defgeneric write-entry-data (tar-file entry stream)
  (:documentation "Write any data associated with ENTRY, possibly found
in STREAM to TAR-FILE; called after WRITE-HEADER-TO-BUFFER.  STREAM is
interpreted as in WRITE-ENTRY."))

(defgeneric finalize-tar-file (tar-file)
  (:documentation "Perform any necessary processing for finalizing TAR-FILE.
This function must be called prior to calling CLOSE-TAR-FILE."))

(defgeneric write-file-entry (tar-file name &rest args &key uname gname mode mtime uid gid size data
                                                         prefix)
  (:documentation
   "Write a FILE-ENTRY to TAR-FILE.

DATA can be either NIL (no data is written), an octet vector (written as is), a
string (encoded using UTF-8 and written), or a PATHNAME (opened, read, and
written to the archive)."))

(defgeneric write-hard-link-entry (tar-file name &rest args &key uname gname mode mtime uid gid linkname prefix)
  (:documentation
   "Write a HARD-LINK-ENTRY to TAR-FILE."))

(defgeneric write-symbolic-link-entry (tar-file name &rest args &key uname gname mode mtime uid gid linkname prefix)
  (:documentation
   "Write a SYMBOLIC-LINK-ENTRY to TAR-FILE."))

(defgeneric write-character-device-entry (tar-file name &rest args &key uname gname mode mtime uid gid
                                                                     devmajor devminor
                                                                     prefix)
  (:documentation
   "Write a CHARACTER-DEVICE-ENTRY to TAR-FILE."))

(defgeneric write-block-device-entry (tar-file name &rest args &key uname gname mode mtime uid gid
                                                                 devmajor devminor
                                                                 prefix)
  (:documentation
   "Write a BLOCK-DEVICE-ENTRY to TAR-FILE."))

(defgeneric write-directory-entry (tar-file name &rest args &key uname gname mode mtime uid gid size
                                                              prefix)
  (:documentation
   "Write a DIRECTORY-ENTRY to TAR-FILE."))

(defgeneric write-fifo-entry (tar-file name &rest args &key uname gname mode mtime uid gid prefix)
  (:documentation
   "Write a FIFO-ENTRY to TAR-FILE."))

(defgeneric write-pax-extended-attributes-entry (tar-file name &rest args &key attributes)
  (:documentation
   "Write a PAX-EXTENDED-ATTRIBUTES-ENTRY to TAR-FILE.

ATTRIBUTES must be either a hash table mapping strings to strings or an alist
mapping strings to strings. If it is an alist, ordering is preserved."))

(defgeneric write-pax-global-attributes-entry (tar-file name &rest args &key attributes)
  (:documentation
   "Write a PAX-GLOBAL-ATTRIBUTES-ENTRY to TAR-FILE.

ATTRIBUTES must be either a hash table mapping strings to strings or an alist
mapping strings to strings. If it is an alist, ordering is preserved."))

(defgeneric write-gnu-long-link-name-entry (tar-file name &rest args &key data)
  (:documentation
   "Write a GNU-LONG-LINK-NAME-ENTRY to TAR-FILE.

DATA must be either a string (which is then UTF-8 encoded) or a byte vector."))

(defgeneric write-gnu-long-name-entry (tar-file name &rest args &key data)
  (:documentation
   "Write a GNU-LONG-NAME-ENTRY to TAR-FILE.

DATA must be either a string (which is then UTF-8 encoded) or a byte vector."))
