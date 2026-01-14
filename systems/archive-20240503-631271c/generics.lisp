(in-package :archive)


(defgeneric (setf name) (value entry)
  (:documentation "Sets the name of ENTRY to VALUE."))

(defgeneric entry-regular-file-p (entry)
  (:documentation "Returns T if ENTRY denotes a regular file."))

(defgeneric entry-directory-p (entry)
  (:documentation "Returns T if ENTRY denotes a directory."))

(defgeneric entry-symbolic-link-p (entry)
  (:documentation "Returns T if ENTRY denotes a symbolic link."))

(defgeneric entry-character-device-p (entry)
  (:documentation "Returns T if ENTRY denotes a character device."))

(defgeneric entry-block-device-p (entry)
  (:documentation "Returns T if ENTRY denotes a block device."))

(defgeneric entry-fifo-p (entry)
  (:documentation "Returns T if ENTRY denotes a fifo."))


;;; reading

(defgeneric read-entry-from-archive (archive)
  (:documentation "Return the next entry in ARCHIVE or NIL if there is no
next entry"))

(defgeneric extract-entry (archive entry)
  (:documentation "Recreate the file represented by ENTRY in ARCHIVE as
an actual file on disk.  This file is created relative to
*DEFAULT-PATHNAME-DEFAULTS*"))

(defgeneric discard-entry (archive entry)
  (:documentation "Advance ARCHIVE's internal state past the end of
ENTRY's data.  Further reads from ENTRY's stream will return EOF."))

(defgeneric transfer-entry-data-to-stream (archive entry stream)
  (:documentation "Write the data in ARCHIVE associated with ENTRY into
STREAM."))


;;; writing

(defgeneric create-entry-from-pathname (archive pathname)
  (:documentation "Create an ENTRY that can be written to ARCHIVE, using
metadata and the name of FILENAME."))

(defgeneric write-entry-to-archive (archive entry
                                            &key stream)
  (:documentation "Write ENTRY to ARCHIVE.  gData associated with ENTRY
is written to ARCHIVE according to the :STREAM argument.  If :STREAM is
T, the expression (NAME ENTRY) is expected to refer to an existing file
from which data may be read.  If :STREAM is a stream, then data is read
from that stream and written to ARCHIVE.  If :STREAM is NIL, then no
entry data is written."))

(defgeneric write-entry-to-buffer (entry buffer &optional start)
  (:documentation "Write the information associated with ENTRY into BUFFER,
beginning at position START."))

(defgeneric write-entry-data (archive entry stream)
  (:documentation "Write any data associated with ENTRY, possibly found
in STREAM to ARCHIVE; called after WRITE-ENTRY-TO-BUFFER.  STREAM is
interpreted as in WRITE-ENTRY-TO-ARCHIVE."))

(defgeneric finalize-archive (archive)
  (:documentation "Perform any necessary processing for finalizing ARCHIVE.
This function must be called prior to calling CLOSE-ARCHIVE."))

