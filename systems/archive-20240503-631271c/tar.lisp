;;;; tar.lisp -- reading and writing tar files from Common Lisp

;;; Yes, perfectly good implementations of tar already exist.  However,
;;; there are none for Common Lisp. :)  Actually, the main motivation
;;; behind writing this was to eventually provide a TAR-OP or something
;;; similar for ASDF.  This would make packaging up systems very easy.
;;; Furthermore, something like asdf-install could use this package as
;;; part of its installation routines.
;;;
;;; Implementation notes:
;;;
;;; Tar archives make ugly conflations between characters and bytes, as
;;; is typical of C programs.  This implementation deals strictly with
;;; bytes internally and will only convert to actual strings when the
;;; user requests it.  Hooks are provided for the user to customize the
;;; conversion of byte vectors to strings.  Bind
;;; *BYTEVEC-TO-STRING-CONVERSION-FUNCTION* or
;;; *STRING-TO-BYTEVEC-CONVERSION-FUNCTION* to use your own custom
;;; conversion functions.  The default functions will convert strings as
;;; ASCII (Latin-1, really), which may be good enough for you.
;;;
;;; Bugs almost certainly remain.  Regular files are the only archivable
;;; entities at the moment; attempting to back up your /dev directory
;;; with the tools contained herein will be a futile exercise.
;;;
;;; The implementation only handles ustar archives (POSIX circa 1988 or
;;; so) and does not handle pax archives (POSIX circa 2001 or so).  This
;;; is a shortcoming that I would like to see fixed, but have not yet
;;; examined what would be necessary to do so.  Archive buffer handling
;;; would likely need to be altered somewhat, as the current version is
;;; biased towards handling tar files (fixed records of 512 bytes).

(in-package :archive)


;;; constants and class definitions
(defconstant +tar-n-block-bytes+ 512)
;;; GNU tar makes this configurable, actually.
(defconstant +tar-n-record-blocks+ 20)
(defconstant +tar-n-record-bytes+ (* +tar-n-block-bytes+ +tar-n-record-blocks+))

;;; values for tar's `typeflag' field
(defconstant +tar-regular-file+ #x30)
;;; backwards compatibility
(defconstant +tar-regular-alternate-file+ #x00)
(defconstant +tar-hard-link+ #x31)
(defconstant +tar-symbolic-link+ #x32)
(defconstant +tar-character-device+ #x33)
(defconstant +tar-block-device+ #x34)
(defconstant +tar-directory-file+ #x35)
(defconstant +tar-fifo-device+ #x36)
(defconstant +tar-implementation-specific-file+ #x37)

(defconstant +posix-extended-header+ #x78)
(defconstant +posix-global-header+ #x67)

;;; non-standard typeflags
(defconstant +gnutar-long-link-name+ #x4b)
(defconstant +gnutar-long-name+ #x4c)
(defconstant +gnutar-sparse+ #x53)

(defconstant +ascii-space+ #x20)
(defconstant +ascii-zero+ #x30)
(defconstant +ascii-nine+ #x39)
(defconstant +ascii-a+ #x61)
(defconstant +ascii-z+ #x7a)

(defparameter *tar-magic-vector*
  (coerce `(,@(map 'list #'char-code "ustar") 0)
          '(vector (unsigned-byte 8))))
(defparameter *tar-version-vector*
  (coerce (map 'list #'char-code "00") '(vector (unsigned-byte 8))))

(defclass tar-archive (archive) ())

(defmethod initialize-instance :after ((instance tar-archive) &rest initargs)
  (declare (ignore initargs))
  (initialize-entry-buffer instance +tar-header-length+))


;;; Conditions

(define-condition tar-error (archive-error)
  ()
  (:documentation "All errors specific to tar archives are of this type."))

(define-condition invalid-tar-checksum-error (tar-error)
  ((provided :initarg :provided :reader provided)
   (computed :initarg :computed :reader computed))
  (:report (lambda (condition stream)
             (format stream "Invalid tar header checksum ~D (wanted ~D)"
                     (provided condition) (computed condition))))
  (:documentation "Signaled when the checksum in a tar header is invalid."))

(define-condition unhandled-error (tar-error)
  ((typeflag :initarg :typeflag :reader typeflag))
  (:documentation "Signaled when a tar entry that ARCHIVE doesn't understand is encountered."))

(define-condition unhandled-read-header-error (tar-error)
  ()
  (:report (lambda (condition stream)
             (let ((flag (typeflag condition)))
               (cond
                 ((= flag +posix-global-header+)
                  (format stream "Don't understand POSIX extended header entry"))
                 ((= flag +gnutar-sparse+)
                  (format stream "Don't understand GNU tar sparse entry"))
                 (t
                  (format stream "Can't understand typeflag: ~A" flag))))))
  (:documentation "Signaled when attempting to parse an unsupported tar entry."))

(define-condition unhandled-extract-entry-error (tar-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Don't know how to extract a type ~A tar entry yet"
                     (typeflag condition))))
  (:documentation "Signaled when attempting to extract an unsupported tar entry."))

(define-condition unhandled-write-entry-error (tar-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Don't know how to write a type ~A tar entry yet"
                     (typeflag condition))))
  (:documentation "Signaled when attempting to write an unsupported tar entry."))

;;; Groveling through tar headers

(defun read-number-from-buffer (buffer &key (start 0) end (radix 10))
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
  (declare (type (integer 2 36) radix))
  (let ((end (or (position-if #'(lambda (b)
                                  ;; For BSD tar, a number can end with
                                  ;; a space or a null byte.
                                  (or (= b +ascii-space+) (zerop b)))
                              buffer :start start :end end)
                 end
                 (length buffer))))
    ;; GNU tar permits storing numbers as binary; a binary number is
    ;; indicated by starting the field with #x80.
    (if (= (aref buffer start) #x80)
        (loop for i from (1- end) downto (1+ start)
           for base = 1 then (* base 256)
           sum (* (aref buffer i) base))
        (loop for i from (1- end) downto start
           for base = 1 then (* base radix)
           sum (let ((byte (aref buffer i)))
                 (cond
                   ((<= +ascii-zero+ byte +ascii-nine+)
                    (* base (- byte +ascii-zero+)))
                   ((<= +ascii-a+ byte +ascii-z+)
                    (* base (+ 10 (- byte +ascii-a+))))
                   (t (error "Invalid byte: ~A in ~A"
                             byte (subseq buffer start end)))))))))

(defun write-number-to-buffer (number buffer
                               &key (start 0) end (radix 10) nullp)
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
  (declare (type (integer 2 36) radix))
  (let ((end (let ((dend (or end (length buffer))))
               (if nullp
                   (1- dend)
                   dend))))
    (loop for i from (1- end) downto start
          do (multiple-value-bind (quo rem) (truncate number radix)
               (setf number quo)
               (setf (aref buffer i)
                     (cond
                       ((<= 0 rem 9) (+ rem +ascii-zero+))
                       ((<= 10 rem 36) (+ (- rem 10) +ascii-a+))
                       (t (error "Don't know how to encode ~A" rem))))))
    (values)))

(defun read-bytevec-from-buffer (buffer &key (start 0) end nullp)
  (let ((end (if nullp
                 (position 0 buffer :start start :end end)
                 end)))
    (subseq buffer start end)))

;;; translate from tar's awkward name/prefix fields into something sane
(defmethod name ((entry tar-entry))
  (let ((prefix (%prefix entry)))
    (if (or (zerop (length prefix))
            (zerop (aref prefix 0)))    ; no prefix given
        (bytevec-to-string (%name entry))
        (bytevec-to-string (concatenate '(vector (unsigned-byte 8))
                                        prefix (%name entry))))))

(defmethod (setf name) (value (entry tar-entry))
  ;;; FIXME: need to handle `PREFIX' correctly too.
  (setf (%name entry) (string-to-bytevec value))
  value)

(defmethod print-object ((entry tar-entry) stream)
  (print-unreadable-object (entry stream)
    (format stream "Tar-Entry ~A"
            (cond
              ((slot-boundp entry 'pathname)
               (namestring (entry-pathname entry)))
              (t (name entry))))))

(defmethod print-object ((entry tar-longname-entry) stream)
  (print-unreadable-object (entry stream)
    (format stream "Tar-Longname-Entry ~A"
            (cond
              ((slot-boundp entry 'pathname)
               (namestring (entry-pathname entry)))
              (t (name entry))))))

(defmethod entry-regular-file-p ((entry tar-entry))
  (eql (typeflag entry) +tar-regular-file+))

(defmethod entry-directory-p ((entry tar-entry))
  (eql (typeflag entry) +tar-directory-file+))

(defmethod entry-symbolic-link-p ((entry tar-entry))
  (eql (typeflag entry) +tar-symbolic-link+))

(defmethod entry-character-device-p ((entry tar-entry))
  (eql (typeflag entry) +tar-character-device+))

(defmethod entry-block-device-p ((entry tar-entry))
  (eql (typeflag entry) +tar-block-device+))

(defmethod entry-fifo-p ((entry tar-entry))
  (eql (typeflag entry) +tar-fifo-device+))

;;; archives

;;; internal functions of all kinds

(defun round-up-to-tar-block (num)
  (* (ceiling num +tar-n-block-bytes+) +tar-n-block-bytes+))

(defun tar-checksum-guts (block start transform-fun)
  (declare (type (simple-array (unsigned-byte 8) (*)) block))
  (let ((end (+ start +tar-n-block-bytes+))
        (checksum-start (+ start +tar-header-checksum-offset+))
        (checksum-end (+ start +tar-header-checksum-offset+
                         +tar-header-checksum-length+)))
    (loop for i from start below end
          sum (if (or (< i checksum-start) (<= checksum-end i))
                  (funcall transform-fun (aref block i))
                  +ascii-space+))))

(defun compute-checksum-for-tar-header (block start)
  (tar-checksum-guts block start #'identity))

(defun compute-old-checksum-for-tar-header (block start)
  (tar-checksum-guts block start #'(lambda (b) (if (< b 128) b (- b 256)))))

(defun tar-block-checksum-matches-p (block checksum start)
  (let ((sum (compute-checksum-for-tar-header block start)))
    (if (= sum checksum)
        t
        ;; try the older, signed arithmetic way
        (let ((signed-sum (compute-old-checksum-for-tar-header block start)))
          (values (= signed-sum checksum) sum)))))

(defun null-block-p (block start)
  (declare (type (simple-array (unsigned-byte 8) (*)) block))
  (null (position-if-not #'zerop block
			 :start start :end (+ start +tar-n-block-bytes+))))

(defparameter *modefuns-to-typeflags*
  (list (cons 'isreg +tar-regular-file+)
        (cons 'isdir +tar-directory-file+)
        (cons 'ischarfile +tar-character-device+)
        (cons 'isblockfile +tar-block-device+)
        (cons 'isfifo +tar-fifo-device+)
        (cons 'islink +tar-symbolic-link+)))

(defun typeflag-for-mode (mode)
  (loop for (modefun . typeflag) in *modefuns-to-typeflags*
        when (funcall modefun mode)
        do (return-from typeflag-for-mode typeflag)
        finally (error "No typeflag found for mode ~A" mode)))

(defmethod create-entry-from-pathname ((archive tar-archive) pathname)
  (let ((stat (stat pathname)))
    (make-instance 'tar-entry
		   :pathname pathname
		   :mode (logand +permissions-mask+
				 (stat-mode stat))
		   :typeflag (typeflag-for-mode (stat-mode stat))
		   :uid (stat-uid stat)
		   :gid (stat-gid stat)
		   :size (stat-size stat)
		   :mtime (stat-mtime stat))))

(defmethod create-entry-from-pathname :around ((archive tar-archive) pathname)
  (let ((instance (call-next-method)))
    (when (fad:directory-pathname-p pathname)
      (change-class instance 'directory-tar-entry))
    instance))
    
(defmethod write-entry-to-archive ((archive tar-archive) (entry tar-entry)
                                   &key (stream t))
  (let ((namestring (namestring (entry-pathname entry))))
    (when (and (>= (length namestring) +tar-header-%name-length+)
               (zerop (length (%name entry))))
      (let ((link-entry (make-instance 'tar-longname-entry
                                       :pathname (entry-pathname entry)
                                       :%name (convert-string-to-bytevec
                                               "././@LongLink")
                                       :uname "root"
                                       :gname "root"
                                       :typeflag +gnutar-long-name+
                                       :size (1+ (length namestring)))))
        (write-entry-to-archive archive link-entry :stream stream)))
    (call-next-method)))

(defmethod write-entry-data ((archive tar-archive)
                             (entry tar-longname-entry) stream)
  (declare (ignore stream))
  ;; This way is slow and inefficient, but obviously correct.  It is
  ;; also easy to guarantee that we're writing in 512-byte blocks.
  (let* ((namestring (namestring (entry-pathname entry)))
         (bytename (string-to-bytevec namestring))
         (entry-length (round-up-to-tar-block (1+ (length bytename))))
         (entry-buffer (make-array entry-length
                                   :element-type '(unsigned-byte 8)
                                   :initial-element 0)))
    (replace entry-buffer bytename)
    (write-sequence entry-buffer (archive-stream archive))
    (values)))

(defmethod write-entry-to-buffer ((entry tar-entry) buffer &optional (start 0))
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
  ;; ensure a clean slate
  (assert (<= (+ start +tar-n-block-bytes+) (length buffer)))
  (fill buffer 0 :start start :end (+ start +tar-n-block-bytes+))

  (cond
    ((> (length (%name entry)) 0)
     (tar-header-write-%name-to-buffer buffer start (%name entry)))
    (t
     (let* ((namestring (namestring (entry-pathname entry)))
            (bytestring (string-to-bytevec namestring)))
       (tar-header-write-%name-to-buffer buffer start
                                         (subseq bytestring 0
                                                 (min (length bytestring)
                                                      (1- +tar-header-%name-length+)))))))
  (tar-header-write-mode-to-buffer buffer start (mode entry))
  (tar-header-write-uid-to-buffer buffer start (uid entry))
  (tar-header-write-gid-to-buffer buffer start (gid entry))
  (tar-header-write-magic-to-buffer buffer start *tar-magic-vector*)
  (tar-header-write-version-to-buffer buffer start *tar-version-vector*)
  (tar-header-write-size-to-buffer buffer start (size entry))
  (tar-header-write-mtime-to-buffer buffer start (mtime entry))
  (tar-header-write-typeflag-to-buffer buffer start (typeflag entry))

  ;; the checksum is written in a peculiar fashion
  (let* ((checksum (compute-checksum-for-tar-header buffer start))
         (checksum-offset (+ start +tar-header-checksum-offset+)))
    (write-number-to-buffer checksum buffer
                            :start checksum-offset
                            :end (+ checksum-offset
                                    +tar-header-checksum-length+ -2)
                            :radix 8)
    ;; terminated with a NULL and then a space (!?)
    (setf (aref buffer (+ checksum-offset 6)) 0
          (aref buffer (+ checksum-offset 7))
          +ascii-space+)))

(defun read-tar-entry-from-buffer (buffer &key (start 0))
  (with-extracted-fields (tar-header buffer start
                                     %name mode mtime size checksum uid
                                     gid magic typeflag uname gname)
    (multiple-value-bind (validp computed)
        (tar-block-checksum-matches-p buffer checksum start)
      (unless validp
        (error 'invalid-tar-checksum-error
               :provided checksum :computed computed))
      (make-instance 'tar-entry
                     :%name %name
                     :mode mode
                     :mtime mtime
                     :size size
                     :checksum checksum
                     :uid uid
                     :gid gid
                     :magic magic
                     :typeflag typeflag
                     :uname uname
                     :gname gname))))


;;; buffering data from the archive's stream
;;;
;;; we want to do a couple of different things with buffered data:
;;;
;;; * read entries from the buffered data--requires a specific amount
;;;   of data at read time.  the data required for this operation is
;;;   discarded immediately after use.
;;; * read variable-sized data from the buffered data or stream--
;;;   requires a specific amount of data at read time.  this data must
;;;   persist after reading it from the buffer--displaced arrays could
;;;   not be used for this purpose.
;;; * transfer data from the archive's stream/data to another stream
;;;   (i.e. processing entry data).

(defmethod read-entry-from-archive ((archive tar-archive))
  (let ((entry-block (read-entry-block archive)))
    (if (null-block-p entry-block 0)
        nil
        (let ((entry (read-tar-entry-from-buffer entry-block :start 0)))
          (cond
            ((= (typeflag entry) +gnutar-long-name+)
             (let ((real-name (read-data-block archive (size entry)
                                               #'round-up-to-tar-block))
                   (entry (read-entry-from-archive archive)))
               (setf (%name entry) real-name)
               entry))
            ((= (typeflag entry) +gnutar-long-link-name+)
             (let ((real-link-name (read-data-block archive (size entry)
                                                    #'round-up-to-tar-block))
                   (entry (read-entry-from-archive archive)))
               (setf (linkname entry) real-link-name)
               entry))
            ((or (= (typeflag entry) +tar-regular-file+)
                 (= (typeflag entry) +tar-directory-file+))
             entry)
            ((= (typeflag entry) +posix-global-header+)
             ;; FIXME: We should make the information from this
             ;; available to the user.  At the moment, however, most
             ;; global headers seen in the wild are git stashing a
             ;; "comment=<sha1>".  So just ignore them for now.
             (let ((global-header (read-data-block archive (size entry)
                                                   #'round-up-to-tar-block)))
               (declare (ignore global-header))
               (read-entry-from-archive archive)))
            (t
             (error 'unhandled-read-header-error
                    :typeflag (typeflag entry))))))))
            
;;; FIXME: must add permissions handling, mtime, etc.  maybe those should
;;; be specified by flags or somesuch?
(defmethod extract-entry ((archive tar-archive) (entry tar-entry))
  ;; FIXME: this is potentially bogus
  (let ((name (merge-pathnames (name entry) *default-pathname-defaults*)))
    (cond
      ((= (typeflag entry) +tar-directory-file+)
       (ensure-directories-exist name))
      ((= (typeflag entry) +tar-regular-file+)
       (ensure-directories-exist name)
       (with-open-file (stream name :direction :output
                               :if-exists :supersede
                               :element-type '(unsigned-byte 8))
         (transfer-entry-data-to-stream archive entry stream)))
      (t
       (error 'unhandled-extract-entry-error :typeflag (typeflag entry))))))

(defmethod transfer-entry-data-to-stream ((archive tar-archive)
                                          (entry tar-entry)
                                          stream)
  (transfer-entry-data-to-stream* archive entry stream #'round-up-to-tar-block))

(defmethod discard-entry ((archive tar-archive) (entry tar-entry))
  (discard-unused-entry-data archive entry #'round-up-to-tar-block))


(defun transfer-stream-to-archive (archive stream)
  (with-slots (file-buffer (archive-stream stream)) archive
    (do ((bytes-read (read-sequence file-buffer stream)
                     (read-sequence file-buffer stream))
         (total-bytes 0 (+ total-bytes bytes-read))
         (length (length file-buffer)))
        ((< bytes-read length)
         (let* ((rounded-length (round-up-to-tar-block bytes-read))
                (total-bytes (+ total-bytes bytes-read))
                (rounded-bytes (round-up-to-tar-block total-bytes )))
           (fill file-buffer 0 :start bytes-read :end rounded-length)
           (incf (bytes-output archive) (+ rounded-bytes +tar-header-length+))
           (write-sequence file-buffer archive-stream
                           :end rounded-length)
           (values)))
      (write-sequence file-buffer archive-stream))))

;;; writing entries in various guises
(defmethod write-entry-to-archive :before ((archive tar-archive)
                                           (entry tar-entry)
                                           &key stream)
  (declare (ignore stream))
  (unless (member (typeflag entry) (list +tar-regular-file+
                                         +tar-directory-file+
                                         +gnutar-long-name+)
                  :test #'=)
    (error 'unhandled-write-entry-error :typeflag (typeflag entry))))

(defmethod finalize-archive ((archive tar-archive))
  (let ((null-block (make-array +tar-n-record-bytes+
                                :element-type '(unsigned-byte 8)
                                :initial-element 0)))
    (declare (dynamic-extent null-block))
    (write-sequence null-block (archive-stream archive)
                    :end (* +tar-header-length+ 2))
    (incf (bytes-output archive) 1024)
    (assert (zerop (mod (bytes-output archive) +tar-header-length+)))
    (multiple-value-bind (multiplier bytes-remaining)
        (ceiling (bytes-output archive) +tar-n-record-bytes+)
      (declare (ignore multiplier))
      (write-sequence null-block (archive-stream archive)
                      :end (- bytes-remaining))
      (values))))

(defun create-tar-file (pathname filelist)
  (with-open-archive (archive pathname :direction :output
                              :if-exists :supersede)
    (dolist (file filelist (finalize-archive archive))
      (let ((entry (create-entry-from-pathname archive file)))
        (write-entry-to-archive archive entry)))))
