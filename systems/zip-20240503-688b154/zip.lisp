;;;; Copyright (c) 2004-2006 David Lichteblau <david@lichteblau.com>
;;;; Lizenz: (L)LGPL
;;;;
;;;; Urspruenglicher Autor: David Lichteblau.
;;;; Aenderungen durch knowledgeTools GmbH.

;;;; http://www.pkware.com/business_and_developers/developer/popups/appnote.txt
;;;; (http://www.pkware.com/company/standards/appnote/)
;;;; http://www.pkware.com/documents/casestudies/APPNOTE.TXT

(in-package :zip)

(defun file-mode (pathname)
  #+(or windows mswindows)
  #o640
  #-(or windows mswindows)
  (progn
    #-(or sbcl allegro ccl)
    (error "ZIP::FILE-MODE not ported")
    #+ccl
    (multiple-value-bind (win mode size mtime inode uid blocksize rmtime gid dev)
        (ccl::%stat (ccl:native-translated-namestring pathname))
      (declare (ignore win size mtime inode uid blocksize rmtime gid dev))
      mode)
    #+sbcl
    (sb-posix:stat-mode (sb-posix:stat pathname))
    #+allegro
    (excl.osi:stat-mode (excl.osi:lstat pathname))))

(defun make-byte-array (n)
  (make-array n :element-type '(unsigned-byte 8)))

(defun get-short (array offset)
  (logior (elt array offset)
	  (ash (elt array (1+ offset)) 8)))

(defun (setf get-short) (newval array offset)
  (setf (elt array (+ offset 0)) (logand newval #xff))
  (setf newval (ash newval -8))
  (setf (elt array (+ offset 1)) (logand newval #xff))
  newval)

(defun get-int (array offset)
  (logior (elt array offset)
	  (ash (elt array (+ offset 1)) 8)
	  (ash (elt array (+ offset 2)) 16)
	  (ash (elt array (+ offset 3)) 24)))

(defun (setf get-int) (newval array offset)
  (setf (elt array (+ offset 0)) (logand newval #xff))
  (setf newval (ash newval -8))
  (setf (elt array (+ offset 1)) (logand newval #xff))
  (setf newval (ash newval -8))
  (setf (elt array (+ offset 2)) (logand newval #xff))
  (setf newval (ash newval -8))
  (setf (elt array (+ offset 3)) (logand newval #xff))
  newval)

(defmacro define-record (constructor
			 (&key (length #-clisp (gensym) #+clisp (gentemp)))
			 &rest fields)
  `(progn
     (defconstant ,length
	 ,(loop
	      for (nil type) in fields
	      sum (ecase type (:int 4) (:short 2))))
     (defun ,constructor (&optional s)
       (let ((bytes (make-byte-array ,length)))
	 (when s
           (read-sequence bytes s))
	 bytes))
     ,@(loop
	   for (name type) in fields
	   for offset = 0 then (+ offset length)
	   for length = (ecase type (:int 4) (:short 2))
	   for reader = (ecase type (:int 'get-int) (:short 'get-short))
	   unless (eq name :dummy)
	   append `((defun ,name (r)
                      (,reader r ,offset))
                    (defun (setf ,name) (newval r)
                      (setf (,reader r ,offset) newval))))))

(define-record make-end-header (:length +end-header-length+)
  (end/signature :int)
  (end/this-disc :short)
  (end/central-directory-disc :short)
  (end/disc-files :short)
  (end/total-files :short)
  (end/central-directory-size :int)
  (end/central-directory-offset :int)
  (end/comment-length :short))

(define-record make-directory-entry ()
  (cd/signature :int)
  (cd/version-made-by :short)
  (cd/version-needed-to-extract :short)
  (cd/flags :short)
  (cd/method :short)
  (cd/time :short)
  (cd/date :short)
  (cd/crc :int)
  (cd/compressed-size :int)
  (cd/size :int)
  (cd/name-length :short)
  (cd/extra-length :short)
  (cd/comment-length :short)
  (cd/disc-number :short)
  (cd/internal-attributes :short)
  (cd/external-attributes :int)
  (cd/offset :int))

(define-record make-local-header ()
  (file/signature :int)
  (file/version-needed-to-extract :short)
  (file/flags :short)
  (file/method :short)
  (file/time :short)
  (file/date :short)
  (file/crc :int)
  (file/compressed-size :int)
  (file/size :int)
  (file/name-length :short)
  (file/extra-length :short))

(define-record make-data-descriptor ()
  (data/crc :int)
  (data/compressed-size :int)
  (data/size :int))

(defun compress (input output compressor)
  (let ((nin 0)
	(nout 0)
	(crc (make-instance 'salza2:crc32-checksum)))
    (flet ((callback (buffer count)
             (write-sequence buffer output :start 0 :end count)
             (incf nout count)))
      (setf (salza2:callback compressor) #'callback)
      (let* ((input-buffer (make-array 8192 :element-type '(unsigned-byte 8))))
	(loop
	  (let ((end (read-sequence input-buffer input)))
            (cond
              ((plusp end)
                (salza2:compress-octet-vector input-buffer compressor :end end)
                (incf nin end)
                (salza2:update crc input-buffer 0 end))
              (t
                (salza2:finish-compression compressor)
                (salza2:reset compressor)
                (return (values nin nout (salza2:result crc)))))))))))

(defun store (in out)
  "Copy uncompressed bytes from IN to OUT and return values like COMPRESS."
  (let ((buf (make-array 8192
                         :initial-element 0
                         :element-type '(unsigned-byte 8)))
        (ntotal 0)
        (crc (make-instance 'salza2:crc32-checksum)))
    (loop
        for n = (read-sequence buf in :end (length buf))
        until (zerop n)
        do
          (write-sequence buf out :end n)
          (incf ntotal n)
          (salza2:update crc buf 0 n))
    (values ntotal ntotal (salza2:result crc))))

(defun seek-to-end-header (s)
  (let* ((len (+ 65536 +end-header-length+))
         (flen (progn
                 (file-position s :end)
                 (file-position s)))
	 (guess (max 0 (- flen len))))
    (file-position s guess)
    (let ((v (make-byte-array (min flen len))))
      (read-sequence v s)
      (let ((n (search #(80 75 5 6) v :from-end t)))
	(unless n
	  (error "end of central directory header not found"))
	(file-position s (+ guess n))))))

(defstruct zipfile
  stream
  entries)

(defstruct zipfile-entry
  name
  stream
  offset
  size
  compressed-size
  comment
  date
  mode)

(defstruct zipwriter
  stream
  compressor
  head
  tail)

(defstruct zipwriter-entry
  file-mode
  name
  position
  header)

(defvar *force-utf-8* nil)

(defun read-entry-object (s)
  (let* ((header (make-directory-entry s))
	 (name (make-array (cd/name-length header)
                           :element-type '(unsigned-byte 8)))
	 (comment
	  (when (plusp (cd/comment-length header))
	    (make-array (cd/comment-length header)
			:element-type '(unsigned-byte 8))))
         (utf8p (or (logtest (cd/flags header) 2048) *force-utf-8*)))
    (assert (= (cd/signature header) #x02014b50))
    (read-sequence name s)
    (setf name (decode-name name utf8p))
    (file-position s (+ (file-position s) (cd/extra-length header)))
    (when comment
      (read-sequence comment s)
      (setf comment (decode-name comment utf8p)))
    (make-zipfile-entry :name name
			:stream s
			:offset (cd/offset header)
			:size (cd/size header)
			:compressed-size (cd/compressed-size header)
			:comment comment
                        :date (cd/date header)
                        :mode (ash (cd/external-attributes header) -16))))

(defmacro with-latin1 ((&optional (enable t)) &body body)
  `(invoke-with-latin1 ,enable (lambda () ,@body)))

(defun invoke-with-latin1 (enable fn)
  (if enable
      (let (#+allegro (excl:*locale* (excl:find-locale :en_US.latin1-base))
                      #+sbcl (sb-impl::*default-external-format* :latin1))
        (funcall fn))
      (funcall fn)))

(defun open-zipfile-from-stream (stream &key force-utf-8)
  (with-latin1 ()
    (let ((*force-utf-8* force-utf-8))
      (seek-to-end-header stream)
      (let* ((end (make-end-header stream))
             (n (end/total-files end))
             (entries (make-hash-table :test #'equal))
             (zipfile (make-zipfile :stream stream
                                    :entries entries)))
        (file-position stream (end/central-directory-offset end))
        (dotimes (x n)
          (let ((entry (read-entry-object stream)))
            (setf (gethash (zipfile-entry-name entry) entries) entry)))
        zipfile))))

(defun open-zipfile (pathname &key force-utf-8)
  (with-latin1 ()
    (let* ((*force-utf-8* force-utf-8)
	   (s (open pathname :element-type '(unsigned-byte 8))))
      (unwind-protect
	   (progn
	     (seek-to-end-header s)
	     (let* ((end (make-end-header s))
		    (n (end/total-files end))
		    (entries (make-hash-table :test #'equal))
		    (zipfile (make-zipfile :stream s
					   :entries entries)))
	       (file-position s (end/central-directory-offset end))
	       (dotimes (x n)
		 (let ((entry (read-entry-object s)))
		   (setf (gethash (zipfile-entry-name entry) entries) entry)))
	       #+sbcl (let ((s s)) (sb-ext:finalize zipfile (lambda ()(close s))))
	       (setf s nil)
	       zipfile))
	(when s
	  (close s))))))

(defun close-zipfile (zipfile)
  (close (zipfile-stream zipfile)))

(defun get-zipfile-entry (name zipfile)
  (gethash name (zipfile-entries zipfile)))

(defvar *allow-cp437* nil)
(defparameter *cp437->unicode* (make-string 256))
(defparameter *unicode->cp437* (make-hash-table))
(dotimes (x 128)
  (let ((c (code-char x)))
    (setf (elt *cp437->unicode* x) c)
    (setf (gethash c *unicode->cp437*) c)))

(defun encode-name (name)
  (flet ((utf8 ()
           (values (babel:string-to-octets name :encoding :utf-8) t)))
    (if *allow-cp437*
        (let ((bytes
               (map 'vector
                 (lambda (c) (char-code (gethash c *unicode->cp437*)))
                 name)))
          (if (every #'identity bytes)
              (values (make-array (length bytes)
                                  :initial-contents bytes
                                  :element-type '(unsigned-byte 8))
                      nil)
              (utf8)))
        (utf8))))

(defun decode-name (name utf8p)
  (if utf8p
      (babel:octets-to-string name :encoding :utf-8)
      (with-output-to-string (s)
        (loop
            for c across name
            do (write-char (elt *cp437->unicode* c) s)))))

;; aus SBCL:
(loop
    for (cp437 unicode)
        :in '((#x80 #x00C7)             ; LATIN CAPITAL LETTER C WITH CEDILLA
              (#x81 #x00FC)             ; LATIN SMALL LETTER U WITH DIAERESIS
              (#x82 #x00E9)             ; LATIN SMALL LETTER E WITH ACUTE
              (#x83 #x00E2)             ; LATIN SMALL LETTER A WITH CIRCUMFLEX
              (#x84 #x00E4)             ; LATIN SMALL LETTER A WITH DIAERESIS
              (#x85 #x016F)             ; LATIN SMALL LETTER U WITH RING ABOVE
              (#x86 #x0107)             ; LATIN SMALL LETTER C WITH ACUTE
              (#x87 #x00E7)             ; LATIN SMALL LETTER C WITH CEDILLA
              (#x88 #x0142)             ; LATIN SMALL LETTER L WITH STROKE
              (#x89 #x00EB)             ; LATIN SMALL LETTER E WITH DIAERESIS
              (#x8A #x0150)             ; LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
              (#x8B #x0151)             ; LATIN SMALL LETTER O WITH DOUBLE ACUTE
              (#x8C #x00EE)             ; LATIN SMALL LETTER I WITH CIRCUMFLEX
              (#x8D #x0179)             ; LATIN CAPITAL LETTER Z WITH ACUTE
              (#x8E #x00C4)             ; LATIN CAPITAL LETTER A WITH DIAERESIS
              (#x8F #x0106)             ; LATIN CAPITAL LETTER C WITH ACUTE
              (#x90 #x00C9)             ; LATIN CAPITAL LETTER E WITH ACUTE
              (#x91 #x0139)             ; LATIN CAPITAL LETTER L WITH ACUTE
              (#x92 #x013A)             ; LATIN SMALL LETTER L WITH ACUTE
              (#x93 #x00F4)             ; LATIN SMALL LETTER O WITH CIRCUMFLEX
              (#x94 #x00F6)             ; LATIN SMALL LETTER O WITH DIAERESIS
              (#x95 #x013D)             ; LATIN CAPITAL LETTER L WITH CARON
              (#x96 #x013E)             ; LATIN SMALL LETTER L WITH CARON
              (#x97 #x015A)             ; LATIN CAPITAL LETTER S WITH ACUTE
              (#x98 #x015B)             ; LATIN SMALL LETTER S WITH ACUTE
              (#x99 #x00D6)             ; LATIN CAPITAL LETTER O WITH DIAERESIS
              (#x9A #x00DC)             ; LATIN CAPITAL LETTER U WITH DIAERESIS
              (#x9B #x0164)             ; LATIN CAPITAL LETTER T WITH CARON
              (#x9C #x0165)             ; LATIN SMALL LETTER T WITH CARON
              (#x9D #x0141)             ; LATIN CAPITAL LETTER L WITH STROKE
              (#x9E #x00D7)             ; MULTIPLICATION SIGN
              (#x9F #x010D)             ; LATIN SMALL LETTER C WITH CARON
              (#xA0 #x00E1)             ; LATIN SMALL LETTER A WITH ACUTE
              (#xA1 #x00ED)             ; LATIN SMALL LETTER I WITH ACUTE
              (#xA2 #x00F3)             ; LATIN SMALL LETTER O WITH ACUTE
              (#xA3 #x00FA)             ; LATIN SMALL LETTER U WITH ACUTE
              (#xA4 #x0104)             ; LATIN CAPITAL LETTER A WITH OGONEK
              (#xA5 #x0105)             ; LATIN SMALL LETTER A WITH OGONEK
              (#xA6 #x017D)             ; LATIN CAPITAL LETTER Z WITH CARON
              (#xA7 #x017E)             ; LATIN SMALL LETTER Z WITH CARON
              (#xA8 #x0118)             ; LATIN CAPITAL LETTER E WITH OGONEK
              (#xA9 #x0119)             ; LATIN SMALL LETTER E WITH OGONEK
              (#xAA #x00AC)             ; NOT SIGN
              (#xAB #x017A)             ; LATIN SMALL LETTER Z WITH ACUTE
              (#xAC #x010C)             ; LATIN CAPITAL LETTER C WITH CARON
              (#xAD #x015F)             ; LATIN SMALL LETTER S WITH CEDILLA
              (#xAE #x00AB)             ; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
              (#xAF #x00BB)             ; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
              (#xB0 #x2591)             ; LIGHT SHADE
              (#xB1 #x2592)             ; MEDIUM SHADE
              (#xB2 #x2593)             ; DARK SHADE
              (#xB3 #x2502)             ; BOX DRAWINGS LIGHT VERTICAL
              (#xB4 #x2524)             ; BOX DRAWINGS LIGHT VERTICAL AND LEFT
              (#xB5 #x00C1)             ; LATIN CAPITAL LETTER A WITH ACUTE
              (#xB6 #x00C2)             ; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
              (#xB7 #x011A)             ; LATIN CAPITAL LETTER E WITH CARON
              (#xB8 #x015E)             ; LATIN CAPITAL LETTER S WITH CEDILLA
              (#xB9 #x2563)             ; BOX DRAWINGS DOUBLE VERTICAL AND LEFT
              (#xBA #x2551)             ; BOX DRAWINGS DOUBLE VERTICAL
              (#xBB #x2557)             ; BOX DRAWINGS DOUBLE DOWN AND LEFT
              (#xBC #x255D)             ; BOX DRAWINGS DOUBLE UP AND LEFT
              (#xBD #x017B)             ; LATIN CAPITAL LETTER Z WITH DOT ABOVE
              (#xBE #x017C)             ; LATIN SMALL LETTER Z WITH DOT ABOVE
              (#xBF #x2510)             ; BOX DRAWINGS LIGHT DOWN AND LEFT
              (#xC0 #x2514)             ; BOX DRAWINGS LIGHT UP AND RIGHT
              (#xC1 #x2534)             ; BOX DRAWINGS LIGHT UP AND HORIZONTAL
              (#xC2 #x252C)             ; BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
              (#xC3 #x251C)             ; BOX DRAWINGS LIGHT VERTICAL AND RIGHT
              (#xC4 #x2500)             ; BOX DRAWINGS LIGHT HORIZONTAL
              (#xC5 #x253C)             ; BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
              (#xC6 #x0102)             ; LATIN CAPITAL LETTER A WITH BREVE
              (#xC7 #x0103)             ; LATIN SMALL LETTER A WITH BREVE
              (#xC8 #x255A)             ; BOX DRAWINGS DOUBLE UP AND RIGHT
              (#xC9 #x2554)             ; BOX DRAWINGS DOUBLE DOWN AND RIGHT
              (#xCA #x2569)             ; BOX DRAWINGS DOUBLE UP AND HORIZONTAL
              (#xCB #x2566)             ; BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
              (#xCC #x2560)             ; BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
              (#xCD #x2550)             ; BOX DRAWINGS DOUBLE HORIZONTAL
              (#xCE #x256C)             ; BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
              (#xCF #x00A4)             ; CURRENCY SIGN
              (#xD0 #x0111)             ; LATIN SMALL LETTER D WITH STROKE
              (#xD1 #x0110)             ; LATIN CAPITAL LETTER D WITH STROKE
              (#xD2 #x010E)             ; LATIN CAPITAL LETTER D WITH CARON
              (#xD3 #x00CB)             ; LATIN CAPITAL LETTER E WITH DIAERESIS
              (#xD4 #x010F)             ; LATIN SMALL LETTER D WITH CARON
              (#xD5 #x0147)             ; LATIN CAPITAL LETTER N WITH CARON
              (#xD6 #x00CD)             ; LATIN CAPITAL LETTER I WITH ACUTE
              (#xD7 #x00CE)             ; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
              (#xD8 #x011B)             ; LATIN SMALL LETTER E WITH CARON
              (#xD9 #x2518)             ; BOX DRAWINGS LIGHT UP AND LEFT
              (#xDA #x250C)             ; BOX DRAWINGS LIGHT DOWN AND RIGHT
              (#xDB #x2588)             ; FULL BLOCK
              (#xDC #x2584)             ; LOWER HALF BLOCK
              (#xDD #x0162)             ; LATIN CAPITAL LETTER T WITH CEDILLA
              (#xDE #x016E)             ; LATIN CAPITAL LETTER U WITH RING ABOVE
              (#xDF #x2580)             ; UPPER HALF BLOCK
              (#xE0 #x00D3)             ; LATIN CAPITAL LETTER O WITH ACUTE
              (#xE1 #x00DF)             ; LATIN SMALL LETTER SHARP S
              (#xE2 #x00D4)             ; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
              (#xE3 #x0143)             ; LATIN CAPITAL LETTER N WITH ACUTE
              (#xE4 #x0144)             ; LATIN SMALL LETTER N WITH ACUTE
              (#xE5 #x0148)             ; LATIN SMALL LETTER N WITH CARON
              (#xE6 #x0160)             ; LATIN CAPITAL LETTER S WITH CARON
              (#xE7 #x0161)             ; LATIN SMALL LETTER S WITH CARON
              (#xE8 #x0154)             ; LATIN CAPITAL LETTER R WITH ACUTE
              (#xE9 #x00DA)             ; LATIN CAPITAL LETTER U WITH ACUTE
              (#xEA #x0155)             ; LATIN SMALL LETTER R WITH ACUTE
              (#xEB #x0170)             ; LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
              (#xEC #x00FD)             ; LATIN SMALL LETTER Y WITH ACUTE
              (#xED #x00DD)             ; LATIN CAPITAL LETTER Y WITH ACUTE
              (#xEE #x0163)             ; LATIN SMALL LETTER T WITH CEDILLA
              (#xEF #x00B4)             ; ACUTE ACCENT
              (#xF0 #x00AD)             ; SOFT HYPHEN
              (#xF1 #x02DD)             ; DOUBLE ACUTE ACCENT
              (#xF2 #x02DB)             ; OGONEK
              (#xF3 #x02C7)             ; CARON
              (#xF4 #x02D8)             ; BREVE
              (#xF5 #x00A7)             ; SECTION SIGN
              (#xF6 #x00F7)             ; DIVISION SIGN
              (#xF7 #x00B8)             ; CEDILLA
              (#xF8 #x00B0)             ; DEGREE SIGN
              (#xF9 #x00A8)             ; DIAERESIS
              (#xFA #x02D9)             ; DOT ABOVE
              (#xFB #x0171)             ; LATIN SMALL LETTER U WITH DOUBLE ACUTE
              (#xFC #x0158)             ; LATIN CAPITAL LETTER R WITH CARON
              (#xFD #x0159)             ; LATIN SMALL LETTER R WITH CARON
              (#xFE #x25A0)             ; BLACK SQUARE
              (#xFF #x00A0)             ; NO-BREAK SPACE
              )
    do
      (setf (elt *cp437->unicode* cp437) (code-char unicode))
      (setf (gethash (code-char unicode) *unicode->cp437*)
            (code-char cp437)))

(defmethod write-zipentry
    (zip-writer name (data pathname)
     &key (file-write-date (file-write-date data)) (deflate t)
     (file-mode (file-mode data)))
  (with-open-file (s data :element-type '(unsigned-byte 8))
    (zip:write-zipentry zip-writer
			name
			s
			:deflate deflate
			:file-write-date file-write-date
                        :file-mode file-mode)))

(defun write-directory-zipentry
    (zip-writer name pathname
     &key (file-write-date (file-write-date pathname)) (deflate t)
     (file-mode (file-mode pathname)))
  (zip:write-zipentry zip-writer
                      name
                      (make-concatenated-stream)
                      :deflate deflate
                      :file-write-date file-write-date
                      :file-mode file-mode))

(defmethod write-zipentry
    (z name (data stream) &key (file-write-date (file-write-date data))
                               (deflate t) (file-mode #o640))
  (setf name (substitute #\/ #\\ name))
  (with-latin1 ()
    (let* ((s (zipwriter-stream z))
	   (header (make-local-header))
	   (entry (make-zipwriter-entry
                   :file-mode file-mode
		   :name name
		   :position (file-position s)
		   :header header)))
      (multiple-value-bind (encoded-name utf8p)
	  (encode-name name)
	(setf (file/signature header) #x04034b50)
	(setf (file/version-needed-to-extract header) 20) ;version 2.0
	(setf (file/flags header) (if utf8p 2048 0))
	(setf (file/method header) (if deflate 8 0))
	(multiple-value-bind (s min h d m y)
	    (decode-universal-time
	     (or file-write-date (encode-universal-time 0 0 0 1 1 1980 0)))
	  (setf (file/time header)
		(logior (ash h 11) (ash min 5) (ash s -1)))
	  (setf (file/date header)
		(logior (ash (- y 1980) 9) (ash m 5) d)))
	(setf (file/crc header) 0)
	(setf (file/compressed-size header) 0)
	(setf (file/size header) 0)
	(setf (file/name-length header) (length encoded-name))
	(setf (file/extra-length header) 0)
	(setf (zipwriter-tail z)
	      (setf (cdr (zipwriter-tail z)) (cons entry nil)))
	(write-sequence header s)
	(write-sequence encoded-name s)
	(multiple-value-bind (nin nout crc)
            (if deflate
                (compress data s (zipwriter-compressor z))
                (store data s))
          (let ((descriptor (make-data-descriptor))
                (fpos (file-position s)))
	    (setf (data/crc descriptor) crc)
	    (setf (data/compressed-size descriptor) nout)
	    (setf (data/size descriptor) nin)
	    ;; record same values for central directory
	    (setf (file/crc header) crc)
	    (setf (file/compressed-size header) nout)
	    (setf (file/size header) nin)
            (file-position s (+ (zipwriter-entry-position entry) 14))
            (write-sequence descriptor s)
            (file-position s fpos)
            #+nil(write-sequence descriptor s)
            ))
	name))))

(defun write-central-directory (z)
  (with-latin1 ()
    (let* ((s (zipwriter-stream z))
	   (pos (file-position s))
	   (n 0))
      (dolist (e (cdr (zipwriter-head z)))
	(incf n)
 	(let ((header (zipwriter-entry-header e))
	      (entry (make-directory-entry)))
	  (setf (cd/signature entry) #x02014b50)
	  (setf (cd/version-made-by entry) #x0314) ;dos compatible
	  (setf (cd/version-needed-to-extract entry)
		(file/version-needed-to-extract header))
	  (setf (cd/flags entry) (file/flags header))
	  (setf (cd/method entry) (file/method header))
	  (setf (cd/time entry) (file/time header))
	  (setf (cd/date entry) (file/date header))
	  (setf (cd/crc entry) (file/crc header))
	  (setf (cd/compressed-size entry) (file/compressed-size header))
	  (setf (cd/size entry) (file/size header))
	  (setf (cd/name-length entry) (file/name-length header))
	  (setf (cd/extra-length entry) (file/extra-length header))
	  (setf (cd/comment-length entry) 0)
	  (setf (cd/disc-number entry) 0) ;XXX ?
	  (setf (cd/internal-attributes entry) 0)
	  (setf (cd/external-attributes entry)
                (ash (zipwriter-entry-file-mode e) 16)) ;XXX directories
	  (setf (cd/offset entry) (zipwriter-entry-position e))
	  (write-sequence entry s)
	  (write-sequence (encode-name (zipwriter-entry-name e))
			  s)))
      (let ((end (make-end-header)))
	(setf (end/signature end) #x06054b50)
	(setf (end/this-disc end) 0)		;?
	(setf (end/central-directory-disc end) 0) ;?
	(setf (end/disc-files end) n)
	(setf (end/total-files end) n)
	(setf (end/central-directory-size end) (- (file-position s) pos))
	(setf (end/central-directory-offset end) pos)
	(setf (end/comment-length end) 0)
	(write-sequence end s)))))

(defun %zipfile-entry-contents (entry stream)
  (with-latin1 ()
    (let ((s (zipfile-entry-stream entry))
	  header)
      (file-position s (zipfile-entry-offset entry))
      (setf header (make-local-header s))
      (assert (= (file/signature header) #x04034b50))
      (file-position s (+ (file-position s)
			  (file/name-length header)
			  (file/extra-length header)))
      (let ((in (make-instance 'truncating-stream
			       :input-handle s
			       :size (zipfile-entry-compressed-size entry)))
	    (outbuf nil)
	    out)
	(if stream
	    (setf out stream)
	    (setf outbuf (make-byte-array (zipfile-entry-size entry))
		  out (make-buffer-output-stream outbuf)))
	(ecase (file/method header)
	  (0 (store in out))
	  (8 (inflate in out)))
	outbuf))))

(defun zipfile-entry-contents (entry &optional stream)
  (if (pathnamep stream)
      (with-open-file (s stream
			 :direction :output
			 :if-exists :supersede
                         :element-type '(unsigned-byte 8))
	(%zipfile-entry-contents entry s))
      (%zipfile-entry-contents entry stream)))

(defmacro with-zipfile ((file pathname &key force-utf-8) &body body)
  `(let ((,file (open-zipfile ,pathname :force-utf-8 ,force-utf-8)))
     (unwind-protect
	 (progn ,@body)
       (close-zipfile ,file))))

(defmacro with-zipfile-stream ((file stream &key force-utf-8) &body body)
  `(let ((,file (open-zipfile-from-stream ,stream :force-utf-8 ,force-utf-8)))
     ,@body))

(defun make-zipfile-writer (pathname &key (if-exists :error))
  (with-latin1 ()
    (let ((c (cons nil nil)))
      (make-zipwriter
       :stream (open pathname
                     :direction :output
                     :if-exists if-exists
                     :element-type '(unsigned-byte 8))
       :compressor (make-instance 'salza2:deflate-compressor)
       :head c
       :tail c))))

(defun close-zipfile-writer (z)
  (write-central-directory z)
  (close (zipwriter-stream z)))

(defmacro with-output-to-zipfile
    ((var pathname &key (if-exists :error)) &body body)
  `(let ((,var (make-zipfile-writer ,pathname :if-exists ,if-exists)))
     (unwind-protect
	 (progn ,@body)
       (close-zipfile-writer ,var))))

(defmacro do-zipfile-entries ((name entry zipfile) &body body)
  (setf name (or name (gensym)))
  (setf entry (or entry (gensym)))
  `(block nil
     (maphash (lambda (,name ,entry)
		(declare (ignorable ,name ,entry))
		,@body)
	      (zipfile-entries ,zipfile))))

(defun unzip (pathname target-directory &key (if-exists :error) verbose force-utf-8)
  ;; <Xof> "When reading[1] the value of any pathname component, conforming
  ;;       programs should be prepared for the value to be :unspecific."
  (when (set-difference (list (pathname-name target-directory)
                              (pathname-type target-directory))
                        '(nil :unspecific))
    (error "pathname not a directory, lacks trailing slash?"))
  (with-zipfile (zip pathname :force-utf-8 force-utf-8)
    (do-zipfile-entries (name entry zip)
      (let* (#+nil (name (ppcre:regex-replace-all "[/*?]" name "_"))
             #+nil (name (subseq name 0 (min (length name) 128)))
             (filename (merge-pathnames name target-directory)))
        (ensure-directories-exist filename)
        (unless (char= (elt name (1- (length name))) #\/)
          (ecase verbose
            ((nil))
            ((t) (write-string name) (terpri))
            (:dots (write-char #\.)))
          (force-output)
          (with-open-file
              (s filename :direction :output :if-exists if-exists
               :element-type '(unsigned-byte 8))
            (zipfile-entry-contents entry s)))))))

(defun %directory-sorted (dir)
  (sort (fad:list-directory dir) #'string<
        :key #+sbcl #'sb-impl::native-namestring #-sbcl #'namestring))

(defun %file-write-date (f)
  #+clisp (posix:file-stat-mtime (posix:file-stat f))
  #-clisp (file-write-date f))

(defun %directory-namestring (d)
  #+clisp (directory-namestring
	   (truename (concatenate 'string (princ-to-string d) "/")))
  #-clisp (make-pathname
	   :host (pathname-host d)
	   :device (pathname-device d)
	   :defaults (directory-namestring d)))

(defun fix-enough-namestring (pathname &optional defaults)
  (let ((x (enough-namestring pathname defaults)))
    #+sbcl (sb-impl::native-namestring (pathname x))
    #-sbcl x))

(defun zip (pathname source-directory &key (if-exists :error) skip-directories-p)
  (let ((base (%directory-namestring (merge-pathnames source-directory))))
    (with-output-to-zipfile (zip pathname :if-exists if-exists)
      (labels ((recurse (d)
                 (dolist (f (%directory-sorted d))
                   (cond
                     ((fad:directory-pathname-p f)
                       (unless skip-directories-p
			 (write-directory-zipentry
                          zip (fix-enough-namestring f base) f))
                       (recurse (fad:pathname-as-directory f)))
                     ((or (pathname-name f) (pathname-type f))
                       (with-open-file (s f :element-type '(unsigned-byte 8))
                         (write-zipentry
                          zip
                          (fix-enough-namestring f base)
                          s :file-write-date (%file-write-date f)
                          :file-mode (file-mode f))))))))
        (recurse (fad:pathname-as-directory source-directory))))))

(defun rezip
    (output-pathname reference-pathname source-directory
     &key (if-exists :error))
  (with-zipfile (ref reference-pathname)
    (let ((entries '())
	  (base source-directory))
      (do-zipfile-entries (name entry ref)
	(push entry entries))
      (setf entries (sort entries #'< :key #'zipfile-entry-offset))
      (with-output-to-zipfile (out output-pathname :if-exists if-exists)
	(dolist (entry entries)
	  (let ((f (merge-pathnames (zipfile-entry-name entry) base)))
	    (if (fad:directory-pathname-p f)
		(write-directory-zipentry
		 out (fix-enough-namestring f base) f)
		(with-open-file (s f :element-type '(unsigned-byte 8))
		  (write-zipentry
		   out
		   (zipfile-entry-name entry)
		   s
		   :file-write-date (zipfile-entry-date entry)
		   :file-mode (zipfile-entry-mode entry))))))))))
