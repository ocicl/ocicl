;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; octet-stream.lisp -- like string-streams, but with (VECTOR (UNSIGNED-BYTE 8))

(in-package :crypto)


;;; portability definitions

#+(or cmu abcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :gray-streams))

;;; TRIVIAL-GRAY-STREAMS has it, we might as well, too...
#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp #+(and allegro-version>= (not (version>= 9)))
                   'stream:stream-write-string
                   #+(and allegro-version>= (version>= 9))
                   'excl:stream-write-string)
    (require "streamc.fasl")))

#+ecl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (gray::redefine-cl-functions))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar *binary-input-stream-class*
  (quote
   #+lispworks stream:fundamental-binary-input-stream
   #+sbcl sb-gray:fundamental-binary-input-stream
   #+openmcl gray:fundamental-binary-input-stream
   #+cmu ext:fundamental-binary-input-stream
   #+allegro excl:fundamental-binary-input-stream
   #+abcl gray-streams:fundamental-binary-input-stream
   #+(or ecl clisp) gray:fundamental-binary-input-stream
   #-(or lispworks sbcl openmcl cmu allegro abcl ecl clisp)
   (error 'ironclad-error :format-control "octet streams not supported in this implementation")))

(defvar *binary-output-stream-class*
  (quote
   #+lispworks stream:fundamental-binary-output-stream
   #+sbcl sb-gray:fundamental-binary-output-stream
   #+openmcl gray:fundamental-binary-output-stream
   #+cmu ext:fundamental-binary-output-stream
   #+allegro excl:fundamental-binary-output-stream
   #+abcl gray-streams:fundamental-binary-output-stream
   #+(or ecl clisp) gray:fundamental-binary-output-stream
   #-(or lispworks sbcl openmcl cmu allegro abcl ecl clisp)
   (error 'ironclad-error :format-control "octet streams not supported in this implementation")))

;;; FIXME: how to do CMUCL support for this?
(defvar *stream-element-type-function*
  (quote
   #+lispworks cl:stream-element-type
   #+sbcl sb-gray::stream-element-type
   #+openmcl cl:stream-element-type
   #+cmu cl:stream-element-type
   #+allegro cl:stream-element-type
   #+abcl cl:stream-element-type
   #+(or ecl clisp) cl:stream-element-type
   #-(or lispworks sbcl openmcl cmu allegro abcl ecl clisp)
   (error 'ironclad-error :format-control "octet streams not supported in this implementation")))

(defvar *stream-read-byte-function*
  (quote
   #+lispworks stream:stream-read-byte
   #+sbcl sb-gray:stream-read-byte
   #+openmcl gray:stream-read-byte
   #+cmu ext:stream-read-byte
   #+allegro excl:stream-read-byte
   #+abcl gray-streams:stream-read-byte
   #+(or ecl clisp) gray:stream-read-byte
   #-(or lispworks sbcl openmcl cmu allegro abcl ecl clisp)
   (error 'ironclad-error :format-control "octet streams not supported in this implementation")))

(defvar *stream-write-byte-function*
  (quote
   #+lispworks stream:stream-write-byte
   #+sbcl sb-gray:stream-write-byte
   #+openmcl gray:stream-write-byte
   #+cmu ext:stream-write-byte
   #+allegro excl:stream-write-byte
   #+abcl gray-streams:stream-write-byte
   #+(or ecl clisp) gray:stream-write-byte
   #-(or lispworks sbcl openmcl cmu allegro abcl ecl clisp)
   (error 'ironclad-error :format-control "octet streams not supported in this implementation")))

(defvar *stream-read-sequence-function*
  (quote
   #+lispworks stream:stream-read-sequence
   #+sbcl sb-gray:stream-read-sequence
   #+openmcl ccl:stream-read-vector
   #+cmu ext:stream-read-sequence
   #+allegro excl:stream-read-sequence
   #+abcl gray-streams:stream-read-sequence
   #+(or ecl clisp) gray:stream-read-sequence
   #-(or lispworks sbcl openmcl cmu allegro abcl ecl clisp)
   (error 'ironclad-error :format-control "octet streams not supported in this implementation")))

(defvar *stream-write-sequence-function*
  (quote
   #+lispworks stream:stream-write-sequence
   #+sbcl sb-gray:stream-write-sequence
   #+openmcl ccl:stream-write-vector
   #+cmu ext:stream-write-sequence
   #+allegro excl:stream-write-sequence
   #+abcl gray-streams:stream-write-sequence
   #+(or ecl clisp) gray:stream-write-sequence
   #-(or lispworks sbcl openmcl cmu allegro abcl ecl clisp)
   (error 'ironclad-error :format-control "octet streams not supported in this implementation")))

(defvar *stream-finish-output-function*
  (quote
   #+lispworks stream:stream-finish-output
   #+sbcl sb-gray:stream-finish-output
   #+openmcl gray:stream-finish-output
   #+cmu ext:stream-finish-output
   #+allegro excl:stream-finish-output
   #+abcl gray-streams:stream-finish-output
   #+(or ecl clisp) gray:stream-finish-output
   #-(or lispworks sbcl openmcl cmu allegro abcl ecl clisp)
   (error 'ironclad-error :format-control "octet streams not supported in this implementation")))

(defvar *stream-force-output-function*
  (quote
   #+lispworks stream:stream-force-output
   #+sbcl sb-gray:stream-force-output
   #+openmcl gray:stream-force-output
   #+cmu ext:stream-force-output
   #+allegro excl:stream-force-output
   #+abcl gray-streams:stream-force-output
   #+(or ecl clisp) gray:stream-force-output
   #-(or lispworks sbcl openmcl cmu allegro abcl ecl clisp)
   (error 'ironclad-error :format-control "octet streams not supported in this implementation")))

(defvar *stream-clear-output-function*
  (quote
   #+lispworks stream:stream-clear-output
   #+sbcl sb-gray:stream-clear-output
   #+openmcl gray:stream-clear-output
   #+cmu ext:stream-clear-output
   #+allegro excl:stream-clear-output
   #+abcl gray-streams:stream-clear-output
   #+(or ecl clisp) gray:stream-clear-output
   #-(or lispworks sbcl openmcl cmu allegro abcl ecl clisp)
   (error 'ironclad-error :format-control "octet streams not supported in this implementation")))
)


;;; implementation via Gray streams

;;; These could be specialized for particular implementations by hooking
;;; in directly to the "native" stream methods for the implementation.

(defclass octet-stream ()
  ((buffer :accessor buffer :initarg :buffer :type simple-octet-vector)))

(defmethod #.*stream-element-type-function* ((stream octet-stream))
  '(unsigned-byte 8))

(defmacro define-stream-read-sequence (specializer type &body body)
  #+sbcl
  `(defmethod sb-gray:stream-read-sequence ((stream ,specializer) seq &optional (start 0) end)
     (typecase seq
       (,type
        (let ((end (or end (length seq))))
          ,@body))
       (t
        (call-next-method))))
  #+cmu
  `(defmethod ext:stream-read-sequence ((stream ,specializer) seq &optional (start 0) end)
     (typecase seq
       (,type
        (let ((end (or end (length seq))))
          ,@body))
       (t
        (call-next-method))))
  #+allegro
  `(defmethod excl:stream-read-sequence ((stream ,specializer) seq &optional (start 0) end)
     (typecase seq
       (,type
        (let ((end (or end (length seq))))
          ,@body))
       (t
        (call-next-method))))
  #+openmcl
  `(defmethod ccl:stream-read-vector ((stream ,specializer) seq start end)
     (typecase seq
       (,type
        ,@body)
       (t
        (call-next-method))))
  #+lispworks
  `(defmethod stream:stream-read-sequence ((stream ,specializer) seq start end)
     (typecase seq
       (,type
        ,@body)
       (t
        (call-next-method))))
  #+abcl
  `(defmethod gray-streams:stream-read-sequence ((stream ,specializer) seq &optional (start 0) end)
     (typecase seq
       (,type
        (let ((end (or end (length seq))))
          ,@body))
       (t
        (call-next-method))))
  #+ecl
  `(defmethod gray:stream-read-sequence ((stream ,specializer) seq &optional (start 0) end)
     (typecase seq
       (,type
        (let ((end (or end (length seq))))
          ,@body))
       (t
        (call-next-method))))
  #+clisp
  `(defmethod gray:stream-read-sequence ((stream ,specializer) seq &key (start 0) end)
     (typecase seq
       (,type
        (let ((end (or end (length seq))))
          ,@body))
       (t
        (call-next-method)))))

(defmacro define-stream-write-sequence (specializer type &body body)
  #+sbcl
  `(defmethod sb-gray:stream-write-sequence ((stream ,specializer) seq &optional (start 0) end)
     (typecase seq
       (,type
        (let ((end (or end (length seq))))
          ,@body))
       (t
        (call-next-method))))
  #+cmu
  `(defmethod ext:stream-write-sequence ((stream ,specializer) seq &optional (start 0) end)
     (typecase seq
       (,type
        (let ((end (or end (length seq))))
          ,@body))
       (t
        (call-next-method))))

  #+allegro
  (let ((stream-write-sequence 
         #+(not allegro-version>=) 'stream:stream-write-sequence
         #+(and allegro-version>= (not (version>= 9)))
         'stream:stream-write-sequence 
         #+(and allegro-version>= (version>= 9)) 'excl:stream-write-sequence))
    `(defmethod ,stream-write-sequence ((stream ,specializer) seq &optional
                                        (start 0) end)
       (typecase seq
         (,type
          (let ((end (or end (length seq))))
            ,@body))
         (t
          (call-next-method)))))

  #+openmcl
  `(defmethod ccl:stream-write-vector ((stream ,specializer) seq start end)
     (typecase seq
       (,type
        ,@body)
       (t
        (call-next-method))))
  #+lispworks
  `(defmethod stream:stream-write-sequence ((stream ,specializer) seq start end)
     (typecase seq
       (,type
        ,@body)
       (t
        (call-next-method))))
  #+abcl
  `(defmethod gray-streams:stream-write-sequence ((stream ,specializer) seq &optional (start 0) end)
     (typecase seq
       (,type
        (let ((end (or end (length seq))))
          ,@body))
       (t
        (call-next-method))))
  #+ecl
  `(defmethod gray:stream-write-sequence ((stream ,specializer) seq &optional (start 0) end)
     (typecase seq
       (,type
        (let ((end (or end (length seq))))
          ,@body))
       (t
        (call-next-method))))
  #+clisp
  `(defmethod gray:stream-write-sequence ((stream ,specializer) seq &key (start 0) end)
     (typecase seq
       (,type
        (let ((end (or end (length seq))))
          ,@body))
       (t
        (call-next-method)))))


;;; input streams

(defclass octet-input-stream (octet-stream #.*binary-input-stream-class*)
  ((index :accessor index :initarg :index :type index)
   (end :accessor end :initarg :end :type index)))

(defmethod #.*stream-read-byte-function* ((stream octet-input-stream))
  (let ((buffer (buffer stream))
        (index (index stream)))
    (declare (type simple-octet-vector buffer))
    (cond
      ((>= index (end stream)) :eof)
      (t
       (setf (index stream) (1+ index))
       (aref buffer index)))))

(define-stream-read-sequence octet-input-stream simple-octet-vector
  (let ((buffer (buffer stream))
        (index (index stream))
        (buffer-end (end stream)))
    (declare (type simple-octet-vector buffer))
    (let* ((remaining (- buffer-end index))
           (length (- end start))
           (amount (min remaining length)))
      (replace seq buffer :start1 start :end1 end
               :start2 index :end2 buffer-end)
      (setf (index stream) (+ index amount))
      (+ start amount))))

(defun make-octet-input-stream (buffer &optional (start 0) end)
  "As MAKE-STRING-INPUT-STREAM, only with octets instead of characters."
  (declare (type simple-octet-vector buffer)
           (type index start)
           (type (or index null) end))
  (let ((end (or end (length buffer))))
    (make-instance 'octet-input-stream
                   :buffer buffer :index start :end end)))

(defmacro with-octet-input-stream ((var buffer &optional (start 0) end) &body body)
  `(with-open-stream (,var (make-octet-input-stream ,buffer ,start ,end))
     ,@body))


;;; output streams

(defclass octet-output-stream (octet-stream #.*binary-output-stream-class*)
  ((index :accessor index :initform 0 :type index)))

(defmethod #.*stream-write-byte-function* ((stream octet-output-stream) integer)
  (declare (type (unsigned-byte 8) integer))
  (let* ((buffer (buffer stream))
         (length (length buffer))
         (index (index stream)))
    (declare (type simple-octet-vector buffer))
    (when (>= index (length buffer))
      (let ((new-buffer (make-array (* 2 length)
                                    :element-type '(unsigned-byte 8))))
        (declare (type simple-octet-vector new-buffer))
        (replace new-buffer buffer)
        (setf buffer new-buffer
              (buffer stream) new-buffer)))
    (setf (aref buffer index) integer
          (index stream) (1+ index))
    integer))

(define-stream-write-sequence octet-output-stream simple-octet-vector
  (let* ((buffer (buffer stream))
         (length (length buffer))
         (index (index stream))
         (amount (- end start)))
    (declare (type simple-octet-vector buffer))
    (when (>= (+ index amount) length)
      (let ((new-buffer (make-array (* 2 (max amount length))
                                    :element-type '(unsigned-byte 8))))
        (declare (type simple-octet-vector new-buffer))
        (replace new-buffer buffer)
        (setf buffer new-buffer
              (buffer stream) new-buffer)))
    (replace buffer seq :start1 index :start2 start :end2 end)
    (incf (index stream) amount)
    seq))

(defmethod #.*stream-clear-output-function* ((stream octet-output-stream))
  (setf (index stream) 0)
  nil)

(defun get-output-stream-octets (stream)
  "As GET-OUTPUT-STREAM-STRING, only with an octet output-stream instead
of a string output-stream."
  (let ((buffer (buffer stream))
        (index (index stream)))
    (setf (index stream) 0)
    (subseq buffer 0 index)))

(defun make-octet-output-stream ()
  "As MAKE-STRING-OUTPUT-STREAM, only with octets instead of characters."
  (make-instance 'octet-output-stream
                 :buffer (make-array 128 :element-type '(unsigned-byte 8))))

(defmacro with-octet-output-stream ((var) &body body)
  `(with-open-stream (,var (make-octet-output-stream))
     ,@body
     (get-output-stream-octets ,var)))


;;; digesting streams

(defclass digesting-stream (#.*binary-output-stream-class*)
  ((digest :initarg :digest :reader stream-digest)
   (buffer :initform (make-array 64 :element-type '(unsigned-byte 8))
           :reader stream-buffer)
   (position :initform 0
             :reader stream-buffer-position)))

(defmethod #.*stream-element-type-function* ((stream digesting-stream))
  '(unsigned-byte 8))

(defun make-digesting-stream (digest &rest args)
  (make-instance 'digesting-stream :digest (apply #'make-digest digest args)))

(defmethod #.*stream-write-byte-function* ((stream digesting-stream) byte)
  (declare (type (unsigned-byte 8) byte))
  (with-slots (digest buffer position) stream
    (setf (aref buffer position) byte)
    (when (= (incf position) 64)
      (update-digest digest buffer :start 0 :end 64)
      (setf position 0))
    byte))

(define-stream-write-sequence digesting-stream simple-octet-vector
  (unless (zerop (stream-buffer-position stream))
    (update-digest (stream-digest stream)
                   (stream-buffer stream)
                   :end (stream-buffer-position stream))
    (setf (slot-value stream 'position) 0))
  (update-digest (stream-digest stream) seq :start start :end end)
  seq)

(defmethod #.*stream-clear-output-function* ((stream digesting-stream))
  (with-slots (digest position) stream
    (setf position 0)
    (reinitialize-instance digest)
    nil))

(defmethod produce-digest ((stream digesting-stream)
                           &key digest (digest-start 0))
  (with-slots ((%digest digest) buffer position) stream
    (unless (zerop position)
      (update-digest %digest buffer :start 0 :end position)
      (setf position 0))
    (produce-digest %digest :digest digest :digest-start digest-start)))

(defun execute-with-digesting-stream (digest fn)
  (with-open-stream (stream (make-digesting-stream digest))
    (funcall fn stream)
    (produce-digest stream)))

(defmacro with-digesting-stream ((var digest &rest args) &body body)
  `(with-open-stream (,var (make-digesting-stream ,digest ,@args))
     ,@body
     (produce-digest ,var)))


;;; encrypting and decrypting streams

(defclass crypting-stream ()
  ((cipher :initarg :cipher :reader stream-cipher)
   (buffer :initarg :buffer :reader stream-buffer)
   (n-bytes-valid :initform 0 :reader stream-n-bytes-valid)
   (position :initform 0 :reader stream-buffer-position)
   (wrapped-stream :initarg :stream :reader stream-wrapped-stream)))

(defmethod #.*stream-element-type-function* ((stream crypting-stream))
  '(unsigned-byte 8))

(defclass encrypting-input-stream (crypting-stream #.*binary-input-stream-class*) ())
(defclass encrypting-output-stream (crypting-stream #.*binary-output-stream-class*) ())
(defclass decrypting-input-stream (crypting-stream #.*binary-input-stream-class*) ())
(defclass decrypting-output-stream (crypting-stream #.*binary-output-stream-class*) ())

(deftype stream-direction () '(member :input :output))

(defun make-encrypting-stream (stream cipher mode key &key initialization-vector (direction :output))
  (declare (type stream-direction direction))
  (unless (member mode '(ctr :ctr cfb :cfb cfb8 :cfb8 ofb :ofb stream :stream))
    (error 'ironclad-error
           :format-control "Encrypting streams support only CTR, CFB, CFB8, OFB and STREAM modes"))
  (let* ((context (make-cipher cipher :mode mode :key key
                               :initialization-vector initialization-vector))
         (block-length (max (block-length cipher) 4096))
         (buffer (make-array block-length :element-type '(unsigned-byte 8))))
    (if (eq direction :input)
        (make-instance 'encrypting-input-stream :stream stream
                       :cipher context :buffer buffer)
        (make-instance 'encrypting-output-stream :stream stream
                       :cipher context :buffer buffer))))

(defun make-decrypting-stream (stream cipher mode key &key initialization-vector (direction :input))
  (declare (type stream-direction direction))
  (unless (member mode '(ctr :ctr cfb :cfb cfb8 :cfb8 ofb :ofb stream :stream))
    (error 'ironclad-error
           :format-control "Decrypting streams support only CTR, CFB, CFB8, OFB and STREAM modes"))
  (let* ((context (make-cipher cipher :mode mode :key key
                               :initialization-vector initialization-vector))
         (block-length (max (block-length cipher) 4096))
         (buffer (make-array block-length :element-type '(unsigned-byte 8))))
    (if (eq direction :input)
        (make-instance 'decrypting-input-stream :stream stream
                       :cipher context :buffer buffer)
        (make-instance 'decrypting-output-stream :stream stream
                       :cipher context :buffer buffer))))

(defmethod #.*stream-read-byte-function* ((stream encrypting-input-stream))
  (with-slots (wrapped-stream cipher buffer n-bytes-valid position)
      stream
    (when (= position n-bytes-valid)
      (setf n-bytes-valid (read-sequence buffer wrapped-stream)
            position 0)
      (when (zerop n-bytes-valid)
        (return-from #.*stream-read-byte-function* :eof))
      (encrypt cipher buffer buffer :plaintext-end n-bytes-valid))
    (prog1 (aref buffer position)
      (incf position))))

(defmethod #.*stream-read-byte-function* ((stream decrypting-input-stream))
  (with-slots (wrapped-stream cipher buffer n-bytes-valid position)
      stream
    (when (= position n-bytes-valid)
      (setf n-bytes-valid (read-sequence buffer wrapped-stream)
            position 0)
      (when (zerop n-bytes-valid)
        (return-from #.*stream-read-byte-function* :eof))
      (decrypt cipher buffer buffer :ciphertext-end n-bytes-valid))
    (prog1 (aref buffer position)
      (incf position))))

(defmethod #.*stream-write-byte-function* ((stream encrypting-output-stream) byte)
  (declare (type (unsigned-byte 8) byte))
  (with-slots (wrapped-stream cipher buffer)
      stream
    (setf (aref buffer 0) byte)
    (encrypt cipher buffer buffer :plaintext-end 1)
    (write-byte (aref buffer 0) wrapped-stream)
    byte))

(defmethod #.*stream-write-byte-function* ((stream decrypting-output-stream) byte)
  (declare (type (unsigned-byte 8) byte))
  (with-slots (wrapped-stream cipher buffer)
      stream
    (setf (aref buffer 0) byte)
    (decrypt cipher buffer buffer :ciphertext-end 1)
    (write-byte (aref buffer 0) wrapped-stream)
    byte))

(define-stream-read-sequence encrypting-input-stream simple-octet-vector
  (with-slots (wrapped-stream cipher buffer n-bytes-valid position)
      stream
    (do ((n 0))
        ((= start end) start)
      (when (= position n-bytes-valid)
        (setf n-bytes-valid (read-sequence buffer wrapped-stream)
              position 0)
        (when (zerop n-bytes-valid)
          (return start))
        (encrypt cipher buffer buffer :plaintext-end n-bytes-valid))
      (setf n (min (- end start) (- n-bytes-valid position)))
      (replace seq buffer :start1 start :end1 end :start2 position :end2 n-bytes-valid)
      (incf start n)
      (incf position n))))

(define-stream-read-sequence decrypting-input-stream simple-octet-vector
  (with-slots (wrapped-stream cipher buffer n-bytes-valid position)
      stream
    (do ((n 0))
        ((= start end) start)
      (when (= position n-bytes-valid)
        (setf n-bytes-valid (read-sequence buffer wrapped-stream)
              position 0)
        (when (zerop n-bytes-valid)
          (return start))
        (decrypt cipher buffer buffer :ciphertext-end n-bytes-valid))
      (setf n (min (- end start) (- n-bytes-valid position)))
      (replace seq buffer :start1 start :end1 end :start2 position :end2 n-bytes-valid)
      (incf start n)
      (incf position n))))

(define-stream-write-sequence encrypting-output-stream simple-octet-vector
  (with-slots (wrapped-stream cipher buffer)
      stream
    (do ((buffer-length (length buffer))
         (length (- end start))
         (n 0))
        ((zerop length))
      (setf n (min buffer-length length))
      (encrypt cipher seq buffer :plaintext-start start :plaintext-end (+ start n))
      (write-sequence buffer wrapped-stream :end n)
      (decf length n)
      (incf start n))
    seq))

(define-stream-write-sequence decrypting-output-stream simple-octet-vector
  (with-slots (wrapped-stream cipher buffer)
      stream
    (do ((buffer-length (length buffer))
         (length (- end start))
         (n 0))
        ((zerop length))
      (setf n (min buffer-length length))
      (decrypt cipher seq buffer :ciphertext-start start :ciphertext-end (+ start n))
      (write-sequence buffer wrapped-stream :end n)
      (decf length n)
      (incf start n))
    seq))


(defmacro with-encrypting-stream ((var stream cipher mode key
                                   &key initialization-vector (direction :output))
                                  &body body)
  `(with-open-stream (,var (make-encrypting-stream ,stream ,cipher ,mode ,key
                                                   :initialization-vector ,initialization-vector
                                                   :direction ,direction))
     ,@body))

(defmacro with-decrypting-stream ((var stream cipher mode key
                                   &key initialization-vector (direction :input))
                                  &body body)
  `(with-open-stream (,var (make-decrypting-stream ,stream ,cipher ,mode ,key
                                                   :initialization-vector ,initialization-vector
                                                   :direction ,direction))
     ,@body))


;;; authenticating streams

(defclass authenticating-stream (#.*binary-output-stream-class*)
  ((mac :initarg :mac :reader stream-mac)
   (buffer :initform (make-array 64 :element-type '(unsigned-byte 8)) :reader stream-buffer)
   (position :initform 0 :reader stream-buffer-position)))

(defmethod #.*stream-element-type-function* ((stream authenticating-stream))
  '(unsigned-byte 8))

(defun make-authenticating-stream (mac key &rest args)
  (make-instance 'authenticating-stream :mac (apply #'make-mac mac key args)))

(defmethod #.*stream-write-byte-function* ((stream authenticating-stream) byte)
  (declare (type (unsigned-byte 8) byte))
  (with-slots (mac buffer position) stream
    (setf (aref buffer position) byte)
    (when (= (incf position) 64)
      (update-mac mac buffer :start 0 :end 64)
      (setf position 0))
    byte))

(define-stream-write-sequence authenticating-stream simple-octet-vector
  (unless (zerop (stream-buffer-position stream))
    (update-mac (stream-mac stream) (stream-buffer stream) :end (stream-buffer-position stream))
    (setf (slot-value stream 'position) 0))
  (update-mac (stream-mac stream) seq :start start :end end)
  seq)

(defmethod produce-mac ((stream authenticating-stream) &key digest (digest-start 0))
  (with-slots (mac buffer position) stream
    (unless (zerop position)
      (update-mac mac buffer :start 0 :end position)
      (setf position 0))
    (produce-mac mac :digest digest :digest-start digest-start)))

(defmacro with-authenticating-stream ((var mac key &rest args) &body body)
  `(with-open-stream (,var (make-authenticating-stream ,mac ,key ,@args))
     ,@body
     (produce-mac ,var)))
