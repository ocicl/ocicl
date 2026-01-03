;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; modes.lisp -- using encryption modes with block ciphers

(in-package :crypto)

(defclass encryption-mode ()
  ((encrypt-function :reader encrypt-function)
   (decrypt-function :reader decrypt-function)))
(defclass padded-mode ()
  ((padding :accessor padding :initform nil)))
(defclass ecb-mode (encryption-mode padded-mode) ())
(defclass stream-mode (encryption-mode) ())
(defclass inititialization-vector-mixin ()
  ((iv :reader iv :initarg :initialization-vector)
   (position :accessor iv-position :initform 0)))
(defclass cbc-mode (encryption-mode inititialization-vector-mixin padded-mode) ())
(defclass ofb-mode (encryption-mode inititialization-vector-mixin) ())
(defclass cfb-mode (encryption-mode inititialization-vector-mixin) ())
(defclass cfb8-mode (encryption-mode inititialization-vector-mixin) ())
(defclass ctr-mode (encryption-mode inititialization-vector-mixin)
  ((keystream-blocks :accessor keystream-blocks :initform 0 :type (integer 0 *))))

(defmethod print-object ((object encryption-mode) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "~A" (class-name (class-of object)))))

(defmethod initialize-instance :after ((mode encryption-mode) &key cipher padding)
  (when (typep mode 'padded-mode)
    (case padding
      ((:pkcs7 pkcs7)
       (setf (padding mode) (make-instance 'pkcs7-padding)))
      ((:ansi-x923 ansi-x923)
       (setf (padding mode) (make-instance 'ansi-x923-padding)))
      ((:iso-7816-4 iso-7816-4)
       (setf (padding mode) (make-instance 'iso-7816-4-padding)))
      ((nil)
       (setf (padding mode) nil))
      (t
       (error 'unsupported-padding :name padding))))
  (multiple-value-bind (efun dfun) (mode-crypt-functions cipher mode)
    (setf (slot-value mode 'encrypt-function) efun
          (slot-value mode 'decrypt-function) dfun)))

(defvar *supported-modes* (list :ecb :cbc :ofb :cfb :cfb8 :ctr))

(defun mode-supported-p (name)
  (and (symbolp name)
       (not (null (member (intern (symbol-name name) :keyword)
                          *supported-modes*)))))

(defun list-all-modes ()
  (sort (copy-seq *supported-modes*) #'string<))

(defmethod encrypt-message (cipher message &key (start 0) (end (length message)) &allow-other-keys)
  (let* ((length (- end start))
         (encrypted-length (encrypted-message-length cipher (mode cipher)
                                                     length t))
         (encrypted-message (make-array encrypted-length
                                        :element-type '(unsigned-byte 8))))
    (encrypt cipher message encrypted-message
             :plaintext-start start :plaintext-end end
             :handle-final-block t)
    encrypted-message))

(defmethod decrypt-message (cipher message &key (start 0) (end (length message)) &allow-other-keys)
  (let* ((length (- end start))
         (decrypted-message (make-array length :element-type '(unsigned-byte 8))))
    (multiple-value-bind (bytes-consumed bytes-produced)
        (decrypt cipher message decrypted-message
                 :ciphertext-start start :ciphertext-end end
                 :handle-final-block t)
      (declare (ignore bytes-consumed))
      (if (< bytes-produced length)
          (subseq decrypted-message 0 bytes-produced)
          decrypted-message))))

(defun increment-counter-block (block n)
  (declare (type simple-octet-vector block)
           (type (mod #.most-positive-fixnum) n)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (loop with carry of-type (mod #.most-positive-fixnum) = n
        with sum of-type (unsigned-byte 16) = 0
        for i of-type fixnum from (1- (length block)) downto 0
        do (setf sum (+ (aref block i) (logand carry #xff))
                 (aref block i) (logand sum #xff)
                 carry (+ (ash carry -8) (ash sum -8)))
        until (zerop carry)))

(declaim (inline increment-counter-block-1))
(defun increment-counter-block-1 (size block)
  (declare (type index size)
           (type simple-octet-vector block)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  #+(and sbcl (or x86 x86-64) ironclad-assembly)
  (inc-counter-block size block)
  #-(and sbcl (or x86 x86-64) ironclad-assembly)
  (loop with sum of-type (unsigned-byte 16) = 1
        for i of-type fixnum from (1- size) downto 0
        do (setf sum (+ (aref block i) sum)
                 (aref block i) (logand sum #xff)
                 sum (ash sum -8))
        until (zerop sum)))

(defun decrement-counter-block (block n)
  (declare (type simple-octet-vector block)
           (type (mod #.most-positive-fixnum) n)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (loop with carry of-type (mod #.most-positive-fixnum) = n
        with sub of-type fixnum = 0
        for i of-type fixnum from (1- (length block)) downto 0
        do (setf sub (- (aref block i) (logand carry #xff))
                 (aref block i) (logand sub #xff)
                 carry (+ (ash carry -8) (if (minusp sub) 1 0)))
        until (zerop carry)))

;;; This way is kind of ugly, but I don't know a better way.
(macrolet ((define-mode-function (&rest mode-definition-funs &environment env)
             (loop for fun in mode-definition-funs
                   collect (macroexpand `(,fun 128-byte-block-mixin 128) env) into forms
                   collect (macroexpand `(,fun 64-byte-block-mixin 64) env) into forms
                   collect (macroexpand `(,fun 32-byte-block-mixin 32) env) into forms
                   collect (macroexpand `(,fun 16-byte-block-mixin 16) env) into forms
                   collect (macroexpand `(,fun 8-byte-block-mixin 8) env) into forms
                   finally (return `(progn ,@forms))))
           (mode-lambda (&body body)
             `(lambda (in out in-start in-end out-start handle-final-block)
                (declare (type simple-octet-vector in out))
                (declare (type index in-start in-end out-start))
                (declare (ignorable handle-final-block))
                (declare (optimize (speed 3) (space 0) (debug 0)))
                ,@body)))


;;; ECB mode

(macrolet ((mode-crypt (cipher-specializer block-length-expr)
             `(defmethod mode-crypt-functions ((cipher ,cipher-specializer)
                                               (mode ecb-mode))
                (let ((efun (encrypt-function cipher))
                      (dfun (decrypt-function cipher))
                      (padding (padding mode)))
                 (declare (type function efun dfun))
                  (values
                   (mode-lambda
                    (let ((offset in-start))
                      (declare (type index offset))
                      (loop with end = (- in-end ,block-length-expr)
                            while (<= offset end)
                            do (funcall efun cipher in offset out out-start)
                               (incf offset ,block-length-expr)
                               (incf out-start ,block-length-expr))
                      (let ((n-bytes-processed (- offset in-start)))
                        (declare (type index n-bytes-processed))
                        (if (and handle-final-block padding)
                            (let ((n-bytes-remaining (- in-end offset)))
                              (declare (type index n-bytes-remaining))
                              (when (< (- (length out) out-start) ,block-length-expr)
                                (error 'insufficient-buffer-space
                                       :buffer out
                                       :start out-start
                                       :length ,block-length-expr))
                              (replace out in
                                       :start1 out-start
                                       :start2 offset :end2 in-end)
                              (add-padding-bytes padding out out-start
                                                 n-bytes-remaining ,block-length-expr)
                              (funcall efun cipher out out-start out out-start)
                              (values (+ n-bytes-processed n-bytes-remaining)
                                      (+ n-bytes-processed ,block-length-expr)))
                            (values n-bytes-processed n-bytes-processed)))))
                   (mode-lambda
                    (let ((temp-block (make-array ,block-length-expr
                                                  :element-type '(unsigned-byte 8)))
                          (offset in-start))
                      (declare (type (simple-octet-vector ,block-length-expr) temp-block))
                      (declare (dynamic-extent temp-block))
                      (declare (type index offset))
                      (loop with end = (if (and handle-final-block padding)
                                           (- in-end (* 2 ,block-length-expr))
                                           (- in-end ,block-length-expr))
                            while (<= offset end)
                            do (funcall dfun cipher in offset out out-start)
                               (incf offset ,block-length-expr)
                               (incf out-start ,block-length-expr))
                      (let ((n-bytes-processed (- offset in-start)))
                        (declare (type index n-bytes-processed))
                        (if (and handle-final-block
                                 padding
                                 (= (- in-end offset) ,block-length-expr))
                            (let ((n-bytes-remaining 0))
                              (declare (type index n-bytes-remaining))
                              (funcall dfun cipher in offset temp-block 0)
                              (setf n-bytes-remaining (- ,block-length-expr
                                                         (count-padding-bytes padding temp-block
                                                                              0 ,block-length-expr)))
                              (replace out temp-block
                                       :start1 out-start
                                       :start2 0 :end2 n-bytes-remaining)
                              (values (+ n-bytes-processed ,block-length-expr)
                                      (+ n-bytes-processed n-bytes-remaining)))
                            (values n-bytes-processed n-bytes-processed)))))))))
           (message-length (cipher-specializer block-length-expr)
             `(defmethod encrypted-message-length ((cipher ,cipher-specializer)
                                                   (mode ecb-mode) length
                                                   &optional handle-final-block)
                (let ((encrypted-length (* (truncate length ,block-length-expr) ,block-length-expr)))
                  (if (and handle-final-block (padding mode))
                      (+ encrypted-length ,block-length-expr)
                      encrypted-length)))))
  (define-mode-function mode-crypt message-length))


;;; CBC mode

(macrolet ((mode-crypt (cipher-specializer block-length-expr)
             `(defmethod mode-crypt-functions ((cipher ,cipher-specializer)
                                               (mode cbc-mode))
                (let ((efun (encrypt-function cipher))
                      (dfun (decrypt-function cipher))
                      (iv (iv mode))
                      (padding (padding mode)))
                  (declare (type function efun dfun))
                  (declare (type (simple-octet-vector ,block-length-expr) iv))
                  (declare (inline xor-block))
                  (declare (inline copy-block))
                  (values
                   (mode-lambda
                    (let ((offset in-start))
                      (declare (type index offset))
                      (loop with end = (- in-end ,block-length-expr)
                            while (<= offset end)
                            do (xor-block ,block-length-expr iv 0 in offset out out-start)
                               (funcall efun cipher out out-start out out-start)
                               (copy-block ,block-length-expr out out-start iv 0)
                               (incf offset ,block-length-expr)
                               (incf out-start ,block-length-expr))
                      (let ((n-bytes-processed (- offset in-start)))
                        (declare (type index n-bytes-processed))
                        (if (and handle-final-block padding)
                            (let ((n-bytes-remaining (- in-end offset)))
                              (declare (type index n-bytes-remaining))
                              (when (< (- (length out) out-start) ,block-length-expr)
                                (error 'insufficient-buffer-space
                                       :buffer out
                                       :start out-start
                                       :length ,block-length-expr))
                              (replace out in
                                       :start1 out-start
                                       :start2 offset :end2 in-end)
                              (add-padding-bytes padding out out-start
                                                 n-bytes-remaining ,block-length-expr)
                              (xor-block ,block-length-expr iv 0 out out-start out out-start)
                              (funcall efun cipher out out-start out out-start)
                              (copy-block ,block-length-expr out out-start iv 0)
                              (values (+ n-bytes-processed n-bytes-remaining)
                                      (+ n-bytes-processed ,block-length-expr)))
                            (values n-bytes-processed n-bytes-processed)))))
                   (mode-lambda
                    (let ((temp-block (make-array ,block-length-expr
                                                  :element-type '(unsigned-byte 8)))
                          (offset in-start))
                      (declare (type (simple-octet-vector ,block-length-expr) temp-block))
                      (declare (dynamic-extent temp-block))
                      (declare (type index offset))
                      (loop with end = (if (and handle-final-block padding)
                                           (- in-end (* 2 ,block-length-expr))
                                           (- in-end ,block-length-expr))
                            while (<= offset end)
                            do (copy-block ,block-length-expr in offset temp-block 0)
                               (funcall dfun cipher in offset out out-start)
                               (xor-block ,block-length-expr iv 0 out out-start out out-start)
                               (copy-block ,block-length-expr temp-block 0 iv 0)
                               (incf offset ,block-length-expr)
                               (incf out-start ,block-length-expr))
                      (let ((n-bytes-processed (- offset in-start)))
                        (declare (type index n-bytes-processed))
                        (if (and handle-final-block
                                 padding
                                 (= (- in-end offset) ,block-length-expr))
                            (let ((n-bytes-remaining 0))
                              (declare (type index n-bytes-remaining))
                              (funcall dfun cipher in offset temp-block 0)
                              (xor-block ,block-length-expr iv 0 temp-block 0 temp-block 0)
                              (setf n-bytes-remaining (- ,block-length-expr
                                                         (count-padding-bytes padding temp-block
                                                                              0 ,block-length-expr)))
                              (replace out temp-block
                                       :start1 out-start
                                       :start2 0 :end2 n-bytes-remaining)
                              (values (+ n-bytes-processed ,block-length-expr)
                                      (+ n-bytes-processed n-bytes-remaining)))
                            (values n-bytes-processed n-bytes-processed)))))))))
           (message-length (cipher-specializer block-length-expr)
             `(defmethod encrypted-message-length ((cipher ,cipher-specializer)
                                                   (mode cbc-mode) length
                                                   &optional handle-final-block)
                (let ((encrypted-length (* (truncate length ,block-length-expr) ,block-length-expr)))
                  (if (and handle-final-block (padding mode))
                      (+ encrypted-length ,block-length-expr)
                      encrypted-length)))))
  (define-mode-function mode-crypt message-length))


;;; CFB mode

(macrolet ((mode-crypt (cipher-specializer block-length-expr)
             `(defmethod mode-crypt-functions ((cipher ,cipher-specializer)
                                               (mode cfb-mode))
                (let ((function (encrypt-function cipher))
                      (iv (iv mode))
                      (iv-position (iv-position mode)))
                  (declare (type function function))
                  (declare (type (simple-octet-vector ,block-length-expr) iv))
                  (declare (type (integer 0 (,block-length-expr)) iv-position))
                  (values
                   (mode-lambda
                    (let ((remaining (- in-end in-start))
                          (offset in-start))
                      (declare (type index remaining offset))

                      ;; Use remaining bytes in iv
                      (loop until (or (zerop iv-position) (zerop remaining)) do
                        (let ((b (logxor (aref in offset) (aref iv iv-position))))
                          (declare (type (unsigned-byte 8) b))
                          (setf (aref out out-start) b
                                (aref iv iv-position) b
                                iv-position (mod (1+ iv-position) ,block-length-expr))
                          (incf offset)
                          (incf out-start)
                          (decf remaining)))

                      ;; Process data by block
                      (multiple-value-bind (q r)
                          (truncate remaining ,block-length-expr)
                        (dotimes (i q)
                          (funcall function cipher iv 0 iv 0)
                          (xor-block ,block-length-expr iv 0 in offset iv 0)
                          (copy-block ,block-length-expr iv 0 out out-start)
                          (incf offset ,block-length-expr)
                          (incf out-start ,block-length-expr))
                        (setf remaining r))

                      ;; Process remaing bytes of data
                      (loop until (zerop remaining) do
                        (when (zerop iv-position)
                          (funcall function cipher iv 0 iv 0))
                        (let ((b (logxor (aref in offset) (aref iv iv-position))))
                          (declare (type (unsigned-byte 8) b))
                          (setf (aref out out-start) b
                                (aref iv iv-position) b
                                iv-position (mod (1+ iv-position) ,block-length-expr))
                          (incf offset)
                          (incf out-start)
                          (decf remaining)))

                      (let ((processed (- offset in-start)))
                        (values processed processed))))
                   (mode-lambda
                    (let ((temp-block (make-array ,block-length-expr
                                                  :element-type '(unsigned-byte 8)))
                          (remaining (- in-end in-start))
                          (offset in-start))
                      (declare (type (simple-octet-vector ,block-length-expr) temp-block)
                               (dynamic-extent temp-block)
                               (type index remaining offset))

                      ;; Use remaining bytes in iv
                      (loop until (or (zerop iv-position) (zerop remaining)) do
                        (let ((b (aref in offset)))
                          (declare (type (unsigned-byte 8) b))
                          (setf (aref out out-start) (logxor b (aref iv iv-position))
                                (aref iv iv-position) b
                                iv-position (mod (1+ iv-position) ,block-length-expr))
                          (incf offset)
                          (incf out-start)
                          (decf remaining)))

                      ;; Process data by block
                      (multiple-value-bind (q r)
                          (truncate remaining ,block-length-expr)
                        (dotimes (i q)
                          (funcall function cipher iv 0 temp-block 0)
                          (copy-block ,block-length-expr in offset iv 0)
                          (xor-block ,block-length-expr temp-block 0 in offset out out-start)
                          (incf offset ,block-length-expr)
                          (incf out-start ,block-length-expr))
                        (setf remaining r))

                      ;; Process remaing bytes of data
                      (loop until (zerop remaining) do
                        (when (zerop iv-position)
                          (funcall function cipher iv 0 iv 0))
                        (let ((b (aref in offset)))
                          (declare (type (unsigned-byte 8) b))
                          (setf (aref out out-start) (logxor b (aref iv iv-position))
                                (aref iv iv-position) b
                                iv-position (mod (1+ iv-position) ,block-length-expr))
                          (incf offset)
                          (incf out-start)
                          (decf remaining)))

                      (let ((processed (- offset in-start)))
                        (values processed processed))))))))
           (message-length (cipher-specializer block-length-expr)
             (declare (ignore block-length-expr))
             `(defmethod encrypted-message-length ((cipher ,cipher-specializer)
                                                   (mode cfb-mode) length
                                                   &optional handle-final-block)
                (declare (ignore handle-final-block))
                ;; We can encrypt the whole thing.
                length)))
  (define-mode-function mode-crypt message-length))


;;; CFB8 mode

(macrolet ((mode-crypt (cipher-specializer block-length-expr)
           `(defmethod mode-crypt-functions ((cipher ,cipher-specializer)
                                             (mode cfb8-mode))
              (let ((function (encrypt-function cipher))
                    (iv (iv mode))
                    (encrypted-iv (make-array ,block-length-expr :element-type '(unsigned-byte 8))))
                (declare (type function function))
                (declare (type (simple-octet-vector ,block-length-expr) iv encrypted-iv))
                (declare (inline copy-block))
                (values
                  (mode-lambda
                   (loop for i of-type index from in-start below in-end
                         for j of-type index from out-start
                         do (copy-block ,block-length-expr iv 0 encrypted-iv 0)
                            (funcall function cipher encrypted-iv 0 encrypted-iv 0)
                            (let ((b (logxor (aref in i) (aref encrypted-iv 0))))
                              (setf (aref out j) b)
                              (replace iv iv :start1 0 :start2 1
                                       :end1 (1- ,block-length-expr) :end2 ,block-length-expr)
                              (setf (aref iv (1- ,block-length-expr)) b))
                         finally (return
                                   (let ((n-bytes-processed (- in-end in-start)))
                                     (values n-bytes-processed n-bytes-processed)))))
                  (mode-lambda
                   (loop for i of-type index from in-start below in-end
                         for j of-type index from out-start
                         do (copy-block ,block-length-expr iv 0 encrypted-iv 0)
                            (funcall function cipher encrypted-iv 0 encrypted-iv 0)
                            (replace iv iv :start1 0 :start2 1
                                     :end1 (1- ,block-length-expr) :end2 ,block-length-expr)
                            (let ((b (aref in i)))
                              (setf (aref iv (1- ,block-length-expr)) b)
                              (setf (aref out j) (logxor b (aref encrypted-iv 0))))
                         finally (return
                                   (let ((n-bytes-processed (- in-end in-start)))
                                     (values n-bytes-processed n-bytes-processed)))))))))
           (message-length (cipher-specializer block-length-expr)
             (declare (ignore block-length-expr))
             `(defmethod encrypted-message-length ((cipher ,cipher-specializer)
                                                   (mode cfb8-mode) length
                                                   &optional handle-final-block)
                (declare (ignore handle-final-block))
                ;; We can encrypt the whole thing.
                length)))
  (define-mode-function mode-crypt message-length))


;;; OFB mode

(macrolet ((mode-crypt (cipher-specializer block-length-expr)
             `(defmethod mode-crypt-functions ((cipher ,cipher-specializer)
                                               (mode ofb-mode))
                (let ((iv (iv mode))
                      (iv-position (iv-position mode)))
                  (declare (type (simple-octet-vector ,block-length-expr) iv))
                  (declare (type (integer 0 (,block-length-expr)) iv-position))
                  (flet ((ofb-crypt-function (function)
                           (declare (type function function))
                           (mode-lambda
                            (let ((remaining (- in-end in-start))
                                  (offset in-start))
                              (declare (type index remaining offset))

                              ;; Use remaining bytes in iv
                              (loop until (or (zerop iv-position) (zerop remaining)) do
                                (setf (aref out out-start)
                                      (logxor (aref in offset) (aref iv iv-position)))
                                (setf iv-position (mod (1+ iv-position) ,block-length-expr))
                                (incf offset)
                                (incf out-start)
                                (decf remaining))

                              ;; Process data by block
                              (multiple-value-bind (q r)
                                  (truncate remaining ,block-length-expr)
                                (dotimes (i q)
                                  (funcall function cipher iv 0 iv 0)
                                  (xor-block ,block-length-expr iv 0 in offset out out-start)
                                  (incf offset ,block-length-expr)
                                  (incf out-start ,block-length-expr))
                                (setf remaining r))

                              ;; Process remaing bytes of data
                              (loop until (zerop remaining) do
                                (when (zerop iv-position)
                                  (funcall function cipher iv 0 iv 0))
                                (setf (aref out out-start)
                                      (logxor (aref in offset) (aref iv iv-position)))
                                (setf iv-position (mod (1+ iv-position) ,block-length-expr))
                                (incf offset)
                                (incf out-start)
                                (decf remaining))

                              (let ((processed (- offset in-start)))
                                (values processed processed))))))
                    (let ((f (ofb-crypt-function (encrypt-function cipher))))
                      (values f f))))))
           (message-length (cipher-specializer block-length-expr)
             (declare (ignore block-length-expr))
             `(defmethod encrypted-message-length ((cipher ,cipher-specializer)
                                                   (mode ofb-mode) length
                                                   &optional handle-final-block)
                (declare (ignore handle-final-block))
                ;; We can encrypt the whole thing.
                length)))
  (define-mode-function mode-crypt message-length))


;;; CTR mode

(macrolet ((mode-crypt (cipher-specializer block-length-expr)
             `(defmethod mode-crypt-functions ((cipher ,cipher-specializer)
                                               (mode ctr-mode))
                (let ((iv (iv mode))
                      (encrypted-iv (make-array ,block-length-expr :element-type '(unsigned-byte 8))))
                  (declare (type (simple-octet-vector ,block-length-expr) iv encrypted-iv))
                  (flet ((ctr-crypt-function (function)
                           (declare (type function function))
                           (mode-lambda
                            (let ((iv-position (iv-position mode))
                                  (keystream-blocks (keystream-blocks mode))
                                  (remaining (- in-end in-start))
                                  (offset in-start))
                              (declare (type (integer 0 (,block-length-expr)) iv-position)
                                       (type (integer 0 *) keystream-blocks)
                                       (type index remaining offset))

                              ;; Use remaining bytes in encrypted-iv
                              (loop until (or (zerop iv-position) (zerop remaining)) do
                                (setf (aref out out-start)
                                      (logxor (aref in offset) (aref encrypted-iv iv-position)))
                                (setf iv-position (mod (1+ iv-position) ,block-length-expr))
                                (incf offset)
                                (incf out-start)
                                (decf remaining))

                              ;; Process data by block
                              (multiple-value-bind (q r)
                                  (truncate remaining ,block-length-expr)
                                (dotimes (i q)
                                  (funcall function cipher iv 0 encrypted-iv 0)
                                  (increment-counter-block-1 ,block-length-expr iv)
                                  (xor-block ,block-length-expr encrypted-iv 0 in offset out out-start)
                                  (incf offset ,block-length-expr)
                                  (incf out-start ,block-length-expr))
                                (incf keystream-blocks q)
                                (setf remaining r))

                              ;; Process remaing bytes of data
                              (loop until (zerop remaining) do
                                (when (zerop iv-position)
                                  (funcall function cipher iv 0 encrypted-iv 0)
                                  (increment-counter-block-1 ,block-length-expr iv)
                                  (incf keystream-blocks))
                                (setf (aref out out-start)
                                      (logxor (aref in offset) (aref encrypted-iv iv-position)))
                                (setf iv-position (mod (1+ iv-position) ,block-length-expr))
                                (incf offset)
                                (incf out-start)
                                (decf remaining))

                              (setf (iv-position mode) iv-position)
                              (setf (keystream-blocks mode) keystream-blocks)
                              (let ((processed (- offset in-start)))
                                (values processed processed))))))
                    (let ((f (ctr-crypt-function (encrypt-function cipher))))
                      (values f f))))))
           (message-length (cipher-specializer block-length-expr)
             (declare (ignore block-length-expr))
             `(defmethod encrypted-message-length ((cipher ,cipher-specializer)
                                                   (mode ctr-mode) length
                                                   &optional handle-final-block)
                (declare (ignore handle-final-block))
                ;; We can encrypt the whole thing.
                length)))
  (define-mode-function mode-crypt message-length))

(defmethod mode-crypt-functions (cipher (mode stream-mode))
  (flet ((stream-crypt-function (function)
           (declare (type function function))
           (mode-lambda
            (let ((length (- in-end in-start)))
              (when (plusp length)
                (funcall function cipher in in-start out out-start length))
              (let ((n-bytes-processed (max 0 length)))
                (values n-bytes-processed n-bytes-processed))))))
    (values (stream-crypt-function (encrypt-function cipher))
            (stream-crypt-function (decrypt-function cipher)))))

) ; DEFINE-MODE-FUNCTION MACROLET

(defmethod encrypted-message-length (context
                                     (mode stream-mode) length
                                     &optional handle-final-block)
  (declare (ignore context mode handle-final-block))
  (declare (type index length))
  length)
