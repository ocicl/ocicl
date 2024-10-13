;;;; macros.lisp -- various internal macros
;;;;
;;;; This is part of tar-file. See README.org and LICENSE for more information.

(in-package #:tar-file)

(defun extractor-function-name (entry-name field-name)
  (intern (with-standard-io-syntax (format nil "~A-READ-~A-FROM-BUFFER" entry-name field-name))))
(defun injector-function-name (entry-name field-name)
  (intern (with-standard-io-syntax (format nil "~A-WRITE-~A-TO-BUFFER" entry-name field-name))))

(defgeneric field-offset (header field-name))

(defgeneric field-length (header field-name))

(defgeneric header-length (header))

(defmacro define-octet-header (class-name &rest field-defs)
  (let ((offset 0))                     ; could be integrated in the LOOP?
    (flet ((keywordify-name (name)
             (intern (symbol-name name) (find-package "KEYWORD"))))
      (loop for (name length kind constant) in field-defs
            collect `(defmethod field-offset ((header ,class-name) (field-name (eql ',name)))
                       ,offset) into offset-defs
            collect `(defmethod field-offset ((header (eql ',class-name)) (field-name (eql ',name)))
                       ,offset) into offset-defs
            collect `(defmethod field-length ((header ,class-name) (field-name (eql ',name)))
                       ,length) into length-defs
            collect `(defmethod field-length ((header (eql ',class-name)) (field-name (eql ',name)))
                       ,length) into length-defs
            collect `(defun ,(extractor-function-name class-name name) (buffer entry-start encoding)
                       (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
                       (declare (ignorable encoding))
                       ,(ecase kind
                          (:string
                           `(bytevec-to-string
                             (read-bytevec-from-buffer buffer :start (+ entry-start ,offset)
                                                              :end (+ entry-start ,offset ,length) :nullp nil)
                             encoding))
                          (:string-null
                           `(bytevec-to-string
                             (read-bytevec-from-buffer buffer :start (+ entry-start ,offset)
                                                              :end (+ entry-start ,offset ,length) :nullp t)
                             encoding))
                          (:byte
                           (unless (= length 1)
                             (error ":BYTE fields cannot be longer than 1"))
                           `(aref buffer (+ entry-start ,offset)))
                          (:bytes
                           `(subseq buffer (+ entry-start ,offset) (+ entry-start ,offset ,length)))
                          (:octnum `(read-number-from-buffer buffer :start (+ entry-start ,offset)
                                                                    :end (+ entry-start ,offset ,length) :radix 8))
                          (:hexnum `(read-number-from-buffer buffer :start (+ entry-start ,offset)
                                                                    :end (+ entry-start ,offset ,length) :radix 16)))) into reader-defs
            collect `(defun ,(injector-function-name class-name name) (buffer entry-start thing encoding)
                       (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
                       (declare (ignorable encoding))
                       ,(ecase kind
                          ((:string :string-null)
                           `(let ((thing (string-to-bytevec thing encoding)))
                              (dotimes (i (length thing) (values))
                                (setf (aref buffer (+ entry-start ,offset i)) (aref thing i)))))
                          (:byte
                           `(setf (aref buffer (+ entry-start ,offset)) thing))
                          (:bytes
                           `(setf (subseq buffer (+ entry-start ,offset) (+ entry-start ,offset ,length))
                                  thing))
                          (:octnum
                           `(let ((start (+ entry-start ,offset))
                                  (end (+ entry-start ,offset ,length)))
                              (write-number-to-buffer thing buffer :start start :end end :radix 8 :nullp t)))
                          (:hexnum
                           `(let ((start (+ entry-start ,offset))
                                  (end (+ entry-start ,offset ,length)))
                              (write-number-to-buffer thing buffer :start start :end end :radix 16 :nullp nil))))
                       (values)) into writer-defs
            collect `(,name :initarg ,(keywordify-name name)
                            :accessor ,name) into slot-definitions
            append `(,(keywordify-name name)
                     ,(if constant
                          constant
                          (case kind
                            ((:string :string-null) "")
                            (t 0)))) into default-initargs
            do (incf offset length)
            finally (return
                      `(progn
                         (defclass ,class-name ()
                           ,slot-definitions
                           (:default-initargs ,@default-initargs))
                         ,@length-defs
                         ,@offset-defs
                         ,@reader-defs
                         ,@writer-defs
                         (defmethod header-length ((header ,class-name))
                           ,offset)
                         (defmethod header-length ((header (eql ',class-name)))
                           ,offset)
                         (defmethod write-header-to-buffer ((header ,class-name) buffer encoding &optional (start 0))
                           (declare (type (simple-array (unsigned-byte 8) (*)) buffer))

                           ;; Ensure we can write the entire header to this
                           ;; buffer.
                           (assert (<= (+ start +tar-n-block-bytes+) (length buffer)))
                           ;; Ensure a clean slate
                           (fill buffer 0 :start start :end (+ start +tar-n-block-bytes+))

                           ,@(loop
                               :for (name length kind) :in field-defs
                               :unless (eql name 'checksum)
                                 :collect `(,(injector-function-name class-name name) buffer start (,name header)
                                            encoding))

                           ;; Write the checksum
                           (let* ((checksum (compute-checksum-for-tar-header header buffer start))
                                  (checksum-offset (+ start (field-offset header 'checksum))))
                             (write-number-to-buffer checksum buffer
                                                     :start checksum-offset
                                                     :end (+ checksum-offset
                                                             (field-length header 'checksum)
                                                             -2)
                                                     :radix 8)
                             ;; terminated with a NULL and then a space (!?)
                             (setf (aref buffer (+ checksum-offset 6)) 0
                                   (aref buffer (+ checksum-offset 7)) +ascii-space+)))
                         (defmethod read-header-from-buffer ((header (eql ',class-name)) buffer encoding &key (start 0))
                           (let ((checksum (,(extractor-function-name class-name 'checksum) buffer start encoding)))
                             (multiple-value-bind (validp computed)
                                 (tar-block-checksum-matches-p header buffer checksum start)
                               (unless validp
                                 (error 'invalid-checksum-error
                                        :provided checksum :computed computed))
                               (make-instance header
                                              ,@(loop
                                                  :for (name length kind) :in field-defs
                                                  :unless (eql name '%%padding)
                                                    :appending `(,(keywordify-name name)
                                                                 (,(extractor-function-name class-name name)
                                                                  buffer start encoding)))))))))))))
