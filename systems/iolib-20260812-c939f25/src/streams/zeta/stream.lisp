;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Zeta Streams.
;;;

(in-package :iolib/zstreams)

;;;-------------------------------------------------------------------------
;;; Classes and Types
;;;-------------------------------------------------------------------------

(defclass buffer ()
  ())

(defclass device-buffer (buffer)
  ((synchronized :initarg :synchronized)
   (device :initarg :device)
   (base-device :initarg :base-device)
   (input-iobuf :initarg :input-buffer)
   (output-iobuf :initarg :output-buffer)
   (buffering :initarg :buffering))
  (:default-initargs :synchronized nil))

(defclass memory-buffer (buffer)
  ((data-vector :initform nil)
   (element-type :initarg :element-type)
   (input-position :initform 0)
   (output-position :initform 0)
   (adjust-size :initarg :adjust-size)
   (adjust-threshold :initarg :adjust-threshold))
  (:default-initargs :element-type 't
                     :adjust-size 1.5
                     :adjust-threshold 1))

(defclass zstream ()
  ((%flags :initform 0 :type ub16)
   external-format))

(defclass device-zstream (device-buffer zstream)
  ())

(defclass single-channel-zstream (device-zstream)
  ((dirtyp :initform nil)))

(defclass dual-channel-zstream (device-zstream)
  ())

(defclass memory-zstream (memory-buffer zstream)
  ())

(defclass octet-memory-zstream (memory-zstream)
  ()
  (:default-initargs :element-type 'octet))

(defclass character-memory-zstream (memory-zstream)
  ()
  (:default-initargs :element-type 'character))


;;;-------------------------------------------------------------------------
;;; Generic Functions
;;;-------------------------------------------------------------------------

;;; Accessors

(defgeneric zstream-synchronized-p (stream))

(defgeneric zstream-dirtyp (stream))

(defgeneric (setf zstream-dirtyp) (value stream))

(defgeneric zstream-device (stream))

(defgeneric zstream-base-device (stream))

(defgeneric zstream-external-format (stream))

(defgeneric (setf zstream-external-format) (external-format stream))

(defgeneric zstream-element-type (stream))

;;; I/O functions

(defgeneric zstream-read-element (stream &key timeout))

(defgeneric zstream-write-element (stream element &key timeout))

(defgeneric zstream-read-vector (stream vector &key start end timeout))

(defgeneric zstream-write-vector (stream vector &key start end timeout))

(defgeneric zstream-read-byte (stream &key width signed))

(defgeneric zstream-write-byte (stream byte &key width signed))

(defgeneric zstream-read-char (stream &key eof-error-p eof-value))

(defgeneric zstream-write-char (stream char &key hangup-error-p hangup-value))

(defgeneric zstream-read-line (stream &key eof-error-p eof-value))

(defgeneric zstream-write-line (stream line &key start end hangup-error-p hangup-value))

;;; Device zstream functions

(defgeneric zstream-position (stream &key direction))

(defgeneric (setf zstream-position) (position stream &key direction from))

(defgeneric zstream-poll (stream &key direction timeout))

(defgeneric zstream-fill (stream &key timeout))

(defgeneric zstream-flush (stream &key timeout))

(defgeneric zstream-clear-input (stream))

(defgeneric zstream-clear-output (stream))

;;; Internal functions

(defgeneric %zstream-read-vector (stream vector start end timeout))

(defgeneric %zstream-write-vector (stream vector start end timeout))

(defgeneric %zstream-fill (stream timeout))

(defgeneric %zstream-flush (stream timeout))

(defgeneric %zstream-clear-input (stream))

(defgeneric %zstream-clear-output (stream))

;; FIXME: choose better name
(defgeneric %ensure-buffer-capacity (stream &optional amount))

;; FIXME: choose better name
(defgeneric %check-buffer-available-data (stream &optional amount))


;;;-------------------------------------------------------------------------
;;; Accessors
;;;-------------------------------------------------------------------------

(defmethod zstream-synchronized-p ((stream device-zstream))
  (slot-value stream 'synchronized))

(defmethod zstream-synchronized-p ((stream memory-zstream))
  (declare (ignore stream))
  (values nil))

(defmethod zstream-dirtyp ((stream single-channel-zstream))
  (slot-value stream 'dirtyp))

(defmethod zstream-dirtyp ((stream dual-channel-zstream))
  (plusp (iobuf-available-octets (slot-value stream 'output-iobuf))))

(defmethod (setf zstream-dirtyp) (value (stream dual-channel-zstream))
  (declare (ignore value))
  (values nil))

(defmethod zstream-device ((stream device-zstream))
  (slot-value stream 'device))

(defmethod zstream-device ((stream memory-zstream))
  (declare (ignore stream))
  (values nil))

(defmethod zstream-base-device ((stream device-zstream))
  (slot-value stream 'base-device))

(defmethod zstream-base-device ((stream memory-zstream))
  (declare (ignore stream))
  (values nil))

(defmethod zstream-external-format ((stream zstream))
  (slot-value stream 'external-format))

(defmethod (setf zstream-external-format)
    (external-format (stream zstream))
  (setf (slot-value stream 'external-format)
        (babel:ensure-external-format external-format)))

(defmethod zstream-element-type ((stream device-zstream))
  '(unsigned-byte 8))

(defmethod zstream-element-type ((stream memory-zstream))
  (slot-value stream 'element-type))


;;;-------------------------------------------------------------------------
;;; Helper macros
;;;-------------------------------------------------------------------------

;; FIXME: synchronize memory streams too ?
(defmacro with-synchronized-device-zstream
    ((stream &optional direction) &body body)
  (with-gensyms (body-fun)
    (labels ((make-locks (body direction)
               (ecase direction
                 (:input
                  `(bt:with-lock-held
                       ((iobuf-lock (slot-value ,stream 'input-iobuf)))
                     ,body))
                 (:output
                  `(bt:with-lock-held
                       ((iobuf-lock (slot-value ,stream 'output-iobuf)))
                     ,body))
                 (:io
                  (make-locks (make-locks body :output) :input)))))
      `(flet ((,body-fun () ,@body))
         (if (zstream-synchronized-p ,stream)
             ,(make-locks `(,body-fun) direction)
             (,body-fun))))))

(defconstant (+flag-bits+ :test 'equal)
  '(:zeta                             ; instance is valid
    :buffering                        ; stream is buffered(not raw device)
    :eof                              ; latched EOF
    :dirty))                          ; output buffer needs write

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %flags (flags)
    (loop :for flag :in flags
          :as pos := (position flag +flag-bits+)
          :if pos
            :sum (ash 1 pos) :into bits
          :else
            :collect flag :into unused
          :finally (when unused
                     (warn "Invalid stream instance flag~P: ~{~S~^, ~}"
                           (length unused) unused))
                   (return bits))))

(defmacro with-zstream-class ((class-name &optional stream) &body body)
  (if stream
      (with-gensyms ((stm "ZSTREAM"))
        `(let* ((,stm ,stream))
           (declare (type ,class-name ,stm))
           (macrolet ((sm (slot-name stream)
                        (declare (ignore stream))
                        `(slot-value ,',stm ',slot-name))
                      (add-zstream-instance-flags (stream &rest flags)
                        (declare (ignore stream))
                        `(setf (sm %flags ,',stm) (logior (sm %flags ,',stm)
                                                          ,(%flags flags))))
                      (remove-zstream-instance-flags (stream &rest flags)
                        (declare (ignore stream))
                        `(setf (sm %flags ,',stm) (logandc2 (sm %flags ,',stm)
                                                            ,(%flags flags))))
                      (any-zstream-instance-flags (stream &rest flags)
                        (declare (ignore stream))
                        `(not (zerop (logand (sm %flags ,',stm)
                                             ,(%flags flags))))))
             ,@body)))
      `(macrolet ((sm (slot-name stream)
                    `(slot-value ,stream ',slot-name)))
         ,@body)))

(defmacro sm (slot-name stream)
  "Access the named slot in Stream."
  (warn "Using ~S macro outside ~S." 'sm 'with-zstream-class)
  `(slot-value ,stream ',slot-name))

(defmacro add-zstream-instance-flags (stream &rest flags)
  "Set the given Flags in Stream."
  (with-gensyms ((s "STREAM"))
    `(let ((,s ,stream))
       (with-zstream-class (zstream ,s)
         (add-zstream-instance-flags ,s ,@flags)))))

(defmacro remove-zstream-instance-flags (stream &rest flags)
  "Clear the given Flags in Stream."
  (with-gensyms ((s "STREAM"))
    `(let ((,s ,stream))
       (with-zstream-class (zstream ,s)
         (remove-zstream-instance-flags ,s ,@flags)))))

(defmacro any-zstream-instance-flags (stream &rest flags)
  "Determine whether any one of the Flags is set in Stream."
  (with-gensyms ((s "STREAM"))
    `(let ((,s ,stream))
       (with-zstream-class (zstream ,s)
         (any-zstream-instance-flags ,s ,@flags)))))


;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

(defmethod shared-initialize :after
    ((stream single-channel-zstream) slot-names
     &key data size buffering)
  (declare (ignore slot-names))
  (with-slots (device input-iobuf output-iobuf)
      stream
    (check-type device device)
    (check-type buffering stream-buffering)
    (when buffering
      (check-type data (or null iobuf))
      (setf input-iobuf  (or data (make-iobuf size))
            output-iobuf input-iobuf)
      (add-zstream-instance-flags stream :buffering))))

(defmethod shared-initialize :after
    ((stream dual-channel-zstream) slot-names
     &key input-data output-data input-size output-size buffering)
  (declare (ignore slot-names))
  (with-slots (device input-iobuf output-iobuf)
      stream
    (check-type device device)
    (check-type buffering stream-buffering)
    (when buffering
      (check-type input-data (or null iobuf))
      (check-type output-data (or null iobuf))
      (setf input-iobuf  (or input-data (make-iobuf input-size))
            output-iobuf (or output-data (make-iobuf output-size)))
      (add-zstream-instance-flags stream :buffering))))

(defmethod shared-initialize :after
    ((stream memory-zstream) slot-names &key data (start 0) end)
  (declare (ignore slot-names))
  (with-slots (data-vector input-position output-position
               element-type adjust-size adjust-threshold)
      stream
    (check-type adjust-size (real 1.001))
    (check-type adjust-threshold (real 0.1 1))
    (setf element-type (upgraded-array-element-type element-type))
    (cond
      (data
       (check-bounds data start end)
       (when (and element-type
                  (not (subtypep element-type (array-element-type data))))
         (error 'subtype-error
                :datum element-type
                :expected-supertype (array-element-type data)))
       (setf data-vector
             (make-array (truncate (* adjust-size (length data)))
                         :element-type (or element-type
                                           (array-element-type data))))
       (setf output-position (- end start))
       (replace data-vector data :start2 start :end2 end))
      (t
       (setf data-vector (make-array 128 :element-type element-type))))
    (add-zstream-instance-flags stream :buffering)))

(defmethod shared-initialize :after ((stream zstream) slot-names
                                     &key (external-format :default))
  (declare (ignore slot-names))
  (setf (zstream-external-format stream) external-format))

(defun make-memory-zstream (&key data (start 0) end (element-type t)
                            (adjust-size 1.5) (adjust-threshold 1)
                            (external-format :default))
  (let ((element-type (upgraded-array-element-type element-type)))
    (cond
      ((subtypep element-type 'octet)
       (make-instance 'octet-memory-zstream
                      :data data :start start :end end
                      :adjust-size adjust-size
                      :adjust-threshold adjust-threshold
                      :external-format external-format))
      ((subtypep element-type 'character)
       (make-instance 'character-memory-zstream
                      :data data :start start :end end
                      :adjust-size adjust-size
                      :adjust-threshold adjust-threshold))
      ((subtypep element-type 't)
       (make-instance 'memory-zstream
                      :data data :start start :end end
                      :element-type element-type
                      :adjust-size adjust-size
                      :adjust-threshold adjust-threshold))
      (t
       (error 'subtype-error :datum element-type
              :expected-supertype '(or (unsigned-byte 8) character t))))))


;;;-------------------------------------------------------------------------
;;; RELINQUISH
;;;-------------------------------------------------------------------------

(defmethod relinquish :after ((stream single-channel-zstream) &key abort)
  (with-synchronized-device-zstream (stream :input)
    (unless abort
      (%zstream-flush stream 0))
    (relinquish (zstream-device stream) :abort abort))
  (values stream))

(defmethod relinquish :after ((stream dual-channel-zstream) &key abort)
  (with-synchronized-device-zstream (stream :io)
    (unless abort
      (%zstream-flush stream 0))
    (relinquish (zstream-device stream) :abort abort))
  (values stream))


;;;-------------------------------------------------------------------------
;;; READ-ELEMENT
;;;-------------------------------------------------------------------------

(defmethod zstream-read-element ((stream device-zstream) &key timeout)
  (let ((v (make-array 1 :element-type 'octet)))
    (zstream-read-vector stream v :timeout timeout)
    (aref v 0)))

(defmethod zstream-read-element ((stream memory-zstream) &key timeout)
  (declare (ignore timeout))
  (let ((v (make-array 1 :element-type (slot-value stream 'element-type))))
    (zstream-read-vector stream v)
    (aref v 0)))


;;;-------------------------------------------------------------------------
;;; READ-VECTOR
;;;-------------------------------------------------------------------------

(defmethod zstream-read-vector :around ((stream zstream) vector &key
                                        (start 0) end timeout)
  (check-bounds vector start end)
  (when (= start end) (return* 0))
  (call-next-method stream vector :start start :end end :timeout timeout))

(defmethod zstream-read-vector ((stream single-channel-zstream) vector
                                &key start end timeout)
  (with-synchronized-device-zstream (stream :input)
    (%zstream-read-vector stream vector start end timeout)))

(defmethod zstream-read-vector ((stream dual-channel-zstream) vector
                                &key start end timeout)
  (with-synchronized-device-zstream (stream :input)
    (%zstream-read-vector stream vector start end timeout)))

(defmethod %zstream-read-vector ((stream device-zstream) vector
                                 start end timeout)
  (with-slots (input-iobuf)
      stream
    (cond
      ((iobuf-empty-p input-iobuf)
       (let ((nbytes (%zstream-fill stream timeout)))
         (if (iobuf-empty-p input-iobuf)
             (if (eql :eof nbytes) :eof 0)
             (iobuf->vector input-iobuf vector start end))))
      (t
       (iobuf->vector input-iobuf vector start end)))))

(defmethod zstream-read-vector ((stream memory-zstream) vector
                                &key start end timeout)
  (declare (ignore timeout))
  (with-slots (data-vector input-position output-position)
      stream
    (%check-buffer-available-data stream 1)
    (replace vector data-vector
             :start1 input-position :end1 output-position
             :start2 start :end2 end)
    (incf input-position (min (- output-position input-position)
                              (- end start)))))


;;;-------------------------------------------------------------------------
;;; WRITE-ELEMENT
;;;-------------------------------------------------------------------------

(defmethod zstream-write-element ((stream device-zstream) octet &key timeout)
  (check-type octet octet)
  (let ((v (make-array 1 :element-type 'octet :initial-element octet)))
    (zstream-write-vector stream v :timeout timeout)))

(defmethod zstream-write-element ((stream memory-zstream) element &key timeout)
  (declare (ignore timeout))
  (let ((v (make-array 1 :element-type (slot-value stream 'element-type)
                       :initial-contents element)))
    (zstream-write-vector stream v)))


;;;-------------------------------------------------------------------------
;;; WRITE-VECTOR
;;;-------------------------------------------------------------------------

(defmethod zstream-write-vector :around ((stream zstream) vector
                                         &key (start 0) end timeout)
  (check-bounds vector start end)
  (when (= start end) (return* 0))
  (call-next-method stream vector :start start :end end :timeout timeout))

(defmethod zstream-write-vector ((stream single-channel-zstream) vector
                                 &key start end timeout)
  (with-synchronized-device-zstream (stream :output)
    ;; If the previous operation was a read, flush the read buffer
    ;; and reposition the file offset accordingly
    (%zstream-clear-input stream)
    (%zstream-write-vector stream vector start end timeout)))

(defmethod zstream-write-vector ((stream dual-channel-zstream) vector
                                 &key start end timeout)
  (with-synchronized-device-zstream (stream :output)
    (%zstream-write-vector stream vector start end timeout)))

(defmethod %zstream-write-vector ((stream device-zstream) vector start end timeout)
  (with-slots (output-iobuf)
      stream
    (multiple-value-prog1
        (vector->iobuf output-iobuf vector start end)
      (when (iobuf-full-p output-iobuf)
        (%zstream-flush stream timeout)))))

(defmethod %zstream-write-vector :after ((stream single-channel-zstream)
                                         vector start end timeout)
  (declare (ignore vector start end timeout))
  (setf (slot-value stream 'dirtyp) t))

(defmethod zstream-write-vector ((stream memory-zstream) vector
                                 &key (start 0) end timeout)
  (declare (ignore timeout))
  (with-slots (data-vector output-position)
      stream
    (%ensure-buffer-capacity stream (length vector))
    (replace data-vector vector :start1 output-position
             :start2 start :end2 end)
    (incf output-position (length vector))))


;;;-------------------------------------------------------------------------
;;; POSITION
;;;-------------------------------------------------------------------------

(defmethod zstream-position ((stream single-channel-zstream) &key direction)
  (declare (ignore direction))
  (with-slots (input-iobuf output-iobuf dirtyp)
      stream
    (with-synchronized-device-zstream (stream :input)
      (let ((position (device-position (zstream-device stream))))
        ;; FIXME: signal proper condition
        (assert (not (null position)) (position)
                "A single-channel-zstream's device must not return a NULL device-position.")
        (if dirtyp
            (+ position (iobuf-available-octets output-iobuf))
            (- position (iobuf-available-octets input-iobuf)))))))

(defmethod zstream-position ((stream dual-channel-zstream) &key direction)
  (declare (ignore direction))
  (with-synchronized-device-zstream (stream :io)
    (device-position (zstream-device stream))))

(defmethod zstream-position ((stream memory-zstream) &key direction)
  (ecase direction
    (:input  (slot-value stream 'input-position))
    (:output (slot-value stream 'output-position))))


;;;-------------------------------------------------------------------------
;;; (SETF POSITION)
;;;-------------------------------------------------------------------------

(defmethod (setf zstream-position)
    (position (stream device-zstream) &key direction (from :start))
  (declare (ignore direction))
  (with-synchronized-device-zstream (stream :input)
    (setf (device-zstream-position stream from) position)))

(defun (setf device-zstream-position) (position stream from)
  (setf (device-position (zstream-device stream) from) position))

(defmethod (setf zstream-position)
    (offset (stream memory-zstream) &key direction (from :start))
  (with-slots (data-vector input-position output-position)
      stream
    (ecase direction
      (:input
       (let ((newpos
              (ecase from
                (:start   offset)
                (:current (+ input-position offset))
                (:output  (+ output-position offset)))))
         (check-bounds data-vector newpos output-position)
         (setf input-position newpos)))
      (:output
       (let ((newpos
              (ecase from
                (:start   offset)
                (:current (+ output-position offset))
                (:input   (+ input-position offset)))))
         (%ensure-buffer-capacity stream (- newpos output-position))
         (setf output-position newpos))))))


;;;-------------------------------------------------------------------------
;;; CLEAR-INPUT
;;;-------------------------------------------------------------------------

(defmethod zstream-clear-input ((stream device-zstream))
  (with-synchronized-device-zstream (stream :input)
    (%zstream-clear-input stream)))

(defmethod %zstream-clear-input ((stream single-channel-zstream))
  (with-slots (input-iobuf dirtyp)
      stream
    (unless dirtyp
      (let ((nbytes (iobuf-available-octets input-iobuf)))
        (unless (zerop nbytes)
          (setf (device-zstream-position stream :current) (- nbytes)))
        (iobuf-reset input-iobuf)))))

(defmethod %zstream-clear-input ((stream dual-channel-zstream))
  (iobuf-reset (slot-value stream 'input-iobuf)))

(defmethod zstream-clear-input ((stream memory-zstream))
  (setf (slot-value stream 'input-position)
        (slot-value stream 'output-position)))


;;;-------------------------------------------------------------------------
;;; CLEAR-OUTPUT
;;;-------------------------------------------------------------------------

(defmethod zstream-clear-output ((stream device-zstream))
  (with-synchronized-device-zstream (stream :output)
    (%zstream-clear-output stream)))

(defmethod %zstream-clear-output ((stream single-channel-zstream))
  (with-slots (output-iobuf dirtyp)
      stream
    (when dirtyp
      (iobuf-reset output-iobuf))))

(defmethod %zstream-clear-output ((stream dual-channel-zstream))
  (iobuf-reset (slot-value stream 'output-iobuf)))

(defmethod zstream-clear-output ((stream memory-zstream))
  (setf (slot-value stream 'output-position)
        (slot-value stream 'input-position)))


;;;-------------------------------------------------------------------------
;;; FILL-INPUT
;;;-------------------------------------------------------------------------

(defmethod zstream-fill ((stream single-channel-zstream) &key timeout)
  (with-synchronized-device-zstream (stream :input)
    (%zstream-flush stream timeout)
    (%zstream-fill stream timeout)))

(defmethod zstream-fill ((stream dual-channel-zstream) &key timeout)
  (with-synchronized-device-zstream (stream :input)
    (%zstream-fill stream timeout)))

(defmethod %zstream-fill ((stream device-zstream) timeout)
  (with-slots (device input-iobuf)
      stream
    (multiple-value-bind (data start end)
        (iobuf-next-empty-zone input-iobuf)
      (let ((nbytes
             (device-read device data :start start
                          :end end :timeout timeout)))
        (etypecase nbytes
          ((eql :eof)
           (error 'end-of-file :stream stream))
          (unsigned-byte
           (setf (iobuf-end input-iobuf) (+ start nbytes))
           (values nbytes (iobuf-available-space input-iobuf))))))))

(defmethod zstream-fill ((stream memory-zstream) &key timeout)
  (declare (ignore stream timeout))
  (values nil))


;;;-------------------------------------------------------------------------
;;; FLUSH-OUTPUT
;;;-------------------------------------------------------------------------

(defmethod zstream-flush ((stream device-zstream) &key timeout)
  (with-synchronized-device-zstream (stream :output)
    (%zstream-flush stream timeout)))

(defmethod %zstream-flush ((stream device-zstream) timeout)
  (with-slots (device output-iobuf dirtyp)
      stream
    (when dirtyp
      (multiple-value-bind (data start end)
          (iobuf-next-data-zone output-iobuf)
        (let ((nbytes
               (device-write device data :start start
                             :end end :timeout timeout)))
          (etypecase nbytes
            ((eql :hangup)
             (error 'hangup :stream stream))
            (unsigned-byte
             (setf (iobuf-start output-iobuf) (+ start nbytes))
             (values nbytes (iobuf-available-octets output-iobuf)))))))))

(defmethod %zstream-flush :after ((stream single-channel-zstream) timeout)
  (declare (ignore timeout))
  (with-slots (output-iobuf dirtyp)
      stream
    (when (iobuf-empty-p output-iobuf)
      (setf dirtyp nil))))

(defmethod zstream-flush ((stream memory-zstream) &key timeout)
  (declare (ignore stream timeout))
  (values nil))


;;;-------------------------------------------------------------------------
;;; MEMORY-ZSTREAM GROWTH
;;;-------------------------------------------------------------------------

(defmethod %ensure-buffer-capacity
    ((stream memory-zstream) &optional (amount 1))
  (check-type amount unsigned-byte)
  (with-slots (data-vector output-position adjust-size adjust-threshold)
      stream
    (let* ((size-needed (+ output-position amount))
           (threshold (ceiling (* adjust-threshold size-needed))))
      (when (> threshold (length data-vector))
        (setf data-vector
              (adjust-array data-vector
                            (truncate (* adjust-size size-needed))))))))

(defmethod %check-buffer-available-data
    ((stream memory-zstream) &optional (amount 1))
  (check-type amount positive-integer)
  (with-slots (input-position output-position)
      stream
    (let ((available-data (- output-position input-position)))
      (check-type available-data unsigned-byte)
      (cond
        ((zerop available-data)
         (error 'end-of-file :stream stream))
        ((< available-data amount)
         ;; FIXME: signal proper condition, soft EOF
         (error "~S elements requested, only ~S available"
                amount available-data))))))


;;;-------------------------------------------------------------------------
;;; I/O WAIT
;;;-------------------------------------------------------------------------

(defmethod zstream-poll ((stream device-zstream) &key direction timeout)
  (device-poll (zstream-device stream) direction timeout))

(defmethod zstream-poll ((stream memory-zstream) &key direction timeout)
  (declare (ignore timeout))
  (with-slots (input-position output-position)
      stream
    (ecase direction
      (:input  (< input-position output-position))
      (:output t))))
