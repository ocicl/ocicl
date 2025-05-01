;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Base class for multiplexers.
;;;

(in-package :iolib/multiplex)

(defvar *available-multiplexers* nil
  "An alist of (PRIORITY . MULTIPLEXER). Smaller values mean higher priority.")

(defvar *default-multiplexer* nil
  "The default multiplexer for the current OS.")

(defun get-fd-limit ()
  "Return the maximum number of FDs available for the current process."
  (let ((fd-limit (isys:getrlimit isys:rlimit-nofile)))
    (if (= fd-limit isys:rlim-infinity)
        65536 ; 64K should be enough for anybody
        fd-limit)))

(defclass multiplexer ()
  ((fd :reader fd-of)
   (fd-limit :initform (get-fd-limit)
             :initarg :fd-limit
             :reader fd-limit-of)
   (closedp :accessor multiplexer-closedp
            :initform nil))
  (:documentation "Base class for I/O multiplexers."))

(defgeneric close-multiplexer (mux)
  (:method-combination progn :most-specific-last)
  (:documentation "Close multiplexer MUX, calling close() on the multiplexer's FD if bound."))

(defgeneric monitor-fd (mux fd-entry)
  (:documentation "Add the descriptor represented by FD-ENTRY to multiplexer MUX.
Must return NIL on failure, T otherwise."))

(defgeneric update-fd (mux fd-entry event-type edge-change)
  (:documentation "Update the status of the descriptor represented by FD-ENTRY in multiplexer MUX.
Must return NIL on failure, T otherwise."))

(defgeneric unmonitor-fd (mux fd-entry)
  (:documentation "Remove the descriptor represented by FD-ENTRY from multiplexer MUX.
Must return NIL on failure, T otherwise."))

(defgeneric harvest-events (mux timeout)
  (:documentation "Wait for events on multiplexer MUX for a maximum time of TIMEOUT seconds.
Returns a list of fd/result pairs which have one of these forms:
  (fd (:read))
  (fd (:write))
  (fd (:read :write))
  (fd . :error)"))

(defmethod close-multiplexer :around ((mux multiplexer))
  (unless (multiplexer-closedp mux)
    (call-next-method)
    (setf (multiplexer-closedp mux) t)))

(defmethod close-multiplexer progn ((mux multiplexer))
  (when (and (slot-boundp mux 'fd) (not (null (fd-of mux))))
    (isys:close (fd-of mux))
    (setf (slot-value mux 'fd) nil))
  (values mux))

(defmethod monitor-fd :before ((mux multiplexer) fd-entry)
  (with-accessors ((fd-limit fd-limit-of))
      mux
    (let ((fd (fd-entry-fd fd-entry)))
      (when (and fd-limit (> fd fd-limit))
        (error "Cannot add such a large FD: ~A" fd)))))

(defmacro define-multiplexer (name priority superclasses slots &rest options)
  `(progn
     (defclass ,name ,superclasses ,slots ,@options)
     (pushnew (cons ,priority ',name) *available-multiplexers*
              :test #'equal)))
