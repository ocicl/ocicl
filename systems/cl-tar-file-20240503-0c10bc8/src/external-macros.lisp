;;;; external-macros.lisp -- various external macros
;;;;
;;;; This is part of tar-file. See README.org and LICENSE for more information.

(in-package #:tar-file)

(defun call-with-open-tar-file (thunk pathname-or-stream
                                &key (direction :input)
                                  (if-exists nil)
                                  (if-does-not-exist nil)
                                  (type :auto)
                                  (blocking-factor 20)
                                  (compression :auto)
                                  (header-encoding *default-header-encoding*))
  (when (or (eq direction :io) (eq direction :probe))
    (error "Cannot open tar-files in direction ~A" direction))
  (let (tar-file
        stream
        (should-close t)
        (abort t))
    (unwind-protect
         (progn
           (when (streamp pathname-or-stream) (setf should-close nil))
           (setf stream (if should-close
                            (apply #'open
                                   pathname-or-stream
                                   :direction direction
                                   :element-type '(unsigned-byte 8)
                                   (append
                                    (when if-exists
                                      (list :if-exists if-exists))
                                    (when if-does-not-exist
                                      (list :if-does-not-exist if-does-not-exist))))
                            pathname-or-stream))
           (setf tar-file (open-tar-file stream :direction direction
                                                :type type
                                                :blocking-factor blocking-factor
                                                :header-encoding header-encoding
                                                :compression compression))
           (multiple-value-prog1
               (funcall thunk tar-file)
             (setf abort nil)))
      (when tar-file
        (when (eql direction :output)
          (finalize-tar-file tar-file))
        (close-tar-file tar-file)
        (setf tar-file nil))
      (when should-close
        (close stream :abort abort)))))

(defmacro with-open-tar-file ((tar-file-var pathname-or-stream
                               &key (direction :input)
                                 (if-exists nil)
                                 (if-does-not-exist nil)
                                 (type :auto)
                                 (compression :auto)
                                 (blocking-factor 20)
                                 (header-encoding '*default-header-encoding*))
                              &body body)
  "Bind TAR-FILE-VAR to a newly opened TAR-FILE, backed by
PATHNAME-OR-STREAM. If PATHNAME-OR-STREAM evaluates to a stream, that stream is
used directly, otherwise, it is opened via OPEN. If PATHNAME-OR-STREAM is a
stream, that stream is not closed upon exiting the body of the macro.

DIRECTION must be either :INPUT or :OUTPUT.

IF-EXISTS and IF-DOES-NOT-EXIST are passed to OPEN if PATHNAME-OR-STREAM is not
a stream.

See OPEN-TAR-FILE for a description of TYPE, BLOCKING-FACTOR, HEADER-ENCODING,
and COMPRESSION."
  (when (or (eq direction :io) (eq direction :probe))
    (error "Cannot open tar-files in direction ~A" direction))
  `(call-with-open-tar-file (lambda (,tar-file-var) ,@body)
                            ,pathname-or-stream
                            :direction ,direction
                            :if-exists ,if-exists
                            :if-does-not-exist ,if-does-not-exist
                            :type ,type
                            :blocking-factor ,blocking-factor
                            :header-encoding ,header-encoding
                            :compression ,compression))

(defmacro do-entries ((entry tar-file &optional result)
                      &body body)
  "Iterate over the entries in TAR-FILE.  For each entry, ENTRY is bound to an
ENTRY representing the entry.  RESULT is used as in DOTIMES."
  (let ((tar-file-var (gensym)))
    `(let ((,tar-file-var ,tar-file))
       (do ((,entry (read-entry ,tar-file-var)
                    (read-entry ,tar-file-var)))
           ((null ,entry) ,result)
         ,@body))))
