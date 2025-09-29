(cl:in-package #:eclector.readtable.simple)

(defmethod eclector.readtable:readtablep ((readtable readtable))
  t)

(defun parse-parameter-and-sub-char (stream)
  (loop for parameter = 0 then (+ (* 10 parameter) value)
        for parameter-given = nil then t
        for char2 = (eclector.base:read-char stream t nil t)
        for value = (digit-char-p char2)
        until (null value)
        finally (return (values (if parameter-given parameter nil)
                                char2))))

(defun make-dispatch-invoker (readtable disp-char)
  (alexandria:named-lambda dispatcher (stream char)
    (declare (ignore char))
    (multiple-value-bind (parameter sub-char)
        (handler-case
            (parse-parameter-and-sub-char stream)
          (end-of-file (condition)
            (eclector.base:%recoverable-reader-error
             stream 'eclector.readtable:unterminated-dispatch-macro
             :stream-position (eclector.base:stream-position condition)
             :disp-char disp-char :report 'ignore-partial-dispatch-macro)
            (return-from dispatcher)))
      (let ((macro-function
              (eclector.readtable:get-dispatch-macro-character
               readtable disp-char sub-char)))
        ;; If there is no macro function for SUB-CHAR, signal an error
        ;; irregardless of *READ-SUPPRESS* since the Hyperspec entry
        ;; for variable *READ-SUPPRESS* says
        ;;
        ;;   Dispatching macro characters (including sharpsign)
        ;;
        ;;   Dispatching macro characters continue to parse an infix
        ;;   numerical argument, and invoke the dispatch function. The
        ;;   standardized sharpsign reader macros do not enforce any
        ;;   constraints on either the presence of or the value of the
        ;;   numerical argument.
        ;;
        ;; We take this to mean that the dispatch function is invoked
        ;; and either calls a user-supplied function if the user has
        ;; installed one in the readtable or calls a standard macro
        ;; function or signals an error according to the table in
        ;; 2.4.8 Sharpsign.
        ;;
        ;; If we signal an error and the client invokes the recovery
        ;; restart, we just return. This means that characters
        ;; following the sub-char will be read outside the context of
        ;; the reader macro.
        (if (null macro-function)
            (eclector.base:%recoverable-reader-error
             stream 'eclector.readtable:unknown-macro-sub-character
             :position-offset -1
             :disp-char disp-char :sub-char sub-char
             :report 'ignore-partial-macro)
            (funcall macro-function stream sub-char parameter))))))

(defmethod eclector.readtable:make-dispatch-macro-character
    ((readtable readtable) char &optional non-terminating-p)
  (setf (gethash char (syntax-types readtable))
        (if non-terminating-p
            :non-terminating-macro
            :terminating-macro))
  (eclector.readtable:set-macro-character
   readtable char (make-dispatch-invoker readtable char) non-terminating-p)
  (setf (gethash char (dispatch-macro-characters readtable))
        (make-hash-table))
  t)

(defmethod eclector.readtable:get-macro-character ((readtable readtable) char)
  (let ((entry (gethash char (macro-characters readtable))))
    (values
     entry
     (eq (gethash char (syntax-types readtable)) :non-terminating-macro))))

(defmethod eclector.readtable:set-macro-character
    ((readtable readtable) char function &optional non-terminating-p)
  (setf (gethash char (syntax-types readtable))
        (if non-terminating-p
            :non-terminating-macro
            :terminating-macro))
  (setf (gethash char (macro-characters readtable))
        function)
  t)

(defmethod eclector.readtable:get-dispatch-macro-character
    ((readtable readtable) disp-char sub-char)
  ;; The HyperSpec does not say whether we should convert
  ;; to upper case here, but we think we should.
  (setf sub-char (char-upcase sub-char))
  (let ((subtable (gethash disp-char (dispatch-macro-characters readtable))))
    (when (null subtable)
      (error 'eclector.readtable:char-must-be-a-dispatching-character
             :disp-char disp-char))
    (nth-value 0 (gethash sub-char subtable))))

(defmethod eclector.readtable:set-dispatch-macro-character
    ((readtable readtable) disp-char sub-char function)
  (when (digit-char-p sub-char)
    (error 'eclector.readtable:sub-char-must-not-be-a-decimal-digit
           :disp-char disp-char
           :sub-char sub-char))
  (setf sub-char (char-upcase sub-char))
  (let ((subtable (gethash disp-char (dispatch-macro-characters readtable))))
    (when (null subtable)
      (error 'eclector.readtable:char-must-be-a-dispatching-character
             :disp-char disp-char))
    (setf (gethash sub-char subtable) function)))

(defmethod eclector.readtable:syntax-type ((readtable readtable) char)
  (let ((type (gethash char (syntax-types readtable))))
    (if (null type) :constituent type)))

(defmethod (setf eclector.readtable:syntax-type)
    (syntax-type (readtable readtable) char)
  (if (eq syntax-type :constituent)
      (remhash char (syntax-types readtable))
      (setf (gethash char (syntax-types readtable)) syntax-type))
  syntax-type)

(defmethod eclector.readtable:copy-readtable-into
    ((from-readtable readtable) (to-readtable readtable))
  (clrhash (syntax-types to-readtable))
  (clrhash (macro-characters to-readtable))
  (maphash (lambda (key value)
             (setf (gethash key (syntax-types to-readtable)) value))
           (syntax-types from-readtable))
  (maphash
   (lambda (char entry)
     (setf (gethash char (macro-characters to-readtable))
           (if (null (gethash char (dispatch-macro-characters from-readtable)))
               entry
               (make-dispatch-invoker to-readtable char))))
   (macro-characters from-readtable))
   (maphash (lambda (char entry)
             (let ((table (make-hash-table)))
               (maphash (lambda (sub-char function)
                          (setf (gethash sub-char table) function))
                        entry)
               (setf (gethash char (dispatch-macro-characters to-readtable))
                     table)))
           (dispatch-macro-characters from-readtable))
  (setf (eclector.readtable:readtable-case to-readtable)
        (eclector.readtable:readtable-case from-readtable))
  to-readtable)

(defmethod eclector.readtable:copy-readtable ((readtable readtable))
  (let ((result (make-instance 'readtable)))
    (eclector.readtable:copy-readtable-into readtable result)
    result))
