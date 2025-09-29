(cl:in-package #:eclector.base)

(defun %reader-error (stream datum
                      &rest arguments
                      &key (stream-position (eclector.base:source-position
                                             *client* stream))
                           (position-offset 0)
                           (length          (- position-offset))
                      &allow-other-keys)
  (apply #'error datum :stream          stream
                       :stream-position stream-position
                       :position-offset position-offset
                       :length          length
                       (alexandria:remove-from-plist
                        arguments :stream-position :position-offset :length)))

(defgeneric recovery-description (strategy &key language)
  (:method ((strategy t) &key (language (acclimation:language
                                         acclimation:*locale*)))
    (recovery-description-using-language strategy language)))

(defgeneric recovery-description-using-language (strategy language))

(defun format-recovery-report (stream strategy &rest args)
  (labels ((resolve (strategy &rest args)
             (etypecase strategy
               (cons (apply #'resolve (append strategy args)))
               (symbol (apply #'resolve (recovery-description strategy) args))
               (string (apply #'format stream strategy args))
               (function (apply strategy stream args)))))
    (apply #'resolve strategy args)))

(defun %recoverable-reader-error (stream datum &rest arguments
                                               &key report &allow-other-keys)
  (restart-case
      (apply #'%reader-error stream datum
             (alexandria:remove-from-plist arguments :report))
    (recover ()
      :report (lambda (stream)
                (format-recovery-report stream report))
      (values))))

(defun recover (&optional condition)
  (alexandria:when-let ((restart (find-restart 'recover condition)))
    (invoke-restart restart)))

(define-condition stream-position-condition (condition)
  ((%stream-position :initarg :stream-position
                     :reader stream-position
                     :documentation
                     #.(format nil
                        "Approximate position in an input stream with ~
                         which the condition is associated. The ~
                         representation is controlled by the client by ~
                         adding methods on the generic function ~
                         STREAM-POSITION."))
   (%position-offset :initarg :position-offset
                     :type integer
                     :reader position-offset
                     :initform 0
                     :documentation
                     #.(format nil
                        "Offset from the approximate position to produce ~
                         the exact position. Always an integer and not ~
                         controlled by the client."))
   (%range-length    :initarg :length
                     :type (integer 0)
                     :reader range-length
                     :initform 1
                     :documentation
                     #.(format nil
                        "Length of the source range with which the ~
                         condition is associated."))))

(define-condition stream-position-reader-error (acclimation:condition
                                                stream-position-condition
                                                reader-error)
  ())

;;; Adds a stream position to CL:END-OF-FILE.
(define-condition end-of-file (acclimation:condition
                               stream-position-condition
                               cl:end-of-file)
  ())

(define-condition incomplete-construct (stream-position-reader-error)
  ())

(define-condition missing-delimiter (end-of-file incomplete-construct)
  ((%delimiter :initarg :delimiter :reader delimiter)))
