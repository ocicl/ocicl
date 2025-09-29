(cl:in-package #:eclector.readtable)

(define-condition dispatch-macro-character-error (acclimation:condition error)
  ((%disp-char :initarg :disp-char :reader disp-char)))

;;; Signaled when end of input is encountered while reading the sub
;;; character of a dispatching macro character.
(define-condition unterminated-dispatch-macro (dispatch-macro-character-error
                                               eclector.base:missing-delimiter)
  ())

;;; Signaled when an attempt is made to set up a digit character as a
;;; sub character of a dispatching macro character.
(define-condition sub-char-must-not-be-a-decimal-digit (dispatch-macro-character-error)
  ((%sub-char :initarg :sub-char :reader sub-char)))

;;; Signaled when an attempt is made to retrieve the macro function
;;; for a dispatching macro character but the supplied character is
;;; not a dispatching character.
(define-condition char-must-be-a-dispatching-character (dispatch-macro-character-error)
  ())

;;; Signaled when an attempt is made to retrieve the macro function
;;; for a sub character that is not defined in the context of a given
;;; dispatching macro character.
(define-condition unknown-macro-sub-character (eclector.base:stream-position-reader-error
                                               dispatch-macro-character-error)
  ((%sub-char :initarg :sub-char :reader sub-char)))
