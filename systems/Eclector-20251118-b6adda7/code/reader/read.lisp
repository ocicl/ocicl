(cl:in-package #:eclector.reader)

;;; Entry points
;;;
;;; READ and READ-PRESERVING-WHITESPACE are the main entry points to
;;; the reader. These functions behave pretty differently depending on
;;; whether they are called recursively or not. Furthermore, a given
;;; call site almost always only requests one of the two behaviors.
;;; Thus, to avoid the overhead of selecting the correct behavior at
;;; runtime for the recursive case, compiler macros for the two
;;; functions transform recursive calls into the effectively resulting
;;; READ-COMMON calls saving full calls to
;;; READ[-PRESERVING-WHITESPACE] and READ-AUX.

(defun read-aux
    (input-stream eof-error-p eof-value recursive-p preserve-whitespace-p)
  (let ((client *client*))
    (if recursive-p
        (with-state-values (client '*consing-dot-allowed-p* nil)
          (read-common client input-stream eof-error-p eof-value))
        (flet ((read-common ()
                 (read-common client input-stream eof-error-p eof-value)))
          (declare (dynamic-extent #'read-common))
          (call-as-top-level-read
           client #'read-common input-stream
           eof-error-p eof-value preserve-whitespace-p)))))

(macrolet
    ((define (name preserve-whitespace-form)
       `(progn
          (defun ,name (&optional
                        (input-stream *standard-input*)
                        (eof-error-p t)
                        (eof-value nil)
                        (recursive-p nil))
            (read-aux input-stream eof-error-p eof-value recursive-p ,preserve-whitespace-form))

          (define-compiler-macro ,name (&whole form
                                        &optional (input-stream '*standard-input*)
                                                  (eof-error-p 't)
                                                  (eof-value 'nil)
                                                  (recursive-p nil))
            (if (and (constantp recursive-p)
                     (eval recursive-p))
                `(let ((client *client*))
                   (with-state-values (client '*consing-dot-allowed-p* nil)
                     (read-common client ,input-stream ,eof-error-p ,eof-value)))
                form)))))
  (define read                       recursive-p)
  (define read-preserving-whitespace t))

(locally (declare #+sbcl (sb-ext:muffle-conditions eclector.base:&optional-and-&key-style-warning))
  (defun read-from-string (string &optional (eof-error-p t)
                                            (eof-value nil)
                                  &key (start 0)
                                       (end nil)
                                       (preserve-whitespace nil))
    (let ((index))
      (values (with-input-from-string (stream string :start start :end end
                                                     :index index)
                (read-aux stream eof-error-p eof-value nil preserve-whitespace))
              index))))

;;; Reading lists

;;; Skip over whitespace and comments until either CLOSE-CHAR is
;;; encountered or an object can be read.
(defun %read-list-element (client stream close-char)
  ;; Note that calling PEEK-CHAR this way skips over whitespace but
  ;; not comments.
  (loop for char = (peek-char t stream t nil)
        if (eql char close-char)
          do (read-char stream)
             (signal-end-of-list char)
        else
          do (multiple-value-bind (object what)
                 (read-maybe-nothing client stream t nil)
               (unless (eq what :skip) ; Skip over comments
                 (return object)))))

;;; Read a list terminated by CLOSE-CHAR from STREAM. For each
;;; encountered list element as well the end of the list (or premature
;;; end of input) call FUNCTION with two arguments: 1) an element kind
;;; indicator which is one of :PROPER, :TAIL and :END 2) the read
;;; element, EOL-VALUE or EOF-VALUE.
(defun %read-list-elements (stream function eol-value eof-value
                            close-char consing-dot-allowed-p)
  (let ((client *client*)
        (state :proper))
    (handler-case
        (with-state-values
            (client '*consing-dot-allowed-p* consing-dot-allowed-p)
          (loop for object = (with-state-values
                                 (client '*consing-dot-allowed-p* nil)
                               (%read-list-element client stream close-char))
                  then (case state
                         (:proper
                          (%read-list-element client stream close-char))
                         (t
                          (%read-list-element client stream close-char)))
                if (eq object *consing-dot*)
                  do (setf state :tail)
                     (funcall function state (read stream t nil t))
                     (setf state :end)
                     ;; This call to READ must not return (it has to signal
                     ;; END-OF-LIST).
                     (read stream t nil t)
                     (%recoverable-reader-error
                      stream 'multiple-objects-following-consing-dot
                      :position-offset -1 :report 'ignore-object) ; not accurate
                     (setf (state-value client '*consing-dot-allowed-p*) nil)
                else
                  do (funcall function state object)))
      (end-of-list (condition)
        (let ((char (%character condition)))
          (unless (char= char close-char)
            (%recoverable-reader-error
             stream 'invalid-context-for-right-parenthesis
             :position-offset -1
             :expected-character close-char :found-character char
             :report 'ignore-trailing-right-paren)))
        (cond ((and (not (null eol-value))
                    (funcall function state eol-value)))
              ((eq state :tail)
               (%recoverable-reader-error
                stream 'object-must-follow-consing-dot
                :position-offset -1 :report '(use-replacement-object #1=1))
               ;; Recover by supplying a non-NIL object so that the
               ;; dotted nature of the list is preserved.
               (funcall function state #1#))))
      ((and end-of-file (not incomplete-construct)) (condition)
        (cond ((and (not (null eof-value))
                    (funcall function state eof-value)))
              (t
               (when (eq state :tail)
                 (%recoverable-reader-error
                  stream 'end-of-input-after-consing-dot
                  :stream-position (stream-position condition)
                  :report '(use-replacement-object #2=1))
                 ;; Recover by supplying a non-NIL object so that the
                 ;; dotted nature of the list is preserved.
                 (funcall function state #2#))
               (%recoverable-reader-error
                stream 'unterminated-list
                :stream-position (stream-position condition)
                :delimiter close-char :report 'use-partial-list)))))))

(defun %read-delimited-list (stream close-char)
  (alexandria:when-let ((list-reader *list-reader*))
    (return-from %read-delimited-list
      (funcall list-reader stream close-char)))
  ;; Continue with default list reader.
  (let ((reversed-result '())
        (tail nil))
    (flet ((element (state value)
             (case state
               (:proper (push value reversed-result))
               (:tail (setf tail value)))))
      (%read-list-elements stream #'element nil nil close-char t))
    (nreconc reversed-result tail)))

(defun read-delimited-list (char &optional (input-stream *standard-input*) recursive-p)
  (if recursive-p
      (%read-delimited-list input-stream char)
      (flet ((do-it ()
               (%read-delimited-list input-stream char)))
        (declare (dynamic-extent #'do-it))
        (call-as-top-level-read *client* #'do-it input-stream nil nil t))))

(define-compiler-macro read-delimited-list (&whole form
                                            char &optional input-stream recursive-p)
  (if (and (constantp recursive-p)
           (eval recursive-p))
      `(%read-delimited-list ,input-stream ,char)
      form))
