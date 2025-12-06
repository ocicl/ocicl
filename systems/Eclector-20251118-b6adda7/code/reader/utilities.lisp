(cl:in-package #:eclector.reader)

(defun numeric-token-length (token)
  (with-standard-io-syntax (length (prin1-to-string token))))

;;; The purpose of this function is to avoid dead code elimination and
;;; the associated compiler notes.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (notinline opaque-identity))
  (defun opaque-identity (value) value))

;;; Token utilities

(deftype token-string ()
  (load-time-value ; compute the expansion only once
   `(and (not (vector nil))
         ;; Try to figure out whether BASE-STRING is the same as
         ;; STRING.
         ,@(multiple-value-bind (result certainp)
               (subtypep 'character 'base-char)
             (when (and (not result) certainp)
               '((not base-string))))
         ;; Try to figure out whether adjusting a simple array makes it
         ;; non-simple.
         ,(if (opaque-identity ; avoid code deletion note
               (adjustable-array-p
                (adjust-array (make-array 1 :element-type 'character) 2)))
              '(array character 1)
              '(simple-array character 1)))
   t))

;;; This macro binds the names PUSH-CHAR, {START,END}-ESCAPE and
;;; FINALIZE to local functions that perform the accumulation of token
;;; characters and, optionally, escape ranges. If LAZY is true, the
;;; array of token characters is allocated lazily.
(defmacro with-token-info
    ((push-char (&optional start-escape end-escape) finalize &key lazy)
     &body body)
  `(let ((token ,(if lazy
                     'nil
                     '(make-array 10 :element-type 'character)))
         (index 0)
         ,@(when start-escape
             `((escape-ranges '()))))
     (declare (type ,(if lazy '(or null token-string) 'token-string) token)
              (type array-index index))
     (labels ((,push-char (char)
                (cond ,@(when lazy
                          `(((null token)
                             (setf token (make-array 10 :element-type 'character)))))
                      ((let ((length (length token)))
                         (unless (< index length)
                           (setf token (adjust-array token (* 2 length)))))))
                (setf (aref token index) char)
                (incf index)
                char)
              ,@(when start-escape
                  `((,start-escape (char)
                      (declare (ignore char))
                      (push (cons index nil) escape-ranges))))
              ,@(when end-escape
                  `((,end-escape ()
                      (setf (cdr (first escape-ranges)) index))))
              (,finalize ()
                (values (if (= index (length token))
                            token
                            (subseq token 0 index))
                        ,@(when start-escape
                            `((if (null escape-ranges)
                                  escape-ranges
                                  (nreverse escape-ranges)))))))
       ,@body)))

;;; This macro generates a state machine for reading a token and in
;;; particular handles single and multiple escapes and
;;; termination. STREAM and READTABLE are expressions which must
;;; evaluate to the stream and readtable respectively that will be
;;; used to read the token. The other required arguments are names of
;;; functions that will be called to handle certain situations.
(defmacro token-state-machine
    (stream readtable
     handle-char start-escape end-escape
     unterminated-single-escape unterminated-multiple-escape
     terminate)
  (once-only (stream readtable)
    (flet ((start-escape (char)
             `((setf escape-char ,char)
               ,@(when start-escape `((,start-escape ,char)))))
           (end-escape ()
             `((setf escape-char nil)
               ,@(when end-escape `((,end-escape))))))
      `(let ((escape-char nil))
         (flet ((read-char-handling-eof (context)
                  (let ((char (read-char ,stream nil nil t)))
                    (cond ((not (null char))
                           (values char (eclector.readtable:syntax-type
                                         ,readtable char)))
                          ((eq context :single-escape)
                           (,unterminated-single-escape escape-char)
                           ,@(end-escape)
                           (,terminate))
                          ((eq context :multiple-escape)
                           (,unterminated-multiple-escape escape-char)
                           ,@(end-escape)
                           (,terminate))
                          (t
                           (,terminate))))))
           (tagbody
            even-escapes
              (multiple-value-bind (char syntax-type)
                  (read-char-handling-eof nil)
                (ecase syntax-type
                  ((:whitespace :terminating-macro)
                   (unread-char char ,stream)
                   (,terminate))
                  (:single-escape
                   ,@(start-escape 'char)
                   (,handle-char (read-char-handling-eof syntax-type) t)
                   ,@(end-escape)
                   (go even-escapes))
                  (:multiple-escape
                   ,@(start-escape 'char)
                   (go odd-escapes))
                  ((:constituent :non-terminating-macro)
                   (,handle-char char nil)
                   (go even-escapes))))
            odd-escapes
              (multiple-value-bind (char syntax-type)
                  (read-char-handling-eof :multiple-escape)
                (case syntax-type
                  (:single-escape
                   (,handle-char (read-char-handling-eof syntax-type) t)
                   (go odd-escapes))
                  (:multiple-escape
                   ,@(end-escape)
                   (go even-escapes))
                  (t
                   (,handle-char char t)
                   (go odd-escapes))))))))))

;;; Escapes and case conversion

(defmacro update-escape-ranges
    (index escape-range-place remaining-escape-ranges-place)
  ;; Set ESCAPE-RANGE-PLACE to the escape range which contains INDEX
  ;; or NIL. If necessary, pop ranges off of
  ;; REMAINING-ESCAPE-RANGES-PLACE that are completely before INDEX.
  (once-only (index)
    `(loop while (and (not (null ,escape-range-place))
                      (>= ,index (the array-index (cdr ,escape-range-place))))
           do (pop ,remaining-escape-ranges-place)
              (setf ,escape-range-place (first ,remaining-escape-ranges-place))
           finally (return (and (not (null ,escape-range-place))
                                (<= (the array-index (car ,escape-range-place)) ,index))))))

(defun convert-according-to-readtable-case (readtable token escape-ranges)
  (declare (type token-string token))
  (macrolet
      ((do-token ((index-var escapep-var) &body body)
         `(loop with remaining-escape-ranges = escape-ranges
                with escape-range = (first remaining-escape-ranges)
                for ,index-var below (length token)
                for ,escapep-var = (update-escape-ranges
                                    ,index-var
                                    escape-range remaining-escape-ranges)
                do (progn ,@body)))
       (change-case (string-function char-function)
         `(cond ;; First common and easy case: no escapes. Change the
                ;; case of all characters in TOKEN at once.
                ((null escape-ranges)
                 (setf token (,string-function token)))
                ;; Second common and easy case: all characters are
                ;; escaped. Just return TOKEN.
                ((and (null (cdr escape-ranges))
                      (zerop (car (first escape-ranges)))
                      (= (length token) (cdr (first escape-ranges))))
                 nil)
                (t
                 (do-token (i escapep)
                   (unless escapep
                     (setf (aref token i) (,char-function (aref token i)))))))))
    (ecase (eclector.readtable:readtable-case readtable)
      (:upcase
       (change-case nstring-upcase char-upcase))
      (:downcase
       (change-case nstring-downcase char-downcase))
      (:preserve
       nil)
      (:invert
       (let ((upper-case-p nil)
             (lower-case-p nil))
         (if (null escape-ranges)
             (setf upper-case-p (find-if #'upper-case-p token)
                   lower-case-p (find-if #'lower-case-p token))
             (block nil
               (do-token (i escapep)
                 (unless escapep
                   (let ((char (aref token i)))
                     (cond ((upper-case-p char)
                            (setf upper-case-p t)
                            (when lower-case-p
                              (return)))
                           ((lower-case-p char)
                            (setf lower-case-p t)
                            (when upper-case-p
                              (return)))))))))
         (cond ((not upper-case-p)
                (change-case nstring-upcase char-upcase))
               ((not lower-case-p)
                (change-case nstring-downcase char-downcase))))))
    token))

;;; READ helpers

(declaim (inline skip-whitespace skip-whitespace*))

;;; Skip zero to one whitespace characters in STREAM. Return NIL when
;;; end-of-input is encountered before reading a character, return T
;;; otherwise.
(defun skip-whitespace (stream readtable)
  (let ((char (read-char stream nil nil t)))
    (cond ((null char)
           nil)
          ((not (eq (eclector.readtable:syntax-type readtable char)
                    :whitespace))
           (unread-char char stream)
           t)
          (t
           t))))

;;; Skip zero or more consecutive whitespace characters in
;;; STREAM. Return NIL when end-of-input is encountered before a
;;; non-whitespace character, return T otherwise.
(defun skip-whitespace* (stream readtable)
  (loop for i from 0
        for char = (read-char stream nil nil t)
        when (null char)
          do (return nil)
        when (not (eq (eclector.readtable:syntax-type readtable char)
                      :whitespace))
          do (unread-char char stream)
             (return t)))
