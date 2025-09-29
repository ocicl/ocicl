(cl:in-package #:eclector.reader)

(defun set-standard-syntax-types (readtable)
  (flet (((setf syntax) (syntax-type char)
           (setf (eclector.readtable:syntax-type readtable char)
                 syntax-type)))
    (setf (syntax #\Space)    :whitespace
          (syntax #\Tab)      :whitespace
          (syntax #\Linefeed) :whitespace
          (syntax #\Return)   :whitespace
          (syntax #\Page)     :whitespace
          (syntax #\\)        :single-escape
          (syntax #\|)        :multiple-escape)))

(defun set-standard-macro-characters (readtable)
  (loop for (char reader-macro) in '((#\( left-parenthesis)
                                     (#\) right-parenthesis)
                                     (#\' single-quote)
                                     (#\" double-quote)
                                     (#\; semicolon)
                                     (#\` backquote)
                                     (#\, comma))
        do (eclector.readtable:set-macro-character
            readtable char reader-macro)))

(defun set-standard-dispatch-macro-characters (readtable)
  (eclector.readtable:make-dispatch-macro-character
   readtable #\# t)

  ;; See HyperSpec section 2.4.8, Figure 2-19.
  ;;
  ;; Entries marked as "undefined" remain, well,
  ;; undefined. ECLECTOR.READTABLE:GET-DISPATCH-MACRO-CHARACTER
  ;; signals an appropriate error for those cases.
  (loop for (dispatch-char sub-char reader-macro)
        in '((#\# #\Backspace sharpsign-invalid)
             (#\# #\Tab       sharpsign-invalid)
             (#\# #\Newline   sharpsign-invalid)
             (#\# #\Linefeed  sharpsign-invalid)
             (#\# #\Page      sharpsign-invalid)
             (#\# #\Return    sharpsign-invalid)
             (#\# #\Space     sharpsign-invalid)

             (#\# #\<         sharpsign-invalid)
             (#\# #\)         sharpsign-invalid)

             (#\# #\'         sharpsign-single-quote)
             (#\# #\(         sharpsign-left-parenthesis)
             (#\# #\.         sharpsign-dot)
             (#\# #\\         sharpsign-backslash)
             (#\# #\b         sharpsign-b)
             (#\# #\x         sharpsign-x)
             (#\# #\o         sharpsign-o)
             (#\# #\r         sharpsign-r)
             (#\# #\*         sharpsign-asterisk)
             (#\# #\|         sharpsign-vertical-bar)
             (#\# #\a         sharpsign-a)
             (#\# #\:         sharpsign-colon)
             (#\# #\c         sharpsign-c)
             (#\# #\s         sharpsign-s)
             (#\# #\p         sharpsign-p)
             (#\# #\+         sharpsign-plus)
             (#\# #\-         sharpsign-minus)
             (#\# #\=         sharpsign-equals)
             (#\# #\#         sharpsign-sharpsign))
        do (eclector.readtable:set-dispatch-macro-character
            readtable dispatch-char sub-char reader-macro)))

(defun set-standard-syntax-and-macros (readtable)
  (set-standard-syntax-types readtable)
  (set-standard-macro-characters readtable)
  (set-standard-dispatch-macro-characters readtable))

(defparameter *standard-readtable*
  (let ((readtable (make-instance 'eclector.readtable.simple:readtable)))
    (set-standard-syntax-and-macros readtable)
    readtable))

(setf *readtable* (eclector.readtable:copy-readtable *standard-readtable*))
