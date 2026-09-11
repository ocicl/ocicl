;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Reader utils
;;;

(in-package :iolib/base)

;; Literal object dispatcher

(defconstant +read-literal-dispatch-char+ #\#)
(defconstant +read-literal-sub-char+ #\/)

(defun read-literal-dispatcher (stream char arg)
  (declare (ignore char arg))
  (let* ((literal-syntax-name
          (with-output-to-string (s)
            (loop :for c := (read-char stream t nil t)
                  :do (if (char= c +read-literal-sub-char+)
                          (loop-finish)
                          (write-char c s)))))
         (literal-reader
          (getf (symbol-plist (read-from-string literal-syntax-name))
                'read-literal-fn)))
    (if (functionp literal-reader)
        (funcall literal-reader stream)
        (error 'unknown-literal-syntax
               :stream stream
               :name literal-syntax-name))))

(defun enable-literal-reader* (&optional (readtable *readtable*))
  (set-dispatch-macro-character +read-literal-dispatch-char+
                                +read-literal-sub-char+
                                'read-literal-dispatcher
                                readtable))

(defmacro enable-literal-reader (&optional (readtable '*readtable*))
  `(eval-when (:compile-toplevel)
     (setf *readtable* (copy-readtable ,readtable))
     (enable-literal-reader*)))

(defmacro define-literal-reader (name (stream) &body body)
  `(setf (getf (symbol-plist ',name) 'read-literal-fn)
         (lambda (,stream) ,@body)))

(defmacro fcase (&body clauses)
  `(cond
     ,@(loop :for c :in clauses
             :for test := (car c)
             :for forms := (cdr c)
             :collect `((featurep ',test) ,@forms))))
