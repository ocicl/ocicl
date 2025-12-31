;;;; reader.lisp - Source reader with position tracking
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.parser)

(defstruct (source-reader (:conc-name reader-)
                          (:constructor %make-source-reader))
  "Wrapper around a stream with position tracking."
  (stream nil :type stream)
  (line 1 :type (integer 1))
  (column 1 :type (integer 1))
  (prev-line 1 :type (integer 1))
  (prev-column 1 :type (integer 1))
  (unread-char nil :type (or null character)))

(defun make-source-reader (stream)
  "Create a source reader from STREAM."
  (%make-source-reader :stream stream))

(defun make-string-reader (string)
  "Create a source reader from STRING."
  (make-source-reader (make-string-input-stream string)))

(defun reader-peek (reader)
  "Peek at next character without consuming it."
  (or (reader-unread-char reader)
      (peek-char nil (reader-stream reader) nil nil)))

(defun reader-read (reader)
  "Read and consume next character, updating position."
  (let ((char (or (reader-unread-char reader)
                  (read-char (reader-stream reader) nil nil))))
    (when (reader-unread-char reader)
      (setf (reader-unread-char reader) nil))
    (when char
      ;; Save previous position for unread
      (setf (reader-prev-line reader) (reader-line reader)
            (reader-prev-column reader) (reader-column reader))
      ;; Update position
      (cond
        ((char= char #\Newline)
         (incf (reader-line reader))
         (setf (reader-column reader) 1))
        (t
         (incf (reader-column reader)))))
    char))

(defun reader-unread (reader char)
  "Push CHAR back to be read again. Only one unread is supported."
  (when (reader-unread-char reader)
    (error "Cannot unread more than one character"))
  (setf (reader-unread-char reader) char)
  ;; Restore previous position
  (setf (reader-line reader) (reader-prev-line reader)
        (reader-column reader) (reader-prev-column reader))
  char)

(defun reader-eof-p (reader)
  "Return T if at end of input."
  (null (reader-peek reader)))

(defun reader-position (reader)
  "Return current position as source-position."
  (make-source-position (reader-line reader)
                        (reader-column reader)))

(defun reader-read-while (reader predicate)
  "Read characters while PREDICATE returns true. Returns string."
  (with-output-to-string (out)
    (loop for char = (reader-peek reader)
          while (and char (funcall predicate char))
          do (write-char (reader-read reader) out))))

(defun reader-read-until (reader end-char)
  "Read characters until END-CHAR is seen (not consumed). Returns string."
  (with-output-to-string (out)
    (loop for char = (reader-peek reader)
          while (and char (char/= char end-char))
          do (write-char (reader-read reader) out))))

(defun reader-skip-while (reader predicate)
  "Skip characters while PREDICATE returns true."
  (loop for char = (reader-peek reader)
        while (and char (funcall predicate char))
        do (reader-read reader)))
