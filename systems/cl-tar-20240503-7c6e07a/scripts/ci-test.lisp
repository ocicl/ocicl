(in-package :cl-user)

(require "asdf")

(defvar *failed-p* nil)

(unless (null (nth-value 1 (ignore-errors (asdf:test-system "tar"))))
  (setf *failed-p* t))

(unless (null (nth-value 1 (ignore-errors (asdf:test-system "tar/simple-extract"))))
  (setf *failed-p* t))

(unless (null (nth-value 1 (ignore-errors (asdf:test-system "tar/extract"))))
  (setf *failed-p* t))

(unless (null (nth-value 1 (ignore-errors (asdf:test-system "tar/create"))))
  (setf *failed-p* t))

(when *failed-p*
  (uiop:quit 1))
