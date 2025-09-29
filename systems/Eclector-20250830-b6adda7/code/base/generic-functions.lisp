(cl:in-package #:eclector.base)

;;; Source location protocol

(defgeneric source-position (client stream)
  (:method ((client t) (stream t))
    (file-position stream)))

(defgeneric make-source-range (client start end)
  (:method ((client t) (start t) (end t))
    (cons start end)))
