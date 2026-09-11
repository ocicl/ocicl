(cl:in-package #:ecclesia)

(defun check-form-proper-list (form)
  (unless (proper-list-p form)
    (error 'form-must-be-proper-list :form form)))
