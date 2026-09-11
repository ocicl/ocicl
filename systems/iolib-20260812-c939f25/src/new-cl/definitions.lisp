;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Various definers
;;;

(in-package :iolib/common-lisp)

(defmacro defconstant (name value &optional documentation
                       &environment env)
  (destructuring-bind (name &key (test ''eql))
      (alexandria:ensure-list name)
    (macroexpand-1
     `(alexandria:define-constant ,name ,value
        :test ,test
        ,@(when documentation `(:documentation ,documentation)))
     env)))
