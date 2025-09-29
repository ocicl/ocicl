(cl:in-package #:acclimation)

(defgeneric language (locale))

(defgeneric (setf language) (language locale))

(defclass locale ()
  ((%language :initarg :language :accessor language)
   ;; This slot holds an integer between 1 and 7 which is the ISO 8601
   ;; day number of the day that is to be considered the first day of
   ;; the week.  By default, we consider Monday to be the first one,
   ;; as ISO 8601 stipulates.
   (%week-start :initarg :week-start
		:initform 1
		:accessor week-start)))

(defvar *locale*)
