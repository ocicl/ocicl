(cl:in-package #:acclimation)

(setf *locale*
      (make-instance 'locale
	:language (make-instance 'english)))
