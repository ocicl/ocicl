(uiop:define-package #:40ants-doc/mito
  (:use #:cl)
  (:import-from #:mito)
  (:import-from #:closer-mop
                #:generic-function-methods
                #:specializer-direct-generic-functions
                #:method-specializers)
  (:documentation "This package contains a helper which sets source slot for all relation reader methods of Mito's table class.

Without this helper you will get these warnings when trying to build a documentation which uses Mito table classes:

```
  WARNING:
     No source location found for reference
     #<REFERENCE MESSAGE-CHAT (40ANTS-DOC/LOCATIVES:READER MESSAGE)>:
     Error: DEFINITION-SOURCE of method #<STANDARD-METHOD {12015E9823}> did not contain meaningful information.
```

This helper sets methods source to the source of the table class.
")
  (:export #:fixed-dao-table-class))
(in-package #:40ants-doc/mito)


#+sbcl
(defun set-methods-sources (class)
  "Sets source slot for all relation reader methods of Mito's table class."
  (let ((class-name (class-name class)))
    (flet ((specializes-on-our-class (method)
             (loop for specializer in (method-specializers method)
                   thereis (and (typep specializer 'class)
                                (eql (class-name specializer) class-name)))))
      (when class
        (loop for gf in (specializer-direct-generic-functions class)
              do (loop for method in (generic-function-methods gf)
                       for method-source = (slot-value method 'sb-pcl::source)
                       when (and (null method-source)
                                 (specializes-on-our-class method))
                         do (setf (slot-value method 'sb-pcl::source)
                                  (slot-value class 'sb-pcl::source)))))))
  (values))

#-sbcl
(defun set-methods-sources (class)
  "Sets source slot for all relation reader methods of Mito's table class."
  (values))


(defclass fixed-dao-table-class (mito:dao-table-class)
  ())


(defmethod initialize-instance :around ((class fixed-dao-table-class) &rest initargs)
  (declare (ignore initargs))
  (multiple-value-prog1
      (call-next-method)
    (set-methods-sources class)))


(defmethod reinitialize-instance :around ((class fixed-dao-table-class) &rest initargs)
  (declare (ignore initargs))
  (multiple-value-prog1
      (call-next-method)
    (set-methods-sources class)))

