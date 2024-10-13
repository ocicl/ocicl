(uiop:define-package #:40ants-doc-full/object-package-impl
  (:use #:cl)
  (:import-from #:40ants-doc/object-package
                #:object-package)
  (:import-from #:swank)
  (:import-from #:closer-mop
                ;; NO-LINT
                #:generic-function-name)
  (:import-from #:40ants-doc-full/utils))
(in-package #:40ants-doc-full/object-package-impl)


(defmethod object-package ((object t))
  (warn "Unable to figure out *package* for object ~S"
        object)
  nil)

(defmethod object-package ((object symbol))
  (symbol-package object))

(defmethod object-package ((object string))
  nil)

(defmethod object-package ((object package))
  object)

(defmethod object-package ((object asdf:system))
  nil)

(defmethod object-package ((object function))
  (object-package
   (swank-backend:function-name object)))

(defmethod object-package ((object generic-function))
  (object-package
   (generic-function-name object)))

(defmethod object-package ((object standard-method))
  ;; Method can be defined in other package than
  ;; a generic function.
  ;; Thus we need to find it's file and package
  (let* ((swank-response
           (swank:find-definition-for-thing object))
         (filename
           (getf (getf swank-response :location) :file))
         (package
           (when filename
             (40ants-doc-full/utils::file-package filename))))
    (if package
        package
        (call-next-method))))

(defmethod object-package ((object standard-class))
  (object-package
   (class-name object)))

#+sbcl
(defmethod object-package ((object sb-pcl::condition-class))
  (object-package
   (slot-value object 'sb-pcl::name)))
