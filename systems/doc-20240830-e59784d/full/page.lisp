(uiop:define-package #:40ants-doc-full/page
  (:use #:cl)
  (:import-from #:40ants-doc
                #:section-title
                #:section-name)
  (:import-from #:40ants-doc/object-package)
  (:import-from #:40ants-doc-full/commondoc/builder)
  ;; TODO: solve circular dependency :(
  ;; (:import-from #:40ants-doc-full/commondoc/page)
  (:import-from #:40ants-doc-full/commondoc/format
                #:ensure-format-class-name)
  (:import-from #:str
                #:title-case)
  (:export #:make-page
           #:ensure-page
           #:page-format
           #:base-filename
           #:page-base-dir
           #:page-base-url
           #:page-sections
           #:page-title))
(in-package #:40ants-doc-full/page)


(defclass page-common-mixin ()
  ((title :initarg :title
          :reader page-title
          :type string)
   (base-filename :initarg :base-filename
                  :reader base-filename
                  :type string
                  :documentation "Keeps a filename without extension.
                                  Extension will be added later during
                                  documentation generation phase.")
   (base-dir :initform nil
             :initarg :base-dir
             :reader page-base-dir)
   (base-url :initform nil
             :initarg :base-url
             :reader page-base-url)
   (format :initform nil
           :initarg :format
           :reader page-format
           :type (or null symbol))))


(defmethod initialize-instance :after ((instance page-common-mixin) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value instance 'format)
        (ensure-format-class-name
         (slot-value instance 'format))))


(defmethod page-base-url ((page (eql :no-page)))
  nil)

(defmethod page-format ((page (eql :no-page)))
  nil)


(defclass page (page-common-mixin)
  ((sections :initarg :sections
             :reader page-sections)))


(defmethod 40ants-doc/object-package::object-package ((obj page))
  nil)


(defun make-base-filename (sections)
  (unless sections
    (error "Page should have at least one section."))
  (concatenate 'string
               (string-downcase
                (string-trim "@"
                             (section-name
                              (first sections))))))


(defun make-page (sections &key title
                                base-filename
                                base-dir
                                base-url
                                format)
  (let* ((sections (uiop:ensure-list sections))
         (first-section (first sections))
         (real-title (or title
                         (section-title first-section)
                         (title-case (symbol-name
                                      (section-name first-section)))))
         (base-filename (or base-filename
                            (make-base-filename sections))))
    (make-instance 'page
                   :title real-title
                   :sections sections
                   :base-filename base-filename
                   :base-dir base-dir
                   :base-url base-url
                   :format format)))


(defun ensure-page (obj)
  (check-type obj (or 40ants-doc:section page))
  (typecase obj
    (page obj)
    (t (make-page obj))))


(defmethod 40ants-doc-full/commondoc/builder:to-commondoc ((obj page))
  (uiop:symbol-call :40ants-doc-full/commondoc/page :make-page
                    (mapcar #'40ants-doc-full/commondoc/builder:to-commondoc
                            (page-sections obj))
                    (base-filename obj)
                    :title (page-title obj)
                    :format (page-format obj)
                    :base-dir (page-base-dir obj)
                    :base-url (page-base-url obj)))
