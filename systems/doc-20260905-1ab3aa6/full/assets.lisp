(uiop:define-package #:40ants-doc-full/assets
  (:use #:cl)
  (:import-from #:40ants-doc-full/commondoc/image
                #:local-image)
  (:import-from #:40ants-doc-full/commondoc/mapper
                #:map-nodes)
  (:import-from #:40ants-doc-full/commondoc/xref
                #:xref
                #:xref-locative
                #:xref-symbol)
  (:export #:defimage))
(in-package #:40ants-doc-full/assets)


(defclass image ()
  ((name :initarg :name
         :reader image-name)
   (source :initarg :source
           :reader image-source)
   (target-filename :initarg :target-filename
                    :reader image-target-filename)
   (description :initarg :description
                :reader image-description)
   (width :initarg :width
          :initform nil
          :type (or null (integer 1 *))
          :reader image-width)
   (height :initarg :height
           :initform nil
           :type (or null (integer 1 *))
           :reader image-height)))


(defun make-image (name source target-filename description &rest restargs
                    &key width height)
  (declare (ignore width height))
  (apply #'make-instance 'image
         :name name
         :source source
         :target-filename target-filename
         :description description
         restargs))


(defvar *images* (make-hash-table :test #'eq))


(defun image-symbol-p (symbol)
  (not (null (gethash symbol *images*))))


(defmethod 40ants-doc::transform-symbol-entry :around ((entry symbol))
  (or (gethash entry *images*)
      (call-next-method)))


(defmethod 40ants-doc/object-package::object-package ((image image))
  (symbol-package (image-name image)))


(defun pathname-designator-string (pathname-designator)
  (namestring (pathname pathname-designator)))


(defun register-image (name source &key target-filename description width height)
  (check-type name symbol)
  (check-type source (or string pathname))
  (check-type width (or null (integer 1 *)))
  (check-type height (or null (integer 1 *)))

  (let* ((source (pathname-designator-string source))
         (target-filename (pathname-designator-string
                           (or target-filename source)))
         (description (or description
                          (symbol-name name)))
         (new-image (make-image name source target-filename description
                                :width width
                                :height height))
         (old-image (gethash name *images*)))
    (when (and old-image
               (not (and (string= (image-source old-image)
                                  (image-source new-image))
                         (string= (image-target-filename old-image)
                                  (image-target-filename new-image))
                         (string= (image-description old-image)
                                  (image-description new-image))
                         (equal (image-width old-image)
                                (image-width new-image))
                         (equal (image-height old-image)
                                (image-height new-image)))))
      (error "Image ~S is already registered with different properties."
             name))
    (setf (gethash name *images*) new-image)
    new-image))


(defmacro defimage (name source &rest args)
  "Register NAME as a local image used in documentation.

SOURCE is a pathname designator.  By default, the file is copied to the
same relative pathname below the documentation output directory.  Use
:TARGET-FILENAME to choose another relative output pathname.

:WIDTH and :HEIGHT are positive pixel values. Only supplied dimensions are
emitted, so browsers preserve the image's aspect ratio when one is omitted.

Every unqualified occurrence of NAME in prose is rendered as an image."
  (check-type name symbol)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (register-image ',name ,source ,@args)))


(defun safe-target-filename-p (target-filename)
  (let ((pathname (pathname target-filename)))
    (and (not (uiop:absolute-pathname-p pathname))
         (not (member :up (pathname-directory pathname))))))


(defun validate-image (image)
  (unless (probe-file
           (40ants-doc-full/commondoc/image::resolve-local-image-path
            (image-source image)))
    (error "Image ~S source file does not exist: ~A"
           (image-name image)
           (image-source image)))

  (unless (safe-target-filename-p (image-target-filename image))
    (error "Image ~S target filename is not below the output directory: ~A"
           (image-name image)
           (image-target-filename image)))

  (maphash (lambda (name other-image)
             (when (and (not (eq name (image-name image)))
                        (string= (image-target-filename other-image)
                                 (image-target-filename image)))
               (error "Images ~S and ~S use the same target filename: ~A"
                      (image-name image)
                      name
                      (image-target-filename image))))
           *images*)
  image)


(defun image-to-local-image (image)
  (validate-image image)
  (local-image (image-source image)
               :target-filename (image-target-filename image)
               :description (image-description image)
               :width (image-width image)
               :height (image-height image)))


(defun replace-images (document)
  "Replace XREF nodes naming registered images with local image nodes."
  (map-nodes document
             (lambda (node)
               (if (and (typep node 'xref)
                        (null (xref-locative node)))
                   (let ((image (gethash (xref-symbol node) *images*)))
                     (if image
                         (image-to-local-image image)
                         node))
                   node))))
