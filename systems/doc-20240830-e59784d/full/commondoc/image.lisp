(uiop:define-package #:40ants-doc-full/commondoc/image
  (:use #:cl)
  (:import-from #:common-html.emitter
                #:define-emitter)
  (:import-from #:str)
  (:import-from #:log)
  (:import-from #:common-doc)
  (:import-from #:40ants-doc-full/commondoc/html
                #:with-html)
  (:import-from #:40ants-doc-full/commondoc/page
                #:make-page-uri)
  (:import-from #:40ants-doc-full/utils
                #:make-relative-path)
  (:import-from #:cl-ppcre
                #:do-register-groups)
  (:import-from #:40ants-doc-full/builder/vars
                #:*current-asdf-system*)
  (:import-from #:40ants-doc-full/commondoc/mapper
                #:map-nodes))
(in-package #:40ants-doc-full/commondoc/image)

(defclass local-image (common-doc:image)
  ((width :initform nil
          :initarg :width
          :reader width)
   (height :initform nil
          :initarg :height
          :reader height)))

(defun full-path (relative-path)
  (if *current-asdf-system*
      (asdf:system-relative-pathname *current-asdf-system*
                                     relative-path)
      relative-path))

(defun make-local-image (relative-path &key width height)
  (unless (probe-file (full-path relative-path))
    (error "Image file \"~A\" not found"
           (full-path relative-path)))
  ;; Here we are saving a relative path
  ;; because we'll need it later for makeing
  ;; the target path:
  (make-instance 'local-image :source relative-path
                              :width width
                              :height height))

(defun replace-images (document)
  (flet ((replacer (node)
           (typecase node
             (common-doc:image
              (let ((source (common-doc:source node)))
                (if (or (str:starts-with-p "http:" source)
                        (str:starts-with-p "https:" source))
                    node
                    (multiple-value-bind (source width height)
                        (extract-width-and-height source)
                      (make-local-image source
                                        :width width
                                        :height height)))))
             (t node))))
    (map-nodes document #'replacer)))

(defun extract-width-and-height (path)
  "Returns 3 values, real path, width and height. Width and height might be NIL.

   For example, on \"blah.png{height=400,width=300}\" it will return:

   blah.png
   300
   400"
  
  (let ((width nil)
        (height nil))
    (do-register-groups (name value) ("[{,](.*?)=(.[^,}]*)"
                                      path)
      (cond
        ((string-equal name "width")
         (setf width
               value))
        ((string-equal name "height")
         (setf height
               value))
        (t
         (error "Parameter \"~A\" is not supported."
                name))))

    (values
     (first (str:split "{" path))
     width
     height)))

(define-emitter (obj local-image)
  "Emit a local-image and move referenced image into the HTML documentation folder."
  (let* ((original-path (common-doc:source obj))
         (source-path (full-path original-path))
         (target-path (uiop:merge-pathnames* original-path
                                             (uiop:merge-pathnames* #P"images/"
                                                                    (uiop:ensure-directory-pathname
                                                                     40ants-doc-full/builder/vars::*base-dir*))))
         (page-uri (make-page-uri 40ants-doc-full/builder/vars::*current-page*))
         (new-source (make-relative-path page-uri
                                         (format nil "images/~A"
                                                 original-path)))
         (src (if common-html.emitter:*image-format-control*
                  (format nil common-html.emitter:*image-format-control*
                          new-source)
                  new-source))
         (description (common-doc:description obj)))

    (ensure-directories-exist target-path)
    (log:info "Copying image from ~A to ~A" source-path target-path)
    (uiop:copy-file source-path
                    target-path)
    (with-html
      (:img :src src
            :alt description
            :title description
            :width (width obj)
            :height (height obj)))))
