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
                #:map-nodes)
  (:import-from #:serapeum
                #:->)
  (:export #:local-image
           #:width
           #:height))
(in-package #:40ants-doc-full/commondoc/image)


(defclass local-image (common-doc:image)
  ((target-filename :initarg :target-filename
                    :type string
                    :reader target-filename)
   (width :initform nil
          :initarg :width
          :reader width)
   (height :initform nil
           :initarg :height
           :reader height)))


(defun full-path (path)
  (cond
    ((str:starts-with-p "asdf:" path)
     (destructuring-bind (prefix asdf-system-name path)
         (str:split ":" path
                    :limit 3)
       (declare (ignore prefix))
       (asdf:system-relative-pathname asdf-system-name
                                      path)))
    ((uiop:absolute-pathname-p path)
     path)
    (*current-asdf-system*
     (asdf:system-relative-pathname *current-asdf-system*
                                    path))
    (t
     (merge-pathnames path))))


(-> relative-path (string)
    (values string &optional))

(defun relative-path (path)
  (cond
    ((str:starts-with-p "asdf:" path)
     (destructuring-bind (prefix asdf-system-name relative-path)
         (str:split ":" path
                    :limit 3)
       (declare (ignore prefix asdf-system-name))
       (values relative-path)))
    ((uiop:absolute-pathname-p path)
     (cond
       (*current-asdf-system*
        (let ((probably-relative
                (enough-namestring path
                                   (asdf:system-relative-pathname *current-asdf-system*
                                                                     "./"))))
          (cond
            ((uiop:absolute-pathname-p probably-relative)
             ;; If filename is not inside the ASDF system, then probably it is
             ;; inside the current directory
             (let ((probably-relative
                     (enough-namestring path)))
               (cond
                 ((uiop:absolute-pathname-p probably-relative)
                  ;; If filename is not inside the current directory, then there is no
                  ;; way to learn what is the relative path would be:
                  (error "Unable to figure out relative path out from ~A path."
                         path))
                 (t
                  probably-relative))))
            (t
             probably-relative))))
       (t
        (let ((probably-relative
                (enough-namestring path)))

          (cond
            ((uiop:absolute-pathname-p probably-relative)
             ;; If filename is not inside the current directory, then there is no
             ;; way to learn what is the relative path would be:
             (error "Unable to figure out relative path out from ~A path."
                    path))
            (t
             probably-relative))))))
    (t
     ;; Path already was relative:
     path)))


(defun local-image (path &key target-filename description width height)
  "Creates a note for rendering an image in the documentation.
   Could be useful if you are constructing document from CommonDoc nodes.

   The SOURCE argument should point to a file on local filesystem.

   For example, here is how this function is used in the
   new [`PlantUML` plugin](https://40ants.com/doc-plantuml/):

   ```
   (defmethod to-commondoc ((diagram diagram))
     (uiop:with-temporary-file (:pathname pathname
                                :type \"png\"
                                :keep t)
       (ensure-directories-exist pathname)
   
       (40ants-plantuml:render (diagram-code diagram)
                            pathname)
       (let ((image
               (local-image
                (namestring pathname)
                :target-filename (diagram-filename diagram))))
         (common-doc:make-paragraph image))))
   ```

   This code creates a temporary file, renders a png image into it
   and then makes a paragraph with image, pointing to this temp file.
"
     
  (let ((full-path (full-path path)))
    (unless (probe-file full-path)
      (error "Image file \"~A\" not found"
             full-path))

    (make-instance
     'local-image
     ;; This is the path from where we will copy image file
     :source (namestring full-path)
     ;; And this is a relative path how we will refer the file
     ;; in the documentation:
     :target-filename (cond 
                        (target-filename
                         (namestring target-filename))
                        (t
                         (namestring (relative-path path))))
     :description description
     :width width
     :height height)))


(defun replace-images (document)
  (flet ((replacer (node)
           (typecase node
             ;; If node is already has needed class,
             ;; then leave it as is:
             (local-image
                node)
             (common-doc:image
                (let ((source (common-doc:source node)))
                  (if (or (str:starts-with-p "http:" source)
                          (str:starts-with-p "https:" source))
                    node
                    ;; We rewrite only nodes pointing to
                    ;; files on local filesystem:
                    (multiple-value-bind (source width height)
                        (extract-width-and-height source)
                      (local-image source
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
  (let* ((source-path
           ;; This is the path from where we will copy file:
           (common-doc:source obj))
         ;; Directory where we render all documentation files:
         (base-dir
           (uiop:ensure-directory-pathname 
            40ants-doc-full/builder/vars::*base-dir*))
         (target-path (uiop:merge-pathnames* (target-filename obj)
                                             base-dir))
         ;; Path of the current page:
         (page-uri (make-page-uri 40ants-doc-full/builder/vars::*current-page*))
         ;; This path will be used on the web page to refer the image:
         (relative-path (namestring
                         (uiop:enough-pathname target-path
                                               base-dir)))
         (new-source (make-relative-path page-uri
                                         relative-path))
         (src (if common-html.emitter:*image-format-control*
                (format nil common-html.emitter:*image-format-control*
                        new-source)
                new-source))
         (description (common-doc:description obj)))

    (unless (equal source-path
                   target-path)
      (log:info "Copying image from ~A to ~A" source-path target-path)
      (ensure-directories-exist target-path)
      (uiop:copy-file source-path
                      target-path))

    (with-html
      (:img :src src
            :alt description
            :title description
            :width (width obj)
            :height (height obj)))))

