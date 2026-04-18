(uiop:define-package #:40ants-doc-full/locatives/include
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
                #:locate-and-find-source
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:40ants-doc/locatives
                #:include)
  (:import-from #:40ants-doc/reference)
  (:import-from #:babel)
  (:import-from #:common-doc)
  (:import-from #:fare-utils)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader)
  (:import-from #:40ants-doc-full/commondoc/builder)
  (:import-from #:40ants-doc/core
                #:exportable-locative-type-p)
  (:import-from #:40ants-doc/source-api))
(in-package #:40ants-doc-full/locatives/include)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(define-locative-type include (source &key lang)
  """Refers to a region of a file. SOURCE can be a string or a
  pathname in which case the whole file is being pointed to or it can
  explicitly supply START, END locatives. INCLUDE is typically used to
  include non-lisp files in the documentation (say markdown or elisp
  as in the next example) or regions of lisp source files. This can
  reduce clutter and duplication.

  ```lisp
  (defsection example-section ()
    (pax.el (include #.(asdf:system-relative-pathname :40ants-doc "elisp/pax.el")
                     :lang "elisp"))
    (foo-example (include (:start (foo function)
                           :end (end-of-foo-example variable))
                          :lang "commonlisp")))

  (defun foo (x)
    (1+ x))

  ;;; Since file regions are copied verbatim, comments survive.
  (defmacro bar ())

  ;;; This comment is the last thing in FOO-EXAMPLE's
  ;;; documentation since we use the dummy END-OF-FOO-EXAMPLE
  ;;; variable to mark the end location.
  (defvar end-of-foo-example)

  ;;; More irrelevant code follows.
  ```

  In the above example, pressing `M-.` on `pax.el` will open the
  `src/pax.el` file and put the cursor on its first character. `M-.`
  on `FOO-EXAMPLE` will go to the source location of the `(asdf:system
  locative)` locative.

  When documentation is generated, the entire `pax.el` file is
  included in the markdown as a code block. The documentation of
  `FOO-EXAMPLE` will be the region of the file from the source location
  of the START locative (inclusive) to the source location of the END
  locative (exclusive). START and END default to the beginning and end
  of the file, respectively.

  Note that the file of the source location of :START and :END must be
  the same. If SOURCE is pathname designator, then it must be absolute
  so that the locative is context independent.
  """)


(defmethod exportable-locative-type-p ((locative-type (eql 'include)))
  nil)

(defmethod locate-object (symbol (locative-type (eql 'include))
                          locative-args)
  (destructuring-bind (source &key lang) locative-args
    (declare (ignore source lang))
    (40ants-doc/reference::make-reference symbol (cons locative-type locative-args))))


(defmethod locate-and-find-source (symbol (locative-type (eql 'include))
                                   locative-args)
  (multiple-value-bind (file start)
      (include-region (first locative-args))
    (assert file)
    `(:location
      (:file ,(namestring file))
      (:position ,(1+ start))
      nil)))


(defmethod 40ants-doc-full/commondoc/builder::reference-to-commondoc ((symbol symbol) (locative-type (eql 'include)) locative-args)
  (destructuring-bind (source &key 
                              lang)
      locative-args
    (common-doc:make-code-block lang
                                (common-doc:make-text
                                 (multiple-value-call #'file-subseq
                                   (include-region source))))))


;;; Return the filename and start, end positions of the region to be
;;; included.
(defun include-region (source)
  (cond ((or (stringp source) (pathnamep source))
         (assert (uiop/pathname:absolute-pathname-p source) ()
                 "Pathnames given as the SOURCE argument of the ~
                 INCLUDE locative must be absolute, but ~S is not."
                 source)
         (values source 0 nil))
        ((and source (listp source))
         (destructuring-bind (&key start end) source
           (let* ((start-reference (40ants-doc/reference:resolve
                                    (40ants-doc/core::entry-to-reference start)))
                  (end-reference (40ants-doc/reference:resolve
                                  (40ants-doc/core::entry-to-reference end)))
                  (start (40ants-doc/source-api:find-source start-reference))
                  (end (40ants-doc/source-api:find-source end-reference)))
             (when start
               (check-location start))
             (when end
               (check-location end))
             (let ((start-file (when start (location-file start)))
                   (start-position (when start (location-position start)))
                   (end-file (when end (location-file end)))
                   (end-position (when end (location-position end))))
               (when (and start end)
                 (assert (string= (namestring (truename start-file))
                                  (namestring (truename end-file)))
                         () "Include starts in file ~S and ends in ~
                         another file ~S." start-file end-file))

               (when (< end-position
                        start-position)
                 (error "Something went wrong end position ~A of ~S goes before start position ~A of ~S"
                        end-position
                        (getf source :end)
                        start-position
                        (getf source :start)))
               (values (or start-file end-file) start-position end-position)))))
        (t
         (error "~@<Malformed include source ~S.~:@>" source))))

;;; Check that LOCATION looks like this:
;;;
;;;     (:location
;;;      (:file "filename")
;;;      (:position 1)
;;;      (:snippet ""))
(defun check-location (location)
  (assert (listp location) () "Location ~S is not a list." location)
  (assert (eq (first location) :location) ()
          "Location ~S does not start with ~S." location :location)
  (assert (and (location-file location)
               (location-position location))
          () "Location ~S should contain: ~S."
          location '(:file :position)))

(defun location-file (location)
  (or (second (find :file (rest location) :key #'first))
      ;; If function was redefined using C-c C-c in Emacs, then
      ;; location will have :BUFFER-AND-FILE instead of :FILE.
      (second (find :buffer-and-file (rest location) :key #'first))))

(defun location-position (location)
  (or (fare-utils:aif (second (find :position (rest location) :key #'first))
                      (1- fare-utils:it))
      ;; If function was redefined using C-c C-c in Emacs, then
      ;; location will have :OFFSET instead of :POSITION.
      (second (find :offset (rest location) :key #'first))))

;; TODO: Find why this get called three times when I have only one include in my document :(
(defun file-subseq (pathname &optional start end)
  ;; START and END arguments contains offsets in bytes,
  ;; thus to process Unicode symbols, encoded in UTF-8,
  ;; we need to read data as bytes and transfrom into
  ;; the characters:
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let* ((*print-pretty* nil)
           (start (or start 0))
           (file-len (file-length stream))
           (end (min (or end file-len)
                     file-len))
           (buffer-size (- end start))
           (buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
      (file-position stream start)
      (read-sequence buffer stream)
      (babel:octets-to-string buffer))))

