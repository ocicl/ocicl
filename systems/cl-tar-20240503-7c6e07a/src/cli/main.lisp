;;;; CLI
;;;;
;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(cl:in-package #:tar-cli)

(defparameter *blocking-factor*
  (adopt:make-option
   :blocking-factor
   :short #\b
   :help "Set the blocking factor used for the archive"
   :initial-value 20
   :key #'parse-integer
   :parameter "BLOCKING-FACTOR"
   :reduce #'adopt:last))

(defparameter *option-version*
  (adopt:make-option
   :version
   :long "version"
   :help "Print version and exit"
   :reduce (constantly t)))

(defparameter *option-verbose*
  (adopt:make-option
   :verbose
   :short #\v
   :help "Add verbosity"
   :reduce (constantly t)))

(defparameter *directory*
  (adopt:make-option
   :directory
   :short #\C
   :help "Change to directory before operation (creating it if necessary)"
   :parameter "DIRECTORY"
   :reduce #'adopt:last))

(defparameter *gzip*
  (adopt:make-option
   :gzip
   :result-key :compression
   :short #\z
   :help "(De)compress the archive using gzip."
   :initial-value :auto
   :reduce (constantly :gzip)))

(defparameter *extract*
  (adopt:make-option
   :extract
   :result-key :operation
   :short #\x
   :help "Extract the archive"
   :reduce (constantly :extract)))

(defparameter *create*
  (adopt:make-option
   :create
   :result-key :operation
   :short #\c
   :help "Create an archive"
   :reduce (constantly :create)))

(defparameter *file*
  (adopt:make-option
   :file
   :short #\f
   :help "Specify a file. A hyphen designates the standard input or output"
   :parameter "FILE-NAME"
   :reduce #'adopt:last))

(defparameter *help*
  (adopt:make-option
   :help
   :long "help"
   :help "Print help and exit"
   :reduce (constantly t)))

(defparameter *ui*
  (adopt:make-interface
   :name "cl-tar"
   :usage "tar -x/c OPTIONS"
   :summary "Tar archive tool written in Common Lisp"
   :help "cl-tar creates and extracts tar archives"
   :contents
   (list *help*
         *option-version*
         *option-verbose*
         *extract*
         *create*
         *file*
         *blocking-factor*
         *gzip*
         *directory*)))

(defun extract (options)
  (tar:with-open-archive (a (gethash :file options)
                            :direction :input :blocking-factor (gethash :blocking-factor options)
                            :compression (gethash :compression options))
    (tar-extract:extract-archive a)))

(defun create (options args)
  (tar:with-open-archive (a (gethash :file options)
                            :direction :output :blocking-factor (gethash :blocking-factor options)
                            :compression (gethash :compression options))
    (tar-create:create-archive a args :recursep t)))

(defun main ()
  (multiple-value-bind (args options)
      (adopt:parse-options *ui* (uiop:command-line-arguments))
    (when (gethash :help options)
      (adopt:print-help-and-exit *ui*))
    (when (gethash :version options)
      (format t "~A~%" (asdf:component-version (asdf:find-system "tar")))
      (when (gethash :verbose options)
        (format t "~%~A ~A~%ASDF ~A~%~%" (lisp-implementation-type) (lisp-implementation-version) (asdf:asdf-version))
        (format t "~S~%" *features*))
      (uiop:quit))
    (when (null (gethash :file options))
      (error "You must specify a file"))
    ;; Resolve the file name against current directory.
    (if (equal (gethash :file options) "-")
        (setf (gethash :file options) (if (eql (gethash :operation options) :create)
                                          *standard-output*
                                          *standard-input*))
        (setf (gethash :file options) (uiop:ensure-absolute-pathname
                                       (merge-pathnames (gethash :file options)
                                                        (uiop:getcwd)))))
    ;; Figure out what we're doing...
    (let ((dir (uiop:ensure-directory-pathname (or (gethash :directory options)
                                                   (uiop:getcwd)))))
      (ensure-directories-exist dir)
      (let ((*default-pathname-defaults* dir))
        (case (gethash :operation options)
          (:extract
           (extract options))
          (:create
           (create options args))
          (t (error "You must specify exactly one of -c or -x")))))))
