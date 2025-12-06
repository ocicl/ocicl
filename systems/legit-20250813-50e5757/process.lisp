(in-package #:org.shirakumo.legit)

(defvar *git-output* T)
(defvar *git-errors* NIL)
(defvar *git-input* NIL)

(define-condition git-error (error)
  ((command :initarg :command)
   (exit-code :initarg :exit-code)
   (error-text :initarg :error-text))
  (:default-initargs
   :command (error "COMMAND required.")
   :exit-code (error "EXIT-CODE required.")
   :error-text NIL)
  (:report (lambda (c s) (format s "Executing git command~%  ~a~%failed with exit code ~s.~@[ Git reported:~%  ~a~]"
                                 (slot-value c 'command) (slot-value c 'exit-code) (slot-value c 'error-text)))))

(defun %resolve-stream (streamish)
  (case streamish
    ((T) *standard-output*)
    ((NIL) (make-broadcast-stream))
    (T streamish)))

(defun flatten (args)
  (let ((list ()))
    (dolist (arg args (nreverse list))
      (if (listp arg)
          (dolist (a (flatten arg))
            (cl:push a list))
          (cl:push arg list)))))

(defun coerce-to-cmdarg (thing)
  (etypecase thing
    (string thing)
    (pathname (uiop:native-namestring thing))
    (number (princ-to-string thing))))

(defun run-git (&rest cmdargs)
  (let* ((error-capture (make-string-output-stream))
         (*git-errors* (if *git-errors*
                           (make-broadcast-stream (%resolve-stream *git-errors*) error-capture)
                           error-capture))
         (cmdargs (mapcar #'coerce-to-cmdarg (flatten cmdargs)))
         (exit (run "git" cmdargs :output *git-output* :error *git-errors* :input *git-input*))
         (error-text (string-right-trim '(#\Newline) (get-output-stream-string error-capture))))
    (case exit
      (0 T)
      (T (error 'git-error :command (cons "git" cmdargs) :exit-code exit :error-text error-text)))))
