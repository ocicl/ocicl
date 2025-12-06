(in-package #:org.shirakumo.simple-inferiors)

(defvar *cwd* NIL)

(define-condition invalid-location-error (error)
  ((location :initarg :location :accessor location))
  (:report (lambda (c s) (format s "Invalid location ~s. Location does not exist on the system."
                                 (location c)))))

(defgeneric location (thing)
  (:method ((null null))
    (uiop:getcwd))
  (:method ((pathname pathname))
    pathname)
  (:method ((string string))
    (uiop:parse-native-namestring string :ensure-directory T)))

(defgeneric valid-location-p (thing)
  (:method (thing)
    (let ((location (ignore-errors (location thing))))
      (and location (uiop:probe-file* location)))))

(defun check-location (thing)
  (unless (valid-location-p thing)
    (error 'invalid-location-error :location thing)))

(defmacro with-chdir ((new-path) &body body)
  `(let ((*cwd* (merge-pathnames (location ,new-path)
                                 (location *cwd*))))
     ,@body))

(defmacro with-exchdir ((&optional (new-path NIL n-p-p)) &body body)
  (let ((old (gensym "OLD"))
        (new (gensym "NEW")))
    `(let* ((,old (or (ignore-errors (uiop:getcwd))
                      (user-homedir-pathname)))
            (,new (location ,(if n-p-p new-path '*cwd*)))
            (*cwd* ,new))
       (check-location ,new)
       (unwind-protect
            (progn
              (uiop:chdir ,new)
              ,@body)
         (uiop:chdir ,old)))))

(defmacro with-resolved-stream ((stream-ish &key args) &body body)
  `(call-with-resolved-stream (lambda (,stream-ish) ,@body) ,stream-ish ,@args))

(defun call-with-resolved-stream (func stream-ish &key args)
  (etypecase stream-ish
    (null
     (funcall func (make-broadcast-stream)))
    (stream
     (funcall func stream-ish))
    (pathname
     (let ((stream (apply #'open stream-ish args))
           (abort T))
       (unwind-protect
            (prog1 (funcall func stream)
              (setf abort NIL))
         (close stream :abort abort))))
    ((eql :string)
     (with-output-to-string (stream)
       (funcall func stream)))
    ((eql T)
     (funcall func *standard-output*))))

(defun copy-stream (input output &key consume-all (buffer 64))
  ;; Ok, this is kludgy. Let's see.
  ;; In order to avoid having to spawn threads to read the
  ;; two streams simultaneously, we have to somehow read only
  ;; as much as is available and then return, to let the
  ;; other stream be read. As such, READ-SEQUENCE by itself
  ;; is not a possible choice as it might well block until the
  ;; end of the program. The only other choice is to use
  ;; READ-CHAR-NO-HANG, which is impossibly inefficient and
  ;; will eat all of the resources. So we opt for a compromise
  ;; in which we check if new input is there by
  ;; READ-CHAR-NO-HANG and then use one of several methods to
  ;; read more or less blocking and more or less efficiently
  ;; from the stream.
  (when (open-stream-p input)
    (let ((char (read-char-no-hang input NIL)))
      (when char
        (write-char char output)
        (etypecase buffer
          ((eql :line)
           (if consume-all
               (loop for line = (read-line input NIL)
                     while line
                     do (write-line line output))
               (write-line (read-line input) output)))
          ((eql :character)
           (if consume-all
               (loop for char = (read-char input NIL)
                     while char
                     do (write-char char output))
               (loop for char = (read-char-no-hang input NIL)
                     while char
                     do (write-char char output))))
          (integer
           (let ((buf (make-array buffer :element-type 'character)))
             (if consume-all
                 (loop for size = (read-sequence buf input)
                       while (< 0 size) do (write-sequence buf output :end size))
                 (write-sequence buf output :end (read-sequence buf input))))))
        (finish-output output)))))

(defun stop-process (process &key (attempts 10) (sleep 0.1))
  (uiop:terminate-process process)
  (loop repeat attempts
        do (sleep sleep)
           (unless (uiop:process-alive-p process)
             (return))
        finally (uiop:terminate-process process :urgent T))
  process)

(defun ensure-process-stopped (process &rest args &key attempts sleep)
  (declare (ignore attempts sleep))
  (when (uiop:process-alive-p process)
    (apply #'stop-process process args))
  process)

(defun handle-process-sequential (copier process out-in out-out err-in err-out &key (cooldown 0.05) (stop-attempts 10) (stop-sleep 0.1))
  (unwind-protect
       (loop do (funcall copier out-in out-out)
                (funcall copier err-in err-out)
                (sleep cooldown)
             while (uiop:process-alive-p process))
    (ensure-process-stopped process :attempts stop-attempts :sleep stop-sleep)
    (funcall copier out-in out-out :consume-all T)
    (funcall copier err-in err-out :consume-all T)))

(defun handle-process-parallel (copier process out-in out-out err-in err-out &key (stop-attempts 10) (stop-sleep 0.1))
  (let ((err-thread (bt:make-thread (lambda () (funcall copier err-in err-out :consume-all T))))
        (out-thread (bt:make-thread (lambda () (funcall copier out-in out-out :consume-all T)))))
    (unwind-protect
         (loop while (uiop:process-alive-p process)
               do (sleep 0.1))
      (ensure-process-stopped process :attempts stop-attempts :sleep stop-sleep)
      (bt:join-thread err-thread)
      (bt:join-thread out-thread))))

(defun make-copier (buffer)
  (lambda (in out &rest args)
    (apply #'copy-stream in out :buffer buffer args)))

(defun ensure-copier (copier-ish)
  (etypecase copier-ish
    (function)
    ((or integer keyword) (make-copier copier-ish))
    (symbol (fdefinition copier-ish))))

#-sbcl (defvar *process-start-lock* (bt:make-lock "Process starting lock"))
(defun %start-process (program args &rest kargs)
  (#-sbcl bt:with-lock-held #-sbcl (*process-start-lock*) #+sbcl progn
   (apply #'uiop:launch-program (list* program args) :directory *cwd* kargs)))

(define-condition inferior-process-failed-condition ()
  ((program :initarg :program :accessor failed-program)
   (args :initarg :args :accessor failed-args)
   (exit :initarg :exit :accessor failed-exit))
  (:report (lambda (c s) (format s "Inferior process~&  ~s~&with arguments~&  ~s~&exited with code ~a."
                                 (failed-program c) (failed-args c) (failed-exit c)))))

(define-condition inferior-process-failed-error (error inferior-process-failed-condition)
  ())

(define-condition inferior-process-failed-warning (warning inferior-process-failed-condition)
  ())

(defun run (program args &key input output error (on-non-zero-exit :return) (handler #'handle-process-sequential) (copier :character))
  (ecase on-non-zero-exit ((NIL :return :error :warn)))
  (let ((copier (ensure-copier copier)))
    (with-resolved-stream (output)
      (with-resolved-stream (error)
        (let* ((process (%start-process program args :output :stream
                                                     :error-output :stream
                                                     :input (case input
                                                              ((T) *standard-input*)
                                                              (T input))))
               (out-in (uiop:process-info-output process))
               (err-in (uiop:process-info-error-output process)))
          (unwind-protect
               (funcall handler copier process out-in output err-in error)
            (uiop:close-streams process))
          (let ((exit (uiop:wait-process process)))
            (if (= 0 exit)
                exit
                (case on-non-zero-exit
                  ((NIL) NIL)
                  (:return exit)
                  (:error (error 'inferior-process-failed-error :program program :args args :exit exit))
                  (:warn (error 'inferior-process-failed-warning :program program :args args :exit exit))))))))))
