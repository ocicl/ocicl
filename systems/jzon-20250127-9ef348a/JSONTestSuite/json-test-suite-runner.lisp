(defpackage #:com.inuoe.json-test-suite-runner
  (:use #:cl)
  (:import-from #:uiop)
  (:export #:main))

(in-package #:com.inuoe.json-test-suite-runner)

(defun test-file (tester file rest-args timeout)
  (let ((p (uiop:launch-program (list* tester (uiop:native-namestring file) rest-args) :error-output *error-output*))
        (start (get-internal-real-time)))
    (loop
      (cond
        ((not (uiop:process-alive-p p))
         (return (uiop:wait-process p)))
        ((>= (- (get-internal-real-time) start) timeout)
         (uiop:terminate-process p :urgent t)
         (uiop:wait-process p)
         (return nil))
        (t
         (sleep 0.05))))))

(defun test-files (tester test-dir rest-args)
  (let ((json-files (uiop:merge-pathnames* (make-pathname :name :wild :type "json") test-dir))
        (any-failed nil))
    (dolist (file (directory json-files))
      (format t "~A.~A ... " (pathname-name file) (pathname-type file))
      (finish-output)
      (let ((result (test-file tester file rest-args (* 2 internal-time-units-per-second))))
        (case (char (pathname-name file) 0)
          (#\y
           (case result
             (0     (format t "OK~%"))
             (1     (format t "ERROR - FAILED~%") (setf any-failed t))
             ((nil) (format t "ERROR - TIMEOUT~%") (setf any-failed t))
             (t     (format t "ERROR - OTHER (0x~4,'0X)~%" result) (setf any-failed t))))
          (#\n
           (case result
             (0     (format t "ERROR - SUCCEEDED~%") (setf any-failed t))
             (1     (format t "OK~%"))
             ((nil) (format t "ERROR - TIMEOUT~%") (setf any-failed t))
             (t     (format t "ERROR - OTHER (0x~4,'0X)~%" result) (setf any-failed t))))
          (#\i
           (case result
             (0     (format t "OK~%"))
             (1     (format t "FAILED~%"))
             ((nil) (format t "TIMEOUT~%"))
             (t     (format t "OTHER (0x~4,'0X)~%" result))))))
      (finish-output))
    (not any-failed)))

(defun main (&rest argv)
  (prog ((tester (second argv))
         (test-dir (third argv))
         (rest-args (cdddr argv)))
     (unless (and tester (ignore-errors (uiop:run-program (list tester "--help"))
                                        t))
       (format *error-output* "First argument must be a program to test with.")
       (return 2))
     (unless (and test-dir (probe-file test-dir))
       (format *error-output* "Second argument must be a directory of files to test.")
       (return 2))
     (return (if (test-files (uiop:parse-native-namestring tester) (uiop:parse-native-namestring test-dir :ensure-directory t) rest-args)
                 0
                 1))))

