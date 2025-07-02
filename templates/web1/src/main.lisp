;;; main.lisp
;;;
;;; SPDX-License-Identifier: <%= (or (@ license) "MIT") %>
;;;
;;; Copyright (C) 2025 <%= (or (@ author) "Your Name") %>

(in-package #:<%= @ app-name %>)

(defun make-app ()
  (let ((p (clingon:make-option :integer :short-name #\p :long-name "port" :key :port
                                         :description "port" :initial-value 8080))
        (s (clingon:make-option :integer :short-name #\s :long-name "slynk-port" :key :slynk-port
                                         :description "slynk-port" :initial-value nil)))
    (clingon:make-command
     :name    "<%= @ app-name %>"
     :version +version+
     :description "A web application"
     :authors (list "<%= (or (@ author) "Your Name") %>")
     :license "<%= (or (@ license) "MIT") %>"
     :usage ""
     :options (list p s)
     :handler (lambda (cmd)
                (let ((port (clingon:getopt cmd :port))
                      (slynk-port (clingon:getopt cmd :slynk-port)))

                  ;; Load environment variables if .env exists.
                  (let ((.env-pathname (merge-pathnames ".env")))
                    (handler-case
                        (.env:load-env .env-pathname)
                      (file-error (_)
                        (declare (ignore _)))
                      (.env:malformed-entry (_)
                        (declare (ignore _))
                        (fatal-error "Malformed entry in ~S" .env-pathname))
                      (.env:duplicated-entry (_)
                        (declare (ignore _))
                        (fatal-error "Duplicated entry in ~S" .env-pathname))))

                  (bt:with-lock-held (*server-lock*)
                    ;; Create the slynk server.  Allow connections from anywhere.
                    (when slynk-port
                      (slynk:create-server :port slynk-port :interface "0.0.0.0" :dont-close t)
                      (log:info "Started slynk server on port ~A" slynk-port))
                    (start-server port)
                    (log:info "Waiting for connections...")
                    ;; Wait forever.
                    (bt:condition-wait *shutdown-cv* *server-lock*))))

     :examples '(("Run web service on port 9090:"
                  . "<%= @ app-name %> -p 9090")))))

(defun main ()
  "The main entrypoint."
  (handler-case
      (clingon:run (make-app))
    (error (e)
      (format *error-output* "Error: ~A~%" e)
      (uiop:quit 1))))
