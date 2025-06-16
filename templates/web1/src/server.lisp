;;; server.asd
;;;
;;; SPDX-License-Identifier: <%= (or (@ license) "MIT") %>
;;;
;;; Copyright (C) 2025 <%= (or (@ author) "Your Name") %>

(in-package :<%= @ app-name %>)

(version-string:define-version-parameter +version+ :<%= @ app-name %>)

;; ----------------------------------------------------------------------------
;; Machinery for managing the execution of the server.
(defvar *shutdown-cv* (bt:make-condition-variable))
(defvar *server-lock* (bt:make-lock))
(defvar *acceptor* nil)

;; Easy-routes setup
(defclass my-acceptor (easy-routes:easy-routes-acceptor)
  ())

(defun app-root ()
  (uiop:getcwd))

(defparameter +static-dispatch-table+
  (list
   (hunchentoot:create-folder-dispatcher-and-handler
    "/images/" (fad:pathname-as-directory
                (merge-pathnames "static/images/" (app-root))))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/js/" (fad:pathname-as-directory
            (merge-pathnames "static/js/" (app-root))))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/css/" (fad:pathname-as-directory
             (merge-pathnames "static/css/" (app-root))))))

(easy-routes:defroute index ("/" :method :get) ()
  "Main index page"
  (setf (hunchentoot:content-type*) "text/html")
  "<!DOCTYPE html>
<html>
<head>
    <title>My Web Application</title>
    <link rel='stylesheet' href='/css/style.css'>
</head>
<body>
    <h1>Welcome to My Web Application</h1>
    <p>This is the index page created with easy-routes.</p>
    <script src='/js/app.js'></script>
</body>
</html>")

(defun start-server (port)
  "Start the web application with easy-routes."
  (setf hunchentoot:*catch-errors-p* t)
  (setf hunchentoot:*show-lisp-errors-p* t)
  (setf hunchentoot:*show-lisp-backtraces-p* t)

  (log:info "Static content directory: ~Astatic" (uiop:getcwd))
  (log:info "Starting server version ~A on port ~A" +version+ port)

  ;; Set up static file handlers in the global dispatch table
  (setf hunchentoot:*dispatch-table* +static-dispatch-table+)

  ;; Create and start the easy-routes acceptor
  (setf *acceptor* (make-instance 'my-acceptor :port port))
  (hunchentoot:start *acceptor*)
  (log:info "Server started successfully on port ~A" port)
  *acceptor*)
