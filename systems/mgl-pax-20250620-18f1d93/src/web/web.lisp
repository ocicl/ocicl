(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

;;;; HANDLE-PAX*-REQUEST

(defmacro with-errors-to-html (&body body)
  (with-gensyms (error)
    `(block nil
       (handler-bind
           ((pax-http-error
              (lambda (error)
                (setf (hunchentoot:return-code*) (http-code-of error))
                (return (format nil "~A" (escape-html
                                          (princ-to-string error))))))
            (error
              (lambda (,error)
                (when hunchentoot:*catch-errors-p*
                  (let ((,error (error-and-backtrace-to-string ,error)))
                    (print ,error)
                    (return (format nil "<h2>Error</h2><pre>~A</pre>"
                                    (escape-html ,error))))))))
         (progn ,@body)))))

(defun error-and-backtrace-to-string (error)
  (with-output-to-string (out)
    (trivial-backtrace:print-backtrace error :output out)))

(defparameter *pax-live-inputs*
  """View Documentation of:
  <input type="text" id="paxToDocument"
         placeholder="reference (e.g. print or print function)">
  <script>mglpaxAddDocumentListener();</script>

  See @BROWSING-LIVE-DOCUMENTATION for the reference syntax.

  Apropos:
  <input type="text" id="paxToApropos"
         placeholder="NAME-PATTERN LOCATIVE-TYPE*">
  <script>mglpaxAddAproposListener();</script>

  See @APROPOS for the syntax.""")

(defun handle-pax*-request ()
  (with-errors-to-html
    (with-swank ()
      (let* ((pax-url (request-pax*-url))
             (pkgname (hunchentoot:get-parameter "pkg"))
             (*package* (or (dref::find-package* pkgname)
                            (dref::find-package* (ignore-errors
                                                  (read-from-string pkgname)))
                            (dref::find-package :cl)))
             (*pax-live-home-page-override* *pax-live-inputs*)
             (editp (hunchentoot:get-parameter "edit")))
        (if editp
            (progn
              (edit-pax-url-in-emacs pax-url)
              (setf (hunchentoot:return-code*) hunchentoot:+http-no-content+)
              nil)
            (uiop:with-temporary-file (:pathname temp-file-name)
              (let ((url (document-for-web pax-url temp-file-name)))
                (multiple-value-bind (scheme authority path) (parse-url url)
                  (declare (ignore authority))
                  (if (and (string= scheme "file")
                           (string= path (namestring temp-file-name)))
                      (read-file-into-string path)
                      (unless (starts-with-subseq "pax" url)
                        (hunchentoot:redirect url)))))))))))

(defun request-pax*-url ()
  (let ((uri (hunchentoot:request-uri*)))
    (assert (char= (aref uri 0) #\/))
    (subseq uri 1)))

(defparameter *pax-live-html-head*
  """<script src="live.js"></script>""")

(defmacro with-document-open-args-for-web ((title &key (link-to-home t))
                                           &body body)
  `(let ((*document-html-head* *pax-live-html-head*)
         (*document-html-top-blocks-of-links*
           (when ,link-to-home
             '((:id "link-to-home" :links (("/" "PAX Home"))))))
         (*document/open-extra-args*
           `(:pages ((:objects :default
                      :header-fn ,(lambda (stream)
                                    (html-header stream
                                                 :title ,title
                                                 :stylesheet "style.css"))
                      :footer-fn ,#'html-footer
                      :source-uri-fn ,#'reference-to-edit-uri)))))
     ,@body))

(defun document-for-web (pax-url filename)
  (with-document-open-args-for-web ((urldecode pax-url))
    (document-pax*-url pax-url filename)))

(defun reference-to-edit-uri (dref)
  (let ((url (finalize-pax-url (dref-to-pax-url dref))))
    ;; Checking whether DREF has SOURCE-LOCATION is too expensive on
    ;; large pages.
    (cond ((find #\? url)
           (format nil "~A&edit" url))
          (t
           (format nil "~A?edit" url)))))

(defun edit-pax-url-in-emacs (pax-url)
  (when (swank::default-connection)
    (multiple-value-bind (scheme authority path) (parse-url pax-url)
      (declare (ignore authority))
      (unless (equal scheme "pax")
        (error "~S doesn't have pax: scheme." pax-url))
      (when-let (drefs (definitions-for-pax-url-path path))
        (when (= (length drefs) 1)
          (swank::with-connection ((swank::default-connection))
            (let* ((dref (first drefs))
                   (dspec (dref::definition-to-dspec dref))
                   (location (source-location dref)))
              (when location
                (swank:eval-in-emacs `(mgl-pax-edit-for-cl
                                       '((,dspec ,location))))))))))))


;;;; HyperSpec

;;; The previous hyperspec root with which ENSURE-WEB-SERVER was
;;; called.
(defvar *web-hyperspec-root* nil)

;;; Dispatcher if the hyperspec is published via this web server.
(defvar *hyperspec-dispatch-table* ())

;;; The *DOCUMENT-HYPERSPEC-ROOT* to use when DOCUMENTing.
(defvar *web-document-hyperspec-root* nil)

(defun set-web-hyperspec-root (hyperspec-root)
  (unless (equal hyperspec-root *web-hyperspec-root*)
    (setq *web-hyperspec-root* hyperspec-root)
    ;; Treat normal file names as file URLs.
    (if (equal (or (ignore-errors (parse-url hyperspec-root))
                   "file")
               "file")
        ;; By default, Chrome and Firefox don't allow linking or
        ;; redirection from a http: URL to a file: URL, so we publish
        ;; the files under CLHS/.
        (let ((path (or (ignore-errors (nth-value 2 (parse-url hyperspec-root)))
                        hyperspec-root)))
          (setq *hyperspec-dispatch-table*
                (list (hunchentoot:create-folder-dispatcher-and-handler
                       "/CLHS/" path)))
          (setq *web-document-hyperspec-root* "/CLHS/"))
        (setq *hyperspec-dispatch-table* ()
              *web-document-hyperspec-root* hyperspec-root))))


;;;; Web server

(defun web-toplevel-static-files ()
  (let ((style-dir (html-style-dir *browse-html-style*)))
    (values (uiop:directory* (merge-pathnames "*.*" style-dir))
            style-dir)))

(defun make-dispatch-table ()
  (multiple-value-bind (static-files static-root) (web-toplevel-static-files)
    (let ((dispatchers ()))
      (dolist (file static-files)
        (unless (let ((name (pathname-name file)))
                  (and (stringp name)
                       (starts-with-subseq "README" name)))
          (let ((uri (format nil "/~A" (enough-namestring file static-root))))
            (pushnew (if (uiop:directory-pathname-p file)
                         (hunchentoot:create-folder-dispatcher-and-handler
                          uri file)
                         (hunchentoot:create-static-file-dispatcher-and-handler
                          uri file))
                     dispatchers))))
      (push (hunchentoot:create-prefix-dispatcher "/pax" 'handle-pax*-request)
            dispatchers)
      (push (create-exact-dispatcher "/" 'handle-homepage-request)
            dispatchers)
      dispatchers)))

(defun create-exact-dispatcher (string handler)
  (lambda (request)
    (and (string= (hunchentoot:script-name request) string)
         handler)))

(defun handle-homepage-request ()
  (with-errors-to-html
    (with-document-open-args-for-web ("PAX" :link-to-home nil)
      (document/open (pax-live-home-page :override *pax-live-inputs*)
                     :stream nil))))

;;; HUNCHENTOOT:*DISPATCH-TABLE* will be bound to this locally to
;;; avoid conflicts with other HUNCHENTOOT servers running in the same
;;; image.
;;;
;;; Whenever this file is recompiled, *DISPATCH-TABLE* is set to NIL,
;;; which invalidates the dispatch table cache, to pick up any changes
;;; during development or upgrades.
(defparameter *dispatch-table* nil)

;;; Cache the dispatch table of the most recent request, which depends
;;; on *BROWSE-HTML-STYLE*.
(defvar *style-and-dispatch-table* nil)

(defun dispatch-table ()
  (if (and *dispatch-table*
           (eq (car *style-and-dispatch-table*) *browse-html-style*))
      (cdr *style-and-dispatch-table*)
      (let ((*browse-html-style* *browse-html-style*))
        (setq *style-and-dispatch-table* (cons *browse-html-style*
                                               (make-dispatch-table)))
        (setq *dispatch-table* 'cached)
        (cdr *style-and-dispatch-table*))))

(defvar *server*
  (make-instance 'hunchentoot:easy-acceptor
                 ;; Any free port
                 :port 0
                 :access-log-destination nil
                 :message-log-destination nil))

(defun %start-server (port)
  (when port
    (setf (slot-value *server* 'hunchentoot::port) port))
  (hunchentoot:start *server*)
  (assert (plusp (hunchentoot:acceptor-port *server*))))

(defmethod hunchentoot:acceptor-dispatch-request
    :around ((acceptor (eql *server*)) request)
  (declare (ignorable request))
  (let ((hunchentoot:*dispatch-table* (append (dispatch-table)
                                              *hyperspec-dispatch-table*))
        (*document-hyperspec-root*
          (cond ((null *web-document-hyperspec-root*)
                 *document-hyperspec-root*)
                ((equal *web-document-hyperspec-root* "/CLHS/")
                 (format nil "~A/CLHS/" (web-base-url)))
                (t
                 *web-document-hyperspec-root*)))
        (*read-eval* nil))
    (call-next-method)))

(defun ensure-web-server-for-emacs (&key port hyperspec-root)
  (swank/backend:converting-errors-to-error-location
    `(:base-url ,(ensure-web-server :port port
                                    :hyperspec-root hyperspec-root))))

(defun/autoloaded ensure-web-server (&key port hyperspec-root)
  """Start or update a web server on PORT for @BROWSING-LIVE-DOCUMENTATION.
  Returns the base URL of the server (e.g. `http://localhost:32790`),
  which goes to the @PAX-LIVE-HOME-PAGE. If the web server is running
  already `(ENSURE-WEB-SERVER)` simply returns its base URL.

  Note that even when using Emacs but @BROWSING-WITH-OTHER-BROWSERS,
  the web server is started automatically. When @BROWSING-WITH-W3M, no
  web server is involved at all. Calling this function explicitly is
  only needed if the Emacs integration is not used, or to override
  PORT and HYPERSPEC-ROOT.

  - If PORT is NIL or 0, then the server will use any free port.

  - If there is a server already running and PORT is not NIL or 0,
    then the server is restarted on PORT.

  - If HYPERSPEC-ROOT is NIL, the HyperSpec pages will be served from
    any previously provided HYPERSPEC-ROOT or, failing that, from
    *DOCUMENT-HYPERSPEC-ROOT*.

  - If HYPERSPEC-ROOT is non-NIL, then pages in the HyperSpec will be
    served from HYPERSPEC-ROOT. The following command changes the root
    without affecting the server in any other way:

          (ensure-web-server :hyperspec-root "/usr/share/doc/hyperspec/")
  """
  (if (not (hunchentoot:started-p *server*))
      (%start-server port)
      ;; Treat both NIL and 0 as 'any port'.
      (when (and port (plusp port)
                 (/= port (hunchentoot:acceptor-port *server*)))
        (hunchentoot:stop *server*)
        (%start-server port)))
  (when hyperspec-root
    (set-web-hyperspec-root hyperspec-root))
  (web-base-url))

(defun web-base-url ()
  (format nil "http://localhost:~S" (hunchentoot:acceptor-port *server*)))
