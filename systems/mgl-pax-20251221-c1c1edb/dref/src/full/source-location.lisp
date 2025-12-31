(in-package :dref)

(in-readtable pythonic-string-syntax)

(defsection @source-locations (:title "Source Locations")
  "These represent the file or buffer position of a [defining
  form][clhs] and are returned by the SOURCE-LOCATION function. For
  the details, see the Elisp function `slime-goto-source-location`."
  (make-source-location function)
  (source-location-p function)
  (source-location-file function)
  (source-location-file-position function)
  (source-location-buffer function)
  (source-location-buffer-position function)
  (source-location-snippet function)
  (source-location-adjusted-file-position function)
  (this-source-location macro))

(defun/autoloaded make-source-location (&key file file-position
                                             buffer buffer-position snippet)
  "Make a Swank source location. The ultimate reference is `slime.el`.
  When SNIPPET is provided, the match nearest to FILE-POSITION is
  determined (see the Elisp `slime-isearch` and
  SOURCE-LOCATION-ADJUSTED-FILE-POSITION)."
  (list :location
        (cond ((and file buffer)
               (list :buffer-and-file buffer (namestring file)))
              (file
               (list :file (namestring file)))
              (buffer
               (list :buffer buffer)))
        (list :position (if file-position
                            (1+ file-position)
                            buffer-position))
        (list :snippet snippet)))

(defun/autoloaded source-location-p (object)
  "See if OBJECT is a source location object."
  (and (listp object)
       (eq (first object) :location)))

(defun/autoloaded source-location-file (location)
  "Return the name of the file of the [defining form][clhs].
  This may be NIL, for example, if LOCATION is of a [defining
  form][clhs] that was entered at the REPL, or compiled in the
  `*slime-scratch*` buffer."
  (let ((file-entry (find :file (rest location) :key #'first))
        (buffer-and-file-entry (find :buffer-and-file (rest location)
                                     :key #'first)))
    (or (second file-entry)
        (third buffer-and-file-entry))))

(defun/autoloaded source-location-file-position (location)
  "Return the file position of the [defining form][clhs] or NIL
  if it's not available. The first position is 0."
  (let ((pos (source-location-buffer-position location)))
    (if pos
        (1- pos)
        pos)))

(defun/autoloaded source-location-buffer (location)
  "Return the name of the Emacs buffer of the [defining form][clhs] or
  NIL if there is no such Emacs buffer."
  (let ((buffer-entry (find :buffer (rest location) :key #'first))
        (buffer-and-file-entry (find :buffer-and-file (rest location)
                                     :key #'first)))
    (or (second buffer-entry)
        (second buffer-and-file-entry))))

(defun/autoloaded source-location-buffer-position (location)
  "Return the position of the [defining form][clhs] in
  SOURCE-LOCATION-BUFFER or NIL if it's not available. The first
  position is 1."
  (let ((position-entry (find :position (rest location) :key #'first))
        (offset-entry (find :offset (rest location) :key #'first)))
    (cond (position-entry
           (second position-entry))
          (offset-entry
           (+ (second offset-entry) (third offset-entry))))))

(defun/autoloaded source-location-snippet (location)
  "Return the [defining form][clhs] or a prefix of it as a string or NIL
  if it's not available."
  (and (eq (first location) :location)
       (getf (fourth location) :snippet)))

(defun/autoloaded source-location-adjusted-file-position (location)
  "Return the actual file position LOCATION points to allowing for 
  some deviation from the raw SOURCE-LOCATION-FILE-POSITION, which is
  adjusted by searching for the nearest occurrence of
  SOURCE-LOCATION-SNIPPET in the file. Needless to say, this can be a
  very expensive operation.

  If SOURCE-LOCATION-FILE is NIL, NIL is returned. If there is no
  snippet, or it doesn't match, then SOURCE-LOCATION-FILE-POSITION (or
  0 if that's NIL) is returned.

  This is a non-interactive companion to the Elisp function
  `slime-location-offset`, supporting only file positions and
  non-partial matching of snippets."
  (let ((file (source-location-file location))
        (pos (or (source-location-file-position location) 0))
        (snippet (source-location-snippet location)))
    (when file
      (or (and snippet
               (let* ((string (slurp-file file))
                      ;; Unlike `slime-isearch', we only do full matches.
                      (pos-before (search snippet string
                                          :end2 pos :from-end t))
                      (pos-after (search snippet string
                                         :start2 pos :from-end t)))
                 (or (and pos-before pos-after
                          (if (< (- pos pos-before)
                                 (- pos-after pos))
                              pos-before
                              pos-after))
                     pos-before pos-after)))
          pos))))

(defparameter *utf-8-external-format*
  #+abcl :utf-8
  #+allegro :utf-8
  #+clisp charset:utf-8
  #-(or abcl allegro clisp) :default)

(defun slurp-file (file)
  ;; FIXME: Determine the external format somehow?
  (alexandria:read-file-into-string
   file :external-format *utf-8-external-format*))
