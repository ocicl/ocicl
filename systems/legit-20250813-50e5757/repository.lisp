(in-package #:org.shirakumo.legit)

(defclass repository ()
  ((location :initarg :location :accessor location)
   (cache :initform (make-hash-table :test 'equal) :accessor cache))
  (:default-initargs
   :location NIL))

(defmethod print-object ((repository repository) stream)
  (print-unreadable-object (repository stream :type T)
    (format stream "~s" (uiop:native-namestring (location repository)))))

(defun handle-init (action location remote branch bare)
  (ecase action
    (:error
     (error "~a is not a GIT repository." location))
    (:ignore
     NIL)
    ((:create :init)
     (ensure-directories-exist location)
     (git-init :directory location :bare bare)
     (when (and branch (string/= branch "master"))
       (git-checkout :branch branch :orphan T)))
    ((:clone)
     (ensure-directories-exist location)
     (git-clone (or remote (error "REMOTE required for :CLONE."))
                :directory location
                :branch branch
                :bare bare))))

(defmacro define-repo-function (name (repo &rest args) &body body)
  (let ((req (purify-args (lambda-fiddle:required-lambda-vars args)))
        (rest (gensym "REST")))
    `(progn (defgeneric ,name (,repo ,@req &key &allow-other-keys))
            (defmethod ,name ((,repo string) ,@req &rest ,rest)
              (apply #',name (pathname ,repo) ,@req ,rest))
            (defmethod ,name ((,repo pathname) ,@req &rest ,rest)
              (apply #',name (make-instance 'repository :location ,repo) ,@req ,rest))
            (defmethod ,name ((,repo repository) ,@args)
              (with-chdir (,repo)
                ,@body)))))

(defmacro git-value (repository name &body forms)
  `(or (gethash ,name (cache ,repository))
       (setf (gethash ,name (cache ,repository))
             (with-chdir (,repository)
               (string-right-trim
                '(#\Newline)
                (with-output-to-string (*git-output*)
                  ,@forms))))))

(define-repo-function clear-cache (repository &key (key NIL k-p))
  (if k-p
      (remhash key (cache repository))
      (clrhash (cache repository)))
  repository)

(defun git-location-p (location)
  (when (probe-file (location location))
    (ignore-errors
     (let ((*git-output* NIL)
           (*git-errors* NIL))
       (with-chdir (location)
         (git-rev-parse NIL :git-dir T))))))

(defgeneric init (repository &key &allow-other-keys)
  (:method ((repository string) &rest args)
    (apply #'init (pathname repository) args))
  (:method ((repository pathname) &key (if-does-not-exist :error) remote branch bare)
    (unless (git-location-p repository)
      (if if-does-not-exist
          (handle-init if-does-not-exist repository remote branch bare)
          (return-from init NIL)))
    (make-instance 'repository :location repository))
  (:method ((repository repository) &key (if-does-not-exist :error) remote branch bare)
    (unless (git-location-p repository)
      (if if-does-not-exist
          (handle-init if-does-not-exist (location repository) remote branch bare)
          (return-from init NIL)))
    repository))

(defgeneric clone (from to &key &allow-other-keys)
  (:method ((from repository) to &rest args &key)
    (apply #'clone (location from) to args))
  (:method ((from pathname) to &rest args &key)
    (apply #'clone (uiop:native-namestring from) to args))
  (:method ((from string) (to repository) &rest args &key)
    (apply #'clone from (location to) args))
  (:method ((from string) (to pathname) &rest args &key)
    (apply #'clone from (uiop:native-namestring to) args))
  (:method ((from string) (to string) &key branch bare single-branch)
    (git-clone from :directory to :branch branch :bare bare :single-branch single-branch)
    (make-instance 'repository :location to)))

(define-repo-function fetch (repository &key (remote "origin") (branch (current-branch repository)))
  (git-fetch :repository remote :refspecs branch)
  (clear-cache repository))

(define-repo-function pull (repository &key (refspecs (current-branch repository)) (remote (default-remote repository :branch refspecs)))
  (cond ((bare-p repository)
         ;; In bare repositories, do the fetch that would be about the same as a pull.
         (fetch repository :branch (format NIL "~a:~:*~a" refspecs)))
        (remote
         (git-pull :repository remote :refspecs refspecs)))
  (clear-cache repository))

(define-repo-function checkout (repository thing &key)
  (git-checkout :tree-ish thing)
  (clear-cache repository))

(define-repo-function reset (repository &key to hard mixed soft)
  (git-reset :tree-ish to :hard hard :mixed mixed :soft soft)
  (clear-cache repository))

(define-repo-function clean (repository &key directories force ignored)
  (git-clean :directories directories :force force :remove-ignored ignored)
  repository)

(define-repo-function add (repository files &key)
  (cond ((eql files :all)
         (git-add :all T))
        (T
         (git-add :paths files)))
  repository)

(define-repo-function commit (repository message &key amend)
  (git-commit :message message :amend amend)
  repository)

(define-repo-function push (repository &key (refspecs (current-branch repository)) (remote (default-remote repository :branch refspecs)))
  (git-push :repository remote :refspecs refspecs)
  repository)

(define-repo-function commits (repository &key)
  (loop with text = (git-value repository `(commits) (git-rev-list :all T))
        with stream = (make-string-input-stream text)
        for line = (read-line stream NIL NIL)
        while line
        when (string/= line "")
        collect line))

(define-repo-function submodules (repository &key recursive only-existing)
  (loop with text = (git-value repository `(submodules ,recursive)
                               (git-submodule :status :recursive recursive))
        with stream = (make-string-input-stream text)
        for line = (read-line stream NIL NIL)
        for path = (when (and line (string/= line ""))
                     (merge-pathnames (subseq line (1+ (position #\  line :start 1)))
                                      (location repository)))
        while line
        when (and path (or (not only-existing) (valid-location-p path)))
        collect (make-instance 'repository :location path)))

(define-repo-function map-submodules (repository function &rest args &key)
  (dolist (submodule (apply #'submodules repository args))
    (funcall function submodule)))

(defmacro do-submodules ((submodule repository &rest args) &body body)
  `(map-submodules ,repository (lambda (,submodule) ,@body) ,@args))

(define-repo-function remotes (repository &key)
  (remove-duplicates
   (loop with text = (git-value repository `remotes (git-remote NIL :verbose T))
         with stream = (make-string-input-stream text)
         for line = (read-line stream NIL NIL)
         while line
         collect (cl-ppcre:register-groups-bind (name remote) ("^(.*?)\\t(.*?) " line)
                   (cons name remote)))
   :key #'car :test #'string=))

(define-repo-function set-remotes (repository (new-remotes list) &key)
  (let ((old-remotes (remotes repository)))
    ;; Rename and remove old.
    (loop for (name . url) in old-remotes
          for new = (find name new-remotes :key #'car :test #'string=)
          do (cond (new
                    (when (string/= url (cdr new))
                      (git-remote :set-url :name name :newurl (cdr new) :oldurl url)))
                   (T
                    (git-remote :remove :name name))))
    ;; Add new.
    (loop for (name . url) in new-remotes
          do (unless (find name old-remotes :key #'car :test #'string=)
               (git-remote :add :name name :url url))))
  ;; Invalidate cache.
  (clear-cache repository :key `remotes)
  new-remotes)

(defun (setf remotes) (new-remotes repo &rest args)
  (apply #'set-remotes repo new-remotes args))

(define-repo-function commit-age (repository commit &key)
  (unix-to-universal-time
   (parse-integer (git-value repository `(age ,commit) (git-log :pretty "%ct" :max-count 1 :paths commit)))))

(define-repo-function commit-message (repository commit &key)
  (git-value repository `(message ,commit) (git-log :pretty "%B" :max-count 1 :paths commit)))

(define-repo-function commit-author (repository commit &key)
  (git-value repository `(author ,commit) (git-log :pretty "%an <%ae>" :max-count 1 :paths commit)))

(define-repo-function current-commit (repository &key short)
  (git-value repository `(commit ,short) (git-rev-parse "HEAD" :short short)))

(define-repo-function current-branch (repository &key)
  (git-value repository `(branch) (git-rev-parse "HEAD" :abbrev-ref T)))

(define-repo-function current-message (repository &key)
  (git-value repository `(message) (git-log :pretty "%B" :max-count 1)))

(define-repo-function current-age (repository &key)
  (unix-to-universal-time
   (parse-integer (git-value repository `(age) (git-log :pretty "%ct" :max-count 1)))))

(define-repo-function branch-remote (repository &key (branch (current-branch repository)))
  (git-value repository '(url) (git-config :name (format NIL "branch.~a.remote" branch))))

(define-repo-function branch-upstream (repository &key (branch (current-branch repository)))
  (git-value repository '(branch) (git-rev-parse (format NIL "~a@{u}" branch) :abbrev-ref T)))

(define-repo-function remote-url (repository &key remote)
  (if remote
      (git-value repository `(url ,remote) (ignore-errors (git-config :name (format NIL "remote.~a.url" remote))))
      (cdr (first (remotes repository)))))

(define-repo-function default-remote (repository &key (branch (current-branch repository)))
  (or (ignore-errors (branch-remote repository :branch branch))
      (car (first (remotes repository)))))

(define-repo-function bare-p (repository &key)
  (string-equal "true" (git-value repository `bare-p (git-rev-parse NIL :is-bare-repository T))))

(define-repo-function changed-p (repository &key)
  (not (string-equal "" (git-value repository 'changed-p (git-status () :porcelain T)))))

(defun ref-names-endings (content)
  (with-input-from-string (in content)
    (remove-duplicates
     (loop
       for line = (read-line in nil nil)
       while line
       for last-slash-position = (position #\/ line :from-end t)
       when last-slash-position
       collect (subseq line (1+ last-slash-position)))
     :test #'string=)))

(define-repo-function branches (repository &key)
  (ref-names-endings
   (git-value repository 'branches
     (git-branch :list t :all t :format "%(refname)"
                 :color nil :column nil))))

(define-repo-function tags (repository &key)
  (ref-names-endings
   (git-value repository 'tag
     (git-tag :list t :format "%(refname)"
              :column nil))))
