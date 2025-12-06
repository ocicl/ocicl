;;;; mgl-pax.el --- MGL-PAX Emacs integration -*- lexical-binding: t -*-

;;;; MGL-PAX Emacs integration
;;;; =========================
;;;;
;;;; SETUP (see MGL-PAX::@EMACS-SETUP)
;;;; ---------------------------------
;;;;
;;;; - `mgl-pax-autoload'
;;;;
;;;; - `mgl-pax-reload'
;;;;
;;;; - `mgl-pax-hijack-slime-doc-keys'
;;;;
;;;; - `mgl-pax-browser-function'
;;;;
;;;; - `mgl-pax-web-server-port'
;;;;
;;;; NAVIGATE (see MGL-PAX::@NAVIGATING-IN-EMACS)
;;;; --------------------------------------------
;;;;
;;;; - `M-.' (`slime-edit-definition') supports new kinds of
;;;;   definitions (e.g. of ASDF/SYSTEMs) and disambiguates based on
;;;;   nearby locatives. Just by loading this file, `M-.' shall be
;;;;   able to recognize disambiguate based on locatives near point as
;;;;   in "function FOO".
;;;;
;;;; - Also, see `mgl-pax-edit-parent-section'.
;;;;
;;;; DOCUMENT (see MGL-PAX::@BROWSING-LIVE-DOCUMENTATION)
;;;; ----------------------------------------------------
;;;;
;;;; - Browse documentation of definitions in the running Lisp live
;;;;   without explicitly generating documentation with
;;;;   `mgl-pax-document'. Bind it to `C-.' to parallel `M-.'.
;;;;
;;;; - Also, see `mgl-pax-current-definition-toggle-view'.
;;;;
;;;; - The functions `mgl-pax-hideshow-documentation' and
;;;;   `mgl-pax-hideshow-comments' help focus on the code only by
;;;;   folding or unfolding MGL-PAX:DEFSECTION,
;;;;   MGL-PAX:DEFINE-GLOSSARY-TERM forms and long strings, or
;;;;   comments.
;;;;
;;;; - `mgl-pax-apropos', `mgl-pax-apropos-all' and
;;;;   `mgl-pax-apropos-package' are replacements for `slime-apropos'
;;;;   `slime-apropos-all' and `slime-apropos-package', respectively.
;;;;   They are all built on top of `mgl-pax-document'.
;;;;
;;;; TRANSCRIBE (see MGL-PAX::@TRANSCRIBING-WITH-EMACS (press `C-.' on this))
;;;; ------------------------------------------------------------------------
;;;;
;;;; - For `mgl-pax-transcribe-last-expression' and
;;;;   `mgl-pax-retranscribe-region'.

(eval-and-compile
  (require 'cl-lib nil t)
  ;; For emacs 23, look for bundled version
  (require 'cl-lib "lib/cl-lib")
  (require 'slime))

(eval-when-compile
  ;; For byte-compiling uses of the `w3m-anchor' macro
  (require 'w3m))


;;;; Autoloading of MGL-PAX on the Common Lisp side

(defcustom mgl-pax-autoload t
  "If true, then the MGL-PAX components will be loaded as necessary
via Slime by `slime-edit-definition', `mgl-pax-document' and
other mgl-pax commands in interactive use."
  :type 'boolean
  :group 'mgl-pax)

(defun mgl-pax-find-file-up (file-name)
  (concat (locate-dominating-file load-file-name file-name)
          file-name))

(defun mgl-pax-read-version ()
  (with-temp-buffer
    (insert-file-contents (mgl-pax-find-file-up "version.lisp-expr"))
    (goto-char (point-min))
    (read (current-buffer))))

;;; See MGL-PAX::CHECK-PAX-ELISP-VERSION.
(defvar mgl-pax-version)
;;; The next line is `(setq mgl-pax-version (mgl-pax-read-version))`
;;; in the sources, which gets replaced by the the version in
;;; `version.lisp-expr` by MGL-PAX:INSTALL-PAX-ELISP.
(setq mgl-pax-version (mgl-pax-read-version))

;;; Check that the Elisp and CL PAX versions match, ensure that
;;; ASDF-SYSTEM is loaded (in the sense of ASDF:COMPONENT-LOADED-P),
;;; and evaluate BODY. If the system was not loaded and couldn't be
;;; autoloaded, then signal an error.
;;;
;;; Note that autoloading happens asynchronously because it may take a
;;; long time and can run into errors. Hence, we only do it for when
;;; the immediately containing function was `called-interactively-p'
;;; (and `mgl-pax-autoload' is true, of course).
;;;
;;; If the ASDF-SYSTEM is `:mgl-pax/document' and `(mgl-pax-use-w3m)`
;;; is nil, then `:mgl-pax/web' is loaded instead and the web server
;;; is started.
(cl-defmacro mgl-pax-with-component ((asdf-system) &body body)
  (declare (indent 1))
  `(let* ((asdf-system (if (and (eq ',asdf-system :mgl-pax/document)
                                (not (mgl-pax-use-w3m)))
                           :mgl-pax/web
                         ',asdf-system))
          (interactivep (called-interactively-p 'any))
          (autoload (and mgl-pax-autoload interactivep)))
     (mgl-pax-maybe-autoload
      asdf-system
      autoload
      (lambda (loadedp)
        (cond ((not loadedp)
               (mgl-pax-system-not-loaded-error asdf-system interactivep))
              ((eq loadedp :failed)
               (mgl-pax-autoloading-failed-error asdf-system))
              (t
               (cl-flet ((foo () ,@body))
                 (if (eq asdf-system :mgl-pax/web)
                     (mgl-pax-call-with-web-server/async #'foo)
                   (foo)))))))))

(defun mgl-pax-system-not-loaded-error (asdf-system interactivep)
  (error "The ASDF system %s is not loaded (%s)."
         asdf-system (if (not interactivep)
                         "non-interactive call"
                       (format "mgl-pax-autoload is %s" mgl-pax-autoload))))

(defun mgl-pax-autoloading-failed-error (asdf-system)
  (unload-feature 'mgl-pax :force)
  (error "Loading the ASDF system %s failed. Unloaded mgl-pax from Emacs."
         asdf-system))

(defun mgl-pax-maybe-autoload (asdf-system autoload cont)
  (let ((check-form
         `(cl:let ((f (cl:and (cl:find-package :mgl-pax)
                              (cl:find-symbol
                               (cl:string '#:check-pax-elisp-version)
                               :mgl-pax))))
                  (cl:when (cl:and f (asdf:component-loaded-p
                                      (cl:ignore-errors
                                       (asdf:find-system ',asdf-system))))
                           (cl:funcall f ',mgl-pax-version)))))
    (cond ((slime-eval check-form)
           (when cont (funcall cont t)))
          ((not autoload)
           (when cont (funcall cont nil)))
          (t
           ;; This is the only async path.
           (mgl-pax-eval-async
            `(cl:progn
              (cl:format cl:t "~&;; Autoloading ~S for Emacs ~
                                 (mgl-pax-autoload is t).~%" ',asdf-system)
              (cl:cond
               ((cl:ignore-errors (asdf:load-system ',asdf-system)
                                  cl:t)
                (cl:format cl:t ";; Done autoloading ~S for Emacs~%"
                           ',asdf-system)
                (cl:and ,check-form :async))
               (cl:t
                (cl:format
                 cl:t
                 ";; FAILED autoloading ~S.~%~
                  ;; Evaluate (ASDF:LOAD-SYSTEM ~S) to debug.~%~
                  ;; If there are missing dependencies, ~
                  under Quicklisp for example, evaluate~%~
                  ;; (QL:QUICKLOAD ~S).~%~
                  ;;~%~
                  ;; Proceeding to unload mgl-pax from Emacs.~%~
                  ;; Once PAX loads cleanly, you may want to ~
                  (require 'mgl-pax) in Emacs.~%"
                 ',asdf-system ',asdf-system ',asdf-system)
                :failed)))
            cont)))))

(defun mgl-pax-component-loaded-p (asdf-system)
  (slime-eval`(asdf:component-loaded-p (cl:ignore-errors
                                        (asdf:find-system ',asdf-system)))))

(defvar mgl-pax-file-name)
(setq mgl-pax-file-name load-file-name)

(defun mgl-pax-reload ()
  "Reload mgl-pax.el. This may be necessary after upgrading MGL-PAX.
See MGL-PAX::@EMACS-SETUP."
  (interactive)
  (let ((sourcefile (concat (file-name-sans-extension mgl-pax-file-name)
                            ".el")))
    (unload-feature 'mgl-pax t)
    (load-file sourcefile)))

;;; This is called automatically by (unload-feature 'mgl-pax).
(defun mgl-pax-unload-function ()
  (mgl-pax-unhijack-slime-doc-keys)
  (advice-remove 'slime-edit-definition #'slime-edit-definition@mgl-pax))


;;;; MGL-PAX::@EMACS-SETUP

(defcustom mgl-pax-hijack-slime-doc-keys t
  "If true, then bind mgl-pax functions in `slime-mode-map'
  by `mgl-pax-hijack-slime-doc-keys' upon loading `mgl-pax'. See
  MGL-PAX::@EMACS-KEYS for details."
  :type 'boolean
  :group 'mgl-pax)

(defvar mgl-pax-slime-doc-map-overrides
  '((?a slime-apropos mgl-pax-apropos)
    (?z slime-apropos-all mgl-pax-apropos-all)
    (?p slime-apropos-package mgl-pax-apropos-package)
    (?f slime-describe-function mgl-pax-document)
    (?d slime-describe-symbol mgl-pax-hideshow-documentation)
    (?c nil mgl-pax-hideshow-comments)
    (?v nil mgl-pax-current-definition-toggle-view)
    (?u nil mgl-pax-edit-parent-section)))

(defun mgl-pax-hijack-slime-doc-keys ()
  "See MGL-PAX::@EMACS-SETUP."
  (interactive)
  (mgl-pax-override-keys slime-doc-map mgl-pax-slime-doc-map-overrides))

(defun mgl-pax-override-keys (keymap overrides &optional reversep)
  (cl-loop for (key old new) in overrides do
           (when reversep
             (cl-rotatef old new))
           ;; This is like `slime-bind-keys' with BOTHP.
           (let ((key `[,key]))
             (when (eq (lookup-key keymap key) old)
               (define-key keymap key new)))
           (let ((key `[(control ,key)]))
             (when (eq (lookup-key keymap key) old)
               (define-key keymap key new)))))

(defun mgl-pax-unhijack-slime-doc-keys ()
  "See MGL-PAX::@EMACS-SETUP."
  (interactive)
  (mgl-pax-override-keys slime-doc-map mgl-pax-slime-doc-map-overrides t))

(when mgl-pax-hijack-slime-doc-keys
  (mgl-pax-hijack-slime-doc-keys))


;;;; Browser configuration (see MGL-PAX::@EMACS-SETUP)

(defcustom mgl-pax-browser-function nil
  "The name of the function to use to browse URLs.
When nil, the value of `browse-url-browser-function' is used. If
the effective value is `w3m-browse-url', then
MGL-PAX::@BROWSING-LIVE-DOCUMENTATION will take place in Emacs
buffers using `w3m'. Else, `mgl-pax-document' will start a web
server on the Common Lisp side for the external browser."
  :type 'symbol
  :group 'mgl-pax)

(defcustom mgl-pax-web-server-port nil
  "If the web server is started, it will be on this port.
See `mgl-pax-autoload'. If nil, then a free port will be used."
  :type 'natnum
  :group 'mgl-pax)

(defun mgl-pax-use-w3m ()
  (eq (or mgl-pax-browser-function browse-url-browser-function)
      'w3m-browse-url))

(defvar mgl-pax-web-server-base-url)

(defun mgl-pax-call-with-web-server/async (fn)
  (mgl-pax-eval-async
   `(mgl-pax::ensure-web-server-for-emacs
     :hyperspec-root ',common-lisp-hyperspec-root
     :port ,mgl-pax-web-server-port)
   (lambda (values)
     (if (eq (cl-first values) :error)
         (error "%s" (cl-second values))
       (cl-assert (eq (cl-first values) :base-url))
       (setq mgl-pax-web-server-base-url (cl-second values))
       (funcall fn)))))


;;;; Communicating with CL

;;; Cancel the non-local exits to avoid "error in process filter"
;;; messages and the subsequent delay when this function is called by
;;; `slime-async-eval' and `read-from-minibuffer' is C-g'ed.
(cl-defmacro mgl-pax-with-nlx-barrier (&body body)
  `(catch 'nlx-barrier
     (let ((done nil))
       (unwind-protect
           (condition-case c
               (progn ,@body)
             (error (message "%s" (error-message-string c)))
             (:success (setq done t)))
         (unless done
           (throw 'nlx-barrier nil))))))

;;; Like `slime-eval-async', but call abort-cont on :abort and
;;; establish an nlx barrier.
(defun mgl-pax-eval-async (sexp ok-cont &optional abort-cont package)
  (slime-rex (ok-cont abort-cont (buffer (current-buffer)))
      (sexp (or package (slime-current-package)))
    ((:ok result)
     (when ok-cont
       (set-buffer buffer)
       (mgl-pax-with-nlx-barrier
        (funcall ok-cont result))))
    ((:abort condition)
     (if abort-cont
         (mgl-pax-with-nlx-barrier
          (funcall abort-cont condition))
       (message "Evaluation aborted on %s." condition)))))

(defun mgl-pax-eval (sexp)
  (let ((values (slime-eval sexp)))
    (if (eq (cl-first values) :error)
        (error "%s" (cl-second values))
      (cl-second values))))


;;;; Parsing utilities

;;; Return a list of strings representing `lisp-mode' sexps in the
;;; current buffer between START and END. The last sexp returned may
;;; be incomplete.
(cl-defun mgl-pax-parse-sexps (&key (start (point)) (end (point-max))
                                    allow-empty)
  (let ((sexps ())
        (bounds ()))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (skip-syntax-forward " ")
        (let ((sexp-start (point)))
          (condition-case nil
              (forward-sexp)
            (scan-error
             (goto-char end)))
          (let* ((sexp-end (point))
                 (sexp (slime-trim-whitespace
                        (buffer-substring-no-properties
                         sexp-start sexp-end))))
            (when (or allow-empty (cl-plusp (length sexp)))
              (push sexp sexps)
              (push (cons sexp-start sexp-end) bounds))))))
    (list (nreverse sexps) (nreverse bounds))))

(cl-defun mgl-pax-parse-sexps-from-string (string &key allow-empty)
  (let ((lisp-mode-hook ()))
    (with-temp-buffer
      (insert string)
      (lisp-mode)
      (mgl-pax-parse-sexps :start (point-min) :allow-empty allow-empty))))


;;;; Find possible words and locatives at point (see MGL-PAX::WALL).

;;; Return a list of of things like (word (locative1 locative2 ...))
;;; representing the possible references (word locative1), (word
;;; locative2), and so on, where MGL-PAX::@WORD may be processed
;;; further into a MGL-PAX::@NAME on the CL side.
;;; MGL-PAX::LOCATE-DEFINITIONS-FOR-EMACS and
;;; MGL-PAX::DOCUMENT-FOR-EMACS take such lists.
;;;
;;; `slime-sexp-at-point' works fine in code, but in printed
;;; representations and docstrings heuristics are needed (just think
;;; "SYM." and "#<SYM"), which we leave for the the Common Lisp side
;;; to resolve. However, we handle here the complications caused by
;;; Markdown, whose syntax for code (`nil`) and reference links
;;; ([title][id]) is used by PAX, maybe both at the same time as in
;;; [`FOO`][function] or [FOO][`function`]. ?` is a delimiter, but ?\[
;;; is not, which means that `slime-sexp-at-point' on FOO will
;;; result in NAME being "FOO" or "[FOO][". "[FOO][" is a valid symbol
;;; name, so we definitely want to look up definitions for it. In
;;; addition, we also look up definitions for the symbol whose name
;;; has the parts beyond [] cut off.
(defun mgl-pax-wall-at-point ()
  ;; `mgl-pax-call-in-lisp-mode' makes this function work even in
  ;; non-lisp buffers.
  (mgl-pax-call-in-lisp-mode
   (lambda ()
     ;; So that locatives spanning multiple comment lines are parsed
     ;; without the semicolons
     (mgl-pax-call-uncommented
      (lambda ()
        (let ((word (slime-sexp-at-point))
              (bounds (slime-bounds-of-sexp-at-point)))
          (when bounds
            (let ((locatives (mgl-pax-find-locatives bounds))
                  (wall (mgl-pax-parse-reflink bounds)))
              (append (and word `((,word ,locatives))) wall)))))))))

;;; If not in a `lisp-mode' buffer, then copy the five lines around
;;; point into a temporary lisp buffer, put point on the same
;;; character in the new buffer and call FN.
(defun mgl-pax-call-in-lisp-mode (fn)
  (if (eq major-mode 'lisp-mode)
      (funcall fn)
    (let ((text-before (buffer-substring-no-properties
                        (line-beginning-position -1)
                        (point)))
          (text-after (buffer-substring-no-properties
                       (point)
                       (line-beginning-position 4)))
          (lisp-mode-hook ()))
      (with-temp-buffer
        (lisp-mode)
        (insert text-before)
        (save-excursion (insert text-after))
        (funcall fn)))))

;;; If point is in a comment, then call FN in in temporary buffer with
;;; all consecutive comment lines uncommented and point position at
;;; the original position. Else just call FN.
(defun mgl-pax-call-uncommented (fn)
  (let ((comment-bounds (mgl-pax-comment-lines-bounds)))
    (if comment-bounds
        (let ((comment (apply #'buffer-substring comment-bounds))
              (pos (1+ (- (point) (car comment-bounds)))))
          (with-temp-buffer
            (lisp-mode)
            (insert comment)
            (goto-char pos)
            (uncomment-region (point-min) (point-max))
            (funcall fn)))
      (funcall fn))))

(defun mgl-pax-comment-lines-bounds ()
  (when (mgl-pax-in-comment-p)
    (let* ((end (save-excursion
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t)
                  (comment-forward (point-max))
                  (point)))
           (beg (save-excursion
                  (forward-line 0)
                  (while (and (not (bobp))
                              (= end (save-excursion
                                       (comment-forward (point-max))
                                       (point))))
                    (forward-line -1))
                  (goto-char (line-end-position))
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t)
                  (ignore-errors
                    (while (looking-at-p comment-start-skip)
                      (forward-char -1)))
                  (point))))
      (list beg end))))

(defun mgl-pax-in-comment-p ()
  (elt (syntax-ppss) 4))

;;; Return the sexps before and after (slime-sexp-at-point),
;;; skipping some markup.
(cl-defun mgl-pax-find-locatives
    (&optional (bounds (slime-bounds-of-symbol-at-point)))
  (remove nil (list (mgl-pax-locative-before (car bounds))
                    (mgl-pax-locative-after (cdr bounds)))))

(cl-defun mgl-pax-locative-before (&optional (point (point)))
  (save-excursion
    (goto-char (1- point))
    (unless (looking-at "(")
      (skip-chars-backward ";` \n\t")
      (let ((sexp (ignore-errors (slime-last-expression))))
        (unless (equal sexp "")
          sexp)))))

(cl-defun mgl-pax-locative-after (&optional (point (point)))
  (save-excursion
    (goto-char point)
    (skip-chars-forward "[];`\" \n\t")
    (if (and (char-after)
             (equal (string (char-after)) "("))
        ;; [FOO][(function)]
        (mgl-pax-next-sexp)
      ;; [FOO][function], [`FOO`][function], [FOO ][function]
      (let ((end-pos+1 (save-excursion
                         (search-forward-regexp "\\(\\]\\|`\\)"
                                                (+ (point) 1000)
                                                t))))
        (if end-pos+1
            (save-restriction
              (narrow-to-region (point) (1- end-pos+1))
              (mgl-pax-next-sexp))
          (mgl-pax-next-sexp))))))

;;; Return the next sexp as a string or nil.
(defun mgl-pax-next-sexp ()
  (let ((string (slime-trim-whitespace
                 (buffer-substring-no-properties
                  (point)
                  (save-excursion
                    (ignore-error scan-error (forward-sexp))
                    (point))))))
    (unless (zerop (length string))
      string)))

;;; With point on FOO or just after, parse "[FOO][function]" as a
;;; Markdown reference link. Return the name and the locative string
;;; as a list like ("FOO" ("function")).
(cl-defun mgl-pax-parse-reflink
    (&optional (bounds (slime-bounds-of-symbol-at-point)))
  (when bounds
    (let ((wall ()))
      (cl-flet ((add (start end)
                  (let ((word (buffer-substring-no-properties start end))
                        (locative (mgl-pax-locative-after end)))
                    (push (list word (if locative
                                         (list locative)
                                       ()))
                          wall))))
        (cl-destructuring-bind (symbol-start . symbol-end) bounds
          (save-restriction
            ;; Do not search beyond the surrounding lines.
            (let* ((min (save-excursion (ignore-errors (forward-line -1))
                                        (beginning-of-line)
                                        (point)))
                   (max (save-excursion (ignore-errors (forward-line))
                                        (end-of-line)
                                        (point)))
                   (start-pos (save-excursion (search-backward "[" min t)))
                   (end-pos (save-excursion (search-forward "]" max t))))
              (when (and start-pos end-pos)
                ;; Exclude the bracket characters.
                (let ((start-pos (save-excursion
                                   (goto-char (1+ start-pos))
                                   (skip-chars-forward " \t\n")
                                   (point)))
                      (end-pos (save-excursion (goto-char (1- end-pos))
                                               (skip-chars-backward " \t\n")
                                               (point))))
                  ;; [lambda lists][clhs]
                  (add start-pos end-pos)
                  ;; [see also][foo function], [FOO function][docstring]
                  (when (and (< symbol-start start-pos)
                             (< symbol-end end-pos))
                    (add start-pos symbol-end))))))))
      (reverse wall))))


;;;; Integration with `M-.' (`slime-edit-definition') (see
;;;; MGL-PAX::@NAVIGATING-IN-EMACS)

;;; Functions `slime-edit-definition-hooks' are called with just a
;;; name, but `mgl-pax-edit-definition' needs `mgl-pax-wall-at-point'.
(defvar mgl-pax-edit-definition-wall :no-wall)

;;; Alterations to `slime-edit-definition':
;;;
;;; - Ensure that :MGL-PAX/NAVIGATE is loaded.
;;;
;;; - Instead of `slime-symbol-at-point' default to
;;;  `slime-sexp-at-point' to support string and list names.
;;;
;;; - Capture the buffer context in `mgl-pax-edit-definition-wall'
;;;   when defaulting.
(define-advice slime-edit-definition (:around (oldfun &optional name where)
                                      mgl-pax)
  (interactive)
  (mgl-pax-with-component (:mgl-pax/navigate)
    (or (when (and interactivep (not current-prefix-arg))
          ;; Default to buffer context.
          (when-let (name (slime-sexp-at-point))
            (let ((mgl-pax-edit-definition-wall (mgl-pax-wall-at-point)))
              (funcall oldfun name where)
              t)))
        ;; Prompt. This involves `mgl-pax-completions-at-point',
        ;; which needs :MGL-PAX/NAVIGATE for completion.
        (let ((name (if interactivep
                        (mgl-pax-read-from-minibuffer "Edit Definition of: "
                                                      'navigate)
                      name)))
          (funcall oldfun name where)))))

(defun mgl-pax-edit-definition (name &optional where)
  (ignore where)
  (when (mgl-pax-component-loaded-p :mgl-pax/navigate)
    (mgl-pax-visit-locations
     (if (eq mgl-pax-edit-definition-wall :no-wall)
         (mgl-pax-edit-reference name)
       (mgl-pax-edit-wall mgl-pax-edit-definition-wall)))))

(add-hook 'slime-edit-definition-hooks 'mgl-pax-edit-definition)

(defun mgl-pax-edit-wall (wall)
  (mgl-pax-locate-definitions wall))

(defun mgl-pax-edit-reference (reference)
  (cl-destructuring-bind (sexp-1 sexp-2) (mgl-pax-parse-reference reference)
    (if sexp-2
        ;; "FOO function" or "function FOO"
        (mgl-pax-locate-definitions `((,sexp-1 (,sexp-2))
                                      (,sexp-2 (,sexp-1))))
      ;; "FOO"
      (mgl-pax-locate-definitions `((,sexp-1 ()))))))

(defun mgl-pax-parse-reference (string &optional allow-fragment)
  (let* ((sexps (cl-first (mgl-pax-parse-sexps-from-string string)))
         (n (length sexps)))
    (if (and allow-fragment (= n 4))
        sexps
      (cond ((= n 0) (list nil nil))
            ((= n 1) (list (cl-first sexps) nil))
            ((= n 2) sexps)
            (t
             (message "Ignoring trailing junk %S" (cl-subseq sexps 2))
             (sit-for 1)
             (cl-subseq sexps 0 2))))))

(defun mgl-pax-locate-definitions (wall)
  (slime-eval `(mgl-pax::locate-definitions-for-emacs ',wall)))

(defun mgl-pax-visit-locations (dspec-and-location-list)
  (when (consp dspec-and-location-list)
    (if (eq (car dspec-and-location-list) :error)
        (error "%s" (cl-second dspec-and-location-list))
      (slime-edit-definition-cont
       (slime-postprocess-xrefs dspec-and-location-list)
       "dummy name"
       nil))))


;;;; Completion of locatives for `slime-edit-definition' (see
;;;; MGL-PAX::@M-.-COMPLETION) and also for `mgl-pax-document',
;;;; `mgl-pax-apropos' and `mgl-pax-apropos-all'.

(add-to-list 'slime-completion-at-point-functions
             'mgl-pax-completions-at-point)

;;; The context in which `mgl-pax-completions-at-point' is invoked.
;;; This affects whether the name entered the list of possible
;;; locatives (doesn't for `apropos') and whether the only
;;; DREF:LISP-LOCATIVE-TYPES are considered (yes, for `navigate').
(defvar mgl-pax-completing-for nil)

(defun mgl-pax-read-from-minibuffer (prompt completing-for)
  (cl-assert (member completing-for
                     '(navigate document apropos apropos-package)))
  (let ((mgl-pax-completing-for completing-for))
    (slime-read-from-minibuffer prompt)))

(defun mgl-pax-completions-at-point ()
  (when (and mgl-pax-completing-for
             (mgl-pax-component-loaded-p :mgl-pax/navigate))
    (if (eq mgl-pax-completing-for 'apropos-package)
        (mgl-pax-completions-at-point-for-apropos-package)
      (let* (;; This the position of the first character after the prompt.
             (bol (line-beginning-position))
             (end (point)))
        (cl-destructuring-bind (sexps bounds)
            (mgl-pax-parse-sexps :start bol :end end :allow-empty t)
          (let ((start (car (cl-first (last bounds)))))
            (if (eq mgl-pax-completing-for 'apropos)
                ;; Punt to Slime completion for the first sexp.
                (cond ((and (= 1 (length sexps))
                            (string= (cl-first sexps) ""))
                       (mgl-pax-completions-at-point-for-apropos-dtype end))
                      ((<= 2 (length sexps))
                       (mgl-pax-completions-at-point-for-apropos-dtype start)))
              ;; Completing for `navigate' and `document'
              (cond ((= (length sexps) 1)
                     ;; point is within the first sexp or just beyond,
                     ;; so the next char to be typed may still be part
                     ;; of it. We only need to complete explicit string
                     ;; names because returning nil lets the default
                     ;; Slime completion take care of symbols.
                     (mgl-pax-string-name-completions))
                    ;; Specified locative
                    ((= (length sexps) 2)
                     ;; The first sexp is definitely complete.
                     (list start end
                           (mgl-pax-names-or-locatives
                            (elt sexps 0) (buffer-substring-no-properties
                                           start end))))
                    ;; `mgl-pax-document' input with fragment. We could
                    ;; also complete the third sexp (the fragment's
                    ;; name) based on what is on the page generated for
                    ;; the main reference.
                    ((= (length sexps) 4)
                     ;; The first sexp is definitely complete.
                     (list start end
                           (mgl-pax-names-or-locatives
                            (elt sexps 2) (buffer-substring-no-properties
                                           start end))))))))))))

;;; Return the completions for a DREF::@NAME typed in is explicitly as
;;; a string (e.g. `"mgl-p', note the missing right quote).
(defun mgl-pax-string-name-completions ()
  (let ((start (save-excursion (re-search-backward "\"[^\t\n]*\\="
                                                   (max (point-min)
                                                        (- (point) 1000))
                                                   t))))
    (when start
      (let* ((string (buffer-substring-no-properties (1+ start) (point)))
             (matches (slime-eval `(mgl-pax::string-name-completions-for-emacs
                                    ,string :locative-types
                                    ,(if (eq mgl-pax-completing-for 'navigate)
                                         '(dref:lisp-locative-types)
                                       '(dref:locative-types))))))
        ;; KLUDGE: Returning this even if MATCHES is () prevents other
        ;; completion functions from running, which is quite desirable
        ;; as currently these functions (`slime-complete-symbol*' and
        ;; `slime-filename-completion') attempt to complete the string
        ;; as a filename.
        (list start (point) matches)))))


;;;; Also for MGL-PAX::@NAVIGATING-IN-EMACS

(defun mgl-pax-edit-parent-section ()
  "Look up the definition of parent section of the definition
`point' is in as if with `M-.' (`slime-edit-definition'). If
there are multiple containing sections, then pop up a selection
buffer."
  (interactive)
  (mgl-pax-with-component (:mgl-pax/navigate)
    (mgl-pax-find-parent-section #'mgl-pax-visit-locations)))

(defun mgl-pax-find-parent-section (cont)
  (mgl-pax-eval-async
   `(mgl-pax::find-parent-section-for-emacs
     ',(buffer-name)
     ',(buffer-file-name)
     ',(mgl-pax-current-definition-possible-names))
   cont))

(defun mgl-pax-current-definition-possible-names ()
  (save-excursion
    (when (looking-at "(")
      (ignore-errors (down-list)))
    (cl-loop for name-snippet-and-pos
             = (mgl-pax-current-sexp-first-arg-snippet-and-pos)
             when name-snippet-and-pos
             collect name-snippet-and-pos
             while (ignore-errors (backward-up-list 1 t t)
                                  t))))

;;; Return 1. the second element of the current sexp (where the name
;;; often is in definition forms) 2. the Slime source location
;;; :SNIPPET, 3. the start position of the sexp. If any movement
;;; fails, then return nil.
(defun mgl-pax-current-sexp-first-arg-snippet-and-pos ()
  (ignore-errors
    (save-excursion
      (backward-up-list 1 t t)
      (let ((snippet (mgl-pax-next-sexp))
            (pos (point)))
        (when (< 200 (length snippet))
          (setq snippet (cl-subseq snippet 0 200)))
        (down-list)
        (slime-forward-sexp)
        (forward-char)
        (let ((next (mgl-pax-next-sexp)))
          (when next
            (list next snippet pos)))))))


;;;; MGL-PAX documentation browser (see
;;;; MGL-PAX::@BROWSING-LIVE-DOCUMENTATION)
;;;;
;;;; Like `C-h f` (describe-function) but for Common Lisp via PAX.

(make-variable-buffer-local
 (defvar mgl-pax-doc-dir nil))

(defvar mgl-pax-doc-buffers ())

;;; Note we allow w3m not to be loaded or loaded after mgl-pax, so we
;;; must silence compilation warnings later on.
(defun mgl-pax-require-w3m ()
  (when (mgl-pax-use-w3m)
    (unless (require 'w3m nil t)
      (error "mgl-pax is configured to use w3m but it cannot be loaded."))
    (advice-add 'w3m-goto-url :around #'mgl-pax-w3m-goto-url)))

(defun mgl-pax-document (pax-url)
  "Browse the documentation of CL definitions for PAX-URL.

The documentation is a single HTML page generated by PAX via
Slime documenting the definitions corresponding to PAX-URL. If
necessary, a disambiguation page is generated with the
documentation of all matching definitions. The HTML page is
opened in the browser specified by `mgl-pax-browser-function'.

Interactive behaviour is documented in
MGL-PAX::@BROWSING-LIVE-DOCUMENTATION (pressing `C-.' on this
works in the docstring in the sources or when viewed in a Help
buffer).

The following describes the syntax of PAX-URL in non-interactive
mode.

When invoked programmatically, PAX-URL must be a properly
urlencoded string with URL scheme \"pax:\". The format of PAX-URL
is:

  URL = \"pax:\" [REFERENCE] [\"#\" FRAGMENT]

where REFERENCE is a complete CL DREF::@REFERENCE as a string in
\"NAME LOCATIVE\" format (e.g. \"standard-object class\") and, if
given, so is FRAGMENT. For example,

  (mgl-pax-document (concat \"pax:\" (url-hexify-string \"print\")))

opens the disambiguation page for \"print\", while

  (mgl-pax-document
    (concat \"pax:\" (url-hexify-string \"pax::@pax-manual pax:section\")))

visits the page with the entire documentation of the PAX, and

  (mgl-pax-document
    (concat \"pax:\"
            (url-hexify-string \"pax::@pax-manual pax:section\")
            \"#\"
            (url-hexify-string \"pax:defsection pax:macro\")))

does the same but scrolls to the documentation of the DEFSECTION
macro on that page."
  ;; end-include
  (interactive (list nil))
  (slime-check-connected)
  (mgl-pax-require-w3m)
  ;; Handle the interactive defaults here because it involves async
  ;; calls.
  (mgl-pax-with-component (:mgl-pax/document)
    (cond
     ;; non-interactive
     (pax-url
      (mgl-pax-document-pax-url pax-url))
     ;; interactive, prefix arg
     (current-prefix-arg
      (mgl-pax-prompt-and-document))
     ;; interactive, no prefix arg, point over a PAX URL in a doc
     ;; buffer
     ((and (mgl-pax-in-doc-buffer-p)
           (mgl-pax-doc-pax-url (mgl-pax-doc-url)))
      (mgl-pax-document-pax-url (mgl-pax-doc-pax-url (mgl-pax-doc-url))))
     ;; interactive, no prefix arg, point not over a PAX URL in a doc
     ;; buffer
     (t
      (let ((wall (mgl-pax-wall-at-point)))
        (if wall
            (mgl-pax-document-pax-url
             (concat "pax-wall:" (url-hexify-string (format "%S" wall))))
          (mgl-pax-prompt-and-document)))))))

(defun mgl-pax-prompt-and-document ()
  (mgl-pax-document-pax-url
   (mgl-pax-urllike-to-url
    (mgl-pax-read-from-minibuffer "View Documentation of: " 'document))))

(defun mgl-pax-document-pax-url (pax-url)
  (if (mgl-pax-use-w3m)
      (mgl-pax-document-pax-url/w3m pax-url)
    (mgl-pax-document-pax-url/other pax-url)))

(defun mgl-pax-document-pax-url/other (pax-url)
  (let ((pax-url (if (string= pax-url "pax:")
                     (mgl-pax-make-pax-eval-url
                      '(mgl-pax::pax-live-home-page))
                   pax-url)))
    ;; Call `mgl-pax-call-document-for-emacs' with DIR nil to check
    ;; for errors (e.g. "no definition for xxx") before launching a
    ;; browser.
    (mgl-pax-call-document-for-emacs
     pax-url nil
     :ok-cont (lambda (url)
                (if (null url)
                    (mgl-pax-prompt-and-document)
                  ;; `url' may be `pax-url' canonicalized (e.g. the
                  ;; fragment) or an external URL.
                  (let ((url (if (string-prefix-p "pax" url)
                                 (concat mgl-pax-web-server-base-url "/" url)
                               url)))
                    (funcall (or mgl-pax-browser-function
                                 browse-url-browser-function)
                             url)))))))

;;; What characters need no escaping when PAX URLs are encoded in a
;;; URL path. This does not allow ?/ to keep relative links working.
(defconst mgl-pax-url-allowed-chars
  (let ((vec (copy-sequence url-path-allowed-chars)))
    (aset vec ?/ nil)
    vec))

(defun mgl-pax-document-pax-url/w3m (pax-url)
  (unless (and (mgl-pax-in-doc-buffer-p) (string= pax-url "pax:"))
    (if (mgl-pax-in-doc-buffer-p)
        ;; When "entering" the documentation browser, reload (see
        ;; `mgl-pax-doc-reload').
        (with-no-warnings (w3m-goto-url pax-url :reload))
      (let* ((doc-buffer (if (mgl-pax-in-doc-buffer-p)
                             (current-buffer)
                           (cl-first mgl-pax-doc-buffers))))
        (if doc-buffer
            (if (string= pax-url "pax:")
                ;; Just show the latest doc buffer if the input is the
                ;; empty string.
                (pop-to-buffer doc-buffer)
              ;; Reuse doc-buffer and its doc dir.
              (let ((doc-dir (buffer-local-value 'mgl-pax-doc-dir doc-buffer)))
                (mgl-pax-call-document-for-emacs
                 pax-url doc-dir
                 :ok-cont (lambda (url)
                            (if (null url)
                                (mgl-pax-prompt-and-document)
                              ;; Maybe pop to a pax doc buffer.
                              (when (and (not (mgl-pax-in-doc-buffer-p))
                                         mgl-pax-doc-buffers)
                                (let ((package (slime-current-package)))
                                  (pop-to-buffer (cl-first mgl-pax-doc-buffers))
                                  (setq slime-buffer-package package)))
                              (with-no-warnings (w3m-goto-url url :reload))
                              (mgl-pax-set-up-doc-buffer doc-dir))))))
          ;; Display the docs of this very documentation browser if
          ;; the input is the empty string.
          (when (string= pax-url "pax:")
            (setq pax-url (mgl-pax-make-pax-eval-url
                           '(mgl-pax::pax-live-home-page))))
          ;; No doc buffer. Create a new dir.
          (let ((doc-dir (file-name-as-directory (make-temp-file "pax-doc" t))))
            (mgl-pax-call-document-for-emacs
             pax-url doc-dir
             :ok-cont (lambda (url)
                        (if (null url)
                            (mgl-pax-prompt-and-document)
                          (let ((package (slime-current-package)))
                            (with-no-warnings (w3m-goto-url url :reload))
                            (setq slime-buffer-package package))
                          (mgl-pax-set-up-doc-buffer doc-dir)))
             :abort-cont (lambda (condition)
                           (ignore condition)
                           (mgl-pax-delete-doc-dir doc-dir)))))))))

(defun mgl-pax-in-doc-buffer-p ()
  (buffer-local-value 'mgl-pax-doc-dir (current-buffer)))

(defun mgl-pax-set-up-doc-buffer (doc-dir)
  (setq mgl-pax-doc-dir doc-dir)
  (push (current-buffer) mgl-pax-doc-buffers)
  (add-hook 'kill-buffer-hook 'mgl-pax-tear-down-doc-buffer nil :local)
  (mgl-pax-doc-set-up-key-bindings))

(defun mgl-pax-doc-set-up-key-bindings ()
  (with-suppressed-warnings ((free-vars w3m-mode-map))
    (use-local-map (copy-keymap w3m-mode-map)))
  ;; `M-.' visits the source when pressed on a "pax:" link.
  (local-set-key (kbd "M-.") 'slime-edit-definition)
  (local-set-key (kbd "M-,") 'slime-pop-find-definition-stack)
  (local-set-key (kbd "C-c C-d") 'slime-doc-map)
  ;; Make reloading regenerate the documentation.
  (local-set-key (kbd "R") 'mgl-pax-doc-reload)
  (local-set-key (kbd "n") 'mgl-pax-doc-next-definition)
  (local-set-key (kbd "p") 'mgl-pax-doc-previous-definition)
  (local-set-key (kbd "u") 'mgl-pax-doc-up-definition)
  (local-set-key (kbd "U") 'mgl-pax-doc-up-definition-and-beginning-of-buffer)
  (local-set-key (kbd "v") 'mgl-pax-doc-edit-current-definition)
  (local-set-key (kbd "V") 'mgl-pax-doc-edit-first-definition))

(defun mgl-pax-tear-down-doc-buffer ()
  (mgl-pax-delete-doc-dir)
  (setq mgl-pax-doc-buffers (remove (current-buffer) mgl-pax-doc-buffers)))

(cl-defun mgl-pax-delete-doc-dir (&optional (doc-dir mgl-pax-doc-dir))
  (when doc-dir
    ;; This could be a simple recursive delete call to
    ;; delete-directory, but this is less dangerous.
    (dolist (file (file-expand-wildcards (concat doc-dir "*.html")))
      (delete-file file nil))
    (ignore-errors
      (delete-directory doc-dir nil nil))))

(defun mgl-pax-urllike-to-url (schemeless-pax-url)
  (if (zerop (length (slime-trim-whitespace schemeless-pax-url)))
      "pax:"
    (cl-destructuring-bind (sexp-1 sexp-2 &optional fragment-1 fragment-2)
        (mgl-pax-parse-reference schemeless-pax-url :allow-fragment)
      (let ((pkg (if (slime-current-package)
                     (concat "?pkg="
                             (url-hexify-string (slime-current-package)))
                   ""))
            (fragment (if fragment-1
                          (concat "#" (url-hexify-string
                                       (format "%s %s" fragment-1 fragment-2)))
                        "")))
        (cond ((null sexp-2)
               (concat "pax:" (url-hexify-string sexp-1) pkg))
              ((null fragment-2)
               (let ((wall `((,sexp-1 (,sexp-2))
                             (,sexp-2 (,sexp-1)))))
                 (concat "pax-wall:" (url-hexify-string (format "%S" wall))
                         pkg)))
              (t
               (concat "pax:" (url-hexify-string sexp-1) " "
                       (url-hexify-string sexp-2) pkg fragment)))))))

(defun mgl-pax-names-or-locatives (sexp-1 prefix)
  (let ((values (slime-eval
                 `(mgl-pax::names-or-locatives-for-emacs
                   ,sexp-1 ,prefix
                   :definitions ',(if (eq mgl-pax-completing-for 'navigate)
                                      'dref:definitions
                                    'mgl-pax::definitions*)))))
    (if (eq (cl-first values) :error)
        (error (cl-second values))
      (cl-second values))))

;;; Make sure the dynamic binding is used below even if w3m is not
;;; loaded at compilation time.
(defvar w3m-confirm-leaving-secure-page)

(defun mgl-pax-w3m-goto-url (oldfun url &rest args)
  (if (not (or (string-prefix-p "pax:" url)
               (string-prefix-p "pax-eval:" url)
               (string-prefix-p "pax-wall:" url)))
      (let ((w3m-confirm-leaving-secure-page nil))
        (apply oldfun url args))
    ;; Set up pax e.g. when the user starts w3m, then presses g and
    ;; enters a PAX URL.
    (unless (mgl-pax-in-doc-buffer-p)
      (mgl-pax-set-up-doc-buffer
       (file-name-as-directory (make-temp-file "pax-doc" t))))
    (let ((buffer (current-buffer)))
      (mgl-pax-call-document-for-emacs url mgl-pax-doc-dir
                                       :ok-cont
                                       (lambda (url)
                                         (when url
                                           (pop-to-buffer buffer)
                                           (apply oldfun url args)))))))

(cl-defun mgl-pax-call-document-for-emacs (url dir &key ok-cont abort-cont)
  (mgl-pax-eval-async
   `(mgl-pax::document-for-emacs  ',url ',dir ',common-lisp-hyperspec-root)
   (lambda (values)
     (if (eq (cl-first values) :url)
         (apply ok-cont (cl-rest values))
       (error "%s" (cl-second values))
       (when abort-cont
         (apply abort-cont (cl-rest values)))))
   abort-cont)
  (message "Generating documentation ..."))

(defun mgl-pax-doc-reload ()
  "Like `w3m-reload-this-page', but also regenerate the documentation
if the current page was generated from a PAX URL."
  (interactive)
  (when mgl-pax-doc-dir
    (let ((buffer (current-buffer)))
      (with-suppressed-warnings ((free-vars w3m-current-url))
        (mgl-pax-call-redocument-for-emacs w3m-current-url mgl-pax-doc-dir
                                           (lambda ()
                                             (pop-to-buffer buffer)
                                             (with-no-warnings
                                               (w3m-reload-this-page))))))))

(defun mgl-pax-call-redocument-for-emacs (file-url dir cont)
  (mgl-pax-eval-async
   `(mgl-pax::redocument-for-emacs ',file-url ',dir
                                   ',common-lisp-hyperspec-root)
   (lambda (values)
     (if (eq (cl-first values) :error)
         (error "%s" (cl-second values))
       (apply cont (cl-rest values)))))
  (message "Generating documentation ..."))


;;;; Navigation commands for w3m PAX doc (see MGL-PAX::@BROWSING-WITH-W3M)
;;;;
;;;; These jump between the HTML anchors (<a id="...">) generated by
;;;; PAX before definitions (e.g. function signature lines, SECTION
;;;; titles).

(defun mgl-pax-doc-next-definition ()
  "Move point to the next PAX definition.
Use it in a PAX doc buffer (see `mgl-pax-document')."
  (interactive)
  (let ((start (mgl-pax-doc-definition-start)))
    (if (and start (< (point) start))
        (goto-char start)
      (let ((next (mgl-pax-doc-next-definition-start)))
        (if next
            (goto-char next)
          (unless start
            ;; There are no PAX definitions at all. Just move to the
            ;; next link.
            (with-no-warnings (w3m-next-anchor))))))))

(defun mgl-pax-doc-previous-definition ()
  "Move point to the previous PAX definition.
Use it in a PAX doc buffer (see `mgl-pax-document')."
  (interactive)
  (let ((start (mgl-pax-doc-definition-start)))
    (if (and start (< start (point)))
        (goto-char start)
      (let ((prev (mgl-pax-doc-prev-definition-start)))
        (if prev
            (goto-char prev)
          (unless start
            (with-no-warnings (w3m-previous-anchor))))))))

;;; Return the buffer position of the first character of the link
;;; corresponding to the current definition.
(defun mgl-pax-doc-definition-start ()
  (mgl-pax-definition-link-pos
   (previous-single-property-change (if (< (point) (buffer-size))
                                        (1+ (point))
                                      (point))
                                    'w3m-name-anchor2)))

(defun mgl-pax-doc-next-definition-start ()
  (mgl-pax-definition-link-pos
   (next-single-property-change (point) 'w3m-name-anchor2)))

(defun mgl-pax-doc-prev-definition-start ()
  (let ((this (previous-single-property-change (if (< (point) (buffer-size))
                                                   (1+ (point))
                                                 (point))
                                               'w3m-name-anchor2)))
    (when this
      (mgl-pax-definition-link-pos
       (previous-single-property-change this 'w3m-name-anchor2)))))

(defun mgl-pax-definition-link-pos (pos)
  (when pos
    (save-excursion
      (goto-char pos)
      (with-no-warnings
        (unless (mgl-pax-doc-url)
          (w3m-next-anchor)))
      (point))))

(defun mgl-pax-doc-up-definition ()
  "Follow the first \"Up:\" link at the top of the PAX documentation if any.

That is, in a PAX doc buffer (see `mgl-pax-document'), open a new
URL with the documentation of the first containing section and
put point on the definition corresponding the current page.

When there multiple sections that contain the current name, the
first one will be chosen heuristically based on the similarity of
the names of the SYMBOL-PACKAGEs of their names."
  (interactive)
  (let ((url (mgl-pax-doc-url-up)))
    (if (null url)
        (message "No containing PAX section found")
      (with-no-warnings (w3m-goto-url url))
      t)))

(defun mgl-pax-doc-up-definition-and-beginning-of-buffer ()
  "Like `mgl-pax-doc-up-definition', but also move point to
the beginning of the buffer. If there is no \"Up:\" link, then
move point to the beginning of the buffer."
  (interactive)
  (let ((url (mgl-pax-doc-url-up t)))
    (if (null url)
        (progn
          (message "No containing PAX section found")
          (goto-char (point-min)))
      (with-no-warnings (w3m-goto-url url))
      t)))

(defun mgl-pax-doc-url-up (&optional strip-fragment-p)
  (let ((up-url (mgl-pax-doc-url-up-1)))
    (when up-url
      (if strip-fragment-p
          (with-no-warnings (w3m-url-strip-fragment up-url))
        up-url))))

(defun mgl-pax-doc-url-up-1 ()
  (save-excursion
    (goto-char (point-min))
    (cond ((mgl-pax-use-w3m)
           (forward-line)
           (when (and (<= (+ (point) 4) (buffer-size))
                      (string= (buffer-substring-no-properties
                                (point) (+ (point) 4))
                               "Up: "))
             (with-no-warnings (w3m-next-anchor)
                               (w3m-anchor))))
          ;; This is only for testing non-w3m browsers
          ((search-forward "Up: " nil t)
           (w3m-anchor)))))

(defun mgl-pax-doc-edit-current-definition ()
  "Visit the source of the current PAX definition on the page."
  (interactive)
  (mgl-pax-doc-edit-pax-definition
   (or (mgl-pax-doc-current-definition-pax-url)
       ;; There is always a current definition unless the point is
       ;; before the first definition, so default to that.
       (mgl-pax-doc-first-definition-pax-url))))

(defun mgl-pax-doc-current-definition-pax-url ()
  (let ((pos (mgl-pax-doc-definition-start)))
    (when pos
      (save-excursion
        (goto-char pos)
        (mgl-pax-doc-pax-url (mgl-pax-doc-url))))))

(defun mgl-pax-doc-edit-first-definition ()
  "Visit the source of the first PAX definition on the page."
  (interactive)
  (mgl-pax-doc-edit-pax-definition (mgl-pax-doc-first-definition-pax-url)))

(defun mgl-pax-doc-first-definition-pax-url ()
  (save-excursion
    (goto-char (point-min))
    (mgl-pax-doc-next-definition)
    (mgl-pax-doc-pax-url (mgl-pax-doc-url))))

(defun mgl-pax-doc-url ()
  (when (featurep 'w3m)
    (w3m-anchor)))


;;;; Make `M-.' (`slime-edit-definition') work on links in w3m PAX
;;;; doc.

;;; If over a link in a w3m buffer, then visit the source if it is a
;;; "pax:" or "file:" URL, else do nothing. For "pax:" URLs, the URL
;;; itself identifies the target. For "file:" URLs, the target is the
;;; PAX reference encoded in the fragment part of the URL if any.
(defun mgl-pax-doc-edit-definition (name &optional where)
  (ignore name where)
  (let ((url (mgl-pax-doc-pax-url (mgl-pax-doc-url))))
    (mgl-pax-doc-edit-pax-definition url)))

(add-hook 'slime-edit-definition-hooks 'mgl-pax-doc-edit-definition)

(defun mgl-pax-doc-edit-pax-definition (pax-url)
  (when (string-prefix-p "pax:" pax-url)
    (let ((dspec-and-location-list
           (slime-eval `(mgl-pax::locate-pax-url-for-emacs ',pax-url))))
      (if dspec-and-location-list
          (mgl-pax-visit-locations dspec-and-location-list)
        (error "No source location for %s"
               (cl-subseq (url-unhex-string pax-url) 4))))))

(defun mgl-pax-doc-pax-url (url)
  (cond ((string-prefix-p "pax:" url)
         url)
        ((string-prefix-p "file:" url)
         (let ((fragment (elt (mgl-pax-parse-path-and-fragment url) 1)))
           (when fragment
             (concat "pax:" fragment))))))

(defun mgl-pax-parse-path-and-fragment (url)
  (let ((fragment-pos (cl-position ?# url :start 1)))
    (if fragment-pos
        (list (cl-subseq url 0 fragment-pos)
              (cl-subseq url (1+ fragment-pos)))
      (list url nil))))


;;;; Clicking on a locative in a non-w3m browser focusses on the Emacs
;;;; window and visit the source location of the corresponding
;;;; definition (see MGL-PAX::@BROWSING-WITH-OTHER-BROWSERS).

(defun mgl-pax-edit-for-cl (dspec-and-location-list)
  ;; There may be no `lisp-mode' buffer at all.
  (ignore-errors (slime-recently-visited-buffer 'lisp-mode))
  (mgl-pax-sync-current-buffer)
  (x-focus-frame nil)
  (raise-frame)
  (mgl-pax-visit-locations dspec-and-location-list))

(defun mgl-pax-sync-current-buffer ()
  ;; https://emacs.stackexchange.com/q/10921
  (set-buffer (window-buffer (selected-window))))


(defun mgl-pax-current-definition-toggle-view ()
  "Document the definition `point' is in with `mgl-pax-document'.
In a PAX doc buffer, it's equivalent to pressing `v'
(`mgl-pax-doc-edit-current-definition')."
  (interactive)
  (if (mgl-pax-in-doc-buffer-p)
      (mgl-pax-doc-edit-current-definition)
    (mgl-pax-with-component (:mgl-pax/document)
      (mgl-pax-document (mgl-pax-current-definition-pax-url)))))

(defun mgl-pax-current-definition-pax-url ()
  (let ((values (slime-eval `(mgl-pax::current-definition-pax-url-for-emacs
                              ',(buffer-name)
                              ',(buffer-file-name)
                              ',(mgl-pax-current-definition-possible-names)))))
    (if (eq (cl-first values) :error)
        (error "%s" (cl-second values))
      (cl-second values))))


;;;; Hideshow documentation and comments
;;;;
;;;; These do not depend on Slime or the Common Lisp side at all.

(defun mgl-pax-hideshow-documentation ()
  "Toggle the folding and unfolding of DEFSECTION and
DEFINE-GLOSSARY-TERM forms and strings in the current buffer with
`hideshow-minor-mode'. If there are such non-hidden forms, then
hide them, else show them all."
  (interactive)
  (hs-minor-mode t)
  (let ((hs-hide-all-non-comment-function
         #'mgl-pax-hs-hide-all-non-comment)
        (hs-block-start-regexp "\\s\"\\|\\s(")
        (hs-block-end-regexp (concat "\\s\"\\|\\s)"))
        (hs-c-start-regexp "dsafsoiufd")
        (orig-n-overlays (mgl-pax-n-overlays))
        ;; Without this `hs-hide-all' discards all overlays.
        (hs-allow-nesting t))
    (hs-hide-all)
    (let ((new-n-overlays (mgl-pax-n-overlays)))
      (if (/= new-n-overlays orig-n-overlays)
          (message "Folded %S documentation forms and long strings"
                   (- new-n-overlays orig-n-overlays))
        (mgl-pax-delete-hs-overlays 'code)
        (let ((new-2-n-overlays (mgl-pax-n-overlays)))
          (if (/= new-2-n-overlays orig-n-overlays)
              (message "Unfolded %S documentation forms and long strings"
                       (- orig-n-overlays new-2-n-overlays))
            (message "No documentation forms or long strings")))))))

(defun mgl-pax-hideshow-comments ()
  "Toggle the folding and unfolding comments in the current
buffer with `hideshow-minor-mode'. If there are non-hidden
comments, then hide them, else show them all."
  (interactive)
  (hs-minor-mode t)
  (let ((hs-hide-all-non-comment-function (lambda ()))
        (hs-hide-comments-when-hiding-all t)
        (orig-n-overlays (mgl-pax-n-overlays))
        (hs-allow-nesting t))
    ;; KLUDGE: `hs-hide-comment-region' accumulates overlays with
    ;; `hs-allow-nesting', so let's delete them.
    (mgl-pax-delete-hs-overlays 'comment)
    (hs-hide-all)
    (let ((new-n-overlays (mgl-pax-n-overlays)))
      (if (/= new-n-overlays orig-n-overlays)
          (message "Folded %S comments" (- new-n-overlays orig-n-overlays))
        (mgl-pax-delete-hs-overlays 'comment)
        (let ((new-2-n-overlays (mgl-pax-n-overlays)))
          (if (/= new-2-n-overlays orig-n-overlays)
              (message "Unfolded %S comments"
                       (- orig-n-overlays new-2-n-overlays))
            (message "No comments")))))))

;;; https://emacs.stackexchange.com/a/20925
(defun mgl-pax-n-overlays ()
  (length (overlays-in (point-min) (point-max))))

(defun mgl-pax-delete-hs-overlays (kind)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (eq (overlay-get ov 'hs) kind)
      (delete-overlay ov))))

;;; To be bound to `hs-hide-all-non-comment-function'
(defun mgl-pax-hs-hide-all-non-comment ()
  (cond
   ;; KLUDGE: This can be called in comments after all.
   ((mgl-pax-in-comment-p)
    (goto-char (cl-second (mgl-pax-comment-lines-bounds))))
   ((and (hs-looking-at-block-start-p)
         (looking-at mgl-pax-doc-form-regexp))
    (save-excursion (hs-hide-block))
    ;; Do not recurse in hidden stuff.
    (forward-sexp 1))
   ((looking-at "\\s(")
    ;; Recurse into lists.
    (forward-char))
   ((looking-at "\\s\"")
    (save-excursion (hs-hide-block))
    (forward-sexp))))

(defvar mgl-pax-doc-form-regexp
  ;; \\S- matches non-whitespace
  "(\\(\\|\\S-+:\\)\\(defsection\\|define-glossary-term\\)")


;;;; Apropos (see MGL-PAX::@APROPOS)

(defun mgl-pax-apropos (string &optional external-only package
                               case-sensitive)
  "Show all PAX definitions that match the arguments.
This is a frontend for DREF:DREF-APROPOS. The syntax is described
in detail in MGL-PAX::@APROPOS. Here, we give only a few
examples.

STRING is basically NAME and an optional DTYPE:

- \":print\", where the colon means exact matching of the
  name (CL:SYMBOL-NAME if the name is a symbol), or

- \"rin function\", where \"rin\" is for substring match, and
  \"function\" restricts the matches to function definitions.

- \"\\\"rin\\\" (or function macro)\"

PACKAGE is a pattern like NAME or the special symbols :NONE or
:ANY.

Matching of NAME and PACKAGE is subject to CASE-SENSITIVE.

With a prefix arg, you're interactively asked for parameters of
the search. Without a prefix arg, EXTERNAL-ONLY defaults to T,
packages are not filtered, and case does not matter.

Also, see `mgl-pax-apropos-all'."
  (interactive (list nil nil nil nil))
  (mgl-pax-with-component (:mgl-pax/document)
    (mgl-pax-document
     (mgl-pax-make-pax-eval-url
      (if string
          `(mgl-pax::pax-apropos* ,string ,external-only
                                  ,package ,case-sensitive)
        `(mgl-pax::pax-apropos*
          ;; Do the defaulting of arguments here instead of in
          ;; `interactive' because
          ;; `mgl-pax-read-from-minibuffer-for-apropos' relies on
          ;; MGL-PAX/NAVIGATE being loaded.
          ,@(if current-prefix-arg
                (list (mgl-pax-read-from-minibuffer "PAX Apropos: "
                                                    'apropos)
                      (y-or-n-p "External symbols only? ")
                      (mgl-pax-read-apropos-package-from-minibuffer)
                      (y-or-n-p "Case-sensitive? "))
              (list (mgl-pax-read-from-minibuffer "PAX Apropos: "
                                                  'apropos)
                    t nil nil))))))))

(defun mgl-pax-completions-at-point-for-apropos-dtype (start)
  ;; For apropos, the first sexp is a pattern for a name, not a whole
  ;; name, so it does not affect completion.
  (let ((start (max start (slime-symbol-start-pos))))
    (list start (point)
          (mgl-pax-eval `(mgl-pax::dtype-symbols-for-emacs
                          ,(buffer-substring-no-properties
                            start (point)))))))

(defun mgl-pax-read-apropos-package-from-minibuffer ()
  (mgl-pax-read-from-minibuffer "Package pattern: " 'apropos-package))

(defun mgl-pax-completions-at-point-for-apropos-package ()
  (list (line-beginning-position) (point)
        (cl-list* ":none" ":any" (mgl-pax-package-names-as-keywords))))

(defun mgl-pax-package-names-as-keywords ()
  (mapcar (lambda (name)
            (concat ":" name))
          (slime-eval `(swank:list-all-package-names t))))

(defun mgl-pax-make-pax-eval-url (sexp)
  (concat "pax-eval:" (url-encode-url (prin1-to-string sexp))))

(defun mgl-pax-apropos-all (string)
  "Shortcut for invoking `mgl-pax-apropos' with EXTERNAL-ONLY NIL."
  (interactive (list nil))
  (mgl-pax-with-component (:mgl-pax/document)
    (let ((string (or string (mgl-pax-read-from-minibuffer
                              "PAX Apropos All: " 'apropos))))
      (mgl-pax-apropos string nil "" nil))))

(defun mgl-pax-apropos-package (package &optional internal)
  "Show apropos listing for symbols in PACKAGE.
With prefix argument include internal symbols.
The empty string means the current package."
  (interactive (list (let ((pkg (slime-read-package-name
                                 "PAX Apropos for Package: ")))
                       (if (string= pkg "") (slime-current-package) pkg))
                     current-prefix-arg))
  (mgl-pax-with-component (:mgl-pax/document)
    (mgl-pax-apropos "" (not internal) `'(,package) nil)))


;;;; Transcribe (see MGL-PAX::@TRANSCRIBING-WITH-EMACS)

(defun mgl-pax-transcribe-last-expression ()
  "A bit like C-u C-x C-e (`slime-eval-last-expression') that
inserts the output and values of the sexp before point, this does
the same but with MGL-PAX:TRANSCRIBE. Use a numeric prefix
argument as in index to select one of the Common Lisp
MGL-PAX:*SYNTAXES* as the SYNTAX argument to MGL-PAX:TRANSCRIBE.
Without a prefix argument, the first syntax is used."
  (interactive)
  (mgl-pax-with-component (:mgl-pax/transcribe)
    (let* ((dynenv (mgl-pax-find-cl-transcript-dynenv))
           (sexp (mgl-pax-call-uncommented 'slime-last-expression))
           (start (point))
           (prefix (save-excursion
                     ;; Go to the first line of the sexp.
                     (forward-line (- (cl-count ?\n sexp)))
                     (mgl-pax-line-prefix))))
      (unless (mgl-pax-blank-line-prefix-p)
        (insert "\n"))
      (insert
       (mgl-pax-transcribe sexp (mgl-pax-transcribe-syntax-arg)
                           nil nil nil dynenv))
      (string-insert-rectangle
       (save-excursion (goto-char start)
                       (forward-line 1)
                       (point))
       (save-excursion (forward-line -1) (point))
       prefix)
      ;; The transcript ends with a newline. Delete it if it would
      ;; result in a blank line.
      (when (looking-at "\n")
        (delete-char 1)))))

(defun mgl-pax-blank-line-prefix-p ()
  (let ((string (buffer-substring-no-properties (line-beginning-position)
                                                (point))))
    (string-match-p "^[[:space:];]*$" string)))

;;; Return the longest run of whitespace and semicolon characters at
;;; the beginning of the current line as a string.
(defun mgl-pax-line-prefix ()
  (save-excursion
    ;; This may more after a prompt on the line ...
    (move-beginning-of-line nil)
    ;; ... so don't match the true beginning of the line with ^.
    (re-search-forward "[[:space:];]*")
    (match-string 0)))

(defun mgl-pax-retranscribe-region (start end)
  "Updates the transcription in the current region (as in calling
MGL-PAX:TRANSCRIBE with :UPDATE-ONLY T). Use a numeric prefix
argument as an index to select one of the Common Lisp
MGL-PAX:*TRANSCRIBE-SYNTAXES* as the SYNTAX argument to
MGL-PAX:TRANSCRIBE. Without a prefix argument, the syntax of the
input will not be changed."
  (interactive "r")
  (mgl-pax-with-component (:mgl-pax/transcribe)
    (let ((dynenv (mgl-pax-find-cl-transcript-dynenv)))
      (let ((point-at-start-p (= (point) start))
            (transcript (mgl-pax-transcribe
                         (buffer-substring-no-properties start end)
                         (mgl-pax-transcribe-syntax-arg)
                         t t nil dynenv)))
        (if (string= transcript (buffer-substring-no-properties start end))
            (deactivate-mark)
          (if point-at-start-p
              (save-excursion
                (goto-char start)
                (delete-region start end)
                (insert transcript))
            (save-excursion
              (goto-char start)
              (delete-region start end))
            (insert transcript)))))))

(defun mgl-pax-transcribe-syntax-arg ()
  (if current-prefix-arg
      (prefix-numeric-value current-prefix-arg)
    nil))

;;; Within the current defun, find the first occurrence of "```"
;;; backwards from point, and if it is followed by "cl-transcript",
;;; return its dynenv argument."
(defun mgl-pax-find-cl-transcript-dynenv ()
  (save-excursion
    (save-restriction
      (narrow-to-defun)
      (when (search-backward "```" nil t)
        (when (looking-at "```cl-transcript")
          (save-restriction
            (narrow-to-region (point) (save-excursion
                                        (end-of-line)
                                        (point)))
            (when (search-forward ":dynenv" nil t)
              (mgl-pax-next-sexp))))))))

(defun mgl-pax-transcribe (string syntax update-only echo
                                  first-line-special-p dynenv)
  (slime-eval `(mgl-pax::transcribe-for-emacs
                ,string ',syntax ',update-only ',echo ',first-line-special-p
                ,dynenv)))


(provide 'mgl-pax)
