;;; Code transcription

(defun 40ants-doc-lisp-eval (form)
  (cond
   ((and (fboundp 'sly-connected-p)
         (sly-connected-p))
    (sly-eval form))
   ((and (fboundp 'slime-connected-p)
         (slime-connected-p))
    (slime-eval form))
   (t
    (error "Nor SLY, nor SLIME is connected to the Lisp."))))


(defun 40ants-doc-transcribe-last-expression ()
  "A bit like C-u C-x C-e (slime-eval-last-expression) that
inserts the output and values of the sexp before the point, this
does the same but with 40ANTS-DOC-FULL/TRANSCRIBE:TRANSCRIBE. Use a numeric prefix
argument as in index to select one of the Common Lisp
40ANTS-DOC-FULL/TRANSCRIBE:*SYNTAXES* as the SYNTAX argument to 40ANTS-DOC-FULL/TRANSCRIBE:TRANSCRIBE.
Without a prefix argument, the first syntax is used."
  (interactive)
  (insert
   (save-excursion
     (let* ((end (point))
            (start (progn (backward-sexp)
                          (move-beginning-of-line nil)
                          (point))))
       (40ants-doc-transcribe start end (40ants-doc-transcribe-syntax-arg)
                           nil nil nil)))))

(defun 40ants-doc-retranscribe-region (start end)
  "Updates the transcription in the current region (as in calling
40ANTS-DOC-FULL/TRANSCRIBE:TRANSCRIBE with :UPDATE-ONLY T). Use a numeric prefix
argument as in index to select one of the Common Lisp
40ANTS-DOC-FULL/TRANSCRIBE:*SYNTAXES* as the SYNTAX argument to 40ANTS-DOC-FULL/TRANSCRIBE:TRANSCRIBE.
Without a prefix argument, the syntax of the input will not be
changed."
  (interactive "r")
  (let ((point-at-start-p (= (point) start)))
    ;; We need to extend selection to the
    ;; beginning of line because otherwise
    ;; block's indentation might be wrong and
    ;; transcription parsing will fail
    (goto-char start)
    (move-beginning-of-line nil)
    (setf start
          (point))
    
    (let ((transcript (40ants-doc-transcribe start end
                                          (40ants-doc-transcribe-syntax-arg)
                                          t t nil)))
      (if point-at-start-p
          (save-excursion
            (goto-char start)
            (delete-region start end)
            (insert transcript))
        (save-excursion
          (goto-char start)
          (delete-region start end))
        (insert transcript)))))

(defun 40ants-doc-transcribe-syntax-arg ()
  (if current-prefix-arg
      (prefix-numeric-value current-prefix-arg)
    nil))

(defun 40ants-doc-transcribe (start end syntax update-only echo
                                    first-line-special-p)
  (let ((transcription
         (40ants-doc-lisp-eval
          `(cl:if (cl:find-package :40ants-doc-full/transcribe)
                  (uiop:symbol-call :40ants-doc-full/transcribe :transcribe-for-emacs
                                    ,(buffer-substring-no-properties start end)
                                    ',syntax ',update-only ',echo ',first-line-special-p)
                  t))))
    (if (eq transcription t)
        (error "40ANTS-DOC is not loaded.")
      transcription)))

