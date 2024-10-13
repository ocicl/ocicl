;;; M-. integration

(defun 40ants-doc-edit-locative-definition (name &optional where)
  (or (40ants-doc-locate-definition name (40ants-doc-locative-before))
      (40ants-doc-locate-definition name (40ants-doc-locative-after))
      (40ants-doc-locate-definition name (40ants-doc-locative-after-in-brackets))
      ;; support "foo function" and "function foo" syntax in
      ;; interactive use
      (let ((pos (cl-position ?\s name)))
        (when pos
          (or (40ants-doc-locate-definition (cl-subseq name 0 pos)
                                            (cl-subseq name (1+ pos)))
              (40ants-doc-locate-definition (cl-subseq name (1+ pos))
                                            (cl-subseq name 0 pos)))))))

(defun 40ants-doc-locative-before ()
  (ignore-errors (save-excursion
                   (slime-beginning-of-symbol)
                   (slime-last-expression))))

(defun 40ants-doc-locative-after ()
  (ignore-errors (save-excursion
                   (slime-end-of-symbol)
                   (slime-forward-sexp)
                   (slime-last-expression))))

(defun 40ants-doc-locative-after-in-brackets ()
  (ignore-errors (save-excursion
                   (slime-end-of-symbol)
                   (skip-chars-forward "`" (+ (point) 1))
                   (when (and (= 1 (skip-chars-forward "\\]" (+ (point) 1)))
                              (= 1 (skip-chars-forward "\\[" (+ (point) 1))))
                     (buffer-substring-no-properties
                      (point)
                      (progn (search-forward "]" nil (+ (point) 1000))
                             (1- (point))))))))

(defun 40ants-doc-locate-definition (name locative)
  (when locative
    (let ((location
           (slime-eval
            ;; Silently fail if mgl-pax is not loaded.
            `(cl:when (cl:find-package :mgl-pax)
                      (cl:funcall
                       (cl:find-symbol
                        (cl:symbol-name :locate-definition-for-emacs) :mgl-pax)
                       ,name ,locative)))))
      (when (and (consp location)
                 (not (eq (car location) :error)))
        (slime-edit-definition-cont
         (list (make-slime-xref :dspec `(,name)
                                :location location))
         "dummy name"
         where)))))

(when (boundp 'slime-edit-definition-hooks)
  (add-hook 'slime-edit-definition-hooks '40ants-doc-edit-locative-definition))
