(mgl-pax:define-package :mgl-pax-test-extension
  (:use #:common-lisp #:mgl-pax #:try)
  (:export #:test))

(in-package :mgl-pax-test-extension)

(defun external-symbols ()
  (let ((externals ()))
    (do-external-symbols (sym '#:mgl-pax)
      (push sym externals))
    (sort externals #'string< :key #'symbol-name)))

(defvar *externals-before* (external-symbols))

(deftest test-exports ()
  (let ((externals (external-symbols)))
    (is (endp (set-difference externals *externals-before*)))
    (is (endp (set-difference *externals-before* externals)))))

(dref-ext:define-symbol-locative-type aaa ())

(dref-ext:define-definer-for-symbol-locative-type define-aaa aaa)

(define-aaa aaa1 ()
  "This is AAA1.")

(defun navigate-system-loaded-p ()
  (boundp 'pax::@navigating-in-emacs))

(defun document-system-loaded-p ()
  (boundp 'pax::@generating-documentation))

(defun transcribe-system-loaded-p ()
  (fboundp 'pax::find-syntax))

;;;; Autoload tests must be run one-by-one in a fresh lisp after the
;;;; MGL-PAX/TEST-EXTENSION system has been loaded.

(deftest test-document-autoload ()
  (is (not (document-system-loaded-p)))
  (let ((*document-max-numbering-level* 1))
    (signals-not (error)
      (document (dref:dref 'aaa1 'aaa))))
  (is (document-system-loaded-p))
  (with-failure-expected ((and (alexandria:featurep
                                '(:not (:or :allegro :ccl :ecl :sbcl)))
                               'failure))
    (when (is (boundp '*document-max-numbering-level*))
      (is (eql *document-max-numbering-level* 3)))))

(deftest test-document-for-emacs-autoload ()
  (is (not (document-system-loaded-p)))
  (is (eq (first (pax::document-for-emacs nil nil)) :error))
  (is (document-system-loaded-p)))

(deftest test-locate-definitions-for-emacs-autoload ()
  (is (not (navigate-system-loaded-p)))
  (is (null (pax::locate-definitions-for-emacs ())))
  (is (navigate-system-loaded-p)))

(deftest test-transcribe-autoload ()
  (is (not (transcribe-system-loaded-p)))
  (is (equal (pax:transcribe ":xxx" nil)
             ":xxx
=> :XXX
"))
  (is (transcribe-system-loaded-p)))

(deftest test-transcribe-for-emacs-autoload ()
  (is (not (transcribe-system-loaded-p)))
  (is (equal (pax::transcribe-for-emacs ":xxx" NIL NIL NIL NIL NIL)
             "
=> :XXX
"))
  (is (transcribe-system-loaded-p)))
