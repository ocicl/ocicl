(mgl-pax:define-package :dref-test-autoload
  (:use #:common-lisp #:dref #:dref-ext #:try)
  (:export #:test))

(in-package :dref-test-autoload)

(define-symbol-locative-type aaa ())

(define-definer-for-symbol-locative-type define-aaa aaa)

(define-aaa aaa1 ()
  "This is AAA1.")

(defun dref-full-loaded-p ()
  (fboundp 'dref::dref-sections))

;;;; Autoload tests must be run one-by-one in a fresh lisp after the
;;;; DREF/TEST-AUTOLOAD system has been loaded.

(deftest test-locate-autoload ()
  (is (not (dref-full-loaded-p)))
  (is (eq (resolve (dref 'locate 'function)) (symbol-function 'locate)))
  (is (dref-full-loaded-p)))

(deftest test-resolve-autoload ()
  (is (not (dref-full-loaded-p)))
  (dref:resolve nil nil)
  (is (dref-full-loaded-p)))

(deftest test-arglist-autoload ()
  (is (not (dref-full-loaded-p)))
  (ignore-errors (arglist nil))
  (is (dref-full-loaded-p)))

(deftest test-docstring-autoload ()
  (is (not (dref-full-loaded-p)))
  (ignore-errors (docstring nil))
  (is (dref-full-loaded-p)))

(deftest test-source-location-autoload ()
  (is (not (dref-full-loaded-p)))
  (ignore-errors (source-location nil))
  (is (dref-full-loaded-p)))

(deftest test-definitions-autoload ()
  (is (not (dref-full-loaded-p)))
  (definitions nil)
  (is (dref-full-loaded-p)))

(deftest test-dref-apropos-autoload ()
  (is (not (dref-full-loaded-p)))
  (dref-apropos 'lkdsajfkjsaf)
  (is (dref-full-loaded-p)))

(deftest test-dref-this-source-location-autoload ()
  (is (not (dref-full-loaded-p)))
  (let ((obj (this-source-location)))
    (is (not (dref-full-loaded-p)))
    (let ((sl (funcall obj)))
      (is (or (source-location-p sl)
              (and (listp sl)
                   (eq (first sl) :error))))))
  (is (dref-full-loaded-p)))

;;; These are currently not tested:
;;; make-source-location
;;; source-location-p
;;; source-location-file
;;; source-location-file-position
;;; source-location-buffer
;;; source-location-buffer-position
;;; source-location-snippet
;;; source-location-adjusted-file-position
