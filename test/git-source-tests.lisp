;;; git-source-tests.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Unit tests for git+ source and fullname parsing (src/git.lisp).

(defpackage :ocicl-git-source-tests
  (:use :cl)
  (:export #:run-all-tests #:*test-failed*))

(in-package :ocicl-git-source-tests)

(defvar *test-failed* 0)
(defvar *test-passed* 0)

(defmacro check (label form)
  `(handler-case
       (if ,form
           (progn (incf *test-passed*)
                  (format t "PASS  ~A~%" ,label))
           (progn (incf *test-failed*)
                  (format t "FAIL  ~A~%" ,label)))
     (error (e)
       (incf *test-failed*)
       (format t "FAIL  ~A (unexpected error: ~A)~%" ,label e))))

(defmacro check-errors (label form)
  `(handler-case
       (progn ,form
              (incf *test-failed*)
              (format t "FAIL  ~A (no error signaled)~%" ,label))
     (error ()
       (incf *test-passed*)
       (format t "PASS  ~A~%" ,label))))

(defun source= (source url ref subdir)
  (let ((parsed (ocicl::parse-git-source source)))
    (and (equal (getf parsed :url) url)
         (equal (getf parsed :ref) ref)
         (equal (getf parsed :subdir) subdir))))

(defun run-all-tests ()
  (let ((*test-failed* 0))
    (declare (special *test-failed*))

    ;; parse-git-source: user-supplied sources
    (check "plain https URL"
           (source= "git+https://github.com/me/my-lib"
                    "https://github.com/me/my-lib" nil nil))
    (check ".git suffix is part of the URL"
           (source= "git+https://github.com/me/my-lib.git"
                    "https://github.com/me/my-lib.git" nil nil))
    (check "branch ref after @"
           (source= "git+https://github.com/me/my-lib@main"
                    "https://github.com/me/my-lib" "main" nil))
    (check "commit SHA after @"
           (source= "git+https://github.com/me/my-lib@6f6959222b65a3d44a818b12b3a104cf822fcd91"
                    "https://github.com/me/my-lib"
                    "6f6959222b65a3d44a818b12b3a104cf822fcd91" nil))
    (check "ref as fragment parameter"
           (source= "git+https://github.com/me/my-lib#ref=main"
                    "https://github.com/me/my-lib" "main" nil))
    (check "ref with slash via fragment parameter"
           (source= "git+https://github.com/me/my-lib#ref=feature/foo"
                    "https://github.com/me/my-lib" "feature/foo" nil))
    (check "subdirectory parameter"
           (source= "git+https://github.com/me/mono#subdirectory=libs/my-lib"
                    "https://github.com/me/mono" nil "libs/my-lib"))
    (check "ref and subdirectory together"
           (source= "git+https://github.com/me/mono@v1.0#subdirectory=libs/my-lib"
                    "https://github.com/me/mono" "v1.0" "libs/my-lib"))
    (check "ssh URL authority @ is not a ref"
           (source= "git+ssh://git@github.com/me/my-lib"
                    "ssh://git@github.com/me/my-lib" nil nil))
    (check "ssh URL with ref"
           (source= "git+ssh://git@github.com/me/my-lib@v2"
                    "ssh://git@github.com/me/my-lib" "v2" nil))
    (check "scp-style URL without ref"
           (source= "git+git@github.com:my-lib"
                    "git@github.com:my-lib" nil nil))
    (check "scp-style URL with ref"
           (source= "git+git@github.com:me/my-lib@main"
                    "git@github.com:me/my-lib" "main" nil))
    (check-errors "both @REF and #ref= is an error"
                  (ocicl::parse-git-source "git+https://github.com/me/my-lib@main#ref=dev"))
    (check-errors "comma is rejected"
                  (ocicl::parse-git-source "git+https://github.com/me/my-lib@ma,in"))
    (check-errors "dot-dot subdirectory is rejected"
                  (ocicl::parse-git-source "git+https://github.com/me/mono#subdirectory=../evil"))
    (check-errors "absolute subdirectory is rejected"
                  (ocicl::parse-git-source "git+https://github.com/me/mono#subdirectory=/etc"))
    (check-errors "unknown fragment parameter is rejected"
                  (ocicl::parse-git-source "git+https://github.com/me/my-lib#egg=foo"))
    (check-errors "non-git source is rejected"
                  (ocicl::parse-git-source "https://github.com/me/my-lib"))

    ;; make-git-fullname / parse-git-fullname round trips
    (let ((sha "6f6959222b65a3d44a818b12b3a104cf822fcd91"))
      (check "fullname round trip: bare pin"
             (multiple-value-bind (url sha2 ref subdir)
                 (ocicl::parse-git-fullname
                  (ocicl::make-git-fullname "https://github.com/me/my-lib" sha))
               (and (equal url "https://github.com/me/my-lib")
                    (equal sha2 sha) (null ref) (null subdir))))
      (check "fullname round trip: ref and subdirectory"
             (multiple-value-bind (url sha2 ref subdir)
                 (ocicl::parse-git-fullname
                  (ocicl::make-git-fullname "https://github.com/me/mono" sha
                                            :ref "feature/foo" :subdir "libs/my-lib"))
               (and (equal url "https://github.com/me/mono")
                    (equal sha2 sha)
                    (equal ref "feature/foo")
                    (equal subdir "libs/my-lib"))))
      (check "fullname round trip: ref needing percent-encoding"
             (multiple-value-bind (url sha2 ref subdir)
                 (ocicl::parse-git-fullname
                  (ocicl::make-git-fullname "https://github.com/me/my-lib" sha
                                            :ref "odd&ref=x"))
               (declare (ignore url subdir))
               (and (equal sha2 sha) (equal ref "odd&ref=x"))))
      (check "fullname with subdirectory but no ref parses"
             (multiple-value-bind (url sha2 ref subdir)
                 (ocicl::parse-git-fullname
                  (format nil "git+https://github.com/me/mono@~A#subdirectory=libs/my-lib" sha))
               (and (equal url "https://github.com/me/mono")
                    (equal sha2 sha) (null ref)
                    (equal subdir "libs/my-lib"))))
      (check-errors "fullname without a full SHA pin is rejected"
                    (ocicl::parse-git-fullname "git+https://github.com/me/my-lib@main")))

    ;; git-repo-basename / git-tree-dirname
    (check "basename from https URL"
           (equal (ocicl::git-repo-basename "https://github.com/me/my-lib") "my-lib"))
    (check "basename strips .git and trailing slash"
           (equal (ocicl::git-repo-basename "https://github.com/me/my-lib.git/") "my-lib"))
    (check "basename from scp-style URL"
           (equal (ocicl::git-repo-basename "git@github.com:me/my-lib") "my-lib"))
    (check "basename prefers the subdirectory"
           (equal (ocicl::git-repo-basename "https://github.com/me/mono" "libs/inner") "inner"))
    (check "tree dirname carries a short pin"
           (equal (ocicl::git-tree-dirname "https://github.com/me/my-lib"
                                           "6f6959222b65a3d44a818b12b3a104cf822fcd91")
                  "my-lib-6f69592"))

    (format t "~%~D passed, ~D failed~%" *test-passed* *test-failed*)
    *test-failed*))
