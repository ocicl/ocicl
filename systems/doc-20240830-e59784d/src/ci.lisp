(uiop:define-package #:40ants-doc/ci
  (:use #:cl)
  (:import-from #:40ants-ci/workflow
                #:defworkflow)
  (:import-from #:40ants-ci/jobs/linter)
  (:import-from #:40ants-ci/jobs/run-tests)
  (:import-from #:40ants-ci/jobs/docs)
  (:import-from #:40ants-ci/jobs/autotag
                #:autotag))
(in-package #:40ants-doc/ci)


(defparameter *lisp-implementations*
  (list "sbcl-bin"
        ;; Some tests fail on CCL
        ;; "ccl-bin"
        ;; CLISP is not supported by bordeaux-threads
        ;; but it is a transitive dependency
        ;; "clisp-head"
        ;; Some tests fail on ABCL
        ;; "abcl-bin"
        ;; At 2023-04-22 tests started to fail on Allegro with error:
        ;; Allegro CL(pid 6257): System Error (gsgc) scavenge found ref to cons outside cons area in 0xffba645c
        ;; "allegro"
        ;; CLASP ails with "Too many arguments for option DOCUMENTATION" error:
        ;; "clasp-bin"
        ;; CMU fails with "Redefining slot accessor CHUNK-CACHE-CHUNKS for structure type CHUNK-CACHE" error,
        ;; occured somewhere inside esrap library:
        ;; "cmu-bin"
        ;; Lispworks is not supported by setup-lisp action:
        ;; "lispworks"
        ;; MKCL has problems when setup-lisp tries to install it:
        ;; https://github.com/40ants/setup-lisp/issues/17
        ;; "mkcl"
        ;; This fails to install under the Roswell on Ubuntu
        ;; "npt"
        "ecl") )


(defworkflow release
  :on-push-to "master"
  :jobs ((autotag)))


(defworkflow linter
  :on-push-to "master"
  :on-pull-request t
  :jobs ((40ants-ci/jobs/linter:linter
          :asdf-systems ("40ants-doc"
                         "40ants-doc-full"
                         "40ants-doc-test")
          :check-imports t)))


(defworkflow ci
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :jobs ((40ants-ci/jobs/run-tests:run-tests
          :asdf-system "40ants-doc-full"
          :quicklisp ("quicklisp"
                      "ultralisp")
          :lisp *lisp-implementations*
          :exclude (append
                    ;; All implementations except SBCL we'll check only on
                    ;; and Ultralisp dist.
                    (loop for lisp in *lisp-implementations*
                          unless (string-equal lisp "sbcl-bin")
                            append (list (list :quicklisp "quicklisp"
                                               :lisp lisp))))
          :coverage t)))


(defworkflow docs
  :on-push-to "master"
  :on-pull-request t
  :jobs ((40ants-ci/jobs/docs:build-docs
          :asdf-system "40ants-doc-full")))
