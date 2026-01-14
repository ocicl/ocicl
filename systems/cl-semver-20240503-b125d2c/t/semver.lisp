(in-package :semver.test)

(in-root-suite)

(defsuite semver-test)

(in-suite semver-test)

(defun run-tests ()
  (without-debugging
    (semver-test)))

#.(enable-version-syntax)

(deftest version-parsing-test ()
  (let ((valid-versions
          (list "1.2.0" "0.1.2"
                "1.2.0-alpha" "1.2.0-alpha.1"
                "1.2.0+build" "1.2.0+build.1"
                "1.2.0-alpha+build" "1.2.0-alpha.1+build.2")))
    (loop for version in valid-versions
          do (is (version-string-valid-p version))))

  (let ((invalid-versions (list "1.2.a")))
    (loop for version in invalid-versions
          do (is (not (version-string-valid-p version)))))
  (let ((pre-releases (list "1.0.0-alpha" "1.0.0-alpha.1"
                            "1.0.0-0.3.7" "1.0.0-x.7.z.92")))
    (loop for version in pre-releases do
      (is (version-string-valid-p version)))))

(deftest version-construction-test ()
  (let ((v1 (read-version-from-string "1.2.0"))
        (v2 (make-instance 'semantic-version
                           :major 1
                           :minor 2
                           :patch 0)))
    (is (version= v1 v2))))

(deftest version-invalid-arguments-test ()
  (signals error
    (make-semantic-version "a" 2 0))
  (signals error
    (make-semantic-version 1 "a" 0))
  (signals error
    (make-semantic-version 1 2 "a"))
  (signals error
    (read-version-from-string "1.2.a")))

(deftest version-printing-test ()
  (let ((version-strings (list "1.2.0" "0.1.2"
                               "1.2.0-alpha" "1.2.0-alpha.1"
                               "1.2.0+build" "1.2.0+build.1"
                               "1.2.0-alpha+build" "1.2.0-alpha.1+build.2")))
    (loop for version-string in version-strings
          do (is (equalp (print-version-to-string (read-version-from-string version-string))
                         version-string)))))

(deftest version-comparison-test ()
  (is (version= #v"1.2.0" #v"1.2.0"))
  (is (version=   "1.2.0"   "1.2.0"))
  (is (version= #v"1.2.0"   "1.2.0"))
  (is (version=   "1.2.0" #v"1.2.0"))
  (is (not (version= #v"1.2.0" #v"1.2.1")))
  (is (not (version=   "1.2.0"   "1.2.1")))
  (is (not (version= #v"1.2.0"   "1.2.1")))
  (is (not (version=   "1.2.0" #v"1.2.1")))

  (is (version/= #v"1.2.0" #v"1.2.1"))
  (is (version/=   "1.2.0"   "1.2.1"))
  (is (version/= #v"1.2.0"   "1.2.1"))
  (is (version/=   "1.2.0" #v"1.2.1"))
  (is (not (version/= #v"1.2.0" #v"1.2.0")))
  (is (not (version/=   "1.2.0"   "1.2.0")))
  (is (not (version/= #v"1.2.0"   "1.2.0")))
  (is (not (version/=   "1.2.0" #v"1.2.0")))

  (is (version> #v"1.2.1" #v"1.2.0"))
  (is (version>   "1.2.1"   "1.2.0"))
  (is (version> #v"1.2.1"   "1.2.0"))
  (is (version>   "1.2.1" #v"1.2.0"))
  (is (not (version> #v"1.2.0" #v"1.2.0")))
  (is (not (version>   "1.2.0"   "1.2.0")))
  (is (not (version> #v"1.2.0"   "1.2.0")))
  (is (not (version>   "1.2.0" #v"1.2.0")))
  (is (not (version> #v"1.2.0" #v"1.2.1")))
  (is (not (version>   "1.2.0"   "1.2.1")))
  (is (not (version> #v"1.2.0"   "1.2.1")))
  (is (not (version>   "1.2.0" #v"1.2.1")))
  (is (version> #v"3.0.0" #v"1.2.0"))
  (is (version>   "3.0.0"   "1.2.0"))
  (is (version> #v"3.0.0"   "1.2.0"))
  (is (version>   "3.0.0" #v"1.2.0"))

  (is (version>= #v"1.2.1" #v"1.2.0"))
  (is (version>=   "1.2.1"   "1.2.0"))
  (is (version>= #v"1.2.1"   "1.2.0"))
  (is (version>=   "1.2.1" #v"1.2.0"))
  (is (version>= #v"1.2.0" #v"1.2.0"))
  (is (version>=   "1.2.0"   "1.2.0"))
  (is (version>= #v"1.2.0"   "1.2.0"))
  (is (version>=   "1.2.0" #v"1.2.0"))
  (is (not (version>= #v"1.2.0" #v"1.2.1")))
  (is (not (version>=   "1.2.0"   "1.2.1")))
  (is (not (version>= #v"1.2.0"   "1.2.1")))
  (is (not (version>=   "1.2.0" #v"1.2.1")))

  (is (version< #v"1.2.0" #v"1.2.1"))
  (is (version<   "1.2.0"   "1.2.1"))
  (is (version< #v"1.2.0"   "1.2.1"))
  (is (version<   "1.2.0" #v"1.2.1"))
  (is (not (version< #v"1.2.0" #v"1.2.0")))
  (is (not (version<   "1.2.0"   "1.2.0")))
  (is (not (version< #v"1.2.0"   "1.2.0")))
  (is (not (version<   "1.2.0" #v"1.2.0")))
  (is (not (version< #v"1.2.1" #v"1.2.0")))
  (is (not (version<   "1.2.1"   "1.2.0")))
  (is (not (version< #v"1.2.1"   "1.2.0")))
  (is (not (version<   "1.2.1" #v"1.2.0")))
  (is (not (version< #v"3.0.0" #v"1.3.0")))
  (is (not (version<   "3.0.0"   "1.3.0")))
  (is (not (version< #v"3.0.0"   "1.3.0")))
  (is (not (version<   "3.0.0" #v"1.3.0")))

  (is (version<= #v"1.2.0" #v"1.2.1"))
  (is (version<=   "1.2.0"   "1.2.1"))
  (is (version<= #v"1.2.0"   "1.2.1"))
  (is (version<=   "1.2.0" #v"1.2.1"))
  (is (version<= #v"1.2.0" #v"1.2.0"))
  (is (version<=   "1.2.0"   "1.2.0"))
  (is (version<= #v"1.2.0"   "1.2.0"))
  (is (version<=   "1.2.0" #v"1.2.0"))
  (is (not (version<= #v"1.2.1" #v"1.2.0")))
  (is (not (version<=   "1.2.1"   "1.2.0")))
  (is (not (version<= #v"1.2.1"   "1.2.0")))
  (is (not (version<=   "1.2.1" #v"1.2.0"))))

(deftest rfc-example-test ()
  (let ((increasing-versions (mapcar #'read-version-from-string
                                     (list "1.0.0-alpha"
                                           "1.0.0-alpha.1"
                                           "1.0.0-beta.2"
                                           "1.0.0-beta.11"
                                           "1.0.0-rc.1"
                                           "1.0.0"))))
    (loop for v1 in increasing-versions
          for v2 in (cdr increasing-versions)
          do (progn
               (is (version< v1 v2))))

    (let ((decreasing-versions (reverse increasing-versions)))
      (loop for v1 in decreasing-versions
            for v2 in (cdr decreasing-versions)
            do (progn
                 (is (version> v1 v2)))))))

(deftest min-max-version-test ()
  (is (version< :min-version #v"0.0.0"))
  (is (version< :min-version   "0.0.0"))
  (is (version<= :min-version #v"0.0.0"))
  (is (version<= :min-version   "0.0.0"))
  (is (not (version> :min-version #v"0.0.0")))
  (is (not (version> :min-version   "0.0.0")))
  (is (not (version>= :min-version #v"0.0.0")))
  (is (not (version>= :min-version   "0.0.0")))

  (is (version> :max-version #v"99.99.99"))
  (is (version> :max-version   "99.99.99"))
  (is (version>= :max-version #v"99.99.99"))
  (is (version>= :max-version   "99.99.99"))
  (is (not (version< :max-version #v"99.99.99")))
  (is (not (version< :max-version   "99.99.99")))
  (is (not (version<= :max-version #v"99.99.99")))
  (is (not (version<= :max-version   "99.99.99")))

  ;; (is (not (version< :min-version :min-version)))
  ;; (is (version<= :min-version :min-version))
  ;; (is (not (version> :min-version :min-version)))
  ;; (is (version>= :min-version :min-version))
  ;; (is (version= :min-version :min-version))

  ;; (is (not (version< :max-version :max-version)))
  ;; (is (version<= :max-version :max-version))
  ;; (is (not (version> :max-version :max-version)))
  ;; (is (version>= :max-version :max-version))
  ;; (is (version= :max-version :max-version))
  )

(deftest build-metadata-version-precedence-test ()
  "Build metadata SHOULD be ignored when determining version precedence. Thus two versions that differ only in the build metadata, have the same precedence. Examples: 1.0.0-alpha+001, 1.0.0-alpha+20130313144700, 1.0.0-alpha+exp.sha.5114f85."
  (let ((versions (mapcar #'read-version-from-string
                          (list "1.0.0-alpha+001"
                                "1.0.0-alpha+20130313144700"
                                "1.0.0-alpha+exp.sha.5114f85"))))
    (loop for v1 in versions
          for v2 in (cdr versions)
          do (progn
               (is (not (version< v1 v2)))
               (is (not (version> v1 v2)))))))

#.(disable-version-syntax)
