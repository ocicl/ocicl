(asdf:defsystem #:cl-semver-test
  :license "MIT"
  :depends-on (#:cl-semver
               #:stefil)
  :components
  ((:module :t
	    :components
            ((:file "package")
	     (:file "semver"))
	    :serial t))
  :perform (asdf:test-op (op c)
			 (uiop:symbol-call :semver.test :run-tests)))
