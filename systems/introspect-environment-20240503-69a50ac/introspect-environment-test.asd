(asdf:defsystem #:introspect-environment-test
  :description "Tests for i-e. Use (asdf:test-system '#:introspect-environment)."
  :author "Bike <aeshtaer@gmail.com>"
  :license "WTFPL"
  :depends-on (#:introspect-environment #:fiveam)
  :version "0.1"
  :components ((:file "test"))
  ;; cargo-culted from bordeaux-threads-test
  :in-order-to ((asdf:test-op
		 (asdf:load-op introspect-environment-test)))
  :perform (asdf:test-op :after (op c)
	     (funcall (find-symbol "RUN!" :fiveam)
		      :introspect-environment)))
