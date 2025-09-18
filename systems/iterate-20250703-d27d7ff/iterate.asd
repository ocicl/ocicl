;;; -*- lisp -*- system definition

(defsystem :iterate
  :description "Jonathan Amsterdam's iterator/gatherer/accumulator facility"
  :licence "MIT"
  :version "1.6.0"
  :in-order-to ((test-op (test-op :iterate/tests)))
  :components ((:file "package")
               (:file "iterate" :depends-on ("package"))))

(defsystem :iterate/tests
  :depends-on (:iterate #+sbcl :sb-rt #-sbcl :rt)
  :components ((:file "iterate-test")))

(defmethod perform ((operation test-op) (component (eql (find-system :iterate/tests))))
  (uiop:symbol-call '#:iterate.test '#:do-iterate-tests :on-failure :error))
