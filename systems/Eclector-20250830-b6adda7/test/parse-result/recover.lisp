(cl:in-package #:eclector.parse-result.test)

(def-suite* :eclector.parse-result.recover
  :in :eclector.parse-result)

(defun read-with-list-result-client (stream)
  (let ((client (make-instance 'list-result-client)))
    (eclector.parse-result:read client stream)))

(test recover/labeled-objects
  "Test recovering from syntax errors related to labeled objects."
  (mapc (alexandria:rcurry #'eclector.test:do-recover-test-case
                           #'read-with-list-result-client)
        '(("#1=#1#"
           (eclector.reader:sharpsign-equals-only-refers-to-self)
           (:result   nil
            :children ()
            :source   (0 . 6)))
          ("(#1="
           (eclector.reader:end-of-input-after-sharpsign-equals
            eclector.reader:unterminated-list)
           (:result   (nil)
            :children ((:result nil :children () :source (1 . 4)))
            :source   (0 . 4)))
          ("#1=
;;2
;;3"
           (eclector.reader:end-of-input-after-sharpsign-equals)
           (:result   nil
            :children ((:reason   (:line-comment . 2)
                        :children ()
                        :source   (4 . 8))
                       (:reason   (:line-comment . 2)
                        :children ()
                        :source   (8 . 11)))
            :source   (0 . 11))))))
