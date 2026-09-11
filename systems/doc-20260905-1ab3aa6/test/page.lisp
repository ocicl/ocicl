(uiop:define-package #:40ants-doc-test/page
  (:use #:cl)
  (:import-from #:40ants-doc-full/commondoc/format
                #:with-format)
  (:import-from #:40ants-doc-full/commondoc/page
                #:make-page
                #:make-page-uri)
  (:import-from #:rove
                #:deftest
                #:testing))
(in-package 40ants-doc-test/page)


(deftest test-make-page-uri
  (with-format (:html)
    (let ((index-page (make-page nil "index" :title "index"))
          (changelog-page (make-page nil "changelog" :title "ChangeLog"))
          (base-url "https://40ants.com/doc/"))
      
      (testing "Referencing the same page should not change base-url"
        (let ((result (make-page-uri index-page
                                     :from-page index-page
                                     :base-url base-url)))
          (rove:ok (equal result base-url))))
      
      (testing "Referencing to the index from the page of one depth nest should lead to the base-url"
        (let ((result (make-page-uri index-page
                                     :from-page changelog-page
                                     :base-url base-url)))
          (rove:ok (equal result base-url))))

      (testing "If base url is not known, then result should be in relative form"
        (let ((result (make-page-uri index-page
                                     :from-page changelog-page)))
          (rove:ok (equal result "../")))))))
