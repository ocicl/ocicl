(uiop:define-package #:40ants-doc-test/autodoc
  (:use #:cl)
  (:import-from #:40ants-doc/autodoc
                #:defautodoc)
  (:import-from #:40ants-doc-full/locatives/slots)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing))
(in-package #:40ants-doc-test/autodoc)


(defclass slot-reader-writer-class ()
  ((accessor-slot :accessor slot-accessor)
   (reader-slot :reader slot-reader)
   (writer-slot :writer slot-writer)))


(deftest test-autodoc-classifies-slot-readers-writers-and-accessors
  (testing "A class with all slot interface kinds is classified correctly"
    (ok (equal (40ants-doc/autodoc::class-readers
                'slot-reader-writer-class)
               '(slot-accessor
                 slot-reader)))
    (ok (equal (40ants-doc/autodoc::class-writers
                'slot-reader-writer-class)
               '(slot-accessor
                 slot-writer)))
    (ok (equal (40ants-doc/autodoc::class-accessors
                'slot-reader-writer-class)
               '(slot-accessor)))
    (let ((readers-and-writers
            (40ants-doc/autodoc::class-readers-writers
             'slot-reader-writer-class)))
      (ok (= 3 (length readers-and-writers)))
      (ok (subsetp '(slot-accessor
                     slot-reader
                     slot-writer)
                   readers-and-writers)))))


(deftest test-writer-locative-finds-a-writer-symbol
  (testing "A writer locative resolves a slot declared with :writer"
    (ok (40ants-doc-full/locatives/slots::find-writer-slot-definition
         'slot-writer
         'slot-reader-writer-class))))


(deftest test-reader-locative-finds-a-reader-symbol
  (testing "A reader locative resolves a slot declared with :reader"
    (ok (40ants-doc-full/locatives/slots::find-reader-slot-definition
         'slot-reader
         'slot-reader-writer-class))))


(deftest test-accessor-locative-finds-an-accessor-symbol
  (testing "An accessor locative resolves a slot declared with :accessor"
    (ok (40ants-doc-full/locatives/slots::find-accessor-slot-definition
         'slot-accessor
         'slot-reader-writer-class))))
