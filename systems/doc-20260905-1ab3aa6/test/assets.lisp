(uiop:define-package #:40ants-doc-test/assets
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:40ants-doc-full/assets
                #:defimage)
  (:import-from #:40ants-doc-full/builder
                #:render-to-files)
  (:import-from #:40ants-doc-full/builder/printer
                #:*document-uppercase-is-code*)
  (:import-from #:rove
                #:deftest
                #:ok
                #:signals
                #:testing)
  (:import-from #:alexandria
                #:read-file-into-string))
(in-package #:40ants-doc-test/assets)


(defimage @test-asset.png
  "static/rendering.png"
  :target-filename "assets/test-rendering.png")

(defimage @scaled-asset.png
  "static/rendering.png"
  :target-filename "assets/scaled-rendering.png"
  :width 240)

(defimage @fixed-size-asset.png
  "static/rendering.png"
  :target-filename "assets/fixed-size-rendering.png"
  :width 320
  :height 180)

(defimage @height-asset.png
  "static/rendering.png"
  :target-filename "assets/height-rendering.png"
  :height 180)

(defimage @missing-asset.png
  "test/data/does-not-exist.png"
  :target-filename "assets/missing.png")

(defimage @unsafe-asset.png
  #.(asdf:system-relative-pathname :40ants-doc "static/rendering.png")
  :target-filename "../outside.png")

(defimage @colliding-asset-a.png
  #.(asdf:system-relative-pathname :40ants-doc "static/rendering.png")
  :target-filename "assets/collision.png")

(defimage @colliding-asset-b.png
  #.(asdf:system-relative-pathname :40ants-doc "static/rendering.png")
  :target-filename "assets/collision.png")


(defsection @asset-page (:export nil)
  "@TEST-ASSET.PNG")


(defsection @repeated-asset-page (:export nil)
  "@TEST-ASSET.PNG and @TEST-ASSET.PNG")


(defsection @explicit-asset-page (:export nil)
  "Here is:"
  @test-asset.png)


(defsection @scaled-asset-page (:export nil)
  "@SCALED-ASSET.PNG")


(defsection @fixed-size-asset-page (:export nil)
  "@FIXED-SIZE-ASSET.PNG")


(defsection @height-asset-page (:export nil)
  "@HEIGHT-ASSET.PNG")


(defsection @missing-asset-page (:export nil)
  "@MISSING-ASSET.PNG")


(defsection @unsafe-asset-page (:export nil)
  "@UNSAFE-ASSET.PNG")


(defsection @colliding-asset-page (:export nil)
  "@COLLIDING-ASSET-A.PNG")


(defun make-test-output-directory ()
  (uiop:ensure-directory-pathname
   (merge-pathnames (format nil "40ants-doc-assets-~A/" (gensym))
                    (uiop:temporary-directory))))


(defun render-asset-page (format base-filename)
  (render-to-files
   (40ants-doc-full/page:make-page @asset-page :base-filename base-filename)
   :base-dir (make-test-output-directory)
   :format format))


(defun count-occurrences (needle text)
  (loop with start = 0
        for position = (search needle text :start2 start)
        while position
        count 1
        do (setf start (+ position (length needle)))))


(deftest test-asset-renders-to-html
  (multiple-value-bind (output-dir output-path)
      (render-asset-page :html "index")
    (testing "The source image is copied before HTML pages are emitted"
      (ok (probe-file (merge-pathnames "assets/test-rendering.png" output-dir))))
    (testing "The asset symbol is emitted as an image"
      (ok (search "<img src=\"assets/test-rendering.png\""
                  (read-file-into-string output-path))))))


(deftest test-asset-renders-to-markdown
  (multiple-value-bind (output-dir output-path)
      (render-asset-page :markdown "nested/page")
    (testing "The source image is copied for Markdown output"
      (ok (probe-file (merge-pathnames "assets/test-rendering.png" output-dir))))
    (testing "The asset has a page-relative Markdown image source"
      (ok (search "![@TEST-ASSET.PNG](../assets/test-rendering.png)"
                  (read-file-into-string output-path))))))


(deftest test-image-width-is-emitted-without-height-in-html
  (multiple-value-bind (output-dir output-path)
      (render-to-files @scaled-asset-page
                       :base-dir (make-test-output-directory)
                       :format :html)
    (declare (ignore output-dir))
    (testing "A width-only declaration does not emit height"
      (let ((output (read-file-into-string output-path)))
        (ok (search "width=240" output))
        (ok (not (search "height=240" output)))))))


(deftest test-image-dimensions-use-html-in-markdown
  (multiple-value-bind (output-dir output-path)
      (render-to-files @fixed-size-asset-page
                       :base-dir (make-test-output-directory)
                       :format :markdown)
    (declare (ignore output-dir))
    (testing "Markdown uses HTML to retain both dimensions"
      (ok (search "<img src=\"assets/fixed-size-rendering.png\" alt=\"@FIXED-SIZE-ASSET.PNG\" width=\"320\" height=\"180\">"
                  (read-file-into-string output-path))))))


(deftest test-image-height-is-emitted-without-width-in-html
  (multiple-value-bind (output-dir output-path)
      (render-to-files @height-asset-page
                       :base-dir (make-test-output-directory)
                       :format :html)
    (declare (ignore output-dir))
    (testing "A height-only declaration does not emit width"
      (let ((output (read-file-into-string output-path)))
        (ok (not (search "width=180" output)))
        (ok (search "height=180" output))))))


(deftest test-repeated-asset-is-copied-to-one-target
  (multiple-value-bind (output-dir output-path)
      (render-to-files @repeated-asset-page
                       :base-dir (make-test-output-directory)
                       :format :markdown)
    (testing "Each occurrence is rendered as an image"
      (ok (= 2 (count-occurrences "![@TEST-ASSET.PNG](assets/test-rendering.png)"
                                  (read-file-into-string output-path)))))
    (testing "Repeated occurrences share one output target"
      (ok (probe-file (merge-pathnames "assets/test-rendering.png" output-dir))))))


(deftest test-asset-renders-when-uppercase-code-is-disabled
  (let ((*document-uppercase-is-code* nil))
    (multiple-value-bind (output-dir output-path)
        (render-asset-page :html "index")
      (testing "An asset name remains an image even without implicit code"
        (ok (probe-file (merge-pathnames "assets/test-rendering.png" output-dir)))
        (ok (search "<img src=\"assets/test-rendering.png\""
                    (read-file-into-string output-path)))))))


(deftest test-explicit-asset-entry-renders-to-markdown
  (multiple-value-bind (output-dir output-path)
      (render-to-files @explicit-asset-page
                       :base-dir (make-test-output-directory)
                       :format :markdown)
    (testing "An asset symbol can be a DEFSECTION entry"
      (ok (probe-file (merge-pathnames "assets/test-rendering.png" output-dir)))
      (ok (search "![@TEST-ASSET.PNG](assets/test-rendering.png)"
                  (read-file-into-string output-path))))))


(deftest test-missing-asset-is-rejected
  (testing "A referenced declaration without a source file fails the build"
    (signals (render-to-files @missing-asset-page
                              :base-dir (make-test-output-directory)
                              :format :html)
             'error)))


(deftest test-unsafe-asset-target-is-rejected
  (testing "An asset target cannot escape the output directory"
    (signals (render-to-files @unsafe-asset-page
                              :base-dir (make-test-output-directory)
                              :format :html)
             'error)))


(deftest test-colliding-asset-target-is-rejected
  (testing "Two asset names cannot claim one output pathname"
    (signals (render-to-files @colliding-asset-page
                              :base-dir (make-test-output-directory)
                              :format :html)
             'error)))
