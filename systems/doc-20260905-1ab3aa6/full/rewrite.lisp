(uiop:define-package #:40ants-doc-full/rewrite
  (:use #:cl)
  (:import-from #:cl-ppcre
                #:regex-replace)
  (:import-from #:str
                #:ends-with-p))
(in-package #:40ants-doc-full/rewrite)


(defvar *clean-urls* t)


(defun rewrite-url (url)
  "This function allows to have \"clean\" URLs on side. It is much
   nicer to see https://40ants.com/doc/some-lib/changelog/ than a
   https://40ants.com/doc/some-lib/changelog.html
"
  (if *clean-urls*
      (regex-replace "index\\.html$" url "")
      url))


(defun rewrite-file (file)
  "Takes a string with filename and transforms to make clean HTML urls.

   Here are a few input -> output examples:

   - index.html -> index.html
   - some/index.html -> some/index.html
   - some.html -> some/index.html
   - foo/bar.html -> foo/bar/index.html
"
  (cond
    ((and *clean-urls*
          (ends-with-p ".html" file)
          (not (ends-with-p "index.html" file)))
     (regex-replace "\\.html"
                    file
                    "/index.html"))
    (t file)))
