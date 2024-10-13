(in-package #:tar-simple-extract-test)

(para:define-test extract-v7
  (with-temp-dir ()
    (tar:with-open-archive (a (asdf:system-relative-pathname
                               :tar "test/v7.tar"))
      (tar-simple-extract:simple-extract-archive a)
      (para:is equal
               "Hello, world!
"
               (uiop:read-file-string (merge-pathnames "a.txt")))
      (para:is equal
               "Hello, world!
"
               (uiop:read-file-string (merge-pathnames "a-symlink.txt")))
      (para:is equal
               "Hello, world!
"
               (uiop:read-file-string (merge-pathnames "a-hardlink.txt"))))))
