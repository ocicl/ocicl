;;;; package.lisp

(defpackage #:trivial-file-size
  (:use #:cl)
  (:import-from #:uiop
    #:ensure-pathname
    #:native-namestring)
  (:export #:file-size-in-octets))
