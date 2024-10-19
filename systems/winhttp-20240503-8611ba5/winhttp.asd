;;;; Copyright (c) Frank James 2017 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(asdf:defsystem :winhttp
  :name "winhttp"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "FFI wrapper to WINHTTP"
  :license "MIT"
  :serial t
  :components
  ((:file "package")
   (:file "ffi")
   (:file "util"))
  :depends-on (:cffi))


