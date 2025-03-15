;;;; package.lisp

(defpackage :diff
  (:use :cl)
  (:export #:*diff-context-lines*
           #:generate-diff
           #:generate-seq-diff
           #:unified-diff #:context-diff

           #:apply-seq-window
           #:apply-seq-diff
           #:apply-seq-patch
           #:apply-patch

           #:render-diff
           #:render-diff-window
           #:format-diff
           #:format-diff-string

           #:diff
           #:original-pathname
           #:modified-pathname
           #:diff-window-class
           #:diff-windows

           #:diff-window
           #:original-start-line
           #:original-length
           #:modified-start-line
           #:modified-length
           #:window-chunks

           #:chunk-kind
           #:chunk-lines

           #:compute-raw-diff
           #:compute-raw-seq-diff
           #:common-diff-region
           #:modified-diff-region
           #:original-start
           #:original-length
           #:modified-start
           #:modified-length))
