(in-package :dref)

(defun dref-sections ()
  (list @dref-manual))

(defun dref-pages ()
  (let ((uri-fn (make-git-source-uri-fn
                 :dref "https://github.com/melisgl/mgl-pax")))
    `((:objects (, @dref-manual)
       :source-uri-fn ,uri-fn))))

(register-doc-in-pax-world :dref 'dref-sections 'dref-pages)


;;;; To generate cross-linked READMEs with PAX, use
;;;; src/document/document-util.lisp in PAX.
