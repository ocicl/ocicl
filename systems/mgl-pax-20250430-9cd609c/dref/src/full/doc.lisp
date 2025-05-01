(in-package :dref)

(defun dref-sections ()
  (list @dref-manual))

(defun dref-pages ()
  (let ((uri-fn (make-git-source-uri-fn
                 :dref "https://github.com/melisgl/mgl-pax")))
    `((:objects (, @dref-manual)
       :source-uri-fn ,uri-fn))))

(register-doc-in-pax-world :dref 'dref-sections 'dref-pages)

#+nil
(progn
  (asdf:load-system :dref/full)
  (time
   (update-asdf-system-readmes (dref-sections) :dref)))
