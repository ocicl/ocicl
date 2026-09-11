(uiop:define-package #:40ants-doc-full/themes/light
  (:use #:cl)
  (:import-from #:40ants-doc-full/themes/default
                #:default-theme)
  (:import-from #:lass)
  (:import-from #:40ants-doc-full/themes/api)
  (:import-from #:40ants-doc-full/plugins/highlightjs
                #:highlightjs)
  (:export #:light-theme))
(in-package #:40ants-doc-full/themes/light)


(defclass light-theme (default-theme)
  ()
  (:default-initargs
   :plugins (list (highlightjs :theme "atom-one-light"))))


(defmethod 40ants-doc-full/themes/api:render-css ((theme light-theme))
  (let ((background "#FFFEFB")
        (font-color "#333"))
    (concatenate
     'string
     (call-next-method)
     
     (lass:compile-and-write
      `(body
        :color ,font-color

        ((:or h1 h2 h3 h4 h5 h6)
         :color ,font-color
         :border-bottom none)
        
        (.sidebar
         :background ,background
         :box-shadow inset -3px 0 3px 0px "#777"
         (.page-toc
          (a
           :color "#333"))
         (.toc-active 
          :background ,font-color
          )
         ((.toc-active > a)
          :color ,background)))))))

