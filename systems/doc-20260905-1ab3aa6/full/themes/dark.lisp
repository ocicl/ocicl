(uiop:define-package #:40ants-doc-full/themes/dark
  (:use #:cl)
  (:import-from #:40ants-doc-full/themes/default
                #:default-theme)
  (:import-from #:lass)
  (:import-from #:40ants-doc-full/themes/api)
  (:export #:dark-theme))
(in-package #:40ants-doc-full/themes/dark)


(defclass dark-theme (default-theme)
  ())

(defmethod 40ants-doc-full/themes/api:render-css ((theme dark-theme))
  (let ((background  "#333")
        (font-color "#FFFEFB"))
    (concatenate
     'string
     (call-next-method)
     
     (lass:compile-and-write
      `(body
        :color ,font-color
        :background ,background

        (.reference-object
         :background "#7b2800")

        ((:or a code)
         :color ,font-color)

        ((:or h1 h2 h3 h4 h5 h6)
         :color ,font-color
         :border-bottom none
         :text-shadow "0.05em 0.05em 0.02em #000")
        
        (.sidebar
         :background ,background
         :box-shadow inset -3px 0 3px 0px "#777"
         (.page-toc
          (a
           :color ,font-color))
         (.toc-active 
          :background ,font-color)
         ((.toc-active > a)
          :color ,background)))))))
