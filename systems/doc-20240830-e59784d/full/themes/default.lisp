(uiop:define-package #:40ants-doc-full/themes/default
  (:use #:cl)
  (:import-from #:common-html.emitter)
  (:import-from #:spinneret)
  (:import-from #:40ants-doc-full/themes/api
                #:inject-after-content
                #:inject-before-content
                #:inject-into-page-header
                #:theme-plugins
                #:render-css)
  (:import-from #:lass)
  (:import-from #:40ants-doc-full/commondoc/html
                #:with-html)
  (:import-from #:40ants-doc-full/utils
                #:make-relative-path)
  (:import-from #:40ants-doc-full/rewrite)
  (:import-from #:40ants-doc-full/commondoc/changelog)
  (:import-from #:alexandria
                #:read-file-into-string)
  (:import-from #:40ants-doc-full/plugins/highlightjs
                #:highlightjs)
  (:export #:default-theme))
(in-package #:40ants-doc-full/themes/default)


(defun %inject (theme func uri)
  (loop for plugin in (theme-plugins theme)
        for html = (funcall func plugin uri)
        when html
        do (fresh-line spinneret:*html*)
           (write-string html spinneret:*html*)))


(defclass default-theme ()
  ((plugins :initarg :plugins
            :initform nil
            :reader theme-plugins))
  (:default-initargs
   :plugins (list (highlightjs))))


(defmethod render-css ((theme default-theme))
  (concatenate
   'string

   ;; Default CSS
   (lass:compile-and-write
    `(body
      :font-family "sans-serif"
      :margin "auto"
      :background-color "#FFFEFB"
      :color "#000000"

      ((:or h1 h2 h3 h4 h5 h6)
       :font-family "serif"
       :font-weight "bold"
       :text-shadow "0.05em 0.05em 0.02em #DDDDDD"
       :line-height 1.2
       (a.header-link
        :color "#DDD"
        :padding 0 4px
        :text-decoration none
        :border-bottom none
        :text-shadow none
        :visibility hidden))

      ((:and
        (:or h1 h2 h3 h4 h5 h6)
        :hover)
       (a.header-link
        :visibility visible)
       ((:and a.header-link :hover)
        :color "#777")
       ((:and a.header-link ::after)
        :content "¶"))

      (h1
       :font-size 250%)
      (h2
       :font-size 200%
       :border-bottom "0.05em solid #CCCCCC")
      (h3
       :font-size 150%)
      (h4
       :font-size 130%)
      (h5
       :font-size 110%
       :margin-top 0.75em
       :margin-bottom 0.5em)
      (h6
       :font-size 100%
       :background-color inherit
       :color "#777777"
       (a
        :color "#777777"))

      (hr
       :height 0.2em
       :border 0
       :color "#CCCCCC"
       :background-color "#CCCCCC")

      ((:or p blockquote table pre)
       :line-height 1.5
       :margin "0.5em 0 0 0")

      ((:or ul ol dl)
       :padding-left 1.1em
       :list-style-position outside)

      (li
       :line-height 1.5
       :margin "0 0 0 0")

      (pre
       :background-color "#F5F3ED"
       :color "#000000"
       :border "0.1em solid #CCCCCC"
       :line-height 1.25em
       :overflow auto
       :margin "0.5em 0 0 0"
       :padding 0.2em)

      (code
       :color "#222222")
      
      (a
       :color "#222222"
       :text-decoration none
       :border-bottom 1px solid)

      (.reference-bullet
       :display flex                     
       :flex-direction column            
       :margin-bottom 1rem
       :margin-top 1rem
       (.reference-bullet-header
        :display flex
        :flex-direction row

        (a :border-bottom none
           :text-decoration none)

        (a.locative-type
         :text-decoration underline
         :white-space nowrap)
        ((:and a.locative-type :before)
         :content "[")
        ((:and a.locative-type :after)
         :content "]")
        
        (a.bullet-link
         :color "#DDD"
         :padding 0 4px
         :text-shadow none
         :visibility hidden)
        ((:and a.bullet-link ::after)
         :content "¶")
        
        (.locative-type
         (a
          :text-decoration none
          :border-bottom 0
          :font-weight bold))

        (.reference-object
         :display flex
         :flex-direction row
         :padding-left 0.3em
         :padding-right 0.3em
         :margin-left 0.2em
         (a
          :border-bottom none)
         (.object-name
          :font-weight bold
          :white-space nowrap)
         (.object-args
          :display flex
          :flex-direction row
          :white-space nowrap
          (.locative-args
           :font-style italic
           :margin-left 0.2em
           :color "#777"
           (code
            :font-family sans-serif))))

        ;; Add () around object name and it's arguments
        ((:and .reference-object :before)
         :content "(")
        ((:and .reference-object :after)
         :content ")"))

       (.bullet-content
        :border-left solid "#e9e9e9" 0.5rem
        :padding-left 1rem
        :margin-top 0.5rem
        ;; When bullet has some content, we want to make it more
        ;; distanced from the next block, to make visual spacing.
        :margin-bottom 1.5rem))
      
      ((:and
        .reference-bullet-header
        :hover)
       (a.bullet-link
        :visibility visible)
       
       ((:and
         a.bullet-link
         :hover)
        :color "#777"))

      (navigation
       :display block
       :visibility hidden
       :margin-bottom -1.5em
       (a
        :color "#CCCCCC"
        :text-shadow none
        :border-bottomi none))

      ((:and .outer-navigation :hover)
       (.navigation
        :visibility visible))

      (.highlighted
       :background "#adff2f")

      ;; Content

      (.page
       :margin 0
       :padding 0)
      
      ((.page > .content)
       :margin-left 40ex
       :padding-left 2.5em
       :max-width 85ex)
      
      (.sidebar
       :top 0px
       :left 0px
       :height 100%
       :width 40ex
       :max-width 33%
       :position fixed
       :overflow-y auto
       :overflow-x hidden
       :background "#333"
       :box-shadow inset -5px 0 5px 0px "#000"
       :color "#aaa"
       (p
        :padding 5px
        :margin 0
        :margin-left 10px
        :font-size 14px)
       (ul 
        :margin-left 10px
        :padding 0
        :list-style none)
       (li
        :line-height 1.0
        :padding 0
        :margin 0)
       (a
        :border-bottom none)
       (hr
        :height 0.05em
        :border 0
        :background "#777")
       (.toc-active 
        :background "#336699"
        :box-shadow inset -5px 0px 10px -5px "#000"))

      ((.sidebar > .header)
       (a
        :color "#777777"))

      ((.sidebar > .footer)
       :margin-left 1.5em
       :margin-top 2em
       :margin-bottom 1em
       (a
        :font-size 80%
        :color "#777777"))

      (form.search
       :margin-left 1.5em
       :margin-top 1.5em)
      
      (.page-toc
       (a
        :color "#fff"))

      (.menu-block 
       :padding-left 10px
       :margin-bottom 1em
       (a 
        :color "#fff"
        :border-bottom none))
      (.menu-block-title 
       :font-size 90%)

      (|#search-results|
       (.search
        (li
         :margin-bottom 1em)))

      (.rss-icon
       :background ,(read-file-into-string
                     (asdf:system-relative-pathname :40ants-doc
                                                    "static/rss-icon.base64"))
       :width 1em
       :height 1em
       :background-size 1em 1em !important
       :margin-left 0.3em
       :user-select none
       :display inline-block
       :text-decoration none
       :border none)

      (blockquote
       :border-left 0.5em solid lightgray
       :padding-left 1em
       :margin-bottom 1em)
      
      (.unresolved-reference
       :color magenta)))


   ;; Mobile 
   (lass:compile-and-write
    '(:media "(max-width: 800px)"
      (body
       ;; Hide things
       (.sidebar
        :display none)

       ;; Fix sizes
       ((.page > .content) :margin 0 1ex 0 1ex
        :padding 1ex))))))


(defmethod 40ants-doc-full/themes/api:render-page ((theme default-theme) uri title
                                                   &key toc content)
  (with-html
    (:html
     (:head
      (40ants-doc-full/themes/api:render-html-head theme uri title))
     (:body
      (:div :class "page"
            (40ants-doc-full/themes/api:render-page-header theme uri title)
            (40ants-doc-full/themes/api:render-sidebar theme uri toc)
            (40ants-doc-full/themes/api:render-content theme uri toc content)
            (40ants-doc-full/themes/api:render-page-footer theme uri))))))


(defmethod 40ants-doc-full/themes/api:render-page-header ((theme default-theme) uri title)
  (declare (ignore uri title)))


(defmethod 40ants-doc-full/themes/api:render-page-footer ((theme default-theme) uri)
  (declare (ignore uri)))


(defmethod 40ants-doc-full/themes/api:render-html-head ((theme default-theme) uri title)
  (let ((theme-uri (make-relative-path uri "theme.css"))
        (jquery-uri (make-relative-path uri "jquery.js"))
        (toc-js-uri (make-relative-path uri "toc.js"))
        (rss-url (40ants-doc-full/commondoc/changelog::get-changelog-rss-url)))
    (with-html
      (:meta :name "viewport"
             :content "width=device-width, initial-scale=1")
      (:title title)
      (when rss-url
        (:link :rel "alternate"
               :href rss-url
               :type "application/rss+xml"))
      (:link :rel "stylesheet"
             :type "text/css"
             :href theme-uri)
      (:script :type "text/javascript"
               :src jquery-uri)
      (:script :type "text/javascript"
               :src toc-js-uri)

      ;; This hack is required, because :HAS CSS selector is not supported by
      ;; browsers yet: https://caniuse.com/css-has
      (:script :type "text/javascript"
               (:raw "$(document).ready(function() {$('a:has(img)').css('border-bottom', 'none')})"))

      (%inject theme #'inject-into-page-header uri))))


(defmethod 40ants-doc-full/themes/api:render-content ((theme default-theme) uri toc content-func)
  (with-html
    (:div :class "content"
          ;; This role is required for Sphinx Doc's
          ;; Javascript code. It searches texts inside
          ;; the role[main] block
          :role "main"
          (when content-func
            (%inject theme #'inject-before-content uri)

            (funcall content-func)
            
            (%inject theme #'inject-after-content uri)))))


(defmethod 40ants-doc-full/themes/api:render-sidebar ((theme default-theme) uri toc)
  (with-html
    (:div :class "sidebar"
          (40ants-doc-full/themes/api:render-sidebar-header theme uri toc)
          (40ants-doc-full/themes/api:render-sidebar-content theme uri toc)
          (40ants-doc-full/themes/api:render-sidebar-footer theme uri toc))))


(defmethod 40ants-doc-full/themes/api:render-search-form ((theme default-theme) uri toc)
  (with-html
    (:form :method "GET"
           :action (40ants-doc-full/rewrite::rewrite-url
                    (make-relative-path uri "search/index.html"))
           :class "search"
           (:input :type "text"
                   :name "q")
           (:input :type "submit"
                   :value "Search")
           (:span :id "search-progress"))))


(defmethod 40ants-doc-full/themes/api:render-toc ((theme default-theme) uri toc)
  (with-html
    (:div :class "page-toc"
          (common-html.emitter::emit toc))))


(defmethod 40ants-doc-full/themes/api:render-sidebar-header ((theme default-theme) uri toc)
  (with-html
    (:div :class "header"
          (40ants-doc-full/themes/api:render-search-form theme uri toc))))


(defmethod 40ants-doc-full/themes/api:render-sidebar-content ((theme default-theme) uri toc)
  (with-html
    (:div :class "content"
          (40ants-doc-full/themes/api:render-toc theme uri toc))))


(defmethod 40ants-doc-full/themes/api:render-sidebar-footer ((theme default-theme) uri toc)
  (declare (ignore uri toc))
  (with-html
    (:div :class "footer"
          (:a :href "https://40ants.com/doc"
              "[generated by 40ANTS-DOC]"))))
