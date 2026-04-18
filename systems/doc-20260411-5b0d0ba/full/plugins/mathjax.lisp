(uiop:define-package #:40ants-doc-full/plugins/mathjax
  (:use #:cl)
  (:import-from #:40ants-doc-full/themes/api
                #:inject-after-content)
  (:import-from #:spinneret
                #:with-html-string)
  (:export #:mathjax))
(in-package #:40ants-doc-full/plugins/mathjax)


(defclass mathjax ()
  ()
  (:documentation "Injects a necessary scripts to use MathJax for rendering math formulas in your documentation."))


(defun mathjax ()
  "Creates a MathJax plugin."
  (make-instance 'mathjax))


(defmethod inject-after-content ((plugin mathjax) uri)
  (with-html-string
    ;; MathJax configuration to display inline formulas
    (:script :type "text/javascript"
             ;; Here we need this :RAW
             ;; because of the bug in the Spinneret
             ;; https://github.com/ruricolist/spinneret/issues/59
             (:raw "
             MathJax = { 
              options: {
                ignoreHtmlClass: 'page',
                processHtmlClass: 'content'
              },
              tex: {
                 inlineMath: [['$','$']]
              },
              svg: {
                 fontCache: 'global'
              }
             };
        "))
    (:script :type "text/javascript"
             :src "https://polyfill.io/v3/polyfill.min.js?features=es6")
    (:script :type "text/javascript"
             :async t
             :src "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js")))
