;;;; -*- mode: Lisp -*-

(defsystem 40ants-doc-full
  :licence "MIT"
  :version "0.1.0"
  :author "Alexander Artemenko"
  :mailto "svetlyak.40wt@gmail.com"
  :homepage "https://40ants.com/doc"
  :bug-tracker "https://github.com/40ants/doc/issues"
  :source-control (:git "https://github.com/40ants/doc.git")
  :description "Documentation generator. You will need to load this system, to build documentation for a library which uses 40ANTS-DOC system."
  :class :package-inferred-system
  :pathname "full"
  :depends-on (;; "40ants-doc/full"
               "40ants-doc-full/builder"
               "40ants-doc-full/doc"

               ;; TODO: Все эти locatives надо сделать доступными в минимальном пакете
               ;; может быть не загружать сами модули, но сделать так, чтобы на них можно было ссылаться
               "40ants-doc-full/commondoc/changelog"
               "40ants-doc-full/commondoc/section"
               "40ants-doc-full/github"
               
               "40ants-doc-full/locatives/argument"
               "40ants-doc-full/locatives/asdf-system"
               "40ants-doc-full/locatives/class"
               "40ants-doc-full/locatives/compiler-macro"
               "40ants-doc-full/locatives/constant"
               "40ants-doc-full/locatives/dislocated"
               "40ants-doc-full/locatives/function"
               "40ants-doc-full/locatives/generic-function"
               "40ants-doc-full/locatives/glossary"
               "40ants-doc-full/locatives/include"
               "40ants-doc-full/locatives/locative"
               "40ants-doc-full/locatives/macro"
               "40ants-doc-full/locatives/method"
               "40ants-doc-full/locatives/package"
               "40ants-doc-full/locatives/restart"
               "40ants-doc-full/locatives/section"
               "40ants-doc-full/locatives/slots"
               "40ants-doc-full/locatives/stdout-of"
               "40ants-doc-full/locatives/structure-accessor"
               "40ants-doc-full/locatives/symbol-macro"
               "40ants-doc-full/locatives/type"
               "40ants-doc-full/locatives/variable"
               
               "40ants-doc-full/object-package-impl"
               "40ants-doc-full/themes/dark"
               "40ants-doc-full/themes/dark""40ants-doc-full/builder"
               "40ants-doc-full/themes/default"
               "40ants-doc-full/themes/light")
  :in-order-to ((asdf:test-op (asdf:test-op "40ants-doc-test"))))


(asdf:register-system-packages "log4cl" '("LOG"))
(asdf:register-system-packages "3bmd-ext-code-blocks" '("3BMD-CODE-BLOCKS"))
(asdf:register-system-packages "common-html" '("COMMON-HTML.EMITTER"))
(asdf:register-system-packages "common-doc" '("COMMON-DOC.OPS"
                                              "COMMON-DOC.FORMAT"))
(asdf:register-system-packages "3bmd-ext-code-blocks" '("3BMD-CODE-BLOCKS"))
(asdf:register-system-packages "swank" '("SWANK-BACKEND"
                                         "SWANK/BACKEND"
                                         "SWANK-MOP"))
