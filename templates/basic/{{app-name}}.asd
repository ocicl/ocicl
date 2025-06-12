;;;; <%= @ app-name %>.asd

(asdf:defsystem #:<%= @ app-name %>
  :description "A basic application."
  :author      "<%= (or (@ author) "Your Name") %>"
  :license     "<%= (or (@ license) "MIT") %>"
  :version     "0.1.0"
  :depends-on  ()
  :serial t
  :components ((:file "src/package")
               (:file "src/main")))
