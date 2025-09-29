(cl:in-package #:asdf-user)

(defsystem :acclimation-temperature
  :serial t
  :components
  ((:file "packages")
   (:file "temperature")
   (:file "ambient-temperature")
   (:file "oven-temperature")))
