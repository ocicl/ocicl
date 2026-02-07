(defpackage #:org.shirakumo.precise-time
  (:use #:cl)
  (:export
   #:query-failed
   #:precise-time-units-per-second
   #:monotonic-time-units-per-second
   #:get-precise-time
   #:get-monotonic-time
   #:get-precise-time/double
   #:get-monotonic-time/double))
