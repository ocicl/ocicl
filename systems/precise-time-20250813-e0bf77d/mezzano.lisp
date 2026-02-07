(in-package #:org.shirakumo.precise-time)

(define-constant PRECISE-TIME-UNITS-PER-SECOND
  INTERNAL-TIME-UNITS-PER-SECOND)

(define-constant MONOTONIC-TIME-UNITS-PER-SECOND
  INTERNAL-TIME-UNITS-PER-SECOND)

(define-implementation get-precise-time ()
  (let ((time (mezzano.supervisor:high-precision-time-units-to-internal-time-units
               (mezzano.supervisor:get-high-precision-timer))))
    (values (get-universal-time)
            (mod time INTERNAL-TIME-UNITS-PER-SECOND))))

(define-implementation get-monotonic-time ()
  (let ((time (get-internal-real-time)))
    (values (truncate time INTERNAL-TIME-UNITS-PER-SECOND)
            (mod time INTERNAL-TIME-UNITS-PER-SECOND))))
