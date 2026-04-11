(in-package #:org.shirakumo.precise-time)

(define-constant PRECISE-TIME-UNITS-PER-SECOND 10000000)
(define-constant MONOTONIC-TIME-UNITS-PER-SECOND
    (cffi:with-foreign-objects ((pf :int64))
      (if (/= 0 (cffi:foreign-funcall "QueryPerformanceFrequency" :pointer pf :int))
          (cffi:mem-ref pf :int64)
          (error "Performance counters unavailable"))))

(define-implementation get-precise-time ()
  (cffi:with-foreign-objects ((ft :uint64))
    (macrolet ((get-time ()
                 (if (cffi:foreign-symbol-pointer "GetSystemTimePreciseAsFileTime")
                     `(cffi:foreign-funcall "GetSystemTimePreciseAsFileTime" :pointer ft :void)
                     `(cffi:foreign-funcall "GetSystemTimeAsFileTime" :pointer ft :void))))
      (get-time))
    (let ((ft (cffi:mem-ref ft :uint64)))
      (values (- (truncate ft PRECISE-TIME-UNITS-PER-SECOND)
                 (- ;; Windows-Unix epoch offset
                  11644473600
                  ;; Unix-Universal time offset
                  (encode-universal-time 0 0 0 1 1 1970 0)))
              (mod ft PRECISE-TIME-UNITS-PER-SECOND)))))

(define-implementation get-monotonic-time ()
  (cffi:with-foreign-objects ((pc :int64))
    (when (= 0 (cffi:foreign-funcall "QueryPerformanceCounter" :pointer pc :int))
      (fail))
    (let ((pc (cffi:mem-ref pc :int64)))
      (values (truncate pc MONOTONIC-TIME-UNITS-PER-SECOND)
              (mod pc MONOTONIC-TIME-UNITS-PER-SECOND)))))
