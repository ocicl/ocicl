(defpackage #:org.shirakumo.precise-time.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:pt #:org.shirakumo.precise-time)))

(in-package #:org.shirakumo.precise-time.test)

(define-test precise-time
  (true (= (get-universal-time) (pt:get-precise-time)))
  (true (<= 1 (let ((a (pt:get-precise-time))) (sleep 1) (- (pt:get-precise-time) a))))
  (true (< (nth-value 1 (pt:get-precise-time)) pt:PRECISE-TIME-UNITS-PER-SECOND))
  (true (< (nth-value 1 (pt:get-precise-time)) (nth-value 1 (pt:get-precise-time)))))

(define-test monotonic-time
  (true (< 0 (pt:get-monotonic-time)))
  (true (<= 1 (let ((a (pt:get-monotonic-time))) (sleep 1) (- (pt:get-monotonic-time) a))))
  (true (< (nth-value 1 (pt:get-monotonic-time)) pt:MONOTONIC-TIME-UNITS-PER-SECOND)))
