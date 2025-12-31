(cl:in-package #:acclimation)

(defclass vietnamese (language)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on LONG-DAY-NAME.

(defmethod long-day-name ((day (eql 1)) (language vietnamese))
  "ngày thứ hai")

(defmethod long-day-name ((day (eql 2)) (language vietnamese))
  "ngày thứ ba")

(defmethod long-day-name ((day (eql 3)) (language vietnamese))
  "ngày thứ tư")

(defmethod long-day-name ((day (eql 4)) (language vietnamese))
  "ngày thứ năm")

(defmethod long-day-name ((day (eql 5)) (language vietnamese))
  "ngày thứ sáu ")

(defmethod long-day-name ((day (eql 6)) (language vietnamese))
  "ngày thứ bảy")

(defmethod long-day-name ((day (eql 7)) (language vietnamese))
  "chủ nhật")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on SHORT-DAY-NAME.

(defmethod short-day-name ((day (eql 1)) (language vietnamese))
  "thứ hai")

(defmethod short-day-name ((day (eql 2)) (language vietnamese))
  "thứ ba")

(defmethod short-day-name ((day (eql 3)) (language vietnamese))
  "thứ tư")

(defmethod short-day-name ((day (eql 4)) (language vietnamese))
  "thứ năm")

(defmethod short-day-name ((day (eql 5)) (language vietnamese))
  "thứ sáu ")

(defmethod short-day-name ((day (eql 6)) (language vietnamese))
  "thứ bảy")

(defmethod short-day-name ((day (eql 7)) (language vietnamese))
  "chủ nhật")
