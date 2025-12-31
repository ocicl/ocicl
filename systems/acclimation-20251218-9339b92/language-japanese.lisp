(cl:in-package #:acclimation)

(defclass japanese (language)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on LONG-DAY-NAME.

(defmethod long-day-name ((day (eql 1)) (language japanese))
  "月曜日")

(defmethod long-day-name ((day (eql 2)) (language japanese))
  "火曜日")

(defmethod long-day-name ((day (eql 3)) (language japanese))
  "水曜日")

(defmethod long-day-name ((day (eql 4)) (language japanese))
  "木曜日")

(defmethod long-day-name ((day (eql 5)) (language japanese))
  "金曜日")

(defmethod long-day-name ((day (eql 6)) (language japanese))
  "土曜日")

(defmethod long-day-name ((day (eql 7)) (language japanese))
  "日曜日")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on SHORT-DAY-NAME.

(defmethod short-day-name ((day (eql 1)) (language japanese))
  "月")

(defmethod short-day-name ((day (eql 2)) (language japanese))
  "火")

(defmethod short-day-name ((day (eql 3)) (language japanese))
  "水")

(defmethod short-day-name ((day (eql 4)) (language japanese))
  "木")

(defmethod short-day-name ((day (eql 5)) (language japanese))
  "金")

(defmethod short-day-name ((day (eql 6)) (language japanese))
  "土")

(defmethod short-day-name ((day (eql 7)) (language japanese))
  "日")
