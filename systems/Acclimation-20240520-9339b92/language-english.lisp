(cl:in-package #:acclimation)

(defclass english (language)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on LONG-DAY-NAME.

(defmethod long-day-name ((day (eql 1)) (language english))
  "Monday")

(defmethod long-day-name ((day (eql 2)) (language english))
  "Tuesday")

(defmethod long-day-name ((day (eql 3)) (language english))
  "Wednesday")

(defmethod long-day-name ((day (eql 4)) (language english))
  "Thursday")

(defmethod long-day-name ((day (eql 5)) (language english))
  "Friday")

(defmethod long-day-name ((day (eql 6)) (language english))
  "Saturday")

(defmethod long-day-name ((day (eql 7)) (language english))
  "Sunday")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on SHORT-DAY-NAME.

(defmethod short-day-name ((day (eql 1)) (language english))
  "Mon")

(defmethod short-day-name ((day (eql 2)) (language english))
  "Tue")

(defmethod short-day-name ((day (eql 3)) (language english))
  "Wed")

(defmethod short-day-name ((day (eql 4)) (language english))
  "Thu")

(defmethod short-day-name ((day (eql 5)) (language english))
  "Fri")

(defmethod short-day-name ((day (eql 6)) (language english))
  "Sat")

(defmethod short-day-name ((day (eql 7)) (language english))
  "Sun")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on LONG-MONTH-NAME.

(defmethod long-month-name ((month (eql 1)) (langauge english))
  "January")

(defmethod long-month-name ((month (eql 2)) (langauge english))
  "February")

(defmethod long-month-name ((month (eql 3)) (langauge english))
  "March")

(defmethod long-month-name ((month (eql 4)) (langauge english))
  "April")

(defmethod long-month-name ((month (eql 5)) (langauge english))
  "May")

(defmethod long-month-name ((month (eql 6)) (langauge english))
  "June")

(defmethod long-month-name ((month (eql 7)) (langauge english))
  "July")

(defmethod long-month-name ((month (eql 8)) (langauge english))
  "August")

(defmethod long-month-name ((month (eql 9)) (langauge english))
  "September")

(defmethod long-month-name ((month (eql 10)) (langauge english))
  "October")

(defmethod long-month-name ((month (eql 11)) (langauge english))
  "November")

(defmethod long-month-name ((month (eql 12)) (langauge english))
  "December")
                            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on SHORT-MONTH-NAME.

(defmethod short-month-name ((month (eql 1)) (langauge english))
  "Jan")

(defmethod short-month-name ((month (eql 2)) (langauge english))
  "Feb")

(defmethod short-month-name ((month (eql 3)) (langauge english))
  "Mar")

(defmethod short-month-name ((month (eql 4)) (langauge english))
  "Apr")

(defmethod short-month-name ((month (eql 5)) (langauge english))
  "May")

(defmethod short-month-name ((month (eql 6)) (langauge english))
  "Jun")

(defmethod short-month-name ((month (eql 7)) (langauge english))
  "Jul")

(defmethod short-month-name ((month (eql 8)) (langauge english))
  "Aug")

(defmethod short-month-name ((month (eql 9)) (langauge english))
  "Sep")

(defmethod short-month-name ((month (eql 10)) (langauge english))
  "Oct")

(defmethod short-month-name ((month (eql 11)) (langauge english))
  "Nov")

(defmethod short-month-name ((month (eql 12)) (langauge english))
  "Dec")
