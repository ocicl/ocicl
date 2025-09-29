(cl:in-package #:acclimation)

;;; DAY is the ISO8601 day number where the days are numbered from 1
;;; starting on Monday. 
(defgeneric long-day-name (day language))

(defgeneric short-day-name (day language))

;;; MONTH is the month number starting with 1 for January
(defgeneric long-month-name (month language))

(defgeneric short-month-name (month language))
