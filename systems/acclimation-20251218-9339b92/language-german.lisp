(cl:in-package #:acclimation)

(defclass german (language)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on LONG-DAY-NAME.

(defmethod long-day-name ((day (eql 1)) (language german))
  "Montag")

(defmethod long-day-name ((day (eql 2)) (language german))
  "Dienstag")

(defmethod long-day-name ((day (eql 3)) (language german))
  "Mittwoch")

(defmethod long-day-name ((day (eql 4)) (language german))
  "Donnerstag")

(defmethod long-day-name ((day (eql 5)) (language german))
  "Freitag")

(defmethod long-day-name ((day (eql 6)) (language german))
  "Samstag")

(defmethod long-day-name ((day (eql 7)) (language german))
  "Sonntag")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on SHORT-DAY-NAME.

(defmethod short-day-name ((day (eql 1)) (language german))
  "Mon")

(defmethod short-day-name ((day (eql 2)) (language german))
  "Die")

(defmethod short-day-name ((day (eql 3)) (language german))
  "Mit")

(defmethod short-day-name ((day (eql 4)) (language german))
  "Don")

(defmethod short-day-name ((day (eql 5)) (language german))
  "Fre")

(defmethod short-day-name ((day (eql 6)) (language german))
  "Sam")

(defmethod short-day-name ((day (eql 7)) (language german))
  "Son")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on LONG-MONTH-NAME.

(defmethod long-month-name ((month (eql 1)) (langauge german))
  "Januar")

(defmethod long-month-name ((month (eql 2)) (langauge german))
  "Februar")

(defmethod long-month-name ((month (eql 3)) (langauge german))
  "März")

(defmethod long-month-name ((month (eql 4)) (langauge german))
  "April")

(defmethod long-month-name ((month (eql 5)) (langauge german))
  "Mai")

(defmethod long-month-name ((month (eql 6)) (langauge german))
  "Juni")

(defmethod long-month-name ((month (eql 7)) (langauge german))
  "Juli")

(defmethod long-month-name ((month (eql 8)) (langauge german))
  "August")

(defmethod long-month-name ((month (eql 9)) (langauge german))
  "September")

(defmethod long-month-name ((month (eql 10)) (langauge german))
  "Oktober")

(defmethod long-month-name ((month (eql 11)) (langauge german))
  "November")

(defmethod long-month-name ((month (eql 12)) (langauge german))
  "Dezember")
                            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on SHORT-MONTH-NAME.

(defmethod short-month-name ((month (eql 1)) (langauge german))
  "Jan")

(defmethod short-month-name ((month (eql 2)) (langauge german))
  "Feb")

(defmethod short-month-name ((month (eql 3)) (langauge german))
  "Mär")

(defmethod short-month-name ((month (eql 4)) (langauge german))
  "Apr")

(defmethod short-month-name ((month (eql 5)) (langauge german))
  "Mai")

(defmethod short-month-name ((month (eql 6)) (langauge german))
  "Jun")

(defmethod short-month-name ((month (eql 7)) (langauge german))
  "Jul")

(defmethod short-month-name ((month (eql 8)) (langauge german))
  "Aug")

(defmethod short-month-name ((month (eql 9)) (langauge german))
  "Sep")

(defmethod short-month-name ((month (eql 10)) (langauge german))
  "Okt")

(defmethod short-month-name ((month (eql 11)) (langauge german))
  "Nov")

(defmethod short-month-name ((month (eql 12)) (langauge german))
  "Dez")
