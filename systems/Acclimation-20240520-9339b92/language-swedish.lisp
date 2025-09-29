(cl:in-package #:acclimation)

(defclass swedish (language)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on LONG-DAY-NAME.

(defmethod long-day-name ((day (eql 1)) (language swedish))
  "måndag")

(defmethod long-day-name ((day (eql 2)) (language swedish))
  "tisdag")

(defmethod long-day-name ((day (eql 3)) (language swedish))
  "onsdag")

(defmethod long-day-name ((day (eql 4)) (language swedish))
  "torsdag")

(defmethod long-day-name ((day (eql 5)) (language swedish))
  "fredag")

(defmethod long-day-name ((day (eql 6)) (language swedish))
  "lördag")

(defmethod long-day-name ((day (eql 7)) (language swedish))
  "söndag")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on SHORT-DAY-NAME.

(defmethod short-day-name ((day (eql 1)) (language swedish))
  "mån")

(defmethod short-day-name ((day (eql 2)) (language swedish))
  "tis")

(defmethod short-day-name ((day (eql 3)) (language swedish))
  "ons")

(defmethod short-day-name ((day (eql 4)) (language swedish))
  "tor")

(defmethod short-day-name ((day (eql 5)) (language swedish))
  "fre")

(defmethod short-day-name ((day (eql 6)) (language swedish))
  "lör")

(defmethod short-day-name ((day (eql 7)) (language swedish))
  "sön")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on LONG-MONTH-NAME.

(defmethod long-month-name ((month (eql 1)) (langauge swedish))
  "januari")

(defmethod long-month-name ((month (eql 2)) (langauge swedish))
  "februari")

(defmethod long-month-name ((month (eql 3)) (langauge swedish))
  "mars")

(defmethod long-month-name ((month (eql 4)) (langauge swedish))
  "april")

(defmethod long-month-name ((month (eql 5)) (langauge swedish))
  "maj")

(defmethod long-month-name ((month (eql 6)) (langauge swedish))
  "juni")

(defmethod long-month-name ((month (eql 7)) (langauge swedish))
  "juli")

(defmethod long-month-name ((month (eql 8)) (langauge swedish))
  "augusti")

(defmethod long-month-name ((month (eql 9)) (langauge swedish))
  "september")

(defmethod long-month-name ((month (eql 10)) (langauge swedish))
  "oktober")

(defmethod long-month-name ((month (eql 11)) (langauge swedish))
  "november")

(defmethod long-month-name ((month (eql 12)) (langauge swedish))
  "december")
                            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on SHORT-MONTH-NAME.

(defmethod short-month-name ((month (eql 1)) (langauge swedish))
  "jan")

(defmethod short-month-name ((month (eql 2)) (langauge swedish))
  "feb")

(defmethod short-month-name ((month (eql 3)) (langauge swedish))
  "mar")

(defmethod short-month-name ((month (eql 4)) (langauge swedish))
  "apr")

(defmethod short-month-name ((month (eql 5)) (langauge swedish))
  "maj")

(defmethod short-month-name ((month (eql 6)) (langauge swedish))
  "jun")

(defmethod short-month-name ((month (eql 7)) (langauge swedish))
  "jul")

(defmethod short-month-name ((month (eql 8)) (langauge swedish))
  "aug")

(defmethod short-month-name ((month (eql 9)) (langauge swedish))
  "sep")

(defmethod short-month-name ((month (eql 10)) (langauge swedish))
  "okt")

(defmethod short-month-name ((month (eql 11)) (langauge swedish))
  "nov")

(defmethod short-month-name ((month (eql 12)) (langauge swedish))
  "dec")
