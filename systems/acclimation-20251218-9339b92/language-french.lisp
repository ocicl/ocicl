(cl:in-package #:acclimation)

(defclass french (language)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on LONG-DAY-NAME.

(defmethod long-day-name ((day (eql 1)) (language french))
  "lundi")

(defmethod long-day-name ((day (eql 2)) (language french))
  "mardi")

(defmethod long-day-name ((day (eql 3)) (language french))
  "mercredi")

(defmethod long-day-name ((day (eql 4)) (language french))
  "jeudi")

(defmethod long-day-name ((day (eql 5)) (language french))
  "vendredi")

(defmethod long-day-name ((day (eql 6)) (language french))
  "samedi")

(defmethod long-day-name ((day (eql 7)) (language french))
  "dimanche")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on SHORT-DAY-NAME.

(defmethod short-day-name ((day (eql 1)) (language french))
  "lun")

(defmethod short-day-name ((day (eql 2)) (language french))
  "mar")

(defmethod short-day-name ((day (eql 3)) (language french))
  "mer")

(defmethod short-day-name ((day (eql 4)) (language french))
  "jeu")

(defmethod short-day-name ((day (eql 5)) (language french))
  "ven")

(defmethod short-day-name ((day (eql 6)) (language french))
  "sam")

(defmethod short-day-name ((day (eql 7)) (language french))
  "dim")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on LONG-MONTH-NAME.

(defmethod long-month-name ((month (eql 1)) (langauge french))
  "janvier")

(defmethod long-month-name ((month (eql 2)) (langauge french))
  "février")

(defmethod long-month-name ((month (eql 3)) (langauge french))
  "mars")

(defmethod long-month-name ((month (eql 4)) (langauge french))
  "avril")

(defmethod long-month-name ((month (eql 5)) (langauge french))
  "mai")

(defmethod long-month-name ((month (eql 6)) (langauge french))
  "juin")

(defmethod long-month-name ((month (eql 7)) (langauge french))
  "juillet")

(defmethod long-month-name ((month (eql 8)) (langauge french))
  "août")

(defmethod long-month-name ((month (eql 9)) (langauge french))
  "septembre")

(defmethod long-month-name ((month (eql 10)) (langauge french))
  "octobre")

(defmethod long-month-name ((month (eql 11)) (langauge french))
  "novembre")

(defmethod long-month-name ((month (eql 12)) (langauge french))
  "décembre")
                            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on SHORT-MONTH-NAME.

(defmethod short-month-name ((month (eql 1)) (langauge french))
  "jan")

(defmethod short-month-name ((month (eql 2)) (langauge french))
  "fév")

(defmethod short-month-name ((month (eql 3)) (langauge french))
  "mar")

(defmethod short-month-name ((month (eql 4)) (langauge french))
  "avr")

(defmethod short-month-name ((month (eql 5)) (langauge french))
  "mai")

(defmethod short-month-name ((month (eql 6)) (langauge french))
  "jun")

(defmethod short-month-name ((month (eql 7)) (langauge french))
  "jul")

(defmethod short-month-name ((month (eql 8)) (langauge french))
  "aug")

(defmethod short-month-name ((month (eql 9)) (langauge french))
  "sep")

(defmethod short-month-name ((month (eql 10)) (langauge french))
  "oct")

(defmethod short-month-name ((month (eql 11)) (langauge french))
  "nov")

(defmethod short-month-name ((month (eql 12)) (langauge french))
  "déc")
