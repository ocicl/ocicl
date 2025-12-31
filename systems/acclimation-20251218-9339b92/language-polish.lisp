(cl:in-package #:acclimation)

(defclass polish (language)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on LONG-DAY-NAME.

(defmethod long-day-name ((day (eql 1)) (language polish))
  "poniedziałek")

(defmethod long-day-name ((day (eql 2)) (language polish))
  "wtorek")

(defmethod long-day-name ((day (eql 3)) (language polish))
  "środa")

(defmethod long-day-name ((day (eql 4)) (language polish))
  "czwartek")

(defmethod long-day-name ((day (eql 5)) (language polish))
  "piątek")

(defmethod long-day-name ((day (eql 6)) (language polish))
  "sobota")

(defmethod long-day-name ((day (eql 7)) (language polish))
  "niedziela")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on SHORT-DAY-NAME.

(defmethod short-day-name ((day (eql 1)) (language polish))
  "pon")

(defmethod short-day-name ((day (eql 2)) (language polish))
  "wt")

(defmethod short-day-name ((day (eql 3)) (language polish))
  "śro")

(defmethod short-day-name ((day (eql 4)) (language polish))
  "czw")

(defmethod short-day-name ((day (eql 5)) (language polish))
  "pt")

(defmethod short-day-name ((day (eql 6)) (language polish))
  "sob")

(defmethod short-day-name ((day (eql 7)) (language polish))
  "nd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on LONG-MONTH-NAME.

(defmethod long-month-name ((month (eql 1)) (langauge polish))
  "styczeń")

(defmethod long-month-name ((month (eql 2)) (langauge polish))
  "luty")

(defmethod long-month-name ((month (eql 3)) (langauge polish))
  "marzec")

(defmethod long-month-name ((month (eql 4)) (langauge polish))
  "kwiecień")

(defmethod long-month-name ((month (eql 5)) (langauge polish))
  "maj")

(defmethod long-month-name ((month (eql 6)) (langauge polish))
  "czerwiec")

(defmethod long-month-name ((month (eql 7)) (langauge polish))
  "lipiec")

(defmethod long-month-name ((month (eql 8)) (langauge polish))
  "sierpień")

(defmethod long-month-name ((month (eql 9)) (langauge polish))
  "wrzesień")

(defmethod long-month-name ((month (eql 10)) (langauge polish))
  "październik")

(defmethod long-month-name ((month (eql 11)) (langauge polish))
  "listopad")

(defmethod long-month-name ((month (eql 12)) (langauge polish))
  "grudzień")
                            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on SHORT-MONTH-NAME.

(defmethod short-month-name ((month (eql 1)) (langauge polish))
  "sty")

(defmethod short-month-name ((month (eql 2)) (langauge polish))
  "lut")

(defmethod short-month-name ((month (eql 3)) (langauge polish))
  "mar")

(defmethod short-month-name ((month (eql 4)) (langauge polish))
  "kwi")

(defmethod short-month-name ((month (eql 5)) (langauge polish))
  "maj")

(defmethod short-month-name ((month (eql 6)) (langauge polish))
  "cze")

(defmethod short-month-name ((month (eql 7)) (langauge polish))
  "lip")

(defmethod short-month-name ((month (eql 8)) (langauge polish))
  "sie")

(defmethod short-month-name ((month (eql 9)) (langauge polish))
  "wrz")

(defmethod short-month-name ((month (eql 10)) (langauge polish))
  "paź")

(defmethod short-month-name ((month (eql 11)) (langauge polish))
  "lis")

(defmethod short-month-name ((month (eql 12)) (langauge polish))
  "gru")
