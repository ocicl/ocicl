(cl:in-package #:acclimation)

(defclass afrikaans (language)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on LONG-DAY-NAME.

(defmethod long-day-name ((day (eql 1)) (language afrikaans))
  "Maandag")

(defmethod long-day-name ((day (eql 2)) (language afrikaans))
  "Dinsdag")

(defmethod long-day-name ((day (eql 3)) (language afrikaans))
  "Woensdag")

(defmethod long-day-name ((day (eql 4)) (language afrikaans))
  "Donderdag")

(defmethod long-day-name ((day (eql 5)) (language afrikaans))
  "Vrydag")

(defmethod long-day-name ((day (eql 6)) (language afrikaans))
  "Saterdag")

(defmethod long-day-name ((day (eql 7)) (language afrikaans))
  "Sondag")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on SHORT-DAY-NAME.

(defmethod short-day-name ((day (eql 1)) (language afrikaans))
  "Ma")

(defmethod short-day-name ((day (eql 2)) (language afrikaans))
  "Di")

(defmethod short-day-name ((day (eql 3)) (language afrikaans))
  "Wo")

(defmethod short-day-name ((day (eql 4)) (language afrikaans))
  "Do")

(defmethod short-day-name ((day (eql 5)) (language afrikaans))
  "Vr")

(defmethod short-day-name ((day (eql 6)) (language afrikaans))
  "Sa")

(defmethod short-day-name ((day (eql 7)) (language afrikaans))
  "So")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on LONG-MONTH-NAME.

(defmethod long-month-name ((month (eql 1)) (langauge afrikaans))
  "Januarie")

(defmethod long-month-name ((month (eql 2)) (langauge afrikaans))
  "Februarie")

(defmethod long-month-name ((month (eql 3)) (langauge afrikaans))
  "Maart")

(defmethod long-month-name ((month (eql 4)) (langauge afrikaans))
  "April")

(defmethod long-month-name ((month (eql 5)) (langauge afrikaans))
  "Mei")

(defmethod long-month-name ((month (eql 6)) (langauge afrikaans))
  "Junie")

(defmethod long-month-name ((month (eql 7)) (langauge afrikaans))
  "Julie")

(defmethod long-month-name ((month (eql 8)) (langauge afrikaans))
  "Augustus")

(defmethod long-month-name ((month (eql 9)) (langauge afrikaans))
  "September")

(defmethod long-month-name ((month (eql 10)) (langauge afrikaans))
  "Oktober")

(defmethod long-month-name ((month (eql 11)) (langauge afrikaans))
  "November")

(defmethod long-month-name ((month (eql 12)) (langauge afrikaans))
  "Desember")
                            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on SHORT-MONTH-NAME.

(defmethod short-month-name ((month (eql 1)) (langauge afrikaans))
  "Jan")

(defmethod short-month-name ((month (eql 2)) (langauge afrikaans))
  "Feb")

(defmethod short-month-name ((month (eql 3)) (langauge afrikaans))
  "Maa")

(defmethod short-month-name ((month (eql 4)) (langauge afrikaans))
  "Apr")

(defmethod short-month-name ((month (eql 5)) (langauge afrikaans))
  "Mei")

(defmethod short-month-name ((month (eql 6)) (langauge afrikaans))
  "Jun")

(defmethod short-month-name ((month (eql 7)) (langauge afrikaans))
  "Jul")

(defmethod short-month-name ((month (eql 8)) (langauge afrikaans))
  "Aug")

(defmethod short-month-name ((month (eql 9)) (langauge afrikaans))
  "Sep")

(defmethod short-month-name ((month (eql 10)) (langauge afrikaans))
  "Okt")

(defmethod short-month-name ((month (eql 11)) (langauge afrikaans))
  "Nov")

(defmethod short-month-name ((month (eql 12)) (langauge afrikaans))
  "Des")
