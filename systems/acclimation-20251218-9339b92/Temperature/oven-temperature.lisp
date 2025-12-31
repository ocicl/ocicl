(cl:in-package #:acclimation-temperature)

(defclass oven-temperature ()
  ((%decimal-count
    :initform 0 :initarg :decimal-count :accessor decimal-count)))

(defclass oven-temperature-celsius (oven-temperature)
  ())

(defclass oven-temperature-fahrenheit (oven-temperature)
  ())

(defgeneric format-oven-temperature (absolute-temperature formater stream))

(defmethod format-oven-temperature
    (absolute-temperature
     (formater oven-temperature-celsius)
     stream-designator)
  (let ((celsius-temperature (kelvin-to-celsius absolute-temperature)))
    (format stream-designator
	    "~d°C"
	    (* 5 (round celsius-temperature 5)))))

(defmethod format-oven-temperature
    (absolute-temperature
     (formater oven-temperature-fahrenheit)
     stream-designator)
  (let ((fahrenheit-temperature (kelvin-to-fahrenheit absolute-temperature)))
    (format stream-designator
	    "~d°F"
	    (* 10 (round fahrenheit-temperature 10)))))
