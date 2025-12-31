(cl:in-package #:acclimation-temperature)

(defclass ambient-temperature ()
  ((%decimal-count
    :initform 0 :initarg :decimal-count :accessor decimal-count)))

(defclass ambient-temperature-celsius (ambient-temperature)
  ())

(defclass ambient-temperature-fahrenheit (ambient-temperature)
  ())

(defgeneric format-ambient-temperature (absolute-temperature formater stream))

(defmethod format-ambient-temperature
    (absolute-temperature
     (formater ambient-temperature-celsius)
     stream-designator)
  (let ((celsius-temperature (kelvin-to-celsius absolute-temperature)))
    (if (zerop (decimal-count formater))
        (format stream-designator
                "~d°C"
                (round celsius-temperature))
        (format stream-designator
                "~,vf°C"
                (decimal-count formater)
                celsius-temperature))))

(defmethod format-ambient-temperature
    (absolute-temperature
     (formater ambient-temperature-fahrenheit)
     stream-designator)
  (let ((fahrenheit-temperature (kelvin-to-fahrenheit absolute-temperature)))
    (if (zerop (decimal-count formater))
        (format stream-designator
                "~d°F"
                (round fahrenheit-temperature))
        (format stream-designator
                "~,vf°F"
                (decimal-count formater)
                fahrenheit-temperature))))
