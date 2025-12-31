(cl:in-package #:acclimation-temperature)

(defun kelvin-to-celsius (kelvin)
  (- kelvin 273.15d0))

(defun celsius-to-kelvin (celsius)
  (+ celsius 273.15d0))

(defun kelvin-to-fahrenheit (kelvin)
  (let ((celsius (kelvin-to-celsius kelvin)))
    (+ (* 9/5 celsius) 32)))

(defun fahrenheit-to-kelvin (fahrenheit)
  (let ((celsius (* 5/9 (- fahrenheit 32))))
    (celsius-to-kelvin celsius)))
