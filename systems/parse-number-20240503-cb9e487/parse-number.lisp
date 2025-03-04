;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; Defines functions to parse any number type, without using the reader
;;;;
;;;; Author: Matthew Danish -- mrd.debian.org
;;;;
;;;; Copyright 2002 Matthew Danish.
;;;; All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;; 1. Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;; 2. Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;; 3. Neither the name of the author nor the names of its contributors
;;;;    may be used to endorse or promote products derived from this software
;;;;    without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;;; SUCH DAMAGE.

(defpackage #:org.mapcar.parse-number
  (:use #:common-lisp)
  (:nicknames #:parse-number)
  (:export #:parse-number
           #:parse-real-number
           #:parse-positive-real-number
           #:invalid-number
           #:invalid-number-value
           #:invalid-number-reason))

(in-package #:org.mapcar.parse-number)

(define-condition invalid-number (parse-error)
  ((value :reader invalid-number-value
          :initarg :value
          :initform nil)
   (reason :reader invalid-number-reason
           :initarg :reason
           :initform "Not specified"))
  (:report (lambda (c s)
             (format s "Invalid number: ~S [Reason: ~A]"
                     (invalid-number-value c)
                     (invalid-number-reason c)))))

(declaim (type cons *white-space-characters*))
(defparameter *white-space-characters*
  '(#\Space #\Tab #\Return #\Linefeed)
  "A list of all of the whitespace characters.")

(declaim (inline white-space-p))
(defun white-space-p (x)
  "Is the given character a whitespace character?"
  (declare (optimize (speed 3) (safety 0))
           (type character x))
  (and (find x *white-space-characters*) t))

(declaim (inline parse-integer-and-places))
(defun parse-integer-and-places (string start end &key (radix 10))
  "Parse an integer and return a 'parsed-integer'. This is an object
   whose numerical value can be accessed with the function
   number-value and whose length can be accessed with the function
   place."
  (declare (optimize (speed 3) (safety 1))
           (type string string)
           (type fixnum start end radix))
  (multiple-value-bind (integer end-pos)
      (if (= start end)
          (values 0 0)
          (parse-integer string
                         :start start
                         :end end
                         :radix radix))
    ;; cl:parse-integer will consume trailing whitespace, thus end-pos may be
    ;; larger than the number of digits. Instead of trimming whitespace
    ;; beforehand we count it here
    (let ((relevant-digits (- end-pos start
                              (loop :for pos :from (- end-pos 1) :downto start
                                 :while (member (char string pos)
                                                *white-space-characters*)
                                 :count 1))))
      (cons integer relevant-digits))))

(defun parse-integers (string start end splitting-points &key (radix 10))
  "Parse a string containing multiple integers where SPLITTING-POINTS
   is a list of locations where each location is inbetween
   consecutive integers. This will return a list of parsed-integers.
   The last parsed-integer will have a negative value for its length."
  (declare (optimize (speed 3) (safety 1))
           (type string string)
           (type fixnum start end radix))
  (values-list (loop for left = start then (1+ right)
                     for point in splitting-points
                     for right = point
                     collect (parse-integer-and-places string
                                                       left
                                                       right
                                                       :radix radix)
                     into integers
                     finally (return
                               (nconc integers
                                      (list
                                       (parse-integer-and-places string
                                                                 left
                                                                 end
                                                                 :radix radix
                                                                 )))))))

(declaim (inline number-value places))
(defun number-value (x) "Get the value of a parsed-integer." (car x))
(defun places (x) "Get the length of a parsed-integer." (cdr x))

;; Numbers which could've been parsed, but intentionally crippled not to:
;; #xFF.AA
;; #o12e3

;; Numbers which CL doesn't parse, but this does:
;; #10r3.2
;; #2r  11

(defun trim-whitespace (string start end)
  (let ((end (or end (length string))))
    (when (= start end)
      (error 'invalid-number :value "" :reason "Empty string"))
    (let ((good-start
           (position-if-not #'white-space-p string
                            :start start
                            :end end))
          (good-end
           (position-if-not #'white-space-p string
                            :start start
                            :end end
                            :from-end t)))
      (unless (and good-start good-end)
        (error 'invalid-number
               :value (subseq string start end)
               :reason "Only whitespace present"))
      (values good-start (1+ good-end)))))

(defun parse-number (string &key (start 0) (end nil) (radix 10)
                                 ((:float-format *read-default-float-format*)
                                  *read-default-float-format*))
  "Given a string, and start, end, and radix parameters, produce a number according to the syntax definitions in the Common Lisp Hyperspec."
  (multiple-value-bind (start end)
      (trim-whitespace string start end)
    (if (and (eql (char string start) #\#)
             (member (char string (1+ start)) '(#\C #\c)))
        (%parse-complex-number string start end radix)
        (%parse-real-number string start end radix))))

(defun %parse-complex-number (string start end radix)
  (flet ((invalid-number (reason)
             (error 'invalid-number
                    :value (subseq string start end)
                    :reason reason)))
    (let ((\(-pos (position #\( string :start start :end end))
          (\)-pos (position #\) string :start start :end end)))
      (when (or (not \(-pos)
                (not \)-pos)
                (position #\( string :start (1+ \(-pos) :end end)
                (position #\) string :start (1+ \)-pos) :end end))
        (invalid-number "Mismatched/missing parenthesis"))
      (let ((real-pos (position-if-not #'white-space-p string
                                       :start (1+ \(-pos) :end \)-pos)))
        (unless real-pos
          (invalid-number "Missing real part"))
        (let ((delimiting-space (position-if #'white-space-p string
                                             :start (1+ real-pos)
                                             :end \)-pos)))
          (unless delimiting-space
            (invalid-number "Missing imaginary part"))
          (let ((img-pos (position-if-not #'white-space-p string
                                          :start (1+ delimiting-space)
                                          :end \)-pos)))
            (unless img-pos
              (invalid-number "Missing imaginary part"))
            (let ((img-end-pos (position-if #'white-space-p string
                                            :start (1+ img-pos)
                                            :end \)-pos)))
              (complex (%parse-real-number string real-pos delimiting-space radix)
                       (%parse-real-number string
                                           img-pos (or img-end-pos \)-pos)
                                           radix)))))))))

(defun parse-real-number (string &key (start 0) (end nil) (radix 10)
                                      ((:float-format *read-default-float-format*)
                                       *read-default-float-format*))
  "Given a string, and start, end, and radix parameters, produce a number according to the syntax definitions in the Common Lisp Hyperspec -- except for complex numbers."
  (multiple-value-bind (start end)
      (trim-whitespace string start end)
    (%parse-real-number string start end radix)))

(defun %parse-real-number (string start end radix)
  (case (char string start)
    ((#\-)
     (* -1 (%parse-positive-real-number string (1+ start) end radix)))
    ((#\+)
     (%parse-positive-real-number string (1+ start) end radix))
    ((#\#)
     (case (char string (1+ start))
       ((#\x #\X)
        (%parse-real-number string (+ start 2) end 16))
       ((#\b #\B)
        (%parse-real-number string (+ start 2) end 2))
       ((#\o #\O)
        (%parse-real-number string (+ start 2) end 8))
       (t (if (digit-char-p (char string (1+ start)))
              (let ((r-pos (position #\r string
                                     :start (1+ start)
                                     :end end
                                     :key #'char-downcase)))
                (unless r-pos
                  (error 'invalid-number
                         :value (subseq string start end)
                         :reason "Missing R in #radixR"))
                (let ((radix (parse-integer string
                                            :start (1+ start)
                                            :end r-pos)))
                  (%parse-real-number string (1+ r-pos) end radix)))))))
    (t (%parse-positive-real-number string start end radix))))

(defun type-for-exponent-marker (char)
  "Return the base for an exponent-marker."
  (case char
    ((#\d #\D)
     'double-float)
    ((#\e #\E)
     *read-default-float-format*)
    ((#\f #\F)
     'single-float)
    ((#\s #\S)
     'short-float)
    ((#\l #\L)
     'long-float)))

(defun make-float/frac (exp-marker whole-place frac-place exp-place)
  "Create a float using EXP-MARKER as the exponent-marker and the
   parsed-integers WHOLE-PLACE, FRAC-PLACE, and EXP-PLACE as the
   integer part, fractional part, and exponent respectively."
  (let* ((exp (expt 10 (number-value exp-place)))
         (integer-value
           (+ (* exp (number-value whole-place))
              (* exp (/ (number-value frac-place)
                        (expt 10 (places frac-place))))))
         (type (type-for-exponent-marker exp-marker)))
    (coerce integer-value type)))

(defun make-float/whole (exp-marker whole-place exp-place)
  "Create a float where EXP-MARKER is the exponent-marker and the
   parsed-integers WHOLE-PLACE and EXP-PLACE as the integer part and
   the exponent respectively."
  (let ((integer-value (* (number-value whole-place)
                          (expt 10 (number-value exp-place))))
        (type (type-for-exponent-marker exp-marker)))
    (coerce integer-value type)))

(defun parse-positive-real-number (string &key (start 0) (end nil) (radix 10)
                                               ((:float-format *read-default-float-format*)
                                                *read-default-float-format*))
  "Given a string, and start, end, and radix parameters, produce a number according to the syntax definitions in the Common Lisp Hyperspec -- except for complex numbers and negative numbers."
  (multiple-value-bind (start end)
      (trim-whitespace string start end)
    (%parse-positive-real-number string start end radix)))

(defun %parse-positive-real-number (string start end radix)
  (let ((first-char (char string start)))
    (flet ((invalid-number (reason)
             (error 'invalid-number
                    :value (subseq string start end)
                    :reason reason)))
      (when (position-if #'white-space-p string
                         :start (or (position-if-not #'white-space-p string
                                                     :start start
                                                     :end end)
                                    0)
                         :end   (position-if-not #'white-space-p string
                                                 :start start
                                                 :end end
                                                 :from-end t))
        (invalid-number "Whitespace inside the number"))
      (case first-char
        ((#\-)
         (invalid-number "Invalid usage of -"))
        ((#\/)
         (invalid-number "/ at beginning of number"))
        ((#\d #\D #\e #\E #\l #\L #\f #\F #\s #\S)
         (when (= radix 10)
           (invalid-number "Exponent-marker at beginning of number"))))
      (let (/-pos .-pos exp-pos exp-marker)
        (loop for index from start below end
              for char = (char string index)
              do (case char
                   ((#\/)
                    (if /-pos
                        (invalid-number "Multiple /'s in number")
                        (setf /-pos index)))
                   ((#\.)
                    (if .-pos
                        (invalid-number "Multiple .'s in number")
                        (setf .-pos index)))
                   ((#\e #\E #\f #\F #\s #\S #\l #\L #\d #\D)
                    ;; We should only execute this if the base is
                    ;; not used for the given radix (ie the digit
                    ;; e is valid in base 15 and up).
                    (when (>= (+ 10
                                 (- (char-code (char-upcase char))
                                    (char-code #\A)))
                              radix)
                      (when exp-pos
                        (invalid-number
                         "Multiple exponent-markers in number"))
                      (setf exp-pos index)
                      (setf exp-marker (char-downcase char)))))
              when (eql index (1- end))
              do (case char
                   ((#\/)
                    (invalid-number "/ at end of number"))
                   ((#\d #\D #\e #\E #\s #\S #\l #\L #\f #\F)
                    (when (= radix 10)
                      (invalid-number "Exponent-marker at end of number")))))
        (cond ((and /-pos .-pos)
               (invalid-number "Both . and / cannot be present simultaneously"))
              ((and /-pos exp-pos)
               (invalid-number "Both an exponent-marker and / cannot be present simultaneously"))
              ((and .-pos exp-pos)
               (if (< exp-pos .-pos)
                   (invalid-number "Exponent-markers must occur after . in number")
                   (if (/= radix 10)
                       (invalid-number "Only decimal numbers can contain exponent-markers or decimal points")
                       (multiple-value-bind (whole-place frac-place exp-place)
                           (parse-integers string start end
                                           (list .-pos exp-pos)
                                           :radix radix)
                         (make-float/frac exp-marker whole-place frac-place exp-place)))))
              (exp-pos
               (if (/= radix 10)
                   (invalid-number "Only decimals can contain exponent-markers")
                   (multiple-value-bind (whole-place exp-place)
                       (parse-integers string start end
                                       (list exp-pos)
                                       :radix radix)
                     (make-float/whole exp-marker whole-place exp-place))))
              (/-pos
               (multiple-value-bind (numerator denominator)
                   (parse-integers string start end
                                   (list /-pos)
                                   :radix radix)
                 (if (>= (number-value denominator) 0)
                     (/ (number-value numerator)
                        (number-value denominator))
                     (invalid-number "Misplaced - sign"))))
              (.-pos
               (if (/= radix 10)
                   (invalid-number "Only decimal numbers can contain decimal points")
                   (multiple-value-bind (whole-part frac-part)
                       (parse-integers string start end
                                       (list .-pos)
                                       :radix 10)
                     (cond
                       ((minusp (places frac-part))
                        (if (and (zerop (number-value whole-part))
                                 (zerop (places whole-part)))
                            (invalid-number "Only the . is present")
                            (number-value whole-part)))
                       ((>= (number-value frac-part) 0)
                        (coerce (+ (number-value whole-part)
                                   (/ (number-value frac-part)
                                      (expt 10 (places frac-part))))
                                *read-default-float-format*))
                       (t
                        (invalid-number "Misplaced - sign"))))))
              (t
               (values (parse-integer string
                                      :start start
                                      :end end
                                      :radix radix))))))))
