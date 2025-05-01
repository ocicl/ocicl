;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Creating and manipulating timer structures.
;;;
;;; Copyright (C) 2003 Zach Beane <xach@xach.com>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge,publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :iolib/multiplex)

;;;
;;; Timer
;;;

(defstruct (timer
             (:conc-name %timer-)
             (:constructor %make-timer (name function expire-time
                                        relative-time one-shot)))
  name
  ;; to call when the timer expires
  function
  ;; absolute expiry time
  expire-time
  ;; relative expiry time
  relative-time
  ;; when NIL, the timer is automatically rescheduled
  ;; when triggered
  one-shot)

(defmethod print-object ((object timer) stream)
  (print-unreadable-object (object stream)
    (format stream "TIMER ~S, Timeout: [ ~A , ~A ], ~:[persistent~;one-shot~]"
            (%timer-name object)
            (%timer-relative-time object)
            (%timer-expire-time object)
            (%timer-one-shot object))))

(defun make-timer (function delay &key name one-shot)
  (flet ((abs-timeout (timeout)
           (+ (isys:get-monotonic-time)
              (normalize-timeout timeout))))
    (let ((name (or name "(unnamed)")))
      (%make-timer name function (abs-timeout delay) delay one-shot))))

(defun timer-name (timer)
  (%timer-name timer))

(defun timer-expired-p (timer now &optional (delta 0.0d0))
  (assert (%timer-expire-time timer) ((%timer-expire-time timer))
          "Timer ~A must have an expiry time set." timer)
  (let ((compare-time (+ now delta)))
    (> compare-time (%timer-expire-time timer))))

(defun reset-timer (timer)
  (setf (%timer-expire-time timer) 0))
