;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Controlling the queue of scheduled events and running expired timers.
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
;;; Public interface
;;;

(defun schedule-timer (schedule timer)
  (priority-queue-insert schedule timer)
  (values timer))

(defun unschedule-timer (schedule timer)
  (priority-queue-remove schedule timer)
  (values timer))

(defun reschedule-timer (schedule timer)
  (incf (%timer-expire-time timer) (%timer-relative-time timer))
  (priority-queue-insert schedule timer))

(defun reschedule-timer-relative-to-now (timer now)
  (setf (%timer-expire-time timer)
        (+ now (%timer-relative-time timer))))

;;;
;;; The scheduler
;;;

(defun peek-schedule (schedule)
  (priority-queue-maximum schedule))

(defun time-to-next-timer (schedule)
  (when-let ((timer (peek-schedule schedule)))
    (%timer-expire-time timer)))

;;;
;;; Expiring timers
;;;

(defun dispatch-timer (timer)
  (funcall (%timer-function timer)))

(defun timer-reschedulable-p (timer)
  (symbol-macrolet ((relative-time (%timer-relative-time timer))
                    (one-shot (%timer-one-shot timer)))
    (and relative-time (not one-shot))))

(defun expire-pending-timers (schedule now)
  (let ((expired-p nil)
        (timers-to-reschedule ()))
    (flet ((handle-expired-timer (timer)
             (when (timer-reschedulable-p timer)
               (push timer timers-to-reschedule))
             (dispatch-timer timer))
           (%return ()
             (dolist (timer timers-to-reschedule)
               (reschedule-timer schedule timer))
             (return* expired-p)))
      (loop
         (let ((next-timer (peek-schedule schedule)))
           (unless next-timer (%return))
           (cond ((timer-expired-p next-timer now)
                  (setf expired-p t)
                  (handle-expired-timer (priority-queue-extract-maximum schedule)))
                 (t
                  (%return))))))))
