;;;; This file was originally written by Peter Keller (psilord@cs.wisc.edu)
;;;; and this code is released under the same license as IOLib.

(defpackage :iolib.examples
  (:nicknames :iolib/examples)
  (:use :cl :iolib :bordeaux-threads)
  #+sb-package-locks
  (:lock t)
  (:export :run-ex1-client
           :run-ex2-client
           :run-ex3-client
           :run-ex4-client
           :run-ex5a-client
           :run-ex5b-client

           :run-ex1-server
           :run-ex2-server
           :run-ex3-server
           :run-ex4-server
           :run-ex5-server
           :run-ex6-server
           :run-ex7-server
           :run-ex8-server
           ))

(in-package :iolib.examples)

;;;; This file also contains some simply utilities to help the writing of the
;;;; examples.

;; The example host:port to which clients connect. Servers often bind to
;; any interface, but listen on this port.
(defparameter *host* "localhost")
(defparameter *port* 9999)

;; A simple, but efficient, queue implementation, used by some examples.
(defun make-queue ()
  (cons nil nil))

(defun enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q)
  (pop (car q)))

(defun empty-queue (q)
  (null (car q)))

