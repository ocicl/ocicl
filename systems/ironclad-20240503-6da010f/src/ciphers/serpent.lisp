;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; serpent.lisp -- implementation of the Serpent block cipher

(in-package :crypto)


;;; S-Boxes

(defmacro serpent-sbox0 (r0 r1 r2 r3 o0 o1 o2 o3 t0)
  `(setf ,r3 (logxor ,r3 ,r0)
         ,t0 ,r1
         ,r1 (logand ,r1 ,r3)
         ,t0 (logxor ,t0 ,r2)
         ,r1 (logxor ,r1 ,r0)
         ,r0 (logior ,r0 ,r3)
         ,r0 (logxor ,r0 ,t0)
         ,t0 (logxor ,t0 ,r3)
         ,r3 (logxor ,r3 ,r2)
         ,r2 (logior ,r2 ,r1)
         ,r2 (logxor ,r2 ,t0)
         ,t0 (mod32lognot ,t0)
         ,t0 (logior ,t0 ,r1)
         ,r1 (logxor ,r1 ,r3)
         ,r1 (logxor ,r1 ,t0)
         ,r3 (logior ,r3 ,r0)
         ,r1 (logxor ,r1 ,r3)
         ,t0 (logxor ,t0 ,r3)
         ,o0 ,r1
         ,o1 ,t0
         ,o2 ,r2
         ,o3 ,r0))

(defmacro serpent-sbox0-inverse (r0 r1 r2 r3 o0 o1 o2 o3 t0)
  `(setf ,r2 (mod32lognot ,r2)
         ,t0 ,r1
         ,r1 (logior ,r1 ,r0)
         ,t0 (mod32lognot ,t0)
         ,r1 (logxor ,r1 ,r2)
         ,r2 (logior ,r2 ,t0)
         ,r1 (logxor ,r1 ,r3)
         ,r0 (logxor ,r0 ,t0)
         ,r2 (logxor ,r2 ,r0)
         ,r0 (logand ,r0 ,r3)
         ,t0 (logxor ,t0 ,r0)
         ,r0 (logior ,r0 ,r1)
         ,r0 (logxor ,r0 ,r2)
         ,r3 (logxor ,r3 ,t0)
         ,r2 (logxor ,r2 ,r1)
         ,r3 (logxor ,r3 ,r0)
         ,r3 (logxor ,r3 ,r1)
         ,r2 (logand ,r2 ,r3)
         ,t0 (logxor ,t0 ,r2)
         ,o0 ,r0
         ,o1 ,t0
         ,o2 ,r1
         ,o3 ,r3))

(defmacro serpent-sbox1 (r0 r1 r2 r3 o0 o1 o2 o3 t0)
  `(setf ,r0 (mod32lognot ,r0)
         ,r2 (mod32lognot ,r2)
         ,t0 ,r0
         ,r0 (logand ,r0 ,r1)
         ,r2 (logxor ,r0 ,r2)
         ,r0 (logior ,r0 ,r3)
         ,r3 (logxor ,r3 ,r2)
         ,r1 (logxor ,r1 ,r0)
         ,r0 (logxor ,r0 ,t0)
         ,t0 (logior ,t0 ,r1)
         ,r1 (logxor ,r1 ,r3)
         ,r2 (logior ,r2 ,r0)
         ,r2 (logand ,r2 ,t0)
         ,r0 (logxor ,r0 ,r1)
         ,r1 (logand ,r1 ,r2)
         ,r1 (logxor ,r1 ,r0)
         ,r0 (logand ,r0 ,r2)
         ,r0 (logxor ,r0 ,t0)
         ,o0 ,r2
         ,o1 ,r0
         ,o2 ,r3
         ,o3 ,r1))

(defmacro serpent-sbox1-inverse (r0 r1 r2 r3 o0 o1 o2 o3 t0)
  `(setf ,t0 ,r1
         ,r1 (logxor ,r1 ,r3)
         ,r3 (logand ,r3 ,r1)
         ,t0 (logxor ,t0 ,r2)
         ,r3 (logxor ,r3 ,r0)
         ,r0 (logior ,r0 ,r1)
         ,r2 (logxor ,r2 ,r3)
         ,r0 (logxor ,r0 ,t0)
         ,r0 (logior ,r0 ,r2)
         ,r1 (logxor ,r1 ,r3)
         ,r0 (logxor ,r0 ,r1)
         ,r1 (logior ,r1 ,r3)
         ,r1 (logxor ,r1 ,r0)
         ,t0 (mod32lognot ,t0)
         ,t0 (logxor ,t0 ,r1)
         ,r1 (logior ,r1 ,r0)
         ,r1 (logxor ,r1 ,r0)
         ,r1 (logior ,r1 ,t0)
         ,r3 (logxor ,r3 ,r1)
         ,o0 ,t0
         ,o1 ,r0
         ,o2 ,r3
         ,o3 ,r2))

(defmacro serpent-sbox2 (r0 r1 r2 r3 o0 o1 o2 o3 t0)
  `(setf ,t0 ,r0
         ,r0 (logand ,r0 ,r2)
         ,r0 (logxor ,r0 ,r3)
         ,r2 (logxor ,r2 ,r1)
         ,r2 (logxor ,r2 ,r0)
         ,r3 (logior ,r3 ,t0)
         ,r3 (logxor ,r3 ,r1)
         ,t0 (logxor ,t0 ,r2)
         ,r1 ,r3
         ,r3 (logior ,r3 ,t0)
         ,r3 (logxor ,r3 ,r0)
         ,r0 (logand ,r0 ,r1)
         ,t0 (logxor ,t0 ,r0)
         ,r1 (logxor ,r1 ,r3)
         ,r1 (logxor ,r1 ,t0)
         ,t0 (mod32lognot ,t0)
         ,o0 ,r2
         ,o1 ,r3
         ,o2 ,r1
         ,o3 ,t0))

(defmacro serpent-sbox2-inverse (r0 r1 r2 r3 o0 o1 o2 o3 t0)
  `(setf ,r2 (logxor ,r2 ,r3)
         ,r3 (logxor ,r3 ,r0)
         ,t0 ,r3
         ,r3 (logand ,r3 ,r2)
         ,r3 (logxor ,r3 ,r1)
         ,r1 (logior ,r1 ,r2)
         ,r1 (logxor ,r1 ,t0)
         ,t0 (logand ,t0 ,r3)
         ,r2 (logxor ,r2 ,r3)
         ,t0 (logand ,t0 ,r0)
         ,t0 (logxor ,t0 ,r2)
         ,r2 (logand ,r2 ,r1)
         ,r2 (logior ,r2 ,r0)
         ,r3 (mod32lognot ,r3)
         ,r2 (logxor ,r2 ,r3)
         ,r0 (logxor ,r0 ,r3)
         ,r0 (logand ,r0 ,r1)
         ,r3 (logxor ,r3 ,t0)
         ,r3 (logxor ,r3 ,r0)
         ,o0 ,r1
         ,o1 ,t0
         ,o2 ,r2
         ,o3 ,r3))

(defmacro serpent-sbox3 (r0 r1 r2 r3 o0 o1 o2 o3 t0)
  `(setf ,t0 ,r0
         ,r0 (logior ,r0 ,r3)
         ,r3 (logxor ,r3 ,r1)
         ,r1 (logand ,r1 ,t0)
         ,t0 (logxor ,t0 ,r2)
         ,r2 (logxor ,r2 ,r3)
         ,r3 (logand ,r3 ,r0)
         ,t0 (logior ,t0 ,r1)
         ,r3 (logxor ,r3 ,t0)
         ,r0 (logxor ,r0 ,r1)
         ,t0 (logand ,t0 ,r0)
         ,r1 (logxor ,r1 ,r3)
         ,t0 (logxor ,t0 ,r2)
         ,r1 (logior ,r1 ,r0)
         ,r1 (logxor ,r1 ,r2)
         ,r0 (logxor ,r0 ,r3)
         ,r2 ,r1
         ,r1 (logior ,r1 ,r3)
         ,r1 (logxor ,r1 ,r0)
         ,o0 ,r1
         ,o1 ,r2
         ,o2 ,r3
         ,o3 ,t0))

(defmacro serpent-sbox3-inverse (r0 r1 r2 r3 o0 o1 o2 o3 t0)
  `(setf ,t0 ,r2
         ,r2 (logxor ,r2 ,r1)
         ,r0 (logxor ,r0 ,r2)
         ,t0 (logand ,t0 ,r2)
         ,t0 (logxor ,t0 ,r0)
         ,r0 (logand ,r0 ,r1)
         ,r1 (logxor ,r1 ,r3)
         ,r3 (logior ,r3 ,t0)
         ,r2 (logxor ,r2 ,r3)
         ,r0 (logxor ,r0 ,r3)
         ,r1 (logxor ,r1 ,t0)
         ,r3 (logand ,r3 ,r2)
         ,r3 (logxor ,r3 ,r1)
         ,r1 (logxor ,r1 ,r0)
         ,r1 (logior ,r1 ,r2)
         ,r0 (logxor ,r0 ,r3)
         ,r1 (logxor ,r1 ,t0)
         ,r0 (logxor ,r0 ,r1)
         ,o0 ,r2
         ,o1 ,r1
         ,o2 ,r3
         ,o3 ,r0))

(defmacro serpent-sbox4 (r0 r1 r2 r3 o0 o1 o2 o3 t0)
  `(setf ,r1 (logxor ,r1 ,r3)
         ,r3 (mod32lognot ,r3)
         ,r2 (logxor ,r2 ,r3)
         ,r3 (logxor ,r3 ,r0)
         ,t0 ,r1
         ,r1 (logand ,r1 ,r3)
         ,r1 (logxor ,r1 ,r2)
         ,t0 (logxor ,t0 ,r3)
         ,r0 (logxor ,r0 ,t0)
         ,r2 (logand ,r2 ,t0)
         ,r2 (logxor ,r2 ,r0)
         ,r0 (logand ,r0 ,r1)
         ,r3 (logxor ,r3 ,r0)
         ,t0 (logior ,t0 ,r1)
         ,t0 (logxor ,t0 ,r0)
         ,r0 (logior ,r0 ,r3)
         ,r0 (logxor ,r0 ,r2)
         ,r2 (logand ,r2 ,r3)
         ,r0 (mod32lognot ,r0)
         ,t0 (logxor ,t0 ,r2)
         ,o0 ,r1
         ,o1 ,t0
         ,o2 ,r0
         ,o3 ,r3))

(defmacro serpent-sbox4-inverse (r0 r1 r2 r3 o0 o1 o2 o3 t0)
  `(setf ,t0 ,r2
         ,r2 (logand ,r2 ,r3)
         ,r2 (logxor ,r2 ,r1)
         ,r1 (logior ,r1 ,r3)
         ,r1 (logand ,r1 ,r0)
         ,t0 (logxor ,t0 ,r2)
         ,t0 (logxor ,t0 ,r1)
         ,r1 (logand ,r1 ,r2)
         ,r0 (mod32lognot ,r0)
         ,r3 (logxor ,r3 ,t0)
         ,r1 (logxor ,r1 ,r3)
         ,r3 (logand ,r3 ,r0)
         ,r3 (logxor ,r3 ,r2)
         ,r0 (logxor ,r0 ,r1)
         ,r2 (logand ,r2 ,r0)
         ,r3 (logxor ,r3 ,r0)
         ,r2 (logxor ,r2 ,t0)
         ,r2 (logior ,r2 ,r3)
         ,r3 (logxor ,r3 ,r0)
         ,r2 (logxor ,r2 ,r1)
         ,o0 ,r0
         ,o1 ,r3
         ,o2 ,r2
         ,o3 ,t0))

(defmacro serpent-sbox5 (r0 r1 r2 r3 o0 o1 o2 o3 t0)
  `(setf ,r0 (logxor ,r0 ,r1)
         ,r1 (logxor ,r1 ,r3)
         ,r3 (mod32lognot ,r3)
         ,t0 ,r1
         ,r1 (logand ,r1 ,r0)
         ,r2 (logxor ,r2 ,r3)
         ,r1 (logxor ,r1 ,r2)
         ,r2 (logior ,r2 ,t0)
         ,t0 (logxor ,t0 ,r3)
         ,r3 (logand ,r3 ,r1)
         ,r3 (logxor ,r3 ,r0)
         ,t0 (logxor ,t0 ,r1)
         ,t0 (logxor ,t0 ,r2)
         ,r2 (logxor ,r2 ,r0)
         ,r0 (logand ,r0 ,r3)
         ,r2 (mod32lognot ,r2)
         ,r0 (logxor ,r0 ,t0)
         ,t0 (logior ,t0 ,r3)
         ,r2 (logxor ,r2 ,t0)
         ,o0 ,r1
         ,o1 ,r3
         ,o2 ,r0
         ,o3 ,r2))

(defmacro serpent-sbox5-inverse (r0 r1 r2 r3 o0 o1 o2 o3 t0)
  `(setf ,r1 (mod32lognot ,r1)
         ,t0 ,r3
         ,r2 (logxor ,r2 ,r1)
         ,r3 (logior ,r3 ,r0)
         ,r3 (logxor ,r3 ,r2)
         ,r2 (logior ,r2 ,r1)
         ,r2 (logand ,r2 ,r0)
         ,t0 (logxor ,t0 ,r3)
         ,r2 (logxor ,r2 ,t0)
         ,t0 (logior ,t0 ,r0)
         ,t0 (logxor ,t0 ,r1)
         ,r1 (logand ,r1 ,r2)
         ,r1 (logxor ,r1 ,r3)
         ,t0 (logxor ,t0 ,r2)
         ,r3 (logand ,r3 ,t0)
         ,t0 (logxor ,t0 ,r1)
         ,r3 (logxor ,r3 ,t0)
         ,t0 (mod32lognot ,t0)
         ,r3 (logxor ,r3 ,r0)
         ,o0 ,r1
         ,o1 ,t0
         ,o2 ,r3
         ,o3 ,r2))

(defmacro serpent-sbox6 (r0 r1 r2 r3 o0 o1 o2 o3 t0)
  `(setf ,r2 (mod32lognot ,r2)
         ,t0 ,r3
         ,r3 (logand ,r3 ,r0)
         ,r0 (logxor ,r0 ,t0)
         ,r3 (logxor ,r3 ,r2)
         ,r2 (logior ,r2 ,t0)
         ,r1 (logxor ,r1 ,r3)
         ,r2 (logxor ,r2 ,r0)
         ,r0 (logior ,r0 ,r1)
         ,r2 (logxor ,r2 ,r1)
         ,t0 (logxor ,t0 ,r0)
         ,r0 (logior ,r0 ,r3)
         ,r0 (logxor ,r0 ,r2)
         ,t0 (logxor ,t0 ,r3)
         ,t0 (logxor ,t0 ,r0)
         ,r3 (mod32lognot ,r3)
         ,r2 (logand ,r2 ,t0)
         ,r2 (logxor ,r2 ,r3)
         ,o0 ,r0
         ,o1 ,r1
         ,o2 ,t0
         ,o3 ,r2))

(defmacro serpent-sbox6-inverse (r0 r1 r2 r3 o0 o1 o2 o3 t0)
  `(setf ,r0 (logxor ,r0 ,r2)
         ,t0 ,r2
         ,r2 (logand ,r2 ,r0)
         ,t0 (logxor ,t0 ,r3)
         ,r2 (mod32lognot ,r2)
         ,r3 (logxor ,r3 ,r1)
         ,r2 (logxor ,r2 ,r3)
         ,t0 (logior ,t0 ,r0)
         ,r0 (logxor ,r0 ,r2)
         ,r3 (logxor ,r3 ,t0)
         ,t0 (logxor ,t0 ,r1)
         ,r1 (logand ,r1 ,r3)
         ,r1 (logxor ,r1 ,r0)
         ,r0 (logxor ,r0 ,r3)
         ,r0 (logior ,r0 ,r2)
         ,r3 (logxor ,r3 ,r1)
         ,t0 (logxor ,t0 ,r0)
         ,o0 ,r1
         ,o1 ,r2
         ,o2 ,t0
         ,o3 ,r3))

(defmacro serpent-sbox7 (r0 r1 r2 r3 o0 o1 o2 o3 t0)
  `(setf ,t0 ,r1
         ,r1 (logior ,r1 ,r2)
         ,r1 (logxor ,r1 ,r3)
         ,t0 (logxor ,t0 ,r2)
         ,r2 (logxor ,r2 ,r1)
         ,r3 (logior ,r3 ,t0)
         ,r3 (logand ,r3 ,r0)
         ,t0 (logxor ,t0 ,r2)
         ,r3 (logxor ,r3 ,r1)
         ,r1 (logior ,r1 ,t0)
         ,r1 (logxor ,r1 ,r0)
         ,r0 (logior ,r0 ,t0)
         ,r0 (logxor ,r0 ,r2)
         ,r1 (logxor ,r1 ,t0)
         ,r2 (logxor ,r2 ,r1)
         ,r1 (logand ,r1 ,r0)
         ,r1 (logxor ,r1 ,t0)
         ,r2 (mod32lognot ,r2)
         ,r2 (logior ,r2 ,r0)
         ,t0 (logxor ,t0 ,r2)
         ,o0 ,t0
         ,o1 ,r3
         ,o2 ,r1
         ,o3 ,r0))

(defmacro serpent-sbox7-inverse (r0 r1 r2 r3 o0 o1 o2 o3 t0)
  `(setf ,t0 ,r2
         ,r2 (logxor ,r2 ,r0)
         ,r0 (logand ,r0 ,r3)
         ,t0 (logior ,t0 ,r3)
         ,r2 (mod32lognot ,r2)
         ,r3 (logxor ,r3 ,r1)
         ,r1 (logior ,r1 ,r0)
         ,r0 (logxor ,r0 ,r2)
         ,r2 (logand ,r2 ,t0)
         ,r3 (logand ,r3 ,t0)
         ,r1 (logxor ,r1 ,r2)
         ,r2 (logxor ,r2 ,r0)
         ,r0 (logior ,r0 ,r2)
         ,t0 (logxor ,t0 ,r1)
         ,r0 (logxor ,r0 ,r3)
         ,r3 (logxor ,r3 ,t0)
         ,t0 (logior ,t0 ,r0)
         ,r3 (logxor ,r3 ,r2)
         ,t0 (logxor ,t0 ,r2)
         ,o0 ,r3
         ,o1 ,r0
         ,o2 ,r1
         ,o3 ,t0))


;;; Linear transformation

(defmacro serpent-linear-transformation (r0 r1 r2 r3)
  `(setf ,r0 (rol32 ,r0 13)
         ,r2 (rol32 ,r2 3)
         ,r1 (logxor ,r1 ,r0 ,r2)
         ,r3 (logxor ,r3 ,r2 (mod32ash ,r0 3))
         ,r1 (rol32 ,r1 1)
         ,r3 (rol32 ,r3 7)
         ,r0 (logxor ,r0 ,r1 ,r3)
         ,r2 (logxor ,r2 ,r3 (mod32ash ,r1 7))
         ,r0 (rol32 ,r0 5)
         ,r2 (rol32 ,r2 22)))

(defmacro serpent-linear-transformation-inverse (r0 r1 r2 r3)
  `(setf ,r2 (rol32 ,r2 10)
         ,r0 (rol32 ,r0 27)
         ,r2 (logxor ,r2 ,r3 (mod32ash ,r1 7))
         ,r0 (logxor ,r0 ,r1 ,r3)
         ,r3 (rol32 ,r3 25)
         ,r1 (rol32 ,r1 31)
         ,r3 (logxor ,r3 ,r2 (mod32ash ,r0 3))
         ,r1 (logxor ,r1 ,r0 ,r2)
         ,r2 (rol32 ,r2 29)
         ,r0 (rol32 ,r0 19)))


;;; Key schedule

(defconstant +serpent-phi+ #x9e3779b9)

(defclass serpent (cipher 16-byte-block-mixin)
  ((subkeys :accessor serpent-subkeys
            :type (simple-array (unsigned-byte 32) (33 4)))))

(defun serpent-pad-key (key)
  (let ((padded-key (make-array 8 :element-type '(unsigned-byte 32)))
        (len (floor (length key) 4)))
    (dotimes (i len)
      (setf (aref padded-key i) (ub32ref/le key (* i 4))))
    (when (< len 8)
      (setf (aref padded-key len) 1)
      (loop for i from (1+ len) below 8
            do (setf (aref padded-key i) 0)))
    padded-key))

(defun serpent-generate-subkeys (key)
  (declare (type (simple-array (unsigned-byte 32) (8)) key)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let ((subkeys (make-array '(33 4) :element-type '(unsigned-byte 32)))
        (w (copy-seq key))
        (ws (make-array 4 :element-type '(unsigned-byte 32)))
        (wt (make-array 4 :element-type '(unsigned-byte 32)))
        (t0 0)
        (t1 0)
        (t2 0)
        (t3 0)
        (t4 0))
    (declare (type (simple-array (unsigned-byte 32) (33 4)) subkeys)
             (type (simple-array (unsigned-byte 32) (8)) w)
             (type (simple-array (unsigned-byte 32) (4)) ws wt)
             (type (unsigned-byte 32) t0 t1 t2 t3 t4))
    (macrolet ((expand-key4 (wo r)
                 `(setf (aref ,wo 0) (rol32 (logxor (aref w ,(mod (+ r 0) 8))
                                                    (aref w ,(mod (+ r 3) 8))
                                                    (aref w ,(mod (+ r 5) 8))
                                                    (aref w ,(mod (+ r 7) 8))
                                                    +serpent-phi+
                                                    ,(+ r 0))
                                            11)
                        (aref w ,(mod (+ r 0) 8)) (aref ,wo 0)
                        (aref ,wo 1) (rol32 (logxor (aref w ,(mod (+ r 1) 8))
                                                    (aref w ,(mod (+ r 4) 8))
                                                    (aref w ,(mod (+ r 6) 8))
                                                    (aref w ,(mod (+ r 0) 8))
                                                    +serpent-phi+
                                                    ,(+ r 1))
                                            11)
                        (aref w ,(mod (+ r 1) 8)) (aref ,wo 1)
                        (aref ,wo 2) (rol32 (logxor (aref w ,(mod (+ r 2) 8))
                                                    (aref w ,(mod (+ r 5) 8))
                                                    (aref w ,(mod (+ r 7) 8))
                                                    (aref w ,(mod (+ r 1) 8))
                                                    +serpent-phi+
                                                    ,(+ r 2))
                                            11)
                        (aref w ,(mod (+ r 2) 8)) (aref ,wo 2)
                        (aref ,wo 3) (rol32 (logxor (aref w ,(mod (+ r 3) 8))
                                                    (aref w ,(mod (+ r 6) 8))
                                                    (aref w ,(mod (+ r 0) 8))
                                                    (aref w ,(mod (+ r 2) 8))
                                                    +serpent-phi+
                                                    ,(+ r 3))
                                            11)
                        (aref w ,(mod (+ r 3) 8)) (aref ,wo 3)))

               (make-subkeys ()
                 (loop for i from 0 to 15
                       for sbox-a = (read-from-string (format nil "serpent-sbox~d" (mod (- 3 (* 2 i)) 8)))
                       for sbox-b = (read-from-string (format nil "serpent-sbox~d" (mod (- 2 (* 2 i)) 8)))
                       append (list `(expand-key4 ws ,(* 8 i))
                                    `(expand-key4 wt ,(+ (* 8 i) 4))
                                    `(setf t0 (aref ws 0)
                                           t1 (aref ws 1)
                                           t2 (aref ws 2)
                                           t3 (aref ws 3))
                                    `(,sbox-a t0 t1 t2 t3 (aref ws 0) (aref ws 1) (aref ws 2) (aref ws 3) t4)
                                    `(setf (aref subkeys ,(* 2 i) 0) (aref ws 0)
                                           (aref subkeys ,(* 2 i) 1) (aref ws 1)
                                           (aref subkeys ,(* 2 i) 2) (aref ws 2)
                                           (aref subkeys ,(* 2 i) 3) (aref ws 3))
                                    `(setf t0 (aref wt 0)
                                           t1 (aref wt 1)
                                           t2 (aref wt 2)
                                           t3 (aref wt 3))
                                    `(,sbox-b t0 t1 t2 t3 (aref wt 0) (aref wt 1) (aref wt 2) (aref wt 3) t4)
                                    `(setf (aref subkeys ,(1+ (* 2 i)) 0) (aref wt 0)
                                           (aref subkeys ,(1+ (* 2 i)) 1) (aref wt 1)
                                           (aref subkeys ,(1+ (* 2 i)) 2) (aref wt 2)
                                           (aref subkeys ,(1+ (* 2 i)) 3) (aref wt 3)))
                       into forms
                       finally (return `(progn ,@forms)))))

      (make-subkeys)
      (expand-key4 ws 128)
      (setf t0 (aref ws 0)
            t1 (aref ws 1)
            t2 (aref ws 2)
            t3 (aref ws 3))
      (serpent-sbox3 t0 t1 t2 t3 (aref ws 0) (aref ws 1) (aref ws 2) (aref ws 3) t4)
      (setf (aref subkeys 32 0) (aref ws 0)
            (aref subkeys 32 1) (aref ws 1)
            (aref subkeys 32 2) (aref ws 2)
            (aref subkeys 32 3) (aref ws 3))

      subkeys)))

(defmethod schedule-key ((cipher serpent) key)
  (setf (serpent-subkeys cipher) (serpent-generate-subkeys (serpent-pad-key key)))
  cipher)


;;; Rounds

(define-block-encryptor serpent 16
  (let ((subkeys (serpent-subkeys context))
        (t0 0)
        (t1 0)
        (t2 0)
        (t3 0)
        (t4 0))
    (declare (type (simple-array (unsigned-byte 32) (33 4)) subkeys)
             (type (unsigned-byte 32) t0 t1 t2 t3 t4))
    (with-words ((b0 b1 b2 b3) plaintext plaintext-start :big-endian nil :size 4)
      (macrolet ((serpent-rounds ()
                   (loop for i from 0 to 30
                      for sbox = (read-from-string (format nil "serpent-sbox~d" (mod i 8)))
                      append (list `(setf t0 (logxor b0 (aref subkeys ,i 0))
                                          t1 (logxor b1 (aref subkeys ,i 1))
                                          t2 (logxor b2 (aref subkeys ,i 2))
                                          t3 (logxor b3 (aref subkeys ,i 3)))
                                   `(,sbox t0 t1 t2 t3 b0 b1 b2 b3 t4)
                                   `(serpent-linear-transformation b0 b1 b2 b3))
                      into forms
                      finally (return `(progn ,@forms)))))

        ;; Regular rounds
        (serpent-rounds)

        ;; Last round
        (setf b0 (logxor b0 (aref subkeys 31 0))
              b1 (logxor b1 (aref subkeys 31 1))
              b2 (logxor b2 (aref subkeys 31 2))
              b3 (logxor b3 (aref subkeys 31 3)))
        (serpent-sbox7 b0 b1 b2 b3 t0 t1 t2 t3 t4)
        (setf b0 (logxor t0 (aref subkeys 32 0))
              b1 (logxor t1 (aref subkeys 32 1))
              b2 (logxor t2 (aref subkeys 32 2))
              b3 (logxor t3 (aref subkeys 32 3)))

        (store-words ciphertext ciphertext-start b0 b1 b2 b3)
        (values)))))

(define-block-decryptor serpent 16
  (let ((subkeys (serpent-subkeys context))
        (t0 0)
        (t1 0)
        (t2 0)
        (t3 0)
        (t4 0))
    (declare (type (simple-array (unsigned-byte 32) (33 4)) subkeys)
             (type (unsigned-byte 32) t0 t1 t2 t3 t4))
    (with-words ((b0 b1 b2 b3) ciphertext ciphertext-start :big-endian nil :size 4)
      (macrolet ((serpent-rounds-inverse ()
                   (loop for i from 30 downto 0
                      for sbox-inverse = (read-from-string (format nil "serpent-sbox~d-inverse" (mod i 8)))
                      append (list `(serpent-linear-transformation-inverse b0 b1 b2 b3)
                                   `(,sbox-inverse b0 b1 b2 b3 t0 t1 t2 t3 t4)
                                   `(setf b0 (logxor t0 (aref subkeys ,i 0))
                                          b1 (logxor t1 (aref subkeys ,i 1))
                                          b2 (logxor t2 (aref subkeys ,i 2))
                                          b3 (logxor t3 (aref subkeys ,i 3))))
                      into forms
                      finally (return `(progn ,@forms)))))

        ;; First inverse round
        (setf b0 (logxor b0 (aref subkeys 32 0))
              b1 (logxor b1 (aref subkeys 32 1))
              b2 (logxor b2 (aref subkeys 32 2))
              b3 (logxor b3 (aref subkeys 32 3)))
        (serpent-sbox7-inverse b0 b1 b2 b3 t0 t1 t2 t3 t4)
        (setf b0 (logxor t0 (aref subkeys 31 0))
              b1 (logxor t1 (aref subkeys 31 1))
              b2 (logxor t2 (aref subkeys 31 2))
              b3 (logxor t3 (aref subkeys 31 3)))

        ;; Regular inverse rounds
        (serpent-rounds-inverse)

        (store-words plaintext plaintext-start b0 b1 b2 b3)
        (values)))))

(defcipher serpent
  (:encrypt-function serpent-encrypt-block)
  (:decrypt-function serpent-decrypt-block)
  (:block-length 16)
  (:key-length (:fixed 16 24 32)))
