(defpackage #:cl+ssl.test.ffi-buffer-clisp
  (:use #:common-lisp #:5am))
(in-package #:cl+ssl.test.ffi-buffer-clisp)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (import'(cl+ssl::make-buffer
           cl+ssl::buffer-length
           cl+ssl::buffer-elt
           cl+ssl::clisp-ffi-buffer
           cl+ssl::clisp-ffi-buffer-pointer
           cl+ssl::clisp-ffi-buffer-size
           cl+ssl::b/s-replace
           cl+ssl::s/b-replace
           cl+ssl::*mem-max*)))

(def-suite :cl+ssl.ffi-buffer-clisp :in :cl+ssl
  :description "Tests ffi-buffer-clisp.lisp's s/b-replace and b/s-replace")

(in-suite :cl+ssl.ffi-buffer-clisp)

;; The number of extra bytes allocated after the end of the buffer.
;; The bytes are zeroed, and after each test case we
;; verify if they stay zeroed to detect buffer overflow.
(defparameter *buf-extra-len* 10)

(defun make-test-buffer (length &optional data)
  (let ((result (make-buffer (+ length *buf-extra-len*))))
    (dotimes (i (buffer-length result))
      (setf (buffer-elt result i)
            (if (and data (< i (length data)))
                (aref data i)
                0)))
    (setf (clisp-ffi-buffer-size result) length)
    result))

(defun release-buffer (buf)
  (ffi:foreign-free (clisp-ffi-buffer-pointer buf)))

(defun call-with-test-buffer (length data body-fn)
  (let ((buf (make-test-buffer length data)))
    (unwind-protect
         (funcall body-fn buf)
      (release-buffer buf))))

(defmacro with-test-buffer ((buf-var length &optional data) &body body)
  `(call-with-test-buffer ,length ,data (lambda (,buf-var) ,@body)))

(with-test-buffer (buf 3 #(1 2 3))
  (assert (= (buffer-elt buf 1))))

;; Returns buffer bytes and its extended memory bytes
;; as an array, with "|" placed between data bytes and the
;; extended bytes
(defgeneric buf-view (buf))

(defmethod buf-view ((buf clisp-ffi-buffer))
  (let ((result (make-array (+ (buffer-length buf)
                               *buf-extra-len*
                               1)
                            :fill-pointer 0)))
    (dotimes (i (buffer-length buf))
      (vector-push (buffer-elt buf i) result))
    (vector-push "|" result)
    (dotimes (i *buf-extra-len*)
      (vector-push (buffer-elt buf (+ (buffer-length buf) i))
                    result))
    result))

(with-test-buffer (buf 4 #(1 2 3 4))
  (assert (equalp #(1 2 3 4 "|" 0 0 0 0 0 0 0 0 0 0)
                  (buf-view buf))))

(defmethod buf-view ((expected-vec array))
  (let ((result (make-array (+ (length expected-vec)
                               *buf-extra-len*
                               1)
                            :fill-pointer (length expected-vec))))
    (replace result expected-vec)
    (vector-push "|" result)
    (dotimes (_ *buf-extra-len*)
      (vector-push 0 result))
    result))

(assert (equalp #(1 2 3 "|" 0 0 0 0 0 0 0 0 0 0)
                (buf-view #(1 2 3))))

(defun assert-buf-equal (expected-vec buf)
  (let ((buf-view (buf-view buf))
        (expected-view (buf-view expected-vec)))
    (is (equalp expected-view buf-view)
        "padded buffer is not exqual to the expected value:~%~S, the actual buffer:~%~S"
        expected-view
        buf-view)))

(defun expect-b/s-replace (expected-buf buf-len vec
                           &rest rest &key start1 end1 start2 end2)
  (declare (ignore start1 end1 start2 end2))
  (dolist (*mem-max* (list *mem-max* 2))
    (dolist (seq (list vec (coerce vec 'list)))
      (with-test-buffer (buf buf-len)
        (apply #'b/s-replace buf seq rest)
        (assert-buf-equal expected-buf buf)))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun intern-test-name (test-name-str)
    ;; like INTERN but respects the current readtable case
    (let ((*package* (find-package '#:cl+ssl.test.ffi-buffer-clisp)))
      (read-from-string test-name-str))))

(defmacro b/s-replace-test ((buf-len
                             vec
                             &key (start1 0 start1-supplied-p)
                               (end1 nil end1-supplied-p)
                               (start2 0 start2-supplied-p)
                               (end2 nil end2-supplied-p))
                            expected-buf)
  (let* ((test-name (format nil
                            "b/s-replace-buf-~A-seq-~A~:[~*~;-start1-~A~]~:[~*~;-end1-~A~]~:[~*~;-start2-~A~]~:[~*~;-end2-~A~]"
                            buf-len (length vec)
                            start1-supplied-p start1
                            end1-supplied-p end1
                            start2-supplied-p start2
                            end2-supplied-p end2))
         (test-name-sym (intern-test-name test-name)))
    `(test ,test-name-sym
       (expect-b/s-replace ,expected-buf ,buf-len ,vec
                           ,@(when start1-supplied-p `(:start1 ,start1))
                           ,@(when end1-supplied-p `(:end1 ,end1))
                           ,@(when start2-supplied-p `(:start2 ,start2))
                           ,@(when end2-supplied-p `(:end2 ,end2))))))

;;; buf is larger than seq
(b/s-replace-test (4 #(1 2 3))
                  #(1 2 3 0))
(b/s-replace-test (4 #(1 2 3) :start1 0 :start2 0)
                  #(1 2 3 0))
(b/s-replace-test (4 #(1 2 3) :start1 0 :end1 4 :start2 0 :end2 3)
                  #(1 2 3 0))
(b/s-replace-test (4 #(1 2 3) :start1 0 :end1 3 :start2 0 :end2 3)
                  #(1 2 3 0))
(b/s-replace-test (4 #(1 2 3) :start1 1 :end1 4 :start2 0 :end2 3)
                  #(0 1 2 3))
(b/s-replace-test (4 #(1 2 3) :start1 0 :end1 2 :start2 1 :end2 3)
                  #(2 3 0 0))

;;; same length
(b/s-replace-test (4 #(1 2 3 4))
                   #(1 2 3 4))
(b/s-replace-test (4 #(1 2 3 4) :start1 0 :start2 0)
                   #(1 2 3 4))
(b/s-replace-test (4 #(1 2 3 4) :start1 0 :end1 4 :start2 0 :end2 4)
                  #(1 2 3 4))
(b/s-replace-test (4 #(1 2 3 4) :start1 1 :end1 4 :start2 1 :end2 4)
                  #(0 2 3 4))
(b/s-replace-test (4 #(1 2 3 4) :start1 1 :end1 4)
                  #(0 1 2 3))

;;; buf is smaller than seq
(b/s-replace-test (4 #(1 2 3 4 5))
                  #(1 2 3 4))
(b/s-replace-test (4 #(1 2 3 4 5) :start1 0 :start2 0)
                  #(1 2 3 4))
(b/s-replace-test (4 #(1 2 3 4 5) :start1 0 :end1 4 :start2 0 :end2 5)
                  #(1 2 3 4))
(b/s-replace-test (4 #(1 2 3 4 5) :start1 0 :end1 4 :start2 0 :end2 4)
                   #(1 2 3 4))
(b/s-replace-test (4 #(1 2 3 4 5) :start1 2 :end1 3 :start2 1 :end2 5)
                  #(0 0 2 0))
(b/s-replace-test (4 #(1 2 3 4 5) :start2 3 :end2 5)
                  #(4 5 0 0))

;;; empty seq
(b/s-replace-test (4 #() :start1 0 :end1 0 :start2 0 :end2 0)
                  #(0 0 0 0))
(b/s-replace-test (0 #() :start1 0 :end1 0 :start2 0 :end2 0)
                  #())

(test b/s-replace-boundary-errors
  (dolist (seq (list '() #()))
    (with-test-buffer (buf 2)
      (signals serious-condition
        (b/s-replace buf seq :end1 4))
      (signals serious-condition
        (b/s-replace buf seq :end2 2))
      (signals serious-condition
        (b/s-replace buf seq :start1 -1))
      (signals serious-condition
        (b/s-replace buf seq :start2 -1)))))

(test s/b-replace-boundary-errors
  (dolist (seq (list '(1 2 3 4) #(1 2 3 4)))
    (with-test-buffer (buf 8)
      (signals serious-condition
        (s/b-replace seq buf :end1 6))
      (signals serious-condition
        (s/b-replace seq buf :end2 10))
      (signals serious-condition
        (s/b-replace seq buf :start1 -1))
      (signals serious-condition
        (s/b-replace seq buf :start2 -1)))))

(defun expect-s/b-replace (expected-seq-data seq-len buf-data
                           &rest rest &key start1 end1 start2 end2)
  (declare (ignore start1 end1 start2 end2))
  (dolist (*mem-max* (list *mem-max* 2))
    (dolist (seq-type '(list (vector (unsigned-byte 8))))
      (with-test-buffer (buf (length buf-data) buf-data)
        (let ((seq (make-sequence seq-type seq-len :initial-element 0)))
          (apply #'s/b-replace seq buf rest)
          (is (equalp (coerce expected-seq-data seq-type)
                      seq)))))))

(defmacro s/b-replace-test ((seq-len
                             buf-data
                             &key (start1 0 start1-supplied-p)
                               (end1 nil end1-supplied-p)
                               (start2 0 start2-supplied-p)
                               (end2 nil end2-supplied-p))
                            expected-seq-data)
  (let* ((test-name (format nil
                            "s/b-replace-seq-~A-buf-~A~:[~*~;-start1-~A~]~:[~*~;-end1-~A~]~:[~*~;-start2-~A~]~:[~*~;-end2-~A~]"
                            seq-len (length buf-data)
                            start1-supplied-p start1
                            end1-supplied-p end1
                            start2-supplied-p start2
                            end2-supplied-p end2))
         (test-name-sym (intern-test-name test-name)))
    `(test ,test-name-sym
       (expect-s/b-replace ,expected-seq-data ,seq-len ,buf-data
                           ,@(when start1-supplied-p `(:start1 ,start1))
                           ,@(when end1-supplied-p `(:end1 ,end1))
                           ,@(when start2-supplied-p `(:start2 ,start2))
                           ,@(when end2-supplied-p `(:end2 ,end2))))))

(s/b-replace-test (2 #(0 1 2 3) :start1 0 :end1 2 :start2 0 :end2 2)
                  #(0 1))
(s/b-replace-test (4 #(0 1 2 3) :start1 0 :end1 4 :start2 0 :end2 4)
                  #(0 1 2 3))
(s/b-replace-test (6 #(0 1 2 3) :start1 0 :end1 4 :start2 0 :end2 4)
                  #(0 1 2 3 0 0))
