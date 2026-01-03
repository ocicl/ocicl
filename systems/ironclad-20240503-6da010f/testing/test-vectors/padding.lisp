;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; padding.lisp

(in-package :crypto-tests)

(rtest:deftest pkcs7-padding
  (let* ((block-size 16)
         (buffer (make-array block-size
                             :element-type '(unsigned-byte 8)
                             :initial-element #xff))
         (padding (make-instance 'crypto::pkcs7-padding)))
    (flet ((pad-and-check (length)
             (crypto::add-padding-bytes padding buffer 0 length block-size)
             (let ((padding-bytes (crypto::count-padding-bytes padding buffer 0 block-size)))
               (and (= padding-bytes (- block-size length))
                    (loop for i from length below block-size
                          always (= (aref buffer i) padding-bytes))))))
      (if (loop for length from 0 below block-size
                always (pad-and-check length))
          :ok
          :error)))
  :ok)

(rtest:deftest ansi-x923-padding
  (let* ((block-size 16)
         (buffer (make-array block-size
                             :element-type '(unsigned-byte 8)
                             :initial-element #xff))
         (padding (make-instance 'crypto::ansi-x923-padding)))
    (flet ((pad-and-check (length)
             (crypto::add-padding-bytes padding buffer 0 length block-size)
             (let ((padding-bytes (crypto::count-padding-bytes padding buffer 0 block-size)))
               (and (= padding-bytes (- block-size length))
                    (loop for i from length below (1- block-size)
                          always (zerop (aref buffer i)))
                    (= (aref buffer (1- block-size)) padding-bytes)))))
      (if (loop for length from 0 below block-size
                always (pad-and-check length))
          :ok
          :error)))
  :ok)

(rtest:deftest iso-7816-4-padding
  (let* ((block-size 16)
         (buffer (make-array block-size
                             :element-type '(unsigned-byte 8)
                             :initial-element #xff))
         (padding (make-instance 'crypto::iso-7816-4-padding)))
    (flet ((pad-and-check (length)
             (crypto::add-padding-bytes padding buffer 0 length block-size)
             (let ((padding-bytes (crypto::count-padding-bytes padding buffer 0 block-size)))
               (and (= padding-bytes (- block-size length))
                    (= (aref buffer length) #x80)
                    (loop for i from (1+ length) below block-size
                          always (zerop (aref buffer i)))))))
      (if (loop for length from 0 below block-size
                always (pad-and-check length))
          :ok
          :error)))
  :ok)
