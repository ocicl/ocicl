;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-iso-2022-jp.lisp --- Implementation of the ISO-2022-JP character encoding.
;;;
;;; Copyright (C) 2025, Bartosz Knapik <knapikbartek@gmail.com>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.


(in-package #:babel-encodings)

(define-character-encoding :iso-2022-jp
    "A 7-bit, stateful character encoding that switches between ASCII, JIS X 0201 and JIS X 0208
     using escape sequences."
  :max-units-per-char 5
  :codespace `(,+unicode-to-jis-x-0208+))

(define-octet-counter :iso-2022-jp (getter type)
  `(lambda (seq start end max)
     (declare (type ,type seq) (fixnum start end max))
     (let ((noctets 0)
           (mode :ascii))
       (loop for i from start below end
             for u1 of-type code-point = (,getter seq i)
             do (cond
                  ((< u1 #x80)
                   (unless (eq mode :ascii)
                     (incf noctets 3)
                     (setf mode :ascii))
                   (incf noctets))

                  ((or (= u1 #x00a5) (= u1 #x203e) (< #xff60 u1 #xffa0))
                   (unless (eq mode :jis-x-0201)
                     (incf noctets 3)
                     (setf mode :jis-x-0201))
                   (incf noctets))

                  ((gethash u1 +unicode-to-jis-x-0208+)
                   (unless (eq mode :jis-x-0208)
                     (incf noctets 3)
                     (setf mode :jis-x-0208))
                   (incf noctets 2)))

                (when (and (plusp max) (= noctets max))
                  (return (values noctets i)))
             finally (return (values noctets i))))))

(define-code-point-counter :iso-2022-jp (getter type)
  `(lambda (seq start end max)
     (declare (type ,type seq)
              (fixnum start end))
     (let ((count 0)
           (mode :ascii))
       (loop with i = start
             while (< i end)
             do (let ((byte (,getter seq i)))
                  (cond
                    ;; ESC sequence
                    ((= byte 27)
                     (let ((b1 (,getter seq (+ i 1)))
                           (b2 (,getter seq (+ i 2))))
                       (cond
                         ((and (= b1 40) (= b2 66)) (setf mode :ascii))                      ; ESC ( B
                         ((and (= b1 40) (= b2 74)) (setf mode :jis-x-0201))                 ; ESC ( J
                         ((and (= b1 36) (or (= b2 64) (= b2 66))) (setf mode :jis-x-0208))) ; ESC $ @ OR ESC $ B)
                       (incf i 3)))

                     ;; ASCII or JIS-X-0201
                     ((or (eq mode :ascii)
                          (eq mode :jis-x-0201))
                      (incf count)
                      (incf i))

                     ;; JIS-X-0208
                     ((eq mode :jis-x-0208)
                      (incf count)
                      (incf i 2))

                     (t
                      (incf i))))
                  finally (return (values count i))))))
 

(define-decoder :iso-2022-jp (getter src-type setter dest-type)
  `(lambda (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (let ((noctets 0)
           (mode :ascii))

       (flet ((read-esc-sequence (i)
                (let ((b1 (,getter src (+ i 1)))
                      (b2 (,getter src (+ i 2))))
                  (cond
                    ((and (= b1 40) (= b2 66)) :ascii)                     ; ESC ( B
                    ((and (= b1 40) (= b2 74)) :jis-x-0201)                ; ESC ( J
                    ((and (= b1 36) (or (= b2 64) (= b2 66))) :jis-x-0208) ; ESC $ @ OR ESC $ B)
                    (t :unknown)))))

         (loop with i = start
               while (< i end)
               do (macrolet
                      ((handle-error (&optional (c 'character-encoding-error))
                         `(encoding-error byte :iso-2022-jp src i +repl+ ',c)))
                    (let ((byte (,getter src i)))
                      (cond
                        ;; ESC sequence
                        ((= byte 27)
                         (let ((new-mode (read-esc-sequence i)))
                           (if (eq new-mode :unknown)
                               (handle-error)
                               (setf mode new-mode))
                           (incf i 3)))

                        ;; ASCII
                        ((eq mode :ascii)
                         (if (>= byte 128)
                             (handle-error)
                             (progn
                               (,setter byte dest noctets)
                               (incf noctets)
                               (incf i))))

                        ;; JIS-X-0201
                        ((eq mode :jis-x-0201)
                         (cond
                           ((< byte #x80)
                            (cond
                              ((= byte #x5c)
                               (,setter #x00a5 dest noctets))
                              ((= byte #x7e)
                               (,setter #x203e dest noctets))
                              (t
                               (,setter byte dest noctets)))
                            (incf noctets)
                            (incf i))
                           ((< #xa0 byte #xe0)
                            (,setter (+ byte #xfec0) dest noctets)
                            (incf noctets)
                            (incf i))
                           (t (handle-error))))

                        ;; JIS-X-0208
                        ((eq mode :jis-x-0208)
                         (let ((b1 byte)
                               (b2 (,getter src (+ i 1)))
                               (unicode #xfffd))
                           (when (and (or (<= #x21 b1 #x28) (<= #x30 b1 #x74))
                                      (<= #x21 b2 #x7e))
                             (let ((index (+ (* 94 (- b1 #x21)) (- b2 #x21))))
                               ;; indexes greater or equal 1410 correspond to page30
                               (if (< index 1410)
                                   (when (< index 690)
                                     (setf unicode (svref +jis-x-0208-to-uni-page21+ index)))
                                   (when (< index 7808) ; 7808 - 1410 is the size of +jis-x-0208-to-uni-page30+ 
                                     (setf unicode (svref +jis-x-0208-to-uni-page30+ (- index 1410)))))))

                           (,setter unicode dest noctets)
                           (incf noctets)
                           (incf i 2)))

                        (t
                         (handle-error)))))
               finally (return (the fixnum (- noctets d-start))))))))

(define-encoder :iso-2022-jp (getter src-type setter dest-type)
  `(lambda (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (let ((c 0) index (noctets 0) (mode :ascii))
       (loop for i from start below end
             for code of-type code-point = (,getter src i)
             do (macrolet
                    ((handle-error (&optional (c 'character-encoding-error))
                       `(encoding-error code :iso-2022-jp src i +repl+ ',c)))

                  (setf c (code-char code))

                  (cond
                    ((< code #x80)
                     (unless (eq mode :ascii)
                       (,setter #x1b dest noctets)
                       (,setter #x28 dest (+ 1 noctets))
                       (,setter #x42 dest (+ 2 noctets))
                       (incf noctets 3)
                       (setf mode :ascii))
                     (,setter code dest noctets)
                     (incf noctets))

                    ;; JIS X 0201
                    ((or (= code #x00a5) (= code #x203e) (< #xff60 code #xffa0))
                     (unless (eq mode :jis-x-0201)
                       (,setter #x1b dest noctets)
                       (,setter #x28 dest (+ 1 noctets))
                       (,setter #x4a dest (+ 2 noctets))
                       (incf noctets 3)
                       (setf mode :jis-x-0201))

                     (cond
                       ((= code #x00a5)
                        (,setter #x5c dest noctets))
                       ((= code #x203e)
                        (,setter #x7e dest noctets))
                       ((< #xff60 code #xffa0)
                        (,setter (- code #xfec0) dest noctets)))

                     (incf noctets))

                    ;; JIS X 0208
                    ((gethash code +unicode-to-jis-x-0208+)
                     (unless (eq mode :jis-x-0208)
                       (,setter #x1b dest noctets)
                       (,setter #x24 dest (+ 1 noctets))
                       (,setter #x42 dest (+ 2 noctets))
                       (incf noctets 3)
                       (setf mode :jis-x-0208))

                     (let* ((bytes (gethash code +unicode-to-jis-x-0208+))
                            (b1 (ldb (byte 8 8) bytes))
                            (b2 (ldb (byte 8 0) bytes)))
                       (,setter b1 dest noctets)
                       (,setter b2 dest (+ 1 noctets)))

                     (incf noctets 2))
                    
                    (t (handle-error))))
             finally (return (the fixnum (- noctets d-start)))))))
