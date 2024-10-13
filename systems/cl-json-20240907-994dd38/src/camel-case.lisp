;;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Base: 10; Package: JSON -*-
;;;; Copyright (c) 2006-2008 Henrik Hjelte
;;;; All rights reserved.
;;;; See the file LICENSE for terms of use and distribution.

(in-package :json)

;;; First a simpler version, see testcase json-object-simplified-camel-case
;;; for difference with the ordinary came-case-to-lisp
(defun simplified-camel-case-to-lisp (camel-string)
 "Insert - between lowercase and uppercase chars.
Ignore _ + * and several consecutive uppercase."
 (declare (string camel-string))
 (let ((*print-pretty* nil))
   (with-output-to-string (result)
     (loop for c across camel-string
           with last-was-lowercase
           when (and last-was-lowercase
                     (upper-case-p c))
             do (princ "-" result)
           if (lower-case-p c)
             do (setf last-was-lowercase t)
           else
             do (setf last-was-lowercase nil)
           do (princ (char-upcase c) result)))))


(defun camel-case-split (string)
  "Assume STRING is in camel case, and split it into largest possible
``homogenous'' parts.  A homogenous part consists either a) of
upper-case alphabetic chars; or b) of lower-case alphabetic chars with
an optional initial upper-case; or c) of decimal digits; or d) of a
single non-alphanumeric char.  The return value is a list of
pairs (CATEGORY . PART) where CATEGORY is one of the keywords :UPPER,
:UPPER-1, :LOWER, :NUMERIC, :MIXED, and PART is a substring of
STRING."
  (let ((length (length string)))
    (macrolet ((shift-part (e new-cat &optional subst-cat)
                 `(prog1 (if b (cons ,(or subst-cat 'cat)
                                     (subseq string b ,e)))
                    (setq b ,e cat ,new-cat))))
      (loop for i from 0 to length
         with cat = nil and b = nil
         if (= i length)
           if (shift-part i nil) collect it end
         else if (let ((c (aref string i)))
                   (cond
                     ((upper-case-p c)
                      (case cat
                        ((:upper-1 :upper) (setq cat :upper) nil)
                        (t (shift-part i :upper-1))))
                     ((lower-case-p c)
                      (case cat
                        (:upper-1 (setq cat :mixed) nil)
                        (:upper (let ((subst-cat
                                       (if (> (- i b) 2) :upper :upper-1)))
                                  (shift-part (1- i) :mixed subst-cat)))
                        ((:numeric :punct nil) (shift-part i :lower))))
                     ((digit-char-p c)
                      (if (not (eql cat :numeric))
                          (shift-part i :numeric)))
                     (t (shift-part i :punct))))
           collect it))))

(defun camel-case-transform-all-caps (parts
                                      &optional cat-before from-numeric)
  "Take a list of PARTS (as returned by CAMEL-CASE-SPLIT) and
transform it into a string with Lisp-style hyphenation, assuming that
some initial portion of it does not contain :MIXED parts."
  (if (endp parts)
      (cond (from-numeric (throw 'all-caps nil))
            ((eql cat-before :punct) nil)
            (t '("+")))
      (destructuring-bind ((cat . part) . rest) parts
        (case cat
          ((:lower :mixed) (throw 'all-caps nil))
          (:punct
           (let ((transformed (if (string= part "_") "-" part)))
             (if (or from-numeric (eql cat-before :punct))
                 (cons transformed (camel-case-transform-all-caps rest cat))
                 (let ((transformed-rest
                        (catch 'all-caps
                          (camel-case-transform-all-caps rest cat))))
                   (if transformed-rest
                       (cons transformed transformed-rest)
                       (list* "+"
                              (if (string= part "_") "--" part)
                              (camel-case-transform rest cat)))))))
          ((:upper :upper1)
           (cons part (camel-case-transform-all-caps rest cat nil)))
          (t (cons part (camel-case-transform-all-caps
                         rest cat from-numeric)))))))

(defun camel-case-transform (parts &optional (cat-before :punct))
  "Take a list of PARTS (as returned by CAMEL-CASE-SPLIT) and
transform it into a string with Lisp-style hyphenation, assuming that
some initial portion of it does not contain :UPPER parts."
  (if (endp parts)
      '("")
      (destructuring-bind ((cat . part) . rest) parts
        (case cat
          (:upper
           (if (eql cat-before :punct)
               (let ((transformed-rest
                      (catch 'all-caps
                        (camel-case-transform-all-caps rest cat))))
                 (if transformed-rest
                     (list* "+" part transformed-rest)
                     (list* "+" part "+" (camel-case-transform rest cat))))
               (list* "-+" part "+" (camel-case-transform rest cat))))
          (:upper-1
           (case cat-before
             (:punct
              (let ((transformed-rest
                     (catch 'all-caps
                       (camel-case-transform-all-caps rest cat))))
                (if transformed-rest
                    (list* "+" part transformed-rest)
                    (list* "*" part (camel-case-transform rest cat)))))
             (:numeric (list* "-*" part (camel-case-transform rest cat)))
             (t (list* "-" part (camel-case-transform rest cat)))))
          (:numeric
           (case cat-before
             (:punct
              (let ((transformed-rest
                     (catch 'all-caps
                       (camel-case-transform-all-caps rest cat t))))
                (if transformed-rest
                    (list* "+" part transformed-rest)
                    (cons part (camel-case-transform rest cat)))))
             (t (list* "-" part (camel-case-transform rest cat)))))
          (:mixed
           (list* (case cat-before (:punct "*") (:numeric "-*") (t "-"))
                  (string-upcase part)
                  (camel-case-transform rest cat)))
          (:lower
           (list* (if (eql cat-before :punct) "" "-")
                  (string-upcase part)
                  (camel-case-transform rest cat)))
          (:punct
           (cons (if (string= part "_") "--" part)
                 (camel-case-transform rest cat)))))))

(defun camel-case-to-lisp (string)
  "Take a camel-case string and convert it into a string with
Lisp-style hyphenation."
  (apply #'concatenate 'string
         (camel-case-transform (camel-case-split string))))

(defun lisp-to-camel-case (string)
  "Take a string with Lisp-style hyphentation and convert it to camel
case.  This is an inverse of CAMEL-CASE-TO-LISP."
  (loop with i = 0 and l = (length string)
     with cc-string = (make-string l) and cc-i = 0
     with init = t and cap = nil and all-caps = nil
     while (< i l)
     do (let ((c (aref string i)))
          (unless (case c
                    (#\* (if init (setq cap t)))
                    (#\+ (cond
                           (all-caps (setq all-caps nil init t))
                           (init (setq all-caps t))))
                    (#\- (progn
                           (setq init t)
                           (cond
                             ((or all-caps
                                  (and (< (1+ i) l)
                                       (char= (aref string (1+ i)) #\-)
                                       (incf i)))
                              (setf (aref cc-string cc-i) #\_)
                              (incf cc-i))
                             (t (setq cap t))))))
            (setf (aref cc-string cc-i)
                  (if (and (or cap all-caps) (alpha-char-p c))
                      (char-upcase c)
                      (char-downcase c)))
            (incf cc-i)
            (setq cap nil init nil))
          (incf i))
     finally (return (subseq cc-string 0 cc-i))))
