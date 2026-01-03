;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; macs.lisp -- common functions for message authentication codes

(in-package :crypto)


(defclass mac () ())

(defun macp (sym)
  (get sym '%make-mac))

(defun list-all-macs ()
  (loop for symbol being each external-symbol of (find-package :ironclad)
        if (macp symbol)
          collect (intern (symbol-name symbol) :keyword) into macs
        finally (return (sort macs #'string<))))

(defun mac-supported-p (name)
  "Return T if the mac NAME is a valid mac name."
  (and (symbolp name)
       (not (null (macp (massage-symbol name))))))

(defmacro defmac (name maker updater producer)
  `(progn
     (setf (get ',name '%make-mac) #',maker)

     (defmethod update-mac ((mac ,name) (sequence vector) &key (start 0) (end (length sequence)))
       (check-type sequence simple-octet-vector)
       (check-type start index)
       (check-type end index)
       (,updater mac sequence :start start :end end)
       (values))

     (defmethod produce-mac ((mac ,name) &key digest (digest-start 0))
       (let* ((mac-digest (,producer mac))
              (digest-size (length mac-digest)))
         (etypecase digest
           (simple-octet-vector
            (if (<= digest-size (- (length digest) digest-start))
                (replace digest mac-digest :start1 digest-start)
                (error 'insufficient-buffer-space
                       :buffer digest
                       :start digest-start
                       :length digest-size)))
           (null
            mac-digest))))))

(defun make-mac (mac-name key &rest args)
  "Return a MAC object which uses the algorithm MAC-NAME
initialized with a KEY."
  (typecase mac-name
    (symbol
     (let ((name (massage-symbol mac-name)))
       (if (macp name)
           (apply (the function (get name '%make-mac)) key args)
           (error 'unsupported-mac :name mac-name))))
    (t
     (error 'type-error :datum mac-name :expected-type 'symbol))))
