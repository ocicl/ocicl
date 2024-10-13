;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar-common-extract)

(defun compute-extraction-pathname (entry name strip-components)
  (let (components
        pn
        device)
    (when (uiop:os-windows-p)
      (setf name (substitute #\/ #\\ name)))
    (when (and (uiop:os-windows-p)
               (eql #\: (aref name 1))
               (member (aref name 0)
                       '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
                         #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)
                       :test #'char-equal))
      (setf name (subseq name 2))
      (restart-case
          (error 'entry-name-contains-device-error
                 :entry entry)
        (relativize-entry-name ())
        (continue ()
          (setf device (aref name 0)))))

    (setf components (split-sequence:split-sequence #\/ name))
    (restart-case
        (if (member ".." components :test #'equal)
            (error 'entry-name-contains-..-error :entry entry)
            (setf pn (uiop:parse-unix-namestring name :dot-dot :back)))
      (treat-..-as-back ()
        (setf pn (uiop:parse-unix-namestring name :dot-dot :back)))
      (treat-..-as-up ()
        (setf pn (uiop:parse-unix-namestring name :dot-dot :up))))

    (restart-case
        (when (uiop:absolute-pathname-p pn)
          (error 'entry-name-is-absolute-error :entry entry))
      (continue ())
      (relativize-entry-name ()
        (setf pn (uiop:relativize-pathname-directory pn))))

    (let* ((directory (if (null (pathname-directory pn))
                          (list :relative)
                          (pathname-directory pn)))
           (num-components (+ (length (rest directory))
                              (if (or (null (file-namestring pn))
                                      (equal "" (file-namestring pn)))
                                  0
                                  1))))
      (when (> num-components strip-components)
        (pathname-coalesce-backs
         (make-pathname :directory (list* (first directory)
                                          (subseq (rest directory)
                                                  strip-components))
                        :defaults pn))))))
