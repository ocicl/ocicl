;;;; patch.lisp -- reading patch files and applying them

(in-package :diff)


;;; Some people, when confronted with a problem think, "I know, I'll
;;; use regular expressions."  Now they have two problems.
;;;
;;;                                               --Jamie Zawinski
(defparameter *number-regex* '(:greedy-repetition 1 nil :digit-class))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defun anchored-line (regex)
  (if (consp regex)
      `(:sequence :start-anchor ,@regex :end-anchor)
      `(:sequence :start-anchor ,regex :end-anchor)))
)

(defparameter *unified-diff-window-header*
  (cl-ppcre:create-scanner
   (anchored-line
    `("@@ -"
      (:register ,*number-regex*)
      (:greedy-repetition 0 1 (:sequence #\, (:register ,*number-regex*)))
      " +"
      (:register ,*number-regex*)
      (:greedy-repetition 0 1 (:sequence #\, (:register ,*number-regex*)))
      " @@"))))

(defparameter *context-diff-window-header*
  (cl-ppcre:create-scanner (anchored-line "***************")))

(defparameter *context-diff-window-original-line*
  (cl-ppcre:create-scanner
   (anchored-line
    `("*** "
      (:register ,*number-regex*)
      #\,
      (:register ,*number-regex*)
      " ****"))))

(defparameter *context-diff-window-modified-line*
  (cl-ppcre:create-scanner
   (anchored-line
    `("--- "
      (:register ,*number-regex*)
      #\,
      (:register ,*number-regex*)
      " ----"))))

(defparameter *index-header-line*
  (cl-ppcre:create-scanner
   (anchored-line
    '("Index: " (:register (:greedy-repetition 0 nil :everything))))))

(defparameter *prereq-header-line*
  (cl-ppcre:create-scanner
   (anchored-line
    '("Prereq: " (:register (:greedy-repetition 0 nil :everything))))))

(defparameter *unified-diff-line*
  (cl-ppcre:create-scanner
   (anchored-line
    `((:register (:alternation #\+ #\- #\Space))
      (:register (:greedy-repetition 0 nil :everything))))))

(defparameter *context-diff-line*
  (cl-ppcre:create-scanner
   (anchored-line
    `((:register (:alternation #\+ #\- #\! #\Space))
      (:register (:greedy-repetition 0 nil :everything))))))

(defparameter *unified-diff-header-original-line*
  (cl-ppcre:create-scanner
   '(:sequence :start-anchor
     "--- " (:register (:greedy-repetition 1 nil :non-whitespace-char-class)))))

(defparameter *unified-diff-header-modified-line*
  (cl-ppcre:create-scanner
   '(:sequence :start-anchor
     "+++ " (:register (:greedy-repetition 1 nil :non-whitespace-char-class)))))

(defparameter *context-diff-header-original-line*
  (cl-ppcre:create-scanner
   '(:sequence :start-anchor
     "*** " (:register (:greedy-repetition 1 nil :non-whitespace-char-class)))))

(defparameter *context-diff-header-modified-line*
  (cl-ppcre:create-scanner
   '(:sequence :start-anchor
     "--- " (:register (:greedy-repetition 1 nil :non-whitespace-char-class)))))

(defun collect-window-lines (stream test)
  (loop for line = (read-line stream)
        while (funcall test line)
        collect line into lines
        finally (return (values lines line))))


;;; abstraction for reading straight lines from a stream

(defclass line-generator ()
  ((peeked-line :accessor peeked-line :initform nil)
   (stream :accessor line-stream :initarg :stream)))

(defun make-line-generator (stream)
  (make-instance 'line-generator :stream stream))

(defun yield-line (line-generator)
  (cond
    ((peeked-line line-generator)
     (prog1 (peeked-line line-generator)
       (setf (peeked-line line-generator) nil)))
    (t
     (read-line (line-stream line-generator) nil nil))))

(defun peek-line (line-generator)
  (cond
    ((peeked-line line-generator)
     (peeked-line line-generator))
    (t
     (setf (peeked-line line-generator)
           (read-line (line-stream line-generator) nil nil)))))

;;; reading diff windows ("hunks")

(defparameter *default-lead-char-alist* '((#\Space . :common)
                                          (#\+ . :create)
                                          (#\- . :delete)))

(defparameter *context-original-lead-char-alist*
  (acons #\! :replace *default-lead-char-alist*))

(defparameter *context-modified-lead-char-alist*
  (acons #\! :insert *default-lead-char-alist*))

(defun line-to-chunk (line &optional (table *default-lead-char-alist*))
  (let* ((char (aref line 0))
         (text-line (subseq line 1))
         (chunk-kind (cdr (assoc char table :test #'char=))))
    (make-instance 'chunk
                   :kind chunk-kind
                   :lines (list text-line))))

(defgeneric read-diff-window (stream diff))

(defmethod read-diff-window (linegen (diff unified-diff))
  (let ((window (create-window-for-diff diff))
        (line (peek-line linegen)))
    (cond
      ((cl-ppcre:register-groups-bind ((#'parse-integer original-start
                                                        original-length
                                                        modified-start
                                                        modified-length))
           (*unified-diff-window-header* line)
         (setf (original-start-line window) (1- original-start)
               (original-length window) (if original-length original-length 1)
               (modified-start-line window) (1- modified-start)
               (modified-length window) (if modified-length modified-length 1))
         (yield-line linegen)))
      (t
       (return-from read-diff-window nil)))
    (loop for line = (peek-line linegen)
          while (and line (cl-ppcre:scan *unified-diff-line* line))
          collect (line-to-chunk (yield-line linegen)) into chunks
          finally (setf (window-chunks window) (consolidate-chunks chunks)))
    window))

;;; We want to convert sequences of chunks with the same kind to a single
;;; chunk containing the lines of the sequence.
(defun consolidate-chunks (chunk-list)
  (declare (type list chunk-list))
  (do ((list chunk-list)
       (consolidated nil))
      ((null list) (nreverse consolidated))
    ;; consolidate chunks which match the kind of the head chunk
    (let* ((head-chunk (first list))
           (kind (chunk-kind head-chunk)))
      (loop for rest on list
         for candidate = (car rest)
         while (eq kind (chunk-kind candidate))
         collect (first (chunk-lines candidate)) into lines
         finally (let ((new-chunk (make-instance 'chunk
                                                 :kind kind
                                                 :lines lines)))
                   (push new-chunk consolidated)
                   (setf list rest))))))

(defmethod read-diff-window (linegen (diff context-diff))
  (let ((window (create-window-for-diff diff))
        (original-chunks nil)
        (modified-chunks nil)
        (line (yield-line linegen)))
    ;; read the "original" part
    (unless line
      (return-from read-diff-window nil))
    (and (cl-ppcre:scan *context-diff-window-header* line)
         (setf line (yield-line linegen)))
    (unless (cl-ppcre:register-groups-bind ((#'parse-integer original-start
                                                             original-end))
                (*context-diff-window-original-line* line)
              (setf (original-start-line window) (1- original-start)
                    (original-length window) (1+ (- original-end
                                                    original-start))))
      (error "Non-matching original window header ~A" line))
    (loop for i from 0 below (original-length window)
          until (cl-ppcre:scan *context-diff-window-modified-line* (peek-line linegen))
          collect (line-to-chunk (yield-line linegen)
                                 *context-original-lead-char-alist*) into chunks
          finally (setf original-chunks chunks))
    ;; read the "modified" part
    (setf line (yield-line linegen))
    (unless (cl-ppcre:register-groups-bind ((#'parse-integer modified-start
                                                             modified-end))
                (*context-diff-window-modified-line* line)
              (setf (modified-start-line window) (1- modified-start)
                    (modified-length window) (1+ (- modified-end
                                                    modified-start))))
      (error "Non-matching modified window header ~A" line))
    (loop for i from 0 below (modified-length window)
          until (let ((maybe-line (peek-line linegen)))
                  (or (not maybe-line)  ; EOF
                      (cl-ppcre:scan *context-diff-window-header* maybe-line)))
          collect (line-to-chunk (yield-line linegen)
                                 *context-modified-lead-char-alist*) into chunks
          finally (setf modified-chunks chunks))
    (setf (window-chunks window)
          (merge-context-chunks (consolidate-chunks original-chunks)
                                (consolidate-chunks modified-chunks)))
    window))

(defun merge-context-chunks (original modified)
  (do ((original original)
       (modified modified)
       (merged-chunks nil))
      ((and (null original) (null modified))
       (nreverse merged-chunks))
    (cond
      ((null original)
       (push (pop modified) merged-chunks))
      ((null modified)
       (push (pop original) merged-chunks))
      (t
       (let ((orig-kind (chunk-kind (first original)))
             (mod-kind (chunk-kind (first modified))))
         (cond
           ((and (eq orig-kind :common) (eq mod-kind :common))
            (push (pop original) merged-chunks)
            (pop modified))
           ((eq orig-kind :common)
            (push (pop modified) merged-chunks))
           ((eq mod-kind :common)
            (push (pop original) merged-chunks))
           (t
            (push (pop original) merged-chunks)
            (push (pop modified) merged-chunks))))))))

;;; reading patches

(defclass patch ()
  ((diff :initarg :diff :reader diff)
   (index-file :initarg :index-file :reader index-file)
   (prereq-string :initarg :prereq-string :reader prereq-string)))

(defun read-patch (linegen)
  (let ((diff nil)
        (index-filename nil)
        (prereq-string nil)
        (line ""))
    (tagbody
     UNKNOWN-STATE
       (setf line (yield-line linegen))
       (unless line
         (go RETURN-VALUE))
       (cond
         ((cl-ppcre:register-groups-bind (original-pathname)
              (*unified-diff-header-original-line* line)
            (setf diff (make-instance 'unified-diff))
            (setf (original-pathname diff) original-pathname))
          (go UNIFIED-MODIFIED-LINE))
         ((cl-ppcre:register-groups-bind (original-pathname)
              (*context-diff-header-original-line* line)
            (setf diff (make-instance 'context-diff))
            (setf (original-pathname diff) original-pathname))
          (go CONTEXT-MODIFIED-LINE))
         ((cl-ppcre:register-groups-bind (index-pathname)
              (*index-header-line* line)
            (setf index-filename index-pathname))
          (go UNKNOWN-STATE))
         ((cl-ppcre:register-groups-bind (prereq)
              (*prereq-header-line* line)
            (setf prereq-string prereq))
          (go UNKNOWN-STATE))
         (t
          (go UNKNOWN-STATE)))
     UNIFIED-MODIFIED-LINE
       (setf line (yield-line linegen))
       (unless line
         (go RETURN-VALUE))
       (cond
         ((cl-ppcre:register-groups-bind (modified-pathname)
              (*unified-diff-header-modified-line* line)
            (setf (modified-pathname diff) modified-pathname))
          (go WINDOW-STATE))
         (t
          (error "Could not read unified modified-line")))
     CONTEXT-MODIFIED-LINE
       (setf line (yield-line linegen))
       (unless line
         (go RETURN-VALUE))
       (cond
         ((cl-ppcre:register-groups-bind (modified-pathname)
              (*context-diff-header-modified-line* line)
            (setf (modified-pathname diff) modified-pathname))
          (go WINDOW-STATE))
         (t
          (error "Could not read context modified-line")))
     WINDOW-STATE
       (loop for window = (read-diff-window linegen diff)
             while window
             collect window into windows
             finally (progn
                       (setf (diff-windows diff) windows)
                       (go RETURN-VALUE)))
       RETURN-VALUE
       (return-from read-patch (values diff index-filename prereq-string)))))

(defun read-patches-from-file (pathname)
  (let ((patches nil))
    (with-open-file (stream pathname :direction :input
                     :if-does-not-exist :error)
      (let ((linegen (make-line-generator stream)))
        (loop
          (multiple-value-bind (diff index prereq)
              (read-patch linegen)
            (unless diff
              (return-from read-patches-from-file (nreverse patches)))
            (push (make-instance 'patch
                                 :diff diff
                                 :index-file (and index (pathname index))
                                 :prereq-string prereq)
                  patches)))))))

(defun apply-seq-patch (original-seq patch)
  "Apply PATCH to the sequence ORIGINAL-SEQ."
  (apply-seq-diff original-seq (diff patch)))

(defun apply-patch (patch &aux original)
  "Apply PATCH."
  (do-file-lines (line (original-pathname (diff patch))) (push line original))
  (with-open-file (out (original-pathname (diff patch))
                       :direction :output :if-exists :supersede)
    (format out "~{~a~^~%~}~%" (apply-seq-patch (nreverse original) patch))))
