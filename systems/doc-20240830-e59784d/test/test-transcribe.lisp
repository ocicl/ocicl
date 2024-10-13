(uiop:define-package #:40ants-doc-test/test-transcribe
  (:use #:cl)
  (:import-from #:alexandria)
  (:import-from #:40ants-doc-full/transcribe)
  (:import-from #:rove
                #:testing
                #:ok
                #:deftest)
  (:import-from #:40ants-doc-test/utils
                #:get-diff)
  (:import-from #:40ants-doc-full/utils
                #:read-prefixed-lines)
  (:import-from #:rove/core/test
                #:with-testing-with-options))
(in-package 40ants-doc-test/test-transcribe)


(deftest test-read-prefixed-lines
  (ok
   (equal (multiple-value-list
           (with-input-from-string (stream (format nil ">1"))
             (read-prefixed-lines stream ">")))
          '("1" 1 nil t 2)))
  (ok
   (equal (multiple-value-list
           (with-input-from-string (stream (format nil ">1~%"))
             (read-prefixed-lines stream ">")))
          '("1" 1 nil t 3)))
  (ok
   (equal (multiple-value-list
           (with-input-from-string (stream (format nil ">1~%>"))
             (read-prefixed-lines stream ">")))
          `(,(format nil "1~%") 2 nil t 4)))
  (ok
   (equal (multiple-value-list
           (with-input-from-string (stream (format nil ">1~%>2~%> 3"))
             (read-prefixed-lines stream ">")))
          `(,(format nil "1~%2~%3") 3 nil t 9)))
  (ok
   (equal (multiple-value-list
           (with-input-from-string (stream (format nil ">1~%>2~%> 3~%xy~%"))
             (read-prefixed-lines stream ">")))
          `(,(format nil "1~%2~%3") 3 "xy" nil 10)))
  (ok
   (equal (multiple-value-list
           (with-input-from-string (stream (format nil ">1~%>2~%> 3~%xy"))
             (read-prefixed-lines stream ">")))
          `(,(format nil "1~%2~%3") 3 "xy" t 10)))
  (ok
   (equal (multiple-value-list
           (with-input-from-string (stream (format nil ">1"))
             (read-prefixed-lines stream ">" :first-line-prefix "")))
          '(">1" 1 nil t 2)))
  (ok
   (equal (multiple-value-list
           (with-input-from-string (stream (format nil ">1~%>2~%> 3"))
             (read-prefixed-lines stream ">" :first-line-prefix "")))
          `(,(format nil ">1~%2~%3") 3 nil t 9)))
  (ok
   (equal (multiple-value-list
           (with-input-from-string (stream (format nil ">1~%>2~%> 3~%xy~%"))
             (read-prefixed-lines stream ">" :first-line-prefix "")))
          `(,(format nil ">1~%2~%3") 3 "xy" nil 10)))
  (ok
   (equal (multiple-value-list
           (with-input-from-string (stream (format nil ">1~%>2~%> 3~%xy"))
             (read-prefixed-lines stream ">" :first-line-prefix "")))
          `(,(format nil ">1~%2~%3") 3 "xy" t 10))))


(defclass bbb ()
  ())

(defmethod print-object ((bbb bbb) stream)
  #-ecl
  (print-unreadable-object (bbb stream :type t))
  ;; PRINT-UNREADABLE-OBJECT lower-cases the name of the class on ECL.
  #+ecl
  (format stream "#<BBB >"))

(defclass bbb* ()
  ())

(defmethod print-object ((bbb bbb*) stream)
  #-ecl
  (print-unreadable-object (bbb stream :type t)
    (format stream "~%"))
  #+ecl
  (format stream "#<BBB* ~%>"))


(defparameter *transcribe-test-cases*
  '((:input "1"
     :transcript (((1 "1") nil))
     :output "1~%"
     :update-only t)
    (:input ";; this~%   (+ 1~%      2)"
     :transcript ((((+ 1 2) ";; this~%   (+ 1~%      2)") nil))
     :output ";; this~%   (+ 1~%      2)~%"
     :update-only t)
    (:input "(+ 1 2)42"
     :transcript ((((+ 1 2) "(+ 1 2)") nil)
                  ((42 "42") nil))
     :output "(+ 1 2)~%42~%"
     :update-only t)
    (:input "(+ 1 2) 42"
     :transcript ((((+ 1 2) "(+ 1 2)") nil)
                  ((42 "42") nil))
     :output "(+ 1 2)~%42~%"
     :update-only t)
    (:input "(princ 1)~%.. 1"
     :transcript ((((princ 1) "(princ 1)")
                   :default
                   (:output "1")))
     :output "(princ 1)~%.. 1~%"
     :update-only t)
    (:input "(progn (princ 1) (print 2))~%.. 1~%.. 2 "
     :transcript ((((progn (princ 1) (print 2)) "(progn (princ 1) (print 2))")
                   :default
                   (:output "1~%2 ")))
     :output "(progn (princ 1) (print 2))~%.. 1~%.. 2 ~%"
     :update-only t)
    (:input "(princ 1)~%;.. 1"
     :transcript ((((princ 1) "(princ 1)") :commented-1
                   (:output "1")))
     :output "(princ 1)~%;.. 1~%"
     :update-only t)
    (:input "(progn (princ 1) (print 2))~%;.. 1~%;.. 2 "
     :transcript ((((progn (princ 1) (print 2)) "(progn (princ 1) (print 2))")
                   :commented-1
                   (:output "1~%2 ")))
     :output "(progn (princ 1) (print 2))~%;.. 1~%;.. 2 ~%"
     :update-only t)
    (:input "1~%=> 1"
     :transcript (((1 "1") :default (:readable (1 "1"))))
     :output "1~%=> 1~%"
     :update-only t)
    (:input "(list 1 2)~%=> ;; this~%   (1~%    2)"
     :transcript ((((list 1 2) "(list 1 2)") :default
                   (:readable ((1 2) ";; this~%   (1~%    2)"))))
     :output "(list 1 2)~%=> ;; this~%   (1~%    2)~%"
     :update-only t)
    (:input "(values)~%=> ; No value"
     :transcript ((((values) "(values)") :default (:no-value nil)))
     :output "(values)~%=> ; No value~%"
     :update-only t)
    (:input "1~%;=> 1"
     :transcript (((1 "1") :commented-1 (:readable (1 "1"))))
     :output "1~%;=> 1~%"
     :update-only t)
    (:input "(list 1 2)~%;=> ;; this~%;-> (1~%;->  2)"
     :transcript ((((list 1 2) "(list 1 2)")
                   :commented-1
                   (:readable ((1 2) ";; this~%(1~% 2)"))))
     :output "(list 1 2)~%;=> ;; this~%;-> (1~%;->  2)~%"
     :update-only t)
    (:input "(make-instance 'bbb)~%==> #<BBB >"
     :transcript ((((make-instance 'bbb) "(make-instance 'bbb)")
                   :default
                   (:unreadable "#<BBB >")))
     :output "(make-instance 'bbb)~%==> #<BBB >~%"
     :update-only t)
    (:input "(make-instance 'bbb*)~%==> #<BBB* ~%--> >"
     :transcript ((((make-instance 'bbb*) "(make-instance 'bbb*)")
                   :default
                   (:unreadable "#<BBB* ~%>")))
     :output "(make-instance 'bbb*)~%==> #<BBB* ~%--> >~%"
     :update-only t)
    (:input "(make-instance 'bbb)~%;==> #<BBB >"
     :transcript ((((make-instance 'bbb) "(make-instance 'bbb)")
                   :commented-1
                   (:unreadable "#<BBB >")))
     :output "(make-instance 'bbb)~%;==> #<BBB >~%"
     :update-only t)
    (:input "(make-instance 'bbb*)~%;==> #<BBB* ~%;--> >"
     :transcript ((((make-instance 'bbb*) "(make-instance 'bbb*)")
                   :commented-1
                   (:unreadable "#<BBB* ~%>")))
     :output "(make-instance 'bbb*)~%;==> #<BBB* ~%;--> >~%"
     :update-only t)
    (:input "42"
     :output "42~%=> 42~%")
    (:input "42~%"
     :output "42~%=> 42~%")
    ;; The spaces are discarded.
    (:input "42  ~%"
     :output "42~%=> 42~%")
    ;; No preceeding "==>" error.
    (:input "42~%-->"
     :output ""
     :errors (3))
    ;; No-value marker after values.
    (:input "42~%=> 42~%=> ; No value"
     :output ""
     :errors (9))
    ;; No whitespace after form.
    (:input "42;comment"
     :output "42~%=> 42~%;comment")
    ;; Unexpected eof on non-empty value line.
    (:input "42~%=> ;"
     :output ""
     :errors (3))
    ;; Multiple forms on the same line.
    (:input "(list)(values 1 2)"
     :output "(list)~%=> NIL~%(values 1 2)~%=> 1~%=> 2~%")
    ;; Multiple forms on the same line with space.
    (:input "(list) (values 1 2)"
     :output "(list)~%=> NIL~%(values 1 2)~%=> 1~%=> 2~%")
    ;; No value included.
    (:input "(values)"
     :output "(values)~%=> ; No value~%"
     :include-no-value t)
    ;; No value not included.
    (:input "(values)"
     :output "(values)~%")
    ;; No output included.
    (:input "(values)"
     :output "(values)~%..~%"
     :include-no-output t)
    ;; No output not included.
    (:input "(values)"
     :output "(values)~%")
    ;; Capturing *ERROR-OUTPUT*.
    (:input "(princ 42 *error-output*)"
     :output "(princ 42 *error-output*)~%.. 42~%=> 42~%")
    ;; Capturing *TRACE-OUTPUT*.
    (:input "(princ 42 *trace-output*)"
     :output "(princ 42 *trace-output*)~%.. 42~%=> 42~%")
    ;; Capturing *DEBUG-IO*.
    (:input "(princ 42 *debug-io*)"
     :output "(princ 42 *debug-io*)~%.. 42~%=> 42~%")
    ;; Capturing *QUERY-IO*.
    (:input "(princ 42 *query-io*)"
     :output "(princ 42 *query-io*)~%.. 42~%=> 42~%")
    ;; Capturing *TERMINAL-IO*.
    (:input "(princ 42 *terminal-io*)"
     :output "(princ 42 *terminal-io*)~%.. 42~%=> 42~%")
    ;; output inconsistency
    (:input "(princ 42)~%.. 7~%=> 42~%"
     :output "(princ 42)~%.. 42~%=> 42~%"
     :check-consistency t :output-consistency-errors (nil))
    ;; readable value inconsistency
    (:input "(princ 42)~%.. 42~%=> 7~%"
     :output "(princ 42)~%.. 42~%=> 42~%"
     :check-consistency t
     :values-consistency-errors (nil))
    ;; unreadable value inconsistency
    (:input "(make-instance 'bbb)~%==> #<CCC >~%"
     :output "(make-instance 'bbb)~%==> #<BBB >~%"
     :check-consistency t :values-consistency-errors (nil))
    ;; commenting of new values with update 1
    (:input "(values 1 2)~%=> 1"
     :output "(values 1 2)~%=> 1~%=> 2~%"
     :update-only t)
    ;; commenting of new values with update 2
    (:input "(values 1 2)~%;=> 1"
     :output "(values 1 2)~%;=> 1~%;=> 2~%"
     :update-only t)
    ;; commenting of new output
    (:input "(princ 1)~%;=> 1"
     :output "(princ 1)~%;.. 1~%;=> 1~%")
    ;; commenting of new output in syntax
    (:input "(princ 1)~%;=> 1"
     :output "(princ 1)~%;;.. 1~%;;=> 1~%"
     :default-syntax :commented-2)
    ;; eof in form
    (:input "(sdf"
     :errors (0))
    ;; eof in readable value 1
    (:input "(list 1 2)~%=> (1"
     :errors (11))
    ;; eof in commented readable value 1
    (:input "(list 1 2)~%;=> (1~%;-> 2"
     :errors (11))
    ;; missing readable value
    (:input "(list 1 2)~%=>"
     :output "(list 1 2)~%=> (1 2)~%")
    ;; missing commented readable value
    (:input "(list 1 2)~%;=>"
     :output "(list 1 2)~%;=> (1 2)~%")
    ;; mixed syntax
    (:input "(princ 1)~%;.. 1~%=> 1~%"
     :errors (nil))))

(defun call-format-on-strings (tree)
  (40ants-doc-full/utils::transform-tree (lambda (parent node)
                                           (declare (ignore parent))
                                           (if (stringp node)
                                               (values (format nil node) nil nil)
                                               (values node t nil)))
                                         tree))


(defun apply-transcript (input &key check-consistency
                                 update-only
                                 include-no-output
                                 include-no-value
                                 default-syntax)
  (let* ((input (call-format-on-strings input))
         (transcript* (40ants-doc-full/transcribe::read-transcript input))
         (output*
           (40ants-doc-full/transcribe::write-transcript
            transcript* nil
            :check-consistency check-consistency
            :update-only update-only
            :include-no-output include-no-output
            :include-no-value include-no-value
            :default-syntax default-syntax)))
    (values output*
            transcript*)))


(deftest test-read-write-transcript
  (loop with *package* = (find-package :40ants-doc-test/test-transcribe)
        for test-case in *transcribe-test-cases*
        for test-id upfrom 1
        do (with-testing-with-options
               (format nil "test case ~A: ~S" test-id test-case)
               (:name (getf test-case :input))
               
             (destructuring-bind (&key input
                                    transcript
                                    output
                                    check-consistency
                                    update-only
                                    (include-no-output update-only)
                                    (include-no-value update-only)
                                    default-syntax
                                    errors
                                    output-consistency-errors
                                    values-consistency-errors)
                 test-case
               (let ((output-consistency-errors* ())
                     (values-consistency-errors* ())
                     (errors* ()))
                 (catch 'here
                   (handler-bind
                       ((40ants-doc-full/transcribe::transcription-output-consistency-error
                          (lambda (e)
                            (push (40ants-doc-full/transcribe::transcription-error-file-position e)
                                  output-consistency-errors*)
                            (continue)))
                        (40ants-doc-full/transcribe::transcription-values-consistency-error
                          (lambda (e)
                            (push (40ants-doc-full/transcribe::transcription-error-file-position e)
                                  values-consistency-errors*)
                            (continue)))
                        (40ants-doc-full/transcribe::transcription-error
                          (lambda (e)
                            (push (40ants-doc-full/transcribe::transcription-error-file-position e)
                                  errors*)
                            (throw 'here nil))))
                     (multiple-value-bind (output*
                                           transcript*)
                         (apply-transcript input
                                           :check-consistency check-consistency
                                           :update-only update-only
                                           :include-no-output include-no-output
                                           :include-no-value include-no-value
                                           :default-syntax  default-syntax)
                       (when transcript
                         (let ((expected-transcript (call-format-on-strings transcript)))
                           (ok (equal transcript*
                                      expected-transcript))))
                       (when output
                         (let ((expected-output (call-format-on-strings output)))
                           (ok (equal output*
                                      expected-output)))))))
                 (ok (equal (reverse errors*)
                            errors))
                 (ok (equal (reverse output-consistency-errors*)
                            output-consistency-errors))
                 (ok (equal (reverse values-consistency-errors*)
                            values-consistency-errors)))))))


(defparameter *transcribe-source-file*
  (asdf:system-relative-pathname
   :40ants-doc-test "test/data/baseline/transcribe-source.lisp"))

(defparameter *transcribe-transcription-file*
  (asdf:system-relative-pathname
   :40ants-doc-test "test/data/baseline/transcribe-transcription.lisp"))


(defun check-transcription (source-file transcription-file
                            &key check-consistency)
  "Check that repeated transcription produces the same results."
  (let ((result (with-output-to-string (transcription)
                  (with-open-file (source source-file)
                    (40ants-doc-full/transcribe::transcribe source
                                                            transcription
                                                            :update-only t
                                                            :check-consistency check-consistency)))))
    (unless (string= (alexandria:read-file-into-string transcription-file)
                     result)
      (cerror "Update transcription file."
              "Transcription differs from ~S:~2%~A"
              transcription-file
              (get-diff transcription-file
                        result))
      (alexandria:write-string-into-file result transcription-file
                                         :if-exists :rename-and-delete))))


(deftest test-transcribe-from-source
  (check-transcription *transcribe-source-file*
                       *transcribe-transcription-file*
                       :check-consistency nil))


(deftest test-transcribe-stability
  (check-transcription *transcribe-transcription-file*
                       *transcribe-transcription-file*
                       :check-consistency t))
