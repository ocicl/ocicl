(in-package :mgl-pax-test)

(deftest test-read-prefixed-lines ()
  (is (equal (multiple-value-list
              (with-input-from-string (stream (format nil ">1"))
                (mgl-pax::read-prefixed-lines stream ">")))
             '("1" 1 nil t 2)))
  (is (equal (multiple-value-list
              (with-input-from-string (stream (format nil ">1~%"))
                (mgl-pax::read-prefixed-lines stream ">")))
             '("1" 1 nil t 3)))
  (is (equal (multiple-value-list
              (with-input-from-string (stream (format nil ">1~%>"))
                (mgl-pax::read-prefixed-lines stream ">")))
             `(,(format nil "1~%") 2 nil t 4)))
  (is (equal (multiple-value-list
              (with-input-from-string (stream (format nil ">1~%>2~%> 3"))
                (mgl-pax::read-prefixed-lines stream ">")))
             `(,(format nil "1~%2~%3") 3 nil t 9)))
  (is (equal (multiple-value-list
              (with-input-from-string (stream
                                       (format nil ">1~%>2~%> 3~%xy~%"))
                (mgl-pax::read-prefixed-lines stream ">")))
             `(,(format nil "1~%2~%3") 3 "xy" nil 10)))
  (is (equal (multiple-value-list
              (with-input-from-string (stream (format nil ">1~%>2~%> 3~%xy"))
                (mgl-pax::read-prefixed-lines stream ">")))
             `(,(format nil "1~%2~%3") 3 "xy" t 10)))
  (is (equal (multiple-value-list
              (with-input-from-string (stream (format nil ">1"))
                (mgl-pax::read-prefixed-lines stream ">"
                                              :first-line-prefix "")))
             '(">1" 1 nil t 2)))
  (is (equal (multiple-value-list
              (with-input-from-string (stream (format nil ">1~%>2~%> 3"))
                (mgl-pax::read-prefixed-lines stream ">"
                                              :first-line-prefix "")))
             `(,(format nil ">1~%2~%3") 3 nil t 9)))
  (is (equal (multiple-value-list
              (with-input-from-string (stream (format nil ">1~%>2~%> 3~%xy~%"))
                (mgl-pax::read-prefixed-lines stream ">"
                                              :first-line-prefix "")))
             `(,(format nil ">1~%2~%3") 3 "xy" nil 10)))
  (is (equal (multiple-value-list
              (with-input-from-string (stream (format nil ">1~%>2~%> 3~%xy"))
                (mgl-pax::read-prefixed-lines stream ">"
                                              :first-line-prefix "")))
             `(,(format nil ">1~%2~%3") 3 "xy" t 10))))

(defclass bbb ()
  ())

(defmethod print-object ((bbb bbb) stream)
  ;; The exact output of PRINT-UNREADABLE-OBJECT is implementation dependent.
  (format stream "#<BBB>"))

(defclass bbb* ()
  ())

(defmethod print-object ((bbb bbb*) stream)
  (format stream "#<BBB*~%>"))

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
    (:input "(make-instance 'bbb)~%==> #<BBB>"
     :transcript ((((make-instance 'bbb) "(make-instance 'bbb)")
                   :default
                   (:unreadable "#<BBB>")))
     :output "(make-instance 'bbb)~%==> #<BBB>~%"
     :update-only t)
    (:input "(make-instance 'bbb*)~%==> #<BBB*~%--> >"
     :transcript ((((make-instance 'bbb*) "(make-instance 'bbb*)")
                   :default
                   (:unreadable "#<BBB*~%>")))
     :output "(make-instance 'bbb*)~%==> #<BBB*~%--> >~%"
     :update-only t)
    (:input "(make-instance 'bbb)~%;==> #<BBB>"
     :transcript ((((make-instance 'bbb) "(make-instance 'bbb)")
                   :commented-1
                   (:unreadable "#<BBB>")))
     :output "(make-instance 'bbb)~%;==> #<BBB>~%"
     :update-only t)
    (:input "(make-instance 'bbb*)~%;==> #<BBB*~%;--> >"
     :transcript ((((make-instance 'bbb*) "(make-instance 'bbb*)")
                   :commented-1
                   (:unreadable "#<BBB*~%>")))
     :output "(make-instance 'bbb*)~%;==> #<BBB*~%;--> >~%"
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
    (:input "(make-instance 'bbb)~%==> #<CCC>~%"
     :output "(make-instance 'bbb)~%==> #<BBB>~%"
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
    ;; mixed syntax (the error message makes it sbcl specific)
    #+sbcl
    (:input "(princ 1)~%;.. 1~%=> 1~%"
     :output "(princ 1)~%~
              ;.. 1~%~
              ;=> 1~%~
              =>~%~
              ;.. debugger invoked on UNBOUND-VARIABLE:~%~
              ;..   The variable => is unbound.~%~
              1~%~
              ;=> 1~%")
    ;; REPL variables -
    (:input "(princ -)~%"
     :output "(princ -)~%~
              .. (PRINC -)~%~
              => (PRINC -)~%")
    ;; REPL variables *, **, ***
    (:input "1~%(1+ *)~%(+ 2 **)~%(+ 3 ***)~%"
     :output "1~%~
              => 1~%~
              (1+ *)~%~
              => 2~%~
              (+ 2 **)~%~
              => 3~%~
              (+ 3 ***)~%~
              => 4~%")
    ;; REPL variables /, //, ///
    (:input "(values 1 0)~%/~%//~%///~%"
     :output "(values 1 0)~%~
              => 1~%~
              => 0~%~
              /~%~
              => (1 0)~%~
              //~%~
              => (1 0)~%~
              ///~%~
              => (1 0)~%")
    ;; REPL variables +, ++, +++
    (:input "+~%+~%2~%+~%++~%+++~%"
     :output "+~%~
              => NIL~%~
              +~%~
              => +~%~
              2~%~
              => 2~%~
              +~%~
              => 2~%~
              ++~%~
              => 2~%~
              +++~%~
              => 2~%")))

(defun call-format-on-strings (tree)
  (mgl-pax::transform-tree (lambda (parent node)
                             (declare (ignore parent))
                             (if (stringp node)
                                 (values (format nil node) nil nil)
                                 (values node t nil)))
                           tree))

(deftest test-read-write-transcript ()
  (let ((*package* (find-package :mgl-pax-test)))
    (loop for test-case in *transcribe-test-cases* do
      (with-test (nil :name test-case)
        (destructuring-bind (&key input transcript output check-consistency
                             update-only (include-no-output update-only)
                             (include-no-value update-only)
                             default-syntax errors output-consistency-errors
                             values-consistency-errors)
            test-case
          (let ((output-consistency-errors* ())
                (values-consistency-errors* ())
                (errors* ()))
            (catch 'here
              (handler-bind
                  ((transcription-output-consistency-error
                     (lambda (e)
                       (push (mgl-pax::transcription-error-file-position e)
                             output-consistency-errors*)
                       (continue)))
                   (transcription-values-consistency-error
                     (lambda (e)
                       (push (mgl-pax::transcription-error-file-position e)
                             values-consistency-errors*)
                       (continue)))
                   (transcription-error
                     (lambda (e)
                       (push (mgl-pax::transcription-error-file-position e)
                             errors*)
                       (throw 'here nil))))
                (let* ((input (when input (format nil input)))
                       (output (when output (format nil output)))
                       (transcript (call-format-on-strings transcript))
                       (transcript* (mgl-pax::read-transcript input))
                       (output*
                         (mgl-pax::write-transcript
                          transcript* nil
                          :check-consistency check-consistency
                          :update-only update-only
                          :include-no-output include-no-output
                          :include-no-value include-no-value
                          :default-syntax default-syntax)))
                  (when transcript
                    (is (equal transcript transcript*)))
                  (when output
                    (is (null (mismatch% output output*)))))))
            (is (equal (reverse errors*) errors))
            (is (equal (reverse output-consistency-errors*)
                       output-consistency-errors))
            (is (equal (reverse values-consistency-errors*)
                       values-consistency-errors))))))))

(defparameter *transcribe-source-file*
  (asdf:system-relative-pathname
   :mgl-pax "test/data/transcribe-source.lisp"))

(defparameter *transcribe-transcription-file*
  (asdf:system-relative-pathname
   :mgl-pax "test/data/transcribe-transcription.lisp"))

(deftest test-transcribe-from-source ()
  (let ((*package* (find-package :mgl-pax-test)))
    (check-transcription *transcribe-source-file*
                         *transcribe-transcription-file*
                         :check-consistency nil)))

;;; Check that repeated transcription produces the same results.
(deftest test-transcribe-stability ()
  (let ((*package* (find-package :mgl-pax-test)))
    (check-transcription *transcribe-transcription-file*
                         *transcribe-transcription-file*
                         :check-consistency t)))

(defun check-transcription (source-file transcription-file
                            &key check-consistency)
  (unless (string= (alexandria:read-file-into-string transcription-file)
                   (with-output-to-string (transcription)
                     (with-open-file (source source-file)
                       (transcribe source transcription :update-only t
                                   :check-consistency check-consistency))))
    (cerror "Update transcription file." "Transcription differs from ~S."
            transcription-file)
    (with-input-from-string (source (alexandria:read-file-into-string
                                     source-file))
      (with-open-file (transcription transcription-file :direction :output
                                     :if-exists :rename-and-delete)
        (transcribe source transcription :update-only t
                    :check-consistency check-consistency)))))

(deftest test-transcribe ()
  (test-read-prefixed-lines)
  (test-read-write-transcript)
  (with-test ("*read-eval* nil")
    (let ((*read-eval* nil))
      (signals-not (error)
        (is (equal (pax:transcribe "#.(1+ 2)" nil)
                   "#.(1+ 2)
=> 3
")))))
  (test-transcribe-from-source)
  (test-transcribe-stability))
