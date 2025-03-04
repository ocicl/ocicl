(defpackage defclass-std-test
  (:use cl prove defclass-std))
(in-package defclass-std-test)

;; NOTE: To run this test file, execute `(asdf:test-system :defclass-std)' in your Lisp.

(plan 9)

(deftest class/std->defclass/std->defclass-expansion-test
  (is-expand (class/std stub slot1 slot2 slot3 slot4 slot5)
             (DEFCLASS/STD STUB ()
               ((SLOT1 SLOT2 SLOT3 SLOT4 SLOT5)))
             "CLASS/STD expands correctly into DEFCLASS/STD.")
  (is-expand (DEFCLASS/STD STUB ()
               ((SLOT1 SLOT2 SLOT3 SLOT4 SLOT5)))
             (DEFCLASS STUB ()
               ((SLOT1 :ACCESSOR SLOT1 :INITARG :SLOT1 :INITFORM NIL)
                (SLOT2 :ACCESSOR SLOT2 :INITARG :SLOT2 :INITFORM NIL)
                (SLOT3 :ACCESSOR SLOT3 :INITARG :SLOT3 :INITFORM NIL)
                (SLOT4 :ACCESSOR SLOT4 :INITARG :SLOT4 :INITFORM NIL)
                (SLOT5 :ACCESSOR SLOT5 :INITARG :SLOT5 :INITFORM NIL)))
             "DEFCLASS/STD generated by CLASS/STD expands as expected to DEFCLASS."))

(deftest class/std->defclass/std->defclass-with-args-expansion-test
  (is-expand (class/std new-stub var1 var2 var3 var4 :with :std :unbound)
             (DEFCLASS/STD NEW-STUB ()
               ((VAR1 VAR2 VAR3 VAR4 :WITH :STD :UNBOUND)))
             "CLASS/STD with :keyword options expand correctly into a DEFCLASS/STD form with the same :keyword options.")
  (is-expand (DEFCLASS/STD NEW-STUB ()
               ((VAR1 VAR2 VAR3 VAR4 :WITH :STD :UNBOUND)))
             (DEFCLASS NEW-STUB ()
               ((VAR1 :ACCESSOR NEW-STUB-VAR1 :INITARG :VAR1)
                (VAR2 :ACCESSOR NEW-STUB-VAR2 :INITARG :VAR2)
                (VAR3 :ACCESSOR NEW-STUB-VAR3 :INITARG :VAR3)
                (VAR4 :ACCESSOR NEW-STUB-VAR4 :INITARG :VAR4)))
             "DEFCLASS/STD with keyword options generated by CLASS/STD with :keyowrd options expands as expected to DEFCLASS."))

(deftest default-accessor-initarg
  (is-expand (DEFCLASS/STD STUB ()
               ((SLOT1 SLOT2 SLOT3 SLOT4 SLOT5)))
             (DEFCLASS STUB ()
               ((SLOT1 :ACCESSOR SLOT1 :INITARG :SLOT1 :INITFORM NIL)
                (SLOT2 :ACCESSOR SLOT2 :INITARG :SLOT2 :INITFORM NIL)
                (SLOT3 :ACCESSOR SLOT3 :INITARG :SLOT3 :INITFORM NIL)
                (SLOT4 :ACCESSOR SLOT4 :INITARG :SLOT4 :INITFORM NIL)
                (SLOT5 :ACCESSOR SLOT5 :INITARG :SLOT5 :INITFORM NIL)))
             "Defaults omitted args (:ai) works correctly."))

(deftest test-all-keyword-option
  (is-expand (defclass/std computer (gadget)
               ((screen mouse keyboard :a :type string :with)
                (bluetooth touchpad :wi :std :unbound)
                (speaker microphone :r)
                (place :@@ :with-prefix :doc "Where it is" :r)
                (owner :static :std "Me" :w)))
             (DEFCLASS COMPUTER (GADGET)
               ((SCREEN :ACCESSOR COMPUTER-SCREEN :INITFORM NIL :TYPE STRING)
                (MOUSE :ACCESSOR COMPUTER-MOUSE :INITFORM NIL :TYPE STRING)
                (KEYBOARD :ACCESSOR COMPUTER-KEYBOARD :INITFORM NIL :TYPE STRING)
                (BLUETOOTH :WRITER BLUETOOTH :INITARG :BLUETOOTH)
                (TOUCHPAD :WRITER TOUCHPAD :INITARG :TOUCHPAD)
                (SPEAKER :READER SPEAKER :INITFORM NIL)
                (MICROPHONE :READER MICROPHONE :INITFORM NIL)
                (PLACE :READER COMPUTER-PLACE :INITFORM NIL :ALLOCATION :CLASS
                       :DOCUMENTATION "Where it is")
                (OWNER :WRITER OWNER :INITFORM "Me" :ALLOCATION :CLASS)))))

(deftest test-*default-std*-binding
  (is-expand (defclass/std default ()
               ((with-std)))
             (DEFCLASS DEFAULT ()
               ((WITH-STD :ACCESSOR WITH-STD :INITARG :WITH-STD :INITFORM NIL)))
             "*DEFAULT-STD* defaults to T, adding :INITFORM NIL")
  (let (*default-std*)
    (is-expand (defclass/std default ()
               ((with-std)))
             (DEFCLASS DEFAULT ()
               ((WITH-STD :ACCESSOR WITH-STD :INITARG :WITH-STD)))
             "When bound to NIL, *DEFAULT-STD* changes the behaviour of DEFCLASS/STD correctly, avoidind the addition of :INITFORM NIL.")))

(deftest test-*with-prefix*-binding
  (is-expand (defclass/std prefix ()
               ((without-prefix)))
             (DEFCLASS PREFIX ()
               ((WITHOUT-PREFIX :ACCESSOR WITHOUT-PREFIX
                                :INITARG :WITHOUT-PREFIX
                                :INITFORM NIL)))
             "*WITH-PREFIX* defaults to NIL, avoiding the addition of the class name as a prefix to the accessor.")
  (let ((*with-prefix* t))
    (is-expand (defclass/std prefix ()
                 ((without-prefix :with)))
               (DEFCLASS PREFIX ()
                 ((WITHOUT-PREFIX :ACCESSOR PREFIX-WITHOUT-PREFIX
                                  :INITARG :WITHOUT-PREFIX
                                  :INITFORM NIL)))
               "When bound to T, *WITH-PREFIX* changes the behaviour of DEFCLASS/STD, add the class name as a prefix to the accessor.")))

(deftest test-ignore-unknown-keywords
  (is-expand (defclass/std unknown ()
               ((slot :unknown :keywords)))
             (DEFCLASS UNKNOWN ()
               ((SLOT :ACCESSOR SLOT
                      :INITARG :SLOT
                      :INITFORM NIL
                      :UNKNOWN :KEYWORDS)))
             "DEFCLASS/STD with unknown keywords/values pairs works as expected, keeping them as they are, when no other option is present.")
  (is-expand (defclass/std unknown ()
               ((slot :wi :unknown keywords :and values)))
             (DEFCLASS UNKNOWN ()
               ((SLOT :WRITER SLOT
                      :INITARG :SLOT
                      :INITFORM NIL
                      :UNKNOWN KEYWORDS
                      :AND VALUES)))
             "DEFCLASS/STD with unknown keywords/values pairs works as expected, keeping them as they are, when other options are present.")
  (is-expand (defclass/std unknown ()
               ((slot :unknown keywords :without-values)))
             (DEFCLASS UNKNOWN ()
               ((SLOT :ACCESSOR SLOT
                      :INITARG :SLOT
                      :INITFORM NIL
                      :UNKNOWN KEYWORDS
                      :WITHOUT-VALUES)))
             "DEFCLASS/STD with unknown keywords without values pairs works as expected, when no other option is present.")
  (is-expand (defclass/std unknown ()
               ((slot :a :unknown keywords :without-values)))
             (DEFCLASS UNKNOWN ()
               ((SLOT :ACCESSOR SLOT
                      :INITFORM NIL
                      :UNKNOWN KEYWORDS
                      :WITHOUT-VALUES)))
             "DEFCLASS/STD with unknown keywords without values pairs works as expected, when other options are present."))

(deftest printing-unreadably-form-expansion-test
  (is-expand (printing-unreadably (id name) (class/std employee name id salary))
             (progn
               (class/std employee
                 name
                 id
                 salary)
               (defmethod print-object ((employee employee) $stream)
                 (print-unreadable-object (employee $stream :type t :identity t)
                   (format $STREAM "ID: ~s, NAME: ~s"
                           (id employee) (name employee)))))))

(deftest repeated-keywords-errors-test
  (is-error (macroexpand-1
             '(defclass/std class1 ()
               ((field :wr))))
            'simple-error
            ":WR throws error.")
  (is-error (macroexpand-1
             '(defclass/std class2 ()
               ((field :wa))))
            'simple-error
            ":WA throws error.")
  (is-error (macroexpand-1
             '(defclass/std class3 ()
               ((field :ra))))
            'simple-error
            ":RA throws error."))

(run-test-all)
