;;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Base:10; Package: JSON-TEST -*-
(in-package :json-test)
(in-suite json)

(test test-json-bind
  (json-bind (hello hi ciao) "{\"hello\":100,\"hi\":5}"
    (is (= hello 100))
    (is (= hi 5))
    (is-false ciao)))


(test test-json-bind-advanced
  (json-bind (hello-world
              sub-obj.property
              sub-obj.missing-property
              sub-obj.even-deeper-obj.some-stuff)
      "{\"helloWorld\":100,\"subObj\":{\"property\":20,\"evenDeeperObj\":{\"someStuff\":\"Guten Tag\"}}}"
    (is (= hello-world 100))
    (is (= sub-obj.property 20))
    (is-false sub-obj.missing-property)
    (is (string= sub-obj.even-deeper-obj.some-stuff "Guten Tag"))))

(test json-bind-in-bind-bug
  ;; A problem with json-bind. TODO: Fix it, but leave this testcase
  (let* ((input-json-rpc "{\"method\":\"rc\",\"id\":\"1\",\"params\":
[\"0\",{\"id\":\"pingId\",\"name\":\"ping\"},[],
[{\"name\":\"tableTennisGroupName\",\"id\":\"tableTennisGroupId\"}]]}")
         (input1 (copy-seq input-json-rpc))
         (input2 (copy-seq input-json-rpc))
         result1 result2 temp)
    (flet ((invoke-json-rpc (struct)
             (json:json-bind (method id params)
                 struct
               (format nil "{\"result\":\"~a\",\"error\":null,\"id\":\"1\"}"
                       params))))
      ;; This does not work correctly
      (json:json-bind (result error id)
          (invoke-json-rpc input1)
        (setf result1 result))
      ;; But this works
      (setf temp (invoke-json-rpc input2))
      (json:json-bind (result error id)
          temp
        (setf result2 result))
      ;; and to prove it:
      (is (string= input1 input2)) ;; We have same input
      (is (string= result1 result2))))) ;; so we should get same output


;;; Invalidated by the patch ``Re-implemented JSON-BIND (to illustrate
;;; dynamic customization).'' (Wed Jan 21 20:49:22 MSK 2009)
; 
; (test test-json-bind-with-alist
;   (with-decoder-simple-list-semantics
;     (let ((the-alist (decode-json-from-string "{\"hello\":100,\"hi\":5}")))
;       (json-bind (hello hi ciao) the-alist
;         (is (= hello 100))
;         (is (= hi 5))
;         (is-false ciao)))))
; 
; (test assoc-lookup
;   (is (equalp '(json::cdas widget-id (json::cdas parent data))
;               (macroexpand-1 '(json::assoc-lookup parent widget-id data)))))



;; JSON-RPC

;; Old syntax:
;;    (defun-json-rpc foo (x y)
;;        "Adds two numbers"
;;      (+ x y))

(defun-json-rpc foo-g :guessing (x y)
     "Adds two numbers, returns number and string"
     (let ((val (+ x y)))
       `((:digits . ,val)
         (:letters . ,(format nil "~R" val)))))

(defun-json-rpc foo-e :explicit (x y)
     "Adds two numbers, returns number and string"
     (let ((val (+ x y)))
       `(:object :digits  ,val 
                 :letters ,(format nil "~R" val))))

(defun-json-rpc foo-s :streaming (x y)
     "Adds two numbers, returns number and string"
     (let ((val (+ x y)))
       (format nil "{\"digits\":~a,\"letters\":\"~R\"}" val val)))

(defun-json-rpc foo-b :boolean (x)
  "Takes a lisp value and just passes it through identity;
we can use this to probe the behavior of the boolean
encoding."
  (if (equalp x "error")
      (error "Intentionally raised error.")
      x))

(defun-json-rpc foo-a :array (x)
  "Takes a lisp value and just passes it through identity;
we can use this to probe the behavior of the array
encoding."
  (if (equalp x "error")
      (error "Intentionally raised error.")
      x))

(defun json-rpc-2-p (&optional (flag json-rpc:*json-rpc-version*))
  (eql (aref flag 0) #\2))

(defun test-json-rpc-helper (method-name)
  (with-decoder-simple-list-semantics
    (let (result)
      (setf result (json-rpc:invoke-rpc
                    (format nil "{~@[~*\"jsonrpc\":\"2.0\",~]\"method\":\"~a\",\"params\":[1,2],\"id\":999}" (json-rpc-2-p) method-name)))
      (is (string= result 
                   (format nil "{~@[~*\"jsonrpc\":\"2.0\",~]\"result\":{\"digits\":3,\"letters\":\"three\"},~@[~*\"error\":null,~]\"id\":999}" 
                           (json-rpc-2-p) (not (json-rpc-2-p))))))))

(defun test-json-rpc-boolean-helper (input-value output-value)
  (with-decoder-simple-list-semantics
    (let* ((error (eq output-value :error))
           (expected (cond ((eq output-value :error) 
                            nil)
                           (output-value "true")
                           (t "false")))
           (input-encoded 
            (with-guessing-encoder
              (encode-json-to-string input-value)))
           (result
            (handler-bind 
                ((json-rpc-call-error #'(lambda (x)
                                          (declare (ignore x))
                                          (invoke-restart 'json-rpc:send-internal-error))))
              (json-rpc:invoke-rpc
               (format nil 
                       "{~@[~*\"jsonrpc\":\"2.0\",~]\"method\":\"fooB\",\"params\":[~a],\"id\":999}" 
                       (json-rpc-2-p) input-encoded)))))
      (if error
          (let ((res (decode-json-from-string result)))
            (if (json-rpc-2-p)
                ;; if there's an error in 2.0, the result must be missing, not null
                (is (and (null (assoc :result res))
                         (cdr (assoc :error res))))
                (is (and (null (cdr (assoc :result res)))
                         (cdr (assoc :error res))))))
          (is (string= result 
                       (format nil "{~@[~*\"jsonrpc\":\"2.0\",~]\"result\":~a,~@[~*\"error\":null,~]\"id\":999}"
                               (json-rpc-2-p) expected
                               (not (json-rpc-2-p)))))))))

(defun test-json-rpc-array-helper (input-value output-value)
  (with-decoder-simple-list-semantics
    (let* ((error (eq output-value :error))
           (expected (cond ((eq output-value :error) 
                            nil)
                           (t 
                            (with-guessing-encoder 
                              (encode-json-to-string output-value)))))
           (input-encoded 
            (with-guessing-encoder
              (encode-json-to-string input-value)))
           (result
            (handler-bind 
                ((error #'(lambda (x)
                            (invoke-restart 'json-rpc:send-error
                                            (format nil "~a" x)
                                            999))))
              (json-rpc:invoke-rpc
               (format nil "{~@[~*\"jsonrpc\":\"2.0\",~]\"method\":\"fooA\",\"params\":[~a],\"id\":999}" 
                       (json-rpc-2-p) input-encoded)))))
      (if error
          (let ((res (decode-json-from-string result)))
            (is (and (null (cdr (assoc :result res)))
                     (cdr (assoc :error res)))))
          (is (string= result 
                       (format nil "{~@[~*\"jsonrpc\":\"2.0\",~]\"result\":~a,~@[~*\"error\":null,~]\"id\":999}"
                               (json-rpc-2-p) expected
                               (not (json-rpc-2-p)))))))))

(defmacro test-json-rpc-both (name &rest body)
  (let ((two-oh-name
         (intern (concatenate 'string (symbol-name name) "-"
                              (symbol-name '#:json-rpc-2.0)))))
  `(progn
     (test ,name 
       ,@body)
     (test ,two-oh-name
       (let ((json-rpc:*json-rpc-version* json-rpc:+json-rpc-2.0+))
         ,@body)))))

(test-json-rpc-both test-json-rpc
  (test-json-rpc-helper "fooG"))

(test-json-rpc-both test-json-rpc-explicit
  (test-json-rpc-helper "fooE"))

(test-json-rpc-both test-json-rpc-streaming
  (test-json-rpc-helper "fooS"))

(test-json-rpc-both test-json-rpc-boolean-true
  (test-json-rpc-boolean-helper t t))

(test-json-rpc-both test-json-rpc-boolean-false
  (test-json-rpc-boolean-helper nil nil))

(test-json-rpc-both test-json-rpc-boolean-error
  (test-json-rpc-boolean-helper :error :error))

(test-json-rpc-both test-json-rpc-array-simple
  (test-json-rpc-array-helper (list 1 2 3) (list 1 2 3)))

(test-json-rpc-both test-json-rpc-array-array
  (test-json-rpc-array-helper #(1 2 3) (list 1 2 3)))

(test-json-rpc-both test-json-rpc-empty-array
  (test-json-rpc-array-helper #() #()))

(test-json-rpc-both test-json-rpc-array-nil
  (test-json-rpc-array-helper nil #()))

(test-json-rpc-both test-json-rpc-array-scalar-error
  (test-json-rpc-array-helper 1 :error))

(test test-json-rpc-unknown-fn
  (with-decoder-simple-list-semantics
    (let ((*json-symbols-package* (find-package :json-test))
          result)
      (setf result (json-rpc:invoke-rpc "{\"method\":\"secretmethod\",\"params\":[1,2],\"id\":\"my id\"}"))
      (json-bind (result error id) result
        (is-false result)
        (is-true error)
        (is (string= id "my id"))))))
