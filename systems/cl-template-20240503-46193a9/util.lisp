(in-package #:cl-template)

(defun scan-string-until-ignoring (string terminator &key ignore-list (start 0) end)
  "Scan a string until a substring is encountered, ignoring anything
  between ignore-list pairs. Probably not super-useful outside of
  CLT. In CLT it is used to scan until the next end delimiter while
  ignoring things in quotes.
  Example:
    (scan-string-until-ignoring \"abc|def\" \"|\")  ; \"abc\"
    (scan-string-until-ignoring \"abc <ignor|ing> def\" \"|\" :ignore-list '((#\< . #\>)))  ; \"abc <ignor|ing> def\"
  "
  ;; This works as sort of a poor man's FSM. The states are:
  ;; ignore state: we are between some characters where the `terminator` should be ignored if we come across it (e.g. it's in quotes).
  ;; terminator state: we are scanning characters that match the `terminator` string.
  ;; regular state: nothing is happening, simply push the current character onto the result.
  
  ;; The variable names in this section are a little weird.
  ;; `string-section` is the part of the string we are actually going to scan (between `start` and `end`).
  ;; `result` is the string we are going to return, a substring of `string-section` starting at 0 and ending at wherever we
  ;; hit the `terminator` string.
  ;; `in-ignore` is the first character of an ignore pair (e.g. #\" . #\" or #\< . #\>) or nil if we aren't in the ignore state.
  ;; `terminator-index` is the index in the `terminator` string that we are at currently, or nil if we aren't in the terminator
  ;; state (i.e. we aren't scanning the `terminator` string.
  ;; `terminator-progress` is the characters we've scanned that are in the `terminator` string, so that if we ever have a mismatch
  ;; with the `terminator` string we can push those characters back onto the result.
  (let ((string-section (subseq string start end))
        (result (make-array 1 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop
       with in-ignore = nil
       with terminator-index = nil
       with terminator-progress = (make-array 1 :element-type 'character :adjustable t :fill-pointer 0)
       for char across string-section do
         (cond
           ;; Not ignoring, but hit an ignore character. Enter ignore state.
           ((and (not in-ignore) (loop for (c . match) in ignore-list thereis (char= c char)))
            (setf in-ignore char)
            (vector-push-extend char result))
           ;; Ignoring and hit the end ignore character. Enter regular state.
           ((and in-ignore (char= char (cdr (assoc in-ignore ignore-list))))
            (setf in-ignore nil)
            (vector-push-extend char result))
           ;; Not in the terminator state or the ignore state, but hit the first character of the terminator. Enter terminator state.
           ((and (not in-ignore) (not terminator-index) (char= char (char terminator 0)))
            ;; If `terminator` is only 1 character long, return immediately.
            (if (= (length terminator) 1)
                (return-from scan-string-until-ignoring result))
            (setf terminator-index 1)
            (vector-push-extend char terminator-progress))
           ;; In terminator state and a character didn't match the terminator. Enter regular state and push the progress back onto the result.
           ((and terminator-index (char/= char (char terminator terminator-index)))
            (setf terminator-index nil)
            (loop for i below (length terminator-progress) do (vector-push-extend (aref terminator-progress i) result))
            (setf terminator-progress (make-array 1 :element-type 'character :adjustable t :fill-pointer 0))
            (vector-push-extend char result))
           ;; In terminator state and the character matches. Push the character onto the terminator progress.
           ((and terminator-index (char= char (char terminator terminator-index)))
            (vector-push-extend char terminator-progress)
            ;; Hit the whole terminator. Return the string until the terminator started.
            (if (= (length terminator-progress) (length terminator))
                (return-from scan-string-until-ignoring result))
            (incf terminator-index))
           ;; Regular state.
           (t
            (vector-push-extend char result))))
    result))

(defun scan-between-delimiters (string start-delimiter end-delimiter &key ignore-list (start 0) end)
  "Scan a string and return a substring between the two delimiter substrings.
  Example:
    (scan-between-delimiters \"abc <%= thingy %> def\" \"<%=\" \"%>\")  ; \" thingy \"
    (scan-between-delimiters \"what{{ever stuff<ignor{{ing>{{now}}xyz\" :ignore-list '((#\< . #\>)) :start 10)  ; \"now\"
  "
  
  ;; This works a lot like #'scan-string-until-ignoring, but with slightly different states:
  ;; ignore state: we are between some characters where `start-delimiter` and `end-delimiter` should be ignored if they are encountered.
  ;; start state: we are scanning characters that match the `start-delimiter` string.
  ;; regular state: nothing is happening, increment `index`.
  (let ((string-section (subseq string start end)) (result (make-array 1 :element-type 'character :adjustable t :fill-pointer 0))
        (index 0))
    (loop
       with in-ignore = nil
       with start-index = nil
       for char across string-section do
         (cond
           ;; Not ignoring, but hit an ignore character. Enter ignore state.
           ((and (not in-ignore) (loop for (c . match) in ignore-list thereis (char= c char)))
            (setf in-ignore char))
           ;; Ignoring and hit the end ignore character. Enter regular state.
           ((and in-ignore (char= char (cdr (assoc in-ignore ignore-list))))
            (setf in-ignore nil))
           ;; Not in the start state or the ignore state, but the character is the first start delimiter character. Enter start state.
           ((and (not in-ignore) (not start-index) (char= char (char start-delimiter 0)))
            (setf start-index 1))
           ;; In start state, but the character doesn't match the next start delimiter character. Enter regular state.
           ((and start-index (char/= char (char start-delimiter start-index)))
            (setf start-index nil))
           ;; In start state and the next character matches. If that's the whole start delimiter, scan to the end delimiter and return.
           ((and start-index (char= char (char start-delimiter start-index)))
            (incf start-index)
            (when (= start-index (length start-delimiter))
              (let ((result (scan-string-until-ignoring string-section end-delimiter :ignore-list ignore-list :start (1+ index))))
                (return-from scan-between-delimiters (values result (+ index 1 (length end-delimiter) (length result))))))))
         (incf index))
    (values result index)))

(defun match-pairs-ignoring (string pair &key ignore-list (start 0) end)
  "Count the occurences of a pair (e.g. []) in a string, ignoring any that occur in between pairs of `ignore-list`.
  Example:
    (match-pairs-ignoring \"(something)\" '(#\( . #\)))  ; t
    (match-pairs-ignoring \"[that <thing]>\" '(#\[ . #\]) :ignore-list '((#\< . #\>)))  ; nil
  "
  ;; Another pseudo-FSM-thingy. TODO: Abstract some of this into a macro.
  ;; Only two states:
  ;; ignore state: scanning between characters where any occurrences of a member of the pair should be ignored.
  ;; regular state: if the first member of the pair is encountered, increment the count, if the second one is
  ;;                encountered, decrement the count.
  
  (let ((string-section (subseq string start end))
        (open-count 0))
    (loop
       with in-ignore = nil
       for char across string-section do
         (cond
           ((and (not in-ignore) (loop for (c . match) in ignore-list thereis (char= c char)))
            (setf in-ignore char))
           ((and in-ignore (char= char (cdr (assoc in-ignore ignore-list))))
            (setf in-ignore nil))
           ((and (not in-ignore) (char= char (car pair)))
            (incf open-count))
           ((and (not in-ignore) (char= char (cdr pair)))
            (decf open-count))))
    open-count))
