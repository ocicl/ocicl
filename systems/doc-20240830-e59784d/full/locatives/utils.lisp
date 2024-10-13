(uiop:define-package #:40ants-doc-full/locatives/utils
  (:use #:cl)
  (:import-from #:str)
  (:import-from #:40ants-doc/locatives/base
                #:locative-type)
  (:import-from #:40ants-doc-full/commondoc/utils
                #:read-locative))
(in-package #:40ants-doc-full/locatives/utils)

;;; A utility for writing FIND-SOURCE methods. Try FILTER-STRINGS one
;;; by one, and if one matches exactly one of LOCATIONS, then return
;;; that location. Matching is performed by substring search on the
;;; stringified first element of the location.
(defun find-one-location (locations filter-strings)
  (let ((n-matches ()))
    (loop for filter-string in filter-strings
          do (let* ((filtered-locations
                      (filter-locations locations filter-string))
                    ;; LispWorks can return a multiple locations of the same kind
                    ;; when you first load system from quicklisp and then from other
                    ;; directory:
                    (filtered-locations (if (> (length filtered-locations) 1)
                                            (remove-if #'from-quicklisp-p
                                                       filtered-locations)
                                            filtered-locations)))
               (cond ((= 1 (length filtered-locations))
                      ;; A location looks like this in SBCL:
                      ;;
                      ;; ((DEFVAR *FOO*)
                      ;;  (:LOCATION
                      ;;   (:BUFFER-AND-FILE "pax.lisp"
                      ;;    "/home/mega/own/mgl/pax/src/pax.lisp")
                      ;;   (:OFFSET 106 0) (:SNIPPET "(defvar *foo*)")))
                      (return-from find-one-location
                        (second (first filtered-locations))))
                     (t
                      (push (length filtered-locations) n-matches)))))
    (error "~@<Could not find a single location in with filters ~S. ~
           Number of matches for each filter ~S.~:@>"
           filter-strings n-matches)))

(defun filter-locations (locations filter-string)
  (remove-if-not (lambda (location)
                   (let ((location-as-string
                           (prin1-to-string (first location))))
                     (search filter-string location-as-string
                             :test #'equalp)))
                 locations))


(defun from-quicklisp-p (location)
  (let ((filename (getf (getf (second location)
                              :location)
                        :file)))
    (str:containsp "quicklisp/dists"
                   filename)))


(defmethod locative-type ((locative string))
  (read-locative locative))
