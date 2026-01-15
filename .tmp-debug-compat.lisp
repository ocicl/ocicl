;;; Debug cl+ssl-compat stream handling

(dolist (p '("/home/green/git/ocicl/systems/pure-tls-20260110-1cfce15/"
             "/home/green/git/ocicl/systems/ironclad-20251216-1ea8653/"
             "/home/green/git/ocicl/systems/bordeaux-threads-20240503-5c5ba68/"
             "/home/green/git/ocicl/systems/flexi-streams-20240503-0fd872a/"
             "/home/green/git/ocicl/systems/trivial-gray-streams-20240503-9c166c0/"
             "/home/green/git/ocicl/systems/usocket-0.8.8/"
             "/home/green/git/ocicl/systems/babel-20240503-627d58e/"
             "/home/green/git/ocicl/systems/trivial-features-20240503-1bc0b61/"
             "/home/green/git/ocicl/systems/alexandria-20240909-68a9f0a/"
             "/home/green/git/ocicl/systems/split-sequence-2.0.1/"
             "/home/green/git/ocicl/systems/idna-20240503-c4a9de8/"
             "/home/green/git/ocicl/systems/puri-20240503-f0a9f0c/"
             "/home/green/git/ocicl/systems/cl-base64-20240503-7a26786/"
             "/home/green/git/ocicl/systems/chipz-20240503-82a1704/"
             "/home/green/git/ocicl/systems/chunga-20240503-1e7f6ee/"
             "/home/green/git/ocicl/systems/cl-ppcre-20240909-d84be75/"
             "/home/green/git/ocicl/systems/drakma-2.0.9/"))
  (push (pathname p) asdf:*central-registry*))

;; Load pure-tls/cl+ssl-compat
(asdf:load-system :pure-tls/cl+ssl-compat)
(asdf:register-immutable-system "cl+ssl")

;; Add debug to make-ssl-client-stream
(let ((orig #'cl+ssl:make-ssl-client-stream))
  (defun cl+ssl:make-ssl-client-stream (socket &rest args)
    (format t "~&; [DEBUG] make-ssl-client-stream called~%")
    (format t "~&;   socket type: ~A~%" (type-of socket))
    (format t "~&;   socket value: ~A~%" socket)
    (format t "~&;   args: ~A~%" args)
    (force-output)
    (apply orig socket args)))

;; Add debug to stream-fd
(let ((orig #'cl+ssl:stream-fd))
  (defun cl+ssl:stream-fd (stream)
    (let ((result (funcall orig stream)))
      (format t "~&; [DEBUG] stream-fd called~%")
      (format t "~&;   input type: ~A~%" (type-of stream))
      (format t "~&;   result type: ~A~%" (type-of result))
      (format t "~&;   result value: ~A~%" result)
      (force-output)
      result)))

(asdf:load-system :drakma)

(format t "~&; Testing with debug output...~%")
(force-output)

(handler-case
    (multiple-value-bind (body status headers)
        (drakma:http-request "https://github.com" :method :head)
      (declare (ignore body headers))
      (format t "~&; Status: ~D~%" status))
  (error (e)
    (format t "~&; ERROR: ~A~%" e)
    (format t "~&; Error type: ~A~%" (type-of e))))

(format t "~&; Done.~%")
