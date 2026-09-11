;;; debug-jws.lisp --- Deep investigation of JWS signature format

(require :asdf)
(setf *compile-verbose* nil *compile-print* nil)
(push #p"/home/green/git/pure-tls/" asdf:*central-registry*)
(handler-bind ((warning (function muffle-warning)))
  (asdf:load-system :pure-tls/acme))

(in-package :pure-tls/acme)

(format t "~%=== JWS Signature Investigation ===~%~%")

;; Generate a key and examine signature in detail
(multiple-value-bind (priv pub)
    (ironclad:generate-key-pair :secp256r1)
  (declare (ignore pub))

  (format t "1. Examining signature format from ironclad:~%")
  (let* ((test-msg (flexi-streams:string-to-octets "test" :external-format :utf-8))
         (sig (ironclad:sign-message priv test-msg)))
    (format t "   Raw signature length: ~A bytes~%" (length sig))
    (format t "   First 32 bytes (R): ~{~2,'0X~}~%" (coerce (subseq sig 0 32) 'list))
    (format t "   Last 32 bytes (S): ~{~2,'0X~}~%" (coerce (subseq sig 32 64) 'list))

    ;; Check if R and S might need padding
    (let ((r-first (aref sig 0))
          (s-first (aref sig 32)))
      (format t "   R first byte: ~2,'0X (high bit: ~A)~%" r-first (if (>= r-first 128) "set" "clear"))
      (format t "   S first byte: ~2,'0X (high bit: ~A)~%" s-first (if (>= s-first 128) "set" "clear"))))

  (format t "~%2. Checking JWK format:~%")
  (let* ((jwk (get-public-key-jwk priv))
         (x-b64 (cdr (assoc "x" jwk :test #'string=)))
         (y-b64 (cdr (assoc "y" jwk :test #'string=))))
    (format t "   x (base64url): ~A~%" x-b64)
    (format t "   y (base64url): ~A~%" y-b64)
    (format t "   x length: ~A~%" (length (base64url-decode x-b64)))
    (format t "   y length: ~A~%" (length (base64url-decode y-b64))))

  (format t "~%3. Building actual ACME request to examine:~%")
  (let* ((jwk (get-public-key-jwk priv))
         (protected `(("alg" . "ES256")
                      ("jwk" . ,jwk)
                      ("nonce" . "test-nonce-12345")
                      ("url" . "https://localhost:14000/sign-me-up")))
         (payload `(("termsOfServiceAgreed" . t)
                    ("contact" . ("mailto:test@example.com"))))
         (protected-json (cl-json:encode-json-to-string protected))
         (payload-json (cl-json:encode-json-to-string payload))
         (protected64 (base64url-encode protected-json))
         (payload64 (base64url-encode payload-json))
         (signing-input (format nil "~A.~A" protected64 payload64)))

    (format t "   Protected JSON: ~A~%" protected-json)
    (format t "   Payload JSON: ~A~%" payload-json)
    (format t "   Protected base64url: ~A~%" protected64)
    (format t "   Payload base64url: ~A~%" payload64)
    (format t "   Signing input: ~A~%" signing-input)

    ;; Sign it
    (let* ((sig-bytes (ironclad:sign-message
                       priv
                       (flexi-streams:string-to-octets signing-input :external-format :utf-8)))
           (sig-b64 (base64url-encode sig-bytes)))
      (format t "~%4. Signature analysis:~%")
      (format t "   Signature (base64url): ~A~%" sig-b64)
      (format t "   Signature length (bytes): ~A~%" (length sig-bytes))

      ;; Decode back to verify
      (let ((decoded (base64url-decode sig-b64)))
        (format t "   Decoded signature length: ~A~%" (length decoded))
        (format t "   Matches original: ~A~%" (equalp sig-bytes decoded)))

      ;; Build final JWS
      (let ((jws `(("protected" . ,protected64)
                   ("payload" . ,payload64)
                   ("signature" . ,sig-b64))))
        (format t "~%5. Final JWS JSON:~%")
        (format t "   ~A~%" (cl-json:encode-json-to-string jws))))))

(sb-ext:exit :code 0)
