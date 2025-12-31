(in-package :mgl-pax)

(defvar *transcribe-check-consistency*)
(export '*transcribe-check-consistency*)
(defvar *transcribe-syntaxes*)
(export '*transcribe-syntaxes*)

(autoload transcribe "mgl-pax/transcribe")
(autoload squeeze-whitespace "mgl-pax/transcribe")
(autoload delete-trailing-whitespace "mgl-pax/transcribe")
(autoload delete-comments "mgl-pax/transcribe")
(autoload ensure-transcribe-loaded "mgl-pax/transcribe" :export nil)

;;; Silence SBCL compiler notes.
#+sbcl
(define-condition transcription-error (error) ())
(export 'transcription-error)

#+sbcl
(define-condition transcription-consistency-error (transcription-error) ())
(export 'transcription-consistency-error)

#+sbcl
(define-condition transcription-consistency-error (transcription-error) ())
(export 'transcription-values-consistency-error)

#+sbcl
(define-condition transcription-consistency-error (transcription-error) ())
(export 'transcription-output-consistency-error)
