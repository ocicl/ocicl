(defpackage :archive
  (:use :cl #+(and lispworks (not win32)) :sys)
  (:export ;; types
           #:archive #:tar-archive #:odc-cpio-archive #:svr4-cpio-archive

           ;; creating
           #:open-archive #:close-archive

           ;; conditions
           #:archive-error
           #:tar-error
           #:invalid-tar-checksum-error #:unhandled-error
           #:unhandled-read-header-error #:unhandled-extract-entry-error
           #:unhandled-write-entry-error
           #:invalid-cpio-magic-error

           ;; entry slot readers
           #:name
           #:entry-stream

           ;; entry tests
           #:entry-directory-p
           #:entry-regular-file-p
           #:entry-symbolic-link-p
           #:entry-character-device-p
           #:entry-block-device-p
           #:entry-fifo-p

           ;; reading archives
           #:read-entry-from-archive
           #:extract-entry
           #:discard-entry

           ;; writing archives
           #:create-entry-from-pathname
           #:write-entry-to-archive #:finalize-archive

           ;; convenience macros
           #:do-archive-entries #:with-open-archive

           ;; external support
           #:*bytevec-to-string-conversion-function*
           #:*string-to-bytevec-conversion-function*))
