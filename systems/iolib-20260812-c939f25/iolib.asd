;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

#.(unless (or #+asdf3.1 (version<= "3.1" (asdf-version)))
    (error "You need ASDF >= 3.1 to load this system correctly."))

#-unix
(error "Unfortunately, IOlib lacks support for non-Unix OSes (like Windows).")

(defsystem :iolib/syscalls
  :description "Syscalls and foreign types."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib.asdf :iolib.conf :cffi-grovel)
  :depends-on (:trivial-features :cffi :iolib.base)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/syscalls/"
  :components
  ((:file "pkgdcl")
   #+unix
   (:file "syscall-path-strings" :pathname "unix-syscall-path-strings")
   ;; Platform-specific files
   (:cffi-grovel-file "ffi-types" :pathname #+unix "ffi-types-unix")
   (:file "conditions")
   (:file "os-conditions" :pathname #+unix "os-conditions-unix")
   (:file "designators")
   (:file "early")
   (:file "ffi-functions" :pathname #+unix "ffi-functions-unix"))
  :serial t)

(defsystem :iolib/multiplex
  :description "I/O multiplexing library."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib.asdf :iolib.conf)
  :depends-on (:iolib.base :iolib/syscalls :cffi)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/multiplex/"
  :components
  ((:file "pkgdcl")

   ;; Scheduler
   (:file "utils" :depends-on ("pkgdcl"))
   (:file "timers" :depends-on ("pkgdcl"))
   (:file "queue" :depends-on ("pkgdcl"))
   (:file "scheduler" :depends-on ("pkgdcl" "timers" "queue"))

   ;; Event loop
   (:file "fd-entry" :depends-on ("pkgdcl" "timers"))
   (:file "multiplexer" :depends-on ("pkgdcl" "utils" "fd-entry"))
   (:file "event-loop" :depends-on ("pkgdcl" "timers" "queue" "scheduler"
                                             "fd-entry" "multiplexer"))

   ;; FD wait
   (:file "fd-wait" :depends-on ("pkgdcl" "utils"))

   ;; Event sources
   (:file "backend-select"
    :depends-on ("pkgdcl" "utils" "fd-entry" "multiplexer"))
   #+linux
   (:file "backend-epoll"
    :depends-on ("pkgdcl" "utils" "fd-entry" "multiplexer"))
   #+bsd
   (:file "backend-kqueue"
    :depends-on ("pkgdcl" "utils" "fd-entry" "multiplexer"))
   (:file "detect"
    :depends-on ("pkgdcl" "multiplexer" "backend-select"
                          #+linux "backend-epoll" #+bsd "backend-kqueue"))))

(defsystem :iolib/streams
  :description "Gray streams."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib.asdf :iolib.conf)
  :depends-on (:iolib.base :iolib/multiplex :cffi)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/streams/gray/"
  :components
  ((:file "pkgdcl")
   (:file "classes" :depends-on ("pkgdcl"))
   (:file "conditions" :depends-on ("pkgdcl"))
   (:file "buffer" :depends-on ("pkgdcl" "classes"))
   (:file "fd-mixin" :depends-on ("pkgdcl" "classes"))
   (:file "io-helpers"
     :depends-on ("pkgdcl" "classes" "conditions" "buffer" "fd-mixin"))
   (:file "gray-stream-methods"
     :depends-on ("pkgdcl" "classes" "conditions" "buffer" "fd-mixin"
                  "io-helpers"))))

(defsystem :iolib/zstreams
  :description "Zeta streams."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib.asdf)
  :depends-on (:iolib.base :iolib/syscalls :iolib/pathnames :cffi :bordeaux-threads)
  :around-compile "iolib.asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/streams/zeta/"
  :components
  ((:file "pkgdcl")
   (:file "types" :depends-on ("pkgdcl"))
   (:file "conditions" :depends-on ("pkgdcl"))

   ;; Platform-specific files
   (:file "ffi-functions" :pathname #+unix "ffi-functions-unix"
     :depends-on ("pkgdcl" "conditions"))

   ;; Device interface definition
   (:file "device" :depends-on ("pkgdcl" "types"))

   ;; Low-level buffers
   (:file "iobuf" :depends-on ("pkgdcl" "types"))

   ;; Streams
   (:file "stream" :depends-on ("pkgdcl" "types" "conditions" "device" "iobuf"))

   ;; Devices
   (:file "file" :pathname #+unix "file-unix"
     :depends-on ("pkgdcl" "types" "conditions" "ffi-functions" "device" "stream"))))

(defsystem :iolib/sockets
  :description "Socket library."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib.asdf :iolib.conf :cffi-grovel)
  :depends-on (:iolib.base :iolib/syscalls :iolib/streams
               :babel :cffi :bordeaux-threads
               :idna :swap-bytes)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/sockets/"
  :components
  ((:file "pkgdcl")
   (:cffi-grovel-file "grovel" :depends-on ("pkgdcl"))
   (:file "conditions" :depends-on ("pkgdcl" "grovel"))
   (:file "bsd" :depends-on ("pkgdcl" "grovel" "conditions"))
   (:file "common" :depends-on ("pkgdcl" "grovel" "bsd"))
   (:file "config" :depends-on ("pkgdcl" "grovel" "bsd"))

   (:file "iface" :depends-on ("pkgdcl" "grovel" "bsd" "common"))

   (:file "address" :depends-on ("pkgdcl" "common"))
   (:file "address-predicates" :depends-on ("pkgdcl" "common" "address"))
   (:file "address-arithmetic" :depends-on ("pkgdcl" "common" "address" "address-predicates"))

   (:file "base-sockets" :depends-on ("pkgdcl" "bsd" "common" "config"))
   (:file "socket-options"
     :depends-on ("pkgdcl" "grovel" "conditions" "bsd" "common" "config" "base-sockets"))

   ;; Local file configuration (/etc/hosts etc...)
   (:file "etc-files" :pathname "namedb/etc-files"
     :depends-on ("pkgdcl"))
   (:file "file-monitor" :pathname "namedb/file-monitor"
     :depends-on ("pkgdcl"))
   (:file "protocols" :pathname "namedb/protocols"
     :depends-on ("pkgdcl" "common" "etc-files" "file-monitor"))
   (:file "services" :pathname "namedb/services"
     :depends-on ("pkgdcl" "common" "etc-files" "file-monitor"))
   (:file "hosts" :pathname "namedb/hosts"
     :depends-on ("pkgdcl" "address" "address-predicates" "etc-files" "file-monitor"))

   (:file "socket-methods"
     :depends-on ("pkgdcl" "grovel" "conditions" "bsd" "common" "config"
                  "address" "address-predicates" "base-sockets" "socket-options"
                  "protocols" "services"))
   (:file "make-socket"
     :depends-on ("pkgdcl" "grovel" "common" "config" "address" "address-predicates"
                  "base-sockets" "socket-options" "services" "socket-methods"))

   ;; DNS client
   (:file "dns-common" :pathname "dns/common"
     :depends-on ("pkgdcl" "common"))
   (:file "nameservers" :pathname "dns/nameservers"
     :depends-on ("pkgdcl" "address" "address-predicates" "etc-files" "file-monitor"))
   (:file "message":pathname "dns/message"
     :depends-on ("pkgdcl" "common" "dns-common"))
   (:file "query" :pathname "dns/query"
     :depends-on ("pkgdcl" "conditions" "address" "address-predicates"
                  "socket-options" "socket-methods" "make-socket" "dns-common"
                  "nameservers" "message"))
   (:file "dns-conditions" :pathname "dns/conditions"
     :depends-on ("pkgdcl"))
   (:file "lookup" :pathname "dns/lookup"
     :depends-on ("pkgdcl" "address" "address-predicates" "file-monitor" "hosts"
                  "nameservers" "message" "query" "dns-conditions"))))

(defsystem :iolib/trivial-sockets
  :description "Trivial-Sockets compatibility layer."
  :author "Dan Barlow <dan@telent.net>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib.asdf :iolib.conf)
  :depends-on (:iolib.base :iolib/sockets)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/sockets/"
  :components
  ((:file "trivial-sockets")))

(defsystem :iolib/pathnames
  :description "New pathnames."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib.asdf :iolib.conf)
  :depends-on (:iolib.base :iolib/syscalls)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/pathnames/"
  :components
  ((:file "pkgdcl")
   (:file "file-path")
   (:file "file-path-os" :pathname #+unix "file-path-unix"))
  :serial t)

(defsystem :iolib/os
  :description "OS interface."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib.asdf :iolib.conf :cffi-grovel)
  :depends-on (:iolib.base :iolib/syscalls
               :iolib/streams :iolib/pathnames)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/os/"
  :components
  ((:file "pkgdcl")
   (:file "os" :pathname #+unix "os-unix")
   (:cffi-grovel-file "ffi-types" :pathname #+unix "ffi-types-unix")
   (:file "ffi-functions" :pathname #+unix "ffi-functions-unix")
   (:file "create-process" :pathname #+unix "create-process-unix"))
  :serial t)

(defsystem :iolib
  :description "I/O library."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib.asdf :iolib.conf)
  :depends-on (:iolib.base :iolib/multiplex :iolib/streams :iolib/sockets)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/iolib/"
  :components ((:file "pkgdcl"))
  :in-order-to ((test-op (test-op :iolib/tests))))

(defsystem :iolib/tests
  :description "IOLib test suite."
  :author "Luis Oliveira <loliveira@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib.base)
  :depends-on (:fiveam :iolib :iolib/pathnames)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "tests/"
  :components
  ((:file "pkgdcl")
   (:file "defsuites" :depends-on ("pkgdcl"))
   (:file "base" :depends-on ("pkgdcl" "defsuites"))
   (:file "file-paths-os" :depends-on ("pkgdcl" "defsuites")
    :pathname #+unix "file-paths-unix")
   (:file "multiplex" :depends-on ("pkgdcl" "defsuites"))
   (:file "streams" :depends-on ("pkgdcl" "defsuites"))
   (:file "sockets" :depends-on ("pkgdcl" "defsuites")))
  :perform (test-op (o c) (symbol-call :5am :run! :iolib)))
