;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: CL-USER -*-
;;;; Copyright (c) 2006-2008 Henrik Hjelte
;;;; All rights reserved.
;;;; See the file LICENSE for terms of use and distribution.

(defpackage :json
  (:nicknames :cl-json)
  (:use :common-lisp)
  (:export
   ;; common.lisp
   #:with-shadowed-custom-vars
   #:bind-custom-vars
   #:set-custom-vars
   #:*use-strict-json-rules*
   #:*json-symbols-package*
   #:json-intern
   #:unknown-symbol-error
   #:safe-json-intern
   #:*json-identifier-name-to-lisp*
   #:*lisp-identifier-name-to-json*
   #:*identifier-name-to-key*
   ;; camel-case.lisp
   #:simplified-camel-case-to-lisp
   #:camel-case-to-lisp
   #:lisp-to-camel-case
   ;; objects.lisp
   #:with-local-class-registry
   #:clear-class-registry
   #:fluid-class
   #:fluid-object
   #:make-object
   #:make-object-prototype
   #:prototype
   #:*prototype-name*
   ;; decoder.lisp
   #:*json-input*
   #:decode-json
   #:decode-json-strict
   #:decode-json-from-string
   #:decode-json-from-source
   #:json-syntax-error
   #:no-char-for-code
   #:substitute-char
   #:pass-code
   #:bignumber-string
   #:rational-approximation
   #:placeholder
   #:*boolean-handler*
   #:*integer-handler*
   #:*real-handler*
   #:*beginning-of-array-handler*
   #:*array-member-handler*
   #:*end-of-array-handler*
   #:*beginning-of-string-handler*
   #:*string-char-handler*
   #:*end-of-string-handler*
   #:*beginning-of-object-handler*
   #:*object-key-handler*
   #:*object-value-handler*
   #:*end-of-object-handler*
   #:*json-array-type*
   #:*internal-decoder*
   #:*array-scope-variables*
   #:*object-scope-variables*
   #:*string-scope-variables*
   #:*aggregate-scope-variables*
   #:current-decoder
   #:custom-decoder
   #:with-custom-decoder-level
   #:set-decoder-simple-list-semantics
   #:with-decoder-simple-list-semantics
   #:set-decoder-simple-clos-semantics
   #:with-decoder-simple-clos-semantics
   ;; encoder.lisp
   #:*json-output*
   #:unencodable-value-error
   #:substitute-printed-representation
   #:with-substitute-printed-representation-restart
   #:encode-json
   #:encode-json-to-string
   #:encode-json-alist
   #:encode-json-alist-to-string
   #:encode-json-plist
   #:encode-json-plist-to-string
   #:with-array
   #:as-array-member
   #:encode-array-member
   #:stream-array-member-encoder
   #:with-object
   #:as-object-member
   #:encode-object-member
   #:stream-object-member-encoder

   #:use-explicit-encoder
   #:use-guessing-encoder
   #:with-explicit-encoder
   #:with-guessing-encoder
   #:json-bool
   #:json-or-null
   ;; utils.lisp
   #:json-bind
   )
  #+cl-json-clos
  (:import-from #+(or mcl openmcl) #:ccl
                #+cmu #:clos-mop
                #+sbcl #:sb-mop
                #+(or clisp ecl scl lispworks) #:clos
                #+(or allegro abcl) #:mop
		#+genera #:clos-internals
    #:class-slots
    #:class-direct-slots
    #:class-direct-superclasses
    #:slot-definition-name
    #:add-direct-subclass
    #:remove-direct-subclass
    #:validate-superclass
    #:class-precedence-list
    #:compute-class-precedence-list
    #:ensure-class
    #:finalize-inheritance
    ))


(defpackage :json-rpc
  (:use :common-lisp :json)
  (:shadow #:defconstant)
  (:export
    #:clear-exported
    #:export-as-json-rpc

    ;; invoke functions for the benefit of JSON-RPC
    #:invoke-rpc
    #:invoke-rpc-parsed

    ;; restarts
    #:send-error
    #:send-error-object
    #:send-nothing
    #:send-internal-error

    ;; special variable for controlling JSON-RPC
    #:*json-rpc-version*

    ;; constants
    #:+json-rpc-1.1+
    #:+json-rpc-2.0+

    ;; condition 
    #:json-rpc-call-error

    ;; declarations
    #:def-json-rpc-encoding
    #:defun-json-rpc
    ))
