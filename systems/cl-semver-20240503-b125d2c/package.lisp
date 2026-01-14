(defpackage :cl-semver
  (:nicknames :semver)
  (:use :cl)
  (:import-from #:esrap
                #:defrule
                #:character-ranges
                #:parse
                #:?)
  (:import-from #:named-readtables
                #:defreadtable)
  (:export #:read-version-from-string
	   #:print-version
	   #:print-version-to-string
	   #:version
	   #:semantic-version
	   #:make-semantic-version
	   #:version-string-valid-p
           #:semantic-version-string
	   #:version-major
	   #:version-minor
	   #:version-patch
	   #:version-pre-release
       #:version-pre-release-identifiers
	   #:version-build
	   #:version=
	   #:version==
	   #:version/=
	   #:version/==
	   #:version<
	   #:version<=
	   #:version>
	   #:version>=
	   #:enable-version-syntax
	   #:disable-version-syntax
       #:semver-syntax
	   #:versionp)
  (:documentation "cl-semver is a Common Lisp implementation of the Semantic Versioning Specification (http://semver.org/"))
