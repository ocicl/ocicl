# CL-SEMVER

cl-semver is a Common Lisp implementation of the Semantic Versioning Specification (http://semver.org/

## Functions
### %disable-version-syntax nil
Internal function used to restore previous readtable.


### %enable-version-syntax nil
Internal function used to enable reader syntax and store current
readtable on stack.


### make-semantic-version (major minor patch &optional pre-release build)
Creates a semantic version


### print-version (version &optional stream)
Prints a version to a stream


### print-version-to-string (version)
Prints a version to a string


### read-version-from-string (string &optional (class (quote semantic-version)))
Parses a semantic version from a string


### version<= (version1 version2)
Version less or equal comparison

### version> (version1 version2)
Version greater than comparison


### version>= (version1 version2)
Version greater or equal comparison


## Macros
### disable-version-syntax
Restore readtable which was active before last call to
ENABLE-VERSION-SYNTAX. If there was no such call, the standard
readtable is used.

### enable-version-syntax
Enable version reader syntax.

## Generic-Functions
### version/=
Version distinct comparison

### version/==
Version shallow distinct comparison

### version<
Version less than comparison

### version=
Version equality comparison

### version==
Version shallow equality comparison

## Slot-Accessors
### version-build
The build version number

### (setf version-build)
The build version number

### version-major
The major version number

### (setf version-major)
The major version number

### version-minor
The minor version number

### (setf version-minor)
The minor version number

### version-patch
The patch (or micro) version number

### (setf version-patch)
The patch (or micro) version number

### version-pre-release
The pre release version number

### (setf version-pre-release)
The pre release version number

### version-pre-release-identifiers
The pre release segment, broken into a list of strings and integers.

### (setf version-pre-release-identifiers)
The pre release segment, broken into a list of strings and integers.

## Variables
## Classes
### semantic-version
Instances represent a full version according to the semantic version specs (version 2.0.0-rc1 of the spec). http://semver.org/ . The main features of this class are validation and version comparison.

### version
Library version

## Conditions
## Constants
## Readtables

### semver-syntax
The named readtable implementing the specialized semver reader using `#v`.
You can use the #v syntax by doing:
`(named-readtables:in-readtable cl-semver:semver-syntax)`
