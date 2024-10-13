;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-INTERPOL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-interpol/alias.lisp,v 1.3 2008/07/23 14:41:37 edi Exp $

;;; Copyright (c) 2003-2008, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-interpol)

;;; define some aliases
(loop for (alias . name) in '(("LINE FEED" . "LINE FEED \(LF)")
                              ("FORM FEED" . "FORM FEED \(FF)")
                              ("CARRIAGE RETURN" . "CARRIAGE RETURN \(CR)")
                              ("NEXT LINE" . "NEXT LINE \(NEL)")
                              ("LF" . "LINE FEED \(LF)")
                              ("FF" . "FORM FEED \(FF)")
                              ("CR" . "CARRIAGE RETURN \(CR)")
                              ("NEL" . "NEXT LINE \(NEL)")
                              ("ZWNJ" . "ZERO WIDTH NON-JOINER")
                              ("ZWJ" . "ZERO WIDTH JOINER")
                              ("BYTE ORDER MARK" . "ZERO WIDTH NO-BREAK SPACE")
                              ("BOM" . "BYTE ORDER MARK")
                              ("HORIZONTAL TABULATION" . "CHARACTER TABULATION")
                              ("VERTICAL TABULATION" . "LINE TABULATION")
                              ("FILE SEPARATOR" . "INFORMATION SEPARATOR FOUR")
                              ("GROUP SEPARATOR" . "INFORMATION SEPARATOR THREE")
                              ("RECORD SEPARATOR" . "INFORMATION SEPARATOR TWO")
                              ("UNIT SEPARATOR" . "INFORMATION SEPARATOR ONE")
                              ("PARTIAL LINE DOWN" . "PARTIAL LINE FORWARD")
                              ("PARTIAL LINE UP" . "PARTIAL LINE BACKWARD"))
      for existing-char = (character-named name)
      when existing-char
      do (setf (gethash (canonicalize-name alias) *unicode-aliases*) existing-char))
