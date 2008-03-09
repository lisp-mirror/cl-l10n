;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-l10n )

;; Classes
(defclass gregorian-calendar ()
  ((month-names
    :initform (make-array 12)
    :accessor month-names-of)
   (abbreviated-month-names
    :initform (make-array 12)
    :accessor abbreviated-month-names-of)
   (day-names
    :initform (make-array 7)
    :accessor day-names-of)
   (abbreviated-day-names
    :initform (make-array 7)
    :accessor abbreviated-day-names-of)
   (quarter-names
    :initform (make-array 4)
    :accessor quarter-names-of)
   (abbreviated-quarter-names
    :initform (make-array 4)
    :accessor abbreviated-quarter-names-of)
   (am
    :accessor am-of)
   (pm
    :accessor pm-of)))

