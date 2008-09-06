;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-l10n )

;; Classes
(defclass gregorian-calendar ()
  ((month-names
    :initform nil
    :type (or null (vector * 12))
    :accessor month-names-of)
   (abbreviated-month-names
    :initform nil
    :type (or null (vector * 12))
    :accessor abbreviated-month-names-of)
   (narrow-month-names
    :initform nil
    :type (or null (vector * 12))
    :accessor narrow-month-names-of)

   (day-names
    :initform nil
    :type (or null (vector * 7))
    :accessor day-names-of)
   (abbreviated-day-names
    :initform nil
    :type (or null (vector * 7))
    :accessor abbreviated-day-names-of)
   (narrow-day-names
    :initform nil
    :type (or null (vector * 7))
    :accessor narrow-day-names-of)

   (quarter-names
    :initform nil
    :type (or null (vector * 4))
    :accessor quarter-names-of)
   (abbreviated-quarter-names
    :initform nil
    :type (or null (vector * 4))
    :accessor abbreviated-quarter-names-of)

   (era-names
    :initform nil
    :type (or null (vector * 2))
    :accessor era-names-of)
   (abbreviated-era-names
    :initform nil
    :type (or null (vector * 2))
    :accessor abbreviated-era-names-of)
   (narrow-era-names
    :initform nil
    :type (or null (vector * 2))
    :accessor narrow-era-names-of)

   (am
    :accessor am-of)
   (pm
    :accessor pm-of)
   (date-formatters
    :initform nil
    :accessor date-formatters-of)))
