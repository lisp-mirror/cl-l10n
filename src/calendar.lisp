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

(defun handle-otherwise/gregorian-calendar-effective-accessor (slot-name otherwise otherwise-provided?)
  (unless otherwise-provided?
    (setf otherwise
          `(:error "Could not find a locale with non-nil ~S slot in the current locale precedence list ~A"
                   ,slot-name ,*locale*)))
  (handle-otherwise otherwise))

(macrolet
    ((define-effective-accessors (&rest slot-names)
       `(progn
          ,@(iter (for slot-name :in slot-names)
                  (for accessor = (symbolicate slot-name '#:-of))
                  (for effective-accessor = (symbolicate '#:effective- slot-name '#:/gregorian-calendar))
                  (collect `(progn
                              (defun ,effective-accessor (&key (otherwise nil otherwise-provided?))
                                (do-current-locales locale
                                  (awhen (gregorian-calendar-of locale)
                                    (awhen (,accessor it)
                                      (return-from ,effective-accessor it))))
                                (handle-otherwise/gregorian-calendar-effective-accessor ',slot-name otherwise otherwise-provided?))
                              (export ',effective-accessor)))))))
  ;; FIXME this should collect the non-nil array values, not only non-nil entire arrays.
  ;; e.g. de_AT has a month override for de, but only for january.
  (define-effective-accessors
    month-names
    abbreviated-month-names
    narrow-month-names

    day-names
    abbreviated-day-names
    narrow-day-names

    quarter-names
    abbreviated-quarter-names

    era-names
    abbreviated-era-names
    narrow-era-names

    am
    pm))
