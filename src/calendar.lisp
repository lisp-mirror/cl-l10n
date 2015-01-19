;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-l10n )

(defclass calendar ()
  ())

;; Classes
(defclass gregorian-calendar (calendar)
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

   ;; day names start from 0, which means Sunday regardless of the locale dependent first day of the week
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
    :initform nil
    :accessor am-of)
   (pm
    :initform nil
    :accessor pm-of)
   (date-formatters
    :initform nil
    :accessor date-formatters-of)
   (time-formatters
    :initform nil
    :accessor time-formatters-of)))

(defun effective-calendar-value (calendar-slot-reader value-slot-reader &optional default)
  (setf calendar-slot-reader (ensure-function calendar-slot-reader))
  (setf value-slot-reader (ensure-function value-slot-reader))
  (or (do-current-locales locale
        ;;find the first non-nil value
        (awhen (funcall calendar-slot-reader locale)
          (awhen (funcall value-slot-reader it)
            (return it))))
      default))

(defun effective-date-related-names (calendar-slot-reader name-vector-slot-reader &optional defaults)
  (bind ((result (effective-calendar-value calendar-slot-reader name-vector-slot-reader)))
    (if result
        (when (some #'null result)
          ;; if it's partial then make a copy and fill it in
          (setf result (copy-seq result))
          (iter (for index :from 0 :below (length result))
                (unless (aref result index)
                  (bind ((inherited-name (do-current-locales locale
                                           (awhen (funcall calendar-slot-reader locale)
                                             (awhen (funcall name-vector-slot-reader it)
                                               (awhen (aref it index)
                                                 (return it)))))))
                    (unless inherited-name
                      (cldr-parser-warning "Locale ~A has no value at index ~A of gregorian date part name ~A"
                                           (current-locale) index name-vector-slot-reader)
                      (when defaults
                        (setf inherited-name (aref defaults index))))
                    (setf (aref result index) inherited-name)))))
        (setf result defaults))
    result))

(defun effective-date-related-names/gregorian-calendar (name-vector-slot-reader &optional defaults)
  (effective-date-related-names 'gregorian-calendar-of name-vector-slot-reader defaults))

(defun effective-am/gregorian-calendar ()
  (effective-calendar-value 'gregorian-calendar-of 'am-of "am"))

(defun effective-pm/gregorian-calendar ()
  (effective-calendar-value 'gregorian-calendar-of 'pm-of "pm"))
