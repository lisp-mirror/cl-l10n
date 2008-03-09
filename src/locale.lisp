;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-l10n )

(defvar *locale*)

(setf (documentation '*locale* 'variable)
      "A list of locales that specifies the order of resource lookup. Please note that there's inheritance between the locales (e.g. 'en_US' inherits for 'en'), but this order here is to specify language preferences, like you can find in web browsers.")

(defvar *root-locale*)

(setf (documentation '*root-locale* 'variable)
      "The root locale found in cldr/main/root.xml. All locales inherit from this, it contains the definitions that are shared for most locales.")

;; Conditions
(define-condition locale-error (simple-error)
  ())

(defun locale-error (message &rest args)
  (error 'locale-error :format-control message :format-arguments args))

;; Classes
(defclass locale ()
  ((language
    :initform (required-arg :language)
    :initarg :language
    :accessor language-of)
   (script
    :initform nil
    :initarg :script
    :accessor script-of)
   (territory
    :initform nil
    :initarg :territory
    :accessor territory-of)
   (variant
    :initform nil
    :initarg :variant
    :accessor variant-of)
   (precedence-list
    :initform nil
    :initarg :precedence-list
    :accessor precedence-list-of)
   (version-info
    :initform nil
    :initarg :version-info
    :accessor version-info-of)
   (number-symbols
    :initform (list)
    :accessor number-symbols-of)
   (currencies
    :initform (make-hash-table :test #'eq)
    :accessor currencies-of)
   (languages
    :initform (make-hash-table :test #'eq)
    :accessor languages-of)
   (scripts
    :initform (make-hash-table :test #'eq)
    :accessor scripts-of)
   (territories
    :initform (make-hash-table :test #'eq)
    :accessor territories-of)
   (variants
    :initform (make-hash-table :test #'eq)
    :accessor variants-of)
   (resources
    :initform (make-hash-table :test #'equal)
    :accessor resources-of)))

(defmethod print-object ((obj locale) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (locale-name obj) stream)))

(defgeneric locale-name (locale &key ignore-script ignore-territory ignore-variant)
  (:method ((locale locale) &key ignore-variant ignore-territory ignore-script)
    (let ((*print-pretty* nil))
      (with-output-to-string (*standard-output*)
        (write-string (language-of locale))
        (unless ignore-script
          (awhen (script-of locale)
            (write-char #\_)
            (write-string it)))
        (unless ignore-territory
          (awhen (territory-of locale)
            (write-char #\_)
            (write-string it)))
        (unless ignore-variant
          (awhen (variant-of locale)
            (write-char #\_)
            (write-string it)))))))

(defun locale-precedence-list (locale)
  "Calculate the precedence list for a locale that should be searched for definitions. For example: (locale-precedence-list (locale \"en_US_POSIX\")) => (en_US_POSIX en_US en root)"
  (let ((result (list locale)))
    (flet ((try (locale-name)
             (awhen (locale locale-name :errorp nil)
               (push it result))))
      (when (variant-of locale)
        (try (locale-name locale
                          :ignore-variant t)))
      (when (territory-of locale)
        (try (locale-name locale
                          :ignore-territory t
                          :ignore-variant t)))
      (when (script-of locale)
        (try (locale-name locale
                          :ignore-script t
                          :ignore-territory t
                          :ignore-variant t))))
    (push *root-locale* result)
    (nreverse result)))

(defmethod precedence-list-of :around ((self locale))
  (let ((result (call-next-method)))
    (unless result
      (setf result (locale-precedence-list self))
      (setf (precedence-list-of self) result))
    result))

(defmacro do-locales (var &rest body)
  "Iterate all locales in *locale* and all their base locales in the right order."
  (with-unique-names (locale)
    `(dolist (,locale *locale*)
       (dolist (,var (precedence-list-of ,locale))
         ,@body))))

;;;
;;; Caching parsed locale instances
;;;
;; TODO locking with a spinlock
(defparameter *locale-cache* (make-hash-table :test #'equal)
  "Hashtable containing all loaded locales keyed on LOCALE-NAME (eg. \"af_ZA\")")

(declaim (inline clear-locale-cache cached-locale (setf cached-locale)))

(defun clear-locale-cache ()
  (clrhash *locale-cache*))

(defun cached-locale (name)
  (gethash name *locale-cache*))

(defun (setf cached-locale) (new-val name)
  (setf (gethash name *locale-cache*)
        new-val))
