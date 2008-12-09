;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package #:cl-l10n)

(defvar *locale*)

(setf (documentation '*locale* 'variable)
      "A list of locales that specifies the order of resource lookup. Please note that there's inheritance between the locales (e.g. 'en_US' inherits for 'en'), but this order here is to specify language preferences, like you can find in web browsers.")

(defvar *root-locale*)

(setf (documentation '*root-locale* 'variable)
      "The root locale found in cldr/main/root.xml. All locales inherit from this, it contains the definitions that are shared for most locales.")

(defun project-relative-pathname (file)
  (asdf:system-relative-pathname :cl-l10n file))

(defparameter *cldr-root-directory* (project-relative-pathname "cldr/main/"))

;; TODO locking for thread safety
(defparameter *locale-cache* (make-hash-table :test #'equal)
  "Hashtable containing all loaded locales keyed on LOCALE-NAME (eg. \"af_ZA\")")

(defun cached-locale (name)
  (gethash name *locale-cache*))

(defun (setf cached-locale) (new-val name)
  (setf (gethash name *locale-cache*)
        new-val))

;; TODO locking for thread safety
(defvar *locale-loaded-listeners* ())

(defun register-locale-loaded-listener (fn)
  (check-type fn symbol)
  (unless (find fn *locale-loaded-listeners*)
    (push fn *locale-loaded-listeners*)
    ;; TODO locking
    ;; call it for the already loaded locales
    (dolist (locale (hash-table-values *locale-cache*))
      (funcall fn (locale-name locale))))
  fn)

(defun unregister-locale-loaded-listener (fn)
  (check-type fn symbol)
  (deletef *locale-loaded-listeners* fn)
  (values))
