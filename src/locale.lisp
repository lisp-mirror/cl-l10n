;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

;; TODO
;;  use LC_COLLATE to define locale-uppercase and friends
;;  Test on windows.
;;  Parsers (money)
;;  locale aliases?
;;  Optimizing print-time
;;  Handle _ and - in time directives (see date --help)
;;  Compile locales into fasl files.

(in-package :cl-l10n )

;; A list of locales in which case resources will be looked for in each locale in order.
(defvar *locale*)

;; The root locale found in cldr/main/root.xml. Contains definitions with their default values for all locales.
(defvar *root-locale*)

(defparameter *locale-cache* (make-hash-table :test #'equal)
  "Hash table containing all loaded locales keyed on LOCALE-NAME (eg. \"af_ZA\")")

;; Conditions
(define-condition locale-error (error)
  ((mesg :accessor mesg :initarg :mesg :initform "Unknown."))
  (:report (lambda (obj stream) (cl:format stream "~A" (mesg obj)))))

(defun locale-error (string &rest args)
  (error 'locale-error :mesg (apply #'cl:format nil string args)))

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
   (resources
    :initform (make-hash-table :test #'equal)
    :accessor resources-of)

   #+nil(locale-name :accessor locale-name :initarg :name 
                :initform (required-arg :name))
   (title :accessor title :initarg :title :initform nil)
   (printers :accessor printers :initarg :printers :initform nil)
   (parsers :accessor parsers :initarg :parsers :initform nil)
   (source :accessor source :initarg :source :initform nil)
   ;;(language :accessor language :initarg :language :initform nil)
   ;;(territory :accessor territory :initarg :territory :initform nil)
   ;;(revision :accessor revision :initarg :revision :initform nil)
   ;;(date :accessor date :initarg :date :initform nil)
   (categories :accessor categories :initarg :categories
               :initform (make-hash-table :test #'equal))))

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

(defmethod print-object ((obj locale) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (locale-name obj) stream)))

(defun locale-precedence-list (locale)
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

(defmacro do-locales (var &rest body)
  "Iterate all locales in *locale* and all their base locales."
  (with-unique-names (locale)
    `(dolist (,locale *locale*)
       (dolist (,var (locale-precedence-list ,locale))
         ,@body))))



(defclass category ()
  ((category-name :accessor category-name :initform (required-arg :name) 
                  :initarg :name)
   (vals :accessor vals :initform (make-hash-table :test #'equal)
         :initarg :vals)))

(defmethod print-object ((obj category) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (category-name obj) stream)))


(declaim (inline clear-locale-cache get-cached-locale (setf get-cached-locale)))

(defun clear-locale-cache ()
  (clrhash *locale-cache*))

(defun get-cached-locale (name)
  (gethash name *locale-cache*))

(defun (setf get-cached-locale) (new-val name)
  (setf (gethash name *locale-cache*)
        new-val))

(defgeneric get-category (locale name)
  (:documentation "Find category called NAME in locale LOCALE.")
  (:method ((locale locale) (name string))
    (gethash name (categories locale))))

(defmethod (setf get-category) ((new-val category) (locale locale) (name string)) 
  (setf (gethash name (categories locale))
        new-val))

(defgeneric category-value (category key)
  (:documentation "Lookup attribute named by string KEY in category CATEGORY.")
  (:method ((category category) (key string))
    (gethash key (vals category))))

(defmethod (setf category-value) ((new-val t) (category category) (key string))
  (setf (gethash key (vals category))
        new-val))

(defun locale-value (locale cat key)
  (awhen (get-category locale cat)
    (category-value it key)))

;; Getters
(defmacro defgetter (key cat &key (wrap '#'identity))
  (let ((name (concatenate-symbol "LOCALE-" (substitute #\- #\_ (string-upcase key)))))
    `(progn 
       (defun ,name (&optional (locale (current-locale)))
         (let ((locale (locale locale)))
           (when locale
             (funcall ,wrap (locale-value locale ,cat ,key)))))
       (export ',name))))

(defun parse-car-or-val (x)
  (values (parse-integer (if (consp x) (car x) x))))

(defgetter "int_curr_symbol" "LC_MONETARY")
(defgetter "currency_symbol" "LC_MONETARY")
(defgetter "mon_decimal_point" "LC_MONETARY")
(defgetter "mon_thousands_sep" "LC_MONETARY")
(defgetter "mon_grouping" "LC_MONETARY" :wrap 'parse-car-or-val)
(defgetter "positive_sign" "LC_MONETARY")
(defgetter "negative_sign" "LC_MONETARY")
(defgetter "int_frac_digits" "LC_MONETARY" :wrap 'parse-car-or-val)
(defgetter "frac_digits" "LC_MONETARY" :wrap 'parse-car-or-val)
(defgetter "p_cs_precedes" "LC_MONETARY" :wrap 'parse-car-or-val)
(defgetter "p_sep_by_space" "LC_MONETARY" :wrap 'parse-car-or-val)
(defgetter "n_cs_precedes" "LC_MONETARY" :wrap 'parse-car-or-val)
(defgetter "n_sep_by_space" "LC_MONETARY" :wrap 'parse-car-or-val)
(defgetter "p_sign_posn" "LC_MONETARY" :wrap 'parse-car-or-val)
(defgetter "n_sign_posn" "LC_MONETARY" :wrap 'parse-car-or-val)
(defgetter "decimal_point" "LC_NUMERIC")
(defgetter "thousands_sep" "LC_NUMERIC")
(defgetter "grouping" "LC_NUMERIC" :wrap 'parse-car-or-val)
(defgetter "abday" "LC_TIME")
(defgetter "day" "LC_TIME")
(defgetter "abmon" "LC_TIME")
(defgetter "mon" "LC_TIME")
(defgetter "d_t_fmt" "LC_TIME")
(defgetter "d_fmt" "LC_TIME")
(defgetter "t_fmt" "LC_TIME")
(defgetter "am_pm" "LC_TIME")
(defgetter "t_fmt_ampm" "LC_TIME")
(defgetter "date_fmt" "LC_TIME")
(defgetter "yesexpr" "LC_MESSAGES")
(defgetter "noexpr" "LC_MESSAGES")
(defgetter "height" "LC_PAPER")
(defgetter "width" "LC_PAPER")
(defgetter "name_fmt" "LC_NAME")
(defgetter "name_gen" "LC_NAME")
(defgetter "name_mr" "LC_NAME")
(defgetter "name_mrs" "LC_NAME")
(defgetter "name_miss" "LC_NAME")
(defgetter "name_ms" "LC_NAME")
(defgetter "postal_fmt" "LC_ADDRESS")
(defgetter "tel_int_fmt" "LC_TELEPHONE")
(defgetter "measurement" "LC_MEASUREMENT")


;; EOF
