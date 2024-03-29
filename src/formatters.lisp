;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package :cl-l10n)

(declaim (inline keyword-to-ldml))
(defun keyword-to-ldml (symbol)
  (case symbol
    (:short  'ldml:short)
    (:medium 'ldml:medium)
    (:long   'ldml:long)
    (:full   'ldml:full)))

(defmacro with-normalized-stream-variable (stream &body body)
  (check-type stream (and symbol (not (member nil t))))
  (with-unique-names (body-fn to-string?)
    `(flet ((,body-fn ()
              ,@body))
       (if (streamp ,stream)
           (,body-fn)
           (bind ((,to-string? nil))
             (cond
               ((null ,stream)
                (setf ,stream (make-string-output-stream))
                (setf ,to-string? t))
               ((eq ,stream t)
                (setf ,stream *standard-output*)))
             (,body-fn)
             (if ,to-string?
                 (get-output-stream-string ,stream)
                 ,stream))))))

(defun %format-iterating-locales (stream locale-visitor fallback-fn)
  (declare (optimize speed)
           (type function locale-visitor fallback-fn))
  (with-normalized-stream-variable stream
    (block iterating-locales
      (do-current-locales locale
        (when (funcall locale-visitor stream locale)
          (return-from iterating-locales)))
      (funcall fallback-fn stream))))

;; TODO this should be cleaned up and finished once local-time settles down on how to represent dates and time of day.
;; for now format-date happily format timestamps and understands time format directives when passed in a custom pattern.
(defun format-date (stream date &key (verbosity 'ldml:medium) pattern (calendar 'gregorian-calendar))
  "Format date from local-time timestamp according to locale"
  (unless (symbolp calendar)
    (setf calendar (type-of calendar)))
  (ecase calendar
    (gregorian-calendar (format-date/gregorian-calendar stream date :verbosity verbosity :pattern pattern))))

(defun format-time (stream time &key (verbosity 'ldml:medium) pattern (calendar 'gregorian-calendar))
  "Format time for local-time timestamp according to locale"
  (unless (symbolp calendar)
    (setf calendar (type-of calendar)))
  (ecase calendar
    (gregorian-calendar (format-time/gregorian-calendar stream time :verbosity verbosity :pattern pattern))))

(defun format-timestamp (stream timestamp &key (verbosity 'ldml:medium) pattern (calendar 'gregorian-calendar))
  "Format localized date and time for a local-time timestamp"
  (unless (symbolp calendar)
    (setf calendar (type-of calendar)))
  (unless verbosity
    (setf verbosity 'ldml:medium))
  (ecase calendar
    (gregorian-calendar (format-timestamp/gregorian-calendar stream timestamp :verbosity verbosity :pattern pattern))))

(defun %format-date-or-time/gregorian-calendar (stream value warning-string formatter-slot-reader fallback-pattern &key (verbosity 'ldml:medium) pattern )
  (check-type pattern (or null string function))
  (setf verbosity (or (keyword-to-ldml verbosity) verbosity))
  (etypecase pattern
    (string
     ;; NOTE: this code path is about 5 times slower and conses about 10 times more...
     ;; OPTIMIZATION: we could implement some per-locale caching here, but it must be
     ;; carefully keyed (a compiled lambda captures stuff at compile time from the compile time value of *locale*)
     ;; and the cache must be properly locked to support threading.
     (with-normalized-stream-variable stream
       (funcall (compile-date-time-pattern/gregorian-calendar pattern) stream value)))
    (compiled-pattern
     (with-normalized-stream-variable stream
       (funcall pattern stream value)))
    (null
     (%format-iterating-locales
      stream
      (named-lambda %format-date-or-time/gregorian-calendar/visitor (stream locale)
        (when-bind gregorian-calendar (gregorian-calendar-of locale)
          (bind ((formatter-entry (getf (funcall formatter-slot-reader gregorian-calendar) verbosity))
                 (formatter (getf formatter-entry :formatter)))
            (if formatter
                (progn
                  (funcall formatter stream value)
                  t)
                nil))))
      (named-lambda %format-date-or-time/gregorian-calendar/fallback (stream)
        (warn warning-string verbosity (current-locale))
        (local-time:format-timestring stream value :format fallback-pattern))))))

(defun format-date/gregorian-calendar (stream date &key (verbosity 'ldml:medium) pattern)
  (%format-date-or-time/gregorian-calendar stream date "No Gregorian calendar date formatter was found with verbosity ~S for locale ~A. Ignoring the locale and printing in a fixed simple format."
                                           'date-formatters-of '((:year 4) #\- (:month 2) #\- (:day 2))
                                           :verbosity verbosity :pattern pattern))

(defun format-time/gregorian-calendar (stream timestamp &key (verbosity 'ldml:medium) pattern)
  (%format-date-or-time/gregorian-calendar stream timestamp "No Gregorian calendar time formatter was found with verbosity ~S for locale ~A. Ignoring the locale and printing in a fixed simple format."
                                           'time-formatters-of '((:hour 2) #\: (:min 2) #\: (:sec 2))
                                           :verbosity verbosity :pattern pattern))

(defun format-timestamp/gregorian-calendar (stream timestamp &key (verbosity 'ldml:medium) pattern)
  (declare (ignore stream timestamp))
  (check-type pattern (or null string function))
  (setf verbosity (or (keyword-to-ldml verbosity) verbosity))
  (with-normalized-stream-variable stream
    (if pattern
        (%format-date-or-time/gregorian-calendar stream timestamp "FORMAT-TIMESTAMP: Should not happen..." nil nil :verbosity verbosity :pattern pattern)
        (progn
          (format-date/gregorian-calendar stream timestamp :verbosity verbosity)
          (write-char #\Space stream)
          (format-time/gregorian-calendar stream timestamp :verbosity verbosity)))))

(defun format-number/currency (stream number currency-code &key (verbosity 'ldml:medium) pattern)
  "Format currency. number is the amount and currency-code is the currency code form the cl-l10n.ldml package."
  (setf verbosity (or (keyword-to-ldml verbosity) verbosity))
  (etypecase pattern
    (compiled-pattern
     (with-normalized-stream-variable stream
       (funcall pattern stream number currency-code)))
    (string
     (with-normalized-stream-variable stream
       (funcall (compile-number-pattern/currency pattern) stream number currency-code)))
    (null
     (%format-iterating-locales
      stream
      (named-lambda format-number/currency/visitor (stream locale)
        (awhen (currency-formatter-of locale)
          (bind ((entry (or (assoc-value (formatters-of it) verbosity)
                            (assoc-value (formatters-of it) nil)))
                 (formatter (getf entry :formatter)))
            (if formatter
                (progn
                  (funcall formatter stream number currency-code)
                  t)
                nil))))
      (named-lambda format-number/currency/fallback (stream)
        (warn "No currency formatter was found with verbosity ~S for locale ~A. Ignoring the locale and printing in a fixed simple format."
              verbosity (current-locale))
        (cl:format stream "~A ~A" number currency-code))))))

(defun %format-number (stream number verbosity
                       pattern pattern-compiler
                       formatter-accessor formatter-name fallback-format-pattern)
  (setf verbosity (or (keyword-to-ldml verbosity) verbosity))
  (etypecase pattern
    (compiled-pattern
     (with-normalized-stream-variable stream
       (funcall pattern stream number)))
    (string
     (with-normalized-stream-variable stream
       (funcall (funcall pattern-compiler pattern) stream number)))
    (null
     (%format-iterating-locales
      stream
      (named-lambda %format-number/visitor (stream locale)
        (bind ((entry (or (assoc-value (funcall formatter-accessor locale) verbosity)
                          (assoc-value (funcall formatter-accessor locale) nil)))
               (formatter (getf entry :formatter)))
          (if formatter
              (progn
                (funcall formatter stream number)
                t)
              nil)))
      (named-lambda %format-number/fallback (stream)
        (warn "No ~A was found with verbosity ~S for locale ~A. Ignoring the locale and printing in a fixed simple format."
              formatter-name verbosity (current-locale))
        (cl:format stream fallback-format-pattern number))))))

(defun format-number/decimal (stream number &key (verbosity 'ldml:medium) pattern)
  "Format number as a decimal number proper format for the given locale. Pattern is a number pattern
as specified here http://unicode.org/reports/tr35/tr35-numbers.html#Number_Format_Patterns.
The stream is treated the same as in cl:format."
  (%format-number stream number verbosity
                  pattern 'compile-number-pattern/decimal
                  #'decimal-formatters-of "decimal number formatter" "~A"))

(defun format-number/percent (stream number &key (verbosity 'ldml:medium) pattern)
  "Format number as a percentage in the proper format for the current locale. For example in the en_US
locale (format-number/percent nil 0.5) returns the string \"50%\""
  (%format-number stream number verbosity
                  pattern 'compile-number-pattern/percent
                  #'percent-formatters-of "percent number formatter" "~A%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customized format directives

(define-constant +directive-replacements+ '((#\N . "/cl-l10n:%format-number.decimal/")
                                            (#\Y . "/cl-l10n:%format-number.percent/")
                                            (#\L . "/cl-l10n:%format-date/")
                                            (#\M . "/cl-l10n:%format-time/")
                                            (#\U . "/cl-l10n:%format-timestamp/")
                                            ;; currency support is pretty hopeless here because it needs the currency name as an argument,
                                            ;; but the format syntax only allows numeric or character arguments...
                                            )
  :test 'equal
  :documentation "alist mapping new format directives to formatting functions")

(define-compiler-macro format (&whole form destination format-control &rest format-arguments)
  "Compiler macro to remove unnecessary calls to parse-format-string."
  (if (stringp format-control)
      `(cl:format ,destination ,(parse-format-string format-control) ,@format-arguments)
      form))

(defmacro formatter (format-string)
  "Shadow fromatter function that adds the same directives as the shadow format."
  (etypecase format-string
    (string `(cl:formatter ,(parse-format-string format-string)))))

(defun format (stream format-control &rest format-arguments)
  "Shadow format function, with additional directives for localized values.
The following additional directives are added.
~N -- Localized decimal number -- %format-number.decimal
~Y -- Localized percent number -- %format-number.percent
~L -- Localized date -- %format-date
~M -- Localized time -- %format-time
~U -- Localized timestamp -- %format-timestamp
See appropriate format functions for details."
  (apply #'cl:format stream
         (etypecase format-control
           (function format-control)
           (string (parse-format-string format-control)))
         format-arguments))

(defun shadow-format (&optional (package *package*))
  "Shadowing import the CL-L10N::FORMAT and CL-L10N::FORMATTER symbols into PACKAGE."
  (shadowing-import '(cl-l10n::format cl-l10n::formatter) package))

(defun %format-number.decimal (stream number colon-modifier? at-modifier?)
  "Format function for localized numbers appropriate for using with the ~// format directive.

Example:
(format t \"~/cl-l10n:%format-number.decimal/\" 1002932)
prints 1,002,932"
  (bind ((print-decimal-point? (not colon-modifier?))
         (print-thousand-separator? (not at-modifier?)))
    (unless print-decimal-point?
      (not-yet-implemented "Turning off the decimal point"))
    (unless print-thousand-separator?
      (not-yet-implemented "Turning off thousand separators"))
    (format-number/decimal stream number))
  (values))

(defun %format-number.percent (stream number colon-modifier? at-modifier?)
  (declare (ignore colon-modifier? at-modifier?))
  "Format function for localized percentages appropriate for use with the ~// format directive."
  (format-number/percent stream number)
  (values))

(defun %format-date (stream date colon-modifier? at-modifier?)
  (declare (ignore colon-modifier? at-modifier?))
  "Format function for localized dates appropriate for use with the ~// format directive."
  (format-date/gregorian-calendar stream date)
  (values))

(defun %format-time (stream time colon-modifier? at-modifier?)
  (declare (ignore colon-modifier? at-modifier?))
  "Format function for localized times appropriate for use with the ~// format directive."
  (format-time/gregorian-calendar stream time)
  (values))

(defun %format-timestamp (stream timestamp colon-modifier? at-modifier?)
  "Format function for localized timestamps appropriate for use with the ~// format directive.
If the colon modifier is used, the timestamp is not printed.
If the at modifier is used, the UTC timezone is used"
  (bind ((show-timezone? (not colon-modifier?))
         (in-utc-zone? at-modifier?)
         (local-time:*default-timezone* (if in-utc-zone? local-time:+utc-zone+ local-time:*default-timezone*)))
    (declare (ignore show-timezone?))
    ;; TODO implement show-timezone?, or maybe just use colon-modifier? as a minimal control on verbosity...
    (format-timestamp/gregorian-calendar stream timestamp))
  (values))

(defun parse-format-string (string)
  (declare (optimize speed)
           (type string string))
  (flet ((needs-parsing? (string)
           (cl-ppcre:scan (load-time-value (cl-ppcre:create-scanner
                                            (cl:format nil "~~[@V,:\\d]*[~{~A~^|~}]" (mapcar 'first +directive-replacements+))))
                          (string-upcase string)))
         (really-parse-format-string (string)
           (declare (optimize speed)
                    (type simple-string string))
           (flet ((get-replacement (char)
                    (or (when (typep char 'base-char)
                          (cdr (assoc (char-upcase (the base-char char))
                                      +directive-replacements+)))
                        char)))
             (declare (inline get-replacement))
             (bind ((*print-pretty* nil)
                    (*print-circle* nil))
               (with-output-to-string (result)
                 (loop
                    :for char :across string
                    :with tilde = nil
                    :do (case char
                          ((#\@ #\v #\, #\:)
                           (princ char result))
                          (#\~
                           (princ char result)
                           (if tilde
                               (setf tilde nil)
                               (setf tilde t)))
                          (t
                           (if tilde
                               (if (or (digit-char-p char)
                                       (member char '(#\' #\,)))
                                   (princ char result)
                                   (progn
                                     (setf tilde nil)
                                     (princ (get-replacement char) result)))
                               (princ char result))))))))))
    (if (needs-parsing? string)
        (really-parse-format-string (coerce string 'simple-string))
        string)))
