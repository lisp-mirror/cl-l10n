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

(defun %format-iterating-locales (stream locale-visitor fallback-fn)
  (declare (optimize speed)
           (type function locale-visitor fallback-fn))
  (bind ((to-string? nil))
    (cond
      ((null stream)
       (setf stream (make-string-output-stream))
       (setf to-string? t))
      ((eq stream t)
       (setf stream *standard-output*)))
    (block iterating-locales
      (do-current-locales locale
        (when (funcall locale-visitor stream locale)
          (return-from iterating-locales)))
      (funcall fallback-fn stream))
    (if to-string?
        (get-output-stream-string stream)
        stream)))

(defun format-date/gregorian-calendar (stream date &key (verbosity 'ldml:medium) pattern)
  (check-type pattern (or null string function))
  (setf verbosity (or (keyword-to-ldml verbosity) verbosity))
  (%format-iterating-locales
   stream
   (if pattern
       (lambda (stream locale)
         (declare (ignore locale))
         (funcall (etypecase pattern
                    (string
                     ;; NOTE: this code path is about 5 times slower and conses about 10 times more...
                     ;; OPTIMIZATION: we could implement some per-locale caching here, but it must be
                     ;; carefully keyed (a compiled lambda captures stuff at compile time from the compile time value of *locale*)
                     ;; and the cache must be properly locked to support threading.
                     (compile-date-time-pattern/gregorian-calendar pattern))
                    (function pattern))
                  stream date)
         t)
       (named-lambda date-format-locale-visitor (stream locale)
         (when-bind gregorian-calendar (gregorian-calendar-of locale)
           (bind ((formatter-entry (getf (date-formatters-of gregorian-calendar) verbosity))
                  (formatter (getf formatter-entry :formatter)))
             (if formatter
                 (progn
                   (funcall formatter stream date)
                   t)
                 nil)))))
   (named-lambda date-format-fallback (stream)
     (warn "No Gregorian calendar date formatter was found with verbosity ~S for locale ~A. Ignoring the locale and printing in a fixed simple format."
           verbosity (current-locale))
     (local-time:format-timestring stream date :format '((:year 4) #\- (:month 2) #\- (:day 2))))))

(defun format-number/currency (stream number currency-code &key (verbosity 'ldml:medium))
  ;; TODO support a :pattern keyword arg
  (setf verbosity (or (keyword-to-ldml verbosity) verbosity))
  (%format-iterating-locales
   stream
   (named-lambda currency-format-visitor (stream locale)
     (awhen (currency-formatter-of locale)
       (awhen (pattern-verbosity-list-of it)
         (awhen (or (getf it verbosity)
                    (getf it nil))
           (bind ((formatter (getf it :formatter)))
             (if formatter
                 (progn
                   (funcall formatter stream number currency-code)
                   t)
                 nil))))))
   (named-lambda currency-format-fallback (stream)
     (warn "No currency formatter was found with verbosity ~S for locale ~A. Ignoring the locale and printing in a fixed simple format."
           verbosity (current-locale))
     (cl:format stream "~A ~A" number currency-code))))

(defun %format-number-iterating-locales (stream number verbosity
                                         pattern pattern-compiler
                                         formatter-accessor formatter-name fallback-format-pattern)
  (setf verbosity (or (keyword-to-ldml verbosity) verbosity))
  (%format-iterating-locales
   stream
   (if pattern
       (lambda (stream locale)
             (declare (ignore locale))
             (funcall (etypecase pattern
                        (string
                         ;; NOTE: this code path is about 10 times slower and conses about 10 times more...
                         ;; OPTIMIZATION: we could implement some per-locale caching here, but it must be
                         ;; carefully keyed (a compiled lambda captures stuff at compile time from the compile time value of *locale*)
                         ;; and the cache must be properly locked to support threading.
                         (funcall pattern-compiler pattern))
                        (function pattern))
                      stream number)
             t)
       (named-lambda number-format-visitor (stream locale)
         (awhen (or (getf (funcall formatter-accessor locale) verbosity)
                    (getf (funcall formatter-accessor  locale) nil))
           (bind ((formatter (getf it :formatter)))
             (if formatter
                 (progn
                   (funcall formatter stream number)
                   t)
                 nil)))))
   (named-lambda number-format-fallback (stream)
     (warn "No ~A was found with verbosity ~S for locale ~A. Ignoring the locale and printing in a fixed simple format."
           formatter-name verbosity (current-locale))
     (cl:format stream fallback-format-pattern number))))

(defun format-number/decimal (stream number &key (verbosity 'ldml:medium) pattern)
  (%format-number-iterating-locales stream number verbosity
                                    pattern 'compile-number-pattern/decimal
                                    #'decimal-formatters-of "decimal number formatter" "~A"))

(defun format-number/percent (stream number &key (verbosity 'ldml:medium) pattern)
  (%format-number-iterating-locales stream number verbosity
                                    pattern 'compile-number-pattern/percent
                                    #'percent-formatters-of "percent number formatter" "~A%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customized format directives

(define-constant +directive-replacements+ '((#\M . "/cl-l10n:%format-currency/")
                                            (#\N . "/cl-l10n:%format-number/")
                                            (#\U . "/cl-l10n:%format-timestamp/")
                                            (#\L . "/cl-l10n:%format-date/"))
  :test 'equal)

(define-compiler-macro format (&whole form destination format-control &rest format-arguments)
  "Compiler macro to remove unnecessary calls to parse-format-string."
  (if (stringp format-control)
      `(cl:format ,destination ,(parse-format-string format-control) ,@format-arguments)
      form))

(defmacro formatter (format-string)
  (etypecase format-string
    (string `(cl:formatter ,(parse-format-string format-string)))))

(defun format (stream format-control &rest format-arguments)
  (apply #'cl:format stream
         (etypecase format-control
           (function format-control)
           (string (parse-format-string format-control)))
         format-arguments))

(defun shadow-format (&optional (package *package*))
  "Shadowing import the CL-L10N:FORMAT symbol into PACKAGE."
  (shadowing-import '(cl-l10n::format cl-l10n::formatter) package))

(defun %format-currency (stream number colon-modifier? no-thousand-separator &optional currency-code)
  ;; FIXME this is probably not going to work... should be able to pass in the currency code
  ;; somehow, but the format syntax only allows numeric or character arguments.
  (bind ((print-decimal-point? (not colon-modifier?))
         (print-thousand-separator? (not no-thousand-separator)))
    (unless print-thousand-separator?
      (cerror "ignore" "Turning off thousand separators is not yet supported"))
    (unless print-decimal-point?
      (cerror "ignore" "Turning off the decimal point is not yet supported"))
    (unless (ldml-symbol-p currency-code)
      (error "You need to specify the currency-code (e.g. 'ldml:usd) when formatting currencies"))
    (format-number/currency stream number currency-code))
  (values))

(defun %format-number (stream number colon-modifier? at-modifier?)
  (bind ((print-decimal-point? (not colon-modifier?))
         (print-thousand-separator? (not at-modifier?)))
    (unless print-decimal-point?
      (cerror "ignore" "Turning off the decimal point is not yet supported"))
    (unless print-thousand-separator?
      (cerror "ignore" "Turning off thousand separators is not yet supported"))
    (format-number/decimal stream number))
  (values))

(defun %format-date (stream date colon-modifier? at-modifier?)
  (declare (ignore colon-modifier? at-modifier?))
  (format-date/gregorian-calendar stream date)
  (values))

(defun %format-timestamp (stream timestamp colon-modifier? at-modifier?)
  (bind ((show-timezone? (not colon-modifier?))
         (in-utc-zone? at-modifier?)
         (timezone (if in-utc-zone? local-time:+utc-zone+ local-time:*default-timezone*))
         (format (if show-timezone?
                     '((:year 4) #\- (:month 2) #\- (:day 2) #\T
                       (:hour 2) #\: (:min 2) #\: (:sec 2) #\.
                       (:usec 6) :gmt-offset-or-z)
                     '((:year 4) #\- (:month 2) #\- (:day 2) #\T
                       (:hour 2) #\: (:min 2) #\: (:sec 2) #\.
                       (:usec 6)))))
    ;; TODO KLUDGE finish this part: should use the cldr date formatter
    ;; (format-date/gregorian-calendar stream timestamp)
    (local-time:format-timestring stream timestamp :timezone timezone :format format))
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
