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

(defun format-date/gregorian-calendar (stream date &key (verbosity 'ldml:medium))
  (declare (optimize speed))
  (setf verbosity (or (keyword-to-ldml verbosity) verbosity))
  (bind ((to-string? (null stream)))
    (unless stream
      (setf stream (make-string-output-stream)))
    (block iterating-locales
      (do-current-locales locale
        (when-bind gregorian-calendar (gregorian-calendar-of locale)
          (bind ((formatter-entry (getf (date-formatters-of gregorian-calendar) verbosity))
                 (formatter (getf formatter-entry :formatter)))
            (when formatter
              (funcall formatter stream date)
              (return-from iterating-locales)))))
      (warn "No Gregorian calendar date formatter was found with verbosity ~S for locale ~A. Ignoring the locale and printing in a fixed simple format."
            verbosity (current-locale))
      (local-time:format-timestring stream date :format '((:year 4) #\- (:month 2) #\- (:day 2))))
   (if to-string?
       (get-output-stream-string stream)
       stream)))

(defun format-number/decimal (stream number &key (verbosity 'ldml:medium))
  (declare (optimize speed))
  (setf verbosity (or (keyword-to-ldml verbosity) verbosity))
  (bind ((to-string? (null stream)))
    (unless stream
      (setf stream (make-string-output-stream)))
    (block iterating-locales
      (do-current-locales locale
        (awhen (or (getf (decimal-formatter-of locale) verbosity)
                   (getf (decimal-formatter-of locale) nil))
          (when-bind formatter (getf it :formatter)
            (funcall formatter stream number)
            (return-from iterating-locales))))
      (warn "No decimal number formatter was found with verbosity ~S for locale ~A. Ignoring the locale and printing in a fixed simple format."
            verbosity (current-locale))
      (format *standard-output* "~A" number))
   (if to-string?
       (get-output-stream-string stream)
       stream)))


#|

TODO revive the same functionality

;; Number
(defun get-sign (arg locale)
  (cond ((plusp arg) (locale-positive-sign locale))
        ((minusp arg) (locale-negative-sign locale))
        (t "")))

(defvar *float-digits* 2
  "Used when all values after the decimal point are zero to
determine the number of zero's to print")

(defun fix-float-string (string size)
  "Pads the string with trailing zero's if it is smaller than size"
  (with-output-to-string (stream)
    (let ((length (length string)))
      (write-string string stream :end (min size length))
      (when (< length size)
        (dotimes (x (- size length))
          (write-char #\0 stream))))))

(defun format-number (stream arg no-dp no-ts &optional (locale (current-locale)))
  (let ((locale (locale locale))
        (float-part (float-part (coerce (abs arg) 'double-float))))
    (cl:format stream
               (getf (printers locale) (if no-ts :number-no-ts :number-ts))
               (get-sign arg locale)
               (truncate (abs arg))
               (unless (and (string= "" float-part) no-dp)
                 (list (locale-decimal-point locale)
                       (if *float-digits*
                           (fix-float-string float-part *float-digits*)
                           float-part))))
    (values)))

(defun print-number (number &key (stream *standard-output*)
                            no-ts no-dp (locale (current-locale)))
  (format-number stream number no-dp no-ts locale)
  number)


;; Money
(defvar *default-round-mode* :round)
  
(defun round-money (float frac-digits &key (round-mode *default-round-mode*))
  (let ((round-fn (ecase round-mode
                    (:round #'fround)
                    (:down #'ffloor)
                    (:up #'fceiling))))
    (let ((size (expt 10 frac-digits)))
      (/ (funcall round-fn (* float size)) size))))

(defun get-money-printer (minusp no-ts)
  (if minusp
      (if no-ts
          :money-n-no-ts
          :money-n-ts)
      (if no-ts
          :money-p-no-ts
          :money-p-ts)))

(defun format-money (stream arg use-int-sym no-ts &optional (locale (current-locale)))
  (let* ((locale (locale locale))
         (frac-digits (max (if use-int-sym
                               (locale-int-frac-digits locale)
                               (locale-frac-digits locale))
                           0))
         (val-to-print (round-money (abs (coerce arg 'double-float))
                                    frac-digits))
         (float-part (float-part val-to-print))
         (sym (if use-int-sym
                  (locale-int-curr-symbol locale)
                  (locale-currency-symbol locale)))
         (prec (= 1 (locale-n-cs-precedes locale))))
    (cl:format stream 
               (getf (printers locale) 
                     (get-money-printer (minusp arg) no-ts))
               (if prec sym "")
               (truncate (abs val-to-print))
               (unless (zerop frac-digits)
                 (list (locale-mon-decimal-point locale)
                       (fix-float-string float-part frac-digits)))
               (if prec "" (trim sym))))
  (values))

(defun print-money (num &key (stream *standard-output*) use-int-sym no-ts
                        (locale (current-locale)))
  (format-money stream num use-int-sym no-ts locale)
  num)

;; ;; Time and date printing.
(defun get-time-format-string (locale show-date show-time)
  (cond ((and show-time show-date)
         (locale-d-t-fmt locale))
        ((and (not show-date) (not show-time))
         (if (string= "" (locale-t-fmt-ampm locale))
             (locale-t-fmt locale)
             (locale-t-fmt-ampm locale)))
        (show-time (locale-t-fmt locale))
        (show-date (locale-d-fmt locale))))

(defvar *time-formatters* (make-hash-table))
(defmacro def-formatter (sym &body body)
  "Creates a function with BODY which can be looked up using lookup-formatter
   using the character SYM." 
  (let ((name (gensym (strcat "FORMATTER-" sym))))
    `(flet ((,name (stream locale ut sec min hour date month year day 
                           daylight-p zone)
              (declare (ignorable stream locale ut sec min hour date month 
                                  year day daylight-p zone))
              ,@body))
       (setf (gethash ,sym *time-formatters*)
             #',name))))

(defun lookup-formatter (char)
  (or (gethash char *time-formatters*)
      (locale-error "No format directive for char ~S." char)))

(defun princ-pad-val (val stream &optional (pad "0") (size 2))
  (declare (type stream stream) (optimize speed)
           (type fixnum val size))
  (assert (not (minusp val)) (val) "Value ~A cannot be smaller than 0." val)
  (cond ((zerop val)
         (dotimes (x (1- size))
           (princ pad stream))
         (princ 0 stream))
        (t       
         (loop with stop-value = (expt 10 size)
               for x of-type integer = (* val 10) then (* x 10)
               until (>= x stop-value) do
               (princ pad stream))
         (princ val stream))))
      
(defun last-2-digits (val)
  (mod val 100))

(def-formatter #\a
  (let ((day (1+ day)))
    (if (> day 6) (decf day 7))
    (princ (nth day (locale-abday locale)) stream)))

(def-formatter #\A
  (let ((day (1+ day)))
    (if (> day 6) (decf day 7))
    (princ (nth day (locale-day locale)) stream)))

(def-formatter #\b
  (cl:format stream (cl:formatter "~A") 
             (nth (1- month) (locale-abmon locale))))

(def-formatter #\B
  (cl:format stream (cl:formatter "~A")
             (nth (1- month) (locale-mon locale))))

(def-formatter #\c
  (print-time-string (locale-d-t-fmt locale) stream ut locale))

(def-formatter #\C
  (princ-pad-val (truncate (/ year 100)) stream))

(def-formatter #\d
  (princ-pad-val date stream))

(def-formatter #\D
  (print-time-string "%m/%d/%y" stream ut locale))

(def-formatter #\e 
  (princ-pad-val date stream " "))

(def-formatter #\F
  (print-time-string "%Y-%m-%d" stream ut locale))
                       
(def-formatter #\g
  (print-time-string "%y" stream ut locale))

(def-formatter #\G
  (print-time-string "%Y" stream ut locale))

(def-formatter #\h
  (princ (nth (1- month) (locale-abmon locale))
         stream))

(def-formatter #\H
  (princ-pad-val hour stream))

(def-formatter #\I
  (princ-pad-val (if (> hour 12) (- hour 12) hour) stream))

(defvar *mon-days* 
  '(31 28 31 30 31 30 31 31 30 31 30 31))

(defvar *mon-days-leap* 
  (substitute 29 28 *mon-days*))

(defun leap-year-p (year)
  (cond ((zerop (mod year 400)) t)
        ((zerop (mod year 100)) nil)
        ((zerop (mod year 4)) t)
        (t nil)))

(defun day-of-year (date month year)
  (let ((total 0))
    (loop repeat (1- month) 
          for x in (if (leap-year-p year) *mon-days-leap* *mon-days*) do
          (incf total x))
    (incf total date)))

(def-formatter #\j 
  (princ-pad-val (day-of-year date month year) stream "0" 3))

(def-formatter #\k
  (princ-pad-val hour stream " "))

(def-formatter #\l
  (princ-pad-val (if (> hour 12) (- hour 12) hour) stream
                 " "))

(def-formatter #\m
  (princ-pad-val month stream))

(def-formatter #\M
  (princ-pad-val min stream))

(def-formatter #\n
  (princ #\Newline stream))

(def-formatter #\N
  (princ "000000000" stream))

(defun get-am-pm (hour locale)
  (funcall (if (< hour 12) #'car #'cadr)
           (locale-am-pm locale)))

(def-formatter #\p
  (princ (string-upcase (get-am-pm hour locale))
         stream))

(def-formatter #\P
  (princ (string-downcase (get-am-pm hour locale))
         stream))

(def-formatter #\r
  (print-time-string "%H:%M:%S %p" stream ut locale))

(def-formatter #\R
  (print-time-string "%I:%M" stream ut locale))

(defvar *1970-01-01* (encode-universal-time 0 0 0 01 01 1970 0))

(def-formatter #\s
  (princ (- ut *1970-01-01*) stream))

(def-formatter #\S
  (princ-pad-val sec stream))

(def-formatter #\t
  (princ #\Tab stream))

(def-formatter #\T
  (print-time-string "%H:%M:%S" stream ut locale))

(def-formatter #\u 
  (let ((day (1+ day)))
    (when (> day 7) (decf day 7))
    (princ day stream)))

;; FIXME
(def-formatter #\U
  (locale-error "Unsupported time format directive ~S." #\U))

;; FIXME
(def-formatter #\V
  (locale-error "Unsupported time format directive ~S." #\V))

(def-formatter #\w
  (let ((day (1+ day)))
    (when (>= day 7) (decf day 7))
    (princ day stream)))

;; FIXME
(def-formatter #\W
  (locale-error "Unsupported time format directive ~S." #\W))

(def-formatter #\x
  (print-time-string (locale-d-fmt locale) stream ut locale))

(def-formatter #\X
  (print-time-string (locale-t-fmt locale) stream ut locale))

(def-formatter #\y
  (princ-pad-val (last-2-digits year) stream))

(def-formatter #\Y
  (princ year stream))


; This was all severely broken until I took a look 
; at Daniel Barlow's net-telent-date package, 
; which is a must read for anyone working with dates 
; in CL.
(def-formatter #\z 
  (let ((d-zone (if daylight-p (1- zone) zone)))
    (multiple-value-bind (hr mn) (truncate (abs d-zone))
      (princ (if (<= d-zone 0) #\+ #\-) stream)
      (cl:format stream (cl:formatter "~2,'0D~2,'0D")
                 hr (floor (* 60 mn))))))

;; Probably Should be printing SAST rather than +0200
;; but since all these wonderful codes are not 
;; standardized i'm keeping it the same as %z
;; so that we can parse it back.
;; eg. Does IST mean 'Israeli Standard Time','Indian Standard Time' 
;;     or 'Irish Summer Time' ? 
(def-formatter #\Z
  (print-time-string "%z" stream ut locale))

(defvar *time-zone*)

(defun format-time (stream ut show-date show-time &optional (locale (current-locale)) fmt time-zone)
  (let ((locale (locale locale))
        (*time-zone* (or time-zone (nth-value 8 (decode-universal-time ut)))))
    (print-time-string (or fmt (get-time-format-string locale 
                                                    show-date show-time))
                       stream ut locale))
  (values))

(defun print-time-string (format-string stream ut locale)
  (declare (optimize speed) (type simple-string format-string))
  (let ((values (multiple-value-list (decode-universal-time ut *time-zone*))))
    (loop for x across format-string 
          with perc = nil 
          with in-dot = nil do
          (case x 
            (#\% (if perc 
                     (progn (princ #\% stream) (setf perc nil))
                     (setf perc t)))
            ;; see compute-order in load-locale.lisp
            ;; for why this line is here.
            (#\. (if perc (setf in-dot t) (princ x stream)))
            (#\1 (if (and perc in-dot) 
                     (setf in-dot nil)
                     (princ x stream)))
            (#\E (unless perc (princ x stream)))
            (t (if perc
                   (progn (apply (the function (lookup-formatter x))
                                 stream locale ut values)
                     (setf perc nil))
                   (princ x stream)))))))

(defun print-time (ut &key show-date show-time (stream *standard-output*)
                      (locale (current-locale)) fmt time-zone)
  (format-time stream ut show-date show-time locale fmt time-zone)
  ut)

||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customized format directives

(define-constant +directive-replacements+ '((#\M . "/cl-l10n:%format-money/")
                                            (#\U . "/cl-l10n:%format-timestamp/")
                                            (#\L . "/cl-l10n:%format-date/")
                                            (#\N . "/cl-l10n:%format-number/"))
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
  (shadowing-import '(cl-l10n::format cl-l10n::formatter) package))

(defun needs-parsing? (string)
  (declare (optimize speed)
           (type string string))
  (cl-ppcre:scan (load-time-value (cl-ppcre:create-scanner
                                   (cl:format nil "~~[@V,:]*[~{~A~^|~}]" (mapcar 'first +directive-replacements+))))
                 (string-upcase string)))

(defun parse-format-string (string)
  (if (needs-parsing? string)
      (really-parse-format-string (coerce string 'simple-string))
      string))

(defun %format-date (stream date colon-modifier? at-modifier?)
  (declare (ignore colon-modifier? at-modifier?))
  (format-date/gregorian-calendar stream date))

(defun really-parse-format-string (string)
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
                      (progn
                        (setf tilde nil)
                        (princ (get-replacement char) result))
                      (princ char result)))))))))
