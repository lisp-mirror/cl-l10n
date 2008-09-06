;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-l10n)

;;; http://www.unicode.org/reports/tr35/tr35-11.html#Date_Format_Patterns
(define-constant +date-pattern-characters/gregorian-calendar+ "GyYuQqMLlwWdDFgEecahHKkjmsSAzZvV" :test #'string=)

(defparameter +date-pattern-scanner/gregorian-calendar+
  (cl-ppcre:create-scanner (coerce (iter (for char :in-sequence +date-pattern-characters/gregorian-calendar+)
                                         (unless (first-time-p)
                                           (collect #\|))
                                         (nconcing (list #\( char #\+ #\))))
                                   'simple-string)))

;;; at the time these functions are called *locale* is bound the the locale from which this pattern comes from.

(defun compile-date-pattern/gregorian-calendar (pattern)
  (declare (type string pattern)
           (optimize speed))
  (macrolet ((piece-formatter (&body body)
               `(lambda (stream date year month day day-of-week)
                  (declare (ignorable date year month day day-of-week)
                           (type fixnum year month day day-of-week))
                  ,@body))
             (era-formatter (vector)
               `(piece-formatter
                 (bind ((era (if (< year 0) 0 1)))
                   (write-string (aref ,vector era) stream))))
             (invalid-number-of-directives ()
               `(error "Invalid number of consecutive '~A' directives in Gregorian calendar date format: \"~A\", piece \"~A\""
                       directive-character pattern piece)))
    (bind ((locale *locale*)
           (calendar (gregorian-calendar-of locale))
           (day-names (day-names-of calendar))
           (abbreviated-day-names (abbreviated-day-names-of calendar))
           (narrow-day-names (narrow-day-names-of calendar))
           (month-names (month-names-of calendar))
           (abbreviated-month-names (abbreviated-month-names-of calendar))
           (narrow-month-names (narrow-month-names-of calendar))
           (era-names (era-names-of calendar))
           (abbreviated-era-names (abbreviated-era-names-of calendar))
           (narrow-era-names (narrow-era-names-of calendar))
           (pieces (cl-ppcre:split +date-pattern-scanner/gregorian-calendar+ pattern :with-registers-p t :omit-unmatched-p t))
           (formatter-list
            (iter (for piece :in pieces)
                  (for length = (length piece))
                  (unless (zerop length)
                    ;; iter does not introduce new bindings, rebind for the closures
                    (let ((piece piece)
                          (length length)
                          (directive-character (char piece 0)))
                      (switch (directive-character :test #'char=)
                        (#\y (cond
                               ((= length 1)
                                (collect (piece-formatter (write-decimal-digits stream year))))
                               ((= length 2)
                                (collect (piece-formatter (write-decimal-digits stream year :maximum-digit-count 2))))
                               (t (collect (piece-formatter (write-decimal-digits stream year :minimum-column-count length))))))
                        (#\M (cond
                               ((= length 3)
                                (collect (piece-formatter (write-string (aref abbreviated-month-names month) stream))))
                               ((= length 4)
                                (collect (piece-formatter (write-string (aref month-names month) stream))))
                               ((= length 5)
                                (collect (piece-formatter (write-string (aref narrow-month-names month) stream))))
                               ((<= length 2)
                                (collect (piece-formatter (write-decimal-digits stream month :minimum-column-count length))))
                               (t
                                (invalid-number-of-directives))))
                        (#\E (cond
                               ((= length 4)
                                (collect (piece-formatter (write-string (aref day-names day) stream))))
                               ((= length 5)
                                (collect (piece-formatter (write-string (aref narrow-day-names day) stream))))
                               ((<= length 3)
                                (collect (piece-formatter (write-string (aref abbreviated-day-names day) stream))))
                               (t
                                (invalid-number-of-directives))))
                        (#\G (cond
                               ((= length 4)
                                (collect (era-formatter era-names)))
                               ((= length 5)
                                (unless narrow-era-names
                                  (error "Locale ~A does not have narrow era names for the Gregorian calendar" *locale*))
                                (collect (era-formatter narrow-era-names)))
                               ((<= length 3)
                                (collect (era-formatter abbreviated-era-names)))
                               (t
                                (invalid-number-of-directives))))
                        (#\d (unless (or (= 1 length)
                                         (= 2 length))
                               (invalid-number-of-directives))
                             (collect (piece-formatter (write-decimal-digits stream day :minimum-column-count length))))
                        (otherwise
                         (when (find directive-character +date-pattern-characters/gregorian-calendar+ :test #'char=)
                           (cerror "Print it unprocessed" "Unexpected or not yet implemented directive in Gregorian calendar date format: \"~A\", character ~A"
                                   pattern directive-character))
                         (collect (piece-formatter (write-string piece stream))))))))))
      (lambda (stream date)
        (local-time:with-decoded-timestamp (:year year :month month :day day :day-of-week day-of-week) date
          (dolist (formatter formatter-list)
            (funcall (the function formatter) stream date year month day day-of-week)))))))
