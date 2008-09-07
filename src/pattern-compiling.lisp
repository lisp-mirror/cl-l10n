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

(defun tokenize-format-pattern (pattern scanner)
  (bind ((quote-pieces (remove-if #'zerop
                                  (cl-ppcre:split "(')" pattern :with-registers-p t :omit-unmatched-p t)
                                  :key #'length)))
    (iter outer
          (generate (piece following-piece) :on quote-pieces)
          (next piece)
          (when (string= piece "")
            (next-iteration))
          (cond
            ((and (string= piece "'")
                  (string= following-piece "'"))
             (next piece)
             (collect "'"))
            ((string= piece "'")
             (collect (apply #'concatenate 'string
                             (iter inner
                                   (in outer (next piece))
                                   (until (and (string= piece "'")
                                               (not (string= following-piece "'"))))
                                   (if (and (string= piece "'")
                                            (string= following-piece "'"))
                                       (progn
                                         (in outer (next piece))
                                         (collect "'"))
                                       (collect piece))))))
            ((not (zerop (length piece)))
             (collect (cl-ppcre:split scanner piece :with-registers-p t :omit-unmatched-p t)))))))

(defun compile-date-pattern/gregorian-calendar (pattern)
  (declare (type string pattern)
           (optimize speed))
  (macrolet ((piece-formatter (&body body)
               `(lambda (stream date year month day day-of-week)
                  (declare (ignorable date year month day day-of-week)
                           (type fixnum year month day day-of-week))
                  ,@body))
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
           (outer-pieces (tokenize-format-pattern pattern +date-pattern-scanner/gregorian-calendar+))
           (formatter-list (list)))
      (flet ((collect (formatter)
               (push formatter formatter-list)))
        (dolist (outer-piece outer-pieces)
          (unless (zerop (length outer-piece))
            ;;(format *debug-io* "processing outer piece ~S~%" outer-piece)
            (if (consp outer-piece)
                (dolist (piece outer-piece)
                  (bind ((length (length piece)))
                    (unless (zerop length)
                      ;;(format *debug-io* "  processing inner piece ~S~%" piece)
                      (bind ((directive-character (char piece 0)))
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
                                  (collect (piece-formatter (write-string (aref day-names day-of-week) stream))))
                                 ((= length 5)
                                  (collect (piece-formatter (write-string (aref narrow-day-names day-of-week) stream))))
                                 ((<= length 3)
                                  (collect (piece-formatter (write-string (aref abbreviated-day-names day-of-week) stream))))
                                 (t
                                  (invalid-number-of-directives))))
                          (#\G (macrolet ((era-formatter (vector)
                                            `(piece-formatter
                                              (bind ((era (if (< year 0) 0 1)))
                                                (write-string (aref ,vector era) stream)))))
                                 (cond
                                   ((= length 4)
                                    (collect (era-formatter era-names)))
                                   ((= length 5)
                                    (unless narrow-era-names
                                      (error "Locale ~A does not have narrow era names for the Gregorian calendar" *locale*))
                                    (collect (era-formatter narrow-era-names)))
                                   ((<= length 3)
                                    (collect (era-formatter abbreviated-era-names)))
                                   (t
                                    (invalid-number-of-directives)))))
                          (#\d (unless (or (= 1 length)
                                           (= 2 length))
                                 (invalid-number-of-directives))
                               (collect (piece-formatter (write-decimal-digits stream day :minimum-column-count length))))
                          (otherwise
                           (when (find directive-character +date-pattern-characters/gregorian-calendar+ :test #'char=)
                             (cerror "Print it unprocessed" "Unexpected or not yet implemented directive in Gregorian calendar date format: \"~A\", character ~A"
                                     pattern directive-character))
                           (collect (piece-formatter (write-string piece stream)))))))))
                (collect (piece-formatter (write-string outer-piece stream)))))))
      (nreversef formatter-list)
      (lambda (stream date)
        (local-time:with-decoded-timestamp (:year year :month month :day day :day-of-week day-of-week) date
          (dolist (formatter formatter-list)
            (funcall (the function formatter) stream date year month day day-of-week)))))))