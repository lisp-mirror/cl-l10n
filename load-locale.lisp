;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package :cl-l10n)

(defparameter *ignore-categories*
  (list "LC_CTYPE" "LC_COLLATE"))


;; Add a restart here?
(defun locale (loc-name &key (use-cache t) (errorp t) (loader nil))
  "Find locale named by the string LOC-NAME. If USE-CACHE
is non-nil forcefully reload the locale from *locale-path* else
the locale is first looked for in *locales*. If ERRORP is non-nil
signal a warning rather than an error if the locale file cannot be found.
If LOADER is non-nil skip everything and call loader with LOC-NAME."
  (let ((name (aif (position #\. loc-name)
                   (subseq loc-name 0 it)
                   loc-name)))
    (acond ((and (not name) (not errorp)) nil)
           ((and use-cache (get-locale name)) it)
           (loader (setf (get-locale name) (funcall loader name)))
           ((probe-file (merge-pathnames *locale-path* name))
            (setf (get-locale name) (load-locale name)))
           (t (funcall (if errorp #'error #'warn)
                       "Can't find locale ~A." name)))))

(defvar *locale-type* 'locale
  "The class of loaded locales.")

(defvar *category-type* 'category
  "The class of loaded categories")

(deftype locale-descriptor ()
  `(or locale string symbol))

(defun locale-des->locale (loc)
  "Turns a locale descriptor(a string, symbol or locale) into an
actual locale object."
  (check-type loc locale-descriptor)
  (etypecase loc
    (locale loc)
    (string (locale loc))
    (symbol (locale (string loc)))))

(defun load-locale (name)
  (let ((path (merge-pathnames *locale-path* name)))
    (cl:format *debug-io* "~&;; Loading locale from ~A.~%" path)
    (let ((locale (make-instance *locale-type* :name name)))
      (with-open-file (stream path
                       :external-format #+(and sbcl sb-unicode) :latin1 
                                        #-(and sbcl sb-unicode) :default)
        (multiple-value-bind (escape comment) (munge-headers stream)
          (loop for header = (next-header stream)
                while header do
            (when-let (cat (make-category locale header 
                                          (parse-category header stream
                                                          escape comment)))
              (setf (get-category locale header) cat)))))
      (add-printers locale)
      (add-parsers locale)
      locale)))

(defun load-all-locales (&key (path *locale-path*) (ignore-errors nil) (use-cache nil))
  "Load all locale found in pathname designator PATH."
  (let ((*locale-path* path))
    (dolist (x (list-directory *locale-path*))
      (when (and (not (directory-pathname-p x)) (pathname-name x))
        (let ((locale (pathname-name x)))
          (with-simple-restart (continue "Ignore locale ~A." x)
            (handler-bind ((error (lambda (&optional c)
                                    (when ignore-errors
                                      (warn "Failed to load locale ~S, Ignoring." locale)
                                      (invoke-restart (find-restart 'continue c))))))
              (locale locale :use-cache use-cache))))))))

(defvar *default-thousands-sep* #\,)

(defun thousands-sep-char (sep)
  (if (> (length sep) 0)
      (schar sep 0)
      *default-thousands-sep*))

(defun create-number-fmt-string (locale no-ts)
  (cl:format nil "~~A~~,,'~A,~A~A~~{~~A~~}" 
             (thousands-sep-char (locale-thousands-sep locale))
             (locale-grouping locale)
             (if no-ts "D" ":D")))

(defun get-descriptors (minusp locale)
  (if minusp 
      (values (locale-n-sep-by-space locale)
              (= 1 (locale-n-cs-precedes locale))
              (locale-n-sign-posn locale)
              (locale-negative-sign locale))
      (values (locale-p-sep-by-space locale)
              (= 1 (locale-p-cs-precedes locale))
              (locale-p-sign-posn locale)
              (locale-positive-sign locale))))

(defun create-money-fmt-string (locale no-ts minusp)
  (multiple-value-bind (sep-by-space prec spos sign) 
      (get-descriptors minusp locale)
    (let ((sym-sep (if (zerop sep-by-space) "" " ")))
      (with-output-to-string (stream)
        ;; sign and sign separator
        (when (or* (= spos 0 1 3))
          (princ (if (zerop spos) "(" sign) stream)
          (when (= 2 sep-by-space)
            (princ #\Space stream)))
        ;; Sym and seperator
        (princ "~A" stream)
        (when prec
          (princ sym-sep stream))
        ;; Actual number
        (cl:format stream "~~,,'~A,~A~A~~{~~A~~}" 
                   (thousands-sep-char (locale-mon-thousands-sep locale))
                   (locale-mon-grouping locale)
                   (if no-ts "D" ":D"))
        (unless prec
          (princ sym-sep stream))
        (princ "~A" stream)
        (when (or* (= spos 0 2 4))
          (when (= 2 sep-by-space)
            (princ #\Space stream))
          (princ (if (zerop spos) ")" sign) stream))))))

(defun add-printers (locale)
  "Creates monetary and numeric format strings for locale LOCALE."
  (when (and (get-category locale "LC_MONETARY")
             (get-category locale "LC_NUMERIC"))
    ;; otherwise its an include locale (tranlit* etc)
    (setf (printers locale)
          (nconc (list :number-no-ts
                       (create-number-fmt-string locale t))
                 (list :number-ts
                       (create-number-fmt-string locale nil))
                 (list :money-p-no-ts
                       (create-money-fmt-string locale t nil))
                 (list :money-p-ts
                       (create-money-fmt-string locale nil nil))
                 (list :money-n-no-ts
                       (create-money-fmt-string locale t t))
                 (list :money-n-ts
                       (create-money-fmt-string locale nil t))
                 (printers locale)))))

(defun day-element-p (x)
  (member x '(#\d #\e)))

(defun month-element-p (x)
  (member x '(#\m #\b #\B)))

(defun year-element-p (x)
  (member x '(#\y #\Y)))

(defun element-type (char)
  (cond ((day-element-p char) 'day)
        ((month-element-p char) 'month)
        ((year-element-p char) 'year)))

(defvar date-dividers '(#\\ #\/ #\-))

;; FIXME
;; this effort definitely doesn't cover
;; every single case but it will do for now.
(defun locale-date-month-order (locale)
  (let ((fmt (locale-d-fmt locale)))
    (cond ((string= fmt "%D") '(month day year))
          ((string= fmt "%F") '(year month day))
          (t (compute-order fmt)))))

(defun compute-order (fmt)
  (let ((res nil))
    (loop for char across fmt 
          with perc = nil 
          with in-dot = nil do
          (cond ((char= char #\%) (setf perc (not perc)))
                ((member char date-dividers) nil)
                ((and perc (char= char #\.))  (setf in-dot t))
                ((and perc in-dot (char= char #\1))  
                 (setf in-dot nil))
                (perc (unless (char= char #\E)
                        ;; some locales (eg lo_LA) have this funny E before
                        ;; various time format designators. Debian 
                        ;; treats this as if it wasn't there so neither do we.
                        (let ((val (element-type char)))
                          (when val (push val res))
                          (setf perc nil))))))
    (nreverse res)))

(defun add-parsers (locale)
  (when (get-category locale "LC_TIME")
    (destructuring-bind (first second third)
        (locale-date-month-order locale)
      (setf (parsers locale)
            (list `((noon-midn) (weekday) ,first (date-divider) ,second (date-divider) ,third (noon-midn))
                  `((weekday) ,first (date-divider) ,second (date-divider) ,third hour (time-divider) minute
                    (time-divider) (secondp) (am-pm) (date-divider) (zone))
                  `(hour (time-divider) minute (time-divider) (secondp) (am-pm) (weekday) ,first (date-divider) 
                         (secondp) (date-divider) ,third (date-divider) (zone)))))))

(defvar *category-loaders*
  '(("LC_IDENTIFICATION" . load-identification)
    ("LC_MONETARY" . load-category)
    ("LC_NUMERIC" . load-category)
    ("LC_TIME" . load-category)
    ("LC_MESSAGES" . load-category)
    ("LC_PAPER" . load-category)
    ("LC_TELEPHONE" . load-category)
    ("LC_MEASUREMENT" . load-category)
    ("LC_NAME" . load-category)
    ("LC_ADDRESS" . load-category))
  "Map of category names to the function which will load them.")

(defun get-loader (name)
  (cdr (assoc name *category-loaders* :test #'string=)))

(defun make-category (locale name vals)
  (when-let (loader (get-loader name))
    (funcall loader locale name vals)))

(defgeneric load-category (locale name vals)
  (:documentation "Load a category for LOCALE using VALS.")
  (:method ((locale locale) (name string) (vals category))
    vals)
  (:method ((locale locale) (name string) (vals cons))
    (let ((cat (make-instance *category-type* :name name)))
      (dolist (x vals)
        (setf (category-value cat (car x)) (cdr x)))
      cat)))

(defvar *id-vals* 
  '(("title" . title)
    ("source" . source)
    ("language" . language)
    ("territory" . territory)
    ("revision" . revision)
    ("date" . date)
    ("categories" . categories)))

(defun load-identification (locale name vals)
  (declare (ignore name))
  (dolist (x *id-vals*)
    (aif (cdr (assoc (car x) vals :test #'string=))
         (setf (slot-value locale (cdr x)) 
               (remove #\" it)))))

(defun line-comment-p (line comment)
  (or (string= line "")
      (and (> (length line) 0)
           (char= (schar line 0) comment))))
      

(defun copy-category (cat line)
  (let ((from (trim (subseq line (position #\Space line))
                    (cons #\" *whitespace*))))
    (handler-case (let* ((locale (locale from)))
                    (or (get-category locale cat)
                        (locale-error "No category ~A in locale ~A." 
                                      cat from)))
      (error (c) (locale-error "Unable to copy Category ~A from ~A. ~A."
                               cat from c)))))

(defun parse-category (name stream escape comment)
  (let ((end (mkstr "END " name))
        (ret nil))
    (loop for line = (read-line stream nil stream)
          until (eq line stream) do
      (cond ((line-comment-p line comment))
            ((search end line) (return-from parse-category ret))
            ((search "END" line) 
             (locale-error "End of wrong block reached ~S. Expected ~S." 
                    line end))
            ((and (> (length line) 3) (search "copy" line :end2 4))
             (return-from parse-category 
               (copy-category name line)))
            (t (push (get-value line stream escape) ret))))))

(defun munge-headers (stream)
  (let ((escape #\\) (comment-char #\#))
    (loop for line = (read-line stream nil stream)
          for i from 1 do
          ;; HACK We assume that if the escape and comment
          ;; lines don't appear right away that they don't exist
          ;; This is to work around lispworks being unable
          ;; to unread a line of text character by character.
      (cond ((> i 3) (return nil))
            ((line-comment-p line comment-char))
            ((search "escape_char" line)
             (setf escape 
                   (schar (cdr (get-value line stream escape)) 0)))
            ((search "comment_char" line)
             (setf comment-char
                   (schar (cdr (get-value line stream escape)) 0)))))
    (values escape comment-char)))

(defun get-full-line (line stream escape)
  (let ((length (length line)))
    (if (char= (elt line (1- length)) escape)
        (let ((next-line (read-line stream nil stream)))
          (if (eq next-line stream)
              (locale-error "EOF Looking for next line of ~A." line)
              (get-full-line (concatenate 
                              'string 
                              (subseq line 0 (1- length))
                              (trim next-line))
                             stream
                             escape)))
        line)))

(defun real-value (string)
  (loop for char across string
        with in-special = nil
        with result = ()
        with special-val = () do
        (cond ((eql char #\"))
              ((eql char #\<) (setf in-special t))
              ((and in-special (eq char #\>))
               (push (code-char 
                      (parse-integer (coerce (cdr (nreverse special-val)) 'string)
                                     :radix 16))
                     result)
               (setf in-special nil 
                     special-val nil))
              (in-special (push char special-val))
              (t (push char result)))
        finally (return (coerce (nreverse result)
                                #-lispworks 'string 
                                #+lispworks 'lw:text-string))))

(defvar *split-scanner* 
  (cl-ppcre:create-scanner '(:char-class #\;)))
                         
(defun parse-value (val)
  (let ((all-vals (split *split-scanner* val)))
    (if (singlep all-vals)
        (real-value (car all-vals))
        (mapcar #'real-value all-vals))))

(defun get-value (line stream escape)
  "Return a cons containing the key of line and its value. 
   Honors lines ending with ESCAPE"
  (let* ((line (get-full-line line stream escape))
         (first-space (position-if #'(lambda (x)
                                       (or* (char= x #\Space #\Tab))) 
                                   line)))
    (if (null first-space)
        (locale-error "No Space in line ~A." line)
        (cons (trim (subseq line 0 first-space))
              (parse-value (trim (subseq line first-space)))))))

(defun next-header (stream)
  (loop for line = (read-line stream nil stream)
        until (eq line stream) do
    (if (and (> (length line) 3) (search "LC_" line :end2 3)
             (notany #'(lambda (x)
                         (search x line :test #'string=))
                     *ignore-categories*))
        (return-from next-header (trim line)))))

(defun load-default-locale ()
  (setf *locale* (get-default-locale)))

(defun get-default-locale () 
  (or (locale (getenv "CL_LOCALE") :errorp nil)
      (locale (getenv "LC_CTYPE") :errorp nil)
      (locale "POSIX")))



;; EOF