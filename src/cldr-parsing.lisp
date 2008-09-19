;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-l10n)

;;; see http://unicode.org/cldr/

(defvar *parser*)

(defclass cldr-parser (flexml:flexml-builder)
  ((source-xml-file :initarg :source-xml-file :accessor source-xml-file-of)))

(defun make-cldr-parser ()
  (make-instance 'cldr-parser :default-package "CL-L10N.LDML"))

(defparameter *cldr-root-directory* (project-relative-pathname "cldr/main/"))

(defun cldr-pathname-for (locale-name)
  (merge-pathnames (concatenate 'string locale-name ".xml") *cldr-root-directory*))

(defun parse-cldr-file (name)
  (bind ((parser (make-cldr-parser))
         (*locale* nil)
         (source-xml-file (cldr-pathname-for name)))
    (bind ((*parser* parser))
      (setf (source-xml-file-of *parser*) source-xml-file)
      (cxml:parse source-xml-file *parser* :entity-resolver 'cldr-entity-resolver)
      (process-ldml-node nil (flexml:root-of *parser*))
      (setf (precedence-list-of *locale*) (compute-locale-precedence-list *locale*)))
    (assert *locale*)
    (unless (boundp '*parser*)
      ;; we are a non-recursive invocation of PARSE-CLDR-FILE, so do
      ;; some postprocessing. these operations must be postponed
      ;; because they need the locale precedence list, which means
      ;; that parsing needs to be called recursively.
      (map nil 'ensure-locale-is-initialized (precedence-list-of *locale*)))
    (values *locale* parser)))

(defun ensure-locale-is-initialized (locale)
  (unless (initialized-p locale)
    (setf (initialized-p locale) t)
    (unless (equal (language-of locale) "root")
      (bind ((*locale* (list locale))
             (gregorian-calendar (gregorian-calendar-of locale)))
        (when gregorian-calendar
          (iter (for (verbosity nil) :on (date-formatters-of gregorian-calendar) :by #'cddr)
                ;; FIXME this should apply locale inheritance when finds the format pattern, then compile the pattern
                ;; but store it to the head of the locale precedence list. some locales don't provide all the date
                ;; format patterns, so we reach the root locale, that in turn has no day/month/etc names, so their
                ;; date formatters are not instantiated. exampl locale: ii, in (it doesn't even have a gregorian calendar).
                (bind (((&key formatter pattern &allow-other-keys) (getf (date-formatters-of gregorian-calendar) verbosity)))
                  (assert (eq formatter 'dummy-formatter))
                  (setf (getf (date-formatters-of gregorian-calendar) verbosity)
                        (list :formatter (compile-date-pattern/gregorian-calendar pattern)
                              :pattern pattern)))))))))

(defun dummy-formatter (&rest args)
  (declare (ignore args))
  (error "Seems like the CLDR file parsing has a bug. This dummy formatter should have been replaced in the postprocessing phase."))

(define-condition cldr-parser-warning (simple-warning)
  ((parser :initform *parser* :initarg :parser :accessor parser-of))
  (:report (lambda (c stream)
             (bind ((source-xml-file (source-xml-file-of (parser-of c))))
               (apply #'format stream (concatenate 'string "~A.~A: " (simple-condition-format-control c))
                      (pathname-name source-xml-file)
                      (pathname-type source-xml-file)
                      (simple-condition-format-arguments c))))))

(defun cldr-parser-warning (message &rest args)
  (warn 'cldr-parser-warning :format-control message :format-arguments args))

(defun cldr-entity-resolver (public-id system-id)
  (declare (ignore public-id))
  (bind ((file (cond
                 ((puri:uri= system-id (load-time-value
                                        (puri:parse-uri "http://www.unicode.org/cldr/dtd/1.6/ldml.dtd")))
                  "cldr/ldml.dtd")
                 ((puri:uri= system-id (load-time-value
                                        (puri:parse-uri "http://www.unicode.org/cldr/dtd/1.6/cldrTest.dtd")))
                  "cldr/cldrTest.dtd"))))
    (when file
      (open (project-relative-pathname file) :element-type '(unsigned-byte 8) :direction :input))))

(defmethod flexml:class-name-for-node-name ((parser cldr-parser) namespace-uri package (local-name string) qualified-name)
  (let ((class-name (find-symbol (string-upcase (camel-case-to-hyphened local-name)) :ldml)))
    (if (find-class class-name nil)
        class-name
        'ldml:node)))

(defclass ldml:node (flexml:flexml-node)
  ())

(defmethod print-object ((self ldml:node) stream)
  (let ((*print-circle* nil))
    (print-unreadable-object (self stream :type t :identity t)
      (princ (flexml::local-name-of self) stream))))

(macrolet ((define (&body entries)
             `(progn
                ,@(iter (for entry :in entries)
                        (destructuring-bind (name &optional supers &body slots)
                            (ensure-list entry)
                          (unless supers
                            (setf supers '(ldml:node)))
                          (collect `(defclass ,name ,supers
                                      (,@slots))))))))
  (define
   ldml:ldml
   ldml:identity
   ldml:language
   ldml:languages
   ldml:script
   ldml:scripts
   ldml:territory
   ldml:territories
   ldml:variant
   ldml:variants
   ldml:numbers
   ldml:symbols
   ldml:currencies
   ldml:currency
   ldml:display-name
   ldml:calendar
   ldml:calendars
   ldml:month-width
   ldml:month
   ldml:day-width
   ldml:day
   ldml:quarter-width
   ldml:quarter
   ldml:am
   ldml:pm
   ldml:date-formats
   ldml:date-format-length
   ldml:era-names
   ldml:era-abbr
   ldml:era-narrow
   ldml:era
   ))

(defmethod sax:characters ((parser cldr-parser) data)
  (unless (every (lambda (char)
                   (member char '(#\Space #\Tab #\Return #\Linefeed) :test #'char=))
                 data)
    (call-next-method)))

(defun ldml-intern (name &key hyphenize)
  (check-type name string)
  (when hyphenize
    (setf name (camel-case-to-hyphened name)))
  (let* ((ldml-package (find-package '#:cl-l10n.ldml))
         (result (intern (string-upcase name) ldml-package)))
    (export result ldml-package)
    result))

(defgeneric process-ldml-node (parent node)
  (:method (parent (node flexml:flexml-node))
    (iter (for child :in-sequence (flexml:children-of node))
          (process-ldml-node node child)))

  (:method ((parent flexml:flexml-node) (node string))
    ;; nop
    )

  (:method ((parent ldml:ldml) (node ldml:identity))
    (flet ((lookup (type)
             (let ((value (slot-value-unless-nil
                           (flexml:first-child-with-type node type)
                           'ldml::type)))
               (assert (or (null value)
                           (stringp value)))
               value)))
      (let ((language  (lookup 'ldml:language))
            (script    (lookup 'ldml:script))
            (territory (lookup 'ldml:territory))
            (variant   (lookup 'ldml:variant))
            (version (slot-value (flexml:first-child-with-local-name node "version")
                                 'ldml::number))
            (date (slot-value (flexml:first-child-with-local-name node "generation")
                              'ldml::date)))
        (assert language)
        (when (and (starts-with-subseq "$Revision: " version)
                   (ends-with-subseq   " $" version))
          (setf version (subseq version 11 (- (length version) 2))))
        (when (and (starts-with-subseq "$Date: " date)
                   (ends-with-subseq   " $" date))
          (setf date (subseq date 7 (- (length date) 2))))
        (setf *locale*
              (make-instance 'locale
                             :language language
                             :script script
                             :territory territory
                             :variant variant
                             :version-info (concatenate 'string version " (" date ")"))))))

  (:method ((parent ldml:numbers) (node ldml:symbols))
    (iter (for symbol-node :in-sequence (flexml:children-of node))
          (for value = (flexml:string-content-of symbol-node))
          (for name = (ldml-intern (flexml:local-name-of symbol-node) :hyphenize t))
          (push (cons name value) (number-symbols-of *locale*))))

  (:method ((parent ldml:currencies) (node ldml:currency))
    (bind ((name (slot-value node 'ldml::type)))
      (assert (every #'upper-case-p name))
      (setf name (ldml-intern name))
      (when-bind display-name-node (flexml:first-child-with-local-name node "displayName")
        (bind ((display-name (flexml:string-content-of display-name-node))
               (symbol (awhen (flexml:first-child-with-local-name node "symbol")
                         (flexml:string-content-of it)))
               (entry (list* display-name
                             (when symbol
                               (list symbol)))))
          (setf (gethash name (currencies-of *locale*)) entry)))))

  (:method ((parent ldml:languages) (node ldml:language))
    (process-langauge-list-like-ldml-node node 'languages-of))

  (:method ((parent ldml:scripts) (node ldml:script))
    (process-langauge-list-like-ldml-node node 'scripts-of))

  (:method ((parent ldml:territories) (node ldml:territory))
    (process-langauge-list-like-ldml-node node 'territories-of))

  (:method ((parent ldml:variants) (node ldml:variant))
    (process-langauge-list-like-ldml-node node 'variants-of))

  (:method ((parent ldml:calendars) (node ldml:calendar))
    (if (string= (slot-value node 'ldml::type) "gregorian")
        (progn
          (setf (gregorian-calendar-of *locale*) (make-instance 'gregorian-calendar))
          (process-ldml-gregorian-calendar-node parent node))
        (call-next-method))))

(defun process-langauge-list-like-ldml-node (node accessor)
  (let* ((name (string-upcase (slot-value node 'ldml::type))))
    (aif (parse-integer name :junk-allowed t)
         (setf name it)
         (setf name (ldml-intern name)))
    (let* ((display-name (flexml:string-content-of node)))
      (setf (gethash name (funcall accessor *locale*)) display-name))))

(defgeneric process-ldml-gregorian-calendar-node (prent node)
  (:method (parent (node flexml:flexml-node))
    (iter (for child :in-sequence (flexml:children-of node))
          (process-ldml-gregorian-calendar-node node child)))

  (:method ((parent flexml:flexml-node) (node string))
    ;; nop
    )

  (:method ((parent ldml:calendar) (node ldml:am))
    (setf (am-of (gregorian-calendar-of *locale*)) (flexml:string-content-of node)))

  (:method ((parent ldml:calendar) (node ldml:pm))
    (setf (pm-of (gregorian-calendar-of *locale*)) (flexml:string-content-of node)))

  (:method ((parent ldml:month-width) (node ldml:month))
    (process-month-list-like-ldml-node
     parent node 12 '(("narrow"      . narrow-month-names-of)
                      ("abbreviated" . abbreviated-month-names-of)
                      ("wide"        . month-names-of))))

  (:method ((parent ldml:day-width) (node ldml:day))
    (process-month-list-like-ldml-node
     parent node 7 '(("narrow"      . narrow-day-names-of)
                     ("abbreviated" . abbreviated-day-names-of)
                     ("wide"        . day-names-of))
     '("sun" "mon" "tue" "wed" "thu" "fri" "sat")))

  (:method ((parent ldml:quarter-width) (node ldml:quarter))
    (process-month-list-like-ldml-node
     parent node 4 '(("abbreviated" . abbreviated-quarter-names-of)
                     ("wide"        . quarter-names-of))))

  (:method ((parent ldml:era-names) (node ldml:era))
    (parse-era-ldml-node node 'era-names-of))

  (:method ((parent ldml:era-abbr) (node ldml:era))
    (parse-era-ldml-node node 'abbreviated-era-names-of))

  (:method ((parent ldml:era-narrow) (node ldml:era))
    (parse-era-ldml-node node 'narrow-era-names-of))

  (:method ((parent ldml:date-formats) (node ldml:date-format-length))
    (bind ((name (ldml-intern (slot-value node 'ldml::type)))
           (inbetween-node (flexml:the-only-child node)))
      (unless (length= 1 (flexml:children-of inbetween-node))
        (cldr-parser-warning "LDML node ~A has multiple children, using the first one" inbetween-node))
      (bind ((pattern (flexml:string-content-of (flexml:first-child inbetween-node))))
        (setf (getf (date-formatters-of (gregorian-calendar-of *locale*)) name)
              (list :formatter 'dummy-formatter :pattern pattern))))))

(defun parse-era-ldml-node (node reader)
  (bind ((calendar (gregorian-calendar-of *locale*))
         (index-designator (slot-value node 'ldml::type))
         (index (parse-integer index-designator))
         (writer (fdefinition `(setf ,reader)))
         (vector (funcall reader calendar)))
    (assert (<= 0 index 1))
    (unless vector
      (setf vector (make-array 2 :initial-element nil))
      (funcall writer vector calendar))
    (setf (aref vector index) (flexml:string-content-of node))))

(defun process-month-list-like-ldml-node (parent node max-count reader-map &optional index-designators)
  (bind ((calendar (gregorian-calendar-of *locale*))
         (reader (awhen (assoc (slot-value parent 'ldml::type) reader-map :test #'string=)
                   (cdr it)))
         (index-designator (slot-value node 'ldml::type))
         (index (aif (parse-integer index-designator :junk-allowed t)
                     (1- it)
                     (position index-designator index-designators :test #'string=))))
    (assert (<= 0 index (1- max-count)))
    (when reader
      (bind ((writer (fdefinition `(setf ,reader)))
             (vector (funcall reader calendar)))
        (unless vector
          (setf vector (make-array max-count :initial-element nil))
          (funcall writer vector calendar))
        (setf (aref vector index) (flexml:string-content-of node))))))
