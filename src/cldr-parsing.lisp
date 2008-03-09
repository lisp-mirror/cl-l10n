;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-l10n)

;;; see http://unicode.org/cldr/

(defvar *parser*)

(defclass cldr-parser (flexml:flexml-builder)
  ())

(defun make-cldr-parser ()
  (make-instance 'cldr-parser :default-package "CL-L10N.LDML"))

(defun cldr-pathname-for (locale-name)
  (project-relative-pathname (concatenate 'string "cldr/main/" locale-name ".xml")))

(defun parse-cldr-file (name)
  (let* ((*parser* (make-cldr-parser))
         (*locale* nil))
    (cxml:parse (cldr-pathname-for name) *parser*
                :entity-resolver 'cldr-entity-resolver)
    (process-ldml-node nil (flexml:root-of *parser*))
    (assert *locale*)
    (values *locale* *parser*)))

(defun cldr-entity-resolver (public-id system-id)
  (declare (ignore public-id))
  (cond
    ((puri:uri= system-id (load-time-value
                           (puri:parse-uri "http://www.unicode.org/cldr/dtd/1.5/ldml.dtd")))
     (open (project-relative-pathname "cldr/ldml.dtd")
           :element-type '(unsigned-byte 8)
           :direction :input))))

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
   ))

(defmethod sax:characters ((parser cldr-parser) data)
  (unless (every (lambda (char)
                   (member char '(#\Space #\Tab #\Return #\Linefeed) :test #'char=))
                 data)
    (call-next-method)))

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
          (for name = (intern (string-upcase
                               (camel-case-to-hyphened
                                (flexml:local-name-of symbol-node)))
                              :cl-l10n.lang))
          (push (cons name value) (number-symbols-of *locale*))))

  (:method ((parent ldml:currencies) (node ldml:currency))
    (let* ((name (slot-value node 'ldml::type)))
      (assert (every #'upper-case-p name))
      (setf name (intern name :cl-l10n.lang))
      (let* ((display-name (flexml:string-content-of
                            (flexml:first-child-with-local-name node "displayName")))
             (symbol (awhen (flexml:first-child-with-local-name node "symbol")
                       (flexml:string-content-of it)))
             (entry (list* display-name
                           (when symbol
                             (list symbol)))))
        (setf (gethash name (currencies-of *locale*)) entry))))

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
         (setf name (intern name :cl-l10n.lang)))
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
     parent node 12 '(("abbreviated" . abbreviated-month-names-of)
                      ("wide"        . month-names-of))))

  (:method ((parent ldml:day-width) (node ldml:day))
    (process-month-list-like-ldml-node
     parent node 7 '(("abbreviated" . abbreviated-day-names-of)
                      ("wide"        . day-names-of))
     '("sun" "mon" "tue" "wed" "thu" "fri" "sat")))

  (:method ((parent ldml:quarter-width) (node ldml:quarter))
    (process-month-list-like-ldml-node
     parent node 4 '(("abbreviated" . abbreviated-quarter-names-of)
                     ("wide"        . quarter-names-of)))))

(defun process-month-list-like-ldml-node (parent node max-count accessor-map &optional index-designators)
  (let* ((calendar (gregorian-calendar-of *locale*))
         (accessor (awhen (assoc (slot-value parent 'ldml::type) accessor-map :test #'string=)
                     (cdr it)))
         (index-designator (slot-value node 'ldml::type))
         (index (aif (parse-integer index-designator :junk-allowed t)
                     (1- it)
                     (position index-designator index-designators :test #'string=))))
    (assert (<= 0 index (1- max-count)))
    (when accessor
      (let ((vector (funcall accessor calendar)))
        (setf (aref vector index) (flexml:string-content-of node))))))
