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
   ldml:script
   ldml:territory
   ldml:variant
   ldml:numbers
   ldml:symbols))

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
          (assert (= (length (flexml:children-of symbol-node)) 1))
          (for value = (elt (flexml:children-of symbol-node) 0))
          (for name = (intern (concatenate 'string
                                           "SYMBOL/"
                                           (string-upcase
                                            (camel-case-to-hyphened
                                             (flexml:local-name-of symbol-node))))
                              :cl-l10n.lang))
          (setf (lookup-resource name) value))))
