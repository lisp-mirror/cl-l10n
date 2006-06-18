;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package :cl-l10n)

#|
(defresources en
  (indefinit-article-for (str)
                         ;; calculate "a"/"an" here
                         )
  (foo.bar "some constant"))

then writing (indefinit-article-for "asdf") will call the locale-specific
implementation of that function

|#

(defvar *resources* (make-hash-table :test 'equal))

(defun clear-resources ()
  (setf *resources* (make-hash-table :test 'equal)))

(defun resource-key (locale name)
  (list (if (stringp locale) locale (locale-name locale))
        (if (stringp name) (string-downcase name) (string-downcase (symbol-name name)))))

(define-condition resource-missing (warning)
  ((name :accessor name-of :initarg :name)))

(defun add-resource (locale name args body)
  ;; store in resouce map
  (setf (gethash (resource-key locale name) *resources*)
        (if (and (= (length body) 1)
                 (stringp (first body)))
            (first body)
            (eval `(lambda ,args ,@body))))
  (when (and args (not (get name :cl-l10n)))
    ;; define a function with this name that'll look at the *locale* list and call the first
    ;; locale specific lambda it finds while walking the locales
    (when (fboundp name)
      (warn "Redefining function definiton of ~S while adding locale specific resource" name))
    (setf (symbol-function name) (eval `(lambda (&rest args) (lookup-resource ',name args))))
    ;; leave a mark that it's been defined by us
    (setf (get name :cl-l10n) t))
  name)

(defun %lookup-resource (locale name args)
  (declare (type locale locale)
           (type (or symbol string) name))
  (let* ((key (resource-key locale name)))
    (multiple-value-bind (resource found)
        (gethash key *resources*)
      (unless found
        ;; try again with the default locale for the language
        (setf key (resource-key (canonical-locale-name-from (first (split "_" (locale-name locale)))) name))
        (setf resource (gethash key *resources*)))
    ;; dispatch on resource type
    (cond ((functionp resource)
           (values (apply resource args) t))
          ;; literal
          ((not (null resource))
           (values resource t))))))

(defun lookup-resource (name args &key (warn-if-missing t) (fallback-to-name t))
  (loop for locale in (if (consp *locale*) *locale* (list *locale*)) do
        (multiple-value-bind (result foundp) (funcall '%lookup-resource locale name args)
          (when foundp
            (return-from lookup-resource (values result t)))))
  (resource-not-found name warn-if-missing fallback-to-name))

(defun lookup-resource-without-fallback (locale name args &key (warn-if-missing t) (fallback-to-name t))
  (aif (%lookup-resource locale name args)
       it
       (resource-not-found name warn-if-missing fallback-to-name)))

(defun resource-not-found (name warn-if-missing fallback-to-name)
  (if warn-if-missing
      (signal 'resource-missing :name name))
  (values (if fallback-to-name
              (string-downcase (string name)))
          nil))

(defmacro defresources (locale &body resources)
  (let ((locale-name (canonical-locale-name-from locale)))
    (cons 'progn
          (iter (for resource in resources)
                (for name = (first resource))
                (if (= 2 (length resource))
                    (collect `(add-resource ,locale-name
                               ',name nil ',(cdr resource)))
                    (collect `(add-resource ,locale-name
                               ',name ',(second resource) ',(cddr resource))))
                (unless (starts-with (symbol-name name) "%")
                  (collect `(export ',name)))))))

(defmacro lookup-first-matching-resource (&body specs)
  "Try to look up the resource keys, return the first match, fallback to the first key.
An example usage:
  (lookup-first-matching-resource
    ((awhen attribute (name-of it)) (name-of state))
    ((name-of (state-machine-of state)) (name-of state))
    (\"state-name\" (name-of state))
    \"last-try\")
When a resource key is a list, its elements will be concatenated separated by dots."
  (iter (with fallback = nil)
        (for spec in specs)
        (for el = (if (or (and (consp spec)
                               (symbolp (car spec)))
                          (atom spec))
                      spec
                      `(strcat-separated-by "." ,@spec)))
        (if (first-time-p)
            (setf fallback el)
            (collect `(lookup-resource ,el nil :warn-if-missing nil :fallback-to-name nil) into lookups))
        (finally (return (with-unique-names (block fallback-tmp)
                           `(block ,block
                             (let ((,fallback-tmp ,fallback))
                               (multiple-value-bind (resource foundp)
                                   (lookup-resource ,fallback-tmp nil :warn-if-missing nil :fallback-to-name nil)
                                 (when foundp
                                   (return-from ,block (values resource t))))
                               ,@(iter (for lookup in lookups)
                                       (collect `(multiple-value-bind (resource foundp) ,lookup
                                                  (when foundp
                                                    (return-from ,block (values resource t))))))
                               (return-from ,block (values ,fallback-tmp nil)))))))))

(defmacro enable-sharpquote-reader ()
  "Enable quote reader for the rest of the file (being loaded or compiled).
#\"my i18n text\" parts will be replaced by a lookup-resource call for the string.
Be careful when using in different situations, because it modifies *readtable*."
  ;; The standard sais that *readtable* is restored after loading/compiling a file,
  ;; so we make a copy and alter that. The effect is that it will be enabled
  ;; for the rest of the file being processed.
  `(eval-when (:compile-toplevel :execute)
    (setf *readtable* (copy-readtable *readtable*))
    (%enable-sharpquote-reader)))

(defun %enable-sharpquote-reader ()
  (set-dispatch-macro-character
   #\# #\"
   #'(lambda (s c1 c2)
       (declare (ignore c2))
       (unread-char c1 s)
       `(lookup-resource ,(read s) nil))))

(defun with-sharpquote-syntax ()
  "To be used with the curly reader from arnesi: {with-sharpquote-reader (foo #\"locale-specific\") }"
  (lambda (handler)
    (%enable-sharpquote-reader)
    `(progn ,@(funcall handler))))



(defgeneric localize (object)
  (:documentation "Override this generic method for various data types. Return (values result foundp)."))

(defmethod localize ((str string))
  (lookup-resource str nil))

(defmethod localize ((str symbol))
  (lookup-resource str nil))
