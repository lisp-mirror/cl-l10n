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

(defvar *fallback-locales* nil)

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
  ;; make a function 
  (setf (symbol-function name) (eval `(lambda (&rest args) (lookup-resource-with-fallback ',name args))))
  name)

(defun %lookup-resource (locale name args)
  (declare (type locale locale)
           (type (or symbol string) name))
  (let* ((key (resource-key locale name)))
    (multiple-value-bind (resource found)
        (gethash key *resources*)
      (unless found
        ;; try again with the default locale for the language
        (setf key (resource-key (canonical-locale-name-from (first (split-sequence #\_ (locale-name locale)))) name))
        (setf resource (gethash key *resources*)))
    ;; dispatch on resource type
    (cond ((functionp resource)
           (apply resource args))
          ;; literal
          ((not (null resource))
           resource)))))

(defun lookup-resource-with-fallback (name args &key (warn-if-missing t) (fallback-to-name t))
  (loop for locale in (or *fallback-locales* (list *locale*)) do
        (let ((result (funcall '%lookup-resource locale name args)))
          (when result
            (return-from lookup-resource-with-fallback (values result t)))))
  (resource-not-found name warn-if-missing fallback-to-name))

(defun lookup-resource (locale name args &key (warn-if-missing t) (fallback-to-name t))
  (aif (%lookup-resource locale name args)
       it
       (resource-not-found name warn-if-missing fallback-to-name)))

(defun resource-not-found (name warn-if-missing fallback-to-name)
  (if warn-if-missing
      (signal 'resource-missing :name name))
  (values (if fallback-to-name
              (string-downcase (string name)))
          nil))

(defparameter *language->default-locale-name* (make-hash-table :test #'equal))

(defun canonical-locale-name-from (locale)
  (if (typep locale 'locale)
      (locale-name locale)
      (let ((name locale))
        (when (and (not (null name))
                   (symbolp name))
          (setf name (symbol-name name)))
        (let* ((parts (split-sequence:split-sequence #\- name))
               (count (list-length parts))
               (first-length (length (first parts)))
               (second-length (length (second parts))))
          (when (> count 2)
            (error "Locale variants are not yet supported"))
          (when (or (> first-length 3)
                    (< first-length 2)
                    (and (> count 1)
                         (or (> second-length 3)
                             (< second-length 2))))
            (error "~A is not a valid locale name (examples: en-gb, en-us, en)" locale))
          (let ((language (string-downcase (first parts)))
                (region (when (> count 1)
                          (second parts))))
            (print language)
            (if (> count 1)
                (concatenate 'string language "_" region)
                (aif (gethash language *language->default-locale-name*)
                     it
                     (concatenate 'string language "_" (string-upcase language)))))))))

(eval-when (:load-toplevel)
  (loop for (language locale) in
        '((en en-us)) do
        (setf (gethash (string-downcase (symbol-name language)) *language->default-locale-name*)
              (canonical-locale-name-from locale)))
  (values))

(defun locale-for (symbol)
  (get-locale (canonical-locale-name-from symbol)))

(defmacro defresources (locale &body resources)
  (let ((locale-name (canonical-locale-name-from locale)))
    (cons 'progn
          (loop for resource in resources
                if (= 2 (length resource))
                collect `(add-resource ,locale-name
                          ',(first resource) nil ',(cdr resource))
                else
                collect `(add-resource ,locale-name
                          ',(first resource) ',(second resource) ',(cddr resource))))))

(defmacro with-locale (name &body body)
  `(let ((*locale* (locale-for ,name)))
    ,@body))

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
       `(lookup-resource *locale* ,(read s) nil))))

(defun with-sharpquote-syntax ()
  "To be used with the curly reader from arnesi: {with-sharpquote-reader (foo #\"locale-specific\") }"
  (lambda (handler)
    (%enable-sharpquote-reader)
    `(progn ,@(funcall handler))))



(defgeneric localize (object)
  (:documentation "Override this generic method for various data types. Return (values result foundp)."))

(defmethod localize ((str string))
  (lookup-resource-with-fallback str nil))

(defmethod localize ((str symbol))
  (lookup-resource-with-fallback str nil))
