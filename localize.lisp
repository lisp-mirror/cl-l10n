(in-package :cl-l10n)

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
  (let ((resource (gethash (resource-key locale name) *resources*)))
    ;; dispatch on resource type
    (cond ((functionp resource)
           (apply resource args))
          ;; literal
          ((not (null resource))
           resource))))

(defun lookup-resource-with-fallback (name args &key (warn-if-missing t) (fallback-to-name t))
  (loop for locale in (or *fallback-locales* (list *locale*)) do
        (let ((result (funcall '%lookup-resource locale name args)))
          (when result
            (return-from lookup-resource-with-fallback result))))
  (resource-not-found name warn-if-missing fallback-to-name))

(defun lookup-resource (locale name args &key (warn-if-missing t) (fallback-to-name t))
  (aif (%lookup-resource locale name args)
       it
       (resource-not-found name warn-if-missing fallback-to-name)))

(defun resource-not-found (name warn-if-missing fallback-to-name)
  (if warn-if-missing
      (signal 'resource-missing :name name))
  (if fallback-to-name
      (string-downcase (string name))))

(defun locale-name-for-symbol (symbol)
  (let ((parts (split-sequence:split-sequence #\- (symbol-name symbol))))
    (concatenate 'string (string-downcase (first parts)) "_" (second parts))))

(defun locale-for (symbol)
  (get-locale (locale-name-for-symbol symbol)))

(defmacro define-locale (name)
  (let ((locale-name (locale-name-for-symbol name)))
    `(progn
       (defmacro ,name (&rest resources)
         (cons 'progn
               (loop for resource in resources
                     if (= 2 (length resource))
                     collect `(add-resource ,',locale-name
                               ',(first resource) ,',() ',(cdr resource))
                     else
                     collect `(add-resource ,',locale-name
                               ',(first resource) ',(second resource) ',(cddr resource)))))
      (export ',name))))

(defmacro with-locale (name &body body)
  `(let ((*locale* (locale-for ,name)))
    ,@body))

(eval-when (:load-toplevel)
  (load-all-locales)
  (eval '(define-locale en-GB))
  (eval '(define-locale hu-HU)))

;;; reader macro for #"my i18n text"
(set-dispatch-macro-character
 #\# #\"
 #'(lambda (s c1 c2)
    (declare (ignore c2))
    (unread-char c1 s)
    `(lookup-resource *locale* ,(read s) nil)))

(defgeneric localize (object)
  (:documentation "Override this generic method for various data types."))

(defmethod localize ((str string))
  (lookup-resource-with-fallback str nil))

(defmethod localize ((str symbol))
  (lookup-resource-with-fallback str nil))
