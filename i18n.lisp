;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package :cl-l10n)


;; (defparameter bundle (make-instance 'bundle))

;; (add-resources (bundle "af_")
;;   "showtime" "Dankie, die tyd is ~:@U~%")

;; ;; an empty string as the locale matcher becomes the default
;; (add-resources (bundle "") 
;;   "showtime" "Thanks, the time is ~:@U~%")

;; (set-dispatch-macro-character
;;  #\# #\i
;;  #'(lambda (s c1 c2)
;;      (declare (ignore c2))
;;      `(cl-l10n:gettext ,(read s) bundle)))

;; or this
;; (defmacro _ (text)
;;   `(cl-l10n:gettext ,text bundle))

;; (defun timey ()
;;   (format t #i"showtime" (get-universal-time)))

(defclass bundle ()
  ((resources :accessor resources :initform (make-hash-table :test #'equal))))

(defgeneric add-resource (bundle from to lang))
(defmethod add-resource (bundle from to lang)
  (aif (assoc lang (gethash from (resources bundle)) :test #'equal)
       (setf (cdr it) to)
       (pushnew (cons lang to) (gethash from (resources bundle))
                :test #'equal))
  t)

(defmacro add-resources ((bundle loc-name) &body args)
  (with-gensyms (gloc gbundle)
    `(let ((,gloc ,loc-name) (,gbundle ,bundle))
       ,@(mapcar #'(lambda (x) `(add-resource ,gbundle ,@x ,gloc))
                 (group args 2)))))

(defgeneric get-name (bundle name)
  (:method ((bundle t) (name t))
    (gethash name (resources bundle))))

(defgeneric lookup-name (bundle name)
  (:method ((bundle t) (name t))
    (when-let (name (get-name bundle name))
      ;; The match with the longest name is the most 
      ;; specific key.
      (winner #'> 
              (load-time-value (compose #'length #'car))
              (remove-if-not #'(lambda (x)
                                 (search (car x)
                                         (locale-name *locale*)))
                             name)))))

(defun gettext (name bundle &optional (loc *locale* ))
  (let ((*locale* (locale-des->locale loc)))
    (or (cdr (lookup-name bundle name))
        name)))




;; EOF
