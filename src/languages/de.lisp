;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package #:cl-l10n)

(defparameter *de-plural-overrides*
  (read-key->value-text-file-into-hashtable
   (project-relative-pathname (make-pathname :directory '(:relative "src" "languages")
                                             :name "de-plural-overrides"
                                             :type "text"))))

(defun de-plural-of (word &optional (uppercase nil))
  "Returns the German plural of the given word."
  ;; http://www.reference.com/browse/wiki/English_plural
  ;; http://www.csse.monash.edu.au/~damian/papers/HTML/Plurals.html
  (declare (type string word)
           (optimize (speed 3) (debug 0)))
  (let ((length (length word)))
    (when (< length 2)
      (error "There's no German word with less then two letters"))
    (aif (gethash (string-downcase word) *de-plural-overrides*)
         (return-from de-plural-of
           (if uppercase
               (string-upcase (first it))
               (first it)))
         word)))

(defun de-indefinite-article-for (word)
  "Returns ein/eine for the given word."
    (aif (gethash (string-downcase word) *de-plural-overrides*)
         (if (string= "f" (second it))
             "eine"
             "ein")))


(defun de-definite-article-for (word)
  "Returns der/die/das for the given word."
  (aif (gethash (string-downcase word) *de-plural-overrides*)
       (if (string= "f" (second it))
           "die"
           (if (string= "n" (second it))
               "das"
               "der"))))
