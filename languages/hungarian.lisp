;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10; encoding: utf-8 -*-
;; See the file LICENCE for licence information.
(in-package #:cl-l10n)

(defparameter *hungarian-plural-overrides*
  (read-key->value-text-file-into-hashtable
   (merge-pathnames (make-pathname :directory '(:relative "languages")
                                   :name "hungarian-plural-overrites"
                                   :type "text")
                    (asdf:component-pathname (asdf:find-system :cl-l10n)))))


(defun hungarian-plural-of (word &optional (uppercase nil uppercase-provided-p))
  "Returns the hungarian plural of the given word."
  (declare (type (simple-array character) word)
           (optimize (speed 3) (debug 0)))
  (aif (gethash word *hungarian-plural-overrides*)
       (return-from hungarian-plural-of it)
       (let* ((length (length word))
              (original-last-letter (elt word (1- length)))
              (last-letter (char-downcase original-last-letter)))
         (unless uppercase-provided-p
           (setf uppercase (upper-case-p original-last-letter)))
         (macrolet ((emit (body &rest pieces)
                      `(return-from hungarian-plural-of
                        (concatenate 'string ,body
                         ,@(iter (for piece in pieces)
                                 (collect `(if uppercase
                                            (string-upcase ,piece)
                                            ,piece)))))))
           (if (vowelp last-letter)
               (emit word "k")
               (when-bind last-vowel (last-vowel-of word)
                 (emit word last-vowel "k")))
           (emit word "-k")))))
