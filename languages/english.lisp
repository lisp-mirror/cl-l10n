;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package #:cl-l10n)

(defparameter *english-plural-overrides*
  (read-key->value-text-file-into-hashtable
   (merge-pathnames (make-pathname :directory '(:relative "languages")
                                   :name "english-plural-overrites"
                                   :type "text")
                    (asdf:component-pathname (asdf:find-system :cl-l10n)))))

(defun english-plural-of (word &optional (uppercase nil uppercase-provided-p))
  "Returns the english plural of the given word."
  ;; TODO http://www.reference.com/browse/wiki/English_plural
  (declare (type (simple-array character) word)
           (optimize (speed 3) (debug 0)))
  (aif (gethash (string-downcase word) *english-plural-overrides*)
       (return-from english-plural-of it)
       (let* ((length (length word))
              (original-last-letter (elt word (1- length)))
              (last-letter (char-downcase original-last-letter))
              (last-letter2 (char-downcase (elt word (- length 2)))))
         (unless uppercase-provided-p
           (setf uppercase (upper-case-p original-last-letter)))
         (macrolet ((all-but-last (&optional (count 1))
                      `(subseq word 0 (- length ,count)))
                    (emit (body postfix)
                      `(return-from english-plural-of
                        (concatenate 'string ,body
                         (if uppercase
                             (string-upcase ,postfix)
                             ,postfix)))))
           (when (>= length 2)
             (cond ((or (and (eql last-letter2 #\s) (eql last-letter #\s))
                        (and (eql last-letter2 #\e) (eql last-letter #\x)))
                    (emit word "es"))
                   ((and (eql last-letter2 #\i) (eql last-letter #\s)) (emit (all-but-last 2) "es"))
                   ((and (eql last-letter2 #\i) (eql last-letter #\x)) (emit (all-but-last 2) "ices"))
                   ((eql last-letter #\s) (emit word "ses"))
                   ((and (eql last-letter2 #\t) (eql last-letter #\y)) (emit (all-but-last) "ies"))))
           (emit word "s")))))

(defun english-indefinit-article-for (word)
  "Returns a/an for the given word."
  (declare (type (simple-array character) word)
           (optimize (speed 3) (debug 0)))
  (if (> (length word) 1)
      (if (vowelp (elt word 0))
          "an"
          "a")
      word))

