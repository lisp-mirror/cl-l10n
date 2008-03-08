;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package :cl-l10n)

(defparameter *ignore-categories*
  (list "LC_CTYPE" "LC_COLLATE"))

(defvar *resource-package* nil
  "Resource files will be loaded into this package. I suggest to create a package called 'lang'
and set/bind this variable to it before calling cl-l10n. Then all the defined resources will be in
this new package and you can refer to them with lang:resource-name so it's easy to search, etc.

An example:

\(defpackage :some-project.lang
  \(:nicknames :lang)
  \(:use
   :common-lisp
   :cl-l10n))")

(defmacro with-resource-package (package &body body)
  `(let ((*resource-package* (find-package ,package)))
    ,@body))

(defparameter *common-resource-file-is-loaded-p* nil)

(deftype locale-designator ()
  `(or locale string symbol))

(defun canonical-locale-name-from (locale)
  (check-type locale locale-designator)
  (cond
    ((typep locale 'locale)
     (locale-name locale))
    ((member locale '("root" "en_US_POSIX") :test #'string=)
     locale)
    (t
     (let ((name locale)
           (language nil)
           (script nil)
           (territory nil))
       (when (and (not (null name))
                  (symbolp name))
         (setf name (symbol-name name)))
       (let* ((parts (cl-ppcre:split "_" name))
              (count (length parts)))
         (unless (<= 1 count 3)
           (error "Can't parse locale name ~S" name))
         (setf language (first parts))
         (if (= count 2)
             (setf territory (second parts))
             (progn
               (setf script (second parts))
               (setf territory (third parts))))
         (unless (and (= (length language) 2)
                      (or (null territory)
                          (= (length territory) 2)))
           (error "~A is not a valid locale name (examples: en_GB, en_US, en, zh_Hans_CN)" locale))
         (setf language (string-downcase language))
         (when script
           (setf script (concatenate 'string
                                     (list (char-upcase (elt script 0)))
                                     (string-downcase (subseq script 1)))))
         (when territory
           (setf territory (string-upcase territory)))
         (let ((*print-pretty* nil))
           (with-output-to-string (*standard-output*)
             (write-string language)
             (awhen script
               (write-char #\_)
               (write-string it))
             (awhen territory
               (write-char #\_)
               (write-string it)))))))))

(defun locale (locale-designator &key (use-cache t) (errorp t))
  "Find locale named by the specification LOCALE-DESIGNATOR. If USE-CACHE
is non-nil forcefully reload/reparse the cldr locale else
the locale is first looked for in *locale-cache*. If ERRORP is non-nil
signal an error that the locale file cannot be found.
If LOADER is non-nil skip everything and call loader with LOCALE-DESIGNATOR."
  (declare (type locale-designator locale-designator))
  (if (typep locale-designator 'locale)
      locale-designator
      (let ((name (canonical-locale-name-from locale-designator)))
        (awhen (and use-cache
                    (get-cached-locale name))
          (return-from locale it))
        (let ((file (cldr-pathname-for name)))
          (if file
              (let ((locale (parse-cldr-file name)))
                (setf (get-cached-locale name) locale)
                (load-resource name)
                locale)
              (when errorp
                (error "Could not find locale definition for ~S among the CLDR files" name)))))))

(defun load-resource (name)
  ;;(l10n-logger.debug "Trying to load resource ~A" name)
  (let ((resource-file (project-relative-pathname
                        (make-pathname :directory
                                       '(:relative "resources")
                                       :name name
                                       :type "lisp"))))
    (awhen (probe-file resource-file)
      (when (pathname-name it)
        ;;(l10n-logger.debug "Resource found at ~A" it)
        (if *resource-package*
            (let ((*package* *resource-package*))
              ;;(l10n-logger.info "Loading resource ~A into ~A" it *resource-package*)
              (load it))
            (progn
              ;;(l10n-logger.debug "*resource-package* is not set, skipped loading ~A" it)
              (warn "*resource-package* is not set, skipped loading resource file ~A" it))))))
  (unless *common-resource-file-is-loaded-p*
    (setf *common-resource-file-is-loaded-p* t)
    (load-resource "common")))

(defun reload-resources ()
  (let ((common-was-already-loaded? *common-resource-file-is-loaded-p*))
    (iter (for (name nil) :in-hashtable *locale-cache*)
          (load-resource name))
    (when common-was-already-loaded?
      (load-resource "common"))))
#+nil
(defun load-all-locales (&key (path *locale-path*) (ignore-errors nil) (use-cache nil))
  "Load all locale found in pathname designator PATH."
  ;; TODO
  (let ((*locale-path* path))
    (dolist (x (list-directory *locale-path*))
      (when (and (not (directory-pathname-p x)) (pathname-name x))
        (let ((locale (pathname-name x)))
          (with-simple-restart (continue "Ignore locale ~A." x)
            (handler-bind ((error (lambda (&optional c)
                                    (when ignore-errors
                                      (warn "Failed to load locale ~S, Ignoring." locale)
                                      (invoke-restart (find-restart 'continue c))))))
              (locale locale :use-cache use-cache))))))))

(declaim (inline current-locale (setf current-locale)))

(defun current-locale ()
  *locale*)

(defun (setf current-locale) (locale-name)
  (setf *locale* (loop for locale :in (ensure-list locale-name)
                       collect (locale locale))))

(defmacro with-locale (locale &body body)
  `(let ((*locale* (mapcar 'locale (ensure-list ,locale))))
     ,@body))

(defun load-default-locale ()
  (flet ((try (env-name)
           (let ((locale-name (getenv env-name)))
             (when locale-name
               (awhen (position #\. locale-name)
                 (setf locale-name (subseq locale-name 0 it)))
               (when (member locale-name '("C" "posix" "POSIX") :test #'string=)
                 (setf locale-name "en_US_POSIX"))
               (locale locale-name :errorp nil)))))
    (let ((locale (or (try "CL_LOCALE")
                      (try "LC_CTYPE")
                      (try "LANG")
                      (locale "en_US_POSIX"))))
      (setf (current-locale) locale))))

(defun load-root-locale ()
  (setf *root-locale* (locale "root")))

(eval-when (:load-toplevel :execute)
  (load-root-locale)
  (load-default-locale))


#+nil
(defun create-number-fmt-string (locale no-ts)
  ;; TODO: Quick workaround for buggy format in openmcl which prints the
  ;; commachar even when the : modifier is not present.
  #+openmcl (when no-ts (return-from create-number-fmt-string "~A~D~{~A~}"))
  (cl:format nil "~~A~~,,'~A,~A~A~~{~~A~~}" 
             (thousands-sep-char (locale-thousands-sep locale))
             (if (minusp (locale-grouping locale)) 3 (max 1 (locale-grouping locale)))
             (if no-ts "D" ":D")))

#+nil
(defun create-money-fmt-string (locale no-ts minusp)
  (multiple-value-bind (sep-by-space prec spos sign) 
      (get-descriptors minusp locale)
    (let ((sym-sep (if (zerop sep-by-space) "" " ")))
      (with-output-to-string (stream)
        ;; sign and sign separator
        (when (or* (= spos 0 1 3))
          (princ (if (zerop spos) "(" sign) stream)
          (when (= 2 sep-by-space)
            (princ #\Space stream)))
        ;; Sym and seperator
        (princ "~A" stream)
        (when prec
          (princ sym-sep stream))
        ;; Actual number
        ;; TODO: workaround for buggy format in openmcl
        ;; (see create-number-fmt-string above)
        #+openmcl (when no-ts (write-string "~D~{~A~}" stream))
        (unless #+openmcl no-ts #-openmcl nil
                (cl:format stream "~~,,'~A,~A~A~~{~~A~~}"
                           (thousands-sep-char (locale-mon-thousands-sep locale))
                           (if (minusp (locale-mon-grouping locale)) 3 (locale-mon-grouping locale))
                           (if no-ts "D" ":D")))
        (unless prec
          (princ sym-sep stream))
        (princ "~A" stream)
        (when (or* (= spos 0 2 4))
          (when (= 2 sep-by-space)
            (princ #\Space stream))
          (princ (if (zerop spos) ")" sign) stream))))))

#+nil
(defun add-printers (locale)
  "Creates monetary and numeric format strings for locale LOCALE."
  (when (and (get-category locale "LC_MONETARY")
             (get-category locale "LC_NUMERIC"))
    ;; otherwise its an include locale (tranlit* etc)
    (setf (printers locale)
          (nconc (list :number-no-ts
                       (create-number-fmt-string locale t))
                 (list :number-ts
                       (create-number-fmt-string locale nil))
                 (list :money-p-no-ts
                       (create-money-fmt-string locale t nil))
                 (list :money-p-ts
                       (create-money-fmt-string locale nil nil))
                 (list :money-n-no-ts
                       (create-money-fmt-string locale t t))
                 (list :money-n-ts
                       (create-money-fmt-string locale nil t))
                 (printers locale)))))


