(in-package #:cl-l10n.system)

(defpackage :cl-l10n.test
  (:use :common-lisp
        :cl-l10n
        :cl-l10n.lang
        :alexandria
        :iter
        :stefil
        )
  (:shadowing-import-from :cl-l10n
   #:format
   #:formatter
   #:*locale-cache*
   #:*locale*
   #:*resources*
   ))

(in-package #:cl-l10n.test)

(in-root-suite)

(defsuite* test)
