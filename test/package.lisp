(in-package #:cl-l10n.system)

(defpackage :cl-l10n.test
  (:use :common-lisp
        :cl-l10n
        :alexandria
        :iter
        :stefil
        )
  (:shadowing-import-from :cl-l10n
   #:format
   #:formatter
   #:*locale-cache*
   #:*locale*
   #:precedence-list-of
   ))

(in-package #:cl-l10n.test)

(in-root-suite)

(defsuite* test)
