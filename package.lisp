;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package #:cl-l10n.system)

(defpackage #:cl-l10n 
  (:use #:cl #:cl-ppcre #:cl-fad #:arnesi #:iterate)
  (:shadow cl:format cl:formatter)
  (:shadowing-import-from :cl-fad
                          #:copy-stream #:copy-file)
  (:export #:locale-name #:category-name #:locale #:category #:locale-error
           #:get-category #:get-cat-val #:locale-value #:load-all-locales
           #:get-locale #:*locale-path* #:*locales* #:load-default-locale
           #:format-number #:print-number #:format-money #:print-money
           #:format-time #:print-time #:add-resources 
           #:parse-number #:*float-digits*  #:shadow-format
           #:parse-time
           ;; attila: these symbols are very frequent and cause a lot of headaches
           ;; when integrating cl-l10n into other projects. they should
           ;; be renamed to *p and *-p if we really want to export them
           ;;#:month #:day #:year #:hour #:minute #:second
           ;;#:date-divider #:time-divider #:weekday #:noon-midn
           ;;#:secondp #:am-pm #:zone
           #:parser-error #:set-locale
           #:with-locale #:lookup-resource
           #:lookup-resource-without-fallback #:localize
           #:missing-resource #:defresources #:enable-sharpquote-reader
           #:with-sharpquote-reader #:lookup-first-matching-resource))

