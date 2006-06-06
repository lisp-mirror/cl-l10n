;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package #:cl-l10n.system)

(defpackage #:cl-l10n 
  (:use #:cl #:cl-ppcre #:cl-fad #:split-sequence)
  (:shadow cl:format cl:formatter)
  (:export #:locale-name #:category-name #:locale #:category #:locale-error
           #:get-category #:get-cat-val #:locale-value #:load-all-locales
           #:get-locale #:*locale-path* #:*locales* #:load-default-locale
           #:format-number #:print-number #:format-money #:print-money
           #:format-time #:print-time #:add-resources 
           #:parse-number #:*float-digits*
           #:parse-time #:month #:day #:year #:hour #:minute #:second
           #:date-divider #:time-divider #:weekday #:noon-midn #:shadow-format
           #:secondp #:am-pm #:zone #:parser-error #:set-locale
           #:with-locale #:lookup-resource
           #:lookup-resource-without-fallback #:localize
           #:missing-resource #:defresources #:enable-sharpquote-reader
           #:with-sharpquote-reader))


