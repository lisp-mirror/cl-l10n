;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package #:cl-l10n.system)

(defpackage #:cl-l10n 
  (:use #:cl #:cl-ppcre #:cl-fad)
  (:shadow cl:format cl:formatter)
  (:export #:locale-name #:category-name #:locale #:category #:locale-error
           #:get-category #:get-cat-val #:locale-value #:load-all-locales
           #:get-locale #:*locale* #:*locale-path* #:*locales*
           #:format-number #:print-number #:format-money #:print-money
           #:format-time #:print-time #:add-resources #:bundle 
           #:add-resource #:gettext #:parse-number #:*float-digits*
           #:parse-time #:month #:day #:year #:hour #:minute #:second
           #:date-divider #:time-divider #:weekday #:noon-midn 
           #:secondp #:am-pm #:zone #:parser-error))
           
