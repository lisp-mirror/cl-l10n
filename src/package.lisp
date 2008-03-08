;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package #:cl-l10n.system)

(defpackage #:cl-l10n 
  (:use #:cl :alexandria :cl-ppcre :flexi-streams :cl-fad :iterate)
  (:shadow cl:format cl:formatter)
  (:shadowing-import-from :cl-fad
                          #:copy-stream #:copy-file)
  (:export
   #:locale
   #:locale-name
   #:current-locale
   #:locale-error
   #:load-all-locales
   #:with-locale

   #:format-number
   #:print-number
   #:format-money
   #:print-money
   #:format-time
   #:print-time
   #:parse-number

   #:*float-digits*
   #:shadow-format
   #:parse-time

   #:*resource-package*
   #:with-resource-package
   #:reload-resources
           
   #:capitalize-first-letter
   #:capitalize-first-letter!

   #:lookup-resource
   #:localize
   #:resource-missing
   #:defresources
   #:enable-sharpquote-reader
   #:with-sharpquote-reader
   #:lookup-first-matching-resource

   #:consonantp
   #:vowelp
   #:high-vowel-p
   #:low-vowel-p
   #:last-vowel-of
   #:starts-with-consonant-p
   #:starts-with-vowel-p
           
   #:english-plural-of
   #:english-indefinite-article-for
   #:hungarian-definite-article-for
   #:hungarian-plural-of
   ))

(defpackage #:cl-l10n.lang
  (:export
   #:symbol/decimal
   #:symbol/group
   #:symbol/list
   #:symbol/percent-sign
   #:symbol/native-zero-digit
   #:symbol/pattern-digit
   #:symbol/plus-sign
   #:symbol/minus-sign
   #:symbol/exponential
   #:symbol/per-mille
   #:symbol/infinity
   #:symbol/nan
   ))

(defpackage #:cl-l10n.ldml
  (:nicknames :ldml)
  (:export
   #:node

   #:ldml
   #:identity
   #:language
   #:script
   #:territory
   #:variant
   #:numbers
   #:symbols
#|
   #:locale-display-names
   #:layout
   #:orientation
   #:in-text
   #:characters
   #:exemplar-characters
   #:delimiters
   #:quotation-start
   #:quotation-end
   #:alternate-quotation-start
   #:alternate-quotation-end
   #:dates
   #:localized-pattern-chars
   #:date-range-pattern
   #:calendars
|#
   ))
