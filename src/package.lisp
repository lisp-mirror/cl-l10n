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
  (:use :common-lisp :cl-l10n)

  (:shadowing-import-from :cl-l10n
   #:defresources)

  (:export

   ;; <numbers> / <symbols>
   #:number-symbol

   #:decimal
   #:group
   #:list
   #:percent-sign
   #:native-zero-digit
   #:pattern-digit
   #:plus-sign
   #:minus-sign
   #:exponential
   #:per-mille
   #:infinity
   #:nan

   #:currency-symbol
   #:currency-display-name
   #:language
   #:script
   #:territory
   #:variant
   ))

(defpackage #:cl-l10n.ldml
  (:nicknames :ldml)
  (:export
   #:node

   #:ldml
   #:identity
   #:language
   #:languages
   #:script
   #:territory
   #:variant
   #:numbers
   #:symbols
   #:currencies
   #:currency
   #:display-name
   #:script
   #:scripts
   #:territories
   #:territory
   #:variants
   #:variant
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

(block check-lisp-source-file-encoding
  (map nil (lambda (a b)
             (unless (eql a (char-code b))
               (cerror "try it anyway"
                       "Your lisp seems to be reading .lisp files in something else then UTF-8. The source files of cl-l10n contain literal strings with unicode characters and failing to properly read them in UTF-8 will cause problems.")
               (return-from check-lisp-source-file-encoding)))
       #(233 225 250 337 243 246 369 237)
       "éáúőóöűí"))
