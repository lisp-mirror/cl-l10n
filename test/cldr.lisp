;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-l10n.test)

(defmacro def-symbol-test (name accessors &body forms)
  `(deftest ,name ()
     ,@(iter (for (locales . body) :in forms)
             (collect `(flet ((one-pass ()
                                ,@(iter (for (symbol . values) :in body)
                                        (collect `(progn
                                                    ,@(iter (for value :in values)
                                                            (for accessor :in accessors)
                                                            (collect `(is (string= (,accessor ,symbol)
                                                                                   ,value)))))))))
                         ,@(iter (for locale :in (ensure-list locales))
                                 (collect `(with-locale ,locale
                                             (one-pass)))))))))

(defsuite (cldr :in test))
(in-suite cldr)

(defsuite (symbols :in cldr))
(in-suite symbols)

(def-symbol-test test/cldr/symbols/number-symbols (cl-l10n.lang:number-symbol)
  ("en_US_POSIX"

   (per-mille "0/00")
   (infinity "INF"))

  (("en_US" "en_GB" "en")

   (decimal ".")
   (group ",")
   (list ";")
   (percent-sign "%")
   (native-zero-digit "0")
   (pattern-digit "#")
   (plus-sign "+")
   (minus-sign "-")
   (exponential "E")
   (per-mille "‰")
   (infinity "∞")
   (nan "NaN")))

(defsuite (currencies :in cldr))
(in-suite currencies)

(def-symbol-test test/currencies (cl-l10n.lang:currency-name cl-l10n.lang:currency-symbol)
  (("en_US_POSIX" "en_US" "en_GB" "en")

   (usd   "US Dollar"              "$")
   ("USN" "US Dollar (Next day)"   nil)
   (uak   "Ukrainian Karbovanetz"  nil)
   (huf   "Hungarian Forint"       "Ft")
   ("GBP" "British Pound Sterling" "£"))

  (("hu_HU" "hu")

   (usd   "USA dollár"                  "USD")
   ("USN" "USA dollár (következő napi)" nil)
   (uak   "Ukrán karbovanec"            nil)
   (huf   "Magyar forint"               "Ft")
   ("GBP" "Brit font sterling"          "GBP")))

(defsuite (languages :in cldr))
(in-suite languages)

(def-symbol-test test/languages/language (cl-l10n.lang:language)
  (("en_US_POSIX" "en_US" "en_GB" "en")

   (aa        "Afar")
   ("ace"     "Achinese")
   (zza       "Zaza")
   (zh_Hans   "Simplified Chinese")
   ("zh_Hant" "Traditional Chinese")
   (zh        "Chinese"))

  (("hu_HU" "hu")

   (aa        "afar")
   ("ace"     "achinéz")
   (zza       "zaza")
   ;; The next two may break until support for the cldr 'draft' attribute is implemented
   (zh_Hans   "kínai (egyszerűsített)")
   ("zh_Hant" "kínai (hagyományos)")
   (zh        "kínai")))

(def-symbol-test test/languages/script (cl-l10n.lang:script)
  (("en_US_POSIX" "en_US" "en_GB" "en")

   (Arab      "Arabic")
   ("Zyyy"    "Common")
   (Tibt      "Tibetan")
   (Knda      "Kannada")
   ("Ital"    "Old Italic"))

  (("hu_HU" "hu")

   ;;(Arab      "Arabic") missing from the xml?!
   ("Zyyy"    "Általános")
   (Visp      "Látható beszéd")
   (Tibt      "Tibeti")
   (Knda      "Kannada")
   ("Ital"    "Régi olasz")))


(def-symbol-test test/languages/territory (cl-l10n.lang:territory)
  (("en_US_POSIX" "en_US" "en_GB" "en")

   (011       "Western Africa")
   (200       "Czechoslovakia")
   (BD        "Bangladesh")
   (ZZ        "Unknown or Invalid Region")
   ("HU"      "Hungary"))

  (("hu_HU" "hu")

   (019       "Amerika")
   (142       "Ázsia")
   (BD        "Banglades")
   (ZZ        "Ismeretlen vagy érvénytelen terület")
   ("HU"      "Magyarország")))

(def-symbol-test test/languages/variant (cl-l10n.lang:variant)
  (("en_US_POSIX" "en_US" "en_GB" "en")

   (1901      "Traditional German orthography")
   (REVISED   "Revised Orthography")
   (ROZAJ     "Resian")
   (POSIX     "Computer"))

  (("hu_HU" "hu")

   (1901      "Hagyományos német helyesírás")
   (REVISED   "Átdolgozott helyesírás")
   (ROZAJ     "Resian")
   (POSIX     "Számítógép")))

#+nil
(

;; TODO revive stuff here
 
(deftest load-locs
    (progn (locale "en_ZA") (locale "sv_SE") (locale "en_GB")
           (locale "en_US") (locale "af_ZA") t)
  t)

;;; Format number tests

(deftest number.1
    (format nil "~v:/cl-l10n:format-number/" "en_ZA" 1000)
  "1,000")

(deftest number.2
    (format nil "~v:@/cl-l10n:format-number/" "en_ZA" 1000)
  "1000")

(deftest number.3
    (format nil "~v/cl-l10n:format-number/" "en_ZA" 1000)
  "1,000.00")

(deftest number.4
    (format nil "~v/cl-l10n:format-number/" "sv_SE" 1000)
  "1 000,00")

(deftest number.5
    (format nil "~v:/cl-l10n:format-number/" "sv_SE" 1000)
  "1 000")

(deftest number.6
    (format nil "~v:/cl-l10n:format-number/" "sv_SE" 1/2)
  "0,50")

(deftest number.7
    (format nil "~v:/cl-l10n:format-number/" "en_GB" 100.12312d0)
  "100.12")

;;; Money tests

(deftest money.1
    (format nil "~v:/cl-l10n:format-money/" "en_ZA" 1000)
  "ZAR 1,000.00")

(deftest money.2
    (format nil "~v@/cl-l10n:format-money/" "en_ZA" 1000)
  "R1000.00")

(deftest money.3
    (format nil "~v:@/cl-l10n:format-money/" "en_ZA" 1000)
  "ZAR 1000.00")

(deftest money.4
    (format nil "~v:/cl-l10n:format-money/" "sv_SE" 1000)
  "1 000,00 SEK")

(deftest money.5
    (format nil "~v@/cl-l10n:format-money/" "sv_SE" 1000)
  "1000,00 kr")

(deftest money.6
    (format nil "~v:@/cl-l10n:format-money/" "sv_SE" 1000)
  "1000,00 SEK")

;;; Time tests

(deftest time.1
    (format nil "~v,,v:@/cl-l10n:format-time/" "en_ZA" 0 3091103120)
  "Sun 14 Dec 1997 15:45:20 +0000")

;;; FIXME
(deftest time.2
    (format nil "~v,,v:@/cl-l10n:format-time/" "sv_SE" 0 3091103120)
  #.(format nil "s~Cn 14 dec 1997 15.45.20"
            #+(or sb-unicode clisp) #\LATIN_SMALL_LETTER_O_WITH_DIAERESIS
            #-(or sb-unicode clisp) (code-char #xF6)))

(deftest time.3
    (format nil "~v,,v/cl-l10n:format-time/" "en_US" 0 3091103120)
  "03:45:20 ")

(deftest time.4
    (format nil "~v:/cl-l10n:format-time/" "en_US" 3091103120)
  "12/14/1997")

(deftest time.5
    (format nil "~v,,v@/cl-l10n:format-time/" "en_US" 0 3091103120)
  "15:45:20 ")

(deftest time.6
    (format nil "~v,,v@/cl-l10n:format-time/" "sv_SE" 0 3091103120)
  "15.45.20")

(defmacro def-time-directive-test (name directive result)
  `(deftest ,name (format nil "~v,v,vU" "en_ZA" ,directive 0 3320556360)
     ,result))

(def-time-directive-test directive.1 "%%" "%")
(def-time-directive-test directive.2 "%a" "Wed")
(def-time-directive-test directive.3 "%A" "Wednesday")
(def-time-directive-test directive.4 "%b" "Mar")
(def-time-directive-test directive.5 "%B" "March")
(def-time-directive-test directive.6 "%c" "Wed 23 Mar 2005 08:46:00 +0000")
(def-time-directive-test directive.7 "%C" "20")
(def-time-directive-test directive.8 "%d" "23")
(def-time-directive-test directive.9 "%D" "03/23/05")
(def-time-directive-test directive.10 "%e" "23")
(def-time-directive-test directive.11 "%F" "2005-03-23")
(def-time-directive-test directive.12 "%g" "05")
(def-time-directive-test directive.13 "%G" "2005")
(def-time-directive-test directive.14 "%h" "Mar")
(def-time-directive-test directive.15 "%H" "08")
(def-time-directive-test directive.16 "%I" "08")
(def-time-directive-test directive.17 "%j" "082")
(def-time-directive-test directive.18 "%k" " 8")
(def-time-directive-test directive.19 "%l" " 8")
(def-time-directive-test directive.21 "%m" "03")
(def-time-directive-test directive.22 "%M" "46")
(def-time-directive-test directive.23 "%n" "
")
(def-time-directive-test directive.24 "%N" "000000000")
(def-time-directive-test directive.25 "%p" "")
(def-time-directive-test directive.26 "%P" "")
(def-time-directive-test directive.27 "%r" "08:46:00 ")
(def-time-directive-test directive.28 "%R" "08:46")
(def-time-directive-test directive.29 "%s" "1111567560")
(def-time-directive-test directive.30 "%S" "00")
(def-time-directive-test directive.31 "%t" "	")
(def-time-directive-test directive.32 "%T" "08:46:00")
(def-time-directive-test directive.33 "%u" "3")
;;(def-time-directive-test directive.34 "%U" "12")
;;(def-time-directive-test directive.35 "%V" "12")
(def-time-directive-test directive.36 "%w" "3")
;;(def-time-directive-test directive.37 "%W" "12")
(def-time-directive-test directive.38 "%x" "23/03/2005")
(def-time-directive-test directive.39 "%X" "08:46:00")
(def-time-directive-test directive.40 "%y" "05")
(def-time-directive-test directive.41 "%Y" "2005")
(def-time-directive-test directive.42 "%z" "+0000")
(def-time-directive-test directive.43 "%Z" "+0000")

;;; i18n tests 

#| TODO: obolete

(defvar *my-bundle* (make-instance 'bundle))

(add-resources (*my-bundle* "af_")
  "howareyou" "Hoe lyk it")

(add-resources (*my-bundle* "en")
  "howareyou" "How are you")

(deftest i18n.1 
    (gettext "howareyou" *my-bundle* "en_ZA")
  "How are you")

(deftest i18n.2
    (gettext "howareyou" *my-bundle* "af_ZA")
  "Hoe lyk it")

|#

;;; format

(deftest format.1
    (format nil "~v,,v:@U" "en_ZA" -2 3091103120)
  "Sun 14 Dec 1997 17:45:20 +0200")

(deftest format.2
    (format nil "~v:n" "en_ZA" 1000)
  "1,000")

(deftest format.3
    (format nil "~v:@m" "sv_SE" 1000)
  "1000,00 SEK")

;;; formatter

(deftest formatter.1
    (format nil (formatter "~v,,v:@U") "en_ZA" -2 3091103120)
  "Sun 14 Dec 1997 17:45:20 +0200")

(deftest formatter.2
    (format nil (formatter "~v:n") "en_ZA" 1000)
  "1,000")

(deftest formatter.3
    (format nil (formatter "~v:@m") "sv_SE" 1000)
  "1000,00 SEK")

;;; parse-number

(deftest parse-number.1
    (parse-number (format nil "~vn" "af_ZA" -1001231.5) "af_ZA")
  -1001231.5)

(deftest parse-number.2
    (parse-number (format nil "~v@:n" "en_ZA" -1001231.5) "en_ZA")
  -1001231.5)

(deftest parse-number.3
    (parse-number (format nil "~v@:n" "sv_SE" -1001231.5) "sv_SE")
  -1001231.5)

;;; parse-time 

(deftest parse-time.1
    (let ((*locale* "en_ZA")
          (time (get-universal-time)))
      (= time (parse-time (format nil "~:U~:* ~@U" time))))
  t)

(deftest parse-time.2
    (let ((*locale* "sv_SE")
          (time (get-universal-time)))
      (= time (parse-time (format nil "~:U~:* ~@U" time))))
  t)

(deftest parse-time.3
    (let ((*locale* "en_US")
          (time (get-universal-time)))
      (= time (parse-time (format nil "~:U~:* ~@U" time))))
  t)

(deftest parse-time.4
    (let ((*locale* "en_GB")
          (time (get-universal-time)))
      (= time (parse-time (format nil "~:U~:* ~@U" time))))
  t)

(deftest parse-time.5
    (parse-time "05/04/03" :default-zone -2 :locale "en_ZA")
  3258482400)

(deftest parse-time.6
    (parse-time "05/04/03" :default-zone -2  :locale "en_US")
  3260988000)

(deftest parse-time.7
    (parse-time "05/04/03"  :default-zone -2 :locale "en_ZA")
  3258482400)

(deftest parse-time.8
    (let ((*locale* "en_ZA")
          (time (get-universal-time)))
      (= time (parse-time (format nil "~:@U" time))))
  t)

(deftest parse-time.9
    (let ((*locale* "en_US")
          (time (get-universal-time)))
      (= time (parse-time (format nil "~:@U" time))))
  t)

(deftest parse-time.10
    (let ((*locale* "sv_SE")
          (time (get-universal-time)))
      (= time (parse-time (format nil "~:@U" time))))
  t)

)
