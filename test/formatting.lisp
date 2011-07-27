;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-l10n.test)

(defsuite* (test/cldr/formatting :in test/cldr))

(deftest (test/cldr/formatting/bug/1) ()
  (flet ((to-string (number)
           (format-number/decimal nil number :pattern "#,##0.###")))
    (is (string= (to-string 3.9990) "3.999"))
    (is (string= (to-string 3.9995) "3.999"))
    (is (string= (to-string 3.9995d0) "3.999"))
    (is (string= (to-string 3.9996) "4"))
    (is (string= (to-string 3.9999) "4"))
    (is (string= (to-string 3.99999999999d0) "4"))
    (is (string= (to-string 4) "4"))))

;; i'm not sure if the following two is strictly speaking a bug, but it's definitely annoying behaviour...
(deftest (test/cldr/formatting/bug/2) ()
  (with-expected-failures
    (is (string= (format-number/decimal nil 3.9995d0 :pattern "#,##0.########################")
                 "3.9995"))))

(deftest (test/cldr/formatting/bug/3) ()
  (bind ((number (/ 7999 2000)))
    (flet ((to-string (number)
             (format-number/decimal nil number :pattern "#,##0.###")))
      (with-expected-failures
        (is (string= (to-string number)
                     (to-string (coerce number 'double-float))))))))
