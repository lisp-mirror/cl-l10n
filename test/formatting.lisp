;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-l10n.test)

(defsuite* (test/cldr/formatting :in test/cldr))

(deftest (test/cldr/formatting/bug/1) ()
  (bind ((compiled-pattern (compile-number-pattern/decimal "#,##0.###")))
    (flet ((to-string (number)
             (bind ((stream nil))
               (with-normalized-stream-variable stream
                 (funcall compiled-pattern stream number)))))
      (is (string= (to-string 3.9990) "3.999"))
      (is (string= (to-string 3.9995) "3.999"))
      (is (string= (to-string 3.9996) "4"))
      (is (string= (to-string 3.9999) "4"))
      (is (string= (to-string 3.99999999999d0) "4"))
      (is (string= (to-string 4) "4")))))
