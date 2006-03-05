(in-package :cl-l10n)

(defun parse-number (num &optional (locale *locale*))
  (let ((locale (locale-des->locale locale)))
    (%parse-number (replace-dp (remove-ts num locale) locale))))

(defun remove-ts (num locale)
  (let ((ts (locale-thousands-sep locale)))
    (case (length ts)
      (0 num)
      (1 (remove (schar ts 0) num))
      (t num)))) ; FIXME 

(defun replace-dp (num locale)
  (let ((dp (locale-decimal-point locale)))
    (case (length dp)
      (0 num)
      (1 (substitute #\. (schar dp 0) num))
      (t num)))) ; FIXME

;; money parser

;; EOF
