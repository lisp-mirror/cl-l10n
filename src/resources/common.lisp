(in-package :cl-l10n.lang)

(defun with-indefinite-article (str &key capitalize-first-letter)
  (let ((article (indefinite-article-for str)))
    (concatenate 'string
                 (if capitalize-first-letter
                     (capitalize-first-letter article)
                     article)
                 (list #\Space)
                 str)))

(defun with-definite-article (str &key capitalize-first-letter)
  (let ((article (definite-article-for str)))
    (concatenate 'string
                 (if capitalize-first-letter
                     (capitalize-first-letter article)
                     article)
                 (list #\Space)
                 str)))

(defmacro number-symbol (name)
  `(cl-l10n.lang:localize-number-symbol
    ',(cl-l10n::ensure-ldml-symbol name)))

(defmacro currency-symbol (name)
  `(cl-l10n.lang:localize-currency-symbol
    ',(cl-l10n::ensure-ldml-symbol name)))

(defmacro currency-name (name)
  `(cl-l10n.lang:localize-currency-name
    ',(cl-l10n::ensure-ldml-symbol name)))

(defmacro language (name)
  `(cl-l10n.lang:localize-language-name
    ',(cl-l10n::ensure-ldml-symbol name)))

(defmacro script (name)
  `(cl-l10n.lang:localize-script-name
    ',(cl-l10n::ensure-ldml-symbol name)))

(defmacro territory (name)
  `(cl-l10n.lang:localize-territory-name
    ',(cl-l10n::ensure-ldml-symbol name)))

(defmacro variant (name)
  `(cl-l10n.lang:localize-variant-name
    ',(cl-l10n::ensure-ldml-symbol name)))

(defmacro month (name &key abbreviated capitalize-first-letter)
  `(cl-l10n.lang:localize-month-name
    ',(cl-l10n::ensure-ldml-symbol name)
    :abbreviated ,abbreviated
    :capitalize-first-letter ,capitalize-first-letter))

(defmacro day (name &key abbreviated capitalize-first-letter)
  `(cl-l10n.lang:localize-day-name
    ',(cl-l10n::ensure-ldml-symbol name)
    :abbreviated ,abbreviated
    :capitalize-first-letter ,capitalize-first-letter))

(defmacro quarter (name &key abbreviated capitalize-first-letter)
  `(cl-l10n.lang:localize-quarter-name
    ',(cl-l10n::ensure-ldml-symbol name)
    :abbreviated ,abbreviated
    :capitalize-first-letter ,capitalize-first-letter))

