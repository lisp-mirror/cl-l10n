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
  `(cl-l10n::number-symbol ',(cl-l10n::ensure-language-symbol name)))

(defmacro currency-symbol (name)
  `(cl-l10n::currency-symbol ',(cl-l10n::ensure-language-symbol name)))

(defmacro currency-name (name)
  `(cl-l10n::currency-name ',(cl-l10n::ensure-language-symbol name)))

(defmacro language (name)
  `(cl-l10n::language-name ',(cl-l10n::ensure-language-symbol name)))

(defmacro script (name)
  `(cl-l10n::script-name ',(cl-l10n::ensure-language-symbol name)))

(defmacro territory (name)
  `(cl-l10n::territory-name ',(cl-l10n::ensure-language-symbol name)))

(defmacro variant (name)
  `(cl-l10n::variant-name ',(cl-l10n::ensure-language-symbol name)))

(defmacro month (name &key abbreviated capitalize-first-letter)
  `(cl-l10n::month-name ',(cl-l10n::ensure-language-symbol name)
                        :abbreviated ,abbreviated
                        :capitalize-first-letter ,capitalize-first-letter))

(defmacro day (name &key abbreviated capitalize-first-letter)
  `(cl-l10n::day-name ',(cl-l10n::ensure-language-symbol name)
                      :abbreviated ,abbreviated
                      :capitalize-first-letter ,capitalize-first-letter))

(defmacro quarter (name &key abbreviated capitalize-first-letter)
  `(cl-l10n::quarter-name ',(cl-l10n::ensure-language-symbol name)
                          :abbreviated ,abbreviated
                          :capitalize-first-letter ,capitalize-first-letter))

