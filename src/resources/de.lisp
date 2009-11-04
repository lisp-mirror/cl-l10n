(in-package :cl-l10n.lang)

(defresources de
  (yes "ja")
  (no "nein")
  (indefinite-article-for (str)
    (de-indefinite-article-for str))
  (definite-article-for (str)
    (de-definite-article-for str))
  (today "heute")
  (yesterday "gestern")
  (tomorrow "morgen")
  (plural-of (str)
    (de-plural-of str)))
