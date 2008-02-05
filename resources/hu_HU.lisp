
(defresources hu
  (yes "igen")
  (no "nem")
  (indefinite-article-for (str)
                          (declare (ignore str))
                          "egy")
  (definite-article-for (str)
                        (hungarian-definite-article-for str))
  (today "ma")
  (yesterday "tegnap")
  (tomorrow "holnap")
  (plural-of (str)
             (hungarian-plural-of str)))
