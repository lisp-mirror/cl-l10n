
(defresources hu
  (yes "igen")
  (no "nem")
  (indefinite-article-for (str)
                          (declare (ignore str))
                          "egy")
  (definite-article-for (str)
                        (hungarian-definite-article-for str))
  (plural-of (str)
             (hungarian-plural-of str)))
