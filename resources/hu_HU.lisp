
(defresources hu
  (yes "igen")
  (no "nem")
  (indefinit-article-for (str)
                         (declare (ignore str))
                         "egy")
  (plural-of (str)
             (hungarian-plural-of str)))
