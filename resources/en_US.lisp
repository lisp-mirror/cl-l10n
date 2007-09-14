
(defresources en_us
  (yes "yes")
  (no "no")
  (indefinite-article-for (str)
                          (english-indefinite-article-for str))
  (definite-article-for (str)
                        (declare (ignore str))
                        "the")
  (plural-of (str)
             (english-plural-of str)))

