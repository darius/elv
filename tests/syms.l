(string->symbol "xyz")
(string->symbol "abc")
(string->symbol "yo")
(cond ((eq? (string->symbol "abc") (string->symbol "abc"))
       (write-char \y))
      ('t (write-char \n)))
(write-char \
)
