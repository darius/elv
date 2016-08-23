(define (write c)
  (write-char c)
  (write-char (+ c 1))
  (write-char \
))

(spawn write \a)
(spawn write \A)
(spawn write \0)
