(start (spawn process))

(define (start pid)
  (! pid \a))

(define (process)
  (write-char (?)))
