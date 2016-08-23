(define false (cond))

(define (string? x)
  (cond ((null? x) 't)
        ((char? x) false)
        ((char? (car x)) (string? (cdr x)))
        ('t false)))

(define (try x)
  (write-char (cond ((string? x) \t) ('t \f)))
  (write-char \
))

(try '())
(try '"hell")
(try '(\h \e "ll"))
