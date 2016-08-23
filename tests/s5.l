(define false (cond))

  (define (string? x)
    (cond ((null? x) 't)
          ((char? x) false)
          ((char? (car x)) (string? (cdr x)))
          ('t false)))

  (define (print x)
    (write x)
    (write-char \
))

  (define (write x)
    (cond ((null? x) (write-string '"()"))
          ((char? x) (write-char \\) (write-char x))
          ((symbol? x) (write-string (symbol->string x)))
          ((string? x) (write-char \") (write-string x) (write-char \"))
          ((pair? x)
           (write-char \()
           (write (car x))
           (write-each (cdr x))
           (write-char \)))))
          
  (define (write-each xs)
    (cond ((null? xs) false)
          ('t (write-char \ )
              (write (car xs))
              (write-each (cdr xs)))))
          
  (define (write-string chars)
    (cond ((null? chars) false)
          ('t (write-char (car chars))
              (write-string (cdr chars)))))
          
  (print '())
  (print '"abc")
  (print '(\a (\b)))
  (print (cons (string->symbol '"hello") '(f "world")))
