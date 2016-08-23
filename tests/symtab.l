(main (spawn symtab '()))

(define (main st)
  (! st (cons 'intern (cons 'world '())))
  (! st (cons 'intern (cons 'hello '())))
  (! st (cons 'get-symbols (cons (self) '())))
  (write-each (?))
  (write-char \
   ))

(define (write-each xs)
  (cond ((pair? xs)
         (write (symbol->string (car xs)))
         (write-each (cdr xs)))))

(define (write cs)
  (cond ((null? cs) (write-char \ ))
        ('t (write-char (car cs))
            (write (cdr cs)))))
  

(define (symtab xs)
  (symtabbing xs (?)))

(define (symtabbing xs request)
  (cond ((eq? (car request) 'intern)
         (symtab (adjoin (car (cdr request)) xs)))
        ((eq? (car request) 'get-symbols)
         (! (car (cdr request)) xs))
        ('t (symtab xs))))

(define (memq? x xs)
  (cond ((null? xs) (cond))
	((eq? x (car xs)) 't)
	('t (memq? x (cdr xs)))))

(define (adjoin x xs)
  (cond ((memq? x xs) xs)
        ('t (cons x xs))))
