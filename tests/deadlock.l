(main (spawn symtab))

(define (main st)
  (! st 'z)  ;(cons 'z '()))
  ;(write-char (?))
  ;(write-char \
 ;  )
)

(define (symtab)
  (symtabbing (?)))

(define (symtabbing request)
  (cond ((symbol? request)
         (write (symbol->string request)))
        (#t (write-char \!))))

(define (write s)
  (cond ((null? s) 'ok)
        (#t (write-char (car s))
            (write (cdr s)))))
