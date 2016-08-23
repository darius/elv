(define false (cond))

  (define (string? x)
    (cond ((pair? x)
           (cond ((char? (car x)) (string? (cdr x)))
                 ('t false)))
          ('t (null? x))))

  (define (string=? s t)
    (cond ((null? s) (null? t))
          ((null? t) false)
          ((eq? (car s) (car t)) (string=? (cdr s) (cdr t)))
          ('t false)))

  (define (memq? x xs)
    (cond ((null? xs) false)
          ((eq? x (car xs)) 't)
          ('t (memq? x (cdr xs)))))

  (define (print x)
    (write x)
    (newline))

  (define (newline)
    (write-char \
))

  (define (write x)
    (cond ((null? x) (write-string '"()"))
          ((char? x) (write-char \\) (write-char x))
          ((symbol? x) (write-string (symbol->string x)))
          ((string? x) (write-char \") (write-string x) (write-char \"))
          ((boolean? x) (write-char \#)
                        (write-char (cond (x \t) ('t \f))))
          ('t (write-char \()
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


(define (error plaint)
  (write-string plaint)
  (newline)
  (abort))          

(define (read)
  (skip-blanks (peek-char))
  (read-dispatch (read-char)))

(define (skip-blanks c)
  (cond ((char-whitespace? c)
	 (read-char)
	 (skip-blanks (peek-char)))
	('t 'ok)))

(define (char-whitespace? c)
  (memq? c '" 
	"))

(define non-symbol-chars (cons \" '"\(')"))

(define eof-object '("eof"))

(define (read-dispatch c)
  (cond ((eq? c false) eof-object)
	((eq? c \\) (read-char-literal (read-char)))
	((eq? c \") (read-string (read-char)))
	((eq? c \() (read-list))
	((eq? c \') (cons 'quote (cons (read) '())))
	((eq? c \)) (error '"Unbalanced parentheses"))
	('t (string->symbol (cons c (read-symbol (peek-char)))))))

(define (read-char-literal c)
  (cond ((eq? c false) (error '"EOF in character literal"))
	('t c)))

(define (read-string c)
  (cond ((eq? c false) (error '"Unterminated string literal"))
	((eq? c \") '())
	('t (cons c (read-string (read-char))))))

(define (read-symbol c)
  (cond ((char-whitespace? c) '())
	((memq? c non-symbol-chars) '())
	('t (read-char) (cons c (read-symbol (peek-char))))))

(define (read-list)
  (skip-blanks (peek-char))
  (read-list-dispatch (read-char)))

(define (read-list-dispatch c)
  (cond ((eq? c false) (error '"Unterminated list"))
	((eq? c \)) '())
	('t (cons (read-dispatch c) (read-list)))))


(print (read))
(print (read))
