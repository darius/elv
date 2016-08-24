(define linefeed \
 )

(define false (cond))
(define true (eq? \a \a))

(define (error complaint)
  (write-string complaint)
  (write-char linefeed)
  (abort))

(define (list1 z)     (cons z '()))
(define (list3 x y z) (cons x (cons y (cons z '()))))

(define (append xs ys)
  (cond ((null? xs) ys)
        ('t (cons (car xs) (append (cdr xs) ys)))))

(define (reverse xs)
  (revappend xs '()))

(define (revappend xs ys)
  (cond ((null? xs) ys)
	('t (revappend (cdr xs) (cons (car xs) ys)))))

(define (memq? x xs)
  (cond ((null? xs) false)
	((eq? x (car xs)) 't)
	('t (memq? x (cdr xs)))))

(define (length xs)
  (list1 (length-digit xs "0123456789")))

(define (length-digit xs digits)
  (cond ((null? xs) (car digits))
	('t (length-digit (cdr xs) (cdr digits)))))

(define (string? x)
  (cond ((null? x) 't)
	((char? x) false)
	((char? (car x)) (string? (cdr x)))
	('t false)))

(define (string=? s t)
  (cond ((null? s) (null? t))
        ((null? t) false)
	((eq? (car s) (car t)) (string=? (cdr s) (cdr t)))
	('t false)))

(define (write-string chars)
  (cond ((pair? chars)
	 (write-char (car chars))
	 (write-string (cdr chars)))))

(define (cons! x xs-box)
  (set-car! xs-box (cons x (car xs-box))))

(define (adjoin! x xs-box)
  (cond ((eq? false (memq? x (car xs-box)))
	 (cons! x xs-box))))


(define primitives '(eq? null? boolean? char? pair? cons car cdr set-car!
                     symbol? string->symbol symbol->string 
                     read-char peek-char write-char abort))

(define symbols-box (list1 (append '(define quote cond) primitives)))
(define (intern? x) (memq? x (car symbols-box)))
(define (intern s)  (interning s (car symbols-box)))

(define (interning s symbols)
  (cond ((null? symbols) (cons! s symbols-box) s)
	((string=? s (car symbols)) (car symbols))
	('t (interning s (cdr symbols)))))


(define (read)
  (read-dispatch (skip-blanks (read-char))))

(define (skip-blanks c)
  (cond ((memq? c whitespace-chars) (skip-blanks (read-char)))
        ((eq? c \;) (skip-blanks (skip-comment (read-char))))
	('t c)))

(define (skip-comment c)
  (cond ((eq? c false) eof-object)
        ((eq? c linefeed) (read-char))
        ('t (skip-comment (read-char)))))

(define whitespace-chars (cons linefeed " 	"))
(define non-symbol-chars (cons false "\"\\(')"))

(define eof-object '("eof"))

(define (read-dispatch c)
  (cond ((eq? c false) eof-object)
	((eq? c \\) (read-char-literal (read-char)))
	((eq? c \") (read-string (read-char)))
	((eq? c \() (read-list))
	((eq? c \') (cons 'quote (cons (read) '())))
        ((eq? c \#) (parse-boolean (read-symbol (peek-char))))
	((eq? c \)) (write-string "Unbalanced parentheses")
                    (write-char linefeed)
                    (echo (read-char))
                    (abort))
	('t (intern (cons c (read-symbol (peek-char)))))))

(define (echo c)
  (cond (c (write-char c) (echo (read-char)))))

(define (read-char-literal c)
  (cond ((eq? c false) (error "EOF in character literal"))
	('t c)))

(define (read-string c)
  (cond ((eq? c false) (error "Unterminated string literal"))
	((eq? c \") '())
        ((eq? c \\) (cons (read-char) (read-string (read-char))))
	('t (cons c (read-string (read-char))))))

(define (read-symbol c)
  (cond ((memq? c whitespace-chars) '())
	((memq? c non-symbol-chars) '())
	('t (read-char) (cons c (read-symbol (peek-char))))))

(define (parse-boolean string)
  (cond ((string=? string "f") false)
        ((string=? string "t") true)
        ('t (error "Bad # syntax"))))

(define (read-list)
  (read-list-dispatch (skip-blanks (read-char))))

(define (read-list-dispatch c)
  (cond ((eq? c false) (error "Unterminated list"))
	((eq? c \)) '())
	('t (cons (read-dispatch c) (read-list)))))


(define (push1         z k) (append z (cons linefeed k)))
(define (push3     x y z k) (append x (append y (push1 z k))))
(define (push5 v w x y z k) (append v (append w (push3 x y z k))))


(define (compile)
  (write-string (compile-procs '(()) (read) '() '() "")))

(define (compile-procs syms form var-defs exprs k)
  (cond ((eq? eof-object form)
         (do-compile-defs syms (reverse var-defs)
           (compile-proc syms '__main '() (reverse exprs) k)))
	((cond ((pair? form) (eq? 'define (car form)))
	       ('t false))
         (cond ((intern? (car (cdr form)))
                (compile-procs syms (read) (cons form var-defs) exprs k))
               ('t (compile-procs syms (read) var-defs exprs
                     (compile-proc syms
                                   (proc.name   form)
			           (proc.params form)
			           (proc.body   form)
                                   k)))))
        ('t (compile-procs syms (read) var-defs (cons form exprs) k))))

(define (do-compile-defs syms var-defs k)
  (compile-symbols syms var-defs
    (compile-defs syms var-defs
      (emit-start k))))

(define (compile-symbols syms var-defs k)
  (emit-globals (append (map-def.name (make-symbol-defs syms))
                        (map-def.name var-defs))
    (emit-prelude
      (compile-defs syms (make-symbol-defs syms) k))))

(define (compile-defs syms defs k)
  (cond ((pair? defs)
	 (compile-def syms (def.name (car defs)) (def.expr (car defs))
           (compile-defs syms (cdr defs) k)))
        ('t k)))

(define (express syms x)
  (cond ((intern? x) (adjoin! x syms) (symbol->var x))
	((pair? x) (express-pair syms x))
	('t x)))

(define (express-pair syms x)
  (list3 'cons (express syms (car x)) (express syms (cdr x))))

(define (make-symbol-defs syms)
  (making-symbol-defs (car syms) '()))

(define (making-symbol-defs symbols defs)
  (cond ((null? symbols) defs)
	('t (making-symbol-defs 
             (cdr symbols)
             (cons (list3 'define (symbol->var (car symbols))
		          (express-pair '() (car symbols)))
		   defs)))))

(define (symbol->var sym)
  (intern (cons \. sym)))

(define (proc.name proc)   (car (car (cdr proc))))
(define (proc.params proc) (cdr (car (cdr proc))))
(define (proc.body proc)        (cdr (cdr proc)))

(define (def.name def)          (car (cdr def)))
(define (def.expr def)     (car (cdr (cdr def))))

(define (map-def.name defs)
  (cond ((null? defs) '())
	('t (cons (def.name (car defs))
		  (map-def.name (cdr defs))))))

(define (compile-proc syms name params body k)
  (emit-proc name params
    (compile-seq syms body params 't
      (emit-end-proc k))))

(define (compile-seq syms es vars tail? k)
  (cond ((null? (cdr es))
         (compile-expr syms (car es) vars tail? k))
	('t (compile-expr syms (car es) vars false
              (emit-pop
                (compile-seq syms (cdr es) vars tail? k))))))

(define (compile-exprs syms es vars k)
  (cond ((null? es) k)
        ('t (compile-expr syms (car es) vars false
              (compile-exprs syms (cdr es) vars k)))))

(define (compile-expr syms e vars tail? k)
  (cond ((char? e) (emit-char-lit e (maybe-return tail? k)))
	((null? e) (emit-nil (maybe-return tail? k)))
        ((pair? e)
	 (cond ((intern? e)
	        (cond ((memq? e vars) (emit-local e (maybe-return tail? k)))
	              ('t (emit-global e (maybe-return tail? k)))))
               ((string? e) (compile-expr syms (express syms e) vars tail? k))
	       ('t (compile-pair syms (car e) (cdr e) vars tail? k))))
        ('t (emit-boolean e (maybe-return tail? k)))))

(define (maybe-return tail? k)
  (cond (tail? (emit-return k))
        ('t k)))

(define (compile-pair syms rator rands vars tail? k)
  (cond ((eq? rator 'cond) (compile-cond syms rands vars tail? k))
        ((eq? rator 'quote)
         (compile-expr syms (express syms (car rands)) vars tail? k))
	('t (compile-exprs syms rands vars
              (compile-call rator (length rands) tail? k)))))

(define (compile-cond syms clauses vars tail? k)
  (cond ((null? clauses) (emit-boolean false (maybe-return tail? k)))
	('t (compile-expr syms (car (car clauses)) vars false
              (emit-if
                (compile-seq syms (cdr (car clauses)) vars tail?
                  (emit-else
                    (compile-cond syms (cdr clauses) vars tail?
                      (emit-end-if k)))))))))

(define (compile-call rator n-args tail? k)
  (cond ((memq? rator primitives)
	 (emit-prim rator n-args
           (maybe-return tail? k)))
	(tail? (emit-tail-call rator n-args k))
        ('t    (emit-call      rator n-args k))))

(define c-char-map-domain  (list3 linefeed \'     \\))
(define c-char-map-range   (list3 "\\n"   "\\'" "\\\\"))
(define (c-char-literal c)
  (translit c (list1 c) c-char-map-domain c-char-map-range))

(define (c-id str) str)

(define (translit x default domain range)
  (cond ((null? domain) default)
        ((eq? x (car domain)) (car range))
        ('t (translit x default (cdr domain) (cdr range)))))

(define (comma names k)
  (cond ((null? names) (push1 "" k))
        ('t (cons \  (append (c-id (car names)) (comma (cdr names) k))))))

(define (emit-start k)         (push1 "  call 0 __main" 
                                 (push1 "  halt" k)))
(define (emit-globals names k) (append "  globals" (comma names k)))
(define (emit-locals names k)  (append "  locals"  (comma names k)))
(define (emit-prelude k)       k)

(define (emit-proc name params k)
  (push1 ""
    (push3 "proc " (c-id name) ""
      (emit-locals params k))))

(define (emit-end-proc k) k)

(define (emit-check-def name k)
  (push3 "  checkdef " (c-id name) "" k))

(define (emit-return k)
  (push1 "  return" k))

(define (emit-tail-call name n-args k)
  (push5 "  tailcall " n-args " " (c-id name) "" k))

(define (emit-call name n-args k)
  (push5 "  call " n-args " " (c-id name) "" k))

(define (emit-prim name n-args k)
  (push5 "  prim " n-args " " (c-id name) "" k))

(define (emit-pop k)
  (push1 "  pop" k))

(define (emit-if k)     (push1 "  if" k))
(define (emit-else k)   (push1 "  else" k))
(define (emit-end-if k) (push1 "  then" k))

(define (emit-char-lit c k)
  (push3 "  char '" (c-char-literal c) "'" k))

(define (emit-nil k)
  (push1 "  nil" k))

(define (emit-boolean b k)
  (push1 (cond (b "  true") ('t "  false")) k))

(define (emit-local name k)
  (push3 "  local " (c-id name) "" k))

(define (emit-global name k)
  (push3 "  global " (c-id name) "" k))

(define (compile-def syms name e k)
  (compile-expr syms e '() false
    (emit-check-def name k)))

(compile)
