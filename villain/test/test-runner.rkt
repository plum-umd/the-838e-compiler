#lang racket
(provide test-runner test-runner-io)
(require rackunit)

(define (test-runner run)
  ;; Abscond examples
  (check-equal? (run 7) 7)
  (check-equal? (run -8) -8)

  ;; Blackmail examples
  (check-equal? (run '(add1 (add1 7))) 9)
  (check-equal? (run '(add1 (sub1 7))) 7)

  ;; Con examples
  (check-equal? (run '(if (zero? 0) 1 2)) 1)
  (check-equal? (run '(if (zero? 1) 1 2)) 2)
  (check-equal? (run '(if (zero? -7) 1 2)) 2)
  (check-equal? (run '(if (zero? 0)
                          (if (zero? 1) 1 2)
                          7))
                2)
  (check-equal? (run '(if (zero? (if (zero? 0) 1 0))
                          (if (zero? 1) 1 2)
                          7))
                7)
  
  ;; String examples 
  (check-equal? (run "Racket") "Racket")
  (check-equal? (run "Rack") "Rack")
  (check-equal? (run "Ra") "Ra")
  (check-equal? (run "R") "R")
  (check-equal? (run "") "")
  (check-equal? (run '(string-length "Rack")) 4)
  (check-equal? (run '(string-length "")) 0)
  (check-equal? (run '(string-ref "Racket" 0)) #\R)
  (check-equal? (run '(string-ref "Racket" 5)) #\t)
  (check-equal? (run '(string-ref "Racket" 3)) #\k)
  (check-equal? (run '(string-ref "Racket" 6)) 'err)
  (check-equal? (run '(string-ref "Racket" -1)) 'err)
  (check-equal? (run '(string? "Racket")) #t)
  (check-equal? (run '(string? "")) #t)
  (check-equal? (run '(string? 5)) #f)
  (check-equal? (run '(string? #\a)) #f)
  (check-equal? (run '(string? '())) #f)
  (check-equal? (run '(string? #t)) #f)
  (check-equal? (run '(make-string 5 #\y)) "yyyyy")
  (check-equal? (run '(make-string 3 #\y)) "yyy")
  (check-equal? (run '(make-string 1 #\y)) "y")
  (check-equal? (run '(make-string 0 #\y)) "")
  (check-equal? (run '(make-string -1 #\y)) 'err)
  (check-equal? (run '(string-set! (make-string 5 #\y) 2 #\n)) (void))
  (check-equal? (run '(let ((str (make-string 5 #\y)))
                        (begin (string-set! str 2 #\n) str))) "yynyy")
  (check-equal? (run '(let ((str (make-string 5 #\y)))
                        (begin (string-set! str 1 #\n) str))) "ynyyy")
  (check-equal? (run '(let ((str (make-string 5 #\y)))
                        (begin (string-set! str 3 #\n) str))) "yyyny")
  (check-equal? (run '(let ((str (make-string 5 #\y)))
                        (begin (string-set! str 4 #\n) str))) "yyyyn")
  (check-equal? (run '(let ((str (make-string 4 #\y)))
                        (begin (string-set! str 3 #\n) str))) "yyyn")
  (check-equal? (run '(let ((str (make-string 3 #\y)))
                        (begin (string-set! str 2 #\n) str))) "yyn")
  (check-equal? (run '(let ((str (make-string 2 #\y)))
                        (begin (string-set! str 0 #\n) str))) "ny")  
  
  ;; if r8 is not pushed backed on stack before jump to 'raise error, these two
  ;; tests cause invalid memory reference and loss of some debugging context
  (check-equal? (run '(let ((str (make-string 3 #\y)))
                        (begin (string-set! str 3 #\n) str))) 'err) 
  (check-equal? (run '(let ((str (make-string 3 #\y)))
                        (begin (string-set! str -1 #\n) str))) 'err)

  ;; Dupe examples
  (check-equal? (run #t) #t)
  (check-equal? (run #f) #f)
  (check-equal? (run (if #t 1 2)) 1)
  (check-equal? (run (if #f 1 2)) 2)
  (check-equal? (run (if 0 1 2)) 1)
  (check-equal? (run '(if #t 3 4)) 3)
  (check-equal? (run '(if #f 3 4)) 4)
  (check-equal? (run '(if  0 3 4)) 3)
  (check-equal? (run '(zero? 4)) #f)
  (check-equal? (run '(zero? 0)) #t)
  ;; Dodger examples
  (check-equal? (run #\a) #\a)
  (check-equal? (run #\b) #\b)
  (check-equal? (run '(char? #\a)) #t)
  (check-equal? (run '(char? #t)) #f)
  (check-equal? (run '(char? 8)) #f)
  (check-equal? (run '(char->integer #\a)) (char->integer #\a))
  (check-equal? (run '(integer->char 955)) #\λ)
  ;; Extort examples
  (check-equal? (run '(add1 #f)) 'err)
  (check-equal? (run '(sub1 #f)) 'err)
  (check-equal? (run '(zero? #f)) 'err)
  (check-equal? (run '(char->integer #f)) 'err)
  (check-equal? (run '(integer->char #f)) 'err)
  (check-equal? (run '(integer->char -1)) 'err)
  (check-equal? (run '(write-byte #f)) 'err)
  (check-equal? (run '(write-byte -1)) 'err)
  (check-equal? (run '(write-byte 256)) 'err)
  ;; Fraud examples
  (check-equal? (run '(let ((x 7)) x)) 7)
  (check-equal? (run '(let ((x 7)) 2)) 2)
  (check-equal? (run '(let ((x 7)) (add1 x))) 8)
  (check-equal? (run '(let ((x (add1 7))) x)) 8)
  (check-equal? (run '(let ((x 7)) (let ((y 2)) x))) 7)
  (check-equal? (run '(let ((x 7)) (let ((x 2)) x))) 2)
  (check-equal? (run '(let ((x 7)) (let ((x (add1 x))) x))) 8)

  (check-equal? (run '(let ((x 0))
                        (if (zero? x) 7 8)))
                7)
  (check-equal? (run '(let ((x 1))
                        (add1 (if (zero? x) 7 8))))
                9)
  (check-equal? (run '(+ 3 4)) 7)
  (check-equal? (run '(- 3 4)) -1)
  (check-equal? (run '(+ (+ 2 1) 4)) 7)
  (check-equal? (run '(+ (+ 2 1) (+ 2 2))) 7)
  (check-equal? (run '(let ((x (+ 1 2)))
                        (let ((z (- 4 x)))
                          (+ (+ x x) z))))
                7)
  ;; Hustle examples
  (check-equal? (run ''()) '())
  (check-equal? (run '(box 1)) (box 1))
  (check-equal? (run '(cons 1 2)) (cons 1 2))
  (check-equal? (run '(unbox (box 1))) 1)
  (check-equal? (run '(car (cons 1 2))) 1)
  (check-equal? (run '(cdr (cons 1 2))) 2)
  (check-equal? (run '(cons 1 '())) (list 1))
  (check-equal? (run '(let ((x (cons 1 2)))
                        (begin (cdr x)
                               (car x))))
                1)
  (check-equal? (run '(let ((x (cons 1 2)))
                        (let ((y (box 3)))
                          (unbox y))))
                3)
  ;; Iniquity tests
  (check-equal? (run
                 '(begin (define (f x) x)
                         (f 5)))
                5)
  (check-equal? (run
                 '(begin (define (tri x)
                           (if (zero? x)
                               0
                               (+ x (tri (sub1 x)))))
                         (tri 9)))
                45)
  (check-equal? (run
                 '(begin
                    (define (len lst)
                      (if (empty? lst)
                          0
                          (+ 1 (len (cdr lst)))))
                    (len (cons 1 (cons 2 (cons 3 '()))))))
                3)

  (check-equal? (run '(<= 0 0)) #t)
  (check-equal? (run '(<= 0 1)) #t)
  (check-equal? (run '(<= 1 0)) #f)

  ;; Pattern Matching Tests
  (check-equal? (run
                 '(match 2 [1 1] [2 2]))
                2)
  (check-equal? (run
                 '(match #t [#t #t] [#f #f]))
                #t)
  (check-equal? (run
                 '(match #\a [#\b #\b] [#\a #\a] [#\c #\c]))
                #\a)
  (check-equal? (run '(match 'x ['x #t] [_ #f])) #t)
  (check-equal? (run '(match 'x ['y #t] [_ #f])) #f)
  (check-equal? (run '(match #f ['y #t] [_ #f])) #f)
  (check-equal? (run '(match 'x ['y #f] ['x #t] [_ #f])) #t)
  (check-equal? (run
                 '(match (cdr (cons 3 '())) ['() #t] [eof #f]))
                #t)
  (check-equal? (run
                 '(match (let ((x (cons 5 (cons 6 (cons #\a '()))))) (car x))
                    [5 #t]
                    [1 #f]
                    [2 (let ((y #\c)) (char->integer y))]))
                #t)

  (check-equal? (run
                 '(match (cons #t #f)
                    [(cons a b) a]
                    [5 2]))
                #t)

  (check-equal? (run
                 '(match (cons #t #f)
                    [(cons a b) b]
                    [5 2]))
                #f)

  (check-equal? (run
                 '(match (cons #t #f)
                    [(cons a b) (char? a)]
                    [5 2]))
                #f)

  (check-equal? (run
                 '(match (cons 1 2)
                    [(cons a b) (+ a b)]
                    [5 2]))
                3)

  (check-equal? (run
                 '(match (cons #t #f)
                    [(cons a b) (eq? a #t)]
                    [5 2]))
                #t)

  (check-equal? (run
                 '(match (cons #t #f)
                    [(cons a b) (eq? b #f)]
                    [5 2]))
                #t)

  (check-equal? (run
                 '(match (cons #t #f)
                    [(cons a b) (begin (eq? a #t) (eq? b #f))]
                    [5 2]))
                #t)
  (check-equal? (run
                 '(match (box 5)
                    [5 #f]
                    [(box v) (let ((y v)) (+ y v))]))
                10)

  (check-equal? (run
                 '(let ((x (cons #\a (cons 2 (cons #\c '())))))
                    (match x
                      [(cons h t) (car t)]
                      [(cons h v) h])))
                2)

  (check-equal? (run
                 '(match 5
                    [1 #f]
                    [2 #f]))
                'err)

  (check-equal? (run
                 '(begin
                    (define (len lst)
                      (match lst
                        [(cons h t) (+ 1 (len t))]
                        ['() 0]))
                    (len (cons 1 (cons 2 (cons 3 '()))))))
                 3)
    (check-equal? (run
                 '(begin
                    (define (len lst)
                      (match lst
                        [(cons h t) (+ 1 (len t))]))
                    (len (cons 1 (cons 2 (cons 3 '()))))))
                 'err)

  (check-equal? (run
                 '(begin (define (tri x)
                           (if (zero? x)
                               0
                               (+ x (tri (sub1 x)))))
                         (tri 9 6)))
                'err) 
  
  (check-equal? (run
                 '(begin (define (tri x)
                           (if (zero? x)
                               0
                               (+ x (tri (sub1 x)))))
                         (tri )))
                'err)

  (check-equal? (run '(integer? 0)) #t)
  (check-equal? (run '(integer? #f)) #f)
  (check-equal? (run '(byte? 0)) #t)
  (check-equal? (run '(byte? 255)) #t)
  (check-equal? (run '(byte? 256)) #f)
  (check-equal? (run '(byte? -1)) #f)
  (check-equal? (run '(integer-length   0)) 0)
  (check-equal? (run '(integer-length  -1)) 0)
  (check-equal? (run '(integer-length   1)) 1)
  (check-equal? (run '(integer-length  -2)) 1)
  (check-equal? (run '(integer-length  16)) 5)
  (check-equal? (run '(integer-length -16)) 4)

  ;;Prefab structure tests
  (check-equal? (run '(make-prefab-struct 'coord 4 5)) (make-prefab-struct 'coord 4 5))
  (check-equal? (run '(make-prefab-struct 'empty)) (make-prefab-struct 'empty))
  (check-equal? (run '(begin
                        (struct coord (x y) #:prefab)
                        (struct empt () #:prefab)
                        (define (extract-x c)
                          (coord-x c))
                        (define (extract-y c)
                          (coord-y c))

                        (extract-x (coord (box 1) (box 2)))))
                (box 1))
  (check-equal? (run '(begin
                        (struct coord (x y) #:prefab)
                        (define (extract-x c)
                          (coord-x c))
                        (struct empt () #:prefab)
                        (define (extract-y c)
                          (coord-y c))

                        (extract-y (coord (box 1) (box 2)))))
                (box 2))

  (check-equal? (run '(begin
                        (struct coord (x y) #:prefab)
                        (define (extract-x c)
                          (coord-x c))
                        (define (extract-y c)
                          (coord-y c))
                        (struct empt () #:prefab)
                        (let ((c (coord (cons 1 (cons 2 '())) (cons 1 (cons 2 '())))))
                          (coord? c))))
                #t)

  (check-equal? (run '(begin
                        (struct coord (x y) #:prefab)
                        (struct empt () #:prefab)
                        (define (extract-x c)
                          (coord-x c))
                        (define (extract-y c)
                          (coord-y c))

                        (let ((c (coord (cons 1 (cons 2 '())) (cons 1 (cons 2 '()))))
                              (e (empt)))
                          (coord? e))))
                #f)

  (check-equal? (run '(begin
                        (struct coord (x y) #:prefab)
                        (struct empt () #:prefab)
                        (define (extract-x c)
                          (coord-x c))
                        (define (extract-y c)
                          (coord-y c))

                        (let ((c (coord (cons 1 (cons 2 '())) (cons 1 (cons 2 '()))))
                              (e (empt)))
                          (empt? e))))
                #t)
  
  (check-equal? (run '(begin
                        (struct coord (x y) #:prefab)
                        (struct empt () #:prefab)
                        (define (extract-x c)
                          (coord-x c))
                        (define (extract-y c)
                          (coord-y c))

                        (coord? (box 1))))
                #f)

  (check-equal? (run '(begin
                        (struct coord (x y) #:prefab)
                        (struct empt () #:prefab)
                        (define (extract-x c)
                          (coord-x c))
                        (define (extract-y c)
                          (coord-y c))

                        (coord-x (box 1))))
                'err)

  (check-equal? (run '(begin
                        (struct coord (x y) #:prefab)
                        (struct empt () #:prefab)
                        (define (extract-x c)
                          (coord-x c))
                        (define (extract-y c)
                          (coord-y c))

                        (coord? (make-prefab-struct 'coord 1 2 3))))
                #f)

  (check-equal? (run '(begin
                        (struct coord (x y) #:prefab)
                        (struct empt () #:prefab)
                        (define (extract-x c)
                          (coord-x c))
                        (define (extract-y c)
                          (coord-y c))

                        (coord? (make-prefab-struct 'coord 1 2))))
                #t)
  
  
#|
  (check-equal? (run
                 '(begin (define (even? x)
                           (if (zero? x)
                               #t
                               (odd? (sub1 x))))
                         (define (odd? x)
                           (if (zero? x)
                               #f
                               (even? (sub1 x))))
                         (even? 101)))
                #f)

  (check-equal? (run
                 '(begin (define (map-add1 xs)
                           (if (empty? xs)
                               '()
                               (cons (add1 (car xs))
                                     (map-add1 (cdr xs)))))
                         (map-add1 (cons 1 (cons 2 (cons 3 '()))))))
  '(2 3 4))|#

  (check-equal? (run '(char-whitespace? #\a)) #f)
  (check-equal? (run '(char-whitespace? #\ )) #t)

  ;; symbols
  (check-equal? (run ''foo) 'foo)
  (check-equal? (run '(string->symbol "foo"))
                'foo)
  (check-equal? (run '(symbol? "foo"))
                #f)
  (check-equal? (run '(symbol? 'foo))
                 #t)
  (check-equal? (run '(symbol? (string->symbol "foo")))
                 #t)
  (check-equal? (run '(symbol? (gensym)))
                 #t)
  (check-equal? (run '(symbol->string 'foo))
                 "foo")
  (check-equal? (run '(eq? 'foo
                            (string->symbol "foo")))
                #t)
  (check-equal? (run '(eq? (string->symbol "foo")
                           (string->symbol "foo")))
                #t)
  (check-equal? (run '(eq? (string->symbol "foo")
                           (string->symbol "bar")))
                #f)
  (check-equal? (run '(symbol->string 'foo))
                "foo")
  (check-equal? (run '(eq? (gensym) (gensym)))
                #f)
  (check-equal? (run '(let ([x (gensym)]) (eq? x x)))
                #t)
  
    ;; Testing floats
  (check-equal? (run 4.2) 4.2)
  (check-equal? (run -4.2) -4.2)
  
  (check-equal? (run 3.3333) 3.3333)
  (check-equal? (run 790.321) 790.321)
  (check-equal? (run -8990.32) -8990.32)
  (check-equal? (run -9999999) -9999999)
  (check-equal? (run .9999999) .9999999)

  ;; Errors and stack alignment
  (define (check-err e)
    ;; check error in both aligned and unaligned config
    (check-equal? (run e) 'err)
    (check-equal? (run `(let ((x 0)) ,e)) 'err)) 

  ;; Variable arity functions tests 
  (check-equal? (run
                 '(begin  
                    (define (at-least-two-lst x y . xs) (cons x (cons y xs))) 
                    (at-least-two-lst 1 2 3)))
                '(1 2 3)) 
  
  (check-equal? (run
                 '(begin  
                    (define (at-least-two-lst x y . xs) (cons x (cons y xs))) 
                    (at-least-two-lst 1 2)))
                '(1 2)) 
  
  (check-equal? (run
                 '(begin  
                    (define (at-least-two-lst x y . xs) xs) 
                    (at-least-two-lst 1)))
                'err)

  (check-equal? (run
                 '(begin  
                    (define (at-least-two-lst x y . xs) (cons x (cons y xs))) 
                    (at-least-two-lst 1 2 (cons 4 '()))))
                '(1 2 (4))) 

  (check-equal? (run
                 '(begin  
                    (define (ret-two . xs) 2) 
                    (ret-two 1 2 (cons 4 '()))))
                  2) 

  (check-equal? (run
                 '(begin  
                    (define (ret-two . xs) 2) 
                    (ret-two 1 2 (cons 4 '()) #\a)))
                  2)

  

  (check-err '(add1 #f))
  (check-err '(sub1 #f))
  (check-err '(zero? #f))
  (check-err '(integer-length #f))
  (check-err '(integer->char #f))
  (check-err '(integer->char 1114112))
  (check-err '(integer->char 55296))
  (check-err '(+ #f 0))
  (check-err '(+ 0 #f))
  (check-err '(- #f 0))
  (check-err '(- 0 #f))
  (check-err '(string-ref "a" #f))
  (check-err '(make-string #f #\a))
  (check-err '(string-set! "a" #f #\b))
  (check-err '(write-byte #f))
  (check-err '(write-byte -1))
  (check-err '(write-byte 256))
  (check-err '(char->integer #f))
  (check-err '(char-whitespace? #f))
  (check-err '(char-alphabetic? #f))
  (check-err '(char-upcase #f))
  (check-err '(char-downcase #f))
  (check-err '(char-titlecase #f))
  (check-err '(make-string 1 #f))
  (check-err '(string-set! "a" 0 #f))
  (check-err '(unbox #f))
  (check-err '(car #f))
  (check-err '(cdr #f))
  (check-err '(string-length #f))
  (check-err '(string-ref #f 0))
  (check-err '(string-set! #f 0 #\b))
  (check-err '(string-ref "a" -1))
  (check-err '(string-ref "a" 1))
  (check-err '(make-string -1 #\a))
  (check-err '(string-set! "a" -1 #\b))
  (check-err '(string-set! "a" 1 #\b))
  (check-err '(match '() [#f #f]))


  ;; Standard library: list.rkt
  (check-equal? (run '(append '() '())) '())
  (check-equal? (run '(append '() (list 4 5 6)))
                (list 4 5 6))
  (check-equal? (run '(append (list 1 2 3) '()))
                (list 1 2 3))
  (check-equal? (run '(append (list 1 2 3) (list 4 5 6)))
                (list 1 2 3 4 5 6))
  (check-equal? (run '(append (list 1 2 3) (cons 4 5)))
                (cons 1 (cons 2 (cons 3 (cons 4 5)))))
  (check-equal? (run '(assq 'x '())) #f)
  (check-equal? (run '(assq 'x (list (list 'x 1)))) '(x 1))
  (check-equal? (run '(assq 'x (list (list 'y 1)))) #f)
  (check-equal? (run '(assq 'x (list (list 'y 1) (list 'x 2)))) '(x 2))
  (check-equal? (run '(assq 'x (list (list 'x 1) (list 'x 2)))) '(x 1))
  (check-equal? (run '(eighth (list 1 2 3 4 5 6 7 8 9 10))) 8)
  (check-equal? (run '(first (list 1 2 3 4 5 6 7 8 9 10))) 1)
  (check-equal? (run '(fifth (list 1 2 3 4 5 6 7 8 9 10))) 5)
  (check-equal? (run '(fourth (list 1 2 3 4 5 6 7 8 9 10))) 4)
  (check-equal? (run '(last (list 1 2 3 4 5 6 7 8 9 10))) 10)
  (check-equal? (run '(length (list 1 2 3 4 5 6 7 8 9 10))) 10)
  (check-equal? (run '(list)) '())
  (check-equal? (run '(list 1)) '(1))
  (check-equal? (run '(list? '())) #t)
  (check-equal? (run '(list? (list 1))) #t)
  (check-equal? (run '(list? (cons 1 2))) #f)
  (check-equal? (run '(list? #f)) #f)
  (check-equal? (run '(memq 'x (list 'x 'y 'z))) #t)
  (check-equal? (run '(memq 'z (list 'x 'y 'z))) #t)
  (check-equal? (run '(memq 'q (list 'x 'y 'z))) #f)
  (check-equal? (run '(memq 'x '())) #f)
  (check-equal? (run '(list-ref (list 1 2 3 4 5 6 7 8 9 10) 0)) 1)
  (check-equal? (run '(list-ref (list 1 2 3 4 5 6 7 8 9 10) 9)) 10)
  (check-equal? (run '(list-tail (list 1 2 3 4 5 6 7 8 9 10) 10)) '())
  (check-equal? (run '(list-tail (cons 'x 'y) 1)) 'y)
  (check-equal? (run '(ninth (list 1 2 3 4 5 6 7 8 9 10))) 9)
  (check-equal? (run '(null? '())) #t)
  (check-equal? (run '(null? #f)) #f)
  (check-equal? (run '(null? (list 1))) #f)
  (check-equal? (run '(pair? '())) #f)
  (check-equal? (run '(pair? #f)) #f)
  (check-equal? (run '(pair? (list 1))) #t)
  (check-equal? (run '(pair? (cons 1 2))) #t)
  (check-equal? (run '(remq 'x (list 'x 'y 'z))) '(y z))
  (check-equal? (run '(remq 'x (list 'x 'y 'z 'x))) '(y z x))
  (check-equal? (run '(remq 'z (list 'x 'y 'z))) '(x y))
  (check-equal? (run '(remq 'q (list 'x 'y 'z))) '(x y z))
  (check-equal? (run '(remq 'x '())) '())
  (check-equal? (run '(remq* 'x (list 'x 'y 'z))) '(y z))
  (check-equal? (run '(remq* 'x (list 'x 'y 'z 'x))) '(y z))
  (check-equal? (run '(remq* 'z (list 'x 'y 'z))) '(x y))
  (check-equal? (run '(remq* 'q (list 'x 'y 'z))) '(x y z))
  (check-equal? (run '(remq* 'x '())) '())
  (check-equal? (run '(rest (list 1))) '())
  (check-equal? (run '(rest (list 1 2 3))) '(2 3))
  (check-equal? (run '(reverse '())) '())
  (check-equal? (run '(reverse (list 1 2 3))) '(3 2 1))
  (check-equal? (run '(second (list 1 2 3 4 5 6 7 8 9 10))) 2)
  (check-equal? (run '(seventh (list 1 2 3 4 5 6 7 8 9 10))) 7)
  (check-equal? (run '(sixth (list 1 2 3 4 5 6 7 8 9 10))) 6)
  (check-equal? (run '(tenth (list 1 2 3 4 5 6 7 8 9 10))) 10)
  (check-equal? (run '(third (list 1 2 3 4 5 6 7 8 9 10))) 3)

  ;; Standard library: bool.rkt
  (check-equal? (run '(boolean? #t)) #t)
  (check-equal? (run '(boolean? #f)) #t)
  (check-equal? (run '(boolean? 0)) #f)
  (check-equal? (run '(not #t)) #f)
  (check-equal? (run '(not #f)) #t)
  (check-equal? (run '(not 0)) #f)
  
  ;; n-ary let
  (check-equal? (run '(let () 4)) 4)
  (check-equal? (run '(let ((x 4)) 3)) 3)
  (check-equal? (run '(let ((x 4)) x)) 4)
  (check-equal? (run '(let ((x 4) (y 6)) (+ x y))) 10)
  (check-equal? (run '(let ((x (let ((y 4)) y)) (y 6)) (+ x y))) 10)
  (check-equal? (run '(let ((y 6) (x (let ((y 4)) y))) (+ x y))) 10)
  (check-equal? (run '(let ((y (let ((y 4)) y)) (x (let ((z 4)) z)) (z 2)) (+ z (+ x y)))) 10)
  (check-equal? (run '(let ((x (add1 12)) (y 9)) (let ((x (add1 x)) (z y)) (+ z x)))) 23)

  ;; cond
  (check-equal? (run '(cond)) (void))
  (check-equal? (run '(cond (else 1))) 1)
  (check-equal? (run '(cond (#t 1))) 1)
  (check-equal? (run '(cond (#t 2) (else 1))) 2)
  (check-equal? (run '(cond (#f 2) (else 1))) 1)
  (check-equal? (run '(cond (#f 2) (else 1))) 1)
  (check-equal? (run '(cond (0 2) (else 1))) 2)
  )


(define (test-runner-io run)
  ;; Evildoer examples
  (check-equal? (run 7 "") (cons 7 ""))
  (check-equal? (run '(write-byte 97) "") (cons (void) "a"))
  (check-equal? (run '(read-byte) "a") (cons 97 ""))
  (check-equal? (run '(begin (write-byte 97) (read-byte)) "b")
                (cons 98 "a"))
  (check-equal? (run '(read-byte) "") (cons eof ""))
  (check-equal? (run '(eof-object? (read-byte)) "") (cons #t ""))
  (check-equal? (run '(eof-object? (read-byte)) "a") (cons #f ""))
  (check-equal? (run '(begin (write-byte 97) (write-byte 98)) "")
                (cons (void) "ab"))

  (check-equal? (run '(peek-byte) "ab") (cons 97 ""))
  (check-equal? (run '(begin (peek-byte) (read-byte)) "ab") (cons 97 ""))
  ;; Extort examples
  (check-equal? (run '(write-byte #t) "") (cons 'err ""))

  ;; Fraud examples
  (check-equal? (run '(let ((x 97)) (write-byte x)) "") (cons (void) "a"))
  (check-equal? (run '(let ((x 97))
                        (begin (write-byte x)
                               x))
                     "")
                (cons 97 "a"))
  (check-equal? (run '(let ((x 97)) (begin (read-byte) x)) "b")
                (cons 97 ""))
  (check-equal? (run '(let ((x 97)) (begin (peek-byte) x)) "b")
                (cons 97 ""))

  ;; Hustle examples
  (check-equal? (run '(let ((x 1))
                        (begin (write-byte 97)
                               1))
                     "")
                (cons 1 "a"))

  (check-equal? (run '(let ((x 1))
                        (let ((y 2))
                          (begin (write-byte 97)
                                 1)))
                     "")
                (cons 1 "a"))

  (check-equal? (run '(let ((x (cons 1 2)))
                        (begin (write-byte 97)
                               (car x)))
                     "")
                (cons 1 "a"))
  ;; Iniquity examples
  (check-equal? (run '(begin (define (print-alphabet i)
                               (if (zero? i)
                                   (void)
                                   (begin (write-byte (- 123 i))
                                          (print-alphabet (sub1 i)))))
                             (print-alphabet 26))
                     "")
                (cons (void) "abcdefghijklmnopqrstuvwxyz"))


 

  )
