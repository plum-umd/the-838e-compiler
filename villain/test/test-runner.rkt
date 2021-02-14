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

  (check-equal? (run '(integer-length   0)) 0)
  (check-equal? (run '(integer-length  -1)) 0)
  (check-equal? (run '(integer-length   1)) 1)
  (check-equal? (run '(integer-length  -2)) 1)
  (check-equal? (run '(integer-length  16)) 5)
  (check-equal? (run '(integer-length -16)) 4)

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

    ;; Testing floats
  (check-equal? (run 4.2) 4.2)
   (check-equal? (run -4.2) -4.2)
  
  (check-equal? (run 3.3333) 3.3333)
  (check-equal? (run 790.321) 790.321)
  (check-equal? (run -8990.32) -8990.32)
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
