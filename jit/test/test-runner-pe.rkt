#lang racket
(provide test-runner) ; test-runner-io)
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
  (check-equal? (run '(if (char? (integer->char 60)) #\b #\c)) #\b)


  ;;Evildoer examples
  (check-equal? (run '(read-byte)) '(unload (cons () (read-byte))))
  (check-equal? (run '(write-byte (add1 2))) '(unload (cons () (write-byte 3))))
  (check-equal? (run '(peek-byte)) '(unload (cons () (peek-byte))))
  (check-equal? (run '(void)) (void))
  (check-equal? (run  '(eof-object? eof)) #t)
  (check-equal? (run '(begin 1 2)) 2)
  (check-equal? (run '(if (read-byte) (add1 1) (sub1 2)))
                '(unload
                  (match
                      (cons () (read-byte))
                    (('err 'err)
                     ((cons h v)
                      (if v
                          (interp-env-heap (add1 1) () h)
                          (interp-env-heap (sub1 2) () h)))))))
  (check-equal? (run '(if (zero? (read-byte)) (add1 1) (sub1 2)))
                '(unload
                  (match
                      (match
                          (cons () (read-byte))
                        (('err 'err) ((cons h a) (interp-prim1 'zero? a h))))
                    (('err 'err)
                     ((cons h v)
                      (if v
                          (interp-env-heap (add1 1) () h)
                          (interp-env-heap (sub1 2) () h)))))))
  (check-equal? (run '(begin (eof-object? (read-byte)) (begin 2 (eof-object? eof))))
                '(unload
                  (match
                      (match
                          (cons () (read-byte))
                        (('err 'err) ((cons h a) (interp-prim1 'eof-object? a h))))
                    (('err 'err) (_ (interp-env-heap (begin 2 (eof-object? eof)) () ()))))))
  (check-equal? (run '(add1 (peek-byte)))
                '(unload
                 (match
                     (cons () (peek-byte))
                   (('err 'err) ((cons h a) (interp-prim1 'add1 a h))))))
  (check-equal? (run '(if (zero? 0) (read-byte) 1)) '(unload (cons () (read-byte))))
  (check-equal? (run '(if (zero? 1) (read-byte) (void))) (void))

  ;;Extort Examples
  (check-equal? (run '(add1 #f)) ''err)
  (check-equal? (run '(sub1 #f)) ''err)
  (check-equal? (run '(zero? #f)) ''err)
  (check-equal? (run '(char->integer #f)) ''err)
  (check-equal? (run '(integer->char #f)) ''err)
  (check-equal? (run '(integer->char -1)) ''err)
  (check-equal? (run '(write-byte #f)) ''err)
  (check-equal? (run '(write-byte -1)) ''err)
  (check-equal? (run '(write-byte 256)) ''err)

  ;; Fraud Examples  
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

  (check-equal? (run '(let ((x 97)) (write-byte x))) '(unload (cons () (write-byte 97))))
  (check-equal? (run '(let ((x 97)) (begin (write-byte x) x)))
                '(unload
                  (match
                      (cons () (write-byte 97))
                    (('err 'err)
                     (_ (interp-env-heap x (cons (cons 'x (cons 97 ())) ()) ()))))))
  (check-equal? (run '(let ((x 97)) (begin (read-byte) x)))
                '(unload
                  (match
                      (cons () (read-byte))
                    (('err 'err)
                     (_ (interp-env-heap x (cons (cons 'x (cons 97 ())) ()) ()))))))
  (check-equal? (run '(let ((x 97)) (begin (peek-byte) x)))
                '(unload
                  (match
                      (cons () (peek-byte))
                    (('err 'err)
                     (_ (interp-env-heap x (cons (cons 'x (cons 97 ())) ()) ()))))))

  ;;Hustle Examples
  (check-equal? (run ''()) '())
  (check-equal? (run '(box 1)) (box 1))
  (check-equal? (run '(cons 1 2)) '(cons 1 2))
  (check-equal? (run '(unbox (box 1))) 1)
  (check-equal? (run '(car (cons 1 2))) 1)
  (check-equal? (run '(cdr (cons 1 2))) 2)
  (check-equal? (run '(cons 1 '())) '(cons 1 ()))
  (check-equal? (run '(let ((x (cons 1 2)))
                        (begin (cdr x)
                               (car x))))
                1)
  (check-equal? (run '(let ((x (cons 1 2)))
                        (car x)))
                 1)
  (check-equal? (run '(let ((x (cons 1 2)))
                        (cdr x)))
                 2)
  (check-equal? (run '(let ((x (cons 1 2)))
                        (let ((y (box 3)))
                          (unbox y))))
                 3)
  (check-equal? (run '(cons 1 (cons 2 (box 1))))
                `(cons 1 (cons 2 ,(box 1)))))


  