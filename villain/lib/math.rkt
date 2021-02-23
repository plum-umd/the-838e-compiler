#lang racket
(provide = * byte?)

;; not exactly the right place, but fine for now
(define (byte? b)
  (if (integer? b)
      (if (<= 0 b)
          (if (<= b 255)
              #t
              #f)
          #f)
      #f))

 ; multiplication
(define (*/2 x y)
  (if (zero? y)
      0
      (+ x (*/2 x (sub1 y)))))

(define (* . xs)
  (match xs
    ['() 1]
    [(cons x xs)
     (*/2 x (apply * xs))]))

(define (= . xs)
  (match xs
    [(cons x xs) (=/a x xs)]))

(define (=/a x xs)
  (match xs
    ['() #t]
    [(cons y xs)
     ; FIXME: using eq? because things are machine integers now
     (if (eq? x y)
         (=/a y xs)
         #f)]))
