#lang racket
(provide = * byte? abs max min)

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

(define (abs x)
  (if (>= x 0) x (- 0 x)))

(define (max/2 x y)
  (if (> x y) x y))

(define (min/2 x y)
  (if (< x y) x y))

(define (max . xs)
  (match xs
    ['() -576460752303423488] ; int_min
    [(cons y ys) (max/2 y (apply max ys))]))

(define (min . xs)
  (match xs
    ['() 576460752303423487] ; int_max
    [(cons y ys) (min/2 y (apply min ys))]))
