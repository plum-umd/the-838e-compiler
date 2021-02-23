#lang racket
(provide append
         assq
         eighth
         first
         fifth
         fourth
         last
         length
         list
         list?
         list-ref
         list-tail
         memq
         ninth
         null?
         pair?
         remq
         remq*
         rest
         reverse
         second
         seventh
         sixth
         tenth
         third)

;; needs to be generalized to n-ary case eventually
(define (append xs ys)
  (match xs
    ['() ys]
    [(cons x xs) (cons x (append xs ys))]))

(define (assq v lst)
  (match lst
    ['() #f]
    [(cons p xs)
     (if (eq? (car p) v)
         p
         (assq v xs))]))

(define (eighth xs)
  (list-ref xs 7))

(define (first xs)
  (match xs
    [(cons x xs) x]))

(define (fifth xs)
  (list-ref xs 4))

(define (fourth xs)
  (list-ref xs 3))

(define (last xs)
  (match xs
    [(cons x xs) (last/acc x xs)]))

(define (last/acc x xs)
  (match xs
    ['() x]
    [(cons x xs)
     (last/acc x xs)]))

(define (length xs)
  (match xs
    ['() 0]
    [(cons x xs) (add1 (length xs))]))

(define (list . xs) xs)

(define (list? xs)
  (match xs
    ['() #t]
    [(cons x xs) (list? xs)]
    [_ #f]))

(define (memq v lst)
  (match lst
    ['() #f]
    [(cons x lst)
     (if (eq? x v)
         #t
         (memq v lst))]))

(define (list-ref lst pos)
  (if (zero? pos)
      (car lst)
      (list-ref (cdr lst) (sub1 pos))))

(define (list-tail lst pos)
  (match pos
    [0 lst]
    [_ (list-tail (cdr lst) (sub1 pos))]))

(define (ninth xs)
  (list-ref xs 8))

(define (null? x)
  (match x
    ['() #t]
    [_ #f]))

(define (pair? x)
  (match x
    [(cons x y) #t]
    [_ #f]))

(define (remq v lst)
  (match lst
    ['() '()]
    [(cons x lst)
     (if (eq? v x)
         lst
         (cons x (remq v lst)))]))

(define (remq* v lst)
  (match lst
    ['() '()]
    [(cons x lst)
     (if (eq? v x)
         (remq* v lst)
         (cons x (remq* v lst)))]))

(define (rest xs)
  (match xs
    [(cons x xs) xs]))

(define (reverse xs)
  (reverse/acc xs '()))

(define (reverse/acc xs ys)
  (match xs
    ['() ys]
    [(cons x xs)
     (reverse/acc xs (cons x ys))]))

(define (second xs)
  (list-ref xs 1))

(define (seventh xs)
  (list-ref xs 6))

(define (sixth xs)
  (list-ref xs 5))

(define (tenth xs)
  (list-ref xs 9))

(define (third xs)
  (list-ref xs 2))
