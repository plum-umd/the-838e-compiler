#lang racket
(begin
  (require "program-ast.rkt")
  (provide lookup ext defns-lookup zip)

  ;; Env Variable -> Answer
  (define (lookup env x)
    (match env
      ['() 'err]
      [(cons (list y i) env)
       (match (symbol=? x y)
         [#t i]
         [#f (lookup env x)])]))

  ;; Env Variable Value -> Value
  (define (ext r x i)
    (cons (list x i) r))
    
  ;; Defns Symbol -> Defn
  (define (defns-lookup ds f)
    (match ds
      ['() 'err]
      [(cons d ds)
        (match d
          [(Defn g xs e) (if (symbol=? f g) d (defns-lookup ds f))])]))

  ;; Assumes that (length xs) == (length ys)
  (define (zip xs ys)
    (match xs
      ['() '()]
      [(cons x xs) 
        (match ys
          [(cons y ys)  (cons (list x y) (zip xs ys))])])))