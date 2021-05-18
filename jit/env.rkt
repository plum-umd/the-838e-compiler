#lang racket
(begin
  (require "program-ast.rkt")
  (provide lookup ext defns-lookup zip)

  ;; Given an environment and an ID, return env[id]
  ;; Env Variable -> Answer
  (define (lookup env x)
    (match env
      ['() 'err]
      [(cons (list y i) env)
       (match (symbol=? x y)
         [#t i]
         [#f (lookup env x)])]))

  ;; Extend an environmen with ID and value
  ;; Env Variable Value -> Env
  (define (ext r x i)
    (cons (list x i) r))
    
  ;; Given list of Defns and an ID, return Defns[id]
  ;; Defns Symbol -> Defn
  (define (defns-lookup ds f)
    (match ds
      ['() 'err]
      [(cons d ds)
        (match d
          [(Defn g xs e) (if (symbol=? f g) d (defns-lookup ds f))])]))

  ;; Assumes that (length xs) == (length ys)
  ;; Takes two lists of same length and combines into a list of pairs
  (define (zip xs ys)
    (match xs
      ['() '()]
      [(cons x xs) 
        (match ys
          [(cons y ys)  (cons (list x y) (zip xs ys))])])))