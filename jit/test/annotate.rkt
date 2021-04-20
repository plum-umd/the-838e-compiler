#lang racket
(require rackunit "../annotate.rkt" "../parse.rkt" "../ast.rkt")


(define (p-annotate e)
  (annotate (parse e)))

;;Dupe tests
(check-equal? (p-annotate 1) (Green (Int 1)))
(check-equal? (p-annotate #t) (Green (Bool #t)))
(check-equal? (p-annotate '(if #t 1 #f)) (Green (If (Green (Bool #t)) (Green (Int 1)) (Green (Bool #f)))))
(check-equal? (p-annotate '(add1 1)) (Green (Prim1 'add1 (Green (Int 1)))))
(check-equal? (p-annotate '(sub1 1)) (Green (Prim1 'sub1 (Green (Int 1)))))
(check-equal? (p-annotate '(zero? 100)) (Green (Prim1 'zero? (Green (Int 100)))))

