#lang racket

(require rackunit "../parse.rkt" "../unparse.rkt")

(define (check-parse-unparse e)
  (check-equal? (unparse (parse e)) e))

;;Dupe tests
(check-parse-unparse 1)
(check-parse-unparse #t)
(check-parse-unparse '(if #t 1 2))
(check-parse-unparse '(if (zero? 0) (add1 3) (sub1 (add1 3))))
(check-parse-unparse '(sub1 (add1 (sub1 (add1 (if (zero? 0) 1 2))))))