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
;;Dodger tests
(check-parse-unparse '#\a)
(check-parse-unparse '(char? #\a))
(check-parse-unparse '(char->integer #\a))
(check-parse-unparse '(integer->char 955))
;;Evildoer tests
(check-parse-unparse '(read-byte))
(check-parse-unparse '(peek-byte))
(check-parse-unparse '(write-byte 1))
(check-parse-unparse '(eof-object? eof))
(check-parse-unparse '(void))
(check-parse-unparse '(begin (if (zero? (read-byte)) 1 (write-byte 1)) eof))
;; Fraud tests
(check-parse-unparse '(+ 2 9))
(check-parse-unparse '(+ (add1 8) (read-byte)))
(check-parse-unparse '(- (+ 2 (let ((x 5)) x)) (read-byte)))
(check-parse-unparse '(let ((x 6)) x))
(check-parse-unparse '(let ((x (+ 5 3))) (- (add1 x) (sub1 6))))