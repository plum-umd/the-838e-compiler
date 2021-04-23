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

;;Dodger tests
(check-equal? (p-annotate #\a) (Green (Char #\a)))
(check-equal? (p-annotate '(integer->char 1)) (Green (Prim1 'integer->char (Green (Int 1)))))
(check-equal? (p-annotate '(char->integer #\a)) (Green (Prim1 'char->integer (Green (Char #\a)))))
(check-equal? (p-annotate '(char? #\a)) (Green (Prim1 'char? (Green (Char #\a)))))
(check-equal? (p-annotate '(if (char? #\a) (integer->char 1) (char->integer #\b)))
              (Green (If (Green (Prim1 'char? (Green (Char #\a)))) (Green (Prim1 'integer->char (Green (Int 1)))) (Green (Prim1 'char->integer (Green (Char #\b)))))))

;;Evildoer tests
(check-equal? (p-annotate '(read-byte)) (Red (Prim0 'read-byte)))
(check-equal? (p-annotate '(peek-byte)) (Red (Prim0 'peek-byte)))
(check-equal? (p-annotate '(if (zero? (read-byte)) #t #f))
              (Red (If (Red (Prim1 'zero? (Red (Prim0 'read-byte)))) (Green (Bool #t)) (Green (Bool #f)))))
(check-equal? (p-annotate '(void)) (Green (Prim0 'void)))
(check-equal? (p-annotate 'eof) (Green (Eof)))
(check-equal? (p-annotate '(write-byte 1)) (Red (Prim1 'write-byte (Green (Int 1)))))
(check-equal? (p-annotate '(eof-object? 1)) (Green (Prim1 'eof-object? (Green (Int 1)))))
(check-equal? (p-annotate '(begin 1 2)) (Green (Begin2 (Green (Int 1)) (Green (Int 2)))))
(check-equal? (p-annotate '(begin (read-byte) 1)) (Red (Begin2 (Red (Prim0 'read-byte)) (Green (Int 1)))))
(check-equal? (p-annotate '(begin 1 (peek-byte))) (Red (Begin2 (Green (Int 1)) (Red (Prim0 'peek-byte)))))
(check-equal? (p-annotate '(begin (read-byte) (peek-byte))) (Red (Begin2 (Red (Prim0 'read-byte)) (Red (Prim0 'peek-byte)))))
(check-equal? (p-annotate '(add1 (read-byte))) (Red (Prim1 'add1 (Red (Prim0 'read-byte)))))

