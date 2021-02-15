#lang racket
(provide parse parse-e)
(require "ast.rkt")

;; S-Expr -> Prog
(define (parse s)
  (match s
    [(list 'begin (and ds (list 'define _ _)) ... e)
     (Prog (map parse-d ds) (parse-e e))]
    [(list 'begin (list 'provide xs ...) (and ds (list 'define _ _)) ...)
     (Lib xs (map parse-d ds))]
    [e (Prog '() (parse-e e))]))

;; S-Expr -> Defn
(define (parse-d s)
  (match s
    [(list 'define (list (? symbol? f) (? symbol? xs) ...) e)
     (Defn f xs (parse-e e))]
    [_ (error "Parse defn error" s)]))

;; S-Expr -> Expr
(define (parse-e s)
  (match s
    [(? integer?)                  (Int s)]
    [(? boolean?)                  (Bool s)]
    [(? char?)                     (Char s)]
    [(? string?)                   (String s)] 
    ['eof                          (Eof)]
    [(? symbol?)                   (Var s)]
    [(list 'quote (list))          (Empty)]
    [(list (? (op? op0) p0))       (Prim0 p0)]
    [(list (? (op? op1) p1) e)     (Prim1 p1 (parse-e e))]
    [(list (? (op? op2) p2) e1 e2) (Prim2 p2 (parse-e e1) (parse-e e2))]
    [(list (? (op? op3) p3) e1 e2 e3) (Prim3 p3 (parse-e e1) (parse-e e2) (parse-e e3))] 
    [(list 'begin e1 e2)
     (Begin (parse-e e1) (parse-e e2))]
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse-e e1) (parse-e e2))]
    [(list 'match e0 cs ...)
     (Match (parse-e e0) (map parse-c cs))]
    [(cons (? symbol? f) es)
     (App f (map parse-e es))]
    [_ (error "Parse error" s)]))

(define (parse-c s)
  (match s
    [(list p e) (Clause (parse-p p) (parse-e e))]
    [_ (error "bad match clause")]))

(define (parse-p s)
  (match s
    ['_           (Wild)]
    [(? symbol?)  (Var s)]
    [(? boolean?) (Lit s)]
    [(? integer?) (Lit s)]
    [(? char?)    (Lit s)]
    [(list 'quote (list))
     (Lit '())]
    [(list 'cons (? symbol? x1) (? symbol? x2))
     (Cons x1 x2)]
    [(list 'box (? symbol? x1))
     (Box x1)]
    [_ (error "bad match pattern" s)]))

(define op0
  '(read-byte peek-byte void))
(define op1
  '(add1 sub1 zero? char? write-byte eof-object?
         integer->char char->integer box unbox empty? car cdr
         integer-length
         char-alphabetic? char-whitespace? char-upcase char-downcase char-titlecase
         string-length string?))   
(define op2
  '(+ - eq? cons string-ref make-string))  
(define op3
  '(string-set!))  

(define (op? ops)
  (λ (x)
    (and (symbol? x)
         (memq x ops))))
