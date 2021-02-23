#lang racket
(provide parse parse-e parse-library)
(require "ast.rkt")

;; S-Expr -> Library
(define (parse-library s)
  (match s
    [(list (list 'provide xs ...) (and ds (list 'define _ _)) ...)
     (Lib xs (map parse-d ds))]))

;; S-Expr -> Prog
(define (parse s)
  (match s
    [(list 'begin (list 'provide pvs ...) (list 'require rqs ...)
           (and ds (list 'define _ _)) ...)
     (Mod pvs rqs (map parse-d ds) (parse-e '(void)))]
    [(list 'begin (list 'require rqs ...)
           (and ds (list 'define _ _)) ...)
     (Mod '() rqs (map parse-d ds) (parse-e '(void)))]
    [(list 'begin (list 'provide pvs ...) (list 'require rqs ...)
           (and ds (list 'define _ _)) ... e)
     (Mod pvs rqs (map parse-d ds) (parse-e e))]
    [(list 'begin (list 'require rqs ...)
           (and ds (list 'define _ _)) ... e)
     (Mod '() rqs (map parse-d ds) (parse-e e))]
    [(list 'begin (and ds (list 'define _ _)) ... e)
     (Prog (map parse-d ds) (parse-e e))]
    [e (Prog '() (parse-e e))]))

;; S-Expr -> Defn
(define (parse-d s)
  (match s
    [(list 'define (list (? symbol? f) (? symbol? xs) ...) e)
     (Defn f xs (parse-e e))] 
    [(list 'define (list-rest (? symbol? f) (? symbol? xs) ... (? symbol? xs*)) e) 
     (Defn* f xs xs* (parse-e e))]
    [_ (error "Parse defn error" s)]))

;; S-Expr -> Expr
(define (parse-e s)
  (match s
    [(? integer?)                  (Int s)]
    [(? boolean?)                  (Bool s)]
    [(? char?)                     (Char s)]
    [(? flonum?)                   (Flonum s)]
    [(? string?)                   (String s)] 
    [(? string?)                   (String s)]
                                                     ;;in order to properly parse the args
    [(? vector?)                   (Vec  (parse-vec-lit (vector->list s)))]
    ['eof                          (Eof)]
    [(? symbol?)                   (Var s)]
    [(list 'quote (list))          (Empty)]
    [(list (? (op? op0) p0))       (Prim0 p0)]
    [(list (? (op? op1) p1) e)     (Prim1 p1 (parse-e e))]
    [(list (? (op? op2) p2) e1 e2) (Prim2 p2 (parse-e e1) (parse-e e2))]
    [(list (? (op? op3) p3) e1 e2 e3) (Prim3 p3 (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'begin e1 e2)
     (Begin (parse-e e1) (parse-e e2))]
    [(list 'cond) (Prim0 'void)]
    [(list 'cond (list 'else e)) (parse-e e)]
    [(list 'cond (list e1 e2) c ...)
     (If (parse-e e1) (parse-e e2) (parse-e (cons 'cond c)))]
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'let bs e)
     (let ((x+es (map parse-binding bs)))
       (Let (map first x+es) (map second x+es) (parse-e e)))]
      ; NOTE: We currently assume that there are no duplicate identifiers in bindings for a let
    [(cons 'quote (list (? symbol? x))) (Symbol x)]
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
    [(list 'quote (? symbol? s))
     (Sym s)]
    [(list 'cons (? symbol? x1) (? symbol? x2))
     (Cons x1 x2)]
    [(list 'box (? symbol? x1))
     (Box x1)]
    [_ (error "bad match pattern" s)]))

(define (parse-binding b)
  (match b
    [(list (? symbol? v) e) (list v (parse-e e))]))

(define (parse-vec-lit-aux s)
  (match s
    [(? integer?) (Int s)]
    [(? boolean?) (Bool s)]
    [(? char?)    (Char s)]
    [(? flonum?)  (Flonum s)]
    [(? string?)  (String s)]
    [(? symbol?)  (Symbol s)]
    [_ (error "unsupported vector literal")]))

(define (parse-vec-lit ds)
  (map parse-vec-lit-aux ds))

(define op0
  '(read-byte peek-byte read-char peek-char void gensym))
(define op1
  '(add1 sub1 zero? char? write-byte write-char eof-object?
         integer->char char->integer box unbox empty? car cdr
         integer-length integer? 
         char-alphabetic? char-whitespace? char-upcase char-downcase char-titlecase
         string-length string? integer?
         flonum?
         symbol->string string->symbol symbol?
         vector? vector-length))
(define op2
  '(+ - eq? cons string-ref make-string <=
      make-vector vector-ref
      fl+ fl- fl<= fl=))

(define op3
  '(string-set!  vector-set!))  
(define op4
  '(vector-cas!))  


(define (op? ops)
  (λ (x)
    (and (symbol? x)
         (memq x ops))))
