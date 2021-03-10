#lang racket
(provide parse parse-e parse-library parse-d)
(require "ast.rkt")

;; S-Expr -> Library
(define (parse-library s)
  (match s
    [(list (list 'provide xs ...) (and ds (list 'define _ _)) ...)
     (Lib xs (map parse-d ds))]))

;; S-Expr -> Prog
(define (parse s)
  (match s
    [(list 'begin (and ds (or (list-rest 'define _ _) (list-rest 'struct _ _ )) ) ... e)
     (Prog (map parse-d ds) (parse-e e))]
    [e (Prog '() (parse-e e))]))

;; S-Expr -> Defn
(define (parse-d s)
  (match s
    [(list-rest 'define (list (? symbol? f) (? symbol? xs) ...) e es)
     (Defn f xs (parse-seq e es))] 
    [(list-rest 'define (list-rest (? symbol? f) (? symbol? xs) ... (? symbol? xs*)) e es) 
     (Defn* f xs xs* (parse-seq e es))]
    [(list-rest 'struct (? symbol? s) (list (? symbol? xs) ...) #:prefab es)
     (Struct s xs)]

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
    [(list 'apply f e)             (Apply f (parse-e e))]
    [(list (? (op? op0) p0))       (Prim0 p0)]
    [(list (? (op? op1) p1) e)     (Prim1 p1 (parse-e e))]
    [(list (? (op? op2) p2) e1 e2) (Prim2 p2 (parse-e e1) (parse-e e2))]
    [(list 'make-prefab-struct (? prefab-key? k) rest) (Mps (parse-prefab-key k) (map parse-e rest))] ;;Takes the fields as a list
    [(list 'make-prefab-struct (? prefab-key? k) rest ...) (Mps (parse-prefab-key k) (map parse-e rest))] ;;Takes the fields individually
    [(list (? (op? op3) p3) e1 e2 e3) (Prim3 p3 (parse-e e1) (parse-e e2) (parse-e e3))] 
    [(list 'begin)                 (Prim0 'void)]
    [(list-rest 'begin e es) (parse-seq e es)]
    [(list 'cond) (Prim0 'void)]
    [(list 'cond (list-rest 'else e es)) (parse-seq e es)]
    [(list 'cond (list-rest e1 e2 es) c ...)
     (If (parse-e e1) (parse-seq e2 es) (parse-e (cons 'cond c)))]
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list-rest 'let bs e es)
     (let ((x+es (map parse-binding bs)))
       (Let (map first x+es) (map second x+es) (parse-seq e es)))]
      ; NOTE: We currently assume that there are no duplicate identifiers in bindings for a let
    [(cons 'quote (list (? symbol? x))) (Symbol x)]
    [(list 'match e0 cs ...)
     (Match (parse-e e0) (map parse-c cs))]
    [(cons (? symbol? f) es)
     (App f (map parse-e es))]
    ['#:prefab '#:prefab]
    [_ (error "Parse error" s)]))

(define (parse-seq e es)
  (match es
    ['() (parse-e e)]
    [(cons e0 es)
     (Begin (parse-e e)
            (parse-seq e0 es))]))

(define (parse-c s)
  (match s
    [(list-rest p e es) (Clause (parse-p p) (parse-seq e es))]
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
    [(list 'struct (? symbol? s) (? symbol? xs))
     (Strct s xs)]
    [(list 'box (? symbol? x1))
     (Box x1)]
    [_ (error "bad match pattern" s)]))

(define (parse-binding b)
  (match b
    [(list (? symbol? v) e) (list v (parse-e e))]))

(define (parse-prefab-key k)
  (match k
    [(cons 'quote (list (? symbol? s)))
     (let ((s (parse-e (cons 'quote (list s))))
               (n1 (parse-e 0))
               (auto (list (parse-e 0) (parse-e #f)))
               (mut '()))
           (Prefab-Key s n1 auto mut))]
    [(list (cons 'quote (list (? symbol? s))) (? exact-nonnegative-integer? n1) 
           (list (? exact-nonnegative-integer? n2) v2)
           (? list? l))
     (let ((s (parse-e (cons 'quote (list s))))
           (n1 (parse-e n1))
           (auto (list (parse-e n2) (parse-e v2)))
           (mut (map parse-e l)))
       (Prefab-Key s n1 auto mut))]
    [_ (error "invalid prefab key")]))

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
         vector? vector-length
         string-length string?
         close-input-port open-input-file port? read-byte peek-byte))

(define op2
  '(+ - quotient remainder eq? cons string-ref make-string 
      > < <= >=
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
(define (prefab-key? k)
  (match k
    [(cons 'quote (list (? symbol? s))) #t]
    [(list (cons 'quote (list (? symbol? s))) (? exact-nonnegative-integer? n1) ...
           (list (? exact-nonnegative-integer? n2) v2) ...
           (? list? l)...) #t] ;;Only allowed until we implement vectors, list? should change to vector?
    [_ #f]))
