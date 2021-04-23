#lang racket
(provide parse parse-e desugar desugar-def parse-library desugar-def-lib)
(require "ast.rkt" "types.rkt")

;; S-Expr -> (Letrec (Lisof Id) (Listof Lambda) Expr)
(define (parse s)
  (desugar (parse-aux s)))

;; S-Expr -> Prog
(define (parse-aux s)
  (match s
    [(list 'begin (list 'provide (list 'contract-out cpvs ...)) (and ds (list 'define _ _)) ...)
     (parse-mod-contract cpvs '() ds '(void))]
    [(list 'begin (list 'provide pvs ...) (list 'require rqs ...)
           (and ds (list 'define _ _)) ...)
     (parse-mod pvs rqs ds '(void))]
    [(list 'begin (list 'require rqs ...) (and ds (list 'define _ _)) ...)
     (parse-mod '() rqs ds '(void))]
    [(list 'begin (list 'provide pvs ...) (and ds (list 'define _ _)) ...)
     (parse-mod pvs '() ds '(void))]
    [(list 'begin (list 'provide pvs ...) (list 'require rqs ...)
           (and ds (list 'define _ _)) ... e)
     (parse-mod pvs rqs ds e)]
    [(list 'begin (list 'require rqs ...) (and ds (list 'define _ _)) ... e)
     (parse-mod '() rqs ds e)]
    [(list 'begin (list 'provide pvs ...) (and ds (list 'define _ _)) ... e)
     (parse-mod pvs '() ds e)]
    [(list 'begin (and ds (list-rest 'define _ _)) ... e)
     (Prog (map parse-d ds) (parse-e e))]
    [e (Prog '() (parse-e e))]))

;;; S-Expr -> Library
(define (parse-library s)
  (match s
    [(list (list 'provide xs ...) (and ds (list 'define _ _)) ...)
     (Lib xs (map parse-d ds))]))

(define (parse-mod-contract cpvs rqs ds e)
  (Mod/contract
    (map parse-contract cpvs)
    '()
    (map parse-d ds) (parse-e e)))

(define (parse-mod pvs rqs ds e)
  (let ((pvs2 (if (equal? pvs '((all-defined-out)))
                  (parse-all-defined-out ds)
                   pvs)))
    (Mod pvs2 rqs (map parse-d ds) (parse-e e))))
                 
(define (parse-contract cvps)
   (match cvps
     [(list id e) (Contract id (parse-e e))]))

(define (parse-all-defined-out ds)
   (match ds
     [(list (list 'define fs _) ...) (map car fs)]
     [_ (error "parse-all-defined-out")]))
  

;; S-Expr -> Defn
(define (parse-d s)
  (match s
    [(list-rest 'define (list (? symbol? f) (? symbol? xs) ...) e es)
     (Defn f xs (parse-seq e es))]
    [(list-rest 'define (list-rest (? symbol? f) (? symbol? xs) ... (? symbol? xs*)) e es)
     (Defn* f xs xs* (parse-seq e es))]
    [_ (error "Parse defn error" s)]))

;; S-Expr -> Expr
(define (parse-e s)
  (match s
    [(? integer?)                  (if (bignum? s) (Bignum s) (Int s))]
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
    [(list 'apply f e)             (Apply (parse-e f) (parse-e e))]
    [(list (? (op? op0) p0))       (Prim0 p0)]
    [(list (? (op? op1) p1) e)     (Prim1 p1 (parse-e e))]
    [(list (? (op? op2) p2) e1 e2) (Prim2 p2 (parse-e e1) (parse-e e2))]
    [(list (? (op? op3) p3) e1 e2 e3) (Prim3 p3 (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'begin)                 (Prim0 'void)]
    [(list 'begin (and ds (list-rest 'define _ _)) ..1 e es ...)
     (Prog (map parse-d ds) (parse-seq e es))]
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
    [(list-rest 'letrec bs e es)
     (let ((x+es (map parse-binding bs)))
       (Letrec (map first x+es) (map second x+es) (parse-seq e es)))]
    [(list-rest (or 'lambda 'λ) (list xs ...) e es)
     (Lam (gensym) xs (parse-seq e es))]
    [(list-rest (or 'lambda 'λ) (list-rest (? symbol? xs) ... (? symbol? xs*)) e es)         
     (Lam* (gensym) xs xs* (parse-seq e es))]
    [(cons e es)
     (LCall (parse-e e) (map parse-e es))]
    ;[(cons (? symbol? f) es)
     ;(App f (map parse-e es))]
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


(define (desugar-def d)
  (match d
    [(Defn f xs e) (cons f (Lam (gensym) xs (desugar e)))]
    [(Defn* f xs xs* e) (cons f (Lam* (gensym) xs xs* (desugar e)))]))

(define (desugar-def-lib d)
  (match d
    [(Defn f xs e) (cons f (Lam (gensym 'lib_lambda_) xs (desugar e)))]
    [(Defn* f xs xs* e) (cons f (Lam* (gensym 'lib_lambda_) xs xs* (desugar e)))]))

(define (desugar e)
  (match e
    [(Prog ds e)
     (let ((bs (map desugar-def ds)))
       (Letrec (map car bs) (map cdr bs) (desugar e)))]
    [(Int i)            e]
    [(Bool b)           e]
    [(Char c)           e]
    [(Flonum f)         e]
    [(Bignum b)         e]
    [(Eof)              e]
    [(Empty)            e]
    [(String s)         e]
    [(Symbol s)         e]
    [(Vec ds)           e]
    [(Var x)            e]
    [(LCall e es)       (LCall (desugar e) (map desugar es))]
;    [(App f es)         (App f (map desugar es))]
    [(Apply f e)        (Apply (desugar f) (desugar e))]
    [(Prim0 p)          e]
    [(Prim1 p e)        (Prim1 p (desugar e))]
    [(Prim2 p e1 e2)    (Prim2 p (desugar e1) (desugar e2))]
    [(Prim3 p e1 e2 e3) (Prim3 p (desugar e1) (desugar e2) (desugar e3))]
    [(If e1 e2 e3)      (If (desugar e1) (desugar e2) (desugar e3))]
    [(Begin e1 e2)      (Begin (desugar e1) (desugar e2))]
    [(Let x e1 e2)      (Let x (map desugar e1) (desugar e2))]
    [(Letrec xs es e)   (Letrec xs (map desugar es) (desugar e))]
    [(Lam l xs e)       (Lam l xs (desugar e))]
    [(Lam* l xs xs* e)  (Lam* l xs xs* (desugar e))]
    [(Mod pvs rqs ds e) (Mod pvs rqs (map desugar-def ds) (desugar e))]
    [(Mod/contract cpvs rqs ds e) (Mod cpvs rqs (map desugar-def ds) (desugar e))]
    [(Match e0 cs)
     (Match (desugar e0) (map (λ (c) (Clause (Clause-p c) (desugar (Clause-e c)))) cs))]))

