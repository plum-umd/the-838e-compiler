#lang racket
(provide eval)
(require "ast.rkt" "annotate.rkt")

;;Env = IEnv | PEnv
;;ENVValue = Annotation | Value

;;IEnv      = (Listof (Pairof Symbol ENVValue))
;;PEnv      = (Listof (Pairof Symbol Value))

;;Value = int | bool


(define debug? #f)

;;Given the name of the starting interpreter function, a list of ASTs for interpreter functions and an annotated program, this function
;;returns racket code that does exactly what the interpreter would do when run
;;on the original program.
;;Symbol (Listof Defn) Annotation -> Expr
(define (eval main interp-fns prog)
  (begin
    (debug "eval" prog (list))
    (eval-interp main interp-fns (list prog) (list) (list))))

  
;;Evaluate a program using the interpreter function specified by f
;;Symbol (Listof Defn) (Listof any) IEnv PEnv -> Expr
(define (eval-interp f interp-fns args interp-env prog-env)
  (let* ((interpreter (lookup-interpreter f interp-fns))
         (f (Defn-f interpreter))
         (xs (Defn-xs interpreter))
         (body (Defn-e interpreter)))
    (debug "eval-interp" "unknown" interp-env)
    (if (equal? (length args) (length xs))
        ;;If number of arguments match, run the body of the appropriate interpreter function with parameters bound to arguments
        (eval-i body (extend-multiple xs args interp-env) prog-env interp-fns)
        (error (string-append (symbol->string f) " expected " (number->string (length xs)) " arguments but was given " (number->string (length args)) " arguments")))))

;;Evaluate the body of the definition that matches the provided function name, passing
;;it the already evaluated arguments
;;Symbol (Listof Defns) (Listof Expr) IEnv PEnv -> Expr       
(define (eval-interp-fn f interp-fns args interp-env prog-env)
  (match (lookup-interpreter f interp-fns)
    [(Defn f pars body)
     (let ((l-args (length args))
           (l-pars (length pars)))
     (if (equal? l-args l-pars)
         (let ((interp-env (extend-multiple pars args (list)))) ;;Function should only see values that are in scope for it
           (eval-i body interp-env prog-env interp-fns))
         (error (string-append "Error while calling function " (symbol->string f)
                               ": Given " (number->string l-args)
                               " arguments but expecting " (number->string l-pars)
                               " arguments"))))]))
                               
     
  
;;Evaluate a program using interp-env
;;Defn IEnv PEnv (Listof Defn) -> Expr
(define (eval-interp-env interpreter interp-env prog-env interp-fns)
  (begin
    (debug "eval-interp-env" "unknown" interp-env)
    (match (Defn-e interpreter)
      [(Match (Var p) cls)
       (let* ((expr (lookup p interp-env))
              (env-clause (find-clause-interp-env cls expr interp-env))
              (new-interp-env (car env-clause))
              (todo (cdr env-clause)))
         (debug "eval-interp-env" expr interp-env)
         ;;Evaluate the body of the match clause in the updated environment
         (eval-i todo new-interp-env prog-env interp-fns))])))

;;Evaluate a call to interp-prim1
;;Defn IEnv PEnv (Listof Defn) -> Expr
(define (eval-interp-prim1 interpreter interp-env prog-env interp-fns)
  (begin
    (debug "eval-interp-prim1" "unknown" interp-env)
    (match (Defn-e interpreter)
      [(Match (Var p) cls)
       (let* ((prim (lookup p interp-env))
              (env-clause (find-clause-interp-prim1 cls prim interp-env)))
         (eval-i (cdr env-clause) (car env-clause) prog-env interp-fns))])))

;;Evaluate a call to interp-prim0
;;Defn IEnv PEnv (Listof Defn) -> Expr
(define (eval-interp-prim0 interpreter interp-env prog-env interp-fns)
  (begin
    (debug "eval-interp-prim0" "unknown" interp-env)
    (match (Defn-e interpreter)
      [(Match (Var p) cls)
       (let* ((prim (lookup p interp-env))
               (env-clause (find-clause-interp-prim0 cls prim interp-env)))
         (eval-i (cdr env-clause) (car env-clause) prog-env interp-fns))])))

;;Evaluate an expression associated with the interpreter
;;Expr IEnv PEnv (Listof Defn) -> Expr
(define (eval-i e interp-env prog-env interp-fns)
  (begin
    (debug "eval-i" e interp-env)
    (match e
      [(Int i) e]
      [(Bool b) e]
      [(Char c) e]
      [(Eof) e]
      [(Err) e]
      [(Var v) ;;This (Var v) is not from the program. This is the interpreter attempting to return the value of a variable v
       (let ((value (lookup v interp-env)))
         (match value
           [(? integer? i) (Int i)]
           [(? boolean? b) (Bool b)]
           [(? char? c) (Char c)]
           [(? ast-expr? v) v]
           [(? symbol? s) (Symbol s)]
           [(? annotation? a) a]))] 
      [(If expr true false)
       (let ((v (eval-i expr interp-env prog-env interp-fns)))
         (match v
           [(Bool b)
            (if (equal? b #f)
                (eval-i false interp-env prog-env interp-fns)
                (eval-i true interp-env prog-env interp-fns))]
           [_
            (eval-i true interp-env prog-env interp-fns)]))]   
      [(Match expr cls)                  ;;This is the interpreter attempting to pattern match an expression expr.
       (let* ((expr (eval-i expr interp-env prog-env interp-fns))
              (env-clause (find-clause-i cls expr interp-env)))
         ;;Evaluate the body of the match clause in the updated environment
         (eval-i (cdr env-clause) (car env-clause) prog-env interp-fns))]
      [(Begin2 e1 e2)
       (let ((v1 (eval-i e1 interp-env prog-env interp-fns))
             (v2 (eval-i e2 interp-env prog-env interp-fns)))
         v2)]
      [(Prim0 p )
       (eval-i-prim0 p interp-env prog-env interp-fns)]
      [(Prim1 p e)
       (eval-i-prim1 p e interp-env prog-env interp-fns)]
      [(App f es)
       (eval-i-app f es interp-env prog-env interp-fns)])))

;;Symbol IEnv PEnv (Listof Defn) -> Expr
(define (eval-i-prim0 p interp-env prog-env interp-fns)
  (match p
    ['void
     (Void)]))

;;Symbol Expr IEnv PEnv (Listof Defn) -> Expr
(define (eval-i-prim1 p e interp-env prog-env interp-fns)
  (match p
    ['add1 (Int (add1 (Int-i (eval-i e interp-env prog-env interp-fns))))]
    ['sub1 (Int (sub1 (Int-i (eval-i e interp-env prog-env interp-fns))))]
    ['zero? (Bool (zero? (Int-i (eval-i e interp-env prog-env interp-fns))))]
    ['char?
     (match (eval-i e interp-env prog-env interp-fns)
       [(Char c) (Bool #t)]
       [_        (Bool #f)])]
    ['integer->char
     (Char (integer->char (Int-i (eval-i e interp-env prog-env interp-fns))))]
    ['char->integer
     (Int (char->integer (Char-c (eval-i e interp-env prog-env interp-fns))))]
    ['eof-object?
     (Bool (Eof? (eval-i e interp-env prog-env interp-fns)))]))

;;Symbol (Listof Expr) IEnv PEnv (Listof Defn) -> Expr
(define (eval-i-app f es interp-env prog-env interp-fns)
  (if (interp-fn? f) ;;Dispatch to the approptriate interpreter function stored in interp-fns
      (eval-interp-fn f interp-fns (eval-i-es es interp-env prog-env interp-fns)
                   interp-env prog-env)
      (match f
        ['integer?
         (if (equal? (length es) 1)
             (match (eval-i (car es) interp-env prog-env interp-fns)
               [(Int i) (Bool #t)]
               [_ (Bool #f)])
             (error (string-append "integer? expected 1 argument but received " (number->string (length es)))))]
        ['byte?
         (if (equal? (length es) 1)    
             (match (eval-i (car es) interp-env prog-env interp-fns)
               [(Int i) (Bool (byte? i))]
               [(Bool b) (Bool (byte? b))]
               [(Char c) (Bool (byte? c))]
               [(Eof) (Bool (byte? eof))])
             (error (string-append "byte? expected 1 argument but received " (number->string (length es)))))]
        [_ (error (string-append "partial evaluator does not support function "
                                 (symbol->string f)))])))

;;(Listof Expr) IEnv PEnv (Listof Defn) -> (Listof Expr)
(define (eval-i-es es interp-env prog-env interp-fns)
  (map (λ (e) (eval-i e interp-env prog-env interp-fns)) es))
             

;;Finds the first clause in a list of clauses that matches a given expression from the program.
;;It returns a pair of the extended interpreter environment and the expression of the matched clause.
;;This function only deals with clauses that can show up in the match case of the body
;;of the interpreter. When the body of the interpreter is parsed, patterns p that looked attempted to match
;;the AST form of the program are parsed as (Pat p), while those used to match interpreter values are parsed
;;similar to the patterns found within the match of a program.

;;(Listof Clause) Expr IEnv -> Clause
(define (find-clause-interp-env clauses expr interp-env)
  (begin
    (debug "find-clause-interp-env" expr interp-env)
    (match clauses
      ['() (error "program contains an invalid expression")]
      [(cons (Clause p b) clauses)
       (match p
         [(Pat (Int (? symbol? s)))
          (match expr
            [(Int i) (cons (extend s i  interp-env) b)]
            [_ (find-clause-interp-env clauses expr interp-env)])]
         [(Pat (If (? symbol? se1) (? symbol? se2) (? symbol? se3)))
          (match expr
            [(If e1 e2 e3) (cons (extend se1 e1 (extend se2 e2 (extend se3 e3 interp-env))) b)]
            [_ (find-clause-interp-env clauses expr interp-env)])]
         [(Pat (Bool (? symbol? sb)))
          (match expr
            [(Bool bool) (cons (extend sb bool interp-env) b)]
            [_ (find-clause-interp-env clauses expr interp-env)])]
         [(Pat (Char (? symbol? sb)))
          (match expr
            [(Char char) (cons (extend sb char interp-env) b)]
            [_ (find-clause-interp-env clauses expr interp-env)])]
         [(Pat (Eof))
          (match expr
            [(Eof) (cons interp-env b)]
            [_ (find-clause-interp-env clauses expr interp-env)])]
         [(Pat (Prim0 (? symbol? s)))
          (match expr
            [(Prim0 p)
             (cons (extend s p interp-env) b)]
            [_ (find-clause-interp-env clauses expr interp-env)])]
         [(Pat (Prim1 (? symbol? p) (? symbol? e)))
          (match expr
            [(Prim1 pr expr) (cons (extend p pr (extend e expr interp-env)) b)]
            [_ (find-clause-interp-env clauses expr interp-env)])]
         [(Pat (Begin2 (? symbol? s1) (? symbol? s2)))
          (match expr
            [(Begin2 e1 e2) (cons (extend s1 e1 (extend s2 e2 interp-env)) b)]
            [_ (find-clause-interp-env clauses expr interp-env)])])])))

;;Find the clause matching the provided prim
;;(Listof Clause) Symbol IEnv -> Clause
(define (find-clause-interp-prim1 cls prim interp-env)
  (match cls
    ['() (error (string-append "program contains an invalid expression: No clause matching prim " (symbol->string prim)))]
    [(cons (Clause p b) cls)
     (match p
       [(Symbol s)
        (if (equal? s prim)
            (cons interp-env b)
            (find-clause-interp-prim1 cls prim interp-env))])]))

;;Find the clause matching the provided prim
;;(Listof Clause) Symbol IEnv -> Clause
(define (find-clause-interp-prim0 cls prim interp-env)
  (match cls
    ['() (error (string-append "program contains an invalid expression: no clause matching prim " (symbol->string prim)))]
    [(cons (Clause p b) cls)
     (match p
       [(Symbol s)
        (if (equal? s prim)
            (cons interp-env b)
            (find-clause-interp-prim0 cls prim interp-env))])]))

;;Finds the first clause in a list of clauses that matches a given expression from the interpreter.
;;It returns a pair of the updated interp-env caused by variable bindings in the clause and the expression
;;associated with the clause.
;;(Listof Clause) Value IEnv -> Clause
(define (find-clause-i clauses value interp-env)
  (match clauses
    ['() (error "program contains an invalid expression")]
    [(cons (Clause p b) clauses)
     (match p
       [(Int s)
        (if (and (Int? value) (equal? s (Int-i value)))
            (cons interp-env b)
            (find-clause-i clauses value interp-env))]
       [(Err)
        (if (Err? value)
            (cons interp-env b)
            (find-clause-i clauses value interp-env))]
       [(Var v) ;;A single varible v in a pattern matches any expression
        (cons (extend v value interp-env) b)]
       [(Wild)
        (cons interp-env b)])]))

;;Symbol Value (Listof (Pairof Symbol Value)) -> (Listof (Pairof Symbol Value))
;;Add a new binding to the environment
(define (extend s v env)
  (cons (cons s v) env))

;;Add multiple bindings to the environment at the same time
;;(Listof Symbol) (Listof Expr) Env -> Env
(define (extend-multiple syms vs env)
  (append (map cons syms vs) env))

;;Symbol Env -> ENVValue 
(define (lookup s env)
    (match env
      ['() (error (string-append "Variable " (symbol->string s) " not found"))]
      [(cons pair env)
       (if (equal? (car pair) s) (cdr pair) (lookup s env))]))

(define (interp-fn? f interp-fns)
  (match interp-fns
    ['() #f]
    [(cons fn interp-fns)
     (if (equal? f fn)
         #t
         (interp-fn? f interp-fns))]))

;;Given a symbol and a list of interpreter functions, return the first function with the same name as the symbol
;;Symbol (Listof Defn) -> Defn
(define (lookup-interpreter f lst)
  (match lst
    ['() (error (string-append (symbol->string f) " is not an interpreter function"))]
    [(cons (Defn fun xs e) lst)
     (if (equal? fun f)
         (Defn fun xs e)
         (lookup-interpreter f lst))]))

;;Print the string s if the debug? global variable is true
(define (debug fn-name prog interp-env)
  (if debug?
      (begin
        (display (string-append "In function: " fn-name))
        (display " with prog: ")
        (display prog)
        (display " and interp-env: ")
        (displayln interp-env)
        (displayln ""))
      (void)))


       