#lang racket
(require "ast.rkt")

;;Env = IEnv | PEnv
;;ENVValue = Prog | Value

;;IEnv      = (Listof (Pairof Symbol Prog))
;;PEnv      = (Listof (Pairof Symbol Value))

;;Value = int


;;Prog Prog -> S-Expression
;;Given the AST for an interpreter and the AST for a program, this function
;;returns racket code that does exactly what the interpreter would do when run
;;on the program. This function takes the interpreter in as one function. We can extend this
;;to take in the starting point of interpreting a program and a list of other relevant interpretation
;;functions.
(define (eval interp prog)
  (match interp
    [(Defn f xs body)
     `(define ,f 
        ,(eval-p body (car xs) (list (cons (car xs) prog)) (list)))])) 

;;Evaluate an expression in the program the way it would be done by the interpreter
;;Match Symbol IEnv PEnv -> S-Expression
(define (eval-p interp-body sym interp-env prog-env)
  (match interp-body
    [(Match (Var p) cls)
     (let* ((expr (lookup p interp-env)) (env-clause (find-clause-interp-body cls expr interp-env)))
       ;;Evaluate the body of the match clause in the updated environment
       (eval-i (cdr env-clause) (car env-clause) prog-env interp-body sym))]))

;;Expr IEnv PEnv Match Symbol-> Value
;;Evaluate an expression associated with the interpreter
(define (eval-i e interp-env prog-env interp-body sym)
  (match e
    [(Var v) (lookup v interp-env)] ;;This (Var v) is not from the program. This is the interpreter attempting to return the value of a variable v
    [(App 'interp-env (list p r ds)) (eval-p interp-body sym (extend sym (lookup p interp-env) interp-env) prog-env)]
    [(If expr true false)
     (if (eval-i expr interp-env prog-env interp-body sym)
         (eval-i true interp-env prog-env interp-body sym)
         (eval-i false interp-env prog-env interp-body sym))]
    
    [(Match expr cls)                  ;;This is the interpreter attempting to pattern match an expression expr.
     (let* ((expr (eval-i expr interp-env prog-env interp-body sym))
            (env-clause (find-clause-i cls expr interp-env)))
       ;;Evaluate the body of the match clause in the updated environment
       (eval-i (cdr env-clause) (car env-clause) prog-env interp-body sym))]))
  
             

;;Finds the first clause in a list of clauses that matches a given expression from the program.
;;It returns a pair of the extended interpreter environment and the expression of the matched clause.
;;This function only deals with clauses that can show up in the match case of the body
;;of the interpreter. When the body of the interpreter is parsed, patterns p that looked attempted to match
;;the AST form of the program are parsed as (Pat p), while those used to match interpreter values are parsed
;;similar to the patterns found within the match of a program.

;;(Listof Clause) Expr IEnv -> Clause
(define (find-clause-interp-body clauses expr interp-env)
  (match clauses
    ['() (error "program contains an invalid expression")]
    [(cons (Clause p b) clauses)
     (match p
       [(Pat (Int (? symbol? s)))
        (match expr
          [(Int i) (cons (extend s i  interp-env) b)]
          [_ (find-clause-interp-body clauses expr interp-env)])]
       [(Pat (If (? symbol? se1) (? symbol? se2) (? symbol? se3)))
        (match expr
          [(If e1 e2 e3) (cons (extend se1 e1 (extend se2 e2 (extend se3 e3 interp-env))) b)]
          [_ (find-clause-interp-body clauses expr interp-env)])]
       [(Pat (Bool (? symbol? sb)))
        (match expr
          [(Bool bool) (cons (extend sb bool interp-env) b)]
          [_ (find-clause-interp-body clauses expr interp-env)])])]))

;;(Listof Clause) Value IEnv -> Clause
;;Finds the first clause in a list of clauses that matches a given expression from the interpreter.
;;It returns a pair of the updated interp-env caused by variable bindings in the clause and the expression
;;associated with the clause.
(define (find-clause-i clauses value interp-env)
  (match clauses
    ['() (error "program contains an invalid expression")]
    [(cons (Clause p b) clauses)
     (match p
       [(Int s)
        (if (eq? s value)
            (cons interp-env b)
            (find-clause-i clauses value interp-env))]    
       [(Var v) ;;A single varible v in a pattern matches any expression
        (cons (extend v value interp-env) b)])]))

;;Symbol Value (Listof (Pairof Symbol Value)) -> (Listof (Pairof Symbol Value))
;;Add a new binding to the environment
(define (extend s v env)
  (cons (cons s v) env))

;;Symbol Env -> ENVValue 
(define (lookup s env)
  (begin 
    (displayln env)
    (match env
      ['() (error (string-append "Variable " (symbol->string s) " not found"))]
      [(cons pair env)
       (if (eq? (car pair) s) (cdr pair) (lookup s env))])))
       