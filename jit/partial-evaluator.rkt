#lang racket
(provide eval)
(require "ast.rkt" "cfg.rkt")

;;Env = IEnv | PEnv
;;ENVValue = Expr | Value

;;IEnv      = (Listof (Pairof Symbol ENVValue))
;;PEnv      = (Listof (Pairof Symbol Value))

;;Value = int | bool

(define debug? #f)

;;Given the name of the starting interpreter function, a list of ASTs for interpreter functions and an annotated program, this function
;;returns racket code that does exactly what the interpreter would do when run
;;on the original program.
;;Symbol (Listof Defn) Annotation -> S-Expression
(define (eval main interp-fns prog)
  (begin
    (debug "eval" prog (list))
    (match prog
      [(Green expr)
       (eval-interp main interp-fns prog (list) (list))]
      [(Red expr)
       0])))
  
;;Evaluate a program using the interpreter function specified by interp-sym
;;Symbol (Listof Defn) Annotation IEnv PEnv -> S-Expression
(define (eval-interp interp-sym interp-fns prog interp-env prog-env)
  (match prog
    [(Green prog)
     (let* ((interpreter (lookup-interpreter interp-sym interp-fns))
            (f (Defn-f interpreter))
            (xs (Defn-xs interpreter)))
       (debug "eval-interp" prog interp-env)
       (match f
         ['interp
          (let ((interp-env (extend (car xs) prog interp-env))) ;;We are using the knowledge that the interp accepts the expression to be interpreted as its first argument
            (eval-interp-env interpreter interp-env prog-env interp-fns))]
         ['interp-prim1
          (match prog
            [(Prim1 prim v)
             (let ((interp-env (extend (car xs) prim (extend (car (cdr xs)) v interp-env)))) ;;We are using the knowledge that interp-prim1 accepts the prim symbol as its
               ;;first argument and the evaluated argument to the prim as the second
               (eval-interp-prim1 interpreter interp-env prog-env interp-fns))])]))]
    [(Red prog)
     prog]))

;;Evaluate a program using interp-env
;;Defn IEnv PEnv (Listof Defn) -> S-Expression
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
;;Defn IEnv PEnv (Listof Defn) -> S-Expression
(define (eval-interp-prim1 interpreter interp-env prog-env interp-fns)
  (begin
    (debug "eval-interp-prim1" "unknown" interp-env)
    (match (Defn-e interpreter)
      [(Match (Var p) cls)
       (let* ((prim (lookup p interp-env))
              (env-clause (find-clause-interp-prim1 cls prim interp-env)))
         (eval-i (cdr env-clause) (car env-clause) prog-env interp-fns))])))

;;Evaluate an expression associated with the interpreter
;;Expr IEnv PEnv (Listof Defn) -> Value
(define (eval-i e interp-env prog-env interp-fns)
  (begin
    (debug "eval-i" e interp-env)
    (match e
      [(Int i) i]
      [(Bool b) b]
      [(Var v) (lookup v interp-env)] ;;This (Var v) is not from the program. This is the interpreter attempting to return the value of a variable v
      [(If expr true false)
       (if (eval-i expr interp-env prog-env interp-fns)
           (eval-i true interp-env prog-env interp-fns)
           (eval-i false interp-env prog-env interp-fns))]
    
      [(Match expr cls)                  ;;This is the interpreter attempting to pattern match an expression expr.
       (let* ((expr (eval-i expr interp-env prog-env interp-fns))
              (env-clause (find-clause-i cls expr interp-env)))
         ;;Evaluate the body of the match clause in the updated environment
         (eval-i (cdr env-clause) (car env-clause) prog-env interp-fns))]
      [(App 'interp (list p))
       (let ((v (eval-i p interp-env prog-env interp-fns)))
         (eval-interp 'interp interp-fns v interp-env prog-env))]
      [(App 'interp-prim1 (list prim e))
       (let* ((prim (eval-i prim interp-env prog-env interp-fns))
              (v (eval-i e interp-env prog-env interp-fns)))
         (eval-interp 'interp-prim1 interp-fns (Green (Prim1 prim v)) interp-env prog-env))]
      [(Prim1 'add1 e)
       (add1 (eval-i e interp-env prog-env interp-fns))]
      [(Prim1 'sub1 e)
       (sub1 (eval-i e interp-env prog-env interp-fns))]
      [(Prim1 'zero? e)
       (zero? (eval-i e interp-env prog-env interp-fns))])))
  
             

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
         [(Pat (Prim1 (? symbol? p) (? symbol? e)))
          (match expr
            [(Prim1 pr expr) (cons (extend p pr (extend e expr interp-env)) b)]
            [_ (find-clause-interp-env clauses expr interp-env)])])])))

;;Find the clause matching the provided prim
;;(Listof Clause) Symbol IEnv -> Clause
(define (find-clause-interp-prim1 cls prim interp-env)
  (match cls
    ['() (error "program contains and invalid expression: No clause matching prim")]
    [(cons (Clause p b) cls)
     (match p
       [(Symbol s)
        (if (equal? s prim)
            (cons interp-env b)
            (find-clause-interp-prim1 cls prim interp-env))])]))
     

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
        (if (equal? s value)
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
    (match env
      ['() (error (string-append "Variable " (symbol->string s) " not found"))]
      [(cons pair env)
       (if (equal? (car pair) s) (cdr pair) (lookup s env))]))

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
        (displayln interp-env))
      (void)))
       