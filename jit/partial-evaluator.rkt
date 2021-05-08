#lang racket

;;The partial evaluator will run the interpreter on the given program. If the interpreter attempts to perform a Red operation (for itself or
;; because that is what the program is doing), the result of the operation will be a Red annotation containing the red operation. It is up the parent
;; expression to handle this.

;; Red means that the contained expression could not be further evaluated
;; Green means the contained expression is an evaluated value. In the case when a green value is the result of the interpreter looking up a variable,
;; it may still be an expression that is considered Red later on.

(provide eval)
(require "ast.rkt")

;;Annotation = Red | Green
(struct Red (e)   #:prefab)
(struct Green (e) #:prefab)

;;Env = IEnv | PEnv
;;ENVValue = Expr | Annotation | Value

;;IEnv      = (Listof (Pairof Symbol ENVValue))
;;PEnv      = (Listof (Pairof Symbol Value))

;;Value = int | bool | eof | char


(define debug? #f)

;;Given the name of the starting interpreter function, a list of ASTs for functions defined by the interpreter and its supporting cast, and an annotated program, this function
;;returns racket code that does exactly what the interpreter would do when run
;;on the original program.
;;symbol (Listof Defn) Annotation -> Expr
(define (eval main interp-fns prog)
  (begin
    (debug "eval" prog (list) (list))
    (match (eval-interp main (list prog) (list) (list) interp-fns)
      [(Green v)
       v]
      [(Red v)
       v])))

  
;;Evaluate a program using the interpreter function specified by f
;;Symbol (Listof any) IEnv PEnv (Listof Defn) -> Annotation
(define (eval-interp f args interp-env prog-env interp-fns)
  (let* ((interpreter (lookup-interpreter f interp-fns))
         (f (Defn-f interpreter))
         (xs (Defn-xs interpreter))
         (body (Defn-e interpreter)))
    (debug "eval-interp" args interp-env prog-env)
    (if (equal? (length args) (length xs))
        ;;If number of arguments match, run the body of the appropriate interpreter function with parameters bound to arguments. The partial evaluator must be able
        ;;to handle any of the racket features used by this function. It is assumed that all the arguments have been evaluated.
        (eval-i body (extend-multiple xs args interp-env) prog-env interp-fns)
        (error (string-append (symbol->string f) " expected " (number->string (length xs)) " arguments but was given " (number->string (length args)) " arguments")))))
                              
;;Evaluate an expression associated with the interpreter
;;Expr IEnv PEnv (Listof Defn) -> Annotatation
(define (eval-i e interp-env prog-env interp-fns)
  (begin
    (debug "eval-i" e interp-env prog-env)
    (match e
      [(Int i) (Green e)]
      [(Bool b) (Green e)]
      [(Char c) (Green e)]
      [(Symbol s) (Green e)]
      [(Eof) (Green e)]
      [(Err) (Green e)]
      [(Empty) (Green e)]
      [(Var v) ;;This (Var v) is not from the program. This is the interpreter attempting to return the value of a variable v
       (let ((value (lookup v interp-env)))
         (match value
           [(? integer? i) (Green (Int i))]
           [(? boolean? b) (Green (Bool b))]
           [(? char? c) (Green (Char c))]
           [(? symbol? s) (Green (Symbol s))]
           [(? ast-expr? v) (Green v)] 
           ))]
      [(And es)
       (match es
         ['() (Green (Bool #t))]
         [_ (eval-and es interp-env prog-env interp-fns)])]
      [(Or es)
       (match es
         ['() (Green (Bool #f))]
         [_ (eval-or es interp-env prog-env interp-fns)])]
      [(If expr true false)
       (let ((v (eval-i expr interp-env prog-env interp-fns)))
         (match v
           [(Green v)
            (match v
              [(Bool b)
               (if (equal? b #f)
                   (eval-i false interp-env prog-env interp-fns)
                   (eval-i true interp-env prog-env interp-fns))]
              [_
               (eval-i true interp-env prog-env interp-fns)])]
           [(Red v)
            (Red (If v true false))]))]   
      [(Match expr cls)                  ;;This is the interpreter attempting to pattern match an expression expr.
       (let ((expr (eval-i expr interp-env prog-env interp-fns)))
         (match expr
           [(Green expr) 
            (let* ((env-clause  (find-clause-i cls expr interp-env prog-env interp-fns))
                   (e           (cdr (cdr env-clause)))
                   (interp-env  (car env-clause))
                   (prog-env    (car (cdr env-clause))))
              ;;Evaluate the body of the match clause in the updated environment
              (eval-i e interp-env prog-env interp-fns))]
           [(Red expr) ;;Cannot further evaluate the match since the expression was not fully evaluated so return it to be evaluated at run time
            ;;after free varibales have been bound to their value in the interpreters environment
            (Red (eval-fvs (Match expr cls) interp-env prog-env interp-fns))]))]
      [(Begin2 e1 e2)
       (let ((v1 (eval-i e1 interp-env prog-env interp-fns))
             (v2 (eval-i e2 interp-env prog-env interp-fns)))
         (match (cons v1 v2)
           [(cons (Red v1) (Red v2))
            (Red (Begin2 v1 v2))]
           [(cons (Green v1) (Green v2))
            (Green v2)]
           [(cons (Green v1) (Red v2))
            (Red v2)]
           [(cons (Red v1) (Green v2))
            (Red (Begin2 v1 v2))]))]
      [(Let x b e)
       (let ((v (eval-i b interp-env prog-env interp-fns))) 
         (match v
           [(Green v1)   (eval-i e (extend x v1 interp-env) prog-env interp-fns)] 
           ;; extend the interp-env because this is a let in interp
           [(Red v1)     (Red (Let x v1 e))]))]
      [(App f es)
       (eval-i-app f es interp-env prog-env interp-fns)]
      [(Prim0 p)
       (eval-i-prim0 p interp-env prog-env interp-fns)]
      [(Prim1 p e)
       (eval-i-prim1 p e interp-env prog-env interp-fns)]
      [(Prim2 p e1 e2)
       (eval-i-prim2 p e1 e2 interp-env prog-env interp-fns)])))

;;Evaluate a list of expressions into value expressions.
;;(Listof Expr) IEnv PEnv (Listof Defn) -> (Listof Expr)
(define (eval-i-es es interp-env prog-env interp-fns)
  (map (λ (e) (eval-i e interp-env prog-env interp-fns)) es))


;;;;;;;;;;;;;;;;;;Conditionals;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(Listof Expr) -> Annotation
(define (eval-and es interp-env prog-env interp-fns)
  (match es
    [(list e) (eval-i e interp-env prog-env interp-fns)]
    [(cons e es)
     (match (eval-i e interp-env prog-env interp-fns)
       [(Green (Bool #f))
        (Green (Bool #f))]
       [(Green _)
        (eval-and es interp-env prog-env interp-fns)]
       [(Red v)
        (Red (And (cons v es)))])]))

;;(Listof Expr) -> Annotation
(define (eval-or es interp-env prog-env interp-fns)
  (match es
    [(list e) (eval-i e interp-env prog-env interp-fns)]
    [(cons e es)
     (match (eval-i e interp-env prog-env interp-fns)
       [(Green (Bool #f))
        (eval-or es interp-env prog-env interp-fns)]
       [(Green v)
        (Green v)]
       [(Red v)
        (Red (Or (cons v es)))])]))
       
    
;;;;;;;;;;;;;;;;;;;;Primitives that accept no arguments;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Symbol IEnv PEnv (Listof Defn) -> Annotation
(define (eval-i-prim0 p interp-env prog-env interp-fns)
  (match p
    ['void
     (Green (Void))]
    ['read-byte
     (Red (Prim0 'read-byte))]
    ['peek-byte
     (Red (Prim0 'peek-byte))]))

;;Symbol Expr IEnv PEnv (Listof Defn) -> Annotation
(define (eval-i-prim1 p e interp-env prog-env interp-fns)
  (match p
    ['add1
     (match (eval-i e interp-env prog-env interp-fns)
       [(Green (Int i))
        (Green (Int (add1 i)))]
       [(Red v)
        (Red (Prim1 'add1 v))])] ;; I believe this should be add1
    ['sub1
     (match (eval-i e interp-env prog-env interp-fns)
       [(Green (Int i))
        (Green (Int (sub1 i)))]
       [(Red v)
        (Red (Prim1 'sub1 v))])]
    ['zero?
     (match (eval-i e interp-env prog-env interp-fns)
       [(Green (Int i))
        (Green (Bool (zero? i)))]
       [(Red v)
        (Red (Prim1 'zero? v))])]
    ['char?
     (match (eval-i e interp-env prog-env interp-fns)
       [(Green (Char c)) (Green (Bool #t))]
       [(Green _) (Green (Bool #f))]
       [(Red v)
        (Red (Prim1 'char? v))])]
    ['integer->char
     (match (eval-i e interp-env prog-env interp-fns)
       [(Green (Int i))
        (Green (Char (integer->char i)))]
       [(Red v)
        (Red (Prim1 'integer->char v))])]
    ['char->integer
     (match (eval-i e interp-env prog-env interp-fns)
       [(Green (Char c))
        (Green (Int (char->integer c)))]
       [(Red v)
        (Red (Prim1 'char->integer v))])]
    ['eof-object?
     (match (eval-i e interp-env prog-env interp-fns)
       [(Green (Eof))
        (Green (Bool #t))]
       [(Green _)
        (Green (Bool #f))]
       [(Red v)
        (Red (Prim1 'eof-object? v))])]
    ['write-byte
     (match (eval-i e interp-env prog-env interp-fns)
       [(Green v)
        (Red (Prim1 'write-byte v))]
       [(Red v)
        (Red (Prim1 'write-byte v))])]
    ['box
     (match (eval-i e interp-env prog-env interp-fns)
       [(Green v)
        (Green (Box v))]
       [(Red v)
        (Red (Prim2 'box v))])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;Primitives that accept two arguments;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Symbol Expr Expr IEnv PEnv (Listof Defn) -> Annotation
(define (eval-i-prim2 p e1 e2 interp-env prog-env interp-fns)
  (match p
    ['+
     (match (eval-i e1 interp-env prog-env interp-fns)
       [(Green (Int i1))
            (match (eval-i e2 interp-env prog-env interp-fns)
              [(Green (Int i2))   (Green (Int (+ i1 i2)))]
              [(Red v2)           (Red (Prim2 '+ (Int i1) v2))])]
       [(Red v1)
            (match (eval-i e2 interp-env prog-env interp-fns)
              [(Green (Int i2))   (Red (Prim2 '+ v1 (Int i2)))]
              [(Red v2)           (Red (Prim2 '+ v1 v2))])])]
    ['-
     (match (eval-i e1 interp-env prog-env interp-fns)
       [(Green (Int i1))
            (match (eval-i e2 interp-env prog-env interp-fns)
              [(Green (Int i2))   (Green (Int (- i1 i2)))]
              [(Red v2)           (Red (Prim2 '- (Int i1) v2))])]
       [(Red v1)
            (match (eval-i e2 interp-env prog-env interp-fns)
              [(Green (Int i2))   (Red (Prim2 '- v1 (Int i2)))]
              [(Red v2)           (Red (Prim2 '- v1 v2))])])]
    ['cons
     (match (cons (eval-i e1 interp-env prog-env interp-fns) (eval-i e2 interp-env prog-env interp-fns))
       [(cons (Green v1) (Green v2))  (Green (Prim2 'cons v1 v2))]
       [(cons (Red v1) (Green v2))    (Red (Prim2 'cons v1 v2))]
       [(cons (Green v1) (Red v2))    (Red (Prim2 'cons v1 v2))]
       [(cons (Red v1) (Red v2))      (Red (Prim2 'cons v1 v2))])]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Function Application (Built in Racket functions and interpreter defined functions);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Symbol (Listof Expr) IEnv PEnv (Listof Defn) -> Annotation
(define (eval-i-app f es interp-env prog-env interp-fns)
  (if (interp-fn? f interp-fns)
      (let ((es2 (eval-i-es es interp-env prog-env interp-fns)))
        (if (contains-red es2)
            ;;At least one argument could not be fully evaluate so keep the function application
            (Red (App f (de-annotate-es es2)))
            ;;All arguments were partially evaluated, evaluate the function call
            ;;Dispatch to the approptriate interpreter function stored in interp-fns
            (eval-interp f (de-annotate-es es2) (list) prog-env interp-fns)))
      ;;Evaluate racket function
      (match f
        ['integer?
         (if (equal? (length es) 1)
             (match (car (eval-i-es es interp-env prog-env interp-fns))
               [(Green (Int i)) (Green (Bool #t))]
               [_ (Green (Bool #f))])
             (error (string-append "integer? expected 1 argument but received " (number->string (length es)))))]
        ['byte?
         (if (equal? (length es) 1)    
             (match (car (eval-i-es es interp-env prog-env interp-fns))
               [(Green (Int i)) (Green (Bool (byte? i)))]
               [(Green (Bool b)) (Green (Bool (byte? b)))]
               [(Green (Char c)) (Green (Bool (byte? c)))]
               [(Green (Eof)) (Green (Bool (byte? eof)))]
               [(Green (Err)) (Green (Bool #f))])
             (error (string-append "byte? expected 1 argument but received " (number->string (length es)))))]
        ['<=
         (if (>= (length es) 2)
             (Green (Bool (apply <= (map Int-i (de-annotate-es (eval-i-es es interp-env prog-env interp-fns))))))
             (error (string-append "<= expected 2 or more arguments but received " (number->string (length es)))))]
        ['list  (Green (eval-i-list (de-annotate-es (eval-i-es es interp-env prog-env interp-fns))))]
        ['symbol=?
         (if (= (length es) 2)
             (Green (Bool (apply symbol=? (map Symbol-s (de-annotate-es (eval-i-es es interp-env prog-env interp-fns))))))
             (error (string-append "symbol=? expected 2 arguments but received " (number->string (length es)))))]
        ['length
         (if (equal? (length es) 1)
             (Green (eval-list-length (car (de-annotate-es (eval-i-es es interp-env prog-env interp-fns)))))
             (error (string-append "length expected 1 argument but received " (number->string (length es)))))]
        ['list-ref
         (if (equal? (length es) 2)
             (Green (apply eval-list-ref (de-annotate-es (eval-i-es es interp-env prog-env interp-fns))))
             (error (string-append "list-ref expected 2 arguments but received " (number->string (length es)))))]
        [_ (error (string-append "partial evaluator does not support function "
                                 (symbol->string f)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Custom implementation of builtin Racket functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Return the expression at index i of a list given in AST form.
;;Needed because we only represent lists as (Prim2 'cons h t)
;;Expr -> Int
(define (eval-list-ref lst i)
  (match i
    [(Int 0)
     (match lst
       [(Prim2 'cons h t) h]
       [(Empty) (error "list-ref: index out of bounds")])]
    [(Int i)
     (if (< i 0)
         (error "list-ref: negative index not allowed")
         (match lst
           [(Prim2 'cons h t) (eval-list-ref t (Int (sub1 i)))]
           [(Empty) (error "list-ref: index out of bounds")]))]))
          
;;Return the length of a list given in AST form
;;Needed because we only represent lists as (Prim2 'cons h t)
;;Expr -> Int
(define (eval-list-length lst)
  (match lst
    [(Empty) (Int 0)]
    [(Prim2 'cons h t) (Int (add1 (Int-i (eval-list-length t))))]))

;;Conver a list of arguments into Prim2 cons list
;;(Listof Expr) -> Expr
(define (eval-i-list es)
  (match es
    [(cons h t)   (Prim2 'cons h (eval-i-list t))]
    [(list)       (Empty)]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Helper Functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Determine if a list of Annotations contains a Red value
;;(Listof Annotation) -> boolean
(define (contains-red es)
  (match es
    ['() #f]
    [(cons e es)
     (match e
       [(Green _)
        (contains-red es)]
       [(Red _)
        #t])]))

;;(Listof Annotation) -> (Listof Expr)
(define (de-annotate-es es)
  (match es
    ['() '()]
    [(cons e es)
     (match e
       [(Green v)
        (cons v (de-annotate-es es))]
       [(Red v)
        (cons v (de-annotate-es es))])]))

;;(Listof Expr) -> (Listof Value)
(define (extract-value-es es)
  (match es
    ['() (list)]
    [(cons h es)
     (let ((v (match h
                [(Int i) i]
                [(Bool b) b]
                [(Char c) c]
                [(Symbol s) s])))
       (cons v (extract-value-es es)))]))
       


;;Finds the first clause in a list of clauses that matches a given expression from the interpreter.
;;It returns a pair of the updated interp-env caused by variable bindings in the clause, the programs environment, and the expression
;;associated with the clause.
;;(Listof Clause) Expr IEnv IEnv (Listof Defn) -> Clause
(define (find-clause-i clauses expr interp-env prog-env interp-fns)
  (begin (debug "find-clause-i" expr interp-env prog-env)
  (match clauses
    ['() (error "program contains an invalid expression")]
    [(cons (Clause p b) clauses)
     (match p
       [(Int s)
        (if (and (Int? expr) (equal? s (Int-i expr)))
            (cons interp-env (cons prog-env b))
            (find-clause-i clauses expr interp-env prog-env interp-fns))]
       [(Bool v)
        (if (and (Bool? expr) (equal? v (Bool-b expr)))
            (cons interp-env (cons prog-env b))
            (find-clause-i clauses expr interp-env prog-env interp-fns))]
       [(Err)
        (if (Err? expr)
            (cons interp-env (cons prog-env b))
            (find-clause-i clauses expr interp-env prog-env interp-fns))]
       [(Var v) ;;A single varible v in a pattern matches any expression
        (cons (extend v expr interp-env) (cons prog-env b))]
       [(Empty)
        (match expr
          [(Empty)  (cons interp-env (cons prog-env b))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(Wild)
        (cons interp-env (cons prog-env b))]
       [(Cons h v)
        (match expr
          [(Prim2 'cons sh sv)
           (cons (extend h sh (extend v sv interp-env)) (cons prog-env b))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(Pred p)
        (match p
          ['integer?
           (match expr
             [(Int _) (cons interp-env (cons prog-env b))]
             [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
          ['boolean?
           (match expr
             [(Bool _) (cons interp-env (cons prog-env b))]
             [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
          ['char?
           (match expr
             [(Char _) (cons interp-env (cons prog-env b))]
             [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
          ['eof-object?
           (match expr
             [(Eof) (cons interp-env (cons prog-env b))]
             [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
          ['void?
           (match expr
             [(Void) (cons interp-env (cons prog-env b))]
             [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])])]
       [(Pat (Empty))
        (match expr
          [(Empty)
           (cons interp-env (cons prog-env b))]
          [_
           (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(Pat (Int (? symbol? s)))
        (match expr
          [(Int i) (cons (extend s i interp-env) (cons prog-env b))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(Pat (If (? symbol? se1) (? symbol? se2) (? symbol? se3)))
        (match expr
          [(If e1 e2 e3) (cons (extend se1 e1 (extend se2 e2 (extend se3 e3 interp-env))) (cons prog-env b))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(Pat (Bool (? symbol? sb)))
        (match expr
          [(Bool bool) (cons (extend sb bool interp-env) (cons prog-env b))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(Pat (Char (? symbol? sb)))
        (match expr
          [(Char char) (cons (extend sb char interp-env) (cons prog-env b))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(Pat (Eof))
        (match expr
          [(Eof) (cons interp-env (cons prog-env b))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(Pat (Var (? symbol? s1)))
        (match expr
          [(Var e1) (cons (extend s1 e1 interp-env) (cons prog-env b))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(Pat (Prim0 (? symbol? s)))
        (match expr
          [(Prim0 p)
           (cons (extend s p interp-env) (cons prog-env b))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(Pat (Prim1 (? symbol? p) (? symbol? e)))
        (match expr
          [(Prim1 pr expr) (cons (extend p pr (extend e expr interp-env)) (cons prog-env b))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(Pat (Prim2 (? symbol? p) (? symbol? e1) (? symbol? e2)))
        (match expr
          [(Prim2 pr v1 v2) (cons (extend p pr (extend e2 v2 (extend e1 v1 interp-env))) (cons prog-env b))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(Pat (Let (? symbol? s1) (? symbol? s2) (? symbol? s3)))
        (match expr
          [(Let x bi e) (cons (extend s3 e (extend s2 bi (extend s1 x interp-env))) (cons prog-env b))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(Pat (Begin2 (? symbol? s1) (? symbol? s2)))
        (match expr
          [(Begin2 e1 e2) (cons (extend s1 e1 (extend s2 e2 interp-env)) (cons prog-env b))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(Symbol s)
        (if (and (Symbol? expr) (equal? s (Symbol-s expr)))
            (cons interp-env (cons prog-env b))
            (find-clause-i clauses expr interp-env prog-env interp-fns))]
       [(Env-Cons (? symbol? s1) (? symbol? s2) (? symbol? s3))
        (match expr
          [(Prim2 'cons (Prim2 'cons x (Prim2 'cons bi (Empty))) rest)
              (cons (extend s1 x (extend s2 bi (extend s3 rest interp-env))) (cons prog-env b))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(List-S-PWVs s (list (PWV ps vs) ...))
        (match expr
          [(Prim2 'cons (Symbol h) t)
           (if (and (equal? s h) (Prim2? t) (equal? (Prim2-p t) 'cons))
               (let ((res (match-pwvs ps vs t interp-env prog-env interp-fns)))
                 (if res
                   (cons res (cons prog-env b))
                   (find-clause-i clauses expr interp-env prog-env interp-fns)))
               (find-clause-i clauses expr interp-env prog-env interp-fns))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(List-S-Vs s (list vs ...))
        (match expr
          [(Prim2 'cons (Symbol h) t)
           (if (and (equal? s h) (Prim2? t) (equal? (Prim2-p t) 'cons))
               (let ((res (match-vs vs t interp-env prog-env interp-fns)))
                 (if res
                   (cons res (cons prog-env b))
                   (find-clause-i clauses expr interp-env prog-env interp-fns)))
               (find-clause-i clauses expr interp-env prog-env interp-fns))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(List-S-Ps s (list ps ...))
        (match expr
          [(Prim2 'cons (Symbol h) t)           
           (if (and (equal? s h) (Prim2? t) (equal? (Prim2-p t) 'cons))
               (let ((res (match-ps ps t interp-env prog-env interp-fns)))
                 (if res
                     (cons interp-env (cons prog-env b))
                     (find-clause-i clauses expr interp-env prog-env interp-fns)))
               (find-clause-i clauses expr interp-env prog-env interp-fns))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(List-S-LSV s1 (list s2 var))
        (match expr
          [(Prim2 'cons (Symbol v1) (Prim2 'cons (Prim2 'cons (Symbol v2) (Prim2 'cons v3 (Empty))) (Empty)))
           (if (and (equal? s1 v1) (equal? s2 v2))
               (cons (extend var v3 interp-env) (cons prog-env b))
               (find-clause-i clauses expr interp-env prog-env interp-fns))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])])])))


;;If all the predicates match the contents of the list in the order they are listed, return
;;the environment extended with the variables bound to the appropriate values. If at any point
;;a predicate does not match, return false.

;;(Listof symbol) (Listof symbol) Expr IEnv PEnv(Listof Defn)-> IEnv or #f
(define (match-pwvs ps vs lst interp-env prog-env interp-fns)
  (begin
    (debug "match-pwvs" lst interp-env prog-env)
    (match (cons ps vs)
      [(cons '() '())
       (match lst
         [(Empty) interp-env]
         [_ #f])]
      [(cons (cons 'integer? ps) (cons v vs))
       (match lst
         [(Prim2 'cons (Int i) lst)
          (match-pwvs ps vs lst (extend v (Int i) interp-env) prog-env interp-fns)]
         [_ #f])]
      [(cons (cons 'boolean? ps) (cons v vs))
       (match lst
         [(Prim2 'cons (Bool b) lst)
          (match-pwvs ps vs lst (extend v (Bool b) interp-env) prog-env interp-fns)]
         [_ #f])]
      [(cons (cons 'char? ps) (cons v vs))
       (match lst
         [(Prim2 'cons (Char c) lst)
          (match-pwvs ps vs lst (extend v (Char c) interp-env) prog-env interp-fns)]
         [_ #f])]
      [(cons (cons 'byte? ps) (cons v vs))
       (match lst
         [(Prim2 'cons (Int i) lst)
          (if (byte? i)
              (match-pwvs ps vs lst (extend v (Int i) interp-env) prog-env interp-fns)
              #f)]
         [_ #f])]
      [(cons (cons s ps) (cons v vs)) ;;Any other predicate is assumed to be defined in one of the interpreter files
       (match lst
         [(Prim2 'cons h lst)
          (let ((res (eval-i (App s (list h)) interp-env prog-env interp-fns)))
            (match res
              [(Green (Bool #t)) (match-pwvs ps vs lst (extend v h interp-env) prog-env interp-fns)]
              [_ #f]))]
         [_ #f])])))

;;If all the predicates match the contents of the list in the order they are listed, return true. If at any point
;;a predicate does not match, return false.

;;(Listof symbol) Expr IEnv PEnv(Listof Defn)-> Bool
(define (match-ps ps lst interp-env prog-env interp-fns)
  (begin
    (debug "match-ps" lst interp-env prog-env)
    (match ps
      ['()
       (match lst
         [(Empty) #t]
         [_ #f])]
      [(cons 'integer? ps)
       (match lst
         [(Prim2 'cons (Int i) lst)
          (match-ps ps lst interp-env prog-env interp-fns)]
         [_ #f])]
      [(cons 'boolean? ps)
       (match lst
         [(Prim2 'cons (Bool b) lst)
          (match-ps ps lst interp-env prog-env interp-fns)]
         [_ #f])]
      [(cons 'char? ps)
       (match lst
         [(Prim2 'cons (Char c) lst)
          (match-ps ps lst interp-env prog-env interp-fns)]
         [_ #f])]
      [(cons 'byte? ps)
       (match lst
         [(Prim2 'cons (Int i) lst)
          (if (byte? i)
              (match-ps ps lst interp-env prog-env interp-fns)
              #f)]
         [_ #f])]
      [(cons s ps) ;;Any other predicate is assumed to be defined in one of the interpreter files
       (match lst
         [(Prim2 'cons h lst)
          (let ((res (eval-i (App s (list h)) interp-env prog-env interp-fns)))
            (match res
              [(Green (Bool #t)) (match-ps ps lst interp-env prog-env interp-fns)]
              [_ #f]))]
         [_ #f])])))

;;Returns the interpreters environment extended with the variable bindings from the pattern or false if the number of
;;variables does not match the number of data items to be bound

;;(Listof symbol) Expr IEnv PEnv (Listof Defn)-> IEnv or #f
(define (match-vs vs lst interp-env prog-env interp-fns)
  (begin
    (debug "match-vs" lst interp-env prog-env)
    (match vs
      [(cons v vs)
       (match lst
         [(Prim2 'cons h lst)
          (match-vs vs lst (extend v h interp-env) prog-env interp-fns)]
         [_ #f])]
      ['()
       (match lst
         [(Empty) interp-env]
         [_ #f])])))


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
    [(cons (Defn fn xs e) interp-fns)
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

;;Find all the free variables within an expression. Every variable is free. It is up to the surrounding expression to remove it if it is bound.
;;Expr -> (Listof Symbol)
(define (free-vars e)
  (match e
    [(Int i) '()]
    [(Bool b) '()]
    [(Char c) '()]
    [(Eof) '()]
    [(Void) '()]
    [(Var v) (list v)]
    [(If e1 e2 e3)
     (append (free-vars e1) (free-vars e2) (free-vars e3))]
    [(Begin2 e1 e2)
     (append (free-vars e1) (free-vars e2))]
    [(App f es)
     (apply append (map free-vars es))]
    [(Match e cls)
     (append (free-vars e) (apply append (map (λ (c) (remove* (pattern-vars (Clause-p c)) (free-vars (Clause-e c)))) cls)))]))

;;Pattern -> (List of symbols)
(define (pattern-vars p)
  (match p
    [(Int i) '()]
    [(Bool b) '()]
    [(Char c) '()]
    [(Eof) '()]
    [(Empty) '()]
    [(Symbol s) '()]
    [(Var v) (list v)]
    [(Wild) '()]
    [(Err) '()]
    [(Cons h v) (list h v)]
    [(Box b) (list b)]
    [(Env-Cons v1 v2 v3) (list v1 v2 v3)]
    [(Pred p) '()]
    [(PWV _ v) (list v)]
    [(List-S-PWVs _ pwvs) (map (λ (pwv)
                              (match pwv
                                [(PWV _ v) v])) pwvs)]
    [(List-S-Ps _ _) '()]
    [(List-S-Vs _ vs) vs]
    [(List-S-LSV _ (list _ v)) (list v)]
    [(Pat (Int s)) (list s)]
    [(Pat (Bool b)) (list b)]
    [(Pat (Char s)) (list s)]
    [(Pat (Eof)) '()]
    [(Pat (Prim0 p)) (list p)]
    [(Pat (Begin2 e1 e2)) (list e1 e2)]
    [(Pat (Prim1 s1 s2)) (list s1 s2)]
    [(Pat (If s1 s2 s3)) (list s1 s2 s3)]))

;;Return the expression with free variables filled out
;;Expr IEnv PEnv (Listof Defn) -> Expr
(define (eval-fvs e interp-env prog-env interp-fns)
  (eval-fvs-h e (list) interp-env prog-env interp-fns))

;;Expr (Listof Symbols) IEnv PEnv (Listof Defn)
(define (eval-fvs-h e bound-vs interp-env prog-env interp-fns)
  (match e
    [(Int i) e]
    [(Bool b) e]
    [(Char c) e]
    [(Symbol s) e]
    [(Eof) e]
    [(Err) e]
    [(Empty) e]
    [(Var v) (if (member v bound-vs)
                 (Var v)
                 (Green-e (eval-i (Var v) interp-env prog-env interp-fns)))]
    [(If e1 e2 e3)
     (If (eval-fvs-h e1 bound-vs interp-env prog-env interp-fns)
         (eval-fvs-h e2 bound-vs interp-env prog-env interp-fns)
         (eval-fvs-h e3 bound-vs interp-env prog-env interp-fns))]
    [(App f es)
     (App f (map (λ (e) (eval-fvs-h e bound-vs interp-env prog-env interp-fns)) es))]
    [(Prim0 p)
     (Prim0 p)]
    [(Prim1 p e)
     (Prim1 p (eval-fvs-h e bound-vs interp-env prog-env interp-fns))]
    [(Prim2 p e1 e2)
     (Prim2 p (eval-fvs-h e1 bound-vs interp-env prog-env interp-fns) (eval-fvs-h e2 bound-vs interp-env prog-env interp-fns))]
    [(Begin2 e1 e2)
     (Begin2
      (eval-fvs-h e1 bound-vs interp-env prog-env interp-fns)
      (eval-fvs-h e2 bound-vs interp-env prog-env interp-fns))]
    [(Match e cls)
     (Match (eval-fvs-h e bound-vs interp-env prog-env interp-fns)
            (map (λ (c) (Clause (Clause-p c) (eval-fvs-h (Clause-e c) (append (pattern-vars (Clause-p c)) bound-vs) interp-env prog-env interp-fns))) cls))]))
     
             
;;Print the string s if the debug? global variable is true
(define (debug fn-name prog interp-env prog-env)
  (if debug?
      (begin
        (display (string-append "In function: " fn-name))
        (display " with prog: ")
        (display prog)
        (display " and interp-env: ")
        (displayln interp-env)
        (display " and prog-env: ")
        (displayln prog-env)
        (displayln ""))
      (void)))


       