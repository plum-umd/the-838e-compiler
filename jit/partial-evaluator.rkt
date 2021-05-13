#lang racket

;;The partial evaluator will run the interpreter on the given program. If the interpreter attempts to perform a Red operation (for itself or
;; because that is what the program is doing), the result of the operation will be a Red annotation containing the red operation. It is up the parent
;; expression to handle this.

;; Red means that the contained expression could not be further evaluated
;; Green means the contained expression is an evaluated value. In the case when a green value is the result of the interpreter looking up a variable,
;; it may still be an expression that is considered Red later on.

(provide eval)
(require "interpreter-ast.rkt")
(require "program-ast.rkt")

;;Expr = PExpr | IExpr
;;Annotation = (Red IExpr) | (Green IExpr)
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
         (f (IDefn-f interpreter))
         (xs (IDefn-xs interpreter))
         (body (IDefn-e interpreter)))
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
      [(IInt i) (Green e)]
      [(IBool b) (Green e)]
      [(IChar c) (Green e)]
      [(ISymbol s) (Green e)]
      [(IEof) (Green e)]
      [(IErr) (Green e)]
      [(IEmpty) (Green e)]
      [(IVar v)
       (let ((value (lookup v interp-env)))
         (Green (match value
                  [(? symbol? s) (ISymbol s)]
                  [(? integer? i) (IInt i)]
                  [(? boolean? b) (IBool b)]
                  [(? char? c) (IChar c)]
                  [(? interpreter-ast? a) a]
                  [(? program-ast? a) a])))]
      [(IAnd es)
       (match es
         ['() (Green (IBool #t))]
         [_ (eval-and es interp-env prog-env interp-fns)])]
      [(IOr es)
       (match es
         ['() (Green (IBool #f))]
         [_ (eval-or es interp-env prog-env interp-fns)])]
      [(IIf expr true false)
       (let ((v (eval-i expr interp-env prog-env interp-fns)))
         (match v
           [(Green v)
            (match v
              [(IBool b)
               (if (equal? b #f)
                   (eval-i false interp-env prog-env interp-fns)
                   (eval-i true interp-env prog-env interp-fns))]
              [_
               (eval-i true interp-env prog-env interp-fns)])]
           [(Red v)
            (Red (IIf v true false))]))]   
      [(IMatch expr cls)                  ;;This is the interpreter attempting to pattern match an expression expr.
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
            (Red (eval-fvs (IMatch expr cls) interp-env prog-env interp-fns))]))]
      [(IBegin2 e1 e2)
       (let ((v1 (eval-i e1 interp-env prog-env interp-fns))
             (v2 (eval-i e2 interp-env prog-env interp-fns)))
         (match (cons v1 v2)
           [(cons (Red v1) (Red v2))
            (Red (IBegin2 v1 v2))]
           [(cons (Green v1) (Green v2))
            (Green v2)]
           [(cons (Green v1) (Red v2))
            (Red v2)]
           [(cons (Red v1) (Green v2))
            (Red (IBegin2 v1 v2))]))]
      [(ILet x b e)
       (let ((v (eval-i b interp-env prog-env interp-fns))) 
         (match v
           [(Green v1)   (eval-i e (extend x v1 interp-env) prog-env interp-fns)] 
           ;; extend the interp-env because this is a let in interp
           [(Red v1)     (Red (ILet x v1 e))]))]
      [(IApp f es)
       (eval-i-app f es interp-env prog-env interp-fns)]
      [(IPrim0 p)
       (eval-i-prim0 p interp-env prog-env interp-fns)]
      [(IPrim1 p e)
       (eval-i-prim1 p e interp-env prog-env interp-fns)]
      [(IPrim2 p e1 e2)
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
       [(Green (IBool #f))
        (Green (IBool #f))]
       [(Green _)
        (eval-and es interp-env prog-env interp-fns)]
       [(Red v)
        (Red (IAnd (cons v es)))])]))

;;(Listof Expr) -> Annotation
(define (eval-or es interp-env prog-env interp-fns)
  (match es
    [(list e) (eval-i e interp-env prog-env interp-fns)]
    [(cons e es)
     (match (eval-i e interp-env prog-env interp-fns)
       [(Green (IBool #f))
        (eval-or es interp-env prog-env interp-fns)]
       [(Green v)
        (Green v)]
       [(Red v)
        (Red (IOr (cons v es)))])]))
       
    
;;;;;;;;;;;;;;;;;;;;Primitives that accept no arguments;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Symbol IEnv PEnv (Listof Defn) -> Annotation
(define (eval-i-prim0 p interp-env prog-env interp-fns)
  (match p
    ['void
     (Green (IVoid))]
    ['read-byte
     (Red (IPrim0 'read-byte))]
    ['peek-byte
     (Red (IPrim0 'peek-byte))]))

;;Symbol Expr IEnv PEnv (Listof Defn) -> Annotation
(define (eval-i-prim1 p e interp-env prog-env interp-fns)
  (match p
    ['add1
     (match (eval-i e interp-env prog-env interp-fns)
       [(Green (IInt i))
        (Green (IInt (add1 i)))]
       [(Red v)
        (Red (IPrim1 'add1 v))])] ;; I believe this should be add1
    ['sub1
     (match (eval-i e interp-env prog-env interp-fns)
       [(Green (IInt i))
        (Green (IInt (sub1 i)))]
       [(Red v)
        (Red (IPrim1 'sub1 v))])]
    ['zero?
     (match (eval-i e interp-env prog-env interp-fns)
       [(Green (IInt i))
        (Green (IBool (zero? i)))]
       [(Red v)
        (Red (IPrim1 'zero? v))])]
    ['char?
     (match (eval-i e interp-env prog-env interp-fns)
       [(Green (IChar c)) (Green (IBool #t))]
       [(Green _) (Green (IBool #f))]
       [(Red v)
        (Red (IPrim1 'char? v))])]
    ['integer->char
     (match (eval-i e interp-env prog-env interp-fns)
       [(Green (IInt i))
        (Green (IChar (integer->char i)))]
       [(Red v)
        (Red (IPrim1 'integer->char v))])]
    ['char->integer
     (match (eval-i e interp-env prog-env interp-fns)
       [(Green (IChar c))
        (Green (IInt (char->integer c)))]
       [(Red v)
        (Red (IPrim1 'char->integer v))])]
    ['eof-object?
     (match (eval-i e interp-env prog-env interp-fns)
       [(Green (IEof))
        (Green (IBool #t))]
       [(Green _)
        (Green (IBool #f))]
       [(Red v)
        (Red (IPrim1 'eof-object? v))])]
    ['write-byte
     (match (eval-i e interp-env prog-env interp-fns)
       [(Green v)
        (Red (IPrim1 'write-byte v))]
       [(Red v)
        (Red (IPrim1 'write-byte v))])]
    ['box
     (match (eval-i e interp-env prog-env interp-fns)
       [(Green v)
        (Green (IPrim1 'box v))]
       [(Red v)
        (Red (IPrim1 'box v))])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;Primitives that accept two arguments;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Symbol Expr Expr IEnv PEnv (Listof Defn) -> Annotation
(define (eval-i-prim2 p e1 e2 interp-env prog-env interp-fns)
  (match p
    ['+
     (match (eval-i e1 interp-env prog-env interp-fns)
       [(Green (IInt i1))
            (match (eval-i e2 interp-env prog-env interp-fns)
              [(Green (IInt i2))   (Green (IInt (+ i1 i2)))]
              [(Red v2)           (Red (IPrim2 '+ (IInt i1) v2))])]
       [(Red v1)
            (match (eval-i e2 interp-env prog-env interp-fns)
              [(Green (IInt i2))   (Red (IPrim2 '+ v1 (IInt i2)))]
              [(Red v2)           (Red (IPrim2 '+ v1 v2))])])]
    ['-
     (match (eval-i e1 interp-env prog-env interp-fns)
       [(Green (IInt i1))
            (match (eval-i e2 interp-env prog-env interp-fns)
              [(Green (IInt i2))   (Green (IInt (- i1 i2)))]
              [(Red v2)           (Red (IPrim2 '- (IInt i1) v2))])]
       [(Red v1)
            (match (eval-i e2 interp-env prog-env interp-fns)
              [(Green (IInt i2))   (Red (IPrim2 '- v1 (IInt i2)))]
              [(Red v2)           (Red (IPrim2 '- v1 v2))])])]
    ['cons
     (match (cons (eval-i e1 interp-env prog-env interp-fns) (eval-i e2 interp-env prog-env interp-fns))
       [(cons (Green v1) (Green v2))  (Green (IPrim2 'cons v1 v2))]
       [(cons (Red v1) (Green v2))    (Red (IPrim2 'cons v1 v2))]
       [(cons (Green v1) (Red v2))    (Red (IPrim2 'cons v1 v2))]
       [(cons (Red v1) (Red v2))      (Red (IPrim2 'cons v1 v2))])]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Function Application (Built in Racket functions and interpreter defined functions);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Symbol (Listof Expr) IEnv PEnv (Listof Defn) -> Annotation
(define (eval-i-app f es interp-env prog-env interp-fns)
  (if (interp-fn? f interp-fns)
      (let ((es2 (eval-i-es es interp-env prog-env interp-fns)))
        
        (if (contains-red es2)
            ;;At least one argument could not be fully evaluate so keep the function application
            (Red (IApp f (de-annotate-es es2)))
            ;;All arguments were partially evaluated, evaluate the function call
            ;;Dispatch to the approptriate interpreter function stored in interp-fns
            (eval-interp f (de-annotate-es es2) (list) prog-env interp-fns)))
      ;;Evaluate racket function
      (match f
        ['integer?
         (if (equal? (length es) 1)
             (match (car (eval-i-es es interp-env prog-env interp-fns))
               [(Green (IInt i)) (Green (IBool #t))]
               [_ (Green (IBool #f))])
             (error (string-append "integer? expected 1 argument but received " (number->string (length es)))))]
        ['byte?
         (if (equal? (length es) 1)    
             (match (car (eval-i-es es interp-env prog-env interp-fns))
               [(Green (IInt i)) (Green (IBool (byte? i)))]
               [(Green (IBool b)) (Green (IBool (byte? b)))]
               [(Green (IChar c)) (Green (IBool (byte? c)))]
               [(Green (IEof)) (Green (IBool (byte? eof)))])
             (error (string-append "byte? expected 1 argument but received " (number->string (length es)))))]
        ['<=
         (if (>= (length es) 2)
             (Green (IBool (apply <= (map IInt-i (de-annotate-es (eval-i-es es interp-env prog-env interp-fns))))))
             (error (string-append "<= expected 2 or more arguments but received " (number->string (length es)))))]
        ['list  (Green (eval-i-list (de-annotate-es (eval-i-es es interp-env prog-env interp-fns))))]
        ['symbol=?
         (if (= (length es) 2)
             (Green (IBool (apply symbol=? (map ISymbol-s (de-annotate-es (eval-i-es es interp-env prog-env interp-fns))))))
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
    [(IInt 0)
     (match lst
       [(IPrim2 'cons h t) h]
       [(IEmpty) (error "list-ref: index out of bounds")])]
    [(IInt i)
     (if (< i 0)
         (error "list-ref: negative index not allowed")
         (match lst
           [(IPrim2 'cons h t) (eval-list-ref t (IInt (sub1 i)))]
           [(IEmpty) (error "list-ref: index out of bounds")]))]))
          
;;Return the length of a list given in AST form
;;Needed because we only represent lists as (Prim2 'cons h t)
;;Expr -> Int
(define (eval-list-length lst)
  (match lst
    [(IEmpty) (IInt 0)]
    [(IPrim2 'cons h t) (IInt (add1 (IInt-i (eval-list-length t))))]))

;;Conver a list of arguments into Prim2 cons list
;;(Listof Expr) -> Expr
(define (eval-i-list es)
  (match es
    [(cons h t)   (IPrim2 'cons h (eval-i-list t))]
    [(list)       (IEmpty)]))



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
                [(IInt i) i]
                [(IBool b) b]
                [(IChar c) c]
                [(ISymbol s) s])))
       (cons v (extract-value-es es)))]))
       


;;Finds the first clause in a list of clauses that matches a given expression from the interpreter.
;;It returns a pair of the updated interp-env caused by variable bindings in the clause, the programs environment, and the expression
;;associated with the clause.
;;(Listof Clause) Expr IEnv IEnv (Listof Defn) -> Clause
(define (find-clause-i clauses expr interp-env prog-env interp-fns)
  (begin (debug "find-clause-i" expr interp-env prog-env)
  (match clauses
    ['() (error "program contains an invalid expression")]
    [(cons (IClause p b) clauses)
     (match p
       [(IInt s)
        (if (and (IInt? expr) (equal? s (IInt-i expr)))
            (cons interp-env (cons prog-env b))
            (find-clause-i clauses expr interp-env prog-env interp-fns))]
       [(IBool v)
        (if (and (IBool? expr) (equal? v (IBool-b expr)))
            (cons interp-env (cons prog-env b))
            (find-clause-i clauses expr interp-env prog-env interp-fns))]
       [(ISymbol s)
        (if (and (ISymbol? expr) (equal? s (ISymbol-s expr)))
            (cons interp-env (cons prog-env b))
            (find-clause-i clauses expr interp-env prog-env interp-fns))]
       [(IErr)
        (if (IErr? expr)
            (cons interp-env (cons prog-env b))
            (find-clause-i clauses expr interp-env prog-env interp-fns))]
       [(IVar v) ;;A single varible v in a pattern matches any expression
        (cons (extend v expr interp-env) (cons prog-env b))]
       [(IEmpty)
        (match expr
          [(IEmpty)  (cons interp-env (cons prog-env b))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(IWild)
        (cons interp-env (cons prog-env b))]
       [(ICons h v)
        (match expr
          [(IPrim2 'cons sh sv)
           (cons (extend h sh (extend v sv interp-env)) (cons prog-env b))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(IPred p)
        (match p
          ['integer?
           (match expr
             [(IInt _) (cons interp-env (cons prog-env b))]
             [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
          ['boolean?
           (match expr
             [(IBool _) (cons interp-env (cons prog-env b))]
             [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
          ['char?
           (match expr
             [(IChar _) (cons interp-env (cons prog-env b))]
             [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
          ['eof-object?
           (match expr
             [(IEof) (cons interp-env (cons prog-env b))]
             [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
          ['void?
           (match expr
             [(IVoid) (cons interp-env (cons prog-env b))]
             [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])])]
       [(IStruct es)
        (match (list-ref es 0)
          ['Empty
           (match expr
             [(Empty)
              (cons interp-env (cons prog-env b))]
             [_
              (find-clause-i clauses expr interp-env prog-env interp-fns)])]
          ['Int
           (match expr
             [(Int i) (cons (extend (list-ref es 1) (IInt i) interp-env) (cons prog-env b))]
             [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
          ['Bool
           (match expr
             [(Bool bool) (cons (extend (list-ref es 1) (IBool bool) interp-env) (cons prog-env b))]
             [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
          ['Char
           (match expr
             [(Char char) (cons (extend (list-ref es 1) (IChar char) interp-env) (cons prog-env b))]
             [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
          ['Eof
           (match expr
             [(Eof) (cons interp-env (cons prog-env b))]
             [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
          ['If
           (match expr
             [(If e1 e2 e3) (cons (extend (list-ref es 1) e1 (extend (list-ref es 2) e2 (extend (list-ref es 3) e3 interp-env))) (cons prog-env b))]
             [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
          ['Var
           (match expr
             [(Var e1) (cons (extend (list-ref es 1) e1 interp-env) (cons prog-env b))]
             [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
          ['Prim0
           (match expr
             [(Prim0 p)
              (cons (extend (list-ref es 1) p interp-env) (cons prog-env b))]
             [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
          ['Prim1
           (match expr
             [(Prim1 pr expr) (cons (extend (list-ref es 1) pr (extend (list-ref es 2) expr interp-env)) (cons prog-env b))]
             [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
          ['Prim2
           (match expr
             [(Prim2 pr v1 v2) (cons (extend (list-ref es 1) pr (extend (list-ref es 2) v1 (extend (list-ref es 3) v2 interp-env))) (cons prog-env b))]
             [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
          ['Let
           (match expr
             [(Let x bi e) (cons (extend (list-ref es 3) e (extend (list-ref es 2) bi (extend (list-ref es 1) x interp-env))) (cons prog-env b))]
             [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
          ['Begin2
           (match expr
             [(Begin2 e1 e2) (cons (extend (list-ref es 1) e1 (extend (list-ref es 2) e2 interp-env)) (cons prog-env b))]
             [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])])]
                        
       [(IEnv-Cons (? symbol? s1) (? symbol? s2) (? symbol? s3))
        (match expr
          [(IPrim2 'cons (IPrim2 'cons x (IPrim2 'cons bi (IEmpty))) rest)
              (cons (extend s1 x (extend s2 bi (extend s3 rest interp-env))) (cons prog-env b))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(IList-S-PWVs s (list (IPWV ps vs) ...))
        (match expr
          [(IPrim2 'cons (ISymbol h) t)
           (if (and (equal? s h) (IPrim2? t) (equal? (IPrim2-p t) 'cons))
               (let ((res (match-pwvs ps vs t interp-env prog-env interp-fns)))
                 (if res
                   (cons res (cons prog-env b))
                   (find-clause-i clauses expr interp-env prog-env interp-fns)))
               (find-clause-i clauses expr interp-env prog-env interp-fns))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(IList-S-Vs s (list vs ...))
        (match expr
          [(IPrim2 'cons (ISymbol h) t)
           (if (and (equal? s h) (IPrim2? t) (equal? (IPrim2-p t) 'cons))
               (let ((res (match-vs vs t interp-env prog-env interp-fns)))
                 (if res
                   (cons res (cons prog-env b))
                   (find-clause-i clauses expr interp-env prog-env interp-fns)))
               (find-clause-i clauses expr interp-env prog-env interp-fns))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(IList-S-Ps s (list ps ...))
        (match expr
          [(IPrim2 'cons (ISymbol h) t)           
           (if (and (equal? s h) (IPrim2? t) (equal? (IPrim2-p t) 'cons))
               (let ((res (match-ps ps t interp-env prog-env interp-fns)))
                 (if res
                     (cons interp-env (cons prog-env b))
                     (find-clause-i clauses expr interp-env prog-env interp-fns)))
               (find-clause-i clauses expr interp-env prog-env interp-fns))]
          [_ (find-clause-i clauses expr interp-env prog-env interp-fns)])]
       [(IList-S-LSV s1 (list s2 var))
        (match expr
          [(IPrim2 'cons (ISymbol v1) (IPrim2 'cons (IPrim2 'cons (ISymbol v2) (IPrim2 'cons v3 (IEmpty))) (IEmpty)))
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
         [(IEmpty) interp-env]
         [_ #f])]
      [(cons (cons 'integer? ps) (cons v vs))
       (match lst
         [(IPrim2 'cons (IInt i) lst)
          (match-pwvs ps vs lst (extend v (IInt i) interp-env) prog-env interp-fns)]
         [_ #f])]
      [(cons (cons 'boolean? ps) (cons v vs))
       (match lst
         [(IPrim2 'cons (IBool b) lst)
          (match-pwvs ps vs lst (extend v (IBool b) interp-env) prog-env interp-fns)]
         [_ #f])]
      [(cons (cons 'char? ps) (cons v vs))
       (match lst
         [(IPrim2 'cons (IChar c) lst)
          (match-pwvs ps vs lst (extend v (IChar c) interp-env) prog-env interp-fns)]
         [_ #f])]
      [(cons (cons 'byte? ps) (cons v vs))
       (match lst
         [(IPrim2 'cons (IInt i) lst)
          (if (byte? i)
              (match-pwvs ps vs lst (extend v (IInt i) interp-env) prog-env interp-fns)
              #f)]
         [_ #f])]
      [(cons (cons s ps) (cons v vs)) ;;Any other predicate is assumed to be defined in one of the interpreter files
       (match lst
         [(IPrim2 'cons h lst)
          (let ((res (eval-i (IApp s (list h)) interp-env prog-env interp-fns)))
            (match res
              [(Green (IBool #t)) (match-pwvs ps vs lst (extend v h interp-env) prog-env interp-fns)]
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
         [(IEmpty) #t]
         [_ #f])]
      [(cons 'integer? ps)
       (match lst
         [(IPrim2 'cons (IInt i) lst)
          (match-ps ps lst interp-env prog-env interp-fns)]
         [_ #f])]
      [(cons 'boolean? ps)
       (match lst
         [(IPrim2 'cons (IBool b) lst)
          (match-ps ps lst interp-env prog-env interp-fns)]
         [_ #f])]
      [(cons 'char? ps)
       (match lst
         [(IPrim2 'cons (IChar c) lst)
          (match-ps ps lst interp-env prog-env interp-fns)]
         [_ #f])]
      [(cons 'byte? ps)
       (match lst
         [(IPrim2 'cons (IInt i) lst)
          (if (byte? i)
              (match-ps ps lst interp-env prog-env interp-fns)
              #f)]
         [_ #f])]
      [(cons s ps) ;;Any other predicate is assumed to be defined in one of the interpreter files
       (match lst
         [(IPrim2 'cons h lst)
          (let ((res (eval-i (IApp s (list h)) interp-env prog-env interp-fns)))
            (match res
              [(Green (IBool #t)) (match-ps ps lst interp-env prog-env interp-fns)]
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
         [(IPrim2 'cons h lst)
          (match-vs vs lst (extend v h interp-env) prog-env interp-fns)]
         [_ #f])]
      ['()
       (match lst
         [(IEmpty) interp-env]
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
    [(cons (IDefn fn xs e) interp-fns)
     (if (equal? f fn)
         #t
         (interp-fn? f interp-fns))]))

;;Given a symbol and a list of interpreter functions, return the first function with the same name as the symbol
;;Symbol (Listof Defn) -> Defn
(define (lookup-interpreter f lst)
  (match lst
    ['() (error (string-append (symbol->string f) " is not an interpreter function"))]
    [(cons (IDefn fun xs e) lst)
     (if (equal? fun f)
         (IDefn fun xs e)
         (lookup-interpreter f lst))]))

;;Find all the free variables within an expression. Every variable is free. It is up to the surrounding expression to remove it if it is bound.
;;Expr -> (Listof Symbol)
(define (free-vars e)
  (match e
    [(IInt i) '()]
    [(IBool b) '()]
    [(IChar c) '()]
    [(IEof) '()]
    [(IEmpty) '()]
    [(IErr) '()]
    [(ISymbol s) '()]
    [(IVoid) '()]
    [(IVar v) (list v)]
    [(IIf e1 e2 e3)
     (append (free-vars e1) (free-vars e2) (free-vars e3))]
    [(ILet x e1 e2)
     (append (free-vars e1) (remove* (free-vars e2) (list x)))]
    [(IOr es)
     (foldl (λ (e a) (append (free-vars e) a)) es)]
    [(IAnd es)
     (foldl (λ (e a) (append (free-vars e) a)) es)]
    [(IPrim0 _) '()]
    [(IPrim1 _ e1) (free-vars e1)]
    [(IPrim2 _ e1 e2) (append (free-vars e1) (free-vars e2))]
    [(IBegin2 e1 e2)
     (append (free-vars e1) (free-vars e2))]
    [(IApp f es)
     (apply append (map free-vars es))]
    [(IMatch e cls)
     (append (free-vars e) (apply append (map (λ (c) (remove* (pattern-vars (IClause-p c)) (free-vars (IClause-e c)))) cls)))]))

;;Pattern -> (List of symbols)
(define (pattern-vars p)
  (match p
    [(IInt i) '()]
    [(IBool b) '()]
    [(IChar c) '()]
    [(IEof) '()]
    [(IEmpty) '()]
    [(ISymbol s) '()]
    [(IVar v) (list v)]
    [(IWild) '()]
    [(IErr) '()]
    [(ICons h v) (list h v)]
    [(IBox b) (list b)]
    [(IEnv-Cons v1 v2 v3) (list v1 v2 v3)]
    [(IPred p) '()]
    [(IPWV _ v) (list v)]
    [(IList-S-PWVs _ pwvs) (map (λ (pwv)
                              (match pwv
                                [(IPWV _ v) v])) pwvs)]
    [(IList-S-Ps _ _) '()]
    [(IList-S-Vs _ vs) vs]
    [(IList-S-LSV _ (list _ v)) (list v)]
    [(IStruct es) (sub1 (length es))]))

;;Return the expression with free variables filled out
;;Expr IEnv PEnv (Listof Defn) -> Expr
(define (eval-fvs e interp-env prog-env interp-fns)
  (eval-fvs-h e (list) interp-env prog-env interp-fns))

;;Expr (Listof Symbols) IEnv PEnv (Listof Defn)
(define (eval-fvs-h e bound-vs interp-env prog-env interp-fns)
  (match e
    [(? program-ast? a) a]
    [(IInt i) e]
    [(IBool b) e]
    [(IChar c) e]
    [(ISymbol s) e]
    [(IEof) e]
    [(IErr) e]
    [(IEmpty) e]
    [(IVar v) (if (member v bound-vs)
                 (IVar v)
                 (Green-e (eval-i (IVar v) interp-env prog-env interp-fns)))]
                   
    [(IIf e1 e2 e3)
     (IIf (eval-fvs-h e1 bound-vs interp-env prog-env interp-fns)
         (eval-fvs-h e2 bound-vs interp-env prog-env interp-fns)
         (eval-fvs-h e3 bound-vs interp-env prog-env interp-fns))]
    [(ILet x e1 e2)
     (ILet x
           (eval-fvs-h e1 bound-vs interp-env prog-env interp-fns)
           (eval-fvs-h e2 bound-vs interp-env prog-env interp-fns))]
    [(IOr es)
     (IOr (map (λ (e) (eval-fvs-h e bound-vs interp-env prog-env interp-fns)) es))]
    [(IAnd es)
     (IAnd (map (λ (e) (eval-fvs-h e bound-vs interp-env prog-env interp-fns)) es))]
    [(IApp f es)
     (IApp f (map (λ (e) (eval-fvs-h e bound-vs interp-env prog-env interp-fns)) es))]
    [(IPrim0 p)
     (IPrim0 p)]
    [(IPrim1 p e)
     (IPrim1 p (eval-fvs-h e bound-vs interp-env prog-env interp-fns))]
    [(IPrim2 p e1 e2)
     (IPrim2 p (eval-fvs-h e1 bound-vs interp-env prog-env interp-fns) (eval-fvs-h e2 bound-vs interp-env prog-env interp-fns))]
    [(IBegin2 e1 e2)
     (IBegin2
      (eval-fvs-h e1 bound-vs interp-env prog-env interp-fns)
      (eval-fvs-h e2 bound-vs interp-env prog-env interp-fns))]
    [(IMatch e cls)
     (IMatch (eval-fvs-h e bound-vs interp-env prog-env interp-fns)
            (map (λ (c) (IClause (IClause-p c) (eval-fvs-h (IClause-e c) (append (pattern-vars (IClause-p c)) bound-vs) interp-env prog-env interp-fns))) cls))]))
     
             
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


       