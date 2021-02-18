#lang racket
(begin
  ;(provide interp interp-env)
  ;(require "ast.rkt" "interp-prim.rkt")
  
  ;; type Expr =
  ;; | (Eof)
  ;; | (Int Integer)
  ;; | (Bool Boolean)
  ;; | (Char Character)
  ;; | (Prim0 Op0)
  ;; | (Prim1 Op1 Expr)
  ;; | (Prim1 Op2 Op2 Expr)
  ;; | (If Expr Expr Expr)
  ;; | (Begin Expr Expr)
  ;; | (Let Id Expr Expr)
  ;; | (Var Id)
  ;; type Id  = Symbol
  ;; type Op0 = 'read-byte
  ;; type Op1 = 'add1 | 'sub1 | 'zero?
  ;;          | 'char? | 'integer->char | 'char->integer
  ;;          | 'write-byte | 'eof-object?
  ;; type Op2 = '+ | '-

  (define (Eof)           (list 'Eof))
  (define (Int i)         (list 'Int i))
  (define (Bool b)        (list 'Bool b))
  (define (Char c)        (list 'Char c))
  (define (Prim0 p)       (list 'Prim0 p))
  (define (Prim1 p e)     (list 'Prim1 p e))
  (define (Prim2 p e1 e2) (list 'Prim2 p e1 e2))
  (define (If e1 e2 e3)   (list 'If e1 e2 e3))
  (define (Begin e1 e2)   (list 'Begin e1 e2))
  (define (Let x e1 e2)   (list 'Let x e1 e2))
  (define (Var x)         (list 'Var x))
  
  ;; type Answer = Value | 'err

  ;; type Value =
  ;; | Integer
  ;; | Boolean
  ;; | Character
  ;; | Eof
  ;; | Void
  
  ;; type REnv = (Listof (List Id Value))
  
  ;; Expr -> Answer
  (define (interp e)
    (interp-env e '()))

  ;; Expr Env -> Answer
  (define (interp-env e r)
    (match (car e)
      ['Int   (let ((i (second e))) i)]
      ['Bool  (let ((b (second e))) b)]
      ['Char  (let ((c (second e))) c)]
      ['Eof   eof] ; hmm
      ['Var   (let ((x (second e))) (lookup r x))]
      ['Prim0 (let ((p (second e))) (interp-prim0 p))]
      ['Prim1
       (let ((p (second e))
             (e (third e)))
         (match (interp-env e r)
           ['err 'err]
           [v (interp-prim1 p v)]))]
      ['Prim2
       (let ((p  (second e))
             (e1 (third e))
             (e2 (fourth e)))
         (match (interp-env e1 r)
           ['err 'err]
           [v1 (match (interp-env e2 r)
                 ['err 'err]
                 [v2 (interp-prim2 p v1 v2)])]))]
      ['If
       (let ((e1 (second e))
             (e2 (third e))
             (e3 (fourth e)))
         (match (interp-env e1 r)
           ['err 'err]
           [v
            (if v
                (interp-env e2 r)
                (interp-env e3 r))]))]
      ['Begin
       (let ((e1 (second e))
             (e2 (third e)))
         (match (interp-env e1 r)
           ['err 'err]
           [v    (interp-env e2 r)]))]
      ['Let
       (let ((x  (second e))
             (e1 (third e))
             (e2 (fourth e)))
         (match (interp-env e1 r)
           ['err 'err]
           [v (interp-env e2 (ext r x v))]))]))

;; Env Id -> Value
  (define (lookup r x)
    (second (assq x r)))

  ;; Env Id Value -> Env
  (define (ext r v val)
    (cons (list v val) r))

  ;; Op0 -> Answer
  (define (interp-prim0 op)
    (match op
      ['read-byte (read-byte)]
      ['peek-byte (peek-byte)]
      ['void      (void)]))
  
  ;; Op1 Value -> Answer
  (define (interp-prim1 op v)
    (match op
      ['add1          (if (integer? v) (add1 v) 'err)]
      ['sub1          (if (integer? v) (sub1 v) 'err)]
      ['zero?         (if (integer? v) (zero? v) 'err)]
      ['char?         (char? v)]
      ['char->integer (if (char? v) (char->integer v) 'err)]
      ['integer->char (if (codepoint? v) (integer->char v) 'err)]
      ['eof-object?   (eof-object? v)]
      ['write-byte    (if (byte? v) (write-byte v) 'err)]))
  
  ;; Op2 Value Value -> Answer
  (define (interp-prim2 op v1 v2)
    (match op
      ['+ (if (if (integer? v1) (integer? v2) #f) (+ v1 v2) 'err)]
      ['- (if (if (integer? v1) (integer? v2) #f) (- v1 v2) 'err)]))
  
  ;; Any -> Boolean
  (define (codepoint? v) ; ugly because no and, or, n-ary <=
    (if (integer? v)
        (if (if (<= 0 v)
                (<= v 55295)
                #f)
            #t
            (if (<= 57344 v)
                (<= v 1114111)
                #f))
        #f))
  
  ;; Go!
  (interp
   (list 'Let 'x (list 'Prim0 'read-byte)
         (list 'Prim2 '+ (list 'Var 'x) (list 'Int 4)))))
