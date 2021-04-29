#lang racket
(provide evaluate)
(require "ast.rkt" "parse.rkt" "partial-evaluator.rkt" "unparse.rkt")

;;Given a program, return the result of partially evaluating it using the
;;interpreter.
;;Prog -> S-Expression
(define (evaluate prog)
  (let* ((interp-in (open-input-file "interp.rkt"))
         (interp-prim-in (open-input-file "interp-prim.rkt"))
         (interp (begin (read-line interp-in) (read interp-in))) ;;Ignore the #lang racket line and read the s-expression that comes next
         (interp-prim (begin (read-line interp-prim-in) (read interp-prim-in))) ;;Ignore the #lang racket line and read the s-expression that comes next
         (parsed-interp (parse interp))
         (parsed-interp-prim (parse interp-prim))
         (parsed-prog (parse prog))
         (interp-defns (extract-interp-definitions parsed-interp))
         (interp-prim-defns (extract-interp-definitions parsed-interp-prim))
         (result-expr (eval 'interp (append interp-defns interp-prim-defns) parsed-prog)))
    (unparse result-expr)))

;;Prog -> (Listof Defn)
(define (extract-interp-definitions prog)
  (match prog
    [(Begin es)
     (extract-interp-definitions-helper es (list))]))

;;(Listof Prog) -> (Listof Defn)
(define (extract-interp-definitions-helper es res)
  (match es
    ['() res]
    [(cons e es)
     (if (Defn? e)
         (extract-interp-definitions-helper es (cons e res))
         (extract-interp-definitions-helper es res))]))
         
  
    
    
    
    
  