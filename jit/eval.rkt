#lang racket
(provide evaluate)
(require "ast.rkt" "parse.rkt" "partial-evaluator.rkt" "unparse.rkt")

;;Given a program, return the result of partially evaluating it using the
;;interpreter.
;;Prog -> S-Expression
(define (evaluate prog)
  (let* ((interp-in (open-input-file "interp-heap.rkt"))
         (interp-prim-in (open-input-file "interp-prims-heap.rkt"))
         (heap-in (open-input-file "heap.rkt"))
         (env-in (open-input-file "env.rkt"))
         (unload-in (open-input-file "unload.rkt"))
         ;;NOTE: We should also include the parsed ast.rkt since the interprter depends on it. Non of its functions is called rn so we are good

         ;;Ignore the #lang racket line and read the s-expression that comes next
         (interp (begin (read-line interp-in) (read interp-in))) 
         (interp-prim (begin (read-line interp-prim-in) (read interp-prim-in)))
         (heap (begin (read-line heap-in) (read heap-in)))
         (env (begin (read-line env-in) (read env-in)))
         (unload (begin (read-line unload-in) (read unload-in)))

         ;;Parse the files and the program
         (parsed-interp (parse interp))
         (parsed-interp-prim (parse interp-prim))
         (parsed-heap (parse heap))
         (parsed-env (parse env))
         (parsed-unload (parse unload))
         (parsed-prog (parse prog))

         ;;Extract the definitions
         (interp-defns (extract-definitions parsed-interp))
         (interp-prim-defns (extract-definitions parsed-interp-prim))
         (heap-defns (extract-definitions parsed-heap))
         (env-defns (extract-definitions parsed-env))
         (unload-defns (extract-definitions parsed-unload))
         
         (result-expr (eval 'interp (append interp-defns interp-prim-defns heap-defns env-defns unload-defns) parsed-prog)))
    (unparse result-expr)))

;;Prog -> (Listof Defn)
(define (extract-definitions prog)
  (match prog
    [(Begin es)
     (extract-definitions-helper es (list))]))

;;(Listof Prog) (Listof Defn) -> (Listof Defn)
(define (extract-definitions-helper es res)
  (match es
    ['() res]
    [(cons e es)
     (if (Defn? e)
         (extract-definitions-helper es (cons e res))
         (extract-definitions-helper es res))]))    
    
    
  