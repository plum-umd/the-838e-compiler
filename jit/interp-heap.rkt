#lang racket
(begin
  (provide interp interp-env-heap)
  (require "heap.rkt"
           "env.rkt"
           "unload.rkt"
           "interp-prims-heap.rkt"
           "program-ast.rkt")

  ;; type Answer* =
  ;; | (cons Heap Value*)
  ;; | 'err

  ;; type Value* =
  ;; | Integer
  ;; | Boolean
  ;; | Character
  ;; | Eof
  ;; | Void
  ;; | '()
  ;; | (list 'box  Address)
  ;; | (list 'cons Address)

  ;; type Heap = (Listof Value*)
  ;; type REnv = (Listof (List Id Value*))

  ;; Expr -> Answer
  (define (interp e)
    (match e  
    [(Prog ds e) (unload (interp-env-heap e '() '() ds))]))

  ;; Expr REnv Heap -> Answer*
  (define (interp-env-heap e r h ds)
    (match e
      [(Int i)  (cons h i)]
      [(Bool b) (cons h b)]
      [(Char c) (cons h c)]
      [(Eof)    (cons h eof)]
      [(Empty)  (cons h '())]
      [(Var x)  (cons h (lookup r x))]
      [(Prim0 p) (interp-prim0 p h)]
      [(Prim1 p e)
       (match (interp-env-heap e r h ds)
         ['err 'err]
         [(cons h a)
          (interp-prim1 p a h)])]
      [(Prim2 p e1 e2)
       (match (interp-env-heap e1 r h ds)
         ['err 'err]
         [(cons h a1)        
          (match (interp-env-heap e2 r h ds)
            ['err 'err]
            [(cons h a2)
             (interp-prim2 p a1 a2 h)])])]
      [(If p e1 e2)
       (match (interp-env-heap p r h ds)
         ['err 'err]
         [(cons h v)
          (if v
              (interp-env-heap e1 r h ds)
              (interp-env-heap e2 r h ds))])]
      [(Begin2 e1 e2)     
       (match (interp-env-heap e1 r h ds)
         ['err 'err]
         [_    (interp-env-heap e2 r h ds)])]
      [(Let x e1 e2)
       (match (interp-env-heap e1 r h ds)
         ['err 'err]
         [(cons h v)
          (interp-env-heap e2 (ext r x v) h ds)])]
      [(App f es)
        (match (interp-env-heap* es r h ds)
          [(cons vs h)
            (match (defns-lookup ds f)
              [(Defn f xs e)
              ; check arity matches
                (if (= (length xs) (length vs))
                  (interp-env-heap e (zip xs vs) h ds)
                  'err)])]
          [_ 'err])]))
          
          
;; (Listof Expr) REnv Defns -> (Listof Value) | 'err
(define (interp-env-heap* es r h ds)
  (match es
    ['() (cons '() h)]     ; return the heap once we finish iterating
    [(cons e es)
     (match (interp-env-heap e r h ds)
       ['err 'err]
       [(cons h v) (match (interp-env-heap* es r h ds)
                      [(cons vs h) (cons (cons v vs) h)])])])))
