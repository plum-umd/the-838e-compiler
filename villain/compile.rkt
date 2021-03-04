#lang racket
(provide (all-defined-out))
(require "ast.rkt" "parse.rkt" "types.rkt" "externs.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return  ; the dividend of div in string-ref and string-set!
(define rbx 'rbx) ; heap
(define rdx 'rdx) ; return, 2  ; remainder of division and scratch in string-ref
                               ; and string-set!

(define r8  'r8)  ; scratch in +, -, compile-chars, compile-prim2, string-ref,
                  ; make-string, compile-prim3, string-ref!, integer-length, match,
                  ; compile-define, open-input-file
(define r9  'r9)  ; scratch in assert-type, compile-str-chars, string-ref,
                  ; string-set!, make-string, compile-define, fl<=
                  ; compile-vector, vector-set!, vector-ref
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg
(define rsi 'rsi) ; arg2
(define r10 'r10) ; scratch in compile-prim3, make-string, string-set!, compile-vector, vector-set!
                  ; compile-define, fl<=
(define rcx 'rcx) ; arity indicator
(define al  'al)  ; low byte of rax ; open-input-file
(define xmm0 'xmm0) ; registers to hold double precision floating numbers

;; type CEnv = [Listof Variable]

;; Expr -> Asm
(define (compile p)
  (match p
    [(Prog ds e)
     (let ((le (label-λ (desugar-prog p))))
       (prog (Global 'entry)
           (Default 'rel)
           (Section '.text)
           (externs p)
           (Extern 'raise_error)
           (Global 'raise_error_align)
           (Extern 'str_to_symbol)
           (Label 'entry)
           (Mov rbx rdi) ; recv heap pointer
      ;     (compile-e-tail e '())
           (compile-e-tail le '())
           (Mov rdx rbx) ; return heap pointer in second return register
           (Ret)
      ;     (compile-defines ds)
           (compile-λ-definitions (λs le))
           (Label 'raise_error_align)
           (Sub rsp 8)
           (Jmp 'raise_error)))]))

;; Expr -> Asm
(define (compile-library p)
  (match p
    [(Lib xs ds)
     (prog (compile-provides xs)
           (Default 'rel)
           (Section '.text)
           (externs p)
           (Extern 'raise_error)
           (Extern 'raise_error_align)
           (Extern 'str_to_symbol)
           (compile-defines ds))]))
;           (compile-λ-definitions (λs
;            (label-λ (let ((bs (map desugar-def ds)))
 ;                      (Letrec (map car bs) (map cdr bs) (void))))))

;; [Listof Id] -> Asm
(define (compile-provides xs)
  (match xs
    ['()
     (seq)]
    [(cons x xs)
     (seq (Global (symbol->label x))
          (compile-provides xs))]))

(define (error-label c)
  (if (even? (length c))
      'raise_error
      'raise_error_align))

;; Expr CEnv Boolean -> Asm
(define (compile-e e c tail?)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Flonum f)         (compile-flonum f)]
    [(Eof)              (compile-value eof)]
    [(Empty)            (compile-value '())]
    [(String s)         (compile-string s)]
    [(Symbol s)         (compile-symbol s c)]
    [(Vec ds)           (compile-vector ds c)]
    [(Var x)            (compile-variable x c)]
    [(LCall e es)       (if (and (Var? e) (memq (Var-x e) stdlib-ids))
                            (compile-app (Var-x e) es c tail?)
                            (compile-call e es c tail?))]
    [(App f es)         (compile-app f es c tail?)]
    [(Apply f e)        (compile-apply f e c tail?)]
    [(Prim0 p)          (compile-prim0 p c)]
    [(Prim1 p e)        (compile-prim1 p e c)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c tail?)]
    [(Begin e1 e2)      (compile-begin e1 e2 c tail?)]
    [(Let x e1 e2)      (compile-let x e1 e2 c tail?)]
    [(Letrec xs es e)   (compile-letrec xs es e c tail?)]
    [(Lam-l l xs e0)     (compile-λ l (fvs e) c)]
    [(Lam*-l l xs xs* e0) (compile-λ l (fvs e) c)]
    [(Match e0 cs)      (compile-match e0 cs c tail?)]))

(define (compile-e-tail e c)
  (compile-e e c #t))
(define (compile-e-nontail e c)
  (compile-e e c #f))

;; Expr -> LExpr
(define (label-λ e)
  (match e
    [(Int i)            e]
    [(Bool b)           e]
    [(Char c)           e]
    [(Flonum f)         e]
    [(Eof)              e]
    [(Empty)            e]
    [(String s)         e]
    [(Symbol s)         e]
    [(Vec ds)           e]
    [(Var x)            e]
    [(LCall e es)       (LCall (label-λ e) (map label-λ es))]
    [(App f es)         (App f (map label-λ es))]
    [(Apply f e)        (Apply f (label-λ e))]
    [(Prim0 p)          e]
    [(Prim1 p e)        (Prim1 p (label-λ e))]
    [(Prim2 p e1 e2)    (Prim2 p (label-λ e1) (label-λ e2))]
    [(Prim3 p e1 e2 e3) (Prim3 p (label-λ e1) (label-λ e2) (label-λ e3))]
    [(If e1 e2 e3)      (If (label-λ e1) (label-λ e2) (label-λ e3))]
    [(Begin e1 e2)      (Begin (label-λ e1) (label-λ e2))]
    [(Let x e1 e2)      (Let x (map label-λ e1) (label-λ e2))]
    [(Letrec xs es e)   (Letrec xs (map label-λ es) (label-λ e))]
    [(Lam xs e)         (Lam-l (gensym) xs (label-λ e))]
    [(Lam* xs xs* e)    (Lam*-l (gensym) xs xs* (label-λ e))]
    [(Match e0 cs)      (Match (label-λ e0) cs)]))  ;; TODO:  label-λ cs

;; LExpr -> (Listof Id)
(define (fvs e)
 (begin
  (define (fvs e)
    (match e
      [(Int i)            '()]
      [(Bool b)           '()]
      [(Char c)           '()]
      [(Flonum f)         '()]
      [(Eof)              '()]
      [(Empty)            '()]
      [(String s)         '()]
      [(Symbol s)         '()]
      [(Vec ds)           '()]
      [(Var x)            (list x)]
      [(LCall e es)       (append (if (and (Var? e) (memq (Var-x e) stdlib-ids))
                                      '()
                                      (fvs e)) (apply append (map fvs es)))]
      [(App f es)         (apply append (map fvs es))]
      [(Apply f e)        (append (fvs f) (fvs e))]
      [(Prim0 p)          '()]
      [(Prim1 p e)        (fvs e)]
      [(Prim2 p e1 e2)    (append (fvs e1) (fvs e2))]
      [(Prim3 p e1 e2 e3) (append (fvs e1) (fvs e2) (fvs e3))]
      [(If e1 e2 e3)      (append (fvs e1) (fvs e2) (fvs e3))]
      [(Begin e1 e2)      (append (fvs e1) (fvs e2))]
      [(Let x e1 e2)      (append (apply append (map fvs e1))
                                (remq* x (fvs e2)))]
      [(Letrec xs es e)   (remq* xs (apply append (fvs e) (map fvs es)))]
      [(Lam-l l xs e)     (remq* xs (fvs e))]
      [(Lam*-l l xs xs* e) (remq* (cons xs* xs) (fvs e))]
      [(Match e0 cs)
       (append (fvs e0) (apply append (map (λ (c) (remq*
                (match-p-fvs (Clause-p c)) (fvs (Clause-e c)))) cs)))])) 
  (remove-duplicates (fvs e))))

;; Pat -> [Listof Id]
(define (match-p-fvs p)
  (match p
    [(Wild) '()]
    [(Var x) (list x)]
    [(Lit l) '()]
    [(Sym s) '()]
    [(Box x) (list x)]
    [(Cons x1 x2) (list x1 x2)]))


;; LExpr -> [Listof LLambda]
(define (λs e)
  (match e
    [(Int i)            '()]
    [(Bool b)           '()]
    [(Char c)           '()]
    [(Flonum f)         '()]
    [(Eof)              '()]
    [(Empty)            '()]
    [(String s)         '()]
    [(Symbol s)         '()]
    [(Vec ds)           '()]
    [(Var x)            '()]
    [(LCall e es)       (append (λs e) (apply append (map λs es)))]
    [(App f es)         (apply append (map λs es))]
    [(Apply f e)        (append (λs f) (λs e))]
    [(Prim0 p)          '()]
    [(Prim1 p e)        (λs e)]
    [(Prim2 p e1 e2)    (append (λs e1) (λs e2))]
    [(Prim3 p e1 e2 e3) (append (λs e1) (λs e2) (λs e3))]
    [(If e1 e2 e3)      (append (λs e1) (λs e2) (λs e3))]
    [(Begin e1 e2)      (append (λs e1) (λs e2))]
    [(Let x e1 e2)      (append (apply append (map λs e1)) (λs e2))]
    [(Letrec xs es e)   (append (apply append (map λs es)) (λs e))]
    [(Lam-l l xs e0)    (cons e (λs e0))]
    [(Lam*-l l xs xs* e0) (cons e (λs e0))]
    [(Lam xs e)         (error "Unlabelled lambda")]
    [(Lam* xs xs* e)    (error "Unlabelled lambda")]
    [(Match e0 cs)      (λs e0)]  ;; TODO: (λs cs)
    [_  '()]))   ;      for '*  

;; [Listof LLambda] -> Asm
(define (compile-λ-definitions ls)
  (match ls
    ['() (seq)]
    [(cons l ls)
     (seq (compile-λ-definition l)
          (compile-λ-definitions ls))]))

;; LLambda -> Asm
(define (compile-λ-definition l)
  (match l
    [(Lam-l f xs e0)
     (seq (%% "compile-λ-definition ")
          (Label f)          
          (Cmp rcx (imm->bits (length xs))) ; arity check
          (Jne 'raise_error)

          (compile-e-tail e0 (reverse (append xs (fvs l))))
          ; return
          (Add rsp (* 8 (+ (length xs) (length (fvs l))))) ; pop args & fvs
          (Mov rdx rbx)
          (Ret))]
    [(Lam*-l f xs xs* e0)
     (let ((loop (gensym 'loop))
           (end (gensym 'end))
           (skip (gensym 'skip)))

       (seq (%% "compile-λ-definition variable-arity")
            (Label f)                        
            (Cmp rcx (imm->bits (length xs)))
            (Jl 'raise_error)
            (Mov rax (imm->bits '()))         ; initialize rest arg
            (Sub rcx (imm->bits (length xs))) ; # of things to pop off of stack

            ;TODO: Move rsp to correct position
            (Add rsp (* 8 (length (fvs l))))
                           ; align stack pointer to where the last arg is
            (Mov r10 rsp)  ; r10 points to last arg (before the first free var)

            (Label loop) ; at each step, rax <- cons pop rax
            (Cmp rcx 0)
            (Je end)
            (Mov (Offset rbx 0) rax)
            (Pop rax)
            (Mov (Offset rbx 8) rax)
            (Mov rax rbx)
            (Add rbx 16)
            (Or rax type-cons)
            (Sub rcx (imm->bits 1))   
            (Jmp loop)
            (Label end)

            (Cmp rsp r10)
            (Je skip)
            ;TODO: Move env vars to correct position
            (%% "reposition env vars ")
            (reposition-env-vars (length (fvs l)))
            (Label skip)
            
            (%% "push the rest list (xs*)")
            (Push rax) ; push the rest list
            (compile-e-tail e0 (cons xs* (reverse (append xs (fvs l)))))
            ; return
            (Add rsp (* 8 (add1 (+ (length xs) (length (fvs l)))))) ; pop args
            (Ret)))]))

;; Integer -> Asm
(define (reposition-env-vars n)
  (match n
    [0 (seq)]
    [n (seq (Sub rsp 8)
            (Sub r10 8)
            (Mov rdx (Offset r10 0))
            (Mov (Offset rsp 0) rdx)
            (reposition-env-vars (sub1 n)))]))

;; LLambda [Listof Id] CEnv -> Asm
(define (compile-λ l fvs c)
  (seq (%% "compile-λ ")
       (Lea rax l)                  ;; address of function label
       (Mov (Offset rbx 0) rax)     ;; write this address on heap
       (Mov r8 (length fvs))
       (Mov (Offset rbx 8) r8)      ;; number of free vars
       (Mov r9 rbx)
       (Add r9 16)
       (%% "copy-env-to-heap ")
       (copy-env-to-heap fvs c 0)
       (%% "copy-env-to-heap-done ")
       (Mov rax rbx)
       (Or rax type-proc)
       (Add rbx (* 8 (+ 2 (length fvs))))
       (%% "compile-λ done ")))

;; [Listof Id] CEnv Integer -> Asm
(define (copy-env-to-heap fvs c i)
  (match fvs
    ['() (seq)]
    [(cons x fvs)
     (seq (Mov r8 (Offset rsp (lookup x c)))
          (Mov (Offset r9 (* 8 i)) r8)
          (copy-env-to-heap fvs c (add1 i)))]))

;; Id [Listof Expr] CEnv Boolean -> Asm
(define (compile-call f es c tail?)
  (if tail?
      (compile-tail-call f es c)
      (compile-nontail-call f es c)))

;; Id [Listof Expr] CEnv -> Asm
(define (compile-tail-call f es c)
  (seq (%% "compile-tail-call ")
       (compile-e-nontail f c)       
       (Push rax)
       (compile-es es (cons #f c))
       (Mov rax (Offset rsp (* 8 (length es))))
       (assert-proc rax c)
       (Xor rax type-proc)
       (%% "move args for tail call")
       (move-args (add1 (length c)) (length es))
       (Add rsp (* 8 (add1 (length c))))
       (copy-closure-env-to-stack)
       (Mov rcx (imm->bits (length es)))
       (Mov rdx (Offset rax 0))
       (Jmp rdx)))  ;; (Offset rax 0)


;; Integer Integer -> Asm
(define (move-args c-ct i)
  (cond [(zero? c-ct) (seq (%% "already in place for tail call"))]
        [(zero? i)    (seq (%% "done moving args for tail call"))]
        [else
         (seq (Mov r8 (Offset rsp (* 8 (sub1 i))))
              (Mov (Offset rsp (* 8 (+ c-ct (sub1 i)))) r8)
              (move-args c-ct (sub1 i)))]))

;; Id [Listof Expr] CEnv -> Asm
;; The return address is placed above the arguments, so callee pops
;; arguments and return address is next frame
(define (compile-nontail-call f es c)
  (let ((ret (gensym 'ret)))
    (if (even? (+ (length c) (length es)))
        (seq (%% "compile-nontail-call")
             (compile-e-nontail f c)
             (Push rax)
             (Lea r8 ret)
             (Push r8)
             (compile-es es (cons #f (cons #f c)))
             (Mov rax (Offset rsp (* 8 (add1 (length es)))))
             (assert-proc rax c)
             (Xor rax type-proc)
             (copy-closure-env-to-stack)
             (Mov rcx (imm->bits (length es)))
             (Mov rdx (Offset rax 0))
             (Jmp rdx)  ; (Offset rax 0)
             (Label ret)
             (Add rsp 8))
        (seq (%% "compile-nontail-call")
             (Sub rsp 8)
             (compile-e-nontail f (cons #f c))
             (Push rax)
             (Lea r8 ret)
             (Push r8)
             (compile-es es (cons #f (cons #f (cons #f c))))
             (Mov rax (Offset rsp (* 8 (add1 (length es)))))
             (assert-proc rax c)
             (Xor rax type-proc)
             (copy-closure-env-to-stack)
             (Mov rcx (imm->bits (length es)))
             (Mov rdx (Offset rax 0))
             (Jmp rdx) ; (Offset rax 0)
             (Label ret)
             (Add rsp 16)))))

(define (copy-closure-env-to-stack)
  (let ((loop (gensym 'copy_closure))
        (done (gensym 'copy_done)))
    (seq (%% "copy-closure-env-to-stack")
         (Mov r8 (Offset rax 8))    ; number of env vars
         (Mov r9 rax)               
         (Add r9 16)                ; start of env
;         (Mov r10 rbx)             ; start of stack
         (Label loop)
         (Cmp r8 0)
         (Je done)
         (Sub rsp 8)                ; Increment stack pointer
         (Mov rdx (Offset r9 0))
         (Mov (Offset rsp 0) rdx)   ; write env var on stack
         (Sub r8 1)
         (Add r9 8)              
         (Jmp loop)
         (Label done))))  

;; [Listof Defn] -> Asm
(define (compile-defines ds)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d)
          (compile-defines ds))]))

;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f xs e)
     (seq (Label (symbol->label f))
          (Cmp rcx (imm->bits (length xs))) ; arity check
          (Jne 'raise_error)
          (compile-e-tail e (reverse xs))
          ; return
          (Add rsp (* 8 (length xs))) ; pop args
          (Ret))]
    [(Defn* f xs xs* e)
     (let ((loop (gensym 'loop))
           (end (gensym 'end)))

       (seq (Label (symbol->label f))
            (Cmp rcx (imm->bits (length xs)))
            (Jl 'raise_error)
            (Mov rax (imm->bits '()))         ; initialize rest arg
            (Sub rcx (imm->bits (length xs))) ; # of things to pop off of stack

            (Label loop) ; at each step, rax <- cons pop rax
            (Cmp rcx 0)
            (Je end)
            (Mov (Offset rbx 0) rax)
            (Pop rax)
            (Mov (Offset rbx 8) rax)
            (Mov rax rbx)
            (Add rbx 16)
            (Or rax type-cons)
            (Sub rcx (imm->bits 1))
            (Jmp loop)
            (Label end)

            (Push rax) ; push the rest list
            (compile-e-tail e (cons xs* (reverse xs)))
            ; return
            (Add rsp (* 8 (add1 (length xs)))) ; pop args
            (Ret)))]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (imm->bits v))))

;; Flonum -> Asm
(define (compile-flonum f)
        (seq
         (Mov rax (flonum->bits f))
         (Mov (Offset rbx 0) rax)
               (Mov rax rbx)
               (Or rax type-flonum)
               (Add rbx 8))
  )

;; String -> Asm
(define (compile-string s)
  (let ((len (string-length s)))
    (seq (Mov r9 (imm->bits len))
         (Mov (Offset rbx 0) r9)         ;; write length in word 0
         (Mov r9 0)
         (compile-str-chars (string->list s) 3 0 1)
         (Mov rax rbx)
         (Or rax type-string)
         (Add rbx (* 8 (add1 (ceiling (/ len 3))))))))

;; Vec CEnv -> Asm
(define (compile-vector ds c)
  (let ((len (length ds)))
    (seq (Mov r9 len)
         (Mov (Offset rbx 0) r9) ;;write length in first word, will also store rbx location
         (Mov r10 rbx)
         (Add rbx 8)
         (Mov r9 rbx)    ;;r9 will be used in compile-vec-elems as a temporary heap pointer
         (Add rbx (* 8 len)) ;; rbx now points to next open space on heap for future calls
         (compile-vec-elems ds c)
         (Mov rax r10)
         (Or rax type-vector))))

;; Recursively adds each element in the list vs
(define (compile-vec-elems vs c)
  (match vs
    ['() (seq)]
    [(cons e vs)
     (seq (Push r10)
          (Push r9)
          (compile-e-nontail e c)
          (Pop r9)
          (Pop r10)
          (Mov (Offset r9 0) rax)
          (Add r9 8)
          (compile-vec-elems vs c))]))

;; (Listof Chars) Integer Integer Integer -> Asm
;; Three 21-bit chars in each word, so with 1-indexing for chars:
;; word 1: char3 char2 char1 ;  word 2: char6 char5 char3;  etc.
(define (compile-str-chars cs n rem quot)
  (match cs
    ['()  (if (= rem 0)
              (seq)
              (seq (Mov (Offset rbx (* 8 quot)) r9)))]
    [(cons c cs)
     (seq (Mov r8 (imm->bits c))
          (if (= rem 0) (seq) (Sal r8 (* rem 21)))
          (Add r9 r8)
          (if (= rem 2)
              (seq  (Mov (Offset rbx (* 8 quot)) r9)
                    (Mov r9 0))
              (seq))
          (let ((m (add1 n)))
            (compile-str-chars cs m (remainder m 3) (quotient m 3))))]))

;; String CEnv -> Asm
;; Transform 'foo into (string->symbol "foo") then compile it
(define (compile-symbol s c)
  (seq (compile-prim1 'string->symbol
                      (String (symbol->string s))
                      c)))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; Id Expr CEnv Boolean -> Asm
(define (compile-apply f e c tail?)
  (if tail?
      (compile-tail-apply f e c)
      (compile-nontail-apply f e c)))

;; Id Expr CEnv -> Asm
(define (compile-tail-apply f e c)
  (seq (compile-e-nontail e c)
       (Add rsp (* 8 (length c))) ; reset stack
       (list->stack c)
       (Jmp (symbol->label f))))

;; Id Expr CEnv -> Asm
(define (compile-nontail-apply f e c)
  (let ((ret  (gensym 'ret)))
    (seq (compile-e-nontail e c)
         (pad-stack c)
         (Lea r8 ret)
         (Push r8)
         (list->stack c)
         (Jmp (symbol->label f))
         (Label ret)
         (unpad-stack c))))

;; Traverse list in rax, pushing elements on to stack,
;; calculating length in rcx
(define (list->stack c)
  (let ((done (gensym 'done))
        (loop (gensym 'loop)))
    (seq (Mov rcx (imm->bits 0))
         (Label loop)
         (Cmp rax (imm->bits '()))
         (Je done)
         (assert-cons rax c)
         (Xor rax type-cons)
         (Mov r8 (Offset rax 8))
         (Push r8)
         (Add rcx (imm->bits 1))
         (Mov rax (Offset rax 0))
         (Jmp loop)
         (Label done))))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (match p
    ['void      (seq (Mov rax val-void))]
    ['read-byte (seq (pad-stack c)
                     (Call 'read_byte)
                     (unpad-stack c))]
    ['peek-byte (seq (pad-stack c)
                     (Call 'peek_byte)
                     (unpad-stack c))]
    ['read-char (seq (pad-stack c)
                     (Call 'read_char)
                     (unpad-stack c))]
    ['peek-char (seq (pad-stack c)
                     (Call 'peek_char)
                     (unpad-stack c))]
    ['gensym    (seq (pad-stack c)
                     (Call 'gensym)
                     (unpad-stack c)
                     (Or rax type-symbol))]))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e-nontail e c)
       (match p
         ['add1
          (seq (assert-integer rax c)
               (Add rax (imm->bits 1)))]
         ['sub1
          (seq (assert-integer rax c)
               (Sub rax (imm->bits 1)))]
         ['zero?
          (let ((l1 (gensym)))
            (seq (assert-integer rax c)
                 (Cmp rax 0)
                 (Mov rax val-true)
                 (Je l1)
                 (Mov rax val-false)
                 (Label l1)))]
         ['integer?
          (let ((l1 (gensym)))
            (seq (And rax mask-int)
                 (Xor rax type-int)
                 (Cmp rax 0)
                 (Mov rax val-true)
                 (Je l1)
                 (Mov rax val-false)
                 (Label l1)))]
         ['integer-length
          (seq (assert-integer rax c)
               (Sar rax imm-shift)
               (Mov r8 rax)
               (Sar r8 63)
               (Xor rax r8)
               (Bsr rax rax)
               (Sal rax int-shift))]
         ['char?
          (let ((l1 (gensym)))
            (seq (And rax mask-char)
                 (Xor rax type-char)
                 (Cmp rax 0)
                 (Mov rax val-true)
                 (Je l1)
                 (Mov rax val-false)
                 (Label l1)))]
         ['char->integer
          (seq (assert-char rax c)
               (Sar rax char-shift)
               (Sal rax int-shift))]
         ['integer->char
          (seq (assert-codepoint c)
               (Sar rax int-shift)
               (Sal rax char-shift)
               (Xor rax type-char))]
         ['eof-object? (eq-imm val-eof)]
         [(or 'char-whitespace? 'char-alphabetic?)
          (let ((l (gensym)))
            (seq (assert-char rax c)
                 (pad-stack c)
                 (Sar rax char-shift)
                 (Mov rdi rax)
                 (Call (char-op->uc p))
                 (unpad-stack c)
                 (Cmp rax 0)
                 (Mov rax val-true)
                 (Jne l)
                 (Mov rax val-false)
                 (Label l)))]
         [(or 'char-upcase 'char-downcase 'char-titlecase)
          (let ((l (gensym)))
            (seq (assert-char rax c)
                 (pad-stack c)
                 (Sar rax char-shift)
                 (Mov rdi rax)
                 (Call (char-op->uc p))
                 (unpad-stack c)
                 (Sal rax char-shift)
                 (Or rax type-char)))]
         ['write-byte
          (seq (assert-byte c)
               (pad-stack c)
               (Mov rdi rax)
               (Call 'write_byte)
               (unpad-stack c)
               (Mov rax val-void))]
         ['write-char
          (seq (assert-char rax c)
               (pad-stack c)
               (Mov rdi rax)
               (Call 'write_char)
               (unpad-stack c)
               (Mov rax val-void))]
         ['box
          (seq (Mov (Offset rbx 0) rax)
               (Mov rax rbx)
               (Or rax type-box)
               (Add rbx 8))]
         ['unbox
          (seq (assert-box rax c)
               (Xor rax type-box)
               (Mov rax (Offset rax 0)))]
         ['car
          (seq (assert-cons rax c)
               (Xor rax type-cons)
               (Mov rax (Offset rax 8)))]
         ['cdr
          (seq (assert-cons rax c)
               (Xor rax type-cons)
               (Mov rax (Offset rax 0)))]
         ['string-length
          (seq (assert-string rax c)
               (Xor rax type-string)
               (Mov rax (Offset rax 0)))]
         ['string?
          (type-pred ptr-mask type-string)]
         ['string->symbol
          (seq (assert-string rax c)
               (Xor rax type-string)
               (pad-stack c)
               (Mov rdi rax)
               (Call 'str_to_symbol)
               (unpad-stack c)
               (Or rax type-symbol))]
         ['symbol->string
          (seq (assert-symbol rax c)
               (Xor rax type-symbol)     ; replace symbol tag with str
               (Or rax type-string))]
         ['symbol?
          (type-pred ptr-mask type-symbol)]
         ['empty? (eq-imm val-empty)]
         ['port?
           (type-pred ptr-mask type-port)]
         ['open-input-file
           (seq
             (assert-string rax c)

             ;; Call to C function that opens file
             (pad-stack c)
             (Mov rdi rax)
             (Call 'open_input_file)
             (unpad-stack c)
             ;; rax now contains a FILE *

             ;; struct Port {
             ;;   FILE *file;
             ;;   int8_t buffer_len;
             ;;   int8_t buffer_offset;
             ;;   int8_t buffer_closed;
             ;;   int8_t buffer[port-buffer-bytes];
             ;; };
             (Mov r8 rbx)
             (Mov (Offset rbx 0) rax) ;; Store file pointer on heap
             (Xor al al)
             (Mov (Offset rbx 8) al)  ;; Store offset into buffer
             (Mov (Offset rbx 9) al)  ;; Store number of buffered bytes
             (Mov (Offset rbx 10) al) ;; Store "closed" flag
             ;; Advance heap pointer, allocating space for a buffer
             ;; Choose actual space allocated based on declared
             ;; port-buffer-size and bytes used by rest of structure to maintain
             ;; heap alignment.
             (Add rbx (+ (- 8 (modulo (+ 11 port-buffer-bytes) 8)) 11
                         port-buffer-bytes))
             (Mov rax r8)
             (Or rax type-port))]
         ['close-input-port
           (seq
             (assert-port rax c)
             (pad-stack c)
             (Mov rdi rax)
             (Call 'close_input_port)
             (unpad-stack c)
             (Mov rax val-void))]
         ['read-byte
          (seq
            (assert-port rax c)
            (pad-stack c)
            (Mov rdi rax)
            (Call 'read_byte_port)
            (unpad-stack c)
            )]
         ['peek-byte
         (seq
           (assert-port rax c)
           (pad-stack c)
           (Mov rdi rax)
           (Call 'peek_byte_port)
           (unpad-stack c)
           )]
         ['vector? (type-pred ptr-mask type-vector)]
         ['vector-length
          (seq (assert-vector rax c)
               (Xor rax type-vector)
               (Mov rax (Offset rax 0))
               (Sal rax int-shift)
               (Or rax type-int))]
         ['flonum?
          (type-pred ptr-mask type-flonum)])))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e-nontail e1 c)
       (Push rax)
       (compile-e-nontail e2 (cons #f c))
       (match p
         ['+
          (seq (Pop r8)
               (assert-integer r8 c)
               (assert-integer rax c)
               (Add rax r8))]
         ['-
          (seq (Pop r8)
               (assert-integer r8 c)
               (assert-integer rax c)
               (Sub r8 rax)
               (Mov rax r8))]
         ['quotient
          (seq (Mov r8 rax)
               (Pop rax)
               (assert-integer r8 c)
               (assert-integer rax c)
               (Cmp r8 (imm->bits 0))
               (Je (error-label c))
               (Cqo)
               (IDiv r8)
               (Sal rax int-shift)
               )]
         ['remainder
          (seq (Mov r8 rax)
               (Pop rax)
               (assert-integer r8 c)
               (assert-integer rax c)
               (Cmp r8 (imm->bits 0))
               (Je (error-label c))
               (Cqo)
               (IDiv r8)
               (Mov rax rdx)
               )]
         ['>
          (let ((gt-true (gensym 'gt)))
            (seq (Pop r8)
                 (assert-integer r8 c)
                 (assert-integer rax c)
                 (Cmp r8 rax)
                 (Mov rax (imm->bits #t))
                 (Jg gt-true)
                 (Mov rax (imm->bits #f))
                 (Label gt-true)))]
         ['<
          (let ((lt-true (gensym 'lt)))
            (seq (Pop r8)
                 (assert-integer r8 c)
                 (assert-integer rax c)
                 (Cmp r8 rax)
                 (Mov rax (imm->bits #t))
                 (Jl lt-true)
                 (Mov rax (imm->bits #f))
                 (Label lt-true)))]
         ['<=
          (let ((leq-true (gensym 'leq)))
            (seq (Pop r8)
                 (assert-integer r8 c)
                 (assert-integer rax c)
                 (Cmp r8 rax)
                 (Mov rax (imm->bits #t))
                 (Jle leq-true)
                 (Mov rax (imm->bits #f))
                 (Label leq-true)))]
         ['>=
          (let ((geq-true (gensym 'geq)))
            (seq (Pop r8)
                 (assert-integer r8 c)
                 (assert-integer rax c)
                 (Cmp r8 rax)
                 (Mov rax (imm->bits #t))
                 (Jge geq-true)
                 (Mov rax (imm->bits #f))
                 (Label geq-true)))]
         ['eq?
          (let ((l (gensym)))
            (seq (Pop r8)
                 (Cmp rax r8)
                 (Mov rax val-true)
                 (Je l)
                 (Mov rax val-false)
                 (Label l)))]
         ['string-ref
          (let ((l1 (gensym 'loopmax2x)) (l2 (gensym 'done)))
            (seq (Pop r8)
                 (assert-string r8 c)         ; r8 = str pointer
                 (assert-integer rax c)       ; rax = index
                 (Cmp rax 0)
                 (Jl (error-label c))
                 (Xor r8 type-string)
                 (Mov r9 (Offset r8 0))       ; r9 = length
                 (Sub r9 (imm->bits 1))       ; 0-indexing
                 (Cmp rax r9)
                 (Jg (error-label c))
                 (Mov rdx 0)
                 (Mov r9 (imm->bits 3))
                 (Div r9)                     ; divide rax by (imm->bits 3)
                 (Sal rax 3)                  ; quot. in rax; rem. in rdx
                 (Add r8 rax)
                 (Mov rax (Offset r8 8))      ; rax = the word containing char
                 (Mov r9 (sub1 (expt 2 21)))
                 (Label l1)                   ; rdx = 0, 1, or 2
                 (Cmp rdx 0)
                 (Je l2)
                 (Sub rdx (imm->bits 1))
                 (Sar rax 21)
                 (Jmp l1)                     ; loop until rdx = 0
                 (Label l2)
                 (And rax r9)))]
         ['make-vector
          (let ((l1 (gensym 'loop_start))
                (l2 (gensym 'loop_end) ))
            (seq (Pop r8)
                 (assert-integer r8 c)              ; r8 = int arg = length
                 (Sar r8 int-shift)                 ; unwrap length
                 (Cmp r8 0)
                 (Jl (error-label c))
                 (Mov r10 rbx)                      ; saves heap pointer in r10
                 (Mov (Offset rbx 0) r8) ;should r8
                 ;(Mov (Offset rbx 8) rax)
                 (Add rbx 8)                        ;advances heap pointer
                 (Label l1)
                 (Cmp r8 0)
                 (Je l2)                           ;(While r8 > 0){
                 (Mov (Offset rbx 0) rax) ;;should rax         ;Copies the value into the spot on the heap
                 (Add rbx 8)
                 (Sub r8 1)                         ;r8--;
                 (Jmp l1)                           ;}
                 (Label l2)                         ;done writing
                 (Mov rax r10)
                 (Or rax type-vector)))]
         ['vector-ref
          (seq (Pop r8)
               (assert-vector r8 c)         ; r8 = vector pointer
               (assert-integer rax c)       ; rax = index
               (Sar rax int-shift)          ; unwrap index
               (Cmp rax 0)
               (Jl (error-label c))
               (Xor r8 type-vector)
               (Mov r9 (Offset r8 0))       ; r9 = length
               (Add r8 8)                   ; r8 will now be pointing to the first element
               (Sub r9 1)                   ; 0-indexing
               (Cmp rax r9)
               (Jg (error-label c))
               (Sal rax 3)                  ; index*=8
               (Add r8 rax)
               (Mov rax (Offset r8 0)))]      ;Accounting for 0-indexing, we need to shift one more spot over
         ['make-string
          (let ((l1 (gensym 'words_loop)) (l2 (gensym 'rem1_))
                    (l3 (gensym 'done)) (l4 (gensym 'exit_loop)))
            (seq (Pop r8)
                 (assert-integer r8 c)        ; r8 = int arg. = length
                 (assert-char rax c)          ; rax = char arg
                 (Cmp r8 0)
                 (Jl (error-label c))
                 (Mov r10 rbx)                ; save heap pointer
                 (Mov (Offset rbx 0) r8)      ; write length in word 0
                 (Add rbx 8)                  ; advance heap pointer
                 (Cmp r8 (imm->bits 1))
                 (Jl l3)
                 (Mov r9 rax)                ; r9 = char arg
                 (Mov rax r8)                ; rax = dividend = length
                 (Mov rdx 0)
                 (Mov r8 (imm->bits 3))
                 (Div r8)                    ; divide rax by (imm->bits 3)
                                             ; quot. in rax; rem. in rdx
                 (Mov r8 r9)                 ; r8 = register to build the word
                 (Sal r8 21)
                 (Add r8 r9)
                 (Sal r8 21)
                 (Add r8 r9)          ; r8 = [char arg][char arg][char arg]
                 (Label l1)           ; loop to set quot. number of words to r11
                 (Cmp rax 0)
                 (Je l4)
                 (Mov (Offset rbx 0) r8)
                 (Add rbx 8)                 ; advance the heap pointer
                 (Sub rax 1)
                 (Jmp l1)
                 (Label l4)
                 (Cmp rdx 0)          ; if remainder = 0, then done
                 (Je l3)
                 (Mov rax r9)            ; if remainder = 1 or 2
                 (Add rbx 8)
                 (Cmp rdx (imm->bits 1))
                 (Je l2)
                 (Sal rax 21)             ; case that remainder = 2
                 (Add rax r9)
                 (Mov (Offset rbx -8) rax)
                 (Jmp l3)
                 (Label l2)               ; case that remainder = 1
                 (Mov (Offset rbx -8) rax)
                 (Label l3)
                 (Mov rax r10)            ; pointer to word 0 of the str
                 (Or rax type-string)))]
         ['cons
          (seq (Mov (Offset rbx 0) rax)
               (Pop rax)
               (Mov (Offset rbx 8) rax)
               (Mov rax rbx)
               (Or rax type-cons)
               (Add rbx 16))]

         ['fl+ (seq
               (Pop r8)
               (assert-flonum r8 c)
               (assert-flonum rax c)
               (Xor rax type-flonum)              
               (Xor r8 type-flonum)
               (Movapd xmm0 (Offset r8 0))
               (Addsd xmm0 (Offset rax 0))              
               (Movapd (Offset rbx 0) xmm0)
               (Mov rax rbx)
               (Or rax type-flonum)
               (Add rbx 8)
               )
               ]
         ['fl- (seq
               (Pop r8)
               (assert-flonum r8 c)
               (assert-flonum rax c)
               (Xor rax type-flonum)
              
               (Xor r8 type-flonum)
               (Movapd xmm0 (Offset r8 0))
               (Subsd xmm0 (Offset rax 0))               
               (Movapd (Offset rbx 0) xmm0)
               (Mov rax rbx)
               (Or rax type-flonum)
               (Add rbx 8)
               )
               ]


         ['fl=
          (let ((eq-true (gensym 'eq)))
            (seq (Pop r8)
                 (assert-flonum r8 c)
                 (assert-flonum rax c)
                 (Xor rax type-flonum)
                 (Mov rax (Offset rax 0))
                 (Xor r8 type-flonum)
                 (Mov r8 (Offset r8 0))
                 (Cmp rax r8)
                 (Mov rax (imm->bits #t))
                 (Je eq-true)
                 (Mov rax (imm->bits #f))
                 (Label eq-true)))]


         ['fl<=
          (let ((leq-true (gensym 'leq)))
            (seq (Pop r8)
                 (assert-flonum r8 c)
                 (assert-flonum rax c)
                 (Xor rax type-flonum)
                 (Mov rax (Offset rax 0))
                 (Xor r8 type-flonum)
                 (Mov r8 (Offset r8 0))
                 (Mov r9 (arithmetic-shift 1 63))
                 (Xor rax r9)
                 (Mov r10 (arithmetic-shift 1 63))
                 (Xor r8 r10)
                 (Cmp r8 rax)
                 (Mov rax (imm->bits #t))
                 (Jle leq-true)
                 (Mov rax (imm->bits #f))
                 (Label leq-true)))]

          )))

;; Op3 Expr Expr Expr CEnv -> Asm
(define (compile-prim3 p e1 e2 e3 c)
  (seq (compile-e-nontail e1 c)
       (Push rax)
       (compile-e-nontail e2 (cons #f c))
       (Push rax)
       (compile-e-nontail e3 (cons #f (cons #f c)))
       (match p
         ['string-set!
          (let ((l1 (gensym 'loopmax2x)) (l2 (gensym 'exit_loop))
                    (l3 (gensym 'done)))
            (seq (Pop r10)                     ; 2nd arg in r10: index
                 (Pop r8)                      ; 1st arg in r8: str
                 (assert-integer r10 c)
                 (assert-string r8 c)
                 (assert-char rax c)           ; 3rd arg in rax: char
                 (Xor r8 type-string)
                 (Mov r9 (Offset r8 0))        ; r9 = length
                 (Cmp r10 0)                   ; 0-indexing
                 (Jl (error-label c))
                 (Sub r9 (imm->bits 1))
                 (Cmp r10 r9)
                 (Jg (error-label c))
                 (Mov r9 rax)                  ; r9 = 3rd arg (char)
                 (Mov rax r10)                 ; rax = 2nd arg (index)
                 (Mov rdx 0)
                 (Mov r10 (imm->bits 3))
                 (Div r10)                     ; divide rax by (imm->bits 3)
                 (Sal rax 3)                   ; quot. in rax ; rem. in rdx
                 (Add r8 rax)                  ; advance the str pointer
                 (Mov rax (Offset r8 8))       ; rax = the word containing char
                 (Mov r10 (sub1 (expt 2 21)))
                 (Label l1)                    ; rdx = 0, 1, or 2
                 (Cmp rdx 0)
                 (Je l2)
                 (Sub rdx (imm->bits 1))
                 (Sal r10 21)
                 (Sal r9 21)
                 (Jmp l1)                      ; loop until rdx = 0
                 (Label l2)
                 (Mov rdx 1)                   ; rdx as mask
                 (Sal rdx 63)
                 (Sub rdx 1)
                 (Xor rdx r10)
                 (And rax rdx)                 ; set to zero char at index
                 (Or rax r9)                   ; write char arg at index
                 (Mov (Offset r8 8) rax)
                 (Mov rax val-void)))]
        ['vector-set!
         (seq (Pop r10)                    ; r10 = index
              (Pop r8)                     ; r8 = vector pointer
                                           ; rax = some value
              (assert-vector r8 c)
              (assert-integer r10 c)
              (Sar r10 int-shift)          ; unwrap index
              (Cmp r10 0)
              (Jl (error-label c))
              (Xor r8 type-vector)
              (Mov r9 (Offset r8 0))       ; r9 = length
              (Add r8 8)                   ; r8 will now be pointing to the first element
              (Sub r9 1)                   ; 0-indexing
              (Cmp r10 r9)
              (Jg (error-label c))
              (Sal r10 3)                  ; index*=8
              (Add r8 r10)
              (Mov (Offset r8 0) rax)
              (Mov rax val-void))])))

;; Id [Listof Expr] CEnv Boolean -> Asm
(define (compile-app f es c tail?)
  (if tail?
      (compile-tail-app f es c)
      (compile-nontail-app f es c)))

;; Id [Listof Expr] CEnv -> Asm
(define (compile-tail-app f es c)
  (seq (compile-es es c)
       (%% "move args for tail call")
       (move-args (length c) (length es))
       (Add rsp (* 8 (length c)))
       (Mov rcx (imm->bits (length es)))
       (Jmp (symbol->label f))))

;; Integer Integer -> Asm
;(define (move-args c-ct i)
;  (cond [(zero? c-ct) (seq (%% "already in place for tail call"))]
;        [(zero? i)    (seq (%% "done moving args for tail call"))]
;        [else
;         (seq (Mov r8 (Offset rsp (* 8 (sub1 i))))
;              (Mov (Offset rsp (* 8 (+ c-ct (sub1 i)))) r8)
;              (move-args c-ct (sub1 i)))]))

;; Id [Listof Expr] CEnv -> Asm
;; The return address is placed above the arguments, so callee pops
;; arguments and return address is next frame
(define (compile-nontail-app f es c)
  (let ((ret (gensym 'ret)))
    (if (odd? (length c))
        (seq (Lea r8 ret)
             (Push r8)
             (compile-es es (cons #f c))
             (Mov rcx (imm->bits (length es)))
             (Jmp (symbol->label f))
             (Label ret))
        (seq (Sub rsp 8)
             (Lea r8 ret)
             (Push r8)
             (compile-es es (cons #f (cons #f c)))
             (Mov rcx (imm->bits (length es)))
             (Jmp (symbol->label f))
             (Label ret)
             (Add rsp 8)))))

;; [Listof Expr] CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e-nontail e c)
          (Push rax)
          (compile-es es (cons #f c)))]))

;; Imm -> Asm
(define (eq-imm imm)
  (let ((l1 (gensym)))
    (seq (Cmp rax imm)
         (Mov rax val-true)
         (Je l1)
         (Mov rax val-false)
         (Label l1))))

;; Expr Expr Expr CEnv Boolean -> Asm
(define (compile-if e1 e2 e3 c tail?)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e-nontail e1 c)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c tail?)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c tail?)
         (Label l2))))

;; Expr Expr CEnv Boolean -> Asm
(define (compile-begin e1 e2 c tail?)
  (seq (compile-e-nontail e1 c)
       (compile-e e2 c tail?)))

;; (Listof Id) (Listof Expr) Expr CEnv Boolean -> Asm
(define (compile-let xs es e c tail?)
  (seq (compile-es es c)
       (compile-e e (append (reverse xs) c) tail?)
       (Add rsp (* 8 (length xs)))))

;; [Listof Id] [Listof LLambda] LExpr CEnv Boolean -> Asm
(define (compile-letrec fs ls e c tail?)
  (seq (%% "compile-letrec-λs")
       (compile-letrec-λs ls c)
       (%% "compile-letrec-init")
       (compile-letrec-init fs ls (append (reverse fs) c))
       (%% "compile-letrec-body")
       (compile-e e (append (reverse fs) c) tail?)
       (Add rsp (* 8 (length fs)))))

;; [Listof LLambda] CEnv -> Asm
(define (compile-letrec-λs ls c)
  (match ls
    ['() '()]
    [(cons l ls)
     (let ((label (if (Lam-l? l)
                      (Lam-l-l l)
                      (if (Lam*-l? l)
                          (Lam*-l-l l)
                          (error "a right-hand-side in letrec not λ")))))
       (let ((length-ys (length (fvs l))))
         (seq (Lea rax label)
              (Mov (Offset rbx 0) rax)
              (Mov r8 length-ys)
              (Mov (Offset rbx 8) r8)
              (Mov rax rbx)
              (Or rax type-proc)
              (Add rbx (* 8 (+ 2 length-ys)))
              (Push rax)
              (compile-letrec-λs ls c))))]))

;; [Listof Id] [Listof LLambda] CEnv -> Asm
(define (compile-letrec-init fs ls c)
  (match fs
    ['() '()]
    [(cons f fs)
     (let ((ys (fvs (first ls))))
       (seq (Mov r9 (Offset rsp (lookup f c)))
            (Xor r9 type-proc)
            (Add r9 16)
            (copy-env-to-heap ys c 0)
            (compile-letrec-init fs (rest ls) c)))]))        

;; Expr [Listof Clause] CEnv Boolean -> Asm
(define (compile-match e0 cs c tail?)
  (let ((return (gensym 'matchreturn)))
    (seq (compile-e-nontail e0 c)
         (compile-match-clauses cs return c tail?)
         (Label return))))

;; [Listof Clauses] Symbol CEnv Boolean -> Asm
(define (compile-match-clauses cs return c tail?)
  (match cs
    ['() (seq (Jmp (error-label c)))]
    [(cons cl cs)
     (let ((next (gensym 'matchclause)))
       (seq (compile-match-clause cl next return c tail?)
            (Label next)
            (compile-match-clauses cs return c tail?)))]))

;; Clause Symbol Symbol CEnv Boolean -> Asm
(define (compile-match-clause cl next return c tail?)
  (match cl
    [(Clause p e)
     (match p
       [(Wild)
        (seq (compile-e e c tail?)
             (Jmp return))]
       [(Var x)
        (seq (Push rax)
             (compile-e e (cons x c) tail?)
             (Add rsp 8)
             (Jmp return))]
       [(Lit l)
        (seq (Cmp rax (imm->bits l))
             (Jne next)
             (compile-e e c tail?)
             (Jmp return))]
       [(Sym s)
        (seq (Push rax)
             (compile-symbol s (cons #f c))
             (Pop r8)
             (Cmp rax r8)
             (Mov rax r8)
             (Jne next)
             (compile-e e c tail?)
             (Jmp return))]
       [(Box x)
        (seq (Mov r8 rax)
             (And r8 ptr-mask)
             (Cmp r8 type-box)
             (Jne next)
             (Xor rax type-box)
             (Mov r8 (Offset rax 0))
             (Push r8)
             (compile-e e (cons x c) tail?)
             (Add rsp 8)
             (Jmp return))]
       [(Cons x1 x2)
        (seq (Mov r8 rax)
             (And r8 ptr-mask)
             (Cmp r8 type-cons)
             (Jne next)
             (Xor rax type-cons)
             (Mov r8 (Offset rax 0))
             (Push r8)
             (Mov r8 (Offset rax 8))
             (Push r8)
             (compile-e e (cons x1 (cons x2 c)) tail?)
             (Add rsp 16)
             (Jmp return))])]))

;; CEnv -> Asm
;; Pad the stack to be aligned for a call with stack arguments
(define (pad-stack-call c i)
  (match (odd? (+ (length c) i))
    [#f (seq (Sub rsp 8) (% "padding stack"))]
    [#t (seq)]))

;; CEnv -> Asm
;; Pad the stack to be aligned for a call
(define (pad-stack c)
  (pad-stack-call c 0))

;; CEnv -> Asm
;; Undo the stack alignment after a call
(define (unpad-stack-call c i)
  (match (odd? (+ (length c) i))
    [#f (seq (Add rsp 8) (% "unpadding"))]
    [#t (seq)]))

;; CEnv -> Asm
;; Undo the stack alignment after a call
(define (unpad-stack c)
  (unpad-stack-call c 0))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

(define (assert-type mask type)
  (λ (arg c)
    (seq (Mov r9 arg)
         (And r9 mask)
         (Cmp r9 type)
         (Jne (error-label c)))))

(define (type-pred mask type)
  (let ((l (gensym)))
    (seq (And rax mask)
         (Cmp rax type)
         (Mov rax (imm->bits #t))
         (Je l)
         (Mov rax (imm->bits #f))
         (Label l))))

(define assert-integer
  (assert-type mask-int type-int))
(define assert-char
  (assert-type mask-char type-char))
(define assert-flonum
  (assert-type ptr-mask type-flonum))
(define assert-box
  (assert-type ptr-mask type-box))
(define assert-cons
  (assert-type ptr-mask type-cons))
(define assert-string
  (assert-type ptr-mask type-string))
(define assert-symbol
  (assert-type ptr-mask type-symbol))
(define assert-port
  (assert-type ptr-mask type-port))
(define assert-vector
  (assert-type ptr-mask type-vector))
(define assert-proc
  (assert-type proc-mask type-proc))

(define (assert-codepoint c)
  (let ((ok (gensym)))
    (seq (assert-integer rax c)
         (Cmp rax (imm->bits 0))
         (Jl (error-label c))
         (Cmp rax (imm->bits 1114111))
         (Jg (error-label c))
         (Cmp rax (imm->bits 55295))
         (Jl ok)
         (Cmp rax (imm->bits 57344))
         (Jg ok)
         (Jmp (error-label c))
         (Label ok))))

(define (assert-byte c)
  (seq (assert-integer rax c)
       (Cmp rax (imm->bits 0))
       (Jl (error-label c))
       (Cmp rax (imm->bits 255))
       (Jg (error-label c))))

;; Symbol -> Label
;; Produce a symbol that is a valid Nasm label
(define (symbol->label s)
  (string->symbol
   (string-append
    "label_"
    (list->string
     (map (λ (c)
            (if (or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (memq c '(#\_ #\$ #\# #\@ #\~ #\. #\?)))
                c
                #\_))
         (string->list (symbol->string s))))
    "_"
    (number->string (eq-hash-code s) 16))))
