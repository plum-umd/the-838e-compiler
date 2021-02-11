#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "externs.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define rdx 'rdx) ; return, 2
(define r8  'r8)  ; scratch in +, -, integer-length
(define r9  'r9)  ; scratch in assert-type
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg

;; type CEnv = [Listof Variable]

;; Expr -> Asm
(define (compile p)
  (match p
    [(Prog ds e)  
     (prog (externs p)
           (Extern 'raise_error)
           (Label 'entry)
           (Mov rbx rdi) ; recv heap pointer
           (compile-e e '(#f))
           (Mov rdx rbx) ; return heap pointer in second return register           
           (Ret)
           (compile-defines ds))]
    [(Lib ps ds)
     (seq 
          (compile-provides ps)
          (Extern 'raise_error)
          (compile-defines ds))]))

(define (compile-provides ps)
  (match ps
    ['() (seq)]
    [(cons p ps)
     (seq (Global (symbol->label p))
          (compile-provides ps))]))

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
          (compile-e e (parity (cons #f (reverse xs))))
          (Ret))]))

(define (parity c)
  (if (even? (length c))
      (append c (list #f))
      c))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (seq (match e
         [(Int i)            (compile-value i)]
         [(Bool b)           (compile-value b)]
         [(Char c)           (compile-value c)]
         [(Eof)              (compile-value eof)]
         [(Empty)            (compile-value '())]
         [(Var x)            (compile-variable x c)]
         [(App f es)         (compile-app f es c)]    
         [(Prim0 p)          (compile-prim0 p c)]
         [(Prim1 p e)        (compile-prim1 p e c)]
         [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
         [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
         [(Begin e1 e2)      (compile-begin e1 e2 c)]
         [(Let x e1 e2)      (compile-let x e1 e2 c)])))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (imm->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))       
    (seq (Mov rax (Offset rsp i)))))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (match p
    ['void      (seq (Mov rax val-void))]
    ['read-byte (seq (pad-stack c)
                     (Call 'read_byte)
                     (unpad-stack c))]
    ['peek-byte (seq (pad-stack c)
                     (Call 'peek_byte)
                     (unpad-stack c))]))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
       (match p
         ['add1
          (seq (assert-integer rax)
               (Add rax (imm->bits 1)))]
         ['sub1
          (seq (assert-integer rax)
               (Sub rax (imm->bits 1)))]         
         ['zero?
          (let ((l1 (gensym)))
            (seq (assert-integer rax)
                 (Cmp rax 0)
                 (Mov rax val-true)
                 (Je l1)
                 (Mov rax val-false)
                 (Label l1)))]
         ['integer-length
          (seq (assert-integer rax)
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
          (seq (assert-char rax)
               (Sar rax char-shift)
               (Sal rax int-shift))]
         ['integer->char
          (seq assert-codepoint
               (Sar rax int-shift)
               (Sal rax char-shift)
               (Xor rax type-char))]
         ['eof-object? (eq-imm val-eof)]
         [(or 'char-whitespace? 'char-alphabetic?)
          (let ((l (gensym)))
            (seq (assert-char rax)
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
            (seq (assert-char rax)
                 (pad-stack c)
                 (Sar rax char-shift)
                 (Mov rdi rax)
                 (Call (char-op->uc p))
                 (unpad-stack c)
                 (Sal rax char-shift)
                 (Or rax type-char)))]
         ['write-byte
          (seq assert-byte
               (pad-stack c)
               (Mov rdi rax)
               (Call 'write_byte)
               (unpad-stack c)
               (Mov rax val-void))]
         ['box
          (seq (Mov (Offset rbx 0) rax)
               (Mov rax rbx)
               (Or rax type-box)
               (Add rbx 8))]
         ['unbox
          (seq (assert-box rax)
               (Xor rax type-box)
               (Mov rax (Offset rax 0)))]
         ['car
          (seq (assert-cons rax)
               (Xor rax type-cons)
               (Mov rax (Offset rax 8)))]
         ['cdr
          (seq (assert-cons rax)
               (Xor rax type-cons)
               (Mov rax (Offset rax 0)))]
         ['empty? (eq-imm val-empty)])))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (match p
         ['+
          (seq (Pop r8)
               (assert-integer r8)
               (assert-integer rax)
               (Add rax r8))]
         ['-
          (seq (Pop r8)
               (assert-integer r8)
               (assert-integer rax)
               (Sub r8 rax)
               (Mov rax r8))]
         ['eq?
          (let ((l (gensym)))
            (seq (Cmp rax (Offset rsp 0))
                 (Sub rsp 8)
                 (Mov rax val-true)
                 (Je l)
                 (Mov rax val-false)
                 (Label l)))]
         ['cons
          (seq (Mov (Offset rbx 0) rax)
               (Pop rax)
               (Mov (Offset rbx 8) rax)
               (Mov rax rbx)
               (Or rax type-cons)
               (Add rbx 16))])))

;; Id [Listof Expr] CEnv -> Asm
;; Here's why this code is so gross: you have to align the stack for the call
;; but you have to do it *before* evaluating the arguments es, because you need
;; es's values to be just above 'rsp when the call is made.  But if you push
;; a frame in order to align the call, you've got to compile es in a static
;; environment that accounts for that frame, hence:
(define (compile-app f es c)
  (if (even? (+ (length es) (length c))) 
      (seq (compile-es es c)
           (Call (symbol->label f))
           (Add rsp (* 8 (length es))))            ; pop args
      (seq (Sub rsp 8)                             ; adjust stack
           (compile-es es (cons #f c))
           (Call (symbol->label f))
           (Add rsp (* 8 (add1 (length es)))))))   ; pop args and pad

;; [Listof Expr] CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c)
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

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c)
         (Label l2))))

;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-e e2 c)))

;; Id Expr Expr CEnv -> Asm
(define (compile-let x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

;; CEnv -> Asm
;; Pad the stack to be aligned for a call with stack arguments
(define (pad-stack-call c i)
  (match (even? (+ (length c) i))
    [#f (seq (Sub rsp 8) (% "padding stack"))]
    [#t (seq)]))

;; CEnv -> Asm
;; Pad the stack to be aligned for a call
(define (pad-stack c)
  (pad-stack-call c 0))

;; CEnv -> Asm
;; Undo the stack alignment after a call
(define (unpad-stack-call c i)
  (match (even? (+ (length c) i))
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
  (λ (arg)
    (seq (Mov r9 arg)
         (And r9 mask)
         (Cmp r9 type)
         (Jne 'raise_error))))

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
(define assert-box
  (assert-type ptr-mask type-box))
(define assert-cons
  (assert-type ptr-mask type-cons))

(define assert-codepoint
  (let ((ok (gensym)))
    (seq (assert-integer rax)
         (Cmp rax (imm->bits 0))
         (Jl 'raise_error)
         (Cmp rax (imm->bits 1114111))
         (Jg 'raise_error)
         (Cmp rax (imm->bits 55295))
         (Jl ok)
         (Cmp rax (imm->bits 57344))
         (Jg ok)
         (Jmp 'raise_error)
         (Label ok))))
       
(define assert-byte
  (seq (assert-integer rax)
       (Cmp rax (imm->bits 0))
       (Jl 'raise_error)
       (Cmp rax (imm->bits 255))
       (Jg 'raise_error)))
       
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
