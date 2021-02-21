#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define rdx 'rdx) ; return, 2
(define r8  'r8)  ; scratch in +, -, integer-length
(define r9  'r9)  ; scratch in assert-type
(define rcx  'rcx)  ; scratch in fl+
(define r10  'r10)  ; scratch in fl+
(define r11  'r11)  ; scratch in fl+
(define r12  'r12)  ; scratch in fl+
(define r13  'r13)  ; scratch in fl+
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg

;; type CEnv = [Listof Variable]

;; Expr -> Asm
(define (compile p)
  (match p
    [(Prog ds e)  
     (prog (Extern 'peek_byte)
           (Extern 'read_byte)
           (Extern 'write_byte)
           (Extern 'raise_error)
           (Label 'entry)
           (Mov rbx rdi) ; recv heap pointer
           (compile-e e '(#f))
           (Mov rdx rbx) ; return heap pointer in second return register           
           (Ret)
           (compile-defines ds))]))

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
         [(Float f)          (compile-value f)]
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
               (Add rbx 16))]

         ['fl+ (seq
               ;; (assert-float r8)
               ;;(assert-float rax)
               (compile-fl) )

          ]

          ['fl-
           (seq
               ;; (assert-float r8)
               ;;(assert-float rax)
                
               ;; flips the sign of e2 since e1 - e2 = e1 + -e2
               (Xor rax (arithmetic-shift 1 (+ float-shift 31)))
               (compile-fl))
          ]
         
             )
         ))

(define (compile-fl)
  (let ((l1 (gensym)) (l2 (gensym)) (l3 (gensym)) (l4 (gensym))
                              (l5 (gensym)) (l6 (gensym)) (l7 (gensym)))
            
       
          (seq (Pop r8)
              

               ;; e1 (the bigger float) will be contained in r8
               ;; and e2 will be contained in rax
               (Mov rbx rax)
               (And rbx (- (arithmetic-shift 1 (+ float-shift 31)) 1))
               (Mov rcx r8)
               (And rcx (- (arithmetic-shift 1 (+ float-shift 31)) 1))
               (Cmp rbx rcx)
               (Jl l1)
               (Mov rbx rax)
               (Mov rax r8)
               (Mov r8 rbx)
               (Label l1)
               
               

               ;; e2's mantissa with a 1 to the left of it
               ;; stored in rcx
               (Mov rcx rax)
               (Sar rcx float-shift)
               (And rcx (- (arithmetic-shift 1 23) 1))
               (Add rcx (arithmetic-shift 1 23))
               
               ;; e1's mantissa with a 1 to the left of it
               ;; stored in r10
               (Mov r10 r8)
               (Sar r10 float-shift)
               (And r10 (- (arithmetic-shift 1 23) 1))
               (Add r10 (arithmetic-shift 1 23))
               
               ;; e2's expt
               ;; stored in rdx
               (Mov rdx rax)
               (Sar rdx (+ float-shift 23))
               (And rdx (- (arithmetic-shift 1 8) 1))

               ;; e1's expt
               ;; stored in r11
               (Mov r11 r8)
               (Sar r11 (+ float-shift 23))
               (And r11 (- (arithmetic-shift 1 8) 1))
              

               ;; makes e2's expt the same size of e1
               ;; and shifts e2's mantissa in this loop
               (Label l2)
               (Cmp rdx r11)
               (Je l3)
               (Sar rcx 1)
               (Add rdx 1)
               (Jmp l2)
               (Label l3)

               ;; e2's sign. stored in rbx
               (Mov rbx rax)
               (Sal rbx (+ float-shift 31))
               (And rbx 1)
               
               ;; e1's sign. stored in r9
               (Mov r9 r8)
               (Sar r9 (+ float-shift 31))
               (And r9 1)

                ;; if e1 and e2 are different signs, m1 - m2,
               (Cmp rbx r9)
               (Je l4)
               (Sub r10 rcx)

                ;; if mantissa does not start with 1, sub exponent by 1,
               ;; shift mantissa left by 1 and loop
               (Label l6)
               (Mov r12 (arithmetic-shift 1 23))
               (Mov r13 r10)
               (And r13 r12)
               (Cmp r12 r13)
               (Je l5)
               (Sal r10 1)
               (Sub r11 1)
               (Jmp l6)
               
               ;; else m1 + m2
               (Label l4)
               (Add r10 rcx)

               ;; if mantissa oveflows, add exponent by 1,
               ;; shift mantissa right by 1 and round
               (Mov r12 (- (arithmetic-shift 1 24) 1))
               (Cmp r12 r10)
               (Jg l5)
               (Add r11 1)
               (Mov r12 r10)
               (Sar r10 1) 
               (And r12 1)
               (Mov r13 0)
               (Cmp r12 r13)
               (Je l5)
               (Add r10 1)

               (Label l5)

               ;; adjusts the mantissa
               (Sub r10 (arithmetic-shift 1 23) )
               
               ;; e2's decimal place. stored in r13
               (Mov r13 rax)
               (Sar r13 (+ float-shift 32))

               ;; e1's decimal place. stored in r12
               (Mov r12 r8)
               (Sar r12 (+ float-shift 32))

               ;; if e2's decimal place > e1's decimal place, switch
               (Cmp r13 r12)
               (Jl l7)
               (Mov r12 r13)
               
               ;; Construction of the final result
               (Label l7)
               (Mov rax 0)
               
               (Add rax r12)
               (Sal rax 1)
               (Add rax r9)
               (Sal rax 8)
               (Add rax r11)
               (Sal rax 23)
               (Add rax r10)
               (Sal rax float-shift)
               (Add rax type-float)
         
               
               
      )))

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
(define assert-float
  (assert-type mask-float type-float))

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

