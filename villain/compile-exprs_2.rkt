#lang racket
(provide (all-defined-out))
(require "ast.rkt" "parse.rkt" "types.rkt" "externs.rkt"
         "compile-intmd-utils_2.rkt"
         a86/ast
         a86/printer (submod a86/printer private))

;; Registers used
(define rax 'rax) ; return  ; the dividend of div in string-ref and string-set!
(define rbx 'rbx) ; heap
(define rdx 'rdx) ; return, 2  ; remainder of division and scratch in string-ref
                               ; and string-set! ; arg3
                               ; scratch in compile-tail-applyL and compile-nontail-applyL
(define r8  'r8)  ; scratch in +, -, compile-chars, compile-prim2, string-ref,
                  ; make-string, compile-prim3, string-ref!, integer-length, match,
                  ; compile-define, open-input-file, integer?, compile-λ,
                  ; copy-env-to-heap, copy-closure-env-to-stack, compile-tail-applyL
                  ; compile-nontail-applyL, compile-letrec-λs, compile-letrec-init
(define r9  'r9)  ; scratch in assert-type, compile-str-chars, string-ref,
                  ; string-set!, make-string, compile-define, fl<=
                  ; compile-vector, vector-set!, vector-ref, compile-bignum
                  ; add1, sub1, integer?, integer-length, +, -, assert-integer/bignum
                  ; compile-λ, copy-env-to-heap, copy-closure-env-to-stack
                  ; compile-tail-applyL, compile-letrec-init
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg
(define rsi 'rsi) ; arg2
(define r10 'r10) ; scratch in compile-prim3, make-string, string-set!, compile-vector, vector-set!
                  ; compile-define, fl<=, compile-bignum, integer?, integer-length, assert-integer/bignum
                  ; compile-λ-definition, reposition-env-vars
(define rcx 'rcx) ; arity indicator
(define al  'al)  ; low byte of rax ; open-input-file
(define xmm0 'xmm0) ; registers to hold double precision floating numbers

;; type CEnv = [Listof Variable]
  
;; Expr CEnv Boolean -> Asm
  (define (compile-e e c tail?)
    (let ((e+
           (match e
             [(Int i)            (compile-value i)]
             [(Bool b)           (compile-value b)]
             [(Char c)           (compile-value c)]
             [(Flonum f)         (compile-flonum f)]
             [(Bignum i)         (compile-bignum i c)]
             [(Eof)              (compile-value eof)]
             [(Empty)            (compile-value '())]
             [(String s)         (compile-string s)]
             [(Symbol s)         (compile-symbol s c)]
             [(Vec ds)           (compile-vector ds c)]
             [(Var x)            (compile-variable x c)]
             [(LCall e es)       (compile-call e es c tail?)]
             [(Apply e0 e1)      (compile-applyL e0 e1 c tail?)]
             [(Prim0 p)          (compile-prim0 p c)]
             [(Prim1 p e)        (compile-prim1 p e c)]
             [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
             [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)]
             [(Mps p rest)       (compile-mps p rest c)]
             [(If e1 e2 e3)      (compile-if e1 e2 e3 c tail?)]
             [(Begin e1 e2)      (compile-begin e1 e2 c tail?)]
             [(Let x e1 e2)      (compile-let x e1 e2 c tail?)]
             [(Letrec xs es e)   (compile-letrec xs es e c tail?)]
             [(Lam l xs e0)      (compile-λ l (fvs e) c)]
             [(Lam* l xs xs* e0) (compile-λ l (fvs e) c)]
             [(Prog sts ds e) (compile-e e c tail?)]
             [(Match e0 cs)      (compile-match e0 cs c tail?)])))
      (compile-intmd-to-a86 e+ c)))

(define (compile-e-tail e c)
  (compile-e e c #t))
(define (compile-e-nontail e c)
  (compile-e e c #f))

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
       (Mov r8 type-proc)
       (Or rax r8)
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
       (Push rax)                    ; rax contains the pointer to λ closure
       (compile-es es (cons #f c))
       (Mov rax (Offset rsp (* 8 (length es))))
       (assert-proc rax c)
       (Mov r8 type-proc)
       (Xor rax r8)
       (%% "move args for tail call")
       (move-args (add1 (length c)) (length es))
       (Add rsp (* 8 (add1 (length c))))
       (copy-closure-env-to-stack)
       (Mov rcx (imm->bits (length es)))
       (Mov rdx (Offset rax 0))
       (Jmp rdx)))      ; (Offset rax 0) contains the address of the label of λ


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
             (Push rax)               ; rax contains the pointer to λ closure
             (Lea r8 ret)
             (Push r8)
             (compile-es es (cons #f (cons #f c)))
             (Mov rax (Offset rsp (* 8 (add1 (length es)))))
             (assert-proc rax c)
             (Mov r8 type-proc)
             (Xor rax r8)
             (copy-closure-env-to-stack)
             (Mov rcx (imm->bits (length es)))
             (Mov rdx (Offset rax 0))
             (Jmp rdx)   ; (Offset rax 0) contains the address of the label of λ
             (Label ret)
             (Add rsp 8))         ; pop the pointer to λ closure off the stack
        (seq (%% "compile-nontail-call")
             (Sub rsp 8)
             (compile-e-nontail f (cons #f c))
             (Push rax)
             (Lea r8 ret)
             (Push r8)
             (compile-es es (cons #f (cons #f (cons #f c))))
             (Mov rax (Offset rsp (* 8 (add1 (length es)))))
             (assert-proc rax c)
             (Mov r8 type-proc)
             (Xor rax r8)
             (copy-closure-env-to-stack)
             (Mov rcx (imm->bits (length es)))
             (Mov rdx (Offset rax 0))
             (Jmp rdx)  ; (Offset rax 0)
             (Label ret)
             (Add rsp 16)))))   

(define (copy-closure-env-to-stack)
  (let ((loop (gensym 'copy_closure))
        (done (gensym 'copy_done)))
    (seq (%% "copy-closure-env-to-stack")
         (Mov r8 (Offset rax 8))    ; number of env vars
         (Mov r9 rax)               
         (Add r9 16)                ; start of env
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
(define (compile-bignum i c)
  (let ((length (ceiling (/ (integer-length (abs i)) 64)))
        (sign (if (>= i 0) 1 -1)))
    (seq (Mov r9 (imm->bits (* sign length)))
         (Mov (Offset rbx 0) r9)          ;; write length in word 0
         (compile-bignum-words (bignum->list (abs i)) 1)
         (Mov rax rbx)                    ;; bignum is on heap
         (Add rbx (* 8 (add1 length)))
         (Mov r10 type-bignum)
         (Or rax r10))))   

;; Integer -> (Listof Integers)
;; Breaks an integer down into 64 bit chunks
;; input should always be positive
(define (bignum->list i)
  (if (< i (arithmetic-shift 1 64))
    (list i) 
    (cons (bitwise-and i (sub1 (arithmetic-shift 1 64))) 
          (bignum->list (arithmetic-shift i -64)))))

;; (Listof Integers) Integer -> Asm
;; Takes list of 64-bit integers and places them on the heap
;; Note: least significant 64-bit integers are placed first
(define (compile-bignum-words ws n)
  (match ws
    ['()  (seq)]
    [(cons w ws)
     (seq (Mov r9 w)
          (Mov (Offset rbx (* 8 n)) r9)
          (compile-bignum-words ws (add1 n)))]))

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

;; LExpr LExpr CEnv Boolean -> Asm
(define (compile-applyL e0 e1 c tail?)
  (if tail?
      (compile-tail-applyL e0 e1 c)
      (compile-nontail-applyL e0 e1 c)))

;; LExpr LExpr CEnv -> Asm
(define (compile-tail-applyL e0 e1 c)
  (let ((loop (gensym 'loop))
        (done (gensym 'done)))
    (seq (%% "compile-tail-applyL ")
         (compile-e-nontail e0 c)       
         (Push rax)                 ; rax has the pointer to λ closure
         (compile-e-nontail e1 (cons #f c))
         (list->stack c)            ; copy arg list elements to stack         
         (Sar rcx (- int-shift 3))  ; rcx = # of elements * 8
         (Mov rdx rsp)               
         (Add rdx rcx)           ; rdx points to where ptr to λ is on the stack          
         (Mov rax (Offset rdx 0))
         (assert-proc rax c)
         (Mov r8 type-proc)
         (Xor rax r8)
    
         (%% "move args for applyL tail call")
         (Mov r8 (* 8 (length c)))
         (Mov r9 rdx)
         (Add r9 r8)               ; r9 points to start of C environment
         (Label loop)
         (Cmp rdx rsp)
         (Je done)                 
         (Sub rdx 8)               ; rdx points to first element of arg list
         (Mov r8 (Offset rdx 0))
         (Mov (Offset r9 0) r8)              ; move up element of arg list on stack.
         (Sub r9 8)                
         (Jmp loop)
         (Label done)   
         (Add rsp (* 8 (add1 (length c))))
         (copy-closure-env-to-stack)
         (Sal rcx (- int-shift 3))  ; rcx communicates # of args to callee
         (Mov rdx (Offset rax 0))
         (Jmp rdx))))    ; (Offset rax 0) contains the address of the λ label

(define (compile-nontail-applyL e0 e1 c)
  (let ((ret (gensym 'ret)))
        ;(display "length of C: ") (display (length c)) (display c) (display e0) (display e1)
        (seq (%% "compile nontail applyL")
            ; (pad-stack c)             
             (Lea r8 ret)
             (Push r8)
             (compile-e-nontail e0 (cons #f c))
             (Push rax)                ; rax contains the pointer to λ closure
             (compile-e-nontail e1 (cons #f (cons #f c)))
             (list->stack (cons #f (cons #f c))) ; copy arg list elements to stack         
             (Sar rcx (- int-shift 3))  ; rcx = # of elements * 8
             (Mov rdx rsp)               
             (Add rdx rcx)        ; rdx points to where ptr to λ is on the stack          
             (Mov rax (Offset rdx 0))
             (assert-proc rax (cons #f (cons #f (cons #f c))))
             (Mov r8 type-proc)
             (Xor rax r8)
             (Mov r8 (Offset rdx 8))   ; copy ret label address to one word 
             (Mov (Offset rdx 0) r8)   ; down on the stack to where ptr to λ is
             (copy-closure-env-to-stack)
             (Sal rcx (- int-shift 3))  ; rcx communicates # of args to callee
             (Mov rdx (Offset rax 0))
             (Jmp rdx)    ; (Offset rax 0) contains the address of the λ label
             (Label ret)
             (Add rsp 8))))   ; pop the initial copy of ret label address off the stack
         ;    (unpad-stack c))))

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
    ['read-byte (seq (ICall 'read_byte))]
    ['peek-byte (seq (ICall 'peek_byte))]
    ['read-char (seq (ICall 'read_char))]
    ['peek-char (seq (ICall 'peek_char))]
    ['gensym    (seq (ICall 'gensym)
                     (Or rax type-symbol))]))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e-nontail e c)
       (match p
         ['add1
          (let ((end (gensym))
                (pos-bignum (gensym)))
            (seq (assert-integer/bignum rax c)
               (Mov rdi rax)
               (Mov rsi rbx)
               (Mov rdx 1)
               (ICall 'add_or_sub1)
               (Mov r9 rax)         ; first check if return value is fixnum
               (And r9 mask-int)
               (Xor r9 type-int)
               (Cmp r9 0)
               (Je end)             ; if not fixnum, we should adjust rbx
               (Mov r9 (Offset rbx 0))
               (Cmp r9 -1)
               (Jg pos-bignum)      ; get absolute value of length
               (Mov r8 0)
               (Sub r8 r9)
               (Mov r9 r8)
               (Label pos-bignum)
               (Sar r9 (- int-shift imm-shift))
               (Add rbx r9)
               (Label end)))]
         ['sub1
          (let ((end (gensym))
                (pos-bignum (gensym)))
            (seq (assert-integer/bignum rax c)
               (Mov rdi rax)
               (Mov rsi rbx)
               (Mov rdx -1)
               (ICall 'add_or_sub1)
               (Mov r9 rax)         ; first check if return value is fixnum
               (And r9 mask-int)
               (Xor r9 type-int)
               (Cmp r9 0)
               (Je end)             ; if not fixnum, we should adjust rbx
               (Mov r9 (Offset rbx 0))
               (Cmp r9 -1)
               (Jg pos-bignum)      ; get absolute value of length
               (Mov r8 0)
               (Sub r8 r9)
               (Mov r9 r8)
               (Label pos-bignum)
               (Sar r9 (- int-shift imm-shift))
               (Add rbx r9)
               (Label end)))]       
         ['zero?
          (let ((l1 (gensym)))
            (seq (assert-integer/bignum rax c)
                 (Cmp rax 0)
                 (Mov rax val-true)
                 (Je l1)
                 (Mov rax val-false)
                 (Label l1)))]
         ['integer?
          (let ((l1 (gensym)))
            (seq (Mov r8 val-true)    ; preemptively store true as result
                 (Mov r9 rax)         ; first check if it's an integer
                 (And r9 mask-int)
                 (Xor r9 type-int)
                 (Cmp r9 0)
                 (Je l1)
                 (Mov r9 rax)         ; if not integer, check if bignum
                 (Mov r10 ptr-mask)
                 (And r9 r10)
                 (Mov r10 type-bignum)
                 (Xor r9 r10)
                 (Cmp r9 0)
                 (Je l1)
                 (Mov r8 val-false)   ; if neither, this line stores false as result
                 (Label l1)           ; move result into rax
                 (Mov rax r8)))]
         ['integer-length
          (let ((fixnum-length (gensym))
                (end (gensym)))
            (seq (assert-integer/bignum rax c)
                (Mov r9 rax)                ; first check if it's an integer
                (And r9 mask-int)
                (Xor r9 type-int)
                (Cmp r9 0)
                (Je fixnum-length)
                (Mov r10 type-bignum)
                (Xor rax r10)        ; take out tag
                (Mov rdi rax)
                (ICall 'bignum_length)
                (Jmp end)            ; jump to end, avoid integer-length for fixnum branch
                (Label fixnum-length)
                (Sar rax imm-shift)   ; if integer, take absolute value and get most significant bit
                (Mov r8 rax)
                (Sar r8 63)
                (Xor rax r8)
                (Bsr rax rax)
                (Sal rax int-shift)
                (Label end)))] 
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
                 (Sar rax char-shift)
                 (Mov rdi rax)
                 (ICall (char-op->uc p))
                 (Cmp rax 0)
                 (Mov rax val-true)
                 (Jne l)
                 (Mov rax val-false)
                 (Label l)))]
         [(or 'char-upcase 'char-downcase 'char-titlecase)
          (let ((l (gensym)))
            (seq (assert-char rax c)
                 (Sar rax char-shift)
                 (Mov rdi rax)
                 (ICall (char-op->uc p))
                 (Sal rax char-shift)
                 (Or rax type-char)))]
         ['write-byte
          (seq (assert-byte c)
               (Mov rdi rax)
               (ICall 'write_byte)
               (Mov rax val-void))]
         ['write-char
          (seq (assert-char rax c)
               (Mov rdi rax)
               (ICall 'write_char)
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
               (Mov rdi rax)
               (ICall 'str_to_symbol)
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
             (Mov rdi rax)
             (ICall 'open_input_file)
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
             (Mov rdi rax)
             (ICall 'close_input_port)
             (Mov rax val-void))]
         ['read-byte
          (seq
            (assert-port rax c)
            (Mov rdi rax)
            (ICall 'read_byte_port)
            )]
         ['peek-byte
         (seq
           (assert-port rax c)
           (Mov rdi rax)
           (ICall 'peek_byte_port)
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
          (let ((end (gensym))
                (pos-bignum (gensym))) 
           (seq (Pop r8)
                (assert-integer/bignum r8 c)
                (assert-integer/bignum rax c)
                (Mov rdi r8)
                (Mov rsi rax)
                (Mov rdx rbx)
                (ICall 'integer_add)
                (Mov r9 rax)         ; first check if return value is fixnum
                (And r9 mask-int)
                (Xor r9 type-int)
                (Cmp r9 0)
                (Je end)             ; if not fixnum, we should adjust rbx
                (Mov r9 (Offset rbx 0))
                (Cmp r9 -1)
                (Jg pos-bignum)      ; get absolute value of length
                (Mov r8 0)
                (Sub r8 r9)
                (Mov r9 r8)
                (Label pos-bignum)
                (Sar r9 (- int-shift imm-shift))
                (Add rbx r9)
                (Label end)))]
         ['-
          (let ((end (gensym))
                (pos-bignum (gensym))) 
           (seq (Pop r8)
                (assert-integer/bignum r8 c)
                (assert-integer/bignum rax c)
                (Mov rdi r8)
                (Mov rsi rax)
                (Mov rdx rbx)
                (ICall 'integer_sub)
                (Mov r9 rax)         ; first check if return value is fixnum
                (And r9 mask-int)
                (Xor r9 type-int)
                (Cmp r9 0)
                (Je end)             ; if not fixnum, we should adjust rbx
                (Mov r9 (Offset rbx 0))
                (Cmp r9 -1)
                (Jg pos-bignum)      ; get absolute value of length
                (Mov r8 0)
                (Sub r8 r9)
                (Mov r9 r8)
                (Label pos-bignum)
                (Sar r9 (- int-shift imm-shift))
                (Add rbx r9)
                (Label end)))]
         ['quotient
          (let ((end (gensym))
                (pos-bignum (gensym))) 
            (seq (Pop r8)
               (assert-integer/bignum r8 c)
               (assert-integer/bignum rax c)
               (Cmp rax (imm->bits 0)) ; error out if divisor is 0
               (Je 'err)
               (Mov rdi r8)
               (Mov rsi rax)
               (Mov rdx rbx)
               (ICall 'integer_quotient)
               (Mov r9 rax)         ; first check if return value is fixnum
               (And r9 mask-int)
               (Xor r9 type-int)
               (Cmp r9 0)
               (Je end)             ; if not fixnum, we should adjust rbx
               (Mov r9 (Offset rbx 0))
               (Cmp r9 -1)
               (Jg pos-bignum)      ; get absolute value of length
               (Mov r8 0)
               (Sub r8 r9)
               (Mov r9 r8)
               (Label pos-bignum)
               (Sar r9 (- int-shift imm-shift))
               (Add rbx r9)
               (Label end)))]
         ['remainder 
          (let ((end (gensym))
                (pos-bignum (gensym))) 
            (seq (Pop r8)
               (assert-integer/bignum r8 c)
               (assert-integer/bignum rax c)
               (Cmp rax (imm->bits 0)) ; error out if divisor is 0
               (Je 'err)
               (Mov rdi r8)
               (Mov rsi rax)
               (Mov rdx rbx)
               (ICall 'integer_remainder)
               (Mov r9 rax)         ; first check if return value is fixnum
               (And r9 mask-int)
               (Xor r9 type-int)
               (Cmp r9 0)
               (Je end)             ; if not fixnum, we should adjust rbx
               (Mov r9 (Offset rbx 0))
               (Cmp r9 -1)
               (Jg pos-bignum)      ; get absolute value of length
               (Mov r8 0)
               (Sub r8 r9)
               (Mov r9 r8)
               (Label pos-bignum)
               (Sar r9 (- int-shift imm-shift))
               (Add rbx r9)
               (Label end)))]
         ['> 
            (seq (Pop r8)
                 (assert-integer/bignum r8 c)
                 (assert-integer/bignum rax c)
                 (Mov rdi r8)
                 (Mov rsi rax)
                 (ICall 'integer_g)
                 )]
         ['< 
            (seq (Pop r8)
                 (assert-integer/bignum r8 c)
                 (assert-integer/bignum rax c)
                 (Mov rdi r8)
                 (Mov rsi rax)
                 (ICall 'integer_l)
                 )]
         ['<=
            (seq (Pop r8)
                 (assert-integer/bignum r8 c)
                 (assert-integer/bignum rax c)
                 (Mov rdi r8)
                 (Mov rsi rax)
                 (ICall 'integer_leq)
                 )]
         ['>=
            (seq (Pop r8)
                 (assert-integer/bignum r8 c)
                 (assert-integer/bignum rax c)
                 (Mov rdi r8)
                 (Mov rsi rax)
                 (ICall 'integer_geq)
                 )]
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
                 (Jl 'err)
                 (Xor r8 type-string)
                 (Mov r9 (Offset r8 0))       ; r9 = length
                 (Sub r9 (imm->bits 1))       ; 0-indexing
                 (Cmp rax r9)
                 (Jg 'err)
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
                 (Jl 'err)
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
               (Jl 'err)
               (Xor r8 type-vector)
               (Mov r9 (Offset r8 0))       ; r9 = length
               (Add r8 8)                   ; r8 will now be pointing to the first element
               (Sub r9 1)                   ; 0-indexing
               (Cmp rax r9)
               (Jg 'err)
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
                 (Jl 'err)
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
               (Movsd xmm0 (Offset r8 0))
               (Addsd xmm0 (Offset rax 0))              
               (Movsd (Offset rbx 0) xmm0)
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
               (Movsd xmm0 (Offset r8 0))
               (Subsd xmm0 (Offset rax 0))               
               (Movsd (Offset rbx 0) xmm0)
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
                 (Jl 'err)
                 (Sub r9 (imm->bits 1))
                 (Cmp r10 r9)
                 (Jg 'err)
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
              (Jl 'err)
              (Xor r8 type-vector)
              (Mov r9 (Offset r8 0))       ; r9 = length
              (Add r8 8)                   ; r8 will now be pointing to the first element
              (Sub r9 1)                   ; 0-indexing
              (Cmp r10 r9)
              (Jg 'err)
              (Sal r10 3)                  ; index*=8
              (Add r8 r10)
              (Mov (Offset r8 0) rax)
              (Mov rax val-void))])))

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

;; [Listof Expr] CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e-nontail e c)
          (Push rax)
          (compile-es es (cons #f c)))]))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

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
     (let ((label (if (Lam? l)
                      (Lam-l l)
                      (if (Lam*? l)
                          (Lam*-l l)
                          (error "a right-hand-side in letrec not λ")))))
     ;  (display l) (display "  fvs l:  ") (display (fvs l)) (display "\n\n")
       (let ((length-ys (length (fvs l))))
         (seq (Lea rax label)
              (Mov (Offset rbx 0) rax)
              (Mov r8 length-ys)
              (Mov (Offset rbx 8) r8)
              (Mov rax rbx)
              (Mov r8 type-proc)
              (Or rax r8)
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
            (Mov r8 type-proc)
            (Xor r9 r8)
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
    ['() (seq (Jmp 'err))]
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
             (Mov r9 ptr-mask)
             (And r8 r9)
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
             (Mov r9 ptr-mask)
             (And r8 r9)
             (Cmp r8 type-cons)
             (Jne next)
             (Xor rax type-cons)
             (Mov r8 (Offset rax 0))
             (Push r8)
             (Mov r8 (Offset rax 8))
             (Push r8)
             (compile-e e (cons x1 (cons x2 c)) tail?)
             (Add rsp 16)
             (Jmp return))]
       [(Strct k xs)
        (let ((loop (gensym "loop"))
              (end (gensym "end")))
          (seq           
           ;;Check if it is a prefab structure
           (Mov r8 rax)
           (Mov r9 ptr-mask)
           (And r8 r9)
           (Mov r9 type-prefab)
           (Cmp r8 r9)
           (Jne next)

           ;;Check if the key of the struct is the same as the key in the pattern
           (Xor rax r9)
           (Push rax)
           (compile-symbol k (cons #f c))
           (Mov r8 rax)
           (Pop rax)
           (Mov r9 (Offset rax 0))
           (Mov r10 type-prefab)
           (Or rax r10)
           (Cmp r8 r9)
           (Jne next)

           ;;Check if the number of fields in the pattern is the same as the number of fields of the struct
           (Mov r10 type-prefab)
           (Xor rax r10)
           (Mov r8 (length xs))
           (Mov r9 (Offset rax 8))
           (Mov r10 type-prefab)
           (Or rax r10)
           (Cmp r8 r9)
           (Jne next)

           ;;Place the fields on the stack
           (Mov r10 type-prefab)
           (Xor rax r10)
           (Mov r9 (Offset rax 8))
           (Add rax 16)

           (Label loop)
           (Cmp r9 0)
           (Jle end)
           (Mov r8 (Offset rax 0))
           (Push r8)
           (Sub r9 1)
           (Add rax 8)
           (Jmp loop)
           
           (Label end)
           (compile-e e (append (reverse xs) c) tail?)
           (Add rsp (* 8 (length xs)))
           (Jmp return)))])]))

(define (extract-struct-labels sts)
  (match sts
    ['() '()]
    [(cons (Struct s xs) sts)
     (let ((constructor-label s)
           (predicate-label (string->symbol (string-append (symbol->string s) "?")))
           (accessor-labels (map (λ (a) (string->symbol (string-append (symbol->string s) "-" (symbol->string a)))) xs)))
       (append (extract-struct-labels sts) (reverse accessor-labels) (list predicate-label) (list constructor-label)))]))

(define (compile-struct-functions sts)
  (match sts
    ['() (seq)]
    [(cons (Struct s xs) sts)
     (let ((constructor-label (symbol->label s))
           (predicate-label (symbol->label (string->symbol (string-append (symbol->string s) "?"))))
           (accessor-labels (map (λ (a) (symbol->label (string->symbol (string-append (symbol->string s) "-" (symbol->string a))))) xs)))
       (let ((length-fv-constructor 0)
             (length-fv-predicate 0)
             (length-fv-accessors 0))
         (seq (Lea rax constructor-label)
              (Mov (Offset rbx 0) rax)
              (Mov r8 length-fv-constructor)
              (Mov (Offset rbx 8) r8)
              (Mov rax rbx)
              (Mov r8 type-proc)
              (Or rax r8)
              (Add rbx (* 8 (+ 2 length-fv-constructor)))
              (Push rax)

              (Lea rax predicate-label)
              (Mov (Offset rbx 0) rax)
              (Mov r8 length-fv-predicate)
              (Mov (Offset rbx 8) r8)
              (Mov rax rbx)
              (Mov r8 type-proc)
              (Or rax r8)
              (Add rbx (* 8 (+ 2 length-fv-predicate)))
              (Push rax)

              (compile-struct-accessors-labels accessor-labels length-fv-accessors)
              
              (compile-struct-functions sts))))]))

(define (compile-struct-accessors-labels labels length-fv)
  (match labels
    ['() (seq)]
    [(cons h labels)
     (seq
      (Lea rax h)
      (Mov (Offset rbx 0) rax)
      (Mov r8 length-fv)
      (Mov (Offset rbx 8) r8)
      (Mov rax rbx)
      (Mov r8 type-proc)
      (Or rax r8)
      (Add rbx (* 8 (+ 2 length-fv)))
      (Push rax)
      (compile-struct-accessors-labels labels length-fv))]))


(define (compile-struct-functions-init sts c)
  (match sts
    ['() '()]
    [(cons (Struct s xs) sts)
     (let ((constructor-label s)
           (predicate-label (string->symbol (string-append (symbol->string s) "?")))
           (accessor-labels (map (λ (a) (string->symbol (string-append (symbol->string s) "-" (symbol->string a)))) xs)))
       (seq (Mov r9 (Offset rsp (lookup constructor-label c)))
            (Mov r8 type-proc)
            (Xor r9 r8)
            (Add r9 16)
            (copy-env-to-heap '() c 0)

            (Mov r9 (Offset rsp (lookup predicate-label c)))
            (Mov r8 type-proc)
            (Xor r9 r8)
            (Add r9 16)
            (copy-env-to-heap '() c 0)


            (compile-struct-functions-accessors-init accessor-labels c)
            (compile-struct-functions-init sts c)))]))

(define (compile-struct-functions-accessors-init labels c)
  (match labels
    ['() (seq)]
    [(cons l labels)
     (seq
      (Mov r9 (Offset rsp (lookup l c)))
      (Mov r8 type-proc)
      (Xor r9 r8)
      (Add r9 16)
      (copy-env-to-heap '() c 0)
      (compile-struct-functions-accessors-init labels c))]))

;;(Letrec (Lisof Id) (Listof Lambda) (Prog (Listof Struct) '() Expr)) -> (Listof Struct)
(define (structs p)
  (match p
    [(Letrec names bodies (Prog sts '() rest)) sts ]
    [_ '()]))

;;[Listof Struct] -> Asm
(define (compile-structs sts)
  (match sts
    ['() (seq)]
    [(cons s sts)
     (seq (compile-struct s)
          (compile-structs sts))]))

;;Struct -> Asm
(define (compile-struct s)
  (match s
    [(Struct s xs)
     (let ((loop (gensym 'loop))
           (end (gensym 'end))
           (end2 (gensym 'end)))
       (seq
        (%% "Struct Constructor Function")
        (Label (symbol->label s)) 
        (Cmp rcx (imm->bits (length xs))) ; arity check
        (Jne (error-label (reverse xs)))
        (compile-e (Symbol s) (reverse xs) #f)
        (Mov r8 (length xs))
        (Mov (Offset rbx 8) r8);;Store the number of fields
        (Mov (Offset rbx 0) rax) ;;Store the key i.e the symbol

        (Mov r8 rbx)
        (Mov r9 rsp)
        (Add r9 (* 8 (sub1 (length xs))))
        (Mov r10 0)

        (Add r8 16) ;;Skip the number of fields and the key
            
        (Label loop)
        (Cmp r10 (length xs))
        (Je end) ;;No more args
        (Mov rax (Offset r9 0))
        (Mov (Offset r8 0) rax)
        (Add r8 8)
        (Sub r9 8)
        (Add r10 1)
        (Jmp loop)

        (Label end)
        (Mov rax rbx)
        (Mov r8 type-prefab)
        (Or rax r8)
        (Add rbx (* 8 (+ 2 (length xs))))
        ; return
        ;;(Pop r8) ; save rp
        (Add rsp (* 8 (length xs))) ; pop args
        ;;(Push r8) ; replace rp
        (Ret)
            
        (%% "Struct Predicate")
        (Label (symbol->label (string->symbol (string-append (symbol->string s) "?"))))
        (Cmp rcx (imm->bits 1)) ;arity check
        (Jne (error-label (list #f)))
        (Mov r8 (Offset rsp 0)) ;;Get the argument
        (Mov r9 ptr-mask)
        (And r8 r9)
        (Mov r9 type-prefab)
        (Cmp r8 r9)
        (Mov rax val-false)
        (Jne end2)
        (compile-e (Symbol s) (parity (list #f #f)) #f)
        (Mov r8 (Offset rsp 0))
        (Mov r9 type-prefab)
        (Xor r8 r9)
        (Mov r9 (Offset r8 0))
        (Cmp r9 rax)
        (Mov rax val-false)
        (Jne end2)
        (Mov r9 (Offset r8 8))
        (Cmp r9 (length xs))
        (Jne end2)
        (Mov rax val-true)
            
        (Label end2)
        ; return
        ;;(Pop r8) ; save rp
        (Add rsp 8) ; pop args
        ;;(Push r8) ; replace rp
        (Ret)

        (compile-struct-accessors s xs 0)))]))

;;Symbol [Listof Symbol] -> Asm
(define (compile-struct-accessors s xs i)
  (match xs
    ['() (seq)]
    [(cons x rest)
     (let ((end (gensym 'end))
           (accessor (string-append (symbol->string s) "-" (symbol->string x))))
       (seq
        (%% (string-append "Struct accessor for " accessor))
        (Label (symbol->label (string->symbol accessor)))
        (Cmp rcx (imm->bits 1)) ;arity check
        (Jne (error-label (list #f)))
        (Mov r8 (Offset rsp 0)) ;;Get the argument
        (Mov r9 ptr-mask)
        (And r8 r9)
        (Mov r9 type-prefab)
        (Cmp r8 r9)
        (Jne (error-label (list #f #f)))
        (compile-e (Symbol s) (parity (list #f #f)) #f)
        (Mov r8 (Offset rsp 0))
        (Mov r9 type-prefab)
        (Xor r8 r9)
        (Mov r9 (Offset r8 0))
        (Cmp r9 rax)
        (Jne (error-label (list #f #f)))
        (Mov r9 (Offset r8 8))
        (Cmp r9 (+ i (length xs)))
        (Jne (error-label (list #f #f)))
        (Mov rax (Offset r8 (* 8 (+ 2 i))))

        (Label end)
         ; return
        ;;(Pop r8) ; save rp
        (Add rsp 8) ; pop args
        ;;(Push r8) ; replace rp
        (Ret)

        (compile-struct-accessors s rest (add1 i))))]))

(define (parity c)
  (if (even? (length c))
      (append c (list #f))
      c))

;;Prefab-Key [Listof Expr] CEnv -> Asm
(define (compile-mps prefab-key rest c)
  (match prefab-key
     [(Prefab-Key s n1 (list n2 v2) muts)
      (let ((buildPrefab (gensym "buildPrefab"))
            (end (gensym "end")))
        (seq
         (compile-prefab-key prefab-key c) ;;Place all the prefab key values on the stack
         (compile-prefab-values rest (cons #f c)) ;;Place all the struct values on the stack
         
         (Mov r8 (length rest))
         (Mov (Offset rbx 0) r8) ;;The number of fields
         
         (Mov rax (* 8 (+ 1 (length rest))))

         (Label buildPrefab)
         (Pop r8)
         (Mov r9 rbx)
         (Add r9 rax)
         (Mov (Offset r9 0) r8)
         (Sub rax 8)
         (Cmp rax 0)
         (Je end)
         (Jmp buildPrefab)
         
         (Label end)

         ;;Swap the placement of the number of fields and the keys
         (Mov r8 (Offset rbx 8)) ;;The key
         (Mov r9 (Offset rbx 0)) ;;The number of fields

         (Mov (Offset rbx 0) r8)
         (Mov (Offset rbx 8) r9)

         ;;Return a pointer to the struct
         (Mov rax rbx)
         (Mov r8 type-prefab) 
         (Or rax r8) ;;If type-prefab is not moved into a 64-bit register, then it is not treated as a 64-bit value in this or
         (Add rbx (* 8 (+ 2 (length rest))))))]))

;;CEnv integer -> CEnv
;;Given a compile time environment and a size, add size number of #f to the environment.
(define (extend c size)
  (if (> size 0)
      (cons #f (extend c (- size 1)))
      c))
    
;;Prefab-Key CEnv -> Asm
(define (compile-prefab-key prefab-key c)
  (match prefab-key
    [(Prefab-Key s n1 (list n2 v2) muts)
     (seq
      (compile-e s c #f)
      (Push rax)
      ;;Ignore the rest of the key for now
      #|(compile-e n1 (cons #f c))
      (Push rax)
      (compile-e n2 (cons #f (cons #f c)))
      (Push rax)
      (compile-e v2 (cons #f (cons #f (cons #f c))))
      (Push rax)
              
      (compile-prefab-muts muts (cons #f (cons #f (cons #f (cons #f c))))|#)]))

;;[Listof Integer] CEnv -> Asm
(define (compile-prefab-muts muts c)
  (match muts
    [(cons (Int i) muts)
     (seq (compile-e (Int i) c #f)
          (Push rax)
          (compile-prefab-muts muts (cons #f c)))]
    ['() (seq)]))


(define (compile-prefab-values values c)
  (match values
    [(cons e1 values)
     (seq (compile-e e1 c #f)
          (Push rax)
          (compile-prefab-values values (cons #f c)))]
    ['() (seq)]))

(define (assert-type mask type)
  (λ (arg c)
    (seq (Push rdx)
         (Push r9)
         (Mov r9 arg)
         (Mov rdx mask)
         (And r9 rdx)
         (Mov rdx type)
         (Cmp r9 rdx)
         (Pop r9)
         (Pop rdx)
         (Jne 'err))))

(define (type-pred mask type)
  (let ((l (gensym)))
    (seq (Push rdx)
         (Mov rdx mask)
         (And rax rdx)
         (Mov rdx type)
         (Cmp rax rdx)
         (Mov rax (imm->bits #t))
         (Je l)
         (Mov rax (imm->bits #f))
         (Label l)
         (Pop rdx))))

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
  (assert-type ptr-mask type-proc))

(define assert-integer/bignum
  (λ (arg c)
    (let ((ok (gensym "intorbig")))
    (seq (Mov r9 arg)       ; first check if integer
         (And r9 mask-int)
         (Cmp r9 type-int)
         (Je ok)
         (Mov r9 arg)       ; then check if bignum
         (Mov r10 ptr-mask)
         (And r9 r10)
         (Mov r10 type-bignum)
         (Cmp r9 r10)
         (Je ok)
         (Jmp 'err)
         (Label ok)))))

(define (assert-codepoint c)
  (let ((ok (gensym)))
    (seq (assert-integer rax c)
         (Cmp rax (imm->bits 0))
         (Jl 'err)
         (Cmp rax (imm->bits 1114111))
         (Jg 'err)
         (Cmp rax (imm->bits 55295))
         (Jl ok)
         (Cmp rax (imm->bits 57344))
         (Jg ok)
         (Jmp 'err)
         (Label ok))))

(define (assert-byte c)
  (seq (assert-integer rax c)
       (Cmp rax (imm->bits 0))
       (Jl 'err)
       (Cmp rax (imm->bits 255))
       (Jg 'err)))

;; LExpr -> (Listof Id)
(define (fvs e)
 (begin
  (define (fvs e)
    (match e
      [(Int i)            '()]
      [(Bool b)           '()]
      [(Char c)           '()]
      [(Flonum f)         '()]
      [(Bignum b)         '()]
      [(Eof)              '()]
      [(Empty)            '()]
      [(String s)         '()]
      [(Symbol s)         '()]
      [(? symbol? s)      '()]  ;; for a library function label in (Apply f e)
      [(Vec ds)           '()]
      [(Var x)            (list x)]
      [(LCall e es)       (append (fvs e) (apply append (map fvs es)))]
      [(Apply f e)        (append (fvs f) (fvs e))]
      [(Prim0 p)          '()]
      [(Prim1 p e)        (fvs e)]
      [(Prim2 p e1 e2)    (append (fvs e1) (fvs e2))]
      [(Prim3 p e1 e2 e3) (append (fvs e1) (fvs e2) (fvs e3))]
      [(Mps p rest)       (apply append (map fvs rest))]
      [(If e1 e2 e3)      (append (fvs e1) (fvs e2) (fvs e3))]
      [(Begin e1 e2)      (append (fvs e1) (fvs e2))]
      [(Let x e1 e2)      (append (apply append (map fvs e1))
                                (remq* x (fvs e2)))]
      [(Letrec xs es e)   (remq* xs (apply append (fvs e) (map fvs es)))]
      [(Lam l xs e)       (remq* xs (fvs e))]
      [(Lam* l xs xs* e)  (remq* (cons xs* xs) (fvs e))]
      [(Prog sts ds e) (fvs e)]
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
    [(Bignum b)         '()]
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
    [(Mps p rest)       (apply append (map λs rest))]
    [(If e1 e2 e3)      (append (λs e1) (λs e2) (λs e3))]
    [(Begin e1 e2)      (append (λs e1) (λs e2))]
    [(Let x e1 e2)      (append (apply append (map λs e1)) (λs e2))]
    [(Letrec xs es e)   (append (apply append (map λs es)) (λs e))]
    [(Lam  l xs e0)     (cons e (λs e0))]
    [(Lam* l xs xs* e0) (cons e (λs e0))]
    [(Prog sts ds e) (λs e)]
    [(Match e0 cs)
     (append (λs e0) (apply append (map (λ (c) (λs (Clause-e c))) cs)))]))  

