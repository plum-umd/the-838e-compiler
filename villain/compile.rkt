#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "externs.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return  ; the dividend of div in string-ref and string-set!
(define rbx 'rbx) ; heap
(define rdx 'rdx) ; return, 2  ; remainder of division and scratch in string-ref
                               ; and string-set!
(define r8  'r8)  ; scratch in +, -, compile-chars, compile-prim2, string-ref,
                  ; make-string, compile-prim3, string-ref!, integer-length, match, 
                  ; compile-define, open-input-file
(define r9  'r9)  ; scratch in assert-type, compile-str-chars, string-ref,
                  ; string-set!, make-string, compile-define
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg
(define rsi 'rsi) ; arg2
(define r10 'r10) ; scratch in compile-prim3, make-string, string-set!, compile-define
(define rcx 'rcx) ; arity indicator
(define al  'al)  ; low byte of rax ; open-input-file

;; type CEnv = [Listof Variable]

;; Expr -> Asm
(define (compile p)
  (match p
    [(Prog ds e)
     (prog (Global 'entry)
           (Default 'rel)
           (Section '.text)
           (externs p)
           (Extern 'raise_error)
           (Global 'raise_error_align)
           (Extern 'str_to_symbol)
           (Label 'entry)
           (Mov rbx rdi) ; recv heap pointer
           (compile-e e '(#f))
           (Mov rdx rbx) ; return heap pointer in second return register           
           (Ret)
           (compile-defines ds)
           (Label 'raise_error_align)
           (Sub rsp 8)
           (Jmp 'raise_error))]))

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

;; [Listof Id] -> Asm
(define (compile-provides xs)
  (match xs
    ['()
     (seq)]
    [(cons x xs)
     (seq (Global (symbol->label x))
          (compile-provides xs))]))

(define (error-label c)
  (if (odd? (length c))
      'raise_error
      'raise_error_align))

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
          (compile-e e (parity (cons #f (reverse xs))))
          ; return
          (Pop r8) ; save rp
          (Add rsp (* 8 (length xs))) ; pop args
          (Push r8) ; replace rp
          (Ret))]
    [(Defn* f xs xs* e) 
     (let ((loop (gensym 'loop))
           (end (gensym 'end)))

       (seq (Label (symbol->label f))
            (Cmp rcx (imm->bits (length xs)))
            (Jl 'raise_error)
            (Pop r10)                         ; store return address
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
            (Push r10) ; reinstall return address
            (compile-e e (parity (cons #f (cons xs* (reverse xs)))))
            ; return
            (Pop r10)  ; save rp
            (Add rsp (* 8 (add1 (length xs)))) ; pop args
            (Push r10) ; replace rp
            (Ret)))]))

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
         [(String s)         (compile-string s)]      
         [(Symbol s)         (compile-symbol s c)]
         [(Var x)            (compile-variable x c)]
         [(App f es)         (compile-app f es c)]    
         [(Prim0 p)          (compile-prim0 p c)]
         [(Prim1 p e)        (compile-prim1 p e c)]
         [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
         [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)]  
         [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
         [(Begin e1 e2)      (compile-begin e1 e2 c)]
         [(Let x e1 e2)      (compile-let x e1 e2 c)]
         [(Match e0 cs)      (compile-match e0 cs c)])))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (imm->bits v))))

;; String -> Asm
(define (compile-string s)
  (let ((length (string-length s)))
    (seq (Mov r9 (imm->bits length))
         (Mov (Offset rbx 0) r9)         ;; write length in word 0
         (Mov r9 0)
         (compile-str-chars (string->list s) 3 0 1)
         (Mov rax rbx)
         (Or rax type-string)
         (Add rbx (* 8 (add1 (ceiling (/ length 3))))))))         

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
;; Transform
;;   'foo
;; into
;;   (string->symbol "foo")
;; then compile it
(define (compile-symbol s c)
  (seq (compile-e (Prim1 'string->symbol
                         (String (symbol->string s)))
        c)))
  
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
                     (unpad-stack c))]
    ['gensym    (seq (pad-stack c)
                     (Call 'gensym)
                     (unpad-stack c)
                     (Or rax type-symbol))]))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
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

             ;; Save the heap pointer as second argument for c function call
             (Mov rsi rbx)
             ;; TODO: this could be stack allocated
             ;; Allocate a buffer on the heap for the c-string
             (Xor rax type-string)
             ;; r8 <- chars in input string
             (Mov r8 (Offset rax 0))
             ;; (r8 * 4) + 1 is upper bound on bytes
             (Sar r8 int-shift)
             (Sal r8 2)
             (Add r8 1)
             ;; Align heap
             (Or r8 7)
             (Add r8 1)
             (Add rbx r8)

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
             (Or rax type-port)
             )]
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
           )])))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
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
               (Add rbx 16))])))

;; Op3 Expr Expr Expr CEnv -> Asm

(define (compile-prim3 p e1 e2 e3 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (Push rax)
       (compile-e e3 (cons #f (cons #f c)))
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
                 (Mov rax val-void)))])))                   

;; Id [Listof Expr] CEnv -> Asm
;; Here's why this code is so gross: you have to align the stack for the call
;; but you have to do it *before* evaluating the arguments es, because you need
;; es's values to be just above 'rsp when the call is made.  But if you push
;; a frame in order to align the call, you've got to compile es in a static
;; environment that accounts for that frame, hence:
(define (compile-app f es c)
  (if (even? (+ (length es) (length c))) 
      (seq (compile-es es c) 
           (Mov rcx (imm->bits (length es)))
           (Call (symbol->label f)))            ; pop args
      (seq (Sub rsp 8)                          ; adjust stack
           (compile-es es (cons #f c))
           (Mov rcx (imm->bits (length es)))
           (Call (symbol->label f))
           (Add rsp 8))))

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

;; Expr [Listof Clause] CEnv-> Asm
(define (compile-match e0 cs c)
  (let ((return (gensym 'matchreturn)))
    (seq (compile-e e0 c)
         (compile-match-clauses cs return c)
         (Label return))))

;; [Listof Clauses] Symbol CEnv -> Asm
(define (compile-match-clauses cs return c)
  (match cs
    ['() (seq (Jmp (error-label c)))]
    [(cons cl cs)
     (let ((next (gensym 'matchclause)))
       (seq (compile-match-clause cl next return c)
            (Label next)
            (compile-match-clauses cs return c)))]))

;; Clause Symbol Symbol CEnv -> Asm
(define (compile-match-clause cl next return c)
  (match cl
    [(Clause p e)
     (match p
       [(Wild)
        (seq (compile-e e c)
             (Jmp return))]
       [(Var x)
        (seq (Push rax)
             (compile-e e (cons x c))
             (Add rsp 8)
             (Jmp return))]
       [(Lit l)
        (seq (Cmp rax (imm->bits l))
             (Jne next)
             (compile-e e c)
             (Jmp return))]
       [(Sym s)
        (seq (Push rax)
             (compile-symbol s (cons #f c))
             (Pop r8)
             (Cmp rax r8)
             (Jne next)
             (compile-e e c)
             (Jmp return))]       
       [(Box x)
        (seq (Mov r8 rax)
             (And r8 ptr-mask)
             (Cmp r8 type-box)
             (Jne next)
             (Xor rax type-box)
             (Mov r8 (Offset rax 0))
             (Push r8)
             (compile-e e (cons x c))
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
             (compile-e e (cons x1 (cons x2 c)))
             (Add rsp 16)
             (Jmp return))])]))

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
