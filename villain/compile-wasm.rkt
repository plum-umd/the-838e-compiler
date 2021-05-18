#lang racket
(provide (all-defined-out))
(require "ast.rkt" "parse.rkt" "wtypes.rkt" "../wasm/printer.rkt")

(define word-size 4)  ;; in bytes

;; Note about addressing and type tags:

;; Our current word-size for compilation to WebAssembly is 4 bytes (32 bits),
;; and current heap addresses can potentially go from 0 to 2^26. But in order to
;; avoid overwriting the user stack which grows downward from the address 2^26 - 4,
;; we should not allow the heap addresses to go beyond 2^25 and also should not
;; allow the stack address to go below 2^25.

;; So, bits 1 and 2 of heap addresses from the right (LSB) are 0, and the
;; component of addresses in bits 3 to 25 can go from 0 to 2^23. So, bits 1, 2,
;; and 26 to 31 of addresses can be used as tag bits. We currently use bits 1,
;; 2, 29, 30, and 31 as tag bits (please see wtypes.rkt). Bit 32 is the sign bit.

;; We have a user stack in memory that grows downward from the highest address of
;; our memory.  This is different from the implicit operand stack that the stack
;; machine of WebAssembly uses.  The stack that we refer to should be clear from
;; the context. Explicitly, we use stack in memory (or mem.) or user stack to refer
;; to the stack we have in memory, and the operand (or op.) stack to refer to the
;; operand stack of WebAssembly.

;; $b is used as scratch register in compile-string, string-ref, and make string.
;; As such, it should not be used in compile-string-chars and assert-type.

;; Expr -> Wasm
(define (compile e)
  (match e
    [(Letrec fs ls e)
     (let ((main-prog-fn
               `(func $sendResult (result i32)
                  (local $a i32)         ;; local var (used as virtual register)
                  (local $b i32)         ;; virtual register
                  (local $c i32)         ;; virtual register
                  ,@(compile-e e '())))) 
       `(module
          (import "writeBytejs" "writeByte" (func $writeByte (param i32)))
          (import "readBytejs" "readByte" (func $readByte (result i32)))
          (import "peekBytejs" "peekByte" (func $peekByte (result i32)))
          (import "errorjs" "error" (func $error))
          (memory 1024)  ;; 1024 pages of memory = 2^26 bytes 
          (export "memory" (memory 0))
          (global $sp (mut i32) (i32.const 67108860))  ;; global variable $sp is
                ;; the stack ptr initially pointing to the top address in memory 0.
          (global $hp (mut i32) (i32.const 0))         ;; global variable $hp is
                           ;; the heap pointer with start addr of 0 in memory 0.
            ,main-prog-fn
            ,@(compile-defines fs ls)
          (export "sendResult" (func $sendResult))
          ))]))

;; Expr CEnv -> Wasm
(define (compile-e e c)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Empty)            (compile-value '())]
    [(String s)         (compile-string s)]
    [(Prim0 p)          (compile-prim0 p)]
    [(Prim1 p e)        (compile-prim1 p e c)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)      (compile-begin e1 e2 c)]
    [(Let x e1 e2)      (compile-let x e1 e2 c)]
    [(LCall e es)       (compile-call (symbol->wa-label (Var-x e)) es c)]
    [(Var x)            (compile-variable x c)]
     ))

;; Value -> Wasm
(define (compile-value v)
  `(i32.const ,(imm->bits v)))

;; Op0 -> Wasm
(define (compile-prim0 p)
  (match p
    ['void  `(i32.const ,val-void)]
    ['read-byte
     `(call $readByte)]
    ['peek-byte
     `(call $peekByte)]))  

;; Op1 Expr CEnv -> Wasm
(define (compile-prim1 p e c)
  (append (compile-e e c)
  (match p
    ['add1
     (append (assert-integer)
     `(i32.const ,(imm->bits 1)
       i32.add))]
    ['sub1
     (append (assert-integer)
     `(i32.const ,(imm->bits 1)
       i32.sub))]
    ['zero?
     (append (assert-integer)
     `(if 
       i32.const ,val-false
       local.set $a
       else    
       i32.const ,val-true
       local.set $a
       end
       local.get $a))]
    ['integer?
     `(i32.const ,mask-int
       i32.and
       i32.const ,type-int
       i32.xor
       if
       i32.const ,val-false
       local.set $a
       else    
       i32.const ,val-true
       local.set $a
       end
       local.get $a)]
    ['integer-length
     (append (assert-integer)
     `(i32.const ,int-shift
       i32.shr_s          
       local.set $a
       local.get $a
       i32.const 31
       i32.shr_s
       local.get $a
       i32.xor
       i32.clz
       local.set $a
       i32.const 32
       local.get $a
       i32.sub
       i32.const ,int-shift
       i32.shl))]
    ['char?
     `(i32.const ,mask-char
       i32.and
       i32.const ,type-char
       i32.xor
       if
       i32.const ,val-false
       local.set $a
       else    
       i32.const ,val-true
       local.set $a
       end
       local.get $a)]
    ['char->integer
     (append (assert-char)
     `(i32.const ,char-shift
       i32.shr_s
       i32.const ,int-shift
       i32.shl))]
    ['integer->char
     (append assert-codepoint
     `(i32.const ,int-shift
       i32.shr_s
       i32.const ,char-shift
       i32.shl
       i32.const ,type-char
       i32.xor))]
    ['eof-object?
     (eq-imm val-eof)]
    ['write-byte
     (append assert-byte
     `(call $writeByte
       i32.const ,val-void))]
    ['box
     `(local.set $a          ;; Store result of (compile-e e c) on heap.
       global.get $hp
       local.get $a
       i32.store              
       global.get $hp         ;; tag the heap ptr
       i32.const ,type-box    
       i32.or                 
       local.set $a
       global.get $hp         ;; Advance heap ptr by word size in bytes.
       i32.const ,word-size
       i32.add                
       global.set $hp          
       local.get $a)]        ;; return the tagged ptr
    ['unbox
     (append (assert-box)
     `(i32.const ,type-box
       i32.xor
       i32.load))]
    ['car
     (append (assert-cons)
     `(i32.const ,type-cons  ;; get the address of pair from the tagged ptr
       i32.xor               
       i32.const ,word-size  ;; get the address of car (in higher address)
       i32.add               
       i32.load))]           ;; get the car
    ['cdr
     `(i32.const ,type-cons  ;; get the address of pair from the tagged ptr
       i32.xor                             
       i32.load)]            ;; get the cdr (in lower word of the pair)
    ['empty? (eq-imm val-empty)]
    ['string?
     (type-pred ptr-mask type-string)]
    ['string-length
     (append (assert-string)
     `(i32.const ,type-string
       i32.xor
       i32.load))]
    
)))

;; Op2 Expr Expr CEnv -> Wasm
(define (compile-prim2 p e1 e2 c)
  (append
   `(,@(compile-e e1 c)
     local.set $a
     global.get $sp
     i32.const ,(* word-size (length c))
     i32.sub
     local.get $a
     i32.store)       
   (match p
     ['+
      `(,@(compile-e e2 (cons #f c))
        local.set $a
        global.get $sp
        i32.const ,(* word-size (length c))
        i32.sub
        i32.load
        local.get $a       
        i32.add)]
     ['-
      `(,@(compile-e e2 (cons #f c))
        local.set $a
        global.get $sp
        i32.const ,(* word-size (length c))
        i32.sub
        i32.load
        local.get $a       
        i32.sub)]
     ['eq?
      `(,@(compile-e e2 (cons #f c))
        local.set $a         
        global.get $sp
        i32.const ,(* word-size (length c))
        i32.sub
        i32.load
        local.get $a
        i32.eq
        if
        i32.const ,val-true
        local.set $a
        else
        i32.const ,val-false
        local.set $a
        end
        local.get $a)]
     ['cons
      `(,@(compile-e e2 (cons #f c))
        local.set $a              ;; store cdr in heap
        global.get $hp
        local.get $a
        i32.store
        global.get $hp             ;; tag the heap ptr
        i32.const ,type-cons    
        i32.or                 
        local.set $b              ;; local var $b contains the reutrn result
        global.get $hp             ;; Advance heap ptr by word size in bytes.
        i32.const ,word-size
        i32.add                
        global.set $hp          
        global.get $sp             ;; get the car from operand stack
        i32.const ,(* word-size (length c))
        i32.sub
        i32.load               
        local.set $a
        global.get $hp             ;; store car in the advanced heap ptr
        local.get $a
        i32.store
        global.get $hp             ;; Advance heap ptr by word size in bytes.
        i32.const ,word-size
        i32.add                
        global.set $hp
        local.get $b)]            ;; return the result
     ['string-ref
      `(,@(compile-e e2 (cons #f c))
        ,@(assert-integer)
        local.set $b              ;; $ b = the integer of index
        global.get $sp           ;; get the tagged str ptr from the user stack 
        i32.const ,(* word-size (length c))
        i32.sub
        i32.load
        ,@(assert-string)         ;; $b should not be used in assert-type
        i32.const ,type-string
        i32.xor
        local.set $a              ;; $a = the start address of string on heap
        local.get $a              ;; get the length of string from str_addr[0]
        i32.load        
        local.set $c              ;; $c = the length of the str
        local.get $b              ;; call $error if the int parameter < 0
        i32.const ,(imm->bits 0)
        i32.lt_s
        if
        call $error
        end
        local.get $b              ;; call $error if the int parameter >= str len
        local.get $c
        i32.ge_s
        if
        call $error
        end
        local.get $a              ;; put str_addr on the operand stack
        local.get $b              ;; Add 1 to the index, as length is at index 0
        i32.const ,(imm->bits 1)
        i32.add          
        i32.const ,int-shift
        i32.shl                   ;; shift the index to the left by int-shift
        i32.const ,word-size
        i32.shr_s                 ;; then shift it to the right by word-size
        i32.add                   ;; add this to the str_addr
        i32.load)]                ;; get str_addr[index] = char of str at index
     ['make-string
      `(,@(compile-e e2 (cons #f c))
        ,@(assert-char)
        local.set $b              ;; $ b = the char parameter
        global.get $sp       ;; get the first parameter (integer) from the user stack.
        i32.const ,(* word-size (length c))
        i32.sub
        i32.load
        ,@(assert-integer)        ;; $b should not be used in assert-type
        local.set $a              ;; $a = the int parameter
        local.get $a
        i32.const ,(imm->bits 0)  ;; call $error if the int parameter < 0 
        i32.lt_s
        if
        call $error
        end
        global.get $hp             ;; put the heap ptr on the operand stack.
        local.get $a
        i32.store               ;; store the length on the heap at str_addr = $hp
        global.get $hp           ;; create the tagged ptr to the str
        i32.const ,type-string
        i32.or
        local.set $c         ;; $ c = the tagged ptr to the start of str on heap
        block               
        loop                      ;; start of the loop
        global.get $hp             ;; advance the heap pointer by the word-size
        i32.const ,word-size
        i32.add
        global.set $hp
        local.get $a            ;; the counter starts from the length and counts
                                ;; down by (imm->bits 1). At the same time, in
                                ;; each iteration, the heap ptr is increased by
                                ;; word-size (≡ i++, where i is the index of the
                                ;; word of str on the heap)
        i32.eqz                 ;; check if counter = 0
        br_if 1                 ;; if so, break to the outer block (with index 1)
        global.get $hp
        local.get $b
        i32.store                 ;; store the char in str_addr[i]
        local.get $a
        i32.const ,(imm->bits 1)  ;; decrement the counter
        i32.sub
        local.set $a
        br 0                      ;; continue with the loop (with index 0)
        end                       ;; end of the loop (with index 0)
        end                       ;; end of the outer block (with index 0)
        local.get $c)]            ;; return the tagged ptr to the start of str
     )))


;; Expr Expr Expr CEnv -> Wasm
(define (compile-if e1 e2 e3 c)
  `(,@(compile-e e1 c)
    i32.const ,val-false
    i32.eq
    if
    ,@(compile-e e3 c)
    local.set $a
    else
    ,@(compile-e e2 c)
    local.set $a
    end
    local.get $a))

;; Expr Expr CEnv -> Wasm
(define (compile-begin e1 e2 c)
  (append (compile-e e1 c)
          `(drop)
          (compile-e e2 c)))

;; Imm -> Wasm 
(define (eq-imm imm)
  `(i32.const ,imm
    i32.eq
    if
    i32.const ,val-true
    local.set $a
    else    
    i32.const ,val-false
    local.set $a
    end
    local.get $a))

;; (Listof Id) (Listof Expr) Expr CEnv -> Wasm
(define (compile-let xs es e c)
  (append (compile-es es c)
          (compile-e e (append (reverse xs) c))))
;;          (local.set $a
;;           i32.const ,(* word-size (length xs))
;;           global.get $sp
;;           i32.add
;;           global.set $sp      ;; consider changing the user stack ptr
;;           local.get $a)))
                     

;; [Listof Expr] CEnv -> Wasm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (append (compile-e e c)
     `(local.set $a
       global.get $sp          
       i32.const ,(* word-size (length c))
       i32.sub
;;     i32.set $sp     ;; consider decrementing the user stack ptr (akin to push)
;;     i32.get $sp
       local.get $a
       i32.store)
      (compile-es es (cons #f c)))]))

;; Id CEnv -> Integer
(define (lookup x c len-c)
  (match c
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t (* word-size (sub1 len-c))]   ;; consider changing the user stack ptr
       [#f (- (lookup x rest len-c) word-size)])]))

;; Id CEnv -> Wasm
(define (compile-variable x c)
  (let ((i (lookup x c (length c))))
    `(global.get $sp    ;; the local var for top address of start of user stack.
      i32.const ,i
      i32.sub          ;; top address - (word-size * ((length c) -  offset))
      i32.load)))

;; String -> Wasm
(define (compile-string s)  
  (let ((len (string-length s)))
    `(global.get $hp                 ;; store length of string on heap 
      i32.const ,(imm->bits len)
      i32.store
      global.get $hp                 ;; store tagged heap pointer in $b
      i32.const ,type-string
      i32.or
      local.set $b        
      global.get $hp                 ;; advance heap pointer
      i32.const ,word-size
      i32.add
      global.set $hp
      ,@(compile-string-chars (string->list s))
      local.get $b)))
   
;; [Listof Char] Nat -> Wasm
(define (compile-string-chars cs)   ;; $b should not be used in this function
  (match cs
    ['() '()]
    [(cons c cs)
     `(global.get $hp               ;; store char on heap
       i32.const ,(imm->bits c)
       i32.store 
       global.get $hp                 ;; advance heap pointer
       i32.const ,word-size
       i32.add
       global.set $hp
       ,@(compile-string-chars cs))]))

;; Integer Integer -> (() -> Wasm)
(define (assert-type mask type)
  (λ ()      
  `(local.set $a           ;; $b should not be used in this function              
    local.get $a           ;; due to use in string-ref 
    i32.const ,mask
    i32.and
    i32.const ,type
    i32.ne
    if
    call $error
    end
    local.get $a)))

;; Integer Integer -> Wasm
(define (type-pred mask type)
  `(i32.const ,mask
    i32.and
    i32.const ,type
    i32.eq
    if
    i32.const ,val-true
    local.set $a
    else
    i32.const ,val-false
    local.set $a
    end
    local.get $a))

;; Integer Integer -> (() -> Wasm)
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

;; Wasm
(define assert-byte
  (append (assert-integer)
   `(local.set $a
     local.get $a
     i32.const ,(imm->bits 0)
     i32.lt_s
     if
     call $error
     end
     local.get $a
     i32.const ,(imm->bits 255)
     i32.gt_s
     if
     call $error
     end
     local.get $a)))

;; Wasm
(define assert-codepoint
  (append (assert-integer)
   `(local.set $a
     local.get $a
     i32.const ,(imm->bits 0)
     i32.lt_s
     if
     call $error
     end
     local.get $a
     i32.const ,(imm->bits 1114111)
     i32.gt_s
     if
     call $error
     end
     local.get $a
     i32.const ,(imm->bits 55296)
     i32.ge_s
     if
     local.get $a
     i32.const ,(imm->bits 57343)
     i32.le_s
     if
     call $error
     end
     end
     local.get $a)))

(define (compile-call f es c)
  (let ((h (* word-size (length c))))
   `(,@(compile-es es c)     ;; put the arguments on the user stack
     global.get $sp          ;; change stack ptr to point to top of the user stack
     i32.const ,h            ;; (to the addr after C env, where first arg to f is)
     i32.sub
     global.set $sp
     call ,f
     global.get $sp
     i32.const ,h
     i32.add
     global.set $sp)))       ;; return the stack ptr to where it was before the call 

(define (compile-defines fs ls)
  (match* (fs ls)
    [('() '()) '()] 
    [((cons f fs) (cons (Lam l xs e) ls))
     `(,(compile-define f xs e)
       ,@(compile-defines fs ls))]))

(define (compile-define f xs e)
  (let ((wa-f (symbol->wa-label f)))
    `(func ,wa-f (result i32)
       (local $a i32)         ;; local var (used as virtual register)
       (local $b i32)         ;; virtual register
       (local $c i32)         ;; virtual register
       ,@(compile-e e (reverse xs)))))

;; Symbol -> Wasm Identifier
;; Produce a symbol that is a valid Wasm symbolic identifier. (Based on section
;; 6.3.5 of WebAssembly Specification, Release 1.1. The characters `'| are also
;; valid, but are excluded here because of use in Racket.)
;; 
(define (symbol->wa-label s)
  (string->symbol
   (string-append
    "$label_"
    (list->string
     (map (λ (c)
            (if (or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (memq c '(#\_ #\$ #\# #\@ #\~ #\. #\? #\! #\% #\& #\^
                              #\* #\+ #\- #\/ #\: #\< #\= #\> #\\)))
                c
                #\_))
         (string->list (symbol->string s))))
    "_"
    (number->string (eq-hash-code s) 16))))       
