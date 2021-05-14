#lang racket
(provide (all-defined-out))
(require "ast.rkt" "parse.rkt" "types.rkt" "externs.rkt"
         "compile-exprs_2.rkt"
         a86/ast
         racket/serialize
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

;; (Letrec (Lisof Id) (Listof Lambda) Expr) -> Asm
(define (compile p)  
   (let ((in-fs (open-input-file "./lib-fs"))
        (in-letrec (open-input-file "./libraries-letrec"))
        (in-ls-ids (open-input-file "./lib-ls-ids"))
        (in-lib-exts (open-input-file "./lib-externs")))
    (let ((fs (read in-fs))
          (compiled-lib-letrec (deserialize (read in-letrec)))
          (ls-ids (deserialize (read in-ls-ids)))
          (libexts (deserialize (read in-lib-exts)))
          (sts (structs p)))                           
      (begin
        (close-input-port in-fs)
        (close-input-port in-letrec)
        (close-input-port in-ls-ids)
        (close-input-port in-lib-exts)
        (let ((lib-ls-ids-exts (seq (map (λ (id) (Extern id)) ls-ids)))
              (struct-labels (extract-struct-labels sts)))
          (prog (Global 'entry)
                (Default 'rel)
                (Section '.text)
                (Extern 'raise_error)
                (Global 'raise_error_align)
                (Extern 'str_to_symbol)
                (bignum-externs)
                lib-ls-ids-exts ;; externs of library lambda defs
                (externs p)
                (apply seq libexts)    ;; externs in library lambdas
                (Label 'entry)
                (Mov rbx rdi) ; recv heap pointer
                (%% "start-of-compiled-library-functions-letrec-fs-ls")
                (apply seq compiled-lib-letrec)
                (%% "end-of-compilation-of-library-functions-letrec-fs-ls")

                (%% "start-of-compilation-of-struct-function-closures")
                (compile-struct-functions sts)
                (compile-struct-functions-init sts (append struct-labels (reverse fs)))
                (%% "end-of-compilation-of-struct-function-closures")
                
                (%% "start-of-compilation-of-program")
                ;;(compile-e-tail p (reverse fs))
                (compile-e-tail p (append struct-labels (reverse fs)))
                ;;(Add rsp (* 8 (length fs)))
                (Add rsp (* 8 (+ (length struct-labels) (length fs))))       ;; to pop lib fs and struct labels off the stack
                (%% "end-of-compilation-of-program")
                (Mov rdx rbx) ; return heap pointer in second return register
                (Ret)
                (%% "start-of-compiling-struct-definitions-of-program")
                (compile-structs sts)
                (%% "end-of-compilating-struct-definitions-of-program")
                (%% "start-of-compiling-λ-definitions-of-program")
                (compile-λ-definitions (λs p))
                (%% "end-of-compiling-λ-definitions-of-program")
                (Label 'raise_error_align)
                (Sub rsp 8)
                (Jmp 'raise_error)))))))

(define (libraries-fs-ls)
  (let ((bs stdlib-fs-ls))
    (let ((fs (map car bs)) (ls (map cdr bs)))
      (cons fs ls))))
  
(define (compile-library-letrec fs ls)
  (begin
    (with-output-to-file "lib-fs"
      #:exists 'truncate
      (λ ()
        (displayln fs)))
    (let ((out (open-output-file "lib-ls-ids" #:exists 'truncate)))
      (begin
        (write (serialize (λs-labels ls)) out)
        (close-output-port out)))
    (let ((out (open-output-file "lib-externs" #:exists 'truncate)))
      (begin
        (write (serialize (externs-es ls)) out)
        (close-output-port out)))
    (seq
     (compile-e-tail (Letrec fs ls #s(Int 0)) '())
     ; to reverse poping lib fs off the stack
     (Sub rsp (* 8 (length fs))))))

(define (compile-library-λdefs ls)
  (prog (compile-module-provides ls)
        (Default 'rel)
        (Section '.text)
        (externs-es ls)
        (Extern 'raise_error)
        (Extern 'raise_error_align)
        (Extern 'str_to_symbol)
        (bignum-externs)
        (compile-λ-definitions (apply append (map λs ls)))))

(define (λs-labels ls)
  (match ls
    ['()  '()]
    [(cons l ls)
     (cons (if (Lam? l)
               (Lam-l l)
               (if (Lam*? l)
                   (Lam*-l l)
                   (error "error in λ-ids \n")))
           (λs-labels ls))]))

;; -> Asm
(define (bignum-externs)
  (seq (Extern 'bignum_length)
       (Extern 'add_or_sub1)
       (Extern 'integer_g)
       (Extern 'integer_geq)
       (Extern 'integer_leq)
       (Extern 'integer_l)
       (Extern 'integer_add)
       (Extern 'integer_sub)
       (Extern 'integer_quotient)
       (Extern 'integer_remainder)))

;; [Listof Id] -> Asm
(define (compile-module-provides ls)
  (match ls
    ['()
     (seq)]
    [(cons l ls)
     (seq (Global (if (Lam? l)
                      (Lam-l l)
                      (if (Lam*? l)
                          (Lam*-l l)
                          (error "error in compile--module-provides"))))
          (compile-module-provides ls))]))

(define (compile-module-externs ls)
  (match ls
    ['()  (seq)]
    [(cons l ls)
     (seq (Extern (if (Lam? l)
                      (Lam-l l)
                      (if (Lam*? l)
                          (Lam*-l l)
                          (error "error in compile-module-externs"))))
          (compile-module-externs ls))]))

(define (compile-module p root)
  (if root
      (let ((bs stdlib-fs-ls))
        (compile-mod-root (map car bs) (map cdr bs) p))
      (compile-mod-not-root p)))

;; Expr Boolean -> Asm
(define (compile-mod-root fs+ ls+ p)
  (match p
    [(CMod pv-exts pvs fs ls dfλs e)
     (prog (Global 'entry)
           (compile-module-provides dfλs)
           (Default 'rel)
           (Section '.text)
           (compile-module-externs (apply append (map λs (remq* dfλs ls))))
           (remove-duplicates (externs-es ls+))
           (externs (CMod pv-exts pvs fs ls dfλs e))
           (Extern 'raise_error)
           (Global 'raise_error_align)
           (Extern 'str_to_symbol)
           (bignum-externs)
           (Label 'entry)
           (Mov rbx rdi) ; recv heap pointer
           (compile-e-tail (Letrec (append fs+ fs) (append ls+ ls) e) '())
           (Mov rdx rbx) ; return heap pointer in second return register           
           (Ret)
           (Label 'raise_error_align)
           (Sub rsp 8)
           (Jmp 'raise_error)
           (compile-λ-definitions (λs e))
           (compile-λ-definitions (apply append (map λs ls+)))
           (compile-λ-definitions (apply append (map λs dfλs))))]))

;; Expr Boolean -> Asm
(define (compile-mod-not-root p)
  (match p
    [(CMod pv-exts pvs fs ls dfλs e)
     (prog (compile-module-provides dfλs)
           (Default 'rel)
           (Section '.text)
           (compile-module-externs (apply append (map λs (remq* dfλs ls))))
           (externs p)
           (Extern 'raise_error)
           (Extern 'raise_error_align)
           (Extern 'str_to_symbol)
           (bignum-externs)
           (compile-λ-definitions (apply append (map λs dfλs))))]))

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
    [(Lam f xs e0)
     (seq (%% "compile-λ-definition ")
          (Label f)          
          (Cmp rcx (imm->bits (length xs))) ; arity check
          (Jne 'raise_error)

          (compile-e-tail e0 (reverse (append xs (fvs l))))
          ; return
          (Add rsp (* 8 (+ (length xs) (length (fvs l))))) ; pop args & fvs
          (Mov rdx rbx)
          (Ret))]
    [(Lam* f xs xs* e0)
     (let ((loop (gensym 'loop))
           (end (gensym 'end))
           (skip (gensym 'skip)))

       (seq (%% "compile-λ-definition variable-arity")
            (Label f)                        
            (Cmp rcx (imm->bits (length xs)))
            (Jl 'raise_error)
            (Mov rax (imm->bits '()))         ; initialize rest arg
            (Sub rcx (imm->bits (length xs))) ; # of things to pop off of stack

            (Add rsp (* 8 (length (fvs l))))
                           ; align stack pointer to where the last arg is
            (Mov r10 rsp)  ; r10 points to word on stack before the first free var
                           ; this will be the last arg if there are args
            
            (Label loop) ; Form rest list. At each step, rax <- cons pop rax
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

            (Cmp rsp r10)    ; case no args poped off stack (rest list is empty)
            (Sub rsp (* 8 (length (fvs l))))
            (Je skip)
            (Add rsp (* 8 (length (fvs l))))
            (%% "reposition env vars ")
            (reposition-env-vars (length (fvs l)))
            (Label skip)
            
            (%% "push the rest list (xs*)")
            (Push rax) ; push the rest list
            (compile-e-tail e0 (cons xs* (reverse (append xs (fvs l)))))
            ; return
            (Add rsp (* 8 (add1 (+ (length xs) (length (fvs l)))))) ; pop args
            (Mov rdx rbx)
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

;; For viewing the assembly code of the program without the libraries code
(define (compile-p p)
  (let ((in-fs (open-input-file "./lib-fs")))
    (let ((fs (read in-fs)))                           
      (begin
        (close-input-port in-fs)
        (prog (Global 'entry)
              (Default 'rel)
              (Section '.text)
              (externs p)
              (Extern 'raise_error)
              (Global 'raise_error_align)
              (Extern 'str_to_symbol)
              (bignum-externs)
              (Label 'entry)
              (Mov rbx rdi) ; recv heap pointer
              (compile-e-tail p (reverse fs))
              (Add rsp (* 8 (length fs)))       ;; to pop lib fs off the stack
              (Mov rdx rbx) ; return heap pointer in second return register
              (Ret)
              (%% "start-of-compiling-λ-definitions-of-program")
              (compile-λ-definitions (λs p))
              (Label 'raise_error_align)
              (Sub rsp 8)
              (Jmp 'raise_error))))))