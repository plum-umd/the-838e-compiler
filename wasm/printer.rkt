#lang racket
(provide wasm-string)

;; Wasm -> String
(define (wasm-string2 a)
  (begin
    (define op1 (open-output-string))
    (write a op1)
    (get-output-string op1)))

(define (wasm-string-aux a)
  (foldr (λ (i s) (string-append (wasm-string i) s)) "" a))

(define (s-exp2string a)
  (begin
    (define op1 (open-output-string))
    (write a op1)
    (get-output-string op1)))

;; Wasm -> String
(define (wasm-string a)
  (match a
    [(list 'module as ...)
     (string-append "(module \n "  (wasm-string-aux as) ") ")]
    [(list 'func (? symbol? s) (list 'result (? symbol? i)) as ...)
     (string-append "   (func " (symbol->string s) "  (result "
                    (symbol->string i) ")\n " (wasm-string-aux as) "   )" "\n ")]
    [(list 'param as ...)  (string-append "(param " (wasm-string-aux as) ") ")]
    [(list 'result as ...) (string-append "(result " (wasm-string-aux as) ")" "\n ")]
    [(list 'local _ ...) (string-append "     " (s-exp2string a) "\n ")]
    [(list 'memory _) (string-append "     " (s-exp2string a) "\n ")]
    [(list 'i32.const _) (string-append "     " (s-exp2string a) "\n ")]
    [(list 'local.set _) (string-append "     " (s-exp2string a) "\n ")]
    [(list 'global.set _) (string-append "     " (s-exp2string a) "\n ")]
    [(list 'global _ ...)   (string-append "   " (s-exp2string a) "\n ")]
    ['i32.const  "      i32.const "]
    ['i32.add    "      i32.add\n "]
    ['i32.sub    "      i32.sub\n "]
    ['i32.and    "      i32.and\n "]
    ['i32.or     "      i32.or\n "]
    ['i32.xor    "      i32.xor\n "]
    ['i32.clz    "      i32.clz\n "]
    ['i32.shl    "      i32.shl\n "]
    ['i32.shr_s  "      i32.shr_s\n "]
    ['i32.eq     "      i32.eq\n "]
    ['i32.ne     "      i32.ne\n "]
    ['i32.eqz    "      i32.eqz\n "]
    ['i32.gt_s   "      i32.gt_s\n "]
    ['i32.lt_s   "      i32.lt_s\n "]
    ['i32.ge_s   "      i32.ge_s\n "]
    ['i32.le_s   "      i32.le_s\n "]
    ['i32.store  "      i32.store\n "]
    ['i32.load   "      i32.load\n "]
    ['if         "      if\n "]
    ['else       "      else\n "]
    ['end        "      end\n "]
    ['block      "      block\n "]
    ['loop       "      loop\n "]
    ['br_if      "      br_if "]
    ['br         "      br "]
    ['local.set  "      local.set "]
    ['local.get  "      local.get "]
    ['global.set "      global.set "]
    ['global.get "      global.get "]
    ['call       "      call "]
    ['drop       "      drop\n "]
    ['$a         "$a\n "]
    [(? integer? i) (string-append (number->string i) "\n ")]
    [(list 'export as ...) (string-append "   " (s-exp2string a) "\n ")]
    [(list 'import as ...) (string-append "   " (s-exp2string a) "\n ")]
    [(? string? _) (string-append "\"" a "\" ")] 
    [(? symbol? s) (string-append (symbol->string s) "\n ")]))


