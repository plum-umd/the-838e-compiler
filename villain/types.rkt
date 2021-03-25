#lang racket
(provide (all-defined-out))

(define imm-shift           3)
(define imm-mask        #b111)
(define ptr-mask        #x7000000000000007)
(define ptr-top-mask    #x7000000000000000)
(define ptr-bottom-mask #x0000000000000007)
(define type-box        #x0000000000000001)
(define type-cons       #x0000000000000002)
(define type-string     #x0000000000000003)  
(define type-symbol     #x0000000000000004)
(define type-port       #x0000000000000005)
(define type-vector     #x0000000000000006)
(define type-flonum     #x0000000000000007)
(define type-proc       #x1000000000000002)
(define proc-mask       #x7000000000000002)
(define type-bignum     #x1000000000000003)

(define int-shift  (+ 1 imm-shift))
(define char-shift (+ 2 imm-shift))
(define type-int       #b0000)
(define mask-int       #b1111)
(define type-char     #b01000)
(define mask-char     #b11111)
(define val-true    #b0011000)
(define val-false   #b0111000)
(define val-eof     #b1011000)
(define val-void    #b1111000)
(define val-empty  #b10011000)

;; Buffer size will get padded so that port structure aligns to 8 byte
;; boundary. Kept low intentionally to test buffering code.
(define port-buffer-bytes 8)

(define (bits->imm b)
  (cond [(= type-int (bitwise-and b mask-int))
         (arithmetic-shift b (- int-shift))]
        [(= type-char (bitwise-and b mask-char))
         (integer->char (arithmetic-shift b (- char-shift)))]
        [(= b val-true)  #t]
        [(= b val-false) #f]
        [(= b val-eof)  eof]
        [(= b val-void) (void)]
        [(= b val-empty) '()]
        [else (error "invalid bits")]))

(define (imm->bits v)
  (cond [(eof-object? v) val-eof]
        [(integer? v) (arithmetic-shift v int-shift)]
        [(char? v)
         (bitwise-ior type-char
                      (arithmetic-shift (char->integer v) char-shift))]
        [(eq? v #t) val-true]
        [(eq? v #f) val-false]
        [(void? v)  val-void]
        [(empty? v) val-empty]
        ))

;; converts a flonum to bits with the IEEE protocol
;; so that the sign, exponent, and mantissa can be stored
;; where the flonum approximates to (-1)^sign * 2^exponent
;; * (1 + .mantissa)
(define (flonum->bits f)
  (let (;; sign = 0 if flonum is positve and 1 if negative
        [sign (if (> f 0) 0 1)]
        ;; gets the exponent and the mantissa in decimal form
        [data (if (= f 0)
                  (cons 0 0)
                  (if (>= (abs f)  1)
                      (calc-exp (abs f) 0 1 /)
                      (calc-exp (abs f) 0 -1 *)))])
    (match data
      ;; convert-streamts the mantissa to binary
      [(cons exp dec)
       (let ([mantissa (dec->binary dec 0 51)])
         (if (= mantissa (arithmetic-shift 1 52))
             (flonum->bits-helper 0 (+ exp 1) 0)
             (flonum->bits-helper sign exp mantissa)))]
      [_ (error "Bad")])))


;; constructs of a 64 binary digit using the IEEE protocol
;; to store the sign, exponent, and mantissa plus the decimal's
;; place to round /to. The decimal's place will be the first non-zero
;; bits. The sign will be the next bit. The exponent will be the next
;; 11 bits. The mantissa will be stored in the next 52 bits.
(define (flonum->bits-helper sign exp mantissa)
  (+
     (arithmetic-shift sign 63)
     (arithmetic-shift exp  52)
     mantissa)
  )

;; finds the exponent where num / 2^exp is greater or equal to 1
;; and less than 2 and returns that and the remaining mantissa
(define (calc-exp num test-exp sign op)
  (let ([check (op num (arithmetic-shift 1 test-exp))])
    (if (and (>= check 1) (< check 2))
        (cons (+ (* test-exp sign) 1023) (- check 1))
        (calc-exp num (+ test-exp 1) sign op))))

;; converts the decimals of the mantissa to binary up to 52 bits
(define (dec->binary dec acc twoExp)
  (match twoExp
    [-1 (if (>= dec .5) (+ acc 1) acc)]
    [_  (let ((check (* dec 2)))
          (if (>= check  1)
              (dec->binary (- check 1)
                           (+ (arithmetic-shift acc 1) 1 )
                           (- twoExp 1))
              (dec->binary check
                           (arithmetic-shift acc 1)
                           (- twoExp 1))))]))

;; converts the binary of the mantissa to decimal with adding
(define (binary->decimal bits acc twoExp)
  (match twoExp
    [0 acc]
    [_  (if (= 1 (bitwise-and bits 1))
            (binary->decimal (arithmetic-shift bits -1)
                             (+ (expt 2 twoExp) acc)
                             (+ twoExp 1))
            (binary->decimal (arithmetic-shift bits -1)
                             acc
                             (+ twoExp 1)))]))



(define (imm-bits? v)
  (zero? (bitwise-and v imm-mask)))

(define (int-bits? v)
  (zero? (bitwise-and v mask-int)))

(define (char-bits? v)
  (= type-char (bitwise-and v mask-char)))

(define (flonum-bits? v)
  (zero? (bitwise-xor (bitwise-and v ptr-mask) type-flonum)))

(define (cons-bits? v)
  (zero? (bitwise-xor (bitwise-and v ptr-mask) type-cons)))

(define (box-bits? v)
  (zero? (bitwise-xor (bitwise-and v ptr-mask) type-box)))

(define (string-bits? v) 
  (zero? (bitwise-xor (bitwise-and v ptr-mask) type-string))) 

(define (symbol-bits? v)
  (zero? (bitwise-xor (bitwise-and v ptr-mask) type-symbol)))

(define (bignum? v) 
  (or (>= v (arithmetic-shift 1 (- 63 int-shift)))
      (<  v (- (arithmetic-shift 1 (- 63 int-shift))))))

(define (bignum-bits? v)
  (zero? (bitwise-xor (bitwise-and v ptr-mask) type-bignum)))

(define (port-bits? v)
  (zero? (bitwise-xor (bitwise-and v ptr-mask) type-port)))

(define (vector-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-vector)))

(define (proc-bits? v)
  (zero? (bitwise-xor (bitwise-and v proc-mask) type-proc)))
  