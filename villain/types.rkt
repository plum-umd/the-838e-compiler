#lang racket
(provide (all-defined-out))

<<<<<<< HEAD
(define imm-shift          3)
(define imm-mask       #b111)
(define ptr-mask       #b111)
(define type-box       #b001)
(define type-cons      #b010)
(define type-string    #b011)  
(define type-symbol    #b100)
(define type-prefab    #b101)
=======
(define imm-shift           3)
(define imm-mask        #b111)
(define ptr-mask        #b111)
(define type-box        #b001)
(define type-cons       #b010)
(define type-string     #b011)  
(define type-symbol     #b100)
>>>>>>> main
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
(define type-float #b11111000)
(define float-shift  (+ 5 imm-shift))
(define mask-float #b11111111)

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
        ;; will round to 7 significant figures
        [(= type-float (bitwise-and b mask-float))
         (bits->float b)]
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
        [(flonum? v) (float->bits v)]))


;; returns the rounding of the float up to sig decimal places
(define (round-dec dec sig)
  (exact->inexact (/ (round (* dec (expt 10 sig))) (expt 10 sig))))

(define (bits->float b)
  (let (; the max decimal places to round to are the first non-zero bits
        (sig (arithmetic-shift b (- (+ float-shift 32))))
        ; then is bit representing the sign (0 if positive, 1 if negative)
        (sign (bitwise-and (arithmetic-shift b (- (+ float-shift 31)))
                           1))
        ; the next 8 bits are the exponential + 127
        (exp (bitwise-and (- (arithmetic-shift 1 8) 1)
                          (arithmetic-shift b (- (+ float-shift 23)))))
        ;; the next 28 bits represent the mantissa
        (mantissa (bitwise-and (- (arithmetic-shift 1 23) 1)
                               (arithmetic-shift b (- float-shift)))))

    ;; result is a rounded version of (-1)^sign * 2^(exp - 127) * (1 + .mantissa)
    (let ((result (* (expt -1 sign) (expt 2 (- exp 127))
                     (+ 1 (*  (binary->decimal mantissa 0 -23))))))
      (round-dec result sig))))


;; converts a float to bits with the IEEE protocol
;; so that the sign, exponent, and mantissa can be stored
;; where the float approximates to (-1)^sign * 2^exponent
;; * (1 + .mantissa)
(define (float->bits f)
  (let (;; sign = 0 if float is positve and 1 if negative
        [sign (if (> f 0) 0 1)]
        ;; gets maximum decimal place to round to
        [sig (decimal-place f 0 (- 7 (integer-size (truncate f) 0)))]
        ;; gets the exponent and the mantissa in decimal form
        [data (if (= f 0)
                  (cons 0 0)
                  (if (>= (abs f)  1)
                      (calc-exp (abs f) 0 1 /)
                      (calc-exp (abs f) 0 -1 *)))])
    (match data
      ;; convert-streamts the mantissa to binary
      [(cons exp dec)
       (let ([mantissa (dec->binary dec 0 22)])
         (if (= mantissa (arithmetic-shift 1 23))
             (float->bits-helper 0 (+ exp 1) 0)
             (float->bits-helper sign exp mantissa sig)))]
      [_ (error "Bad")])))

(define (integer-size i acc)
  (if (= i 0)
      acc
      (integer-size (truncate (/ i 10)) (+ acc 1))))

;; checks how many decimal places the number goes down to
(define (decimal-place dec acc up-to)
  (cond [(= acc up-to)  acc]
        [(integer? dec) acc]
        [else
         (decimal-place (* dec 10) (+ acc 1) up-to)]))

;; constructs of a 64 binary digit using the IEEE protocol
;; to store the sign, exponent, and mantissa plus the decimal's
;; place to round to. The decimal's place will be the first non-zero
;; bits. The sign will be the next bit. The exponent will be the next
;; 8 bits. The mantissa will be stored in the next 23 bits. The type
;; flag gets stored in the last 5 bits
(define (float->bits-helper sign exp mantissa sig)
  (+ type-float
     (arithmetic-shift sig  (+ 32 float-shift))
     (arithmetic-shift sign (+ 31 float-shift))
     (arithmetic-shift exp  (+ 23 float-shift))
     (arithmetic-shift mantissa float-shift)))

;; finds the exponent where num / 2^exp is greater or equal to 1
;; and less than 2 and returns that and the remaining mantissa
(define (calc-exp num test-exp sign op)
  (let ([check (op num (arithmetic-shift 1 test-exp))])
    (if (and (>= check 1) (< check 2))
        (cons (+ (* test-exp sign) 127) (- check 1))
        (calc-exp num (+ test-exp 1) sign op))))

;; converts the decimals of the mantissa to binary up to 23 bits
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

(define (float-bits? v)
  (= type-float (bitwise-and v mask-float)))

(define (cons-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-cons)))

(define (box-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-box)))

(define (string-bits? v) 
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-string))) 

(define (symbol-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-symbol)))

(define (prefab-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-prefab)))
