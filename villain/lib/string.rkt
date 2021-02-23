#lang racket
(provide string
         string-append
         string-copy!
         string->list
         list->string)

(define (^string-append2 s1 s2)
  (let ((dst (make-string (+ (string-length s1) (string-length s2)) #\0)))
    (begin (begin (^string-copy! dst 0 s1 0 (string-length s1))
                  (^string-copy! dst (string-length s1) s2 0 (string-length s2)))
           dst)))

(define (string-append . ss)
  (match ss
    ['() ""]
    [(cons s ss)
     (^string-append2 s (apply string-append ss))]))

(define (string-copy! dst dst-start src . rest)
  ; ugly, bad error messages
  (match rest
    ['() (^string-copy! dst dst-start src 0 (string-length src))]
    [(cons src-start rest)
     (match rest
       ['() (^string-copy! dst dst-start src src-start (string-length src))]
       [(cons src-end rest)
        (match rest
          ['() (^string-copy! dst dst-start src src-start src-end)])])]))

(define (^string-copy! dst dst-start src src-start src-end)
  ; doesn't do any error checking currently
  (if (= src-start src-end)
      dst
      (begin (string-set! dst dst-start (string-ref src src-start))
             (string-copy! dst (add1 dst-start) src (add1 src-start) src-end))))

(define (substring str start . rest)
  (match rest
    ['() (substring str start (string-length str))]
    [(cons end rest)
     (match rest
       ['() (let ((dst (make-string (- end start) #\0)))
              (^string-copy! dst 0 str start end))])]))

(define (string->list s)
  (^string->list/i s 0))

(define (^string->list/i s i)
  (if (= i (string-length s))
      '()
      (cons (string-ref s i)
            (^string->list/i s (add1 i)))))

(define (list->string cs)
  (^list->string cs 0 (make-string (length cs) #\0)))

(define (^list->string cs i dst)
  (if (empty? cs)
      dst
      (begin (string-set! dst i (car cs))
             (^list->string (cdr cs) (add1 i) dst))))

(define (string . cs)
  (list->string cs))
