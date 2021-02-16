#lang racket
(begin
  (provide * byte?)

  ;; not exactly the right place, but fine for now
  (define (byte? b)
    (if (integer? b)
        (if (<= 0 b)
            (if (<= b 255)
                #t
                #f)
            #f)
        #f))

  ; multiplication
  (define (* x y)
    (if (zero? y)
        0
        (+ x (* x (sub1 y)))))

  )
